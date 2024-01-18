#define EMAIL   "howe.r.j.89@gmail.com"
#define AUTHOR  "Richard James Howe"
#define PROJECT "Forth File System"
#define REPO    "https://github.com/howerj/subleq"

/* TODO:
 * - Make a file system
 * - Compile time configuration options with "CONFIG_"
 * - Make fread/fwrite/fclose/fseek equivalents
 * - File system commands cd/pwd/ls/mkdir/rmdir/rm/cat/ed/...
 * - Shell; DOS like shell, integrate Forth interpreter
 * - Turn into library?
 * - Help / documentation
 * - Pre/Post conditions, stop write/modify on error, system
 *   check, tests
 * - Make a system of error codes (e.g. Disk full, file does not
 *   exists, etcetera).
 * - Fix error messaging ELINE/fatal/WARN/INFO
 * - Feature Flags; CRC, File CRC, Arbitrary length directories,
 *   multiple fat blocks, arbitrary length files, ...
 * - Include SUBLEQ/eFORTH access */

#include <assert.h>
#include <errno.h>
#include <limits.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <stdarg.h>

#define BBUF            (1024)
#define MIN_BLOCKS      (3)
#define MAX_FILE_LENGTH (16)
#define ID              "eFORTH FFS HOWE " /* TODO: Change this */
#define VERSION         (0x0001)
#define SIZE            (0x0010)
#define NELEMS(X)       (sizeof(X) / sizeof((X)[0]))
#define MAX_DIR_DEPTH   (8)
#define DIRENT_SIZE     (32) 
#define DIRS_PER_BLOCK  (BBUF / DIRENT_SIZE) /* TODO BUILD_BUG_ON remainder */

static int eline(FILE *out, const char *file, const char *func, int line) {
	assert(out);
	assert(func);
	assert(file);
	const int r1 = fprintf(out, "Error: %s/%s/%d\n", file, func, line);
	const int r2 = fflush(out);
	assert(r1 >= 0 && r2 >= 0);
	return line;
}

#define ELINE eline(stderr, __FILE__, __func__, __LINE__)

#ifndef ELINE
#define ELINE (-__LINE__)
#endif

enum {
	FAT_FREE    = 0x0000, /* Free FAT block */
	FAT_USED    = 0x0001, /* End of FAT Linked List / sentinel value */
	FAT_SPECIAL = 0xFFFD, /* Special file / structure */
	FAT_ERROR   = 0xFFFE, /* Bad block */
	FAT_INVALID = 0xFFFF, /* Not part of file system */
};

enum {
	DIR_UNUSED,  /* Directory entry is unused */
	DIR_DIR,     /* Directory entry refers to another directory */
	DIR_FILE,    /* Directory entry is a file */
	DIR_SPECIAL, /* Directory entry is special somehow (and not a file nor a directory) */
};

enum { /* N.B. Negative log levels result in log functions returning -1 and setting error flags */
	LOG_LEVEL_FATAL = -1,
	LOG_LEVEL_WARN  =  0,
	LOG_LEVEL_INFO  =  1,
};

// TODO: RWX permissions, BLOCK type?
typedef struct {
	uint8_t type;
	uint8_t reserved1;
	uint16_t bytes_used_in_last_block;
	uint16_t file_start_block;
	uint16_t reserved2;
	uint8_t time[4];
	uint8_t reserved3[4];
	uint8_t name[MAX_FILE_LENGTH];
} dirent_t;

enum { /* TODO: Implement these flags / error out on unimplemented features */
	FLAG_CRC_ON                  = 1 << 0, /* CRC is on, modifying system will invalidate this */
	FLAG_SINGLE_BLOCK_DIRS_ON    = 1 << 1, /* If on directories cannot grow and there is a 32 file limit per directory */
	FLAG_SINGLE_BLOCK_FILES_ON   = 1 << 2, /* */
	FLAG_DIRECTOR_RECURSION_OFF  = 1 << 3, /* */
	FLAG_TIME_ON                 = 1 << 4, /* A time source is on and present */
	FLAG_MULTIPLE_FAT_BLOCKS_ON  = 1 << 5, /* Multiple FAT blocks are present instead of one (limit to 512 blocks if off */
	FLAG_BOOTABLE                = 1 << 6, /* Bootable block is present */
	FLAG_DIRTY                   = 1 << 7, /* File system could be in inconsistent state */
	FLAG_READ_ONLY               = 1 << 8, /* File system should be read only */
};

typedef struct {
	uint8_t id[16];
	uint16_t version;
	uint16_t size;
	uint16_t start;    /* Start block; block in which the header starts */
	uint16_t end;      /* End block; last block in file system */
	uint16_t flags;
	uint16_t crc;      /* (Optional) CRC of entire file system */
	uint8_t time[4];   /* TODO: Replace with 16-bit year, 8 bit month, 8 bit day? */
	uint16_t boot_block; /* (Optional) boot block, a block, not a file, to load and execute at startup */
	/* TODO: Add block size, Max directory depth, max path length, etc */
} header_t;

typedef struct {
	FILE *use, *in, *out, *err;
	uint8_t block[BBUF]; /* multiple block buffers could be used to speed up disk access */
	int loaded, start, end, after_fat;
	bool dirty, error, init;
	header_t header;

	dirent_t cwd[MAX_DIR_DEPTH];
	int dir_depth;
} ffs_t;

static int msg(ffs_t *f, int level, const char *file, const char *func, unsigned line, const char *fmt, ...) {
	FILE *out = f ? f->err : stderr;
	if (f && level < 0)
		f->error = true;
	if (!out)
		goto end;
	if (file && func)
		if (fprintf(out, "%s:%s:%u ", file, func, line) < 0)
			goto fatal;
	va_list ap;
	va_start(ap, fmt);
	const int r = vfprintf(out, fmt, ap);
	va_end(ap);
	if (r < 0)
		goto fatal;
	if (fputc('\n', out) != '\n')
		goto fatal;
	if (fflush(out) < 0)
		goto fatal;
end:
	return level < 0 ? -1 : 0;
fatal:
	if (f)
		f->error = true;
	return -1;
}

#define warning(F,  ...) msg((F), LOG_LEVEL_WARN,  __FILE__, __func__, __LINE__, __VA_ARGS__)
#define fatal(F, ...) msg((F), LOG_LEVEL_FATAL, __FILE__, __func__, __LINE__, __VA_ARGS__)
#define INFO(F,  ...) msg((F), LOG_LEVEL_INFO,  __FILE__, __func__, __LINE__, __VA_ARGS__)

static inline uint16_t r16(const uint8_t *m) {
	assert(m);
	return (((uint16_t)m[0]) << 0) | (((uint16_t)m[1]) << 8);
}

// TODO: Prevent writes on error
static inline void w16(uint8_t *m, uint16_t v) {
	assert(m);
	m[0] = v >> 0;
	m[1] = v >> 8;
}

static inline uint16_t fr16(ffs_t *f, const uint8_t *m) {
	if (!m) {
		f->error = true;
		return 0;
	}
	return r16(m);
}

static inline void fw16(ffs_t *f, uint8_t *m, uint16_t v) {
	if (!m) {
		f->error = true;
		return;
	}
	w16(m, v);
}

static int convert(const char *s, long *o) {
	assert(s);
	assert(o);
	char *end = NULL;
	int e1 = errno, e2 = 0;
	errno = 0;
	const long r = strtol(s, &end, 10);
	*o = r;
	e2 = errno;
	errno = e1;
        if ((e2 == ERANGE && (r == LONG_MAX || r == LONG_MIN)) || (e2 != 0 && r == 0))
		return -1;
	return 0;
}

static int iconvert(const char *s, int *o) {
	assert(s);
	assert(o);
	long lo = 0;
	const int r = convert(s, &lo);
	*o = lo;
	return r;
}

static bool valid(ffs_t *f, int blkno) {
	if (blkno < 1)
		return false;
	if (blkno < f->start)
		return false;
	if (blkno > f->end)
		return false;
	return true;
}

static int load(ffs_t *f, int blkno) {
	assert(f);
	if (f->error)
		return ELINE;
	//if (!valid(f, blkno))
	//	return ELINE;
	if (f->use == NULL)
		return fatal(f, "use file not set");
	errno = 0;
	if (fseek(f->use, blkno * BBUF, SEEK_SET) < 0)
		return fatal(f, "seek %p/%d failed: %s", f->use, blkno * BBUF, strerror(errno));
	errno = 0;
	if (fread(f->block, 1, BBUF, f->use) != BBUF)
		return fatal(f, "fread failed of size %d: %s", BBUF, strerror(errno));
	return 0;
}

static int store(ffs_t *f, int blkno) {
	if (f->error)
		return ELINE;
	if (!valid(f, blkno))
		return fatal(f, "invalid block number: %d", blkno);
	if (f->use == NULL)
		goto fail;
	if (fseek(f->use, blkno * BBUF, SEEK_SET) < 0)
		goto fail;
	if (fwrite(f->block, 1, BBUF, f->use) != BBUF)
		goto fail;
	if (fflush(f->use) < 0)
		goto fail;
	return 0;
fail:
	f->error = true;
	return ELINE;
}

static int flush(ffs_t *f) {
	assert(f);
	if (f->error)
		return fatal(f, "flush failed, fatal error occurred previously");
	if (f->dirty) {
		if (valid(f, f->loaded))
			if (store(f, f->loaded) < 0)
				return fatal(f, "stored failed");
		f->dirty = 0;
	}
	return 0;
}

static int empty(ffs_t *f) {
	assert(f);
	f->dirty = false;
	f->loaded = -1;
	return 0;
}

static int update(ffs_t *f) {
	assert(f);
	f->dirty = true;
	return f->error ? -1 : 0;
}

static uint8_t *block(ffs_t *f, int blkno) { /* TODO: Make "buffer", testing, ... */
	assert(f);
	if (f->error)
		return NULL;
	if (blkno < 1)
		return fatal(f, "invalid block: %d", blkno), NULL;
	if (f->loaded == blkno)
		return f->block;
	if (flush(f) < 0)
		return fatal(f, "flush failed"), NULL;
	if (load(f, blkno) < 0)
		return fatal(f, "load failed"), NULL;
	f->loaded = blkno;
	return f->block;
}

static int header_read(ffs_t *f, int blkno) {
	assert(f);
	/*if (!valid(f, blkno))
		return ELINE;*/
	header_t *h = &f->header;
	uint8_t *b = block(f, blkno);
	if (!b)
		return fatal(f, "block failed: %d", blkno);
	memcpy(h->id, b, sizeof (h->id)); 
	b += sizeof(h->id);
	h->version = r16(b);
	b += 2;
	h->size = r16(b);
	b += 2;
	h->start = r16(b);
	b += 2;
	h->end = r16(b);
	b += 2;
	h->flags = r16(b);
	b += 2;
	h->crc = r16(b);
	b += 2;
	memcpy(h->time, b, sizeof(h->time));
	b += 4;

	const int blocks = h->end - h->start;
	if (blocks < MIN_BLOCKS || h->end <= h->start)
		return fatal(f, "invalid block range: %d - %d = %d", h->end, h->start, blocks);
	if (h->version > VERSION)
		return fatal(f, "unknown version (expected < %04x): %04x", VERSION, h->version);
	return 0;
}

static int header_write(ffs_t *f, int blkno) {
	assert(f);
	uint8_t *b = block(f, blkno);
	if (!b)
		return fatal(f, "block failed: %d", blkno);
	memset(b, 0, BBUF);
	header_t *h = &f->header;
	memcpy(b, h->id, sizeof(h->id));
	b += sizeof(h->id);
	w16(b, h->version);
	b += 2;
	w16(b, h->size);
	b += 2;
	w16(b, h->start);
	b += 2;
	w16(b, h->end);
	b += 2;
	w16(b, h->flags);
	b += 2;
	w16(b, h->crc);
	b += 2;
	memcpy(b, h->time, sizeof(h->time));
	b += 4;
	if (update(f) < 0)
		return fatal(f, "updated failed");
	if (flush(f) < 0)
		return fatal(f, "flush failed");
	return 0;
}

static int header_print(ffs_t *f) {
	assert(f);
	header_t *h = &f->header;
	uint8_t id[sizeof (h->id) + 1] = { 0, };
	memcpy(id, h->id, sizeof (h->id));
	if (fprintf(f->out, "id: \"%s\"\n", id) < 0)
		goto fail;
	if (fprintf(f->out, "version: $%04x\n", h->version) < 0)
		goto fail;
	if (fprintf(f->out, "size: %d\n", h->size) < 0)
		goto fail;
	if (fprintf(f->out, "start: %d\n", h->start) < 0)
		goto fail;
	if (fprintf(f->out, "end: %d\n", h->end) < 0)
		goto fail;
	if (fprintf(f->out, "flags: %d\n", h->flags) < 0)
		goto fail;
	if (fprintf(f->out, "CRC: %d\n", h->crc) < 0)
		goto fail;
	if (fprintf(f->out, "time: %d.%d.%d.%d\n", h->time[0], h->time[1], h->time[2], h->time[3]) < 0)
		goto fail;
	return 0;
fail:
	return fatal(f, "print failed");
}

static bool is_valid_type(const uint8_t type) {
	return type == DIR_UNUSED || type == DIR_SPECIAL || type == DIR_FILE || type == DIR_DIR;
}

static bool dirent_is_valid(ffs_t *f, const dirent_t *d) {
	assert(f);
	assert(d);
	if (!is_valid_type(d->type))
		return false;
	/*if (!valid(f, d->file_start_block))
		return false;*/
	if (d->reserved1 || d->reserved2 || d->reserved3[0] || d->reserved3[1] || d->reserved3[2] || d->reserved3[3])
		return false;
	return true;
}

static int dirent_read(ffs_t *f, dirent_t *d, const uint8_t *b) {
	assert(f);
	assert(d);
	assert(b);
	d->type = b[0];
	d->reserved1 = b[1];
	d->bytes_used_in_last_block = r16(&b[2]);
	d->file_start_block = r16(&b[4]);
	d->reserved2 = r16(&b[6]);
	memcpy(d->time, &b[8], sizeof(d->time));
	memcpy(d->reserved3, &b[12], sizeof(d->reserved3));
	memcpy(d->name, &b[15], sizeof(d->name));
	if (!dirent_is_valid(f, d)) {
		f->error = true;
		return ELINE;
	}
	return 0;
}

static int dirent_write(ffs_t *f, const dirent_t *d, uint8_t *b) {
	assert(f);
	assert(d);
	assert(b);
	if (!dirent_is_valid(f, d))
		return fatal(f, "invalid dirent");
	b[0] = d->type;
	b[1] = d->reserved1;
	w16(&b[2], d->bytes_used_in_last_block);
	w16(&b[4], d->file_start_block);
	w16(&b[6], d->reserved2);
	memcpy(&b[8], d->time, sizeof(d->time));
	memcpy(&b[12], d->reserved3, sizeof(d->reserved3));
	memcpy(&b[15], d->name, sizeof (d->name));
	return 0;
}

static int dirent_print(ffs_t *f, const dirent_t *d) {
	assert(f);
	assert(d);
	if (!dirent_is_valid(f, d)) {
		f->error = true;
		return ELINE;
	}
	const char *type[] = {
		[DIR_DIR]     = "DIR",
		[DIR_FILE]    = "FIL",
		[DIR_SPECIAL] = "SPC",
		[DIR_UNUSED]  = "   ",
	};
	assert(d->type < NELEMS(type));

	uint8_t name[sizeof (d->name) + 1] = { 0, };
	memcpy(name, d->name, sizeof(d->name));
	if (fprintf(f->out, "\t%s ", type[d->type]) < 0)
		goto fail;
	if (fprintf(f->out, "%4x ", (int)d->file_start_block) < 0)
		goto fail;
	if (fprintf(f->out, "%4x ", (int)d->bytes_used_in_last_block) < 0)
		goto fail;
	if (fprintf(f->out, "%s ", d->name) < 0)
		goto fail;
	if (fprintf(f->out, "\n") < 0)
		goto fail;
	return 0;
fail:
	f->error = true;
	return ELINE;
}

static uint8_t *indx(ffs_t *f, int blkno, int cell) {
	assert(f);
	if (!valid(f, blkno))
		goto error;
	if (cell < 0 || cell > (BBUF/2))
		goto error;
	uint8_t *b = block(f, blkno);
	if (!b)
		goto error;
	return b + (cell * 2);
error:
	f->error = true;
	return NULL;
}

static uint8_t *bindx(ffs_t *f, int blkno, int cell) {
	assert(f);
	return indx(f, f->start + 1, cell);
}

static uint8_t *aindx(ffs_t *f, int addr) {
	if (addr < 0)
		goto error;
	const int blkno = addr / (BBUF/2);
	const int cell = addr % (BBUF/2);
	return indx(f, blkno, cell);
error:
	f->error = true;
	return NULL;
}

static uint8_t *baindx(ffs_t *f, int addr) {
	assert(f);
	return bindx(f, addr / (BBUF/2), addr % (BBUF/2));
}

static uint8_t *next(ffs_t *f, int blkno) {
	assert(f);
	return baindx(f, blkno);
}

static uint8_t *find_free(ffs_t *f) {
	assert(f);
	return NULL;
}

static int pushd(ffs_t *f, dirent_t *d) {
	assert(f);
	assert(d);
	assert(f->dir_depth >= 0 && f->dir_depth < (int)NELEMS(f->cwd));
	if (f->dir_depth >= (int)NELEMS(f->cwd))
		return fatal(f, "invalid peekd depth: %d", f->dir_depth);
	f->cwd[f->dir_depth++] = *d;
	return 0;
}

static dirent_t *popd(ffs_t *f) {
	assert(f);
	assert(f->dir_depth >= 0 && f->dir_depth < (int)NELEMS(f->cwd));
	if (f->dir_depth <= 0) {
		fatal(f, "invalid peekd depth: %d", f->dir_depth);
		return NULL;
	}
	return &f->cwd[--f->dir_depth];
}

static dirent_t *peekd(ffs_t *f) {
	if (f->dir_depth <= 0) {
		fatal(f, "invalid peekd depth: %d", f->dir_depth);
		return NULL;
	}
	return &f->cwd[f->dir_depth - 1];
}

static int mount(ffs_t *f, int start) {
	assert(f);

	if (header_read(f, start) < 0)
		return ELINE;

	// TODO: Turn into function, error check / validate
	const int blocks = f->end - f->start;
	const int fat_start = f->start + 1;
	const int fat_blocks_needed = (blocks / (BBUF/2)) + !!(blocks % (BBUF/2));
	//const int fat_invalid_blocks = (BBUF/2) - (blocks % (BBUF/2));
	f->after_fat = fat_start + fat_blocks_needed + 1;

	dirent_t dir = { .type = DIR_DIR, .file_start_block = f->after_fat, }, *d = &dir;
	if (pushd(f, d) < 0)
		return ELINE;
	f->init = true;
	return 0;
}

static int format(ffs_t *f) {
	assert(f);
	if (f->end < 0 || f->start < 0 || f->start >= f->end)
		return ELINE;
	const int blocks = f->end - f->start;
	if (blocks < MIN_BLOCKS)
		return ELINE;

	for (int i = f->start; i < f->end; i++) {
		uint8_t *b = block(f, i);
		if (!b)
			return ELINE;
		memset(b, 0, BBUF);
		update(f);
	}

	header_t header = {
		.id = ID,
		.version = VERSION,
		.size = SIZE,
		.start = f->start,
		.end = f->end,
	};
	f->header = header;

	if (header_write(f, f->start) < 0)
		return ELINE;

	const int fat_start = f->start + 1;
	const int fat_blocks_needed = (blocks / (BBUF/2)) + !!(blocks % (BBUF/2));
	const int fat_invalid_blocks = (BBUF/2) - (blocks % (BBUF/2));
	f->after_fat = fat_start + fat_blocks_needed + 1;
	if (fprintf(f->out, "fat: start=%d, needed=%d, invalid=%d\n", fat_start, fat_blocks_needed, fat_invalid_blocks) < 0)
		return ELINE;
	
	for (int i = 0; i < fat_blocks_needed; i++) {
		for (int j = 0; j < (BBUF/2); j++) {
			fw16(f, baindx(f, (i*BBUF/2) + j), FAT_INVALID);
			if (update(f) < 0)
				return fatal(f, "update failed");
		}
	}
	fw16(f, baindx(f, 0), FAT_SPECIAL); /* Contains file system header */
	int i = 0;
	for (i = 1; i < fat_blocks_needed + 1; i++)
		fw16(f, baindx(f, i + 1), FAT_SPECIAL); /* Blocks containing FAT entries */
	for (; i < f->end; i++)
		fw16(f, baindx(f, i), FAT_FREE);
	fw16(f, baindx(f, f->after_fat), FAT_USED); /* First directory entry */

	if (update(f) < 0)
		return fatal(f, "update failed");
	if (flush(f) < 0)
		return fatal(f, "flush failed");

	/* TODO: Feature check */
	return 0;
}

static int create(ffs_t *f, const char *file, int blocks) {
	assert(f);
	assert(file);
	errno = 0;
	const char *mode = "wb";
	FILE *b = fopen(file, mode);
	if (!b) {
		(void)fprintf(stderr, "Could not open file '%s' in mode %s: %s\n", file, mode, strerror(errno));
		return ELINE;
	}
	blocks = blocks < MIN_BLOCKS ? MIN_BLOCKS : blocks;
	uint8_t blk[BBUF] = { 0, };
	for (int i = 0; i < blocks; i++) {
		if (fwrite(blk, 1, sizeof(blk), b) != sizeof(blk))
			goto fail;
	}
	if (fclose(b) < 0)
		return ELINE;
	return 0;
fail:
	if (b)
		(void)fclose(b);
	b = NULL;
	f->error = true;
	return ELINE;
}

static bool exists(ffs_t *f, const char *file) {
	assert(f);
	assert(file);
	FILE *b = fopen(file, "rb");
	const bool e = !!b;
	if (b) {
		if (fclose(b) < 0)
			f->error = true;
	}
	return e;
}

static int use(ffs_t *f, const char *file) {
	assert(f);
	assert(file);
	/*if (f->start < 1 || f->end < 1 || f->start >= f->end) {
		f->error = true;
		return ELINE;
	}*/
	if (!exists(f, file))
		if (create(f, file, f->end) < 0) {
			f->error = true;
			return ELINE;
		}
	errno = 0;
	const char *mode = "rb+";
	f->use = fopen(file, mode);
	if (!f->use) {
		(void)fprintf(stderr, "Could not open file '%s' in mode %s: %s\n", file, mode, strerror(errno));
		f->error = true;
		return ELINE;
	}
	return 0;
}

static int cleanup(ffs_t *f) {
	assert(f);
	if (f->use) {
		if (fflush(f->use) < 0) {
			(void)fclose(f->use);
			goto fail;
		}
		if (fclose(f->use) < 0)
			goto fail;
	}
	f->use = NULL;
	return 0;
fail:
	f->error = true;
	f->use = NULL;
	return ELINE;
}

static int cwdblk(ffs_t *f) {
	assert(f);
	dirent_t *cwd = peekd(f);
	if (!cwd)
		return -1;
	int blk = cwd->file_start_block;
	return f->after_fat + blk; // offset needed?
}

static int ls(ffs_t *f) {
	assert(f);
	assert(f->dir_depth >= 0 && f->dir_depth < (int)NELEMS(f->cwd));

	uint8_t *b = block(f, cwdblk(f));
	if (!b)
		return ELINE;

	for (int i = 0; i < DIRS_PER_BLOCK; i++) {
		dirent_t dir = { .type = DIR_UNUSED, }, *d = &dir;
		if (dirent_read(f, d, &b[i*DIRENT_SIZE]) < 0)
			return ELINE;
		if (d->type == DIR_UNUSED)
			continue; /* or, break */
		if (dirent_print(f, d) < 0)
			return ELINE;
	}

	/* TODO: Feature test; growable directories */
	//next();

	return 0;
}

static uint8_t *find_dir(ffs_t *f, int dirblk, const char *dirname) {
	assert(f);
	assert(dirname);
	if (!valid(f, dirblk))
		return NULL;

	uint8_t *b = block(f, dirblk);
	if (!b)
		return NULL;

	const size_t dlen = strlen(dirname);
	if (dlen > MAX_FILE_LENGTH)
		return warning(f, "file name too long: %s", dirname), NULL;

	uint8_t dn[MAX_FILE_LENGTH] = { 0, };
	memcpy(dn, dirname, dlen);

	for (int i = 0; i < DIRS_PER_BLOCK; i++) {
		dirent_t dir = { .type = DIR_UNUSED, }, *d = &dir;
		uint8_t *entry = &b[i * DIRENT_SIZE];
		if (dirent_read(f, d, entry) < 0) {
			f->error = true;
			return NULL;
		}
		if (!memcmp(d->name, dn, MAX_FILE_LENGTH))
			return entry;
	}

	/* TODO: Feature test; growable directories */
	//next();

	return 0;
}

static uint8_t *find_free_dir(ffs_t *f, int dirblk) {
	assert(f);
	//if (!valid(f, dirblk))
	//	return fatal(f, "invalid block: %d", dirblk), NULL;
	uint8_t *b = block(f, dirblk);
	if (!b)
		return fatal(f, "block failed: %d", dirblk), NULL;
	for (int i = 0; i < DIRS_PER_BLOCK; i++) {
		dirent_t dir = { .type = DIR_UNUSED, }, *d = &dir;
		uint8_t *entry = &b[i * DIRENT_SIZE];
		if (dirent_read(f, d, entry) < 0)
			return NULL;
		if (dir.type == DIR_UNUSED)
			return entry;
	}
	// TODO: Grow directory
	return NULL;
}

static int cd(ffs_t *f, const char *dirname) {
	assert(f);
	assert(dirname);
	if (!strcmp(dirname, ".")) {
		return 0;
	} else if (!strcmp(dirname, "..")) {
		if (f->dir_depth > 1) {
			if (!popd(f))
				return fatal(f, "ran out of pop");
			return 0;
		}
	}

	uint8_t *b = find_dir(f, cwdblk(f), dirname);
	if (!b)
		return warning(f, "directory not found"), 1;
	dirent_t dir = { .type = DIR_UNUSED, }, *d = &dir;
	if (dirent_read(f, d, b) < 0)
		return fatal(f, "reading directory entry failed");
	if (f->dir_depth >= MAX_DIR_DEPTH)
		return warning(f, "directory depth exceeded: %d", MAX_DIR_DEPTH), 1;
	if (pushd(f, d) < 0)
		return fatal(f, "pushing directory failed");
	return 0;
}

static int mkdir(ffs_t *f, const char *dirname) {
	assert(f);
	assert(dirname);
	// if (!valid_name(dirname)) { return ERROR; }

	if (find_dir(f, cwdblk(f), dirname))
		return warning(f, "directory name already exists: %s", dirname), 1;
	uint8_t *b = find_free_dir(f, cwdblk(f));
	if (!b)
		return fatal(f, "OOM");

	dirent_t dir = { .type = DIR_DIR, }, *d = &dir;
	const size_t dlen = strlen(dirname);
	if (dlen > MAX_FILE_LENGTH)
		return warning(f, "file name too long: %s", dirname), 1;
	memcpy(d->name, dirname, dlen);
	if (dirent_write(f, d, b) < 0)
		return fatal(f, "dirent write failed");
	//dirent_print(f, d);
	if (update(f) < 0)
		return fatal(f, "updated failed");
	if (flush(f) < 0)
		return fatal(f, "flush failed");
	return 0;
}

static int rmdir(ffs_t *f, const char *dirname) {
	assert(f);
	assert(dirname);
	uint8_t *b = find_dir(f, cwdblk(f), dirname);
	if (!b)
		return warning(f, "directory does not exist: %s", dirname), 1;
	// TODO: Implement
	return 0;
}

static int renamedir(ffs_t *f, const char *olddir, const char *newdir) {
	assert(f);
	assert(olddir);
	assert(newdir);
	// if !find_dir(olddir) return ERROR
	// if find_dir(newdir) return ERROR
	return 0;
}

/* N.B. For a more generic system we could make an N-bit SUBLEQ machine, and
 * pass in callbacks with data instead of using "in/fputc" and "out/fputc" */
static int subleq(FILE *in, FILE *out, uint16_t *m, const size_t mlen, uint16_t *pc) {
	assert(in);
	assert(out);
	assert(pc);
	int r = 0;
	uint16_t p = *pc;
	if (mlen < 1) { return -1; }

	for (; p != 0xFFFF && p < mlen;) {
		uint16_t a = m[p++ % mlen], b = m[p++ % mlen], c = m[p++ % mlen];
		if (a == 0xFFFF) {
			m[b % mlen] = fgetc(in);
		} else if (b == 0xFFFF) {
			if (fputc(m[a % mlen], out) < 0) { r = -2; break; }
			if (fflush(out) < 0) { r = -3; break; }
		} else {
			uint16_t r = m[b % mlen] - m[a % mlen];
			if (r & 0x8000 || r == 0)
				p = c;
			m[b % mlen] = r;
		}
	}
	*pc = p;
	return r;
}

static int help(FILE *output, const char *arg0) {
	assert(output);
	assert(arg0);
	static const char *fmt = "\
Usage %s {shell|pack|unpack|list|format|mkdir|rmdir} ...\n\n\
Project: " PROJECT "\n\
Author:  " AUTHOR  "\n\
Email:   " EMAIL   "\n\
Repo:    " REPO    "\n\
\n";
	return fprintf(output, fmt, arg0);
}

int main(int argc, char **argv) {
	ffs_t ffs = { .in = stdin, .out = stdout, .err = stderr, }, *f = &ffs;

	if (argc < 2)
		goto help_fail;
	if (!strcmp(argv[1], "shell") || !strcmp(argv[1], "sh")) {
		if (argc < 3)
			goto help_fail;
	} else if (!strcmp(argv[1], "subleq")) {
		/* TODO: include default eForth program image */
		static const size_t mlen = 65536;
		uint16_t m[mlen], pc = 0, prog = 0;
		memset(m, 0, sizeof(m));
		if (argc < 3)
			goto help_fail;
		for (long i = 2, d = 0; i < argc; i++) {
			FILE *f = fopen(argv[i], "rb");
			if (!f)
				return 1;
			while (fscanf(f, "%ld,", &d) > 0)
				m[prog++] = d;
			if (fclose(f) < 0)
				return 2;
		}
		return subleq(stdin, stdout, m, mlen, &pc);
	} else if (!strcmp(argv[1], "pack")) {
	} else if (!strcmp(argv[1], "unpack")) {
	} else if (!strcmp(argv[1], "mkdir")) {
		int mount_block = 1;
		if (argc < 4)
			goto help_fail;
		const char *fb = argv[2];
		if (use(f, fb) < 0)
			goto fail;
		if (mount(f, mount_block) < 0)
			goto fail;
		if (mkdir(f, argv[3]) < 0)
			goto fail;
	} else if (!strcmp(argv[1], "list")) {
		int mount_block = 1;
		if (argc < 3)
			goto help_fail;

		const char *fb = argv[2];
		if (argc > 3) {
			if (iconvert(argv[3], &mount_block) < 0)
				goto help_fail;
		}
		if (use(f, fb) < 0)
			goto fail;
		if (mount(f, mount_block) < 0)
			goto fail;
		if (header_print(f) < 0)
			goto fail;
		if (ls(f) < 0)
			goto fail;
	} else if (!strcmp(argv[1], "format")) {
		if (argc < 5)
			goto help_fail;
		const char *fb = argv[2];
		if (iconvert(argv[3], &f->start) < 0)
			goto help_fail;
		if (iconvert(argv[5], &f->end) < 0)
			goto help_fail;
		if (use(f, fb) < 0)
			goto fail;
		if (format(f) < 0)
			goto fail;
	} else {
		(void)fprintf(stderr, "Invalid command %s\n", argv[0]);
		return 1;
	}
	if (cleanup(f) < 0)
		return 1;
	return 0;
help_fail:
	(void)help(stderr, argv[0]);
fail:
	(void)cleanup(f);
	return 1;
}



