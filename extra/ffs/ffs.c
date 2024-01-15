#define EMAIL   "howe.r.j.89@gmail.com"
#define AUTHOR  "Richard James Howe"
#define PROJECT "Forth File System"
#define REPO    "https://github.com/howerj/subleq"

/* TODO:
 * - Make a file system
 * - Make fread/fwrite/fclose/fseek equivalents
 * - File system commands cd/pwd/ls/mkdir/rmdir/rm/cat/ed/...
 * - Shell; DOS like shell, integrate Forth interpreter
 * - Turn into library?
 * - Help / documentation
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

#define BBUF (1024)
#define MIN_BLOCKS (3)
#define MAX_FILE_LENGTH (16)
#define ID "eFORTH FFS HOWE "
#define VERSION (0x0001)
#define SIZE (0x0010)
#define NELEMS(X) (sizeof(X) / sizeof((X)[0]))

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
	DIR_UNUSED,
	DIR_DIR,
	DIR_FILE,
	DIR_SPECIAL,
};

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
};

typedef struct {
	uint8_t id[16];
	uint16_t version;
	uint16_t size;
	uint16_t start;
	uint16_t end;
	uint16_t flags;
	uint16_t crc;
	uint8_t time[4];
	uint16_t boot_block;
} header_t;

typedef struct {
	FILE *use, *in, *out;
	uint8_t block[BBUF]; /* multiple block buffers could be used to speed up disk access */
	int loaded, start, end;
	bool dirty, error;
	header_t header;
} ffs_t;

static inline uint16_t r16(const uint8_t *m) {
	assert(m);
	return (((uint16_t)m[0]) << 0) | (((uint16_t)m[1]) << 8);
}

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
		return ELINE;
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
		goto fail;
	if (!valid(f, blkno))
		goto fail;
	if (f->use == NULL)
		goto fail;
	if (fseek(f->use, blkno * BBUF, SEEK_SET) < 0)
		goto fail;
	if (fread(f->block, 1, BBUF, f->use) != BBUF)
		goto fail;
	return 0;
fail:
	f->error = true;
	return ELINE;
}

static int store(ffs_t *f, int blkno) {
	if (f->error)
		goto fail;
	if (!valid(f, blkno))
		goto fail;
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
		return ELINE;
	if (f->dirty) {
		if (valid(f, f->loaded))
			if (store(f, f->loaded) < 0)
				return ELINE;
		f->dirty = -1;
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
		goto fail;
	if (blkno < 1)
		goto fail;
	if (f->loaded == blkno)
		return f->block;
	if (flush(f) < 0)
		goto fail;
	if (load(f, blkno) < 0)
		goto fail;
	f->loaded = blkno;
	return f->block;
fail:
	f->error = true;
	return NULL;
}

static int header_read(ffs_t *f, int blkno) {
	assert(f);
	if (!valid(f, blkno))
		return ELINE;
	header_t *h = &f->header;
	uint8_t *b = block(f, blkno);
	if (!b)
		return ELINE;
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
		return ELINE;
	if (h->version > VERSION)
		return ELINE;
	return 0;
}

static int header_write(ffs_t *f, int blkno) {
	assert(f);
	uint8_t *b = block(f, blkno);
	if (!b)
		return ELINE;
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
	update(f);
	flush(f);
	return 0;
}

static int header_print(ffs_t *f) {
	assert(f);
	header_t *h = &f->header;
	uint8_t id[sizeof (h->id) + 1] = { 0, };
	memcpy(id, h->id, sizeof (h->id));
	if (fprintf(f->out, "id: %s\n", id) < 0)
		goto fail;
	if (fprintf(f->out, "version: %d\n", h->version) < 0)
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
	f->error = true;
	return ELINE;
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
	if (!dirent_is_valid(f, d)) {
		f->error = true;
		return ELINE;
	}
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
	if (fprintf(f->out, "%s ", d->name) < 0)
		goto fail;

	/*
	b[0] = d->type;
	b[1] = d->reserved1;
	w16(&b[2], d->bytes_used_in_last_block);
	w16(&b[4], d->file_start_block);
	w16(&b[6], d->reserved2);
	memcpy(&b[8], d->time, sizeof(d->time));
	memcpy(&b[12], d->reserved3, sizeof(d->reserved3));
	memcpy(&b[15], d->name, sizeof (d->name));
*/


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
	return NULL;
}

static uint8_t *find_free(ffs_t *f) {
	assert(f);
	return NULL;
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
	if (fprintf(f->out, "fat: start=%d, needed=%d, invalid=%d\n", fat_start, fat_blocks_needed, fat_invalid_blocks) < 0)
		return ELINE;
	
	for (int i = 0; i < fat_blocks_needed; i++) {
		for (int j = 0; j < (BBUF/2); j++) {
			fw16(f, baindx(f, (i*BBUF/2) + j), FAT_INVALID);
			update(f);
		}
	}
	fw16(f, baindx(f, 0), FAT_SPECIAL); /* Contains file system header */
	int i = 0;
	for (i = 1; i < fat_blocks_needed + 1; i++)
		fw16(f, baindx(f, i + 1), FAT_SPECIAL); /* Blocks containing FAT entries */
	for (; i < f->end; i++)
		fw16(f, baindx(f, i), FAT_FREE);
	fw16(f, baindx(f, fat_blocks_needed + 1), FAT_USED); /* First directory entry */

	update(f);
	flush(f);

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
	uint8_t block[BBUF] = { 0, };
	for (int i = 0; i < blocks; i++) {
		if (fwrite(block, 1, sizeof(block), b) != sizeof(block))
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
	if (f->start < 1 || f->end < 1 || f->start >= f->end) {
		f->error = true;
		return ELINE;
	}
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

static int help(FILE *output, const char *arg0) {
	assert(output);
	assert(arg0);
	static const char *fmt = "\
Usage %s {shell|pack|unpack|list|format} ...\n\n\
Project: " PROJECT "\n\
Author:  " AUTHOR  "\n\
Email:   " EMAIL   "\n\
Repo:    " REPO    "\n\
\n";
	return fprintf(output, fmt, arg0);
}

int main(int argc, char **argv) {
	ffs_t ffs = {
		.in = stdin, .out = stdout,
	}, *f = &ffs;

	if (argc < 2)
		goto help_fail;
	if (!strcmp(argv[1], "shell") || !strcmp(argv[1], "sh")) {
		if (argc < 3)
			goto help_fail;
	} else if (!strcmp(argv[1], "pack")) {
	} else if (!strcmp(argv[1], "unpack")) {
	} else if (!strcmp(argv[1], "list")) {
	} else if (!strcmp(argv[1], "format")) {
		if (argc < 5)
			goto help_fail;
		const char *fb = argv[2];
		if (iconvert(argv[3], &f->start) < 0)
			goto help_fail;
		if (iconvert(argv[4], &f->end) < 0)
			goto help_fail;
		if (use(f, fb) < 0)
			goto fail;
		if (format(f) < 0)
			goto fail;
	} else {
		(void)fprintf(stderr, "Invalid command %s\n", argv[0]);
		return 1;
	}

	return 0;
help_fail:
	(void)help(stderr, argv[0]);
fail:
	return 1;
}


