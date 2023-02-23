/* Compression experiments for large 16-bit SUBLEQ images 
 *
 * See: <https://richg42.blogspot.com/>,
 * <https://news.ycombinator.com/item?id=32401548>,
 * <https://en.wikipedia.org/wiki/Lempel%E2%80%93Ziv%E2%80%93Storer%E2%80%93Szymanski>
 * */
#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <errno.h>

#define SZ (1ul << 16)

typedef struct {
	uint16_t m[SZ], pc;
	FILE *in, *out;
	int encode;
} codec_t;

static FILE *fopen_or_die(const char *file, const char *mode) {
	assert(file);
	assert(mode);
	errno = 0;
	FILE *r = fopen(file, mode);
	if (!r) {
		(void)fprintf(stderr, "Unable to open file '%s' in mode '%s': %s\n", file, mode, strerror(errno));
		exit(1);
	}
	return r;
}

static int load(codec_t *c) {
	assert(c);
	long d = 0;
	while (fscanf(c->in, "%ld", &d) > 0)
		c->m[c->pc++] = (long)(int16_t)d;
	return 0;
}

static int save(codec_t *c) {
	assert(c);
	for (size_t i = 0; i < c->pc; i++)
		if (fprintf(c->out, "%ld\n", (long)(int16_t)c->m[i]) < 0)
			return -1;
	return 0;
}

static int encode(codec_t *c) {
	assert(c);
	return 0;
}

static int decode(codec_t *c) {
	assert(c);
	const int base = 8192, width = 2048;
	for (size_t i = 0; i < c->pc;) {
		uint16_t instr = c->m[i];
		if (instr <= (base + (width * 0))) { /* LIT */
			i++;
		} else if (instr <= (base + (width * 1))) { /* LIT RUN */
			uint16_t len = instr - (base + width * 1);
			for (uint16_t j = 0; j < len; j++) {
				c->m[j + i] = c->m[j + i + 1];
			}
			i += len + 1;
		} else if (instr <= (base + (width * 2))) { /* COPY */
			uint16_t len = instr - (base + width * 2);
		} else if (instr <= (base + (width * 3))) { /* RLE */
			uint16_t len = instr - (base + width * 3);
		} else if (instr <= (base + (width * 4))) { /* SUB */
		}
	}
	return 0;
}

static int codec(codec_t *c) {
	assert(c);
	return c->encode ? encode(c) : decode(c);
}

static int help(FILE *out, const char *arg0) {
	assert(out);
	assert(arg0);
	static const char *fmt = "Usage:  %s e|d in? out?\n\
Project: Experimental SUBLEQ VM Image Compressor.\n\
Author:  Richard James Howe.\n\
Repo:    https://github.com/howerj/subleq\n\
License: Public Domain / The Unlicense\n\
Email:   howe.r.j.89@gmail.com\n\n";
	return fprintf(out, fmt, arg0);
}

int main(int argc, char **argv) {
	int r = 0, encode = -1;
	if (argc < 2) {
		(void)help(stderr, argv[0]);
		return 1;
	}
	if (!strcmp(argv[1], "e") || !strcmp(argv[1], "encode")) {
		encode = 1;
	} else if (!strcmp(argv[1], "d") || !strcmp(argv[1], "decode")) {
		encode = 0;
	} else {
		(void)help(stderr, argv[0]);
		return 2;
	}

	FILE *in  = argc > 2 ? fopen_or_die(argv[2], "rb") : stdin;
	FILE *out = argc > 3 ? fopen_or_die(argv[3], "wb") : stdout;
	codec_t c = { .in = in, .out = out, .encode = encode, };

	if (load(&c) < 0)  { r = -1; goto end; }
	if (codec(&c) < 0) { r = -2; goto end; }
	if (save(&c) < 0)  { r = -3; goto end; }
end:
	if (fclose(in) < 0)  { r = -4; }
	if (fclose(out) < 0) { r = -5; }
	if (r < 0) {
		(void)fprintf(stderr, "Error: %d\n", r);
	}
	return r < 0 ? 1 : 0;
}
