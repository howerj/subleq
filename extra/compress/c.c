#include "c.h"

#define ELINE (-__LINE__)

#ifndef CONFIG_DEBUG_OUT
#define CONFIG_DEBUG_OUT (0)
#endif

int mode(const char *opt) {
	assert(opt);
	if (!strcmp(opt, "compress"))
		return 1;
	if (!strcmp(opt, "decompress"))
		return 0;
	return -1;
}

int load(u16 *m, u16 *prog, int file_count, char **files) {
	assert(m);
	assert(prog);
	assert(files);
	assert(file_count >= 0);
	*prog = 0;
	size_t p = 0;
	for (long i = 0, d = 0; i < file_count; i++) {
		if (CONFIG_DEBUG_OUT)
			if (fprintf(stderr, "reading: %s\n", files[i]) < 0)
				return ELINE;
		FILE *f = fopen(files[i], "rb");
		if (!f)
			return ELINE;
		while (fscanf(f, "%ld,", &d) > 0)
			m[p++] = d;
		if (fclose(f) < 0)
			return ELINE;
	}
	if (CONFIG_DEBUG_OUT)
		if (fprintf(stderr, "read %ld words\n", (long)p) < 0)
			return ELINE;
	*prog = p;
	return 0;
}

int save(u16 *m, size_t mlen, char *file) {
	assert(m);
	assert(mlen);
	assert(file);
	if (CONFIG_DEBUG_OUT)
		if (fprintf(stderr, "writing %ld words: %s\n", (long)mlen, file) < 0)
			return ELINE;
	FILE *f = fopen(file, "wb");
	if (!f)
		return ELINE;
	for (size_t i = 0; i < mlen; i++)
		if (fprintf(f, "%d\n", (int)(int16_t)m[i]) < 0) {
			(void)fclose(f);
			return ELINE;
		}
	if (fclose(f) < 0)
		return ELINE;
	return 0;
}

int stats(FILE *o, const u16 prog, const u16 oprog) {
	if (!o)
		return 0;
	const double percent = (double)oprog / (double)prog * 100.0;
	const int diff = (prog - oprog) * 2;
	if (fprintf(o, "in=%d out=%d diff=%d ratio=%.3f%%\n", (int)prog*2, (int)oprog*2, diff, percent) < 0)
		return ELINE;
	return 0;
}

