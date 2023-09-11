/* From <https://github.com/lmcilroy/lzp>
 * Also see
 * <https://cs.stackexchange.com/questions/134277/what-is-the-simplest-algorithm-to-compress-a-string>
 *
 * LZP compression, not really suitable for a SUBLEQ machine, but
 * a simple test. */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#ifndef HASH_ORDER
#define HASH_ORDER	(16)
#endif

#ifndef MLEN
#define MLEN		(16)
#endif

#define HASH_SIZE	(1 << HASH_ORDER)

typedef uint16_t u16;
static u16 m[1<<16], n[1<<16], prog = 0, oprog = 0;
static u16 table[HASH_SIZE] = { 0, };
static u16 buf[MLEN + 1] = { 0, };
static u16 hash = 0; //5381;
static long in = 0;

static u16 djb2(u16 ohash, unsigned char *s, size_t len) {
	unsigned long hash = ohash;
	for (size_t i = 0; i < len; i++)
		hash = ((hash << 5) + hash) ^ s[i]; /* hash * 33 + c */
	return hash;
}

static inline u16 u16djb2(u16 ohash, u16 x) {
	unsigned char bh[2] = { (x >> 0) & 255, (x >> 8) & 255, };
	return djb2(ohash, bh, sizeof (bh));
}

//#define HASH(h, x) (h = u16djb2(h, x))
//#define HASH(h, x) (h = u16djb2(0, x))

#ifndef HASH
#define HASH(h, x) (h = (h << 4) ^ x)
#endif


int main(int argc, char **argv) {
	if (argc < 3)
		return 1;
	for (long i = 1, d = 0; i < (argc - 1); i++) {
		FILE *f = fopen(argv[i], "rb");
		if (!f)
			return 2;
		while (fscanf(f, "%ld,", &d) > 0)
			m[prog++] = d;
		if (fclose(f) < 0)
			return 3;
	}

	for (;;) {
		long i = 0, j = 1;
		long mask = 0, end = 0;
		for (i = 0; i < MLEN; i++) {
			if (in >= prog) {
				end = 1;
				break;
			}
			long c = m[in++];
			if (c == table[hash]) {
				mask |= 1 << i;
			} else {
				table[hash] = c;
				buf[j++] = c;
			}
			HASH(hash, c);
		}
		if (i > 0) {
			buf[0] = mask;

			for (long k = 0; k < j; k++)
				n[oprog++] = buf[k];

		}
		if (end)
			break;
	}


	FILE *f = fopen(argv[argc - 1], "wb");
	if (!f)
		return 4;
	for (long i = 0; i < oprog; i++)
		if (fprintf(f, "%d\n", (int)(int16_t)n[i]) < 0)
			return 5;
	const double percent = (double)oprog / (double)prog * 100.0;
	const int diff = (prog - oprog)*2;
	if (fprintf(stderr, "in=%d out=%d diff=%d ratio=%.3f%%\n", (int)prog*2, (int)oprog*2, diff, percent) < 0)
		return 6;
}
