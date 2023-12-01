/* From <https://github.com/lmcilroy/lzp>
 * Also see
 * <https://cs.stackexchange.com/questions/134277/what-is-the-simplest-algorithm-to-compress-a-string>
 *
 * LZP compression, not really suitable for a SUBLEQ machine, but
 * a simple test. */
#include "c.h"

#ifndef HASH_ORDER
#define HASH_ORDER	(16)
#endif

#ifndef MLEN
#define MLEN		(16)
#endif

#define HASH_SIZE	(1 << HASH_ORDER)

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
	if (argc < 4)
		return 1;
	const int t = mode(argv[1]);
	if (t < 0)
		return 2;
	if (load(m, &prog, argc - 3, &argv[2]) < 0)
		return 3;
	if (t) {
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
	} else {
		long c = 0;
		for (;;) {
			long j = 0;
			if (in >= prog) {
				break;
			}
			long mask = m[in++];
			for (long i = 0; i < MLEN; i++) {
				if ((mask & (1 << i)) != 0) {
					c = table[hash];
				} else {
					if (in >= prog) {
						break;
					}
					c = m[in++];
					table[hash] = c;
				}
				buf[j++] = c;
				HASH(hash, c);
			}
			if (j > 0) {
				for (long k = 0; k < j; k++)
					n[oprog++] = buf[k];
			}
		}
	}
	if (save(n, oprog, argv[argc - 1]) < 0)
		return 4;
	if (stats(stderr, prog, oprog) < 0)
		return 5;
	return 0;
}


