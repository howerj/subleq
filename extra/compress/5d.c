/* Based off of LZSS encoder-decoder (Haruhiko Okumura; public domain),
 * 16-bit version */

#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define EI 11  /* typically 10..13 */
#define EJ  4  /* typically 4..5 */
#define P   1  /* If match length <= P then output one character */
#define N (1 << EI)  /* buffer size */
#define F ((1 << EJ) + 1)  /* lookahead buffer size */

#define MASK_HIGHEST_BIT (1 << 15)
#define MASK_HIGHEST	 (1 << 16)

typedef uint16_t u16;
static u16 m[1<<16], n[1<<16], prog = 0, pc = 0, oprog = 0;
static u16 buffer[N * 2];

static long get16(void) {
	if (pc < prog)
		return m[pc++];
	return EOF;
}

static int put16(u16 v) {
	n[oprog++] = v;
	return 0;
}

static long getbit(int n) { /* get n bits */ 
	long i, x;
	static long buf, mask = 0;
	
	x = 0;
	for (i = 0; i < n; i++) {
		if (mask == 0) {
			if ((buf = get16()) == EOF) return EOF;
			mask = MASK_HIGHEST_BIT;
		}
		x <<= 1;
		if (buf & mask) x++;
		mask >>= 1;
	}
	return x;
}

static void decode(void) {
	long i, j, k, r, c;
	
	for (i = 0; i < N - F; i++) buffer[i] = 0;
	r = N - F;
	while ((c = getbit(1)) != EOF) {
		if (c) {
			if ((c = getbit(16)) == EOF) break;
			put16(c);
			buffer[r++] = c;  r &= (N - 1);
		} else {
			if ((i = getbit(EI)) == EOF) break;
			if ((j = getbit(EJ)) == EOF) break;
			for (k = 0; k <= j + 1; k++) {
				c = buffer[(i + k) & (N - 1)];
				put16(c);
				buffer[r++] = c;  r &= (N - 1);
			}
		}
	}
}

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

	decode();

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
