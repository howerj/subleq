/* Based off of LZSS encoder-decoder (Haruhiko Okumura; public domain),
 * 16-bit version */

#include "c.h"

#define EI 11  /* typically 10..13 */
#define EJ  4  /* typically 4..5 */
#define P   1  /* If match length <= P then output one character */
#define N (1 << EI)  /* buffer size */
#define F ((1 << EJ) + 1)  /* lookahead buffer size */

#define MASK_HIGHEST_BIT (1 << 15)
#define MASK_HIGHEST	 (1 << 16)

static u16 m[1<<16], n[1<<16], prog = 0, pc = 0, oprog = 0;
static unsigned long bit_buffer = 0, bit_mask = MASK_HIGHEST_BIT;
static u16 buffer[N * 2];

static void error(void) {
	printf("Output error\n");  
	exit(1);
}

static long get16(void) {
	if (pc < prog)
		return m[pc++];
	return EOF;
}

static int put16(u16 v) {
	n[oprog++] = v;
	return 0;
}

static void putbit1(void) {
	bit_buffer |= bit_mask;
	if ((bit_mask >>= 1) == 0) {
		if (put16(bit_buffer) == EOF) error();
		bit_buffer = 0;  bit_mask = MASK_HIGHEST_BIT;
	}
}

static void putbit0(void) {
	if ((bit_mask >>= 1) == 0) {
		if (put16(bit_buffer) == EOF) error();
		bit_buffer = 0;  bit_mask = MASK_HIGHEST_BIT;
	}
}

static void flush_bit_buffer(void) {
	if (bit_mask != MASK_HIGHEST_BIT) {
		if (put16(bit_buffer) == EOF) error();
	}
}

static void output1(int c) {
	long mask;
	
	putbit1();
	mask = MASK_HIGHEST;
	while (mask >>= 1) {
		if (c & mask) putbit1();
		else putbit0();
	}
}

static void output2(int x, int y) {
	long mask;
	
	putbit0();
	mask = N;
	while (mask >>= 1) {
		if (x & mask) putbit1();
		else putbit0();
	}
	mask = (1 << EJ);
	while (mask >>= 1) {
		if (y & mask) putbit1();
		else putbit0();
	}
}

static void encode(void) {
	long i, j, f1, x, y, r, s, bufferend, c;
	
	for (i = 0; i < N - F; i++) buffer[i] = 0;
	for (i = N - F; i < N * 2; i++) {
		if ((c = get16()) == EOF) break;
		buffer[i] = c;
	}
	bufferend = i;  r = N - F;  s = 0;
	while (r < bufferend) {
		f1 = (F <= bufferend - r) ? F : bufferend - r;
		x = 0;  y = 1;  c = buffer[r];
		for (i = r - 1; i >= s; i--)
			if (buffer[i] == c) {
				for (j = 1; j < f1; j++)
					if (buffer[i + j] != buffer[r + j]) break;
				if (j > y) {
					x = i;  y = j;
				}
			}
		if (y <= P) {  y = 1;  output1(c);  }
		else output2(x & (N - 1), y - 2);
		r += y;  s += y;
		if (r >= N * 2 - F) {
			for (i = 0; i < N; i++) buffer[i] = buffer[i + N];
			bufferend -= N;  r -= N;  s -= N;
			while (bufferend < N * 2) {
				if ((c = get16()) == EOF) break;
				buffer[bufferend++] = c;
			}
		}
	}
	flush_bit_buffer();
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
	if (argc < 4)
		return 1;
	const int t = mode(argv[1]);
	if (t < 0)
		return 2;
	if (load(m, &prog, argc - 3, &argv[2]) < 0)
		return 3;
	if (t)
		encode();
	else
		decode();

	if (save(n, oprog, argv[argc - 1]) < 0)
		return 4;
	if (stats(stderr, prog, oprog) < 0)
		return 5;
	return 0;

}

