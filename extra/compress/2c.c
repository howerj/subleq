#include <stdint.h>
#include <stdio.h>
#include <string.h>

typedef uint16_t u16;
static u16 m[1<<16], n[1<<16], prog = 0, oprog = 0;

#ifndef MIN
#define MIN(X, Y) ((X) < (Y) ? (X) : (Y))
#endif

#ifndef MAX 
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
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

	for (long i = 0; i < prog; i++) {
		u16 o = m[i];
		/* TODO: Find matches of 2-5 cells in length, brute force encoder,
		 * this requires a lookahead pointer/buffer */
		long pos = 0, len = -1;

		/*f1 = (F <= bufferend - r) ? F : bufferend - r;
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
		else output2(x & (N - 1), y - 2);*/

		for (int j = 2; j <= 5; j++) {

		}



#if 0
		if (i > 5*2) {
			for (int j = 2; j <= 5; j++) {
				for (long k = i - (j*2); k > MAX(0, i - 0xFFF); k--)
					if (!memcmp(&m[i-j],&m[k], k * sizeof(u16))) {
						if (j > len) {
							pos = k;
							len = j;
						}
					}
			}
		}


		for (long j = prog; j >= 0; j--) {
			if (j > 0xFFF) /* match length exceeded */
				break;
			if (m[j] == m[i]) {
			}
		}

		/*for (long j = 2; j < 5; j++) {
			for (long k = MIN(j, prog - 0xFFF); k < i; k++) {
				if (x != y) {
					break;
				}
			}
		}*/
#endif
		if (len > 2 || (len == 2 && pos != 1)) {
			o = pos;
			if (len == 5) { o += 0x4000; } 
			else if (len == 4) { o += 0x2000; } 
			else if (len == 3) { o += 0x1000; } 
			else { }
			o = -o;
			n[oprog++] = o;
		} else {
			if (0x8000 & o) {
				n[oprog++] = 0xFFFF;
				n[oprog++] = m[i];
			}  else {
				n[oprog++] = o;
			}
		}
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

#if 0
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

	encode();

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
#endif
