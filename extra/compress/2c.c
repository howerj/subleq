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

