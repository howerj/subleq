#include <stdio.h>
#include <stdint.h>
#define TRACE (0)
#define SZ    (32768)
int main(int argc, char **argv) { /* 16-bit SUBLEQ OISC */
	uint16_t m[SZ] = { 0, }, pc = 0;
	for (int i = 1, d = 0, j = 0; i < argc && j < SZ; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f) return 1;
		while (j < SZ && fscanf(f, "%d", &d) > 0)
			m[j++] = d;
		if (fclose(f) < 0) return 2;
	}
	do {
		const uint16_t a = m[pc++ % SZ], b = m[pc++ % SZ], c = m[pc++ % SZ];
		if (a == 0xFFFFu) { m[b % SZ] = (int16_t)getchar(); }
		else if (b == 0xFFFFu) { if (putchar(m[a % SZ]) < 0) return 3; }
		else {
			const uint16_t lb = m[b % SZ], la = m[a % SZ];
			const uint16_t r = lb - la;
			if (TRACE)
				if (fprintf(stderr, "% 6hd a:% 6hd b:% 6hd la:% 6hd lb:% 6hd c:% 6hd\n", 
						(short)pc-3, (short)a, (short)b, (short)la, (short)lb, (short)c) < 0)
					return 4;
			if (r & 0x8000u || r == 0u) pc = c;
			m[b % SZ] = r;
		}
	} while (!(pc & 0x8000u));
	return 0;
}
