#include <stdint.h>
#include <stdio.h>
int main(int argc, char **argv) { /* 16-bit SUBLEQ OISC */
	static uint16_t m[UINT16_MAX], pc = 0;
	for (int i = 1, d = 0, j = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f) return 1;
		while (fscanf(f, "%d", &d) > 0)
			m[j++ % UINT16_MAX] = d;
		if (fclose(f) < 0) return 2;
	}
	do {
		const uint16_t a = m[pc++], b = m[pc++], c = m[pc++];
		if (a == 0xFFFFu) { m[b] = (int16_t)getchar(); }
		else if (b == 0xFFFFu) {
			if (putchar(m[a]) < 0) return 3;
			if (fflush(stdout) < 0) return 4;
		} else {
			const uint16_t r = m[b] - m[a];
			if (r & 0x8000u || r == 0u) pc = c;
			m[b] = r;
		}
	} while (!(pc & 0x8000u));
	return 0;
}
