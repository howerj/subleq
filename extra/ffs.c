/* SUBLEQ VM, RICHARD JAMES HOWE */
#include <stdint.h>
#include <stdio.h>

#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

typedef uint16_t u16;
static const u16 n = -1;
static u16 m[1<<16], max = 0, pc = 0;

int main(int argc, char **argv) {
	if (setvbuf(stdout, NULL, _IONBF, 0) < 0)
		return 1;
	for (long i = 1, d = 0; i < (argc - (argc > 2)); i++) {
		FILE *f = fopen(argv[i], "rb");
		if (!f)
			return 2;
		while (fscanf(f, "%ld,", &d) > 0)
			m[max++] = d;
		if (fclose(f) < 0)
			return 3;
	}
	for (pc = 0; pc < 32768;) {
		u16 a = m[pc++], b = m[pc++], c = m[pc++];
		if (a == n) {
			m[b] = getchar();
			max = MAX(max, b);
		} else if (b == n) {
			if (putchar(m[a]) < 0)
				return 4;
		} else {
			u16 r = m[b] - m[a];
			if (r == 0 || r & 32768)
				pc = c;
			m[b] = r;
			max = MAX(max, b);
		}
	}
	while (!m[max])
		max--;
	if (argc > 2) {
		FILE *f = fopen(argv[argc - 1], "wb");
		if (!f)
			return 5;
		for (unsigned i = 0; i < max; i++) {
			if (fprintf(f, "%d\n", (int16_t)m[i]) < 0) {
				(void)fclose(f);
				return 6;
			}
		}
		if (fclose(f) < 0)
			return 7;
	}
	return 0;
}
