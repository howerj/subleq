#include <stdint.h>
#include <stdio.h>

typedef uint16_t u16;
static const u16 n = -1;
static u16 m[1<<16], prog = 0, pc = 0;

int main(int argc, char **argv) {
	if (setvbuf(stdout, NULL, _IONBF, 0) < 0)
		return 1;
	for (long i = 1, d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "rb");
		if (!f)
			return 2;
		while (fscanf(f, "%ld,", &d) > 0)
			m[prog++] = d;
		if (fclose(f) < 0)
			return 3;
	}
	for (pc = 0; pc < 32768;) {
		u16 a = m[pc++], b = m[pc++], c = m[pc++];
		if (a == n) {
			m[b] = getchar();
		} else if (b == n) {
			if (putchar(m[a]) < 0)
				return 3;
		} else {
			u16 r = m[b] - m[a];
			if (r == 0 || r & 32768)
				pc = c;
			m[b] = r;
		}
	}
	return 0;
}
