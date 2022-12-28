#include <stdint.h>
#include <stdio.h>
#define SZ   (1<<16)
#define L(X) ((X)%SZ)
int main(int argc, char **argv) {
	static uint16_t m[SZ];
	uint16_t pc = 0;
	for (int i = 1, d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%d", &d) > 0)
			m[L(pc++)] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (pc = 0; !(pc & 0x8000u);) {
		uint16_t a = m[L(pc++)];
		uint16_t b = m[L(pc++)];
		uint16_t c = m[L(pc++)];
		if (a == 65535) {
			m[L(b)] = getchar();
		} else if (b == 65535) {
			if (putchar(m[L(a)]) < 0)
				return 3;
			if (fflush(stdout) < 0)
				return 4;
		} else {
			uint16_t r = m[L(b)] - m[L(a)];
			if (r & 32768 || r == 0)
				pc = c;
			m[L(b)] = r;
		}
	}
	return 0;
}
