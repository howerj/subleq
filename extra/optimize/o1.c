#include <stdint.h>
#include <stdio.h>

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)

typedef uint16_t u16;
static u16 m[1 << 16], pc = 0, prog = 0;

int main(int argc, char **argv) {
	static const u16 n = -1;
	for (long i = 1, d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "rb");
		if (!f)
			return 1;
		while (fscanf(f, "%ld,", &d) > 0)
			m[prog++] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (pc = 0;;) {
		u16 a = m[pc++], b = m[pc++], c = m[pc++];
		if (likely(a != n && b != n)) {
			u16 r = m[b] - m[a];
			if (r == 0 || r & 32768) {
				pc = c;
				if (pc & 32768)
					break;
			}
			m[b] = r;
		} else {
			if (a == n) {
				m[b] = getchar();
			} else {
				if (putchar(m[a]) < 0)
					return 3;
				if (fflush(stdout) < 0)
					return 4;
			}
		} 
	}
	return 0;
}
