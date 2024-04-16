#include <stdint.h>
#include <stdio.h>
#include <assert.h>

typedef uint16_t u16;
static const u16 n = -1;
static u16 m[1<<13], pc = 0, prog = 0;
static u16 A(u16 x) { assert(x < (1<<13)); return x; }

int main(int argc, char **argv) {
	for (long i = 1, d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%ld,", &d) > 0)
			m[prog++] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (pc = 0; pc < 32768;) {
		u16 a = m[A(pc++)], b = m[A(pc++)], c = m[A(pc++)];
		if (a == n) {
			m[A(b)] = getchar();
		} else if (b == n) {
			if (putchar(m[A(a)]) < 0)
				return 3;
			if (fflush(stdout) < 0)
				return 4;
		} else {
			u16 r = m[A(b)] - m[A(a)];
			if (r == 0 || r & 32768)
				pc = c;
			m[A(b)] = r;
		}
	}
	return 0;
}
