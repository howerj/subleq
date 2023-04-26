#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#if defined(LIKELY)
#define unlikely(EXPR) __builtin_expect(!!(EXPR), 0)
#define likely(EXPR) __builtin_expect(!!(EXPR), 1)
#else
#define unlikely(EXPR) (EXPR)
#define likely(EXPR) (EXPR)
#endif
#define always __attribute__((always_inline))

typedef uint16_t v;
static v m[1<<16], A = 0, n = -1, end = 0;

static inline void always step(void) {
	v a = m[A++], b = m[A++], c = m[A++];
	if (unlikely(a == n)) {
		m[b] = getchar();
	} else if (unlikely(b == n)) {
		if (putchar(m[a]) < 0)
			exit(3);
		if (fflush(stdout) < 0)
			exit(4);
	} else {
		v r = m[b] - m[a];
		if (r & 32768 || r == 0)
			A = c;
		m[b] = r;
	}
	end |= A >= 32768;
}

#define S0 step();
#define S1 S0 S0
#define S2 S1 S1
#define S3 S2 S2
#define S4 S3 S3
#define S5 S4 S4
#define S6 S5 S5
#define S7 S6 S6
#define S8 S7 S7

int main(int a, char **as) {
	for (int i = 1, d = 0; i < a; i++) {
		FILE *f = fopen(as[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%d,", &d) > 0)
			m[A++] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (A = 0; !end;) {
		S0
	}
	return 0;
}
