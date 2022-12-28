/* N-Bit SUBLEQ machine, Twos compliment. Richard James Howe */
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <inttypes.h>
#define SZ     (1<<16)
#define L(X)   ((X)%SZ)
#define HI(X)  (1ull << ((X) - 1))

static inline uint64_t msk(int n) {
	return n < 64 ? 
		(1ull << n) + 0xFFFFFFFFFFFFFFFFull :
		0xFFFFFFFFFFFFFFFFull;
}

int main(int argc, char **argv) {
	if (argc < 2)
		return 1;
	static uint64_t m[SZ];
	uint64_t pc = 0, N = atoi(argv[1]);
	if (N < 8 || N > 64)
		return 2;
	for (long i = 2, d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f)
			return 3;
		while (fscanf(f, "%ld", &d) > 0)
			m[L(pc++)] = ((int64_t)d) & msk(N);
		if (fclose(f) < 0)
			return 4;
	}
	for (pc = 0; pc < SZ && pc != msk(N);) {
		uint64_t a = m[L(pc++)], 
			 b = m[L(pc++)], 
			 c = m[L(pc++)];
		if (a == msk(N)) {
			m[L(b)] = getchar() & msk(N);
		} else if (b == msk(N)) {
			if (putchar(m[L(a)]) < 0)
				return 5;
			if (fflush(stdout) < 0)
				return 6;
		} else {
			uint64_t r = m[L(b)] - m[L(a)];
			r &= msk(N);
			if (r & HI(N) || r == 0)
				pc = c;
			m[L(b)] = r;
		}
	}
	return 0;
}
