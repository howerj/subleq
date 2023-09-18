#include <stdint.h>
#include <stdio.h>

#define likely(x)       __builtin_expect(!!(x), 1)
#define unlikely(x)     __builtin_expect(!!(x), 0)

#define ALIGN _Alignas(uint64_t)

typedef uint16_t u16;

//#define UNROLL
#define GLOBAL
#define LOAD (2)

#ifdef GLOBAL
static u16 ALIGN m[1 << 16];
//static u16 pc = 0, prog = 0;
#endif

int main(int argc, char **argv) {
	static const u16 n = -1;
#ifndef GLOBAL
	static u16 ALIGN m[1 << 16];
#endif
	unsigned pc = 0, prog = 0;
	for (long i = 1, d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "rb");
		if (!f)
			return 1;
		while (fscanf(f, "%ld,", &d) > 0)
			m[prog++] = d;
		if (fclose(f) < 0)
			return 2;
	}
#ifdef UNROLL
#pragma GCC unroll 32
	for (pc = 0;pc < 32768;) {
#else
	for (pc = 0;;) {
#endif

#if LOAD == 4
		const unsigned c = m[pc+2];
		const unsigned ab = *((uint32_t*)&m[pc+0]); /* non-portable, obviously */
		const unsigned a = (ab >> 00) & 0xFFFF;
		const unsigned b = (ab >> 16) & 0xFFFF;
#elif LOAD == 8
		const uint64_t abc = *((uint64_t*)&m[pc+0]);  /* non-portable, obviously */
		const unsigned a = (abc >> 00) & 0xFFFF;
		const unsigned b = (abc >> 16) & 0xFFFF;
		const unsigned c = (abc >> 32) & 0xFFFF;
#else
		const unsigned a = m[pc+0], b = m[pc+1], c = m[pc+2];
#endif
		if (likely(a != n && b != n)) {
			const unsigned r = m[b] - m[a];
			if (r == 0 || r & 32768) {
				pc = c;
#ifndef UNROLL
				if (pc & 32768)
					break;
#endif
			} else {
				pc += 3;
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
			pc += 3;
		} 
	}
	return 0;
}
