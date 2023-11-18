#include <stdint.h>
#include <stdio.h>

#define SZ   (32768)

uint16_t m[SZ] = {
#include "subleq-dec.c"
};

#define L(X) ((X)%SZ)

int main(int s, char **v) {
	uint16_t pc = 0;
	for (pc = 0; pc < SZ;) {
		uint16_t a = m[L(pc++)], b = m[L(pc++)], c = m[L(pc++)];
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
