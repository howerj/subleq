#include <stdint.h>
#include <stdio.h>
#define SZ   (1<<16)
int main(int a, char **as) {
	typedef uint16_t v;
	v m[SZ], pc = 0, n = -1;
	for (int i = 1, d = 0; i < a; i++) {
		FILE *f = fopen(as[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%d,", &d) > 0)
			m[pc++] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (pc = 0; pc < (SZ/2);) {
		v a = m[pc++], b = m[pc++], c = m[pc++];
		if (a == n) {
			m[b] = getchar();
		} else if (b == n) {
			if (putchar(m[a]) < 0)
				return 3;
			if (fflush(stdout) < 0)
				return 4;
		} else {
			v r = m[b] - m[a];
			if (r & 32768 || r == 0)
				pc = c;
			m[b] = r;
		}
	}
	return 0;
}
