#include <stdint.h>
#include <stdio.h>
int main(int a, char **as) {
	typedef uint16_t v;
	v m[1<<16], A = 0, n = -1;
	for (int i = 1, d = 0; i < a; i++) {
		FILE *f = fopen(as[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%d,", &d) > 0)
			m[A++] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (A = 0; A < 32768;) {
		v a = m[A++], b = m[A++], c = m[A++];
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
				A = c;
			m[b] = r;
		}
	}
	return 0;
}
