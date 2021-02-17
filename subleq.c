#include <stdint.h>
#include <stdio.h>
int main(int s, char **v) {
	uint16_t m[65536], pc = 0;
	for (int i = 1, d = 0; i < s; i++) {
		FILE *f = fopen(v[i], "r");
		if (!f) return 1;
		while (fscanf(f, "%d", &d) > 0)
			m[pc++] = d;
		if (fclose(f) < 0) return 2;
	}
	for (pc = 0;!(pc & 32768);) {
		uint16_t a = m[pc++], b = m[pc++], c = m[pc++];
		if (a == 65535) {
			m[b] = getchar();
		} else if (b == 65535) {
			if (putchar(m[a]) < 0) return 3;
			if (fflush(stdout) < 0) return 4;
		} else {
			uint16_t r = m[b] - m[a];
			if (r & 32768 || r == 0) pc = c;
			m[b] = r;
		}
	}
	return 0;
}
