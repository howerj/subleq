#include <stdint.h>
#include <stdio.h>

typedef uint16_t u16;
static u16 m[1<<16], n[1<<16], prog = 0, oprog = 0;

int main(int argc, char **argv) {
	if (argc < 3)
		return 1;
	for (long i = 1, d = 0; i < (argc - 1); i++) {
		FILE *f = fopen(argv[i], "rb");
		if (!f)
			return 2;
		while (fscanf(f, "%ld,", &d) > 0)
			m[prog++] = d;
		if (fclose(f) < 0)
			return 3;
	}

	for (long i = 0; i < prog; i++) {
		u16 o = m[i];
		if (o == 0) {
			n[oprog++] = m[++i];
		} else if (o & 0x8000) {
			o -= 0x8000;
			for (long j = 0; j < o; j++)
				n[oprog++] = 0;
		} else {
			n[oprog++] = o;
		}
	}

	FILE *f = fopen(argv[argc - 1], "wb");
	if (!f)
		return 4;
	for (long i = 0; i < oprog; i++)
		if (fprintf(f, "%d\n", (int)(int16_t)n[i]) < 0)
			return 5;
	const double percent = (double)oprog / (double)prog * 100.0;
	const int diff = (prog - oprog)*2;
	if (fprintf(stderr, "in=%d out=%d diff=%d ratio=%.3f%%\n", (int)prog*2, (int)oprog*2, diff, percent) < 0)
		return 6;
	return 0;
}
