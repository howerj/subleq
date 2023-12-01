#include "c.h"

static u16 m[1<<16], n[1<<16], prog = 0, oprog = 0;

int main(int argc, char **argv) {
	if (argc < 4)
		return 1;
	const int t = mode(argv[1]);
	if (t < 0)
		return 2;
	if (load(m, &prog, argc - 3, &argv[2]) < 0)
		return 3;
	if (t) {
		for (long i = 0; i < prog; i++) {
			u16 o = m[i], q = m[i + 1];
			if (0x8000 & o) {
				n[oprog++] = 0xFFFF;
				n[oprog++] = m[i];
			} else if (o < 128 && q < 256) {
				n[oprog++] = 0x8000 + (o << 8) + q;
				i++;
			} else {
				n[oprog++] = o;
			}
		}
	} else {
		for (long i = 0; i < prog; i++) {
			u16 o = m[i];
			if (o == 0xFFFF) {
				n[oprog++] = m[++i];
			} else if (o & 0x8000) {
				o -= 0x8000;
				n[oprog++] = (o >> 8) & 127;
				n[oprog++] = o & 255;
			} else {
				n[oprog++] = o;
			}
		}
	}
	if (save(n, oprog, argv[argc - 1]) < 0)
		return 4;
	if (stats(stderr, prog, oprog) < 0)
		return 5;
	return 0;
}

