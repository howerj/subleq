#include <assert.h>
#include <stdint.h>
#include <stdio.h>

#define SZ (8192)

typedef struct {
	int16_t pc, m[SZ];
	int (*put)(void *out, int ch);
	int (*get)(void *in);
	void *in, *out;
} subleq_t;

static inline int load(subleq_t *s, const unsigned loc) {
	assert(s);
	assert(SZ < INT16_MAX);
	if (loc < SZ)
		return s->m[loc];
	return loc == (SZ + 0) ? s->get(s->in) : 0;
}

static inline int store(subleq_t *s, const unsigned loc, const int val) {
	assert(s);
	assert(SZ < (1l + INT16_MAX));
	if (loc < SZ)
		s->m[loc] = val;
	return loc == (SZ + 1) ? s->put(s->out, val) : 0;
}

static int subleq(subleq_t *s, const uint64_t cycles, const int trace) {
	assert(s);
	long pc = s->pc;
	for (uint64_t i = 0; (i < cycles || cycles == 0) && pc >= 0; i++) {
		const int a = load(s, pc + 0);
		const int b = load(s, pc + 1);
		const int c = load(s, pc + 2);
		long next = pc + 3;
		const int la = load(s, a);
		const int lb = load(s, b);
		const long r = (long)lb - (long)la;
		if (store(s, b, r) < 0)
			return -1;
		if (r <= 0)
			next = c;
		if (next >= SZ)
			next = -1;
		if (trace)
			if (fprintf(stderr, "(pc=%04lx a=%04x b=%04x c=%04x la=%04x lb=%04x r=%04lx)\n", pc, a, b, c, la, lb, r) < 0)
				return -1;
		pc = next;
	}
	s->pc = pc;
	return 0;
}

static int put(void *out, const int ch) {
	assert(out);
	return (fputc(ch, out) < 0 || fflush(out) < 0) ? -1 : 0;
}

static int get(void *in) {
	assert(in);
	return fgetc(in);
}

int main(int argc, char **argv) {
	subleq_t s = { .get = get, .put = put, .in = stdin, .out = stdout, };
	if (argc != 2) {
		(void)fprintf(stderr, "usage: %s file.bin\n", argv[0]);
		return 1;
	}
	FILE *program = fopen(argv[1], "rb");
	if (!program) {
		(void)fprintf(stderr, "load failed -- %s\n", argv[1]);
		return 2;
	}
	for (size_t i = 0; i < SZ; i ++) {
		const int v1 = fgetc(program);
		const int v2 = fgetc(program);
		if (v1 < 0 || v2 < 0)
			break;
		s.m[i] = (v1 << 0) | (v2 << 8);
	}
	if (fclose(program) < 0)
		return 3;
	return subleq(&s, 0, 0) < 0 ? 1 : 0;
}

