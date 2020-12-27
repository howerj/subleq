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

static int subleq(subleq_t *s, const long cycles) {
	assert(s);
	int16_t pc = s->pc, *const m = s->m;
	for (long i = 0; (i < cycles || cycles <= 0) && pc >= 0; i++) {
		const int a = m[(pc + 0) % SZ], b = m[(pc + 1) % SZ], c = m[(pc + 2) % SZ];
		int16_t next = pc + 3;
		if (a < 0) {
			m[b] = s->get(s->in);
		} else if (b < 0) {
			(void)s->put(s->out, a);
		} else {
			const int r = m[b % SZ] - m[a % SZ];
			m[b % SZ] = r;
			if (r <= 0)
				next = c;
		}
		if (next >= SZ)
			next = -1;
		pc = next;
	}
	s->pc = pc;
	return 0;
}

static int put(void *out, int ch) { assert(out); return fputc(ch, out); }
static int get(void *in)          { assert(in);  return fgetc(in); }

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
		s.m[i] = (v1 << 8) | (v2 << 0);
	}
	if (fclose(program) < 0)
		return 3;
	return subleq(&s, 0);
}

