#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <limits.h>

#define SZ       (65536u/2u)
#define ALL_SET  ((subleq_cell_t)~0ul)
#define HIGH_SET ((subleq_cell_t)(1ul << ((sizeof(subleq_cell_t) * CHAR_BIT) - 1ul)))

typedef uint16_t subleq_cell_t;

typedef struct {
	subleq_cell_t pc, m[SZ];
	int (*put)(void *out, int ch);
	int (*get)(void *in);
	void *in, *out;
} subleq_t;

static inline subleq_cell_t load(subleq_t *s, const subleq_cell_t loc) {
	assert(s);
	return loc < SZ ? s->m[loc] : 0;
}

static inline int store(subleq_t *s, const subleq_cell_t loc, const subleq_cell_t val) {
	assert(s);
	const int lz = loc < SZ;
	if (lz)
		s->m[loc] = val;
	return lz ? 0 : -1;
}

static int subleq(subleq_t *s, const uint64_t cycles, const int trace) {
	assert(s);
	subleq_cell_t pc = s->pc;
	for (uint64_t i = 0; (i < cycles || cycles == 0) && !(pc & HIGH_SET); i++) {
		const subleq_cell_t a = load(s, pc + 0);
		const subleq_cell_t b = load(s, pc + 1);
		const subleq_cell_t c = load(s, pc + 2);
		subleq_cell_t next = pc + 3;
		if (a == ALL_SET) {
			if (store(s, b, s->get(s->in)) < 0)
				return -1;
		} else if (b == ALL_SET) {
			if (s->put(s->out, load(s, a)) < 0)
				return -2;
		} else {
			const subleq_cell_t la = load(s, a);
			const subleq_cell_t lb = load(s, b);
			const subleq_cell_t r = lb - la;
			if (store(s, b, r) < 0)
				return -3;
			if (r == 0u || r & HIGH_SET)
				next = c;
		}
		if (next >= SZ)
			next = -1;
		if (trace) {
			if (fprintf(stderr, "%d %d %d %d\n", pc, a, b, c) < 0)
				return -4;
		}
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
	if (argc < 2) {
		(void)fprintf(stderr, "usage: %s file.dec...\n", argv[0]);
		return 1;
	}
	size_t pc = 0;
	for (int i = 1; i < argc; i++) {
		FILE *program = fopen(argv[i], "rb");
		if (!program) {
			(void)fprintf(stderr, "load failed -- %s\n", argv[1]);
			return 2;
		}
		for (; pc < SZ; pc++) {
			long l = 0;
			if (fscanf(program, "%ld", &l) != 1)
				break;
			s.m[pc] = l;
		}
		if (fclose(program) < 0)
			return 3;
	}
	return subleq(&s, 0, 0) < 0 ? 1 : 0;
}

