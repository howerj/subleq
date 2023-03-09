/* This short program uses Less-Than-Or-Equal-To-Zero
 * to create the various signed comparison operators.
 *
 * The reason for this program is to understand how
 * those operators can be implemented on a SUBLEQ
 * One Instruction Set Computer.
 *
 * It is trivial to construct addition, subtraction
 * and branching with the SUBLEQ machine, but the
 * comparison and bitwise operators are more complex. */

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

#define NELEMS(X) (sizeof((X)) / sizeof((X)[0]))

typedef int (*cmp_fn)(uint16_t a, uint16_t b);

typedef struct {
	const char *name;
	cmp_fn original, subleq;
} cmp_t;

int o_more(uint16_t a, uint16_t b) { return (int16_t)a > (int16_t)b; }
int o_less(uint16_t a, uint16_t b) { return (int16_t)a < (int16_t)b; }
int o_eq(uint16_t a, uint16_t b) { return a == b; }

int leq0(uint16_t a) {
	return ((int16_t)a) <= (int16_t)0;
}

int s_less(uint16_t a, uint16_t b) {
	const int a0 = leq0(a);
	const int b0 = leq0(b);
	if (a0 && !b0)
		return 1;
	if (!a0 && b0)
		return 0;
	if (a0 && b0) {
		if (!leq0(a + 1) && leq0(b + 1))
			return 0;
	}
	const int l = leq0((uint16_t)(a - b));
	return l ? leq0((uint16_t)((a + 1) - b)) : 0;
}

int s_more(uint16_t a, uint16_t b) {
	return s_less(b, a);
}

int s_eq(uint16_t a, uint16_t b) {
	return !s_more(a, b) && !s_less(a, b);
}

static const char *yn(int y) { return y ? "YES" : " NO"; }

static int number(const char *n) { return strtol(n, NULL, 0); }

static cmp_t  compares[] = {
//	{ .name = " <", .original = o_less, .subleq = s_less, },
	{ .name = " >", .original = o_more, .subleq = s_more, },
	{ .name = "==", .original = o_eq,   .subleq = s_eq,   },
};
static const size_t len = NELEMS(compares);

static int test(cmp_t *c, uint16_t a, uint16_t b, int print) {
	const char *name = c->name;
	const int r_orig   = c->original(a, b);
	const int r_subleq = c->subleq(a, b);
	const int same = r_orig == r_subleq;
	if (print || !same) {
		const char *yes = yn(same);
		const int r = fprintf(stdout, "%d %s %d = %s : subleq(%d) orig(%d)\n", (int)(int16_t)a, name, (int)(int16_t)b, yes, r_subleq, r_orig);
		if (r < 0)
			return -1;
	}
	return 0;
}

int main(int argc, char **argv) {
	if (argc == 1) {
		for (size_t i = 0; i < len; i++) {
			cmp_t *c = &compares[i];
			uint32_t cnt = 0;
			do {
				uint32_t a = cnt & 0xFFFF, b = (cnt >> 16) & 0xFFFFu;
				if (test(c, a, b, 0) < 0)
					return 1;
			} while (cnt++ != 0xFFFFFFFFul);
		}
	}
	if (argc != 3)
		return 2;
	uint16_t a = number(argv[1]), b = number(argv[2]);

	for (size_t i = 0; i < len; i++) {
		cmp_t *c = &compares[i];
		if (test(c, a, b, 1) < 0)
			return 3;
	}

	return 0;
}
