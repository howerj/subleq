#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <assert.h>

#define MAX_LABELS (256u)
#define MAX_HOLES  (512u)
#define MAX_NAME   (16u)

typedef struct {
	char name[MAX_NAME];
	uint64_t location;
	unsigned used:1, located:1;
} label_t;

typedef struct {
	label_t lb[MAX_LABELS];
	label_t holes[MAX_HOLES];
	uint64_t m[65536], max, pc;
	FILE *in, *out;
} asm_t;

int add_label(asm_t *a, const char *name, uint64_t location, int located) {
	assert(a);
	if ((strlen(name) + 1) > MAX_NAME)
		return -1;
	for (size_t i = 0; i < MAX_LABELS; i++) {
		label_t *lb = &a->lb[i];
		if (lb->used == 0) {
			lb->used = 1;
			strcpy(lb->name, name);
			lb->location = location;
			lb->located = located;
			return 0;
		}
	}
	return -2;
}

int add_hole(asm_t *a, const char *name, uint64_t location) {
	assert(a);
	// Attempt resolve, if not found, add hole
	return 0;
}

int resolve(asm_t *a) {
	assert(a);
	return 0;
}

static int cmd(asm_t *a) {
	assert(a);
	char name[MAX_NAME] = {0,};
	int i = 0;
	/* Expr, Sep, Newline/';', EOF, */

	for (int ch = 0;(ch = fgetc(a->in)) != EOF;) {
		if (ch == ';' || ch == '\n') {
			a->pc++;
			i = 0;
			// TODO: Output instruction
		} else if (ch == ':') {
		} else if (ch == '.') {
		} else if (ch == '#') {
			while ((ch = fgetc(a->in)))
				if (ch == '\n' || ch == EOF)
					break;
			if (ch == EOF) {
				// TODO: Final instruction
				return 0;
			}
		} else if (ch == '(') {
		} else {
		}
	}


	return 0;
}

static int assemble(asm_t *a) {
	assert(a);
	assert(a->in);
	for (char line[512]={0,};fgets(line, sizeof (line), a->in);) {
	}
	return 0;
}

int main(int argc, char **argv) {
	asm_t a = { .m = {0, }, .in = stdin, .out = stdout, };
	if (argc != 1) {
		(void)fprintf(stderr, "Usage: %s < file.asq > file.dec\n", argv[0]);
		return 1;
	}
	return assemble(&a);
}
