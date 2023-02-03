#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <assert.h>

#define SZ (1<<16)
#define MAX_LABELS (512u)
#define MAX_HOLES  (512u)
#define MAX_NAME   (16u)

typedef struct {
	char name[MAX_NAME];
	uint64_t location;
	unsigned used:1, negate:1;
} label_t;

typedef struct {
	label_t lb[MAX_LABELS];
	label_t holes[MAX_HOLES];
	uint64_t m[SZ], max, pc;
	FILE *in, *out;
} asm_t;

static int add_label(label_t *lbs, size_t lables, const char *name, uint64_t location) {
	assert(lbs);
	if ((strlen(name) + 1) > MAX_NAME)
		return -1;
	for (size_t i = 0; i < lables ; i++) {
		label_t *lb = &lbs[i];
		if (lb->used == 0) {
			lb->used = 1;
			strcpy(lb->name, name);
			lb->location += location;
			return 0;
		}
	}
	return -2;
}

static int find_label(label_t *lbs, size_t labels, const char *name, uint64_t *loc)  {
	assert(lbs);
	if (loc)
		*loc = 0;
	if ((strlen(name) + 1) > MAX_NAME)
		return -1;
	for (size_t i = 0; i < labels; i++) {
		label_t *lb = &lbs[i];
		if (lb->used != 0) {
			if (!strcmp(lbs->name, name)) {
				if (loc)
					*loc = lbs->location;
				return i;
			}
		}
	}
	return -1;
}

static int resolve(asm_t *a) {
	assert(a);
	/* TODO: Add "len" as special label? */

	for (size_t j = 0; j < MAX_HOLES; j++) {
		int found = 0;
		label_t *hole = &a->holes[j];
		if (hole->used == 0)
			continue;
		for (size_t i = 0; i < MAX_LABELS; i++) {
			label_t *lb = &a->lb[i];
			if (lb->used) {
				if (!strcmp(hole->name, lb->name)) {
					found = 1;
					/* add or subtract here? */
					a->m[hole->location] = lb->location;
					break;
				}
			}
			hole->used = 0;
		}
		if (!found)
			return -1;
	}
	return 0;
}

static int dump(asm_t *a) {
	assert(a);
	for (size_t i = 0; i < a->pc; i++)
		if (fprintf(a->out, "%ld\n", (long)a->m[i]) < 0)
			return -1;
	return 0;
}

static int assemble(asm_t *a) {
	assert(a);
	assert(a->in);
	for (char line[512]={0,};fgets(line, sizeof (line), a->in);) {
		int v[3] = { 0, }, s = 0, i = 0, done = 0, newline = 0, single = 0;
	again:
		for (int ch = 0;;i++) {
			assert(i < sizeof(line));
			if (s == 3 || (single && s == 1)) {
				done = 1;
				goto proc;
			}

			switch ((ch = line[i])) {
			case '.': single = 1; break;
			case ' ': case '\t': break;
			case 0: case '#': case '\n': done = 1; newline = 1; single = 0; goto proc;
			case ';': done = 1; newline = 0; goto proc;
			default: {
				/* TODO: Process [-+]<id>[-+]<id>...
				 * With special IDs '?', 'len', ... */
				/* TODO: Copy holes! */
				char name[512] = { 0, };
				int j = 0;
				if (line[i] == '+' || line[i] == '-')
					name[j++] = line[i++];
				for (int ch = 0; isalnum(line[i]); i++)
					name[j++] = line[i];
				if (isalpha(name[0])) { /* handle negation? */
					j = 0;
					while (isalnum(name[j++]))
						;
					if (line[i] == ':') {
						i++;
						if (find_label(a->lb, MAX_LABELS, name, NULL) >= 0) {
							(void)fprintf(stderr, "Label already exists: '%s'\n", name);
							return -1;
						}
						if (add_label(a->lb, MAX_LABELS, name, a->pc + s) < 0) {
							(void)fprintf(stderr, "Adding label '%s' failed\n", name);
							return -1;
						}
					} else {
						uint64_t loc = 0;
						if (find_label(a->lb, MAX_LABELS, name, &loc) >= 0) {
							v[s++] = loc;
						} else {
							if (add_label(a->holes, MAX_HOLES, name, a->pc + s) < 0) {
								(void)fprintf(stderr, "Adding hole '%s' failed\n", name);
								return -1;
							}
							v[s++] = 0;
						}
					}
				} else if (name[0] == '-' || name[0] == '+' || isdigit(name[0])) {
					char *end = NULL;
					long val = strtol(name, &end, 0);
					if (*end) {
						(void)fprintf(stderr, "Invalid number: '%s'\n", name);
						return -1;
					}
					v[s++] = val;
				} else {
					(void)fprintf(stderr, "Invalid identifier in line: '%s'\n", line);
					return -1;
				}
				break;
			}
			}
		}
	proc:
		if (!done)
			goto again;
		done = 0;
		assert(s <= 3);
		if (single) {
			if (s) {
				a->m[a->pc] = v[0];
				a->pc += 1;
			}

		} else {
			switch (s) {
			case 0: break;
			case 1: 
				a->m[a->pc+0] = v[0];
				a->m[a->pc+1] = v[0];
				a->m[a->pc+2] = a->pc + 3;
				a->pc += 3;
				break;
			case 2: 
				a->m[a->pc+0] = v[0];
				a->m[a->pc+1] = v[1];
				a->m[a->pc+2] = a->pc + 3;
				a->pc += 3;
				break;
			case 3: 
				a->m[a->pc+0] = v[0];
				a->m[a->pc+1] = v[1];
				a->m[a->pc+2] = v[2];
				a->pc += 3;
				break;
			}
		}
		s = 0;
		if (!newline)
			goto again;
	}
	if (resolve(a) < 0) {
		(void)fprintf(stderr, "Resolving labels failed\n");
		return -1;
	}
	return dump(a);
}

/* TODO: Example programs */
static int assembly_help(FILE *out, const char *arg0) {
	const char *help = "Usage: %s < file.asq > file.dec\n\n\
SUBLEQ ASSEMBLER. This is an assembler for a SUBLEQ Single Instruction\n\
machine. A SUBLEQ instruction consists of three operands; 'a', 'b'\n\
and 'c'.\n\
\n\
The assembler syntax is as follows:\n\n\
* '#' a comment until the end of the line.\n\
* number, a number compiled into the current instruction.\n\
* 'label', a reference to a label, the address of which\n\
will be filled into the current cell location.\n\
* 'label:', a label, forward references are allowed\n\
* '.', allows output of single cells and not instruction on\n\
the current line.\n\
* A special label identifier, such as '?' or 'len'.\n\
* Simple expressions with '+' and '-' are allowed.\n\
\n\
Instructions consist of three numbers or expressions. They\n\
can either be explicitly stated or the last two can be missing.\n\
If there are only two operands are given then the jump destination\n\
will be the next instruction, if the last two are missing the\n\
second will be a copy of the first and the third a jump to\n\
the next instruction.\n\
";
	return fprintf(out, help, arg0);
}

/* TODO: Help */
int main(int argc, char **argv) {
	asm_t a = { .m = {0, }, .in = stdin, .out = stdout, };
	if (argc != 1) {
		(void)assembly_help(stderr, argv[0]);
		return 1;
	}
	return assemble(&a);
}
