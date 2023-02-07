#include <assert.h>
#include <ctype.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SZ (1<<16)
#define MAX_LABELS (512u)
#define MAX_HOLES  (512u)
#define MAX_COPY   (512u)
#define MAX_NAME   (16u)

typedef struct {
	char name[MAX_NAME];
	uint64_t location;
	unsigned used:1, operation:8;
} label_t;

typedef struct {
	label_t lb[MAX_LABELS];
	label_t holes[MAX_HOLES];
	uint64_t from[MAX_COPY], to[MAX_COPY];
	uint64_t m[SZ], max, pc, copies;
	FILE *in, *out, *err;

	int token, unget, error, line;
	char name[MAX_NAME];
	uint64_t number;
} asm_t;

static int find_label(label_t *lbs, size_t labels, const char *name, uint64_t *loc)  {
	assert(lbs);
	if (loc)
		*loc = 0;
	if ((strlen(name) + 1) > MAX_NAME)
		return -2;
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

static int validop(int operation) {
	switch (operation) {
	case '+': return 1;
	case '-': return 1;
	case '=': return 1;
	}
	return 0;
}

static int add(label_t *lbs, size_t lables, const char *name, uint64_t location, int unique, int operation) {
	assert(lbs);
	if ((strlen(name) + 1) > MAX_NAME)
		return -1;
	if (!validop(operation))
		return -2;
	if (unique)
		if (find_label(lbs, lables, name, NULL) >= 0)
			return -3;
	for (size_t i = 0; i < lables ; i++) {
		label_t *lb = &lbs[i];
		if (lb->used == 0) {
			lb->used = 1;
			strcpy(lb->name, name);
			lb->location = location;
			lb->operation = operation;
			return 0;
		}
	}
	return -4;
}

static int error(asm_t *a, const char *fmt, ...) {
	assert(a);
	va_list ap;
	va_start(ap, fmt);
	(void)fprintf(a->err, "Error (line %d): ", a->line);
	(void)vfprintf(a->err, fmt, ap);
	(void)fputc('\n', a->err);
	va_end(ap);
	a->error = -1;
	return -1;
}

static int add_error(asm_t *a, const char *name, int r) {
	assert(a);
	assert(name);
	const char *msg = NULL;
	switch (r) {
	case -1: msg = "too long"; break;
	case -2: msg = "invalid operation"; break;
	case -3: msg = "label already exists"; break;
	case -4: msg = "no more spare labels"; break;
	default: if (r < 0) msg = "unknown"; break;
	}
	if (msg)
		return error(a, "Adding location '%s' failed: %s", name, msg);
	return r;
}

static int add_label(asm_t *a, const char *name, uint64_t location) {
	assert(a);
	assert(name);
	return add_error(a, name, add(a->lb, MAX_LABELS, name, location, 1, '='));
}

static int add_hole(asm_t *a, const char *name, uint64_t location, int operation) {
	assert(a);
	assert(name);
	return add_error(a, name, add(a->holes, MAX_HOLES, name, location, 0, operation));
}

static int add_copy(asm_t *a, uint64_t from, uint64_t to) {
	assert(a);
	if (a->copies >= MAX_COPY)
		return error(a, "Too many copies");
	a->from[a->copies] = from;
	a->to[a->copies] = to;
	a->copies++;
	return 0;
}

static int copy(asm_t *a) {
	assert(a);
	assert(a->copies <= MAX_COPY);
	for (size_t i = 0; i < a->copies; i++)
		a->m[a->to[i]] = a->m[a->from[i]];
	return 0;
}

static int resolve(asm_t *a) {
	assert(a);
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
					uint64_t val = lb->location;
					assert(validop(hole->operation));
					switch (hole->operation) {
					case '=': a->m[hole->location]  = val; break;
					case '+': a->m[hole->location] += val; break;
					case '-': a->m[hole->location] -= val; break;
					default: return -1;
					}

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

static int dump(asm_t *a, int mod3) {
	assert(a);
	for (size_t i = 0; i < a->pc; i++) {
		if (mod3 && i && (i % 3) == 0)
			if (fprintf(a->out, "\n") < 0)
				return -1;
		if (fprintf(a->out, "%ld%c", (long)a->m[i], mod3 ? ' ' : '\n') < 0)
			return -1;
	}
	if (fprintf(a->out, "\n") < 0)
		return -1;
	return 0;
}

static char *strset(char *s, char *set) {
	assert(s);
	assert(set);
	int sch = 0, tch = 0;
	for (size_t i = 0; (sch = s[i]); i++)
		for (size_t j = 0; (tch = set[j]); j++)
			if (sch == tch)
				return &s[i];
	return NULL;
}

enum {  
	ERROR = -'X',
	EOI = 'E', EOL = 'L', 
	DOT = '.', LPAR = '(', RPAR = ')', SEP = ' ', COMMA = ',', 
	SEMI = ';', PLUS = '+', MINUS = '-', NUMBER = '0', LABEL = 'N',  COLON = ':', QUESTION = '?', AT = '@',  
};

static int inner(asm_t *a) {
	assert(a);
	if (a->unget) {
		a->unget = 0;
		return a->token;
	}
	int ch = 0;
	a->number = 0;
	memset(a->name, 0, sizeof(a->name));
//again:
	switch ((ch = fgetc(a->in))) {
	case ' ': case '\t': 
		while (ch == ' ' || ch == '\t') {
			ch = fgetc(a->in);
			if (ch == EOF)
				return EOI;
		}
		if (ungetc(ch, a->in) < 0)
			return error(a, "ungetc failed");
		return SEP;
	//	goto again;
	case '#':
		while (ch != '\n') {
			ch = fgetc(a->in);
			if (ch == EOF)
				return EOI;
		}
		/* fall-through */
	case '\n': a->line++; return EOL;
       	case ';': return SEMI; case '.': return DOT;
	case '@': return AT; case '?': return QUESTION;
	case '+': return PLUS; case '-': return MINUS;
	case '(': return LPAR; case ')': return RPAR;
	case EOF: return EOI;
	default:
		if (isalnum(ch)) {
			int number = isdigit(ch);
			int colon = 0;
			a->name[0] = ch;
			for (size_t i = 1;;i++) {
				if (i >= (sizeof(a->name) - 1)) {
					a->error = -1;
					return ERROR;
				}
				ch = fgetc(a->in);
				if (ch == EOF)
					break;
				int check = number ? isdigit(ch) : isalnum(ch); 
				if (!check) {
					if (!number && ch == ':') {
						colon = 1;
					} else if (ungetc(ch, a->in) < 0) {
						return error(a, "ungetc failed");
					}
					break;
				}
				if (!colon)
					a->name[i] = ch;
			}
			if (number)
				a->number = strtol(a->name, NULL, 0);
			/* key words, if any, would go here...*/
			return number ? NUMBER : (colon ? COLON : LABEL);
		} else {
			char c[2] = { ch, };
			char *e = isgraph(ch) ? c : "non-graphic";
			return error(a, "Invalid token character: %d:'%s'", ch, e);
		}
	}
	return error(a, "Unknown error");
}

static int lexer(asm_t *a) {
	assert(a);
	if (a->error)
		return -1;
	int r = inner(a);
	if (r < 0)
		a->error = -1;
	a->token = r;
	/*(void)fprintf(a->err, "token:%d/%c\n", r, r);*/
	return r;
}

static int unget(asm_t *a) {
	assert(a);
	if (a->unget)
		return error(a, "unget twice");
	a->unget = 1;
	return 0;
}

static int end(int token) {
	return token == EOL || token == SEMI || token == EOI;
}

static int assemble(asm_t *a) {
	assert(a);

	int dot = 0, cnt = 0;
	uint64_t v[3] = { 0, };
	for (;a->error == 0;) {
		int t = lexer(a);
		if (t == DOT) { dot = 1; continue; }
		if (t == EOL) { dot = 0; }
		if (end(t)) {
			switch (cnt) {
			case 0: break;
			case 1: 
				a->m[a->pc + 0] = v[0];
				a->m[a->pc + 1] = v[0];
				a->m[a->pc + 2] = a->pc + 3;
				if (add_copy(a, a->pc + 0, a->pc + 1) < 0)
					return -1;
				a->pc += 3;
				break;
			case 2: 
				a->m[a->pc + 0] = v[0];
				a->m[a->pc + 1] = v[1];
				a->m[a->pc + 2] = a->pc + 3;
				a->pc += 3;
				break;
			case 3: 
				a->m[a->pc + 0] = v[0];
				a->m[a->pc + 1] = v[1];
				a->m[a->pc + 2] = v[2];
				a->pc += 3;
				break;
			}
			memset(v, 0, sizeof (v));
			a->number = 0;
			cnt = 0;
			if (t == EOI) { break; }
			continue;
		}

		if (t == COLON) {
			/* TODO: In debugger, use names of symbols for breakpoints and tracing */
			/*printf("%s:%d\n", name, (int)(a->pc + cnt));*/
			if (add_label(a, a->name, a->pc + cnt) < 0)
				return -1;
			continue;
		}

		uint64_t n = 0;
		int op = '+';
	again:
		if (t == MINUS || t == PLUS) {
			if (t == MINUS)
				op = '-';
			t = lexer(a);
		}

		if (t == NUMBER) {
			n = n + (op == '+' ? a->number : -a->number);
		} else if (t == LABEL) {
			uint64_t ll = 0;
			if (find_label(a->lb, MAX_LABELS, a->name, &ll) < 0) {
				if (add_hole(a, a->name, a->pc + cnt, op) < 0)
					return -1;
			} else {
				n = n + (op == '+' ? ll : ll);
			}
		} else if (t == QUESTION) {
			uint64_t val = a->pc + cnt + 1;
			n = n + (op == '+' ? val : -val);
		} else if (t == AT) {
			uint64_t val = a->pc - (dot ? 0 : (a->pc % 3));
			n = n + (op == '+' ? val : -val);
		} else if (t == SEP) {
			continue;
		} else {
			return error(a, "Invalid token %d/%c", t, t);
		}

		t = lexer(a);
		if (t == PLUS || t == MINUS) {
			goto again;
		} else {
			unget(a);
		}

		if (dot) {
			a->m[a->pc++] = n;
		} else {
			if (cnt >= 3)
				return error(a, "Too many operands per instruction");
			v[cnt++] = n;
		}
	}
	if (a->error)
		return error(a, "Assembly failed");
	if (resolve(a) < 0)
		return error(a, "Resolving labels failed");
	if (copy(a) < 0)
		return error(a, "Copying locations failed");
	return dump(a, 1);
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
* A special label identifier, such as '?' or '@'.\n\
* Simple expressions with '+' and '-' are allowed.\n\
\n\
Instructions consist of three numbers or expressions. They\n\
can either be explicitly stated or the last two can be missing.\n\
If there are only two operands are given then the jump destination\n\
will be the next instruction, if the last two are missing the\n\
second will be a copy of the first and the third a jump to\n\
the next instruction.\n\
\n\
There are some things missing which would be useful for\n\
larger programs; entering string constants, macros, removal\n\
of memory limitations, etcetera. There is not much need for\n\
them though.\n\
";
	return fprintf(out, help, arg0);
}

int main(int argc, char **argv) {
	asm_t a = { .m = { 0, }, .in = stdin, .out = stdout, .err = stderr, };
	if (argc != 1) {
		(void)assembly_help(stderr, argv[0]);
		return 1;
	}
	return assemble(&a);
}
