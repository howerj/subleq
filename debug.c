/* This is a  MS-DOS DEBUG.EXE/DEBUG.COM like tool-chain for
 * a SUBLEQ One Instruction machine, containing a virtual
 * machine for that platform.
 *
 * See:
 *
 * - <https://montcs.bloomu.edu/Information/LowLevel/DOS-Debug.html> 
 * - <https://en.wikipedia.org/wiki/Debug_(command)> 
 * - <https://en.wikipedia.org/wiki/One-instruction_set_computer>
 * - <https://github.com/howerj/subleq> */
#define PROJECT "N-Bit SUBLEQ Toolchain and Virtual Machine"
#define AUTHOR  "Richard James Howe"
#define REPO    "https://github.com/howerj/subleq"
#define EMAIL   "howe.r.j.89@gmail.com"
#define LICENSE "Public Domain / Unlicense for code only"
#define VERSION "1.0"
/* TODO:
 * - I/O better escape handling, terminal options via CLI, retry option, retry if not Escape
 * - add named breakpoints to assembler
 * - Gather more information about running environment (which cells are modified, etc). */

#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <inttypes.h>
#include <limits.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>

#define MAX_LINE (512)
#define SZ   (1<<16)
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define HI(X)  (1ull << ((X) - 1))
#define UNUSED(X) ((void)(X))
#define NELEMS(X) (sizeof (X) / sizeof((X)[0]))

#ifdef _WIN32 /* Used to unfuck file mode for "Win"dows. Text mode is for losers. */
#include <windows.h>
#include <io.h>
#include <fcntl.h>
static void binary(FILE *f) { _setmode(_fileno(f), _O_BINARY); }
#else
static inline void binary(FILE *f) { UNUSED(f); }
#endif

/* A small, portable, terminal library would be nice, not something
 * as heavy as the standard ones, perhaps something like linenoise. */
#define ESCAPE    (27)
#define DELETE    (127)
#define BACKSPACE (8)
#ifdef __unix__
#include <unistd.h>
#include <termios.h>
typedef struct {
	struct termios oldattr, newattr;
	int fd, setup;
	FILE *in, *out;
} terminal_t;

static void term_restore(terminal_t *t) {
	assert(t);
	if (t->setup) {
		tcsetattr(t->fd, TCSANOW, &t->oldattr);
		t->setup = 0;
		t->fd = -1;
	}
}

/* Instead of the setup code, which is reliant on a global structure, we
 * could instead set and then restore the terminal surrounding calls to the
 * get character function... */

static int setup(terminal_t *t) {
	assert(t);
	if (t->setup)
		return 0;
	t->fd = fileno(t->in);
	if (!isatty(t->fd))
		return 0;
	tcgetattr(t->fd, &t->oldattr);
	t->newattr = t->oldattr;
	t->newattr.c_iflag &= ~(ICRNL);
	t->newattr.c_lflag &= ~(ICANON | ECHO);
	t->newattr.c_cc[VMIN]  = 0;
	t->newattr.c_cc[VTIME] = 0;
	tcsetattr(t->fd, TCSANOW, &t->newattr);
	t->setup = 1;
	return 0;
}

static int term_getch(terminal_t *t) {
	if (setup(t) < 0)
		return -1;
	if (!isatty(t->fd))
	       return fgetc(t->in);	
	unsigned char r = 0;
	if (read(t->fd, &r, 1) != 1)
		return -1;
	return r;
}

static int term_putch(terminal_t *t, int c) {
	const int r = fputc(c, t->out);
	if (fflush(t->out) < 0)
		return -1;
	return r;
}

static void term_sleep_ms(unsigned ms) {
	usleep((unsigned long)ms * 1000);
}
#else
#ifdef _WIN32
typedef struct { FILE *in, *out; } terminal_t;
extern int getch(void);
extern int putch(int c);
static int term_getch(terminal_t *t) { assert(t); assert(t->in); return t->in == stdin ? getch() : fgetc(t->in); }
static int term_putch(terminal_t *t, const int c, FILE *out) { UNUSED(t); return putch(c); }
static int term_restore(terminal_t *t) { UNUSED(t); return 0; }
static void term_sleep_ms(unsigned ms) { usleep((unsigned long)ms * 1000); }
#else
typedef struct { FILE *in, *out; } terminal_t;
static int term_getch(terminal_t *t) {  return fgetc(t->in); }
static int term_putch(term_putch *t, const int c) { return fputc(c, t->out); }
static int term_restore(terminal_t *t) { UNUSED(t); return 0; }
static void term_sleep_ms(unsigned ms) { (void)ms; }
#endif
#endif /** __unix__ **/

typedef struct {
	char *arg;   /* parsed argument */
	int index,   /* index into argument list */
	    option,  /* parsed option */
	    reset;   /* set to reset */
	char *place; /* internal use: scanner position */
	int  init;   /* internal use: initialized or not */
	FILE *error; /* turn error report on if not NULL */
} sys_getopt_t;   /* getopt clone; with a few modifications */

static int sys_is_number(const char *num, int is_unsigned, int base, long *out) {
	assert(num);
	char *end = NULL;
	enum { BAD_NUM = -'#', BAD_RANGE = -'~', BAD_BASE = -'b', };
	if (out)
		*out = 0;
	errno = 0;
	if ((base < 2 || base > 36) && base != 0)
		return BAD_BASE;
	if (is_unsigned) {
		unsigned long v = strtoul(num, &end, base);
		if (v == ULONG_MAX)
			if (errno == ERANGE)
				return BAD_RANGE;
		if (out)
			*out = v;
	} else {
		long v = strtol(num, &end, base);
		if (v == LONG_MAX || v == LONG_MIN) {
			if (errno == ERANGE)
				return BAD_RANGE;
		}
		if (out)
			*out = v;
	}
	if (*end)
		return BAD_NUM;
	return 0;
}

/* Adapted from: <https://stackoverflow.com/questions/10404448>,
 * NB. Parsing and checking for numeric input could be added
 * to this easily, as could other more advance argument processing.
 * This function could also be added to <https://github.com/howerj/pickle>,
 * it would make a good addition to a TCL like language. */
static int sys_getopt(sys_getopt_t *opt, const int argc, char *const argv[], const char *fmt) {
	assert(opt);
	assert(fmt);
	assert(argv);
	enum { BADARG_E = ':', BADCH_E = '?', BADIO_E = 'i', };

	if (!(opt->init)) {
		opt->place = ""; /* option letter processing */
		opt->init  = 1;
		opt->index = 1;
	}

	if (opt->reset || !*opt->place) { /* update scanning pointer */
		opt->reset = 0;
		if (opt->index >= argc || *(opt->place = argv[opt->index]) != '-') {
			opt->place = "";
			return -1;
		}
		if (opt->place[1] && *++opt->place == '-') { /* found "--" */
			opt->index++;
			opt->place = "";
			return -1;
		}
	}

	const char *oli = NULL; /* option letter list index */
	if ((opt->option = *opt->place++) == ':' || !(oli = strchr(fmt, opt->option))) { /* option letter okay? */
		 /* if the user didn't specify '-' as an option, assume it means -1.  */
		if (opt->option == '-')
			return -1;
		if (!*opt->place)
			opt->index++;
		if (opt->error && *fmt != ':')
			if (fprintf(opt->error, "Illegal option -- %c\n", opt->option) < 0)
				return BADIO_E;
		return BADCH_E;
	}

	if (*++oli != ':') { /* don't need argument */
		opt->arg = NULL;
		if (!*opt->place)
			opt->index++;
	} else {  /* need an argument */
		if (*opt->place) { /* no white space */
			opt->arg = opt->place;
		} else if (argc <= ++opt->index) { /* no arg */
			opt->place = "";
			if (*fmt == ':')
				return BADARG_E;
			if (opt->error)
				if (fprintf(opt->error, "Option requires an argument -- %c\n", opt->option) < 0)
					return BADIO_E;
			return BADCH_E;
		} else	{ /* white space */
			opt->arg = argv[opt->index];
		}
		opt->place = "";
		opt->index++;
	}
	return opt->option; /* dump back option letter */
}

/* NB. We could extend these routines to deal with format specifiers
 * and more options (such as floating point values, ranges of values,
 * etcetera). This would add more complexity. It might also be
 * worth adding something like this to the 
 * <https://github.com/howerj/pickle> project, a TCL like programming
 * language interpreter which could use something like this in validating
 * functions, along with the function for explaining those format specifier
 * strings. It might also be possible to make a simpler TCL like language
 * from the functions in here, there is a lot of short command line
 * processing functions. 
 *
 * This function could probably be simplified, as well as extended. 
 *
 * Possible improvements:
 *
 * - Argument annotations (instead of "decimal" expected, we could print
 *   "address" expected in the error reporting).
 * - Range checking (bytes, shorts, specific ranges, ...)
 * - More format types (floats, enums, ...) */
static int args_validate(const char *fmt, int argc, char **argv) {
	assert(fmt);
	int i = -1;
	enum { BAD_ARGC = -'A', BAD_ARGV = -'V', BAD_NUM = -'#', BAD_FMT = -'F', };
	if (argc < 1 || !argv)
		return BAD_ARGV;
retry:
	if (i != -1) {
		char *n = strchr(fmt, '|');
		if (!n)
			return BAD_ARGC;
		fmt = n + 1;
	}
alternative:
	i = 0;
	for (int ch = 0; (ch = *fmt++); i++) {
		if (ch == '|') { if (i >= argc) return 0; goto alternative; }
		if (ch == '*') { if (*fmt != '\0' && *fmt != '|') return BAD_FMT; return 0; }
		if (i >= argc) goto retry;
		switch (ch) {
		case 's': break;
		case 'c': if (strlen(argv[i]) != 1) goto retry; break;
		case 'u': if (sys_is_number(argv[i], 1, 0, NULL) != 0) goto retry; break;
		case 'n': if (sys_is_number(argv[i], 0, 0, NULL) != 0) goto retry; break;
		/* NB. We could turn this into a regex over the arguments, of
		 * sorts, a "+" would check that the next arguments were same
		 * as the previous ones, "*" would do the same but for zero or
		 * more arguments. "s" would be the stand-in for "." */
		case '+': if (*fmt != '\0' && *fmt != '|') return BAD_FMT; return 0;
		default: return BAD_FMT;
		}
	}
	if (i != argc)
		return BAD_ARGC;
	return 0;
}

static int args_explain(FILE *out, const char *fmt, int argc, char **argv) {
	assert(out);
	assert(fmt);
	if (argc < 1 || !argv)
		return -1;
	if (*fmt++ != 's')
		return -1;
	if (fprintf(out, "Usage: %s ", argv[0]) < 0)
		return -1;
	for (int ch = 0; (ch = *fmt++);) {
		const char *print = "unknown";
		switch (ch) {
		case 's': print = "string"; break;
		case 'c': print = "character"; break;
		case 'u': print = "unsigned"; break;
		case 'n': print = "decimal"; break;
		case '|': print = " *OR*"; if (*fmt++ != 's') return -1; break;
		case '+': print = "arg..."; break;
		case '*': print = "[arg]..."; break;
		}
		if (fprintf(out, "%s ", print) < 0)
			return -1;
		if (ch == '|')
			if (fprintf(out, "%s ", argv[0]) < 0)
				return -1;
	}
	if (fputc('\n', out) < 0)
		return -1;
	return 0;
}

static long number(const char *s) {
	assert(s);
	return strtol(s, NULL, 0);
}

enum { 
	META_BPS = 1ull << 0,
	META_WRT = 1ull << 1,
	META_PRN = 1ull << 2,
};

#define DEPTH (3*64)

enum {
	SUBLEQ, JMP, ADD, SUB, MOV,
	ZERO, PUT, GET, HALT,
	IJMP, ILOAD, ISTORE, INC, DEC,
	INV, DOUBLE, LSHIFT,

	MAX
};

static const uint64_t instruction_increment[] = {
	[SUBLEQ] = 3, [JMP] = 3/*Disassembly only*/, [MOV] = 12,
	[ADD] = 9, [DOUBLE] = 9, [LSHIFT] = 9 /* multiplied by src*/, [SUB] = 3,
	[ZERO] = 3, [IJMP] = 15/*Disassembly only*/, [ILOAD] = 24,
	[ISTORE] = 48, [PUT] = 3, [GET] = 3, [HALT] = 3/*Disassembly only*/,
	[INC] = 3, [DEC] = 3, [INV] = 21,
};

static const char *instruction_names[] = {
	"SUBLEQ ", "JMP    ", "ADD    ", "SUB    ",
	"MOV    ", "ZERO   ", "PUT    ", "GET    ",
	"HALT   ", "IJMP   ", "ILOAD  ", "ISTORE ",
	"INC    ", "DEC    ", "INV    ", "DOUBLE ",
	"LSHIFT ",
};

typedef struct {
	int instruction;
	uint64_t s, d;
} instruction_t;

typedef struct {
	int matches[MAX];
	int set[9];
	uint64_t v[9];
	unsigned char z_reg[SZ], one_reg[SZ], neg1_reg[SZ];
	clock_t start, end;
	int64_t cnt[MAX];
} optimizer_t;

#define MAX_LABELS (512u)
#define MAX_HOLES  (512u)
#define MAX_COPY   (2048u)
#define MAX_NAME   (16u)

typedef struct {
	char name[MAX_NAME];
	uint64_t location;
	unsigned used:1, operation:8;
} label_t;

typedef struct {
	uint64_t m[SZ], meta[SZ], N, pc, cycles, count, load, max;
	size_t sz;
	FILE *in, *out, *trace;
	int  error, tron;
	char prompt[MAX_LINE];
	char name[MAX_LINE];
	optimizer_t o;
	instruction_t im[SZ];
	unsigned debug: 1, 
		 debug_on_io: 1,
		 debug_on_halt: 1,
		 debug_on_jump: 1,
		 exit_on_cycles :1, 
		 non_blocking :1,
		 exit_on_escape :1,
		 optimize :1,
		 stats :1;

	/* assembler stuff */
	label_t lb[MAX_LABELS];
	label_t holes[MAX_HOLES];
	uint64_t from[MAX_COPY], to[MAX_COPY];
	uint64_t apc, copies;
	int token, unget, aerror, line;
	uint64_t number;
} subleq_t;

typedef subleq_t asm_t;

static inline uint64_t msk(int n) {
	return n < 64 ? 
		(1ull << n) + 0xFFFFFFFFFFFFFFFFull :
		0xFFFFFFFFFFFFFFFFull;
}

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
	(void)fprintf(a->trace, "Error (line %d): ", a->line);
	(void)vfprintf(a->trace, fmt, ap);
	(void)fputc('\n', a->trace);
	va_end(ap);
	a->aerror = -1;
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
	for (size_t i = 0; i < a->apc; i++) {
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
					a->aerror = -1;
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
				a->number = strtol(a->name, NULL, 0) & msk(a->N);
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
	if (a->aerror)
		return -1;
	int r = inner(a);
	if (r < 0)
		a->aerror = -1;
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

static int assemble(asm_t *a, int output) {
	assert(a);

	int dot = 0, cnt = 0;
	uint64_t v[3] = { 0, };
	for (;a->aerror == 0;) {
		int t = lexer(a);
		if (t == DOT) { dot = 1; continue; }
		if (t == EOL) { dot = 0; }
		if (end(t)) {
			switch (cnt) {
			case 0: break;
			case 1: 
				a->m[a->apc + 0] = v[0] & msk(a->N);
				a->m[a->apc + 1] = v[0] & msk(a->N);
				a->m[a->apc + 2] = a->apc + 3;
				if (add_copy(a, a->apc + 0, a->apc + 1) < 0)
					return -1;
				a->apc += 3;
				break;
			case 2: 
				a->m[a->apc + 0] = v[0] & msk(a->N);
				a->m[a->apc + 1] = v[1] & msk(a->N);
				a->m[a->apc + 2] = a->apc + 3;
				a->apc += 3;
				break;
			case 3: 
				a->m[a->apc + 0] = v[0] & msk(a->N);
				a->m[a->apc + 1] = v[1] & msk(a->N);
				a->m[a->apc + 2] = v[2] & msk(a->N);
				a->apc += 3;
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
			/*printf("%s:%d\n", name, (int)(a->apc + cnt));*/
			if (add_label(a, a->name, a->apc + cnt) < 0)
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
				if (add_hole(a, a->name, a->apc + cnt, op) < 0)
					return -1;
			} else {
				n = n + (op == '+' ? ll : ll);
			}
		} else if (t == QUESTION) {
			uint64_t val = a->apc + cnt + 1;
			n = n + (op == '+' ? val : -val);
		} else if (t == AT) {
			uint64_t val = a->apc - (dot ? 0 : (a->apc % 3));
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
			a->m[a->apc++] = n;
		} else {
			if (cnt >= 3)
				return error(a, "Too many operands per instruction");
			v[cnt++] = n;
		}
	}
	if (a->aerror)
		return error(a, "Assembly failed");
	if (resolve(a) < 0)
		return error(a, "Resolving labels failed");
	if (copy(a) < 0)
		return error(a, "Copying locations failed");
	return output ? dump(a, 1) : 0;
}

/* TODO: Example programs, better help, assemble then run... */
static int assembly_help(FILE *out) {
	const char *help = "\
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
\n\
Some of the built in limitations are:\n\
Number of labels:  %u (labels of the type 'label:'\n\
Number of holes:   %u (references to a label)\n\
Number of copies:  %u (single operand instructions)\n\
Label name length: %u\n\n\
It would be possible to remove these restrictions, but it\n\
is far easier to keep them in.\n";


	return fprintf(out, help, MAX_LABELS, MAX_HOLES, MAX_COPY, MAX_NAME);
}

static int tracer(subleq_t *s, const char *fmt, ...) {
	assert(s);
	assert(fmt);
	int r = 0;
	va_list ap;
	if (s->tron <= 0 || s->trace == NULL)
		return 0;
	va_start(ap, fmt);
	if (vfprintf(s->trace, fmt, ap) < 0) {
		s->error = -1;
		r = -1;
	}
	va_end(ap);
	return r;
}

static int printer(subleq_t *s, const char *fmt, ...) {
	assert(s);
	assert(s->out);
	assert(fmt);
	int r = 0;
	va_list ap;
	va_start(ap, fmt);
	if (vfprintf(s->trace, fmt, ap) < 0) {
		s->error = -1;
		r = -1;
	}
	va_end(ap);
	return r;
}

static uint64_t L(subleq_t *s, uint64_t a) {
	assert(s);
	assert(s->sz);
	return a % s->sz;
}

static void asserts_for_cmd(subleq_t *s, int argc, char **argv) {
	assert(s);
	assert(argc >= 1);
	assert(argv);
}

static int cmd_continue(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	if (argc > 1)
		s->pc = number(argv[1]);
	s->debug = 0;
	return 0;
}

static int cmd_dump(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const uint64_t addr = number(argv[1]), len = number(argv[2]);
	size_t width = 4;
	size_t bytes = 8;
	const char *fmt = "%016lx ";
	if (s->N <= 32) { fmt = "%08lx "; bytes = 4; width = 8; }
	if (s->N <= 16) { fmt = "%04lx "; bytes = 2; width = 8; }
	if (s->N <=  8) { fmt = "%02lx "; bytes = 1; width = 16; }

	for (size_t i = 0; i < len; i += width) {
		printer(s, fmt, (long)i);
		printer(s, ": ");
		for (size_t j = 0; j < width && ((j + i) < len); j++) {
			uint64_t d = s->m[(addr + i + j) % s->sz];
			printer(s, fmt, (long)d);
		}
		if ((i + width) > len) {
		}
		printer(s, "\t");
		for (size_t j = 0; j < width && ((j + i) < len); j++) {
			uint64_t d = s->m[(addr + i + j) % s->sz];
			for (size_t k = 0; k < bytes; k++) {
				int ch = d & 255;
				d >>= 8;
				printer(s, "%c", isgraph(ch) ? ch : '.');
			}
		}
		printer(s, "\n");
	}
	return 0;
}

static int cmd_breakp(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	for (int i = 1; i < argc; i++) {
		const uint64_t addr = number(argv[i]);
		s->meta[L(s, addr)] |= META_BPS;
	}
	return 0;
}

static int cmd_breakp_clear(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	for (size_t i = 0; i < s->sz; i++) {
		s->meta[i] &= ~META_BPS;
	}
	return 0;
}

static int cmd_breakp_list(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	printer(s, "pc=%lx cycles=%ld bps: ", (long)s->pc, (long)s->count);
	for (size_t i = 0; i < s->sz; i++)
		if (s->meta[i] & META_BPS)
			printer(s, "%lx ", (long)i);
	printer(s, "\n");
	return 0;
}


static int cmd_store(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const uint64_t loc = number(argv[1]), val = number(argv[2]);
	s->m[L(s, loc)] = val & msk(s->N);
	return 0;
}

static int cmd_read(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const uint64_t loc = number(argv[1]);
	printer(s, "%ld\n", (long)s->m[L(s, loc)]);
	return 0;
}

static int subleq_save(subleq_t *s, uint64_t start, uint64_t len, const char *file) {
	assert(s);
	assert(file);
	FILE *f = fopen(file, "wb");
	if (!f)
		return -1;
	for (size_t i = 0; i < len; i++) {
		if (fprintf(f, "%ld\n", (long)s->m[L(s, i + start)]) < 0)
			return -2;
	}
	if (fclose(f) < 0)
		return -3;
	return 0;
}

static int subleq_load(subleq_t *s, uint64_t start, const char *file) {
	FILE *f = fopen(file, "rb");
	if (!f)
		return -1;
	s->load = start;
	long d = 0;
	while (fscanf(f, "%ld,", &d) > 0)
		s->m[L(s, s->load++)] = ((int64_t)d) & msk(s->N);
	if (fclose(f) < 0)
		return -1;
	return 0;
}

static int cmd_save(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	/* NB. Could offer to save to different formats */
	const long start  = argc > 2 ? number(argv[2]) : 0;
	const long length = argc > 3 ? number(argv[3]) : (long)s->max;
	if (subleq_save(s, start, length, argv[1]) < 0) {
		printer(s, "save to file '%s' failed\n", argv[1]);
		return 1;
	}
	return 0;
}

static int cmd_prompt(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const size_t len = strlen(argv[1]) + 1;
	assert(sizeof (s->prompt) >= len);
	memcpy(s->prompt, argv[1], len);
	return 0;
}

static int cmd_fill(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const size_t start = number(argv[1]);
	const size_t length = number(argv[2]);
	if (argc < 4) {
		for (size_t i = 0; i < length; i++) {
			s->m[L(s, start + i)] = 0;
		}
		return 0;
	}
	const size_t slen = strlen(argv[3]);
	for (size_t i = 0; i < length; i++) {
		s->m[L(s, start + i)] = argv[3][i % slen];
	}
	return 0;
}


static int cmd_quit(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	exit(0);
	return 0;
}

static int cmd_move(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const size_t start = number(argv[1]);
	const size_t length = number(argv[2]);
	const size_t dest = number(argv[3]);
	for (size_t i = 0; i < length; i++)
		s->m[L(s, dest + i)] = s->m[L(s, start + i)];
	return 0;
}

static int cmd_compare(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const size_t start = number(argv[1]);
	const size_t length = number(argv[2]);
	const size_t dest = number(argv[3]);
	for (size_t i = 0; i < length; i++) {
		const long a = L(s, start + i);
		const long b = L(s, dest + i);
		const long la = s->m[a];
		const long lb = s->m[b];
		if (la != lb)
			printer(s, "%04lx:%04lx - %04lx:%04lx\n", a, la, b, lb);
	}
	return 0;
}


static int cmd_load(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	s->load = 0;
	const long addr = argc < 2 ? 0 : number(argv[1]);
	const char *file = argc < 3 ? s->name : argv[2];
	if (subleq_load(s, addr, file) < 0) {
		printer(s, "File load of '%s' failed\n", file);
		return 1;
	}
	return 0;
}

static int cmd_register(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const long pc = s->pc;
	const long a = s->m[L(s, pc + 0)];
	const long b = s->m[L(s, pc + 1)];
	const long c = s->m[L(s, pc + 2)];
	printer(s, "pc=%04lx, a=%04lx, b=%04lx, c=%04lx, N=%d, load=%ld\n", pc, a, b, c, (int)s->N, (long)s->load);
	return 0;
}

static int cmd_search(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	const size_t start = number(argv[1]);
	const size_t length = number(argv[2]);

	for (int i = 3; i < argc; i++) {
		const long find = number(argv[i]);
		for (size_t j = 0; j < length; j++) {
			const long addr = L(s, start + j);
			if (find == (long)s->m[addr])
				printer(s, "%04lx:%04lx\n", addr, find);
		}
	}


	return 0;
}

static int assemble_with_opts(subleq_t *s, int clear, int dump, uint64_t start) {
	assert(s);
	if (clear) {
		s->aerror = 0;
		s->copies = 0;
		s->unget = 0;
		s->line = 0;
		s->name[0] = '\0';
		memset(s->holes, 0, sizeof (s->holes));
		memset(s->lb, 0, sizeof (s->lb));
	}
	s->apc = start;
	return assemble(s, dump);
}

static int assemble_from_file(subleq_t *s, const char *name, int clear, int dump, uint64_t start) {
	assert(s);
	assert(name);
	errno = 0;
	FILE *f = fopen(name, "rb");
	if (!f) {
		(void)fprintf(s->trace, "Unable to open file '%s' for reading: %s\n", name, strerror(errno));
		return -1;
	}
	FILE *t = s->in;
	s->in = f;
	int r = assemble_with_opts(s, clear, dump, start);
	if (fclose(f) < 0)
		r = -1;
	s->in = t;
	return r;
}

/* We probably want all kinds of fancy options for this assembler */
static int cmd_assemble(subleq_t *s, int argc, char **argv) {
	asserts_for_cmd(s, argc, argv);
	return assemble_with_opts(s, 1, 0, argc > 1 ? atol(argv[1]) : 0) < 0 ? 1 : 0;
}

static int subleq_getch(subleq_t *s, terminal_t *t) {
	assert(s);
	assert(t);
	if (s->non_blocking == 0) {
		const int ch = fgetc(t->in);
		if (ch == ESCAPE)
			s->debug = 1;
		return ch;
	}
	const int ch = term_getch(t);
	if (ch == EOF) 
		term_sleep_ms(1);
	if (ch == ESCAPE) {
		s->debug = 1;
		if (s->exit_on_escape)
			exit(0);
	}
	return ch == DELETE ? BACKSPACE : ch;
}


static int match(subleq_t *s, uint64_t *n, int sz, uint64_t pc, const char *st, ...) {
	va_list ap;
	int r = 0, i = 0, j = 0;
	for (int i = 0; i < 9; i++) {
		s->o.set[i] = 0;
		s->o.v[i] = 0;
	}
	va_start(ap, st);
	for (i = 0, j = 0; st[j] && i < sz; j++) {
		switch (st[j]) {
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
		case '8': case '9': {
			int p = st[j] - '0';
			if (s->o.set[p]) {
				if (n[i] != s->o.v[p]) goto end;
			} else {
				s->o.set[p] = 1;
				s->o.v[p] = n[i];
			}
			i++;
			break;
		 }
		 /* Mem location 0 must be 0 in SUBLEQ image! */
		 case 'Z': if (n[i] != 0) goto end; i++; break;
		 case 'N': if (n[i] != msk(s->N)) goto end; i++; break;
		 case '>': if (n[i] != (pc + i + 1)) goto end; i++;
			break;
		 case '%': {
			 uint64_t q = va_arg(ap, int);
			 if (n[i] != q)
				 goto end;
			 i++;
		 } break;
		 case '!': {
			uint64_t *p = va_arg(ap, uint64_t*);
			*p = n[i];
			i++;
		 } break;
		 case '?': i++; break;
		 case ' ': case '\t':
		 case '\n': case '\r': break;
		 default: r = -1; goto end;
		 }
	}
	while (isspace(st[j]))
		j++;
	r = (st[j] == 0) && (i <= sz);
end:
	va_end(ap);
	return r;
}

static uint64_t get(optimizer_t *o, char var) {
	if (var < '0' || var > '9' || o->set[var - '0'] == 0)
		return -1ll;
	return o->v[var - '0'];
}

/* This section pattern matches the code finding
* sequences of SUBLEQ instructions against known
* instruction macros.	It is essentially a
* disassembler. It is liable not to work for every
* case, but will do so for the code that *I* want to
* speed up. */
static int optimizer(subleq_t *s, uint64_t pc) {
	assert(s);

	for (uint64_t i = 0; i < pc; i++) {
		switch (s->m[i]) {
		case 0: s->o.z_reg[i] = 1; break;
		case 1: s->o.one_reg[i] = 1; break;
		/*case 0xFFFF: s->o.neg1_reg[i] = 1; break;*/
		}
		if (msk(s->N) == s->m[i])
			s->o.neg1_reg[i] = 1;
	}

	for (uint64_t i = 0; i < pc; i++) {
		uint64_t q0 = 0, q1 = 0;
		uint64_t n[DEPTH] = { 0, };

		for (size_t j = 0; j < DEPTH; j++)
			n[j] = s->m[L(s, i + j)];

		/* Largest instructions *must* go first */

		if (match(s, n, DEPTH, i, "00> !Z> Z0> ZZ> 11> ?Z> Z1> ZZ> 22> ?Z> Z2> ZZ> 33> !Z> Z3> ZZ>", &q0, &q1) == 1
			&& get(&s->o, '0') == (i+(3*12))
			&& get(&s->o, '1') == (i+(3*12)+1)) {
			s->im[L(s, i)].instruction = ISTORE;
			s->im[L(s, i)].d = L(s, q0);
			s->im[L(s, i)].s = L(s, q1);
			s->o.matches[ISTORE]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "00> !Z> Z0> ZZ> 11> ?Z> Z1> ZZ>", &q0) == 1 && get(&s->o, '0') == (i + 15)) {
			s->im[L(s, i)].instruction = ILOAD;
			s->im[L(s, i)].d = L(s, get(&s->o, '1'));
			s->im[L(s, i)].s = L(s, q0);
			s->o.matches[ILOAD]++;
			continue;
		}

		uint64_t shift = 0, l = 0, dest = 0;
		for (l = 0; l < DEPTH; l += 9) {
			if (match(s, n+l, DEPTH-l, i+l, "!Z> Z!> ZZ>", &q0, &q1) == 1 && q0 == q1) {
				if (l == 0) {
					dest = q0;
				} else {
					if (dest != q0) {
						break;
					}
				}
				shift++;
			} else {
				break;
			}
		}
		if (shift >= 2) {
			s->im[L(s, i)].instruction = LSHIFT;
			s->im[L(s, i)].d = L(s, dest);
			s->im[L(s, i)].s = shift;
			s->o.matches[LSHIFT]++;
			continue;
		}


		if (match(s, n, DEPTH, i, "00> 10> 11> 2Z> Z1> ZZ> !1>", &q0) == 1
				&& s->o.one_reg[q0]) {
			s->im[L(s, i)].instruction = INV;
			s->im[L(s, i)].d = L(s, get(&s->o, '1'));
			s->o.matches[INV]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "00> !Z> Z0> ZZ> ZZ>", &q0) == 1 && get(&s->o, '0') == (i + (3*4) + 2)) {
			s->im[L(s, i)].instruction = IJMP;
			s->im[L(s, i)].d = L(s, q0);
			s->o.matches[IJMP]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "00> !Z> Z0> ZZ>", &q0) == 1) {
			s->im[L(s, i)].instruction = MOV;
			s->im[L(s, i)].d = L(s, get(&s->o, '0'));
			s->im[L(s, i)].s = L(s, q0);
			s->o.matches[MOV]++;
			continue;
		}

		/* We should match multiple ones in a row and
			* turn them into a left shift */
		if (match(s, n, DEPTH, i, "!Z> Z!> ZZ>", &q0, &q1) == 1 && q0 == q1) {
			s->im[L(s, i)].instruction = DOUBLE;
			s->im[L(s, i)].d = L(s, q1);
			s->im[L(s, i)].s = L(s, q0);
			s->o.matches[DOUBLE]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "!Z> Z!> ZZ>", &q0, &q1) == 1) {
			s->im[L(s, i)].instruction = ADD;
			s->im[L(s, i)].d = L(s, q1);
			s->im[L(s, i)].s = L(s, q0);
			s->o.matches[ADD]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "00>") == 1) {
			s->im[L(s, i)].instruction = ZERO;
			s->im[L(s, i)].d = L(s, get(&s->o, '0'));
			s->o.matches[ZERO]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "ZZ!", &q0) == 1 && q0 >= SZ) {
			s->im[L(s, i)].instruction = HALT;
			s->o.matches[HALT]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "00!", &q0) == 1) {
			s->im[L(s, i)].instruction = JMP;
			s->im[L(s, i)].d = q0;
			s->im[L(s, i)].s = L(s, get(&s->o, '0'));
			s->o.matches[JMP]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "N!>", &q0) == 1) {
			s->im[L(s, i)].instruction = GET;
			s->im[L(s, i)].d = L(s, q0);
			s->o.matches[GET]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "!N>", &q0) == 1) {
			s->im[L(s, i)].instruction = PUT;
			s->im[L(s, i)].s = L(s, q0);
			s->o.matches[PUT]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1 && s->o.neg1_reg[L(s, q0)]) {
			s->im[L(s, i)].instruction = INC;
			s->im[L(s, i)].d = L(s, q1);
			s->o.matches[INC]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1 && s->o.one_reg[L(s, q0)]) {
			s->im[L(s, i)].instruction = DEC;
			s->im[L(s, i)].d = L(s, q1);
			s->o.matches[DEC]++;
			continue;
		}

		if (match(s, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1) {
			s->im[L(s, i)].instruction = SUB;
			s->im[L(s, i)].d = L(s, q1);
			s->im[L(s, i)].s = L(s, q0);
			s->o.matches[SUB]++;
			continue;
		}

		s->o.matches[SUBLEQ]++;
	}
	return 0;
}

static int report(subleq_t *s) {
	assert(s);
	optimizer_t *o = &s->o;
	double elapsed_s = (double)(o->end - o->start);
	elapsed_s /= CLOCKS_PER_SEC;
	int64_t total = 0, subs = 0;
	FILE *e = stderr;
	for (int i = 0; i < MAX; i++) {
		total += o->cnt[i];
		subs  += o->matches[i];
	}
	static const char *rep_div = "+--------+--------+--------------+----------+\n";

	if (fputs(rep_div, e) < 0)
		return -1;
	if (fprintf(e, "| Instr. | Subs.  | Instr. Cnt   | Instr. %% |\n") < 0)
		return -1;
	if (fputs(rep_div, e) < 0)
		return -1;
	for (int i = 0; i < MAX; i++)
		if (fprintf(e, "| %s| % 6d | % 12"PRId64" | % 7.1f%% |\n", instruction_names[i], o->matches[i], o->cnt[i], 100.0*((float)o->cnt[i])/(float)total) < 0)
			return 1;
	if (fputs(rep_div, e) < 0)
		return -1;
	if (fprintf(e, "| Totals | % 6d | % 12"PRId64" |          |\n", (int)subs, total) < 0)
		return -1;
	if (fputs(rep_div, e) < 0)
		return -1;
	if (fprintf(e, "|         EXECUTION TIME %.3f  SECONDS     |\n", elapsed_s) < 0)
		return -1;
	if (fputs(rep_div, e) < 0)
		return -1;
	return 0;
}


static int cmd_disassemble(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);

	/* We could add more options here, to better control
	 * disassembly, one useful one would be to round down
	 * to the nearest multiple of three when taking addresses,
	 * another would be to always increment by the same amount.
	 *
	 * We also might want an option to turn off re-running
	 * the optimizer. */

	const unsigned long addr = number(argv[1]);
	const unsigned long numb = number(argv[2]);

	if (optimizer(s, 0) < 0)
		return 1;

	int inc = 0;
	for (size_t i = 0; i < numb; i += inc) {
		instruction_t *im = &s->im[L(s, addr + i)];
		const int instruction = im->instruction;
		const char *name = instruction_names[instruction];
		if (instruction == SUBLEQ) {
			const long a = s->m[L(s, i + 0)];
			const long b = s->m[L(s, i + 1)];
			const long c = s->m[L(s, i + 2)];
			printer(s, "%04lx: %s %04lx %04lx %04lx\n", (long)i, name, a, b, c);
		} else {
			printer(s, "%04lx: %s s=%04lx d=%04lx\n", (long)i, name, (long)im->s, (long)im->d); 
		}
		inc = instruction_increment[instruction];
		if (inc <= 0)
			inc = 3;
	}

	return 0;
}

static int cmd_tron(subleq_t *s, int argc, char **argv) {
	s->debug = 0;
	s->tron = 1;
	s->count = 0;
	s->cycles = argc > 1 ? number(argv[1]) : 0;
	return 0;
}


static int cmd_trace(subleq_t *s, int argc, char **argv) {
	s->debug_on_io = 0;
	s->debug_on_halt = 0;
	s->debug_on_jump = 0;
	return cmd_tron(s, argc, argv);
}

static int cmd_proceed(subleq_t *s, int argc, char **argv) {
	s->debug_on_io = 1;
	s->debug_on_halt = 1;
	s->debug_on_jump = 1;
	return cmd_tron(s, argc, argv);
}

static int cmd_name(subleq_t *s, int argc, char **argv) {
	memset(s->name, 0, sizeof (s->name));
	if (argc < 2)
		return 0;
	strcpy(s->name, argv[1]);
	return 0;
}

static int cmd_input(subleq_t *s, int argc, char **argv) {
	UNUSED(argc);
	s->m[L(s, number(argv[1]))] = fgetc(s->in);
	return 0;
}

static int cmd_output(subleq_t *s, int argc, char **argv) {
	UNUSED(argc);
	if (fputc(s->m[L(s, number(argv[1]))], s->out) < 0)
		return 1;
	return 0;
}

static int cmd_hex(subleq_t *s, int argc, char **argv) {
	UNUSED(argc);
	long a = strtol(argv[1], NULL, 0);
	long b = strtol(argv[2], NULL, 0);

	long sub = a - b;
	long add = a + b;
	long mul = a * b;
	long div = a / (b ? b : 1);

	printer(s, "hex: 0x%04lx 0x%04lx add=%04lx sub=%04lx mul=%04lx div=%04lx\n", a, b, add, sub, mul, div);
	printer(s, "dec:   %04ld   %04ld add=%04ld sub=%04ld mul=%04ld div=%04ld\n", a, b, add, sub, mul, div);

	long and = a & b;
	long or  = a | b;
	long xor = a ^ b; 
	long ina = ~a;
	long inb = ~b;

	printer(s, "hex: 0x%04lx 0x%04lx and=%04lx or=%04lx xor=%04lx ~a=%04lx ~b=%04lx\n", a, b, and, or, xor, ina, inb);
	printer(s, "dec:   %04ld   %04ld and=%04ld or=%04ld xor=%04ld ~a=%04ld ~b=%04ld\n", a, b, and, or, xor, ina, inb);

	return 0;
}

static int cli_help(FILE *out) {
	assert(out);

	const char *help_string = "\n\
Project: %s\n\
Author:  %s\n\
Email:   %s\n\
Repo:    %s\n\
Version: %s\n\
\n\
This program contains an entire tool-chain and virtual\n\
machine for a SUBLEQ One Instruction Set Computer, the\n\
utility is similar to the MS-DOS DEBUG.COM / DEBUG.EXE\n\
program, featuring similar commands.\n\n\
Program returns non-zero on failure, zero on success.\n\n\
This debugging program is much more complex than the simple\n\
SUBLEQ architecture, as an example of the simplicity of SUBLEQ\n\
the following program will run SUBLEQ programs:\n\n\
\t#include <stdio.h>\n\
\tint main(int x,char**v){FILE*f=fopen(v[1],\"r\");short p=0,m[1<<16],*i=m;\n\
\twhile(fscanf(f,\"%%hd\",i++)>0);for(;p>=0;){int a=m[p++],b=m[p++],c=m[p++];\n\
\ta<0?m[b]=getchar():b<0?putchar(m[a]):(m[b]-=m[a])<=0?p=c:0;}}\n\
\n\n\
Options:\n\n\
\t-h\n\t\tDisplay this help message and exit.\n\
\t-H\n\t\tDisplay the assembler help and exit.\n\
\t-k\n\t\tTurn non-blocking terminal input on.\n\
\t-t\n\t\tTurn tracing on.\n\
\t-r\n\t\tPrint report after execution.\n\
\t-z\n\t\tRecompile SUBLEQ code for speed, might break program\n\
\t-d\n\t\tRun debugger first before execution.\n\
\t-c num\n\t\tRun for 'num' cycles before halting into debugger.\n\
\t-b break-point\n\t\tAdd break point.\n\
\t-s file.dec\n\t\tSave to file after running.\n\
\t-a file.asq\n\t\tAssemble a file and dump it to standard out.\n\
\t-A file.asq\n\t\tAssemble a file into a memory.\n\
\t-n num (8-64)\n\t\tSet SUBLEQ VM to bit-width, inclusive.\n\
\t-p num\n\t\tPrint out cell contents after exiting VM.\n\
\t-S num\n\t\tChange memory size, max size is %ld.\n\
\t-x forth,hi,echo,halt,self\n\t\tLoad example image from internal storage.\n";

	if (fprintf(out, help_string, PROJECT, AUTHOR, EMAIL, REPO, VERSION, (long)SZ) < 0)
		return -1;
	return 0;
}


static int debug_help(subleq_t *s);
static int cmd_help(subleq_t *s, int argc, char **argv);

/* NB. As part of this structure some argument validation could be
 * done, for example, specifying that there needs to be exactly or
 * at least three arguments, and so forth. This *could* be specified
 * with a format string, from which help messages *could* be generated
 * from. So, "ssuf|s" could specify that the function takes either
 * a; string, another string, an unsigned number and a float OR just
 * a string. This first argument, the command name, would always be
 * a string. */
typedef struct {
	const char *name, *alt, *help, *fmt;
	int (*fn)(subleq_t *s, int argc, char **argv);
} subleq_debug_command_t;

static subleq_debug_command_t cmds[] = {
	{ .name = "assemble",   .alt = "a", .fn = cmd_assemble,     .fmt = "sn|s", .help = "Assemble SUBLEQ program at location", },
	{ .name = "break",      .alt = "b", .fn = cmd_breakp,       .fmt = "s+",   .help = "Add a break-point", },
	{ .name = "clear",                  .fn = cmd_breakp_clear, .fmt = "s",    .help = "Clear all break-points", },
	{ .name = "compare",    .alt = "c", .fn = cmd_compare,      .fmt = "snnn", .help = "Compare two sections of memory", },
	{ .name = "continue",               .fn = cmd_continue,     .fmt = "s",    .help = "Continue execution", },
	{ .name = "dump",       .alt = "d", .fn = cmd_dump,         .fmt = "snn",  .help = "Hex-dump an area of memory", },
	{ .name = "enter",      .alt = "e", .fn = cmd_store,        .fmt = "snn",  .help = "Store a value at an address", },
	{ .name = "fill",       .alt = "f", .fn = cmd_fill,         .fmt = "snns", .help = "Fill section of memory with value", },
	{ .name = "go",         .alt = "g", .fn = cmd_continue,     .fmt = "sn|s", .help = "Continue execution, optionally starting from address", },
	{ .name = "help",       .alt = "h", .fn = cmd_help,         .fmt = "s|ss", .help = "Print this help, or explain another commands arguments", },
	{ .name = "hex",        .alt = "x", .fn = cmd_hex,          .fmt = "snn",  .help = "A small calculator, accepts hex with 0x prefix, octal with 0 prefix", },
	{ .name = "input",      .alt = "i", .fn = cmd_input,        .fmt = "sn",   .help = "Get a byte of input and place it at address", },
	{ .name = "list",       .alt = "?", .fn = cmd_breakp_list,  .fmt = "s",    .help = "List all break-points", },
	{ .name = "load",       .alt = "l", .fn = cmd_load,         .fmt = "sns|sn|s",  .help = "Load from disk to memory", },
	{ .name = "move",       .alt = "m", .fn = cmd_move,         .fmt = "snnn", .help = "Move one section of memory to another", },
	{ .name = "name",       .alt = "n", .fn = cmd_name,         .fmt = "s|ss", .help = "Name a file for use with load/write", },
	{ .name = "output",     .alt = "o", .fn = cmd_output,       .fmt = "sn",   .help = "Get a byte from an address and output it", },
	{ .name = "proceed",    .alt = "p", .fn = cmd_proceed,      .fmt = "s|sn", .help = "Turn tracing on, or turn it on for X instructions", },
	{ .name = "prompt",                 .fn = cmd_prompt,       .fmt = "ss",   .help = "Change the command prompt", },
	{ .name = "quit",       .alt = "q", .fn = cmd_quit,         .fmt = "s",    .help = "Quit", },
	{ .name = "read",       .alt = "@", .fn = cmd_read,         .fmt = "sn",   .help = "Read a value from an address", },
	{ .name = "register",   .alt = "r", .fn = cmd_register,     .fmt = "s",    .help = "Display registers", },
	{ .name = "search",     .alt = "s", .fn = cmd_search,       .fmt = "snn+", .help = "Search through memory for a pattern", },
	{ .name = "trace",      .alt = "t", .fn = cmd_trace,        .fmt = "s|sn", .help = "Turn tracing on, or turn it on for X instructions", },
	{ .name = "unassemble", .alt = "u", .fn = cmd_disassemble,  .fmt = "snn",  .help = "Disassemble/Unassemble a section of memory", },
	{ .name = "write",      .alt = "w", .fn = cmd_save,         .fmt = "ssn|ssnn", .help = "Write section of memory to disk", },
	{ .fn = NULL }, 
};

static int cmd_help(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 2)
		return debug_help(s);
	const char *name = argv[1];
	subleq_debug_command_t *d = NULL;
	for (size_t i = 0; (d = &cmds[i])->fn; i++) {
		if (!strcmp(name, d->name) || (d->alt && !strcmp(name, d->alt))) {
			printer(s, "Name: %s\nAlternate: %s\nDescription: %s\n", d->name, d->alt ? d->alt : "(none)", d->help);
			args_explain(s->out, d->fmt, 1, (char*[]){(char*)d->name});
			return 0;
		}
	}
	return 1;
}

static int debug_help(subleq_t *s) {
	assert(s);
	printer(s, "      name alt -- description\n");
	subleq_debug_command_t *d = NULL;
	for (int i = 0; (d = &cmds[i])->fn; i++) {
		printer(s, "% 10s   %s -- %s\n", d->name, d->alt ? d->alt : " ",  d->help);
	}
	return 0;
}

static int command(subleq_t *s) {
	assert(s);
	char buffer[MAX_LINE] = { 0, };
	char *argv[(MAX_LINE / 2) - 1] = { NULL, };
	int argc = 0;
	s->debug_on_jump = 0;
	s->debug_on_io = 0;
	if (s->prompt[0] == 0) {
		static const char *prompt = "sq>";
		const size_t len = strlen(prompt) + 1;
		assert(sizeof (s->prompt) >= len);
		memcpy(s->prompt, prompt, len);
	}
again:
	if (s->error)
		return -1;
	argc = 0;
	memset(argv, 0, sizeof (argv));
	memset(buffer, 0, sizeof (buffer));
	printer(s, "%s ", s->prompt);
	if (!fgets(buffer, sizeof buffer, s->in))
		return -1;
	if (buffer[sizeof buffer - 2]) {
		printer(s, "Line '%s' too long\n", buffer);
		goto again;
	}
	for (char *p = buffer; *p; p++) {
		while (isspace(*p))
			p++;
		if (*p)
			argv[argc++] = p;
		while (*p && !isspace(*p))
			p++;
		*p = 0;
	}
	if (argc == 0)
		goto again;

	/* NB. sys_getopt could be reused for the command processing... */

	subleq_debug_command_t *d = NULL;
	for (int i = 0; (d = &cmds[i])->fn; i++) {
		if (!strcmp(argv[0], d->name) || (d->alt && !strcmp(argv[0], d->alt))) {
			if (d->fmt && d->fmt[0]) {
				if (args_validate(d->fmt, argc, argv) < 0) {
					if (args_explain(s->out, d->fmt, argc, argv) < 0) {
						s->error = -1;
						return -1;
					}
					goto again;
				}
			}
			if (d->fn(s, argc, argv) < 0) {
				s->error = -1;
				return -1;
			}
			if (s->debug == 0)
				return 0;
			goto again;
		}
	}
	printer(s, "Command '%s' not found\n", argv[0]);
	if (!(s->error))
		goto again;
	return 0;
}

static int subleq_break(subleq_t *s, int instruction) {
	assert(s);
	if (s->meta[L(s, s->pc + 0)] & META_BPS)
		return 1;
	if (s->meta[L(s, s->pc + 1)] & META_BPS)
		return 1;
	if (s->meta[L(s, s->pc + 2)] & META_BPS)
		return 1;
	if (s->debug)
		return 1;
	if (s->debug_on_io) {
		if (instruction == PUT || instruction == GET)
			return 1;
		if (instruction == SUBLEQ) {
			/*if (a == -1 || b == -1) {
			}*/
		}
	}
	return 0;
}

/* NOTE: This function is *much* more complex than the original
 * SUBLEQ VM, which is contained within the SUBLEQ instruction,
 * if you are wondering why this single instruction computer has
 * more than one instruction, the answer is "optimizations", the
 * code can be recompiled with common expressions factored out into
 * instructions. */
static int subleq(subleq_t *s, terminal_t *t) {
	assert(s);
	assert(t);
	assert(s->N >= 8 && s->N <= 64);
	assert(s->in);
	assert(s->out);
	if (optimizer(s, s->load) < 0)
		return 1;
	s->o.start = clock();
	uint64_t *m = s->m;
	instruction_t *im = s->im;
	for (; !(s->error) ; s->count++) {

		if (s->pc >= SZ || (s->pc == msk(s->N))) {
			if (s->debug_on_halt) {
				if (command(s) < 0)
					return 0;
				s->pc = L(s, s->pc);
			} else {
				break;
			}
		}

		const int instruction = s->optimize ? im[s->pc].instruction : SUBLEQ;
		const int inc = instruction_increment[instruction];
		const uint64_t src = im[s->pc].s, d = im[s->pc].d;

		if (s->cycles) {
			if (s->count >= s->cycles) {
				if (s->exit_on_cycles)
					break;
				if (command(s) < 0)
					return 0;
			}
		} else if (subleq_break(s, instruction)) {
			if (command(s) < 0)
				return 0;
		}

		s->o.cnt[instruction/*% MAX*/]++;

		tracer(s, "pc:%04lx: ", (long)s->pc);

		if (instruction != SUBLEQ)
			tracer(s, "i=%s src=%04lx dst=%04lx ", instruction_names[instruction], (long)src, (long)d);

		switch (instruction) {
		case SUBLEQ: { /* OG Instruction */
			const uint64_t a = s->m[L(s, s->pc++)];
			const uint64_t b = s->m[L(s, s->pc++)];
			const uint64_t c = s->m[L(s, s->pc++)];
			const size_t la = L(s, a), lb = L(s, b);

			tracer(s, "a=%04lx b=%04lx c=%04lx ", (long)a, (long)b, (long)c);
			if (a == msk(s->N)) {
				s->m[lb] = subleq_getch(s, t) & msk(s->N);
				tracer(s, "i=%04lx ", (long)s->m[lb]);
			} else if (b == msk(s->N)) {
				tracer(s, "o=%04lx ", (long)s->m[la]);
				if (term_putch(t, s->m[la]) < 0) {
					s->error = -1;
					return -1;
				}
				if (fflush(s->out) < 0) {
					s->error = -1;
					return -1;
				}
			} else {
				uint64_t r = s->m[lb] - s->m[la];
				r &= msk(s->N);
				tracer(s, "[a]=%04lx [b]=%04lx r=%04lx ", (long)s->m[la], (long)s->m[lb], (long)r);
				if (r & HI(s->N) || r == 0) {
					tracer(s, "%c ", s->pc == c ? '=' : '!');
					if (s->pc != c)
						s->debug = s->debug_on_jump;
					s->pc = c;
				}
				s->max = MAX(lb, s->max);
				s->meta[lb] |= META_WRT;
				s->m[lb] = r;
			}
		} break;
		/* NB. We might be able to run more programs
			* correctly if we disable these instructions if
			* a write occurs within the bounds of an
			* instruction macro, this would slow things down
			* however. */
		case JMP: s->pc = d; m[src] = 0; s->debug = s->debug_on_jump; break;
		case MOV: m[d]	= m[src]; s->pc += inc; break;
		case ADD: m[d] += m[src]; s->pc += inc; break;
		case DOUBLE: m[d] <<= 1; s->pc += inc; break;
		case LSHIFT: m[d] <<= src; s->pc += inc * src; break;
		case SUB: m[d] -= m[src]; s->pc += inc; break;
		case ZERO: m[d] = 0; s->pc += inc; break;
		case IJMP: s->pc = m[d]; s->debug = s->debug_on_jump; break;
		case ILOAD: m[d] = m[L(s, m[src])]; s->pc += inc;
			break;
		case ISTORE: m[L(s, m[d])] = m[src]; s->pc += inc;
			break;
		case PUT:
			if (term_putch(t, m[im[L(s, s->pc)].s]) < 0)
				return -1;
			s->pc += inc;
			break;
		case GET: m[im[s->pc].d] = subleq_getch(s, t) & msk(s->N); s->pc += inc; break;
		case HALT: s->pc |= msk(s->N); break;
		case INC: m[d]++; s->pc += inc; break;
		case DEC: m[d]--; s->pc += inc; break;
		case INV: m[d] = ~m[d]; s->pc += inc; break;
		default:
			return -1;
		}
		tracer(s, "\n");
	}
	s->o.end = clock();
	if (s->stats)
		if (report(s) < 0)
			return 1;
	return s->error;
}

static terminal_t terminal = { .in = NULL, };

static void restore(void) {
	term_restore(&terminal);
}

static int subleq_examples(subleq_t *s, const char *name) {
	assert(s);
	assert(name);
	uint64_t *load = NULL;
	size_t nelems = 0;
	if (!strcmp(name, "forth") || !strcmp(name, "eforth")) {
		static uint64_t eforth_image[] = {
#include "subleq.cma"
		};
		load = eforth_image;
		nelems = NELEMS(eforth_image);
	}
	if (!strcmp(name, "hi")) {
		static uint64_t hi[] = { 
			9, -1, 3, 10, -1, 6, 0, 0, -1, 72, 105, 0, 
		};
		load = hi;
		nelems = NELEMS(hi);
	}
	if (!strcmp(name, "hello")) {
		static uint64_t hello[] = { 
			15, 17, -1, 17, -1, -1, 16, 1, -1, 16, 3, -1, 
			15, 15, 0, 0, -1, 72, 101, 108, 108, 111, 44, 
			32, 119, 111, 114, 108, 100, 33, 10, 0,
		};
		load = hello;
		nelems = NELEMS(hello);
	}
	if (!strcmp(name, "echo")) {
		static uint64_t echo[] = { 
			0, 0, 3, -1, 0, 6, 4, 0, -1, 0, -1, 12, 0, -1, 
			15, 0, 0, 0, 
		};
		load = echo;
		nelems = NELEMS(echo);
	}
	if (!strcmp(name, "halt")) {
		static uint64_t halt[] = { 0, 0, -1, };
		load = halt;
		nelems = NELEMS(halt);
	}
	if (!strcmp(name, "forever")) {
		static uint64_t forever[] = { 0, 0, 0, };
		load = forever;
		nelems = NELEMS(forever);
	}
	if (!strcmp(name, "self")) {
		static uint64_t self_image[] = {
#include "self.cma"
		};
		load = self_image;
		nelems = NELEMS(self_image);
	}
	/* An image that performed introspection into the
	 * SUBLEQ machine; machine width, arithmetic type (twos,
	 * ones compliment) and more would be useful, it does not 
	 * exist yet though. */
	if (!load)
		return -1;
	for (size_t i = 0; i < nelems; i++)
		s->m[L(s, s->load++)] = load[i] & msk(s->N);
	return 0;
}

int main(int argc, char **argv) {
	sys_getopt_t opt = { .init = 0, .error = stdout, };
	char *save = NULL;
	subleq_t s = { 
		.N = 16, .sz = SZ,
		.in = stdin, .out = stdout, .trace = stderr,
       	};
	binary(stdin);
	binary(stdout);
	binary(stderr);
	terminal.in = stdin;
	terminal.out = stdout;
	atexit(restore);

	for (int ch = 0; (ch = sys_getopt(&opt, argc, argv, "rzhHtdkc:s:b:x:n:p:S:a:A:")) != -1;) {
		switch (ch) {
		case 'h': return cli_help(stderr) < 0 ? 1 : 0;
		case 'H': return assembly_help(stderr) < 0 ? 1 : 0;
		case 'k': s.non_blocking = 1; break;
		case 't': s.tron++; break;
		case 'd': s.debug = 1; s.debug_on_halt = 1; break;
		case 'c': s.cycles = number(opt.arg); break;
		case 'b': s.meta[L(&s, number(opt.arg))] |= META_BPS; break;
		case 's': save = opt.arg; break;
		case 'n': s.N = atoi(opt.arg); break;
		case 'x': if (subleq_examples(&s, opt.arg) < 0) return 1; break;
		case 'z': s.optimize = 1; break;
		case 'p': s.meta[L(&s, number(opt.arg))] |= META_PRN; break;
		case 'r': s.stats = 1; break;
		case 'S': { long sz = number(opt.arg); if (sz <= 0 || sz > SZ) { (void)fprintf(stderr, "Incorrect size: %ld\n", sz); return 1; } s.sz = sz; } break;
		case 'a': return assemble_from_file(&s, opt.arg, 1, 1, 0) < 0 ? 1 : 0;
		case 'A': if (assemble_from_file(&s, opt.arg, 0, 0, s.apc) < 0) return 1; break;
		default: return 1;
		}
	}
	if (s.N < 8 || s.N > 64)
		return 2;

	for (int i = opt.index; i < argc; i++)
		if (subleq_load(&s, s.load, argv[i]) < 0) {
			(void)fprintf(stderr, "Load '%s' failed\n", argv[i]);
			return 3;
		}
	/* We could implement SUBNEG and other variants, if we wanted */
	s.max = s.load;
	const int r = subleq(&s, &terminal);

	for (size_t i = 0; i < s.sz; i++)
		if (s.meta[i] & META_PRN)
			if (printer(&s, "%02lx:%02lx\n", (long)i, (long)s.m[i]) < 0)
				return 4;

	if (save)
		if (subleq_save(&s, 0, s.max, save) < 0) {
			(void)fprintf(stderr, "Save '%s' failed\n", save);
			return 5;
		}
	return r < 0 ? 6 : 0;
}

