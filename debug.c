#define PROJECT "N-Bit SUBLEQ Toolchain and Virtual Machine"
#define AUTHOR  "Richard James Howe"
#define REPO    "https://github.com/howerj/subleq"
#define EMAIL   "howe.r.j.89@gmail.com"
#define LICENSE "Public Domain / Unlicense for code only"
#define VERSION "1.0"
/* TODO: SUBNEG and other variants, changeable memory size,
 * primitive assembler, disassembler, 
 *
 * This version could replace "nbit.c"... 
 *
 * TODO:
 * - Add in recompiler, use as disassembler
 * - I/O better escape handling, terminal options via CLI, retry
 *   option, retry if not Escape
 * - Integrate assembler
 * - Annotated assembly
 * - Better help
 * - Gather more information about running environment (which
 *   cells are modified, etc).
 *
 */

#include <assert.h>
#include <ctype.h>
#include <inttypes.h>
#include <stdarg.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>


#define MAX_LINE (512)
#define EN   __LINE__
#define SZ   (1<<16)
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))
#define HI(X)  (1ull << ((X) - 1))
#define UNUSED(X) ((void)(X))
#define NELEMS(X) (sizeof (X) / sizeof((X)[0]))

#ifdef _WIN32 /* Used to unfuck file mode for "Win"dows. Text mode is for losers. */
#include <windows.h>
#include <io.h>
#include <fcntl.h>
static void binary(FILE *f) { _setmode(_fileno(f), _O_BINARY); } /* only platform specific code... */
#else
static inline void binary(FILE *f) { UNUSED(f); }
#endif

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

/* TODO: Remove setup and restore code, instead calling it
 * around the read? */

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
/* TODO: isatty, notes */
typedef struct { FILE *in, *out; } terminal_t;
extern int getch(void);
extern int putch(int c);
static int term_getch(terminal_t *t, FILE *in) { UNUSED(t); return getch(); }
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
} subleq_getopt_t;   /* getopt clone; with a few modifications */

/* Adapted from: <https://stackoverflow.com/questions/10404448> */
static int subleq_getopt(subleq_getopt_t *opt, const int argc, char *const argv[], const char *fmt) {
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

enum { 
	META_BPS = 1ull << 0,
	META_WRT = 1ull << 1,
	META_PRN = 1ull << 2,
};

typedef struct {
	uint64_t m[SZ], meta[SZ], N, pc, cycles, count, load, max;
	size_t sz;
	FILE *in, *out, *trace;
	int  error, tron;
	char prompt[MAX_LINE];
	unsigned debug: 1, 
		 exit_on_cycles :1, 
		 non_blocking :1,
		 exit_on_escape :1;
} subleq_t;

static inline uint64_t msk(int n) {
	return n < 64 ? 
		(1ull << n) + 0xFFFFFFFFFFFFFFFFull :
		0xFFFFFFFFFFFFFFFFull;
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

static int cmd_continue(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc > 1)
		s->pc = atol(argv[1]);
	s->debug = 0;
	return 0;
}

static int cmd_dump(subleq_t *s, int argc, char **argv) {
	assert(s);
	assert(argc > 0);
	if (argc != 3) {
		printer(s, "Usage: %s location range\n", argv[0]);
		return 1;
	}
	const uint64_t addr = atol(argv[1]), len = atol(argv[2]);
	const size_t width = 4;
	size_t bytes = 8;
	const char *fmt = "%016lx ";
	if (s->N <= 32) { fmt = "%08lx "; bytes = 4; }
	if (s->N <= 16) { fmt = "%04lx "; bytes = 2; }
	if (s->N <=  8) { fmt = "%02lx "; bytes = 1; }

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
	assert(s);
	if (argc < 2) {
		printer(s, "Usage: %s locations\n", argv[0]);
		return 1;
	}
	for (int i = 1; i < argc; i++) {
		const uint64_t addr = atol(argv[i]);
		s->meta[L(s, addr)] |= META_BPS;
	}
	return 0;
}

static int cmd_breakp_clear(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	for (size_t i = 0; i < s->sz; i++) {
		s->meta[i] &= ~META_BPS;
	}
	return 0;
}

static int cmd_breakp_list(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	printer(s, "pc=%lx cycles=%ld bps: ", (long)s->pc, (long)s->count);
	for (size_t i = 0; i < s->sz; i++)
		if (s->meta[i] & META_BPS)
			printer(s, "%lx ", (long)i);
	printer(s, "\n");
	return 0;
}


static int cmd_store(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc != 3) {
		printer(s, "Usage: %s location value\n", argv[0]);
		return 1;
	}
	const uint64_t loc = atol(argv[1]), val = atol(argv[2]);
	s->m[L(s, loc)] = val & msk(s->N);
	return 0;
}

static int cmd_read(subleq_t *s, int argc, char **argv) {
	if (argc != 2) {
		printer(s, "Usage: %s location\n", argv[0]);
		return 1;
	}
	const uint64_t loc = atol(argv[1]);
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
	if (argc < 2) {
		printer(s, "Usage: %s location\n", argv[0]);
		return 1;
	}
	/* NB. Could offer to save to different formats */
	const long start  = argc > 2 ? atol(argv[2]) : 0;
	const long length = argc > 3 ? atol(argv[3]) : (long)s->max;
	if (subleq_save(s, start, length, argv[1]) < 0) {
		printer(s, "save to file '%s' failed\n", argv[1]);
		return 1;
	}
	return 0;
}

static int cmd_prompt(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc != 2) {
		printer(s, "Usage: %s new-prompt\n", argv[0]);
		return 1;
	}
	const size_t len = strlen(argv[1]) + 1;
	assert(sizeof (s->prompt) >= len);
	memcpy(s->prompt, argv[1], len);
	return 0;
}

static int cmd_fill(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 3) {
		printer(s, "Usage: %s addr length string?\n", argv[0]);
		return 1;
	}
	const size_t start = atol(argv[1]);
	const size_t length = atol(argv[2]);
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
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	exit(0);
	return 0;
}

static int cmd_move(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 4) {
		printer(s, "Usage: %s location length location\n", argv[0]);
		return 1;
	}
	const size_t start = atol(argv[1]);
	const size_t length = atol(argv[2]);
	const size_t dest = atol(argv[3]);
	for (size_t i = 0; i < length; i++)
		s->m[L(s, dest + i)] = s->m[L(s, start + i)];
	return 0;
}

static int cmd_compare(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 4) {
		printer(s, "Usage: %s location length location\n", argv[0]);
		return 1;
	}
	const size_t start = atol(argv[1]);
	const size_t length = atol(argv[2]);
	const size_t dest = atol(argv[3]);
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

static int cli_help(FILE *out) {
	assert(out);

	const char *help = "\n\
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
Program returns non-zero on failure, zero on success.\n\
\n\n\
Options:\n\n\
\t-h\n\t\tDisplay this help message and exit.\n\
\t-k\n\t\tTurn non-blocking terminal input on.\n\
\t-t\n\t\tTurn tracing on.\n\
\t-d\n\t\tRun debugger first before execution.\n\
\t-c num\n\t\tRun for 'num' cycles before halting into debugger.\n\
\t-b break-point\n\t\tAdd break point.\n\
\t-s file.dec\n\t\tSave to file after running.\n\
\t-n num (8-64)\n\t\tSet SUBLEQ VM to bit-width, inclusive.\n\
\t-p num\n\t\tPrint out cell contents after exiting VM.\n\
\t-x forth,hi,echo,halt,self\n\t\tLoad example image from internal storage.\n";

	if (fprintf(out, help, PROJECT, AUTHOR, EMAIL, REPO, VERSION) < 0)
		return -1;
	return 0;
}

static int debug_help(FILE *out) {
	assert(out);
/* TODO:
 Implement MS-DOS Debug Commands <https://montcs.bloomu.edu/Information/LowLevel/DOS-Debug.html>

  - assemble    A  [address]
  - *compare     C  range       address
  - *dump        D  [range]
  - *enter       E  address     [list]
  - *fill        F  range       list
  - *go          G  [=address]  [addresses]
  - hex         H  value1      value2
  - *input       I  port
  - *load        L  [address]   [drive]      [firstsector]  [number]
  - *move        M  range       address
  - name        N  [pathname]  [arglist]
  - *output      O  port        byte
  - proceed     P  [=address]  [number]
  - *quit        Q
  - *register    R  [register]
  - search      S  range       list
  - trace       T  [=address]  [number]
  - unassemble  U  [range]
  - *write       W  [address]   [drive]      [firstsector]  [number] */
	const char *help = "";

	if (fprintf(out, "%s", help) < 0)
		return -1;
	return 0;
}

static int cmd_help(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	return debug_help(s->out);
}

static int cmd_load(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 3) {
		printer(s, "Usage: %s address file", argv[0]);
		return 1;
	}
	s->load = 0;
	const long addr = atol(argv[1]);
	if (subleq_load(s, addr, argv[2]) < 0) {
		printer(s, "File load of '%s' failed", argv[2]);
		return 1;
	}
	return 0;
}

static int cmd_register(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	const long pc = s->pc;
	const long a = s->m[L(s, pc + 0)];
	const long b = s->m[L(s, pc + 1)];
	const long c = s->m[L(s, pc + 2)];
	printer(s, "pc=%04lx, a=%04lx, b=%04lx, c=%04lx, N=%d, load=%ld\n", pc, a, b, c, (int)s->N, (long)s->load);
	return 0;
}


/* TODO: string version */
static int cmd_search(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 4) {
		printer(s, "Usage: %s location length values...\n", argv[0]);
		return 1;
	}
	const size_t start = atol(argv[1]);
	const size_t length = atol(argv[2]);

	for (int i = 3; i < argc; i++) {
		const long find = atol(argv[i]);
		for (size_t j = 0; j < length; j++) {
			const long addr = L(s, start + j);
			if (find == (long)s->m[addr])
				printer(s, "%04lx:%04lx\n", addr, find);
		}
	}


	return 0;
}

static int cmd_assemble(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	return 0;
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



#define MD(X) ((X)%SZ)
#define DEPTH (3*64)

enum {
	SUBLEQ, JMP, ADD, SUB, MOV,
	ZERO, PUT, GET, HALT,
	IJMP, ILOAD, ISTORE, INC, DEC,
	INV, DOUBLE, LSHIFT,

	MAX
};

static const char *names[] = {
	"SUBLEQ ", "JMP    ", "ADD    ", "SUB    ",
	"MOV    ", "ZERO   ", "PUT    ", "GET    ",
	"HALT   ", "IJMP   ", "ILOAD  ", "ISTORE ",
	"INC    ", "DEC    ", "INV    ", "DOUBLE ",
	"LSHIFT ",
};

typedef struct {
	int instruction;
	uint64_t m, s, d;
} instruction_t;

typedef struct {
	int matches[MAX];
	int set[9];
	uint64_t v[9];
	unsigned char z_reg[SZ], one_reg[SZ], neg1_reg[SZ];
	clock_t start, end;
	int64_t cnt[MAX];
} optimizer_t;

static int match(subleq_t *s, optimizer_t *o, uint64_t *n, int sz, uint64_t pc, const char *st, ...) {
	va_list ap;
	int r = 0, i = 0, j = 0;
	for (int i = 0; i < 9; i++) {
		o->set[i] = 0;
		o->v[i] = 0;
	}
	va_start(ap, st);
	for (i = 0, j = 0; st[j] && i < sz; j++) {
		switch (st[j]) {
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
		case '8': case '9': {
			int p = st[j] - '0';
			if (o->set[p]) {
				if (n[i] != o->v[p]) goto end;
			} else {
				o->set[p] = 1;
				o->v[p] = n[i];
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
static int optimizer(subleq_t *s, optimizer_t *o, instruction_t *m, uint64_t pc) {
	assert(s);
	assert(o);
	assert(m);

	for (uint64_t i = 0; i < pc; i++) {
		switch (m[i].m) {
		case 0: o->z_reg[i] = 1; break;
		case 1: o->one_reg[i] = 1; break;
		/*case 0xFFFF: o->neg1_reg[i] = 1; break;*/
		}
		if (msk(s->N) == m[i].m)
			o->neg1_reg[i] = 1;
	}

	for (uint64_t i = 0; i < pc; i++) {
		uint64_t q0 = 0, q1 = 0;
		uint64_t n[DEPTH] = { 0, };

		for (size_t j = 0; j < DEPTH; j++)
			n[j] = m[MD(i + j)].m;

		/* Largest instructions *must* go first */

		if (match(s, o, n, DEPTH, i, "00> !Z> Z0> ZZ> 11> ?Z> Z1> ZZ> 22> ?Z> Z2> ZZ> 33> !Z> Z3> ZZ>", &q0, &q1) == 1
			&& get(o, '0') == (i+(3*12))
			&& get(o, '1') == (i+(3*12)+1)) {
			m[MD(i)].instruction = ISTORE;
			m[MD(i)].d = MD(q0);
			m[MD(i)].s = MD(q1);
			o->matches[ISTORE]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "00> !Z> Z0> ZZ> 11> ?Z> Z1> ZZ>", &q0) == 1 && get(o, '0') == (i + 15)) {
			m[MD(i)].instruction = ILOAD;
			m[MD(i)].d = MD(get(o, '1'));
			m[MD(i)].s = MD(q0);
			o->matches[ILOAD]++;
			continue;
		}

		uint64_t shift = 0, l = 0, dest = 0;
		for (l = 0; l < DEPTH; l += 9) {
			if (match(s, o, n+l, DEPTH-l, i+l, "!Z> Z!> ZZ>", &q0, &q1) == 1 && q0 == q1) {
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
			m[MD(i)].instruction = LSHIFT;
			m[MD(i)].d = MD(dest);
			m[MD(i)].s = shift;
			o->matches[LSHIFT]++;
			continue;
		}


		if (match(s, o, n, DEPTH, i, "00> 10> 11> 2Z> Z1> ZZ> !1>", &q0) == 1
				&& o->one_reg[q0]) {
			m[MD(i)].instruction = INV;
			m[MD(i)].d = MD(get(o, '1'));
			o->matches[INV]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "00> !Z> Z0> ZZ> ZZ>", &q0) == 1 && get(o, '0') == (i + (3*4) + 2)) {
			m[MD(i)].instruction = IJMP;
			m[MD(i)].d = MD(q0);
			o->matches[IJMP]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "00> !Z> Z0> ZZ>", &q0) == 1) {
			m[MD(i)].instruction = MOV;
			m[MD(i)].d = MD(get(o, '0'));
			m[MD(i)].s = MD(q0);
			o->matches[MOV]++;
			continue;
		}

		/* We should match multiple ones in a row and
			* turn them into a left shift */
		if (match(s, o, n, DEPTH, i, "!Z> Z!> ZZ>", &q0, &q1) == 1 && q0 == q1) {
			m[MD(i)].instruction = DOUBLE;
			m[MD(i)].d = MD(q1);
			m[MD(i)].s = MD(q0);
			o->matches[DOUBLE]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "!Z> Z!> ZZ>", &q0, &q1) == 1) {
			m[MD(i)].instruction = ADD;
			m[MD(i)].d = MD(q1);
			m[MD(i)].s = MD(q0);
			o->matches[ADD]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "00>") == 1) {
			m[MD(i)].instruction = ZERO;
			m[MD(i)].d = MD(get(o, '0'));
			o->matches[ZERO]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "ZZ!", &q0) == 1 && q0 >= SZ) {
			m[MD(i)].instruction = HALT;
			o->matches[HALT]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "00!", &q0) == 1) {
			m[MD(i)].instruction = JMP;
			m[MD(i)].d = q0;
			m[MD(i)].s = MD(get(o, '0'));
			o->matches[JMP]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "N!>", &q0) == 1) {
			m[MD(i)].instruction = GET;
			m[MD(i)].d = MD(q0);
			o->matches[GET]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "!N>", &q0) == 1) {
			m[MD(i)].instruction = PUT;
			m[MD(i)].s = MD(q0);
			o->matches[PUT]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1 && o->neg1_reg[MD(q0)]) {
			m[MD(i)].instruction = INC;
			m[MD(i)].d = MD(q1);
			o->matches[INC]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1 && o->one_reg[MD(q0)]) {
			m[MD(i)].instruction = DEC;
			m[MD(i)].d = MD(q1);
			o->matches[DEC]++;
			continue;
		}

		if (match(s, o, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1) {
			m[MD(i)].instruction = SUB;
			m[MD(i)].d = MD(q1);
			m[MD(i)].s = MD(q0);
			o->matches[SUB]++;
			continue;
		}

		o->matches[SUBLEQ]++;
	}
	return 0;
}

static int report(optimizer_t *o) {
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
		if (fprintf(e, "| %s| % 6d | % 12"PRId64" | % 7.1f%% |\n", names[i], o->matches[i], o->cnt[i], 100.0*((float)o->cnt[i])/(float)total) < 0)
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

int subleq_recompiler(subleq_t *s, terminal_t *t) {
	instruction_t m[SZ];
	optimizer_t o = { .matches = { 0, }, };
	uint64_t pc = 0;
	const int dbg = 0, optimize = 1, stats = 1;
	
	for (size_t i = 0; i < SZ; i++)
		m[i].m = s->m[i];

	if (optimize)
		if (optimizer(s, &o, m, s->load) < 0)
			return 1;
	o.start = clock();
	for (; pc < SZ && (pc != msk(s->N)) && !(s->error); s->count++) {
		const int instruction = m[pc].instruction;
		const uint64_t src = m[pc].s, d = m[pc].d;
		if (dbg) {
			if (fprintf(stderr, "{%ld:%d}", (long)pc, m[pc].instruction) < 0)
				return 1;
				/* Could return __LINE__ for simple debugging, but return val is limited to 255 usually */
		}
		if (stats) {
			o.cnt[instruction/*% MAX*/]++;
		}
		switch (instruction) {
		case SUBLEQ: { /* OG Instruction */
			uint64_t a = m[pc++].m, b = m[MD(pc++)].m, c = m[MD(pc++)].m;
			if (a == msk(s->N)) {
				m[MD(b)].m = subleq_getch(s, t) & msk(s->N);
			} else if (b == msk(s->N)) {
				if (term_putch(t, m[MD(a)].m) < 0)
					return 3;
			} else {
				uint64_t r = m[MD(b)].m - m[MD(a)].m;
				r &= msk(s->N);
				if (r & HI(s->N) || r == 0)
					pc = c;
				m[MD(b)].m = r;
			}
			}
			break;
		/* NB. We might be able to run more programs
			* correctly if we disable these instructions if
			* a write occurs within the bounds of an
			* instruction macro, this would slow things down
			* however. */
		case JMP: pc = d; m[src].m = 0; break;
		case MOV: m[d].m	= m[src].m; pc += 12; break;
		case ADD: m[d].m += m[src].m; pc += 9; break;
		case DOUBLE: m[d].m <<= 1; pc += 9; break;
		case LSHIFT: m[d].m <<= src; pc += 9 * src; break;
		case SUB: m[d].m -= m[src].m; pc += 3; break;
		case ZERO: m[d].m = 0; pc += 3; break;
		case IJMP: pc = m[d].m;	break;
		case ILOAD: m[d].m = m[MD(m[src].m)].m; pc += 24;
			break;
		case ISTORE: m[MD(m[d].m)].m = m[src].m; pc += 48;
			break;
		case PUT:
			if (term_putch(t, m[m[MD(pc)].s].m) < 0)
				return 3;
			pc += 3;
			break;
		case GET: m[m[pc].d].m = subleq_getch(s, t) & msk(s->N); pc += 3; break;
		case HALT: goto done;
		case INC: m[d].m++; pc += 3; break;
		case DEC: m[d].m--; pc += 3; break;
		case INV: m[d].m = ~m[d].m; pc += 21; break;
		default:
			return 5;
		}
	}
done:
	o.end = clock();
	if (stats)
	if (report(&o) < 0)
		return 1;
	return 0;
}

static int cmd_disassemble(subleq_t *s, int argc, char **argv) {
	assert(s);
	UNUSED(argc);
	UNUSED(argv);
	return 0;
}

static int cmd_input(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 2) {
		printer(s, "Usage: %s location\n");
		return 1;
	}
	s->m[L(s, atol(argv[1]))] = fgetc(s->in);
	return 0;
}

static int cmd_output(subleq_t *s, int argc, char **argv) {
	assert(s);
	if (argc < 2) {
		printer(s, "usage: %s location\n");
		return 1;
	}
	if (fputc(s->m[L(s, atol(argv[1]))], s->out) < 0)
		return 1;
	return 0;
}

static int command(subleq_t *s) {
	assert(s);
	char buffer[MAX_LINE] = { 0, };
	char *argv[(MAX_LINE / 2) - 1] = { NULL, };
	int argc = 0;
	if (s->prompt[0] == 0) {
		static const char *prompt = "sq> ";
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
	printer(s, "%s", s->prompt);
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

	/* NB. subleq_getopt could be reused for the command processing... */

	struct {
		const char *name, *alt;
		int (*fn)(subleq_t *s, int argc, char **argv);
	} cmds[] = { 
		{ .name = "continue",             .fn = cmd_continue, },
		{ .name = "go",         .alt = "g", .fn = cmd_continue, },
		//{ .name = "hex",      .alt = "h", .fn = cmd_hex, },
		{ .name = "dump",       .alt = "d", .fn = cmd_dump, },
		{ .name = "break",      .alt = "b", .fn = cmd_breakp, },
		{ .name = "clear",                .fn = cmd_breakp_clear, },
		{ .name = "list",       .alt = "?", .fn = cmd_breakp_list, },
		{ .name = "enter",      .alt = "e", .fn = cmd_store, },
		{ .name = "read",       .alt = "@", .fn = cmd_read, },
		{ .name = "write",      .alt = "w", .fn = cmd_save, },
		{ .name = "load",       .alt = "l", .fn = cmd_load, },
		{ .name = "help",       .alt = "h", .fn = cmd_help, },
		{ .name = "prompt",                 .fn = cmd_prompt, },
		{ .name = "fill",       .alt = "f", .fn = cmd_fill, },
		{ .name = "quit",       .alt = "q", .fn = cmd_quit, },
		{ .name = "move",       .alt = "m", .fn = cmd_move, },
		{ .name = "compare",    .alt = "c", .fn = cmd_compare, },
		{ .name = "register",   .alt = "r", .fn = cmd_register, },
		{ .name = "search",     .alt = "s", .fn = cmd_search, },
		{ .name = "assemble",   .alt = "a", .fn = cmd_assemble, },
		{ .name = "input",      .alt = "i", .fn = cmd_input, },
		{ .name = "output",     .alt = "o", .fn = cmd_output, },
		{ .name = "unassemble", .alt = "u", .fn = cmd_disassemble, },
		{ .fn = NULL }, 
	};

	for (int i = 0; cmds[i].fn; i++) {
		if (!strcmp(argv[0], cmds[i].name) || (cmds[i].alt && !strcmp(argv[0], cmds[i].alt))) {
			if (cmds[i].fn(s, argc, argv) < 0) {
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

static int subleq_break(subleq_t *s) {
	assert(s);
	if (s->meta[L(s, s->pc + 0)] & META_BPS)
		return 1;
	if (s->meta[L(s, s->pc + 1)] & META_BPS)
		return 1;
	if (s->meta[L(s, s->pc + 2)] & META_BPS)
		return 1;
	if (s->debug)
		return 1;
	return 0;
}

static int subleq(subleq_t *s, terminal_t *t) {
	assert(s);
	assert(t);
	assert(s->N >= 8 && s->N <= 64);
	assert(s->in);
	assert(s->out);
	for (; s->pc < SZ && (s->pc != msk(s->N)) && !(s->error); s->count++) {
		tracer(s, "pc:%04lx: ", (long)s->pc);
		if (s->cycles) {
			if (s->count >= s->cycles) {
				if (s->exit_on_cycles)
					break;
				if (command(s) < 0)
					return 0;
			}
		} else if (subleq_break(s)) {
			if (command(s) < 0)
				return 0;
		}

		uint64_t a = s->m[L(s, s->pc++)];
		uint64_t b = s->m[L(s, s->pc++)];
		uint64_t c = s->m[L(s, s->pc++)];
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
				return -2;
			}
		} else {
			uint64_t r = s->m[lb] - s->m[la];
			r &= msk(s->N);
			tracer(s, "[a]=%04lx [b]=%04lx r=%04lx ", (long)s->m[la], (long)s->m[lb], (long)r);
			if (r & HI(s->N) || r == 0) {
				tracer(s, "%c ", s->pc == c ? '=' : '!');
				s->pc = c;
			}
			s->max = MAX(lb, s->max);
			s->meta[lb] |= META_WRT;
			s->m[lb] = r;
		}
		tracer(s, "\n");
	}
	return s->error;
}

static terminal_t terminal = { .in = NULL, };

static void restore(void) {
	term_restore(&terminal);
}


/* TODO: Add self-interpreter, bit width test? */
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
	}
	if (!load)
		return -1;
	for (size_t i = 0; i < nelems; i++)
		s->m[L(s, s->load++)] = load[i] & msk(s->N);
	return 0;
}

int main(int argc, char **argv) {
	subleq_getopt_t opt = { .init = 0, .error = stdout, };
	int i = 1, recompile = 0;
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

	for (int ch = 0; (ch = subleq_getopt(&opt, argc, argv, "zhtdkc:s:b:x:n:p:")) != -1; i++) {
		switch (ch) {
		case 'h': cli_help(stderr); return 0;
		case 'k': s.non_blocking = 1; break;
		case 't': s.tron++; break;
		case 'd': s.debug = 1; break;
		case 'c': s.cycles = atol(opt.arg); i++; break;
		case 'b': s.meta[L(&s, atol(opt.arg))] |= META_BPS; i++; break;
		case 's': save = opt.arg; i++; break;
		case 'n': s.N = atoi(opt.arg); i++; break;
		case 'x': if (subleq_examples(&s, opt.arg) < 0) return 1; i++; break;
		case 'z': recompile = 1; break;
		case 'p': s.meta[L(&s, atol(opt.arg))] |= META_PRN; i++; break;
		default: return 1;
		}
	}
	if (s.N < 8 || s.N > 64)
		return EN;

	for (; i < argc; i++)
		if (subleq_load(&s, s.load, argv[i]) < 0)
			return 1;
	s.max = s.load;
	const int r = recompile ? subleq_recompiler(&s, &terminal) : subleq(&s, &terminal);

	for (size_t i = 0; i < s.sz; i++)
		if (s.meta[i] & META_PRN)
			printer(&s, "%02lx:%02lx\n", (long)i, (long)s.m[i]);

	if (save)
		if (subleq_save(&s, 0, s.max, save) < 0)
			return 4;

	return r < 0 ? 5 : 0;
}

