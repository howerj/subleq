#include <assert.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#define SZ   (1<<16)
#define L(X) ((X)%SZ)
#define EN   __LINE__
#define MAX(X, Y) ((X) > (Y) ? (X) : (Y))

typedef struct {
	char *arg;   /* parsed argument */
	int error,   /* turn error reporting on/off */
	    index,   /* index into argument list */
	    option,  /* parsed option */
	    reset;   /* set to reset */
	char *place; /* internal use: scanner position */
	int  init;   /* internal use: initialized or not */
} subleq_getopt_t;   /* getopt clone; with a few modifications */

/* Adapted from: <https://stackoverflow.com/questions/10404448> */
static int subleq_getopt(subleq_getopt_t *opt, const int argc, char *const argv[], const char *fmt) {
	assert(opt);
	assert(fmt);
	assert(argv);
	enum { BADARG_E = ':', BADCH_E = '?' };

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
			(void)fprintf(stderr, "illegal option -- %c\n", opt->option);
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
				(void)fprintf(stderr, "option requires an argument -- %c\n", opt->option);
			return BADCH_E;
		} else	{ /* white space */
			opt->arg = argv[opt->index];
		}
		opt->place = "";
		opt->index++;
	}
	return opt->option; /* dump back option letter */
}

int main(int argc, char **argv) {
	FILE *trace = stderr, *in = stdin, *out = stdout;
	static uint16_t m[SZ], bp[SZ];
	uint16_t pc = 0, max = 0;
	int i = 1, tron = 0;
	long cycles = 0, cnt = 0;
	char *save = NULL;
	subleq_getopt_t opt = { .init = 0, };

	/* TODO: interactive command line, start up in command line, SUBNEG and
	 * other variants, variable bitwidth, changeable memory size, help,
	 * project info, primitive assembler, disassembler...
	 *
	 * Commands:
	 * b <loc> <type>? | set break point
	 * ! <loc> <val>   | set memory location
	 * @ <loc>         | get memory location
	 * x <loc> <len>   | hex dump memory
	 * S <file>        | save to file
	 * s               | single step
	 * c               | continue
	 * C <cycles>      | continue to run for X cycles, then break
	 */
	for (int ch = 0; (ch = subleq_getopt(&opt, argc, argv, "tc:s:")) != -1; i++) {
		switch (ch) {
		case 'c': cycles = atol(opt.arg); i++; break;
		case 't': tron++; break;
		case 'b': bp[L(atol(opt.arg))] = 1; i++; break;
		case 's': save = opt.arg; i++; break;
		default:
			return EN;
		}
	}

	for (int d = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f)
			return EN;
		while (fscanf(f, "%d,", &d) > 0)
			m[L(pc++)] = d;
		if (fclose(f) < 0)
			return EN;
	}
	max = pc;

	for (pc = 0; !(pc & 0x8000u); cnt++) {
		if (tron > 0)
			if (fprintf(trace, "pc:%04x: ", (int)pc) < 0)
				return EN;
		if (cycles)
			if (cnt >= cycles)
				break;

		uint16_t a = m[L(pc++)];
		uint16_t b = m[L(pc++)];
		uint16_t c = m[L(pc++)];

		if (bp[L(pc)] || bp[L(pc + 1)] || bp[L(pc + 2)]) {
			/* TODO: Breakpoints on I/O, [a], [b], c, and pc */
		}

		if (tron > 0)
			if (fprintf(trace, "a=%04x b=%04x c=%04x ", (int)a, (int)b, (int)c) < 0)
				return EN;
		if (a == 65535) {
			m[L(b)] = fgetc(in);
			if (tron > 0)
				if (fprintf(trace, "i=%04x ", (int)m[L(b)]) < 0)
					return EN;
		} else if (b == 65535) {
			if (tron > 0)
				if (fprintf(trace, "o=%04x ", (int)m[L(a)]) < 0)
					return EN;
			if (fputc(m[L(a)], out) < 0)
				return EN;
			if (fflush(out) < 0)
				return EN;
		} else {
			uint16_t r = m[L(b)] - m[L(a)];
			if (tron > 0)
				if (fprintf(trace, "[a]=%04x [b]=%04x r=%04x ", (int)m[L(a)], (int)m[L(b)], (int)r) < 0)
					return EN;
			if (r & 32768 || r == 0) {
				if (tron > 0)
					if (fprintf(trace, "%c ", pc == c ? '=' : '!') < 0)
						return EN;
				pc = c;
			}
			max = MAX(L(b), max);
			m[L(b)] = r;
		}
		if (tron > 0)
			if (fputc('\n', trace) < 0)
				return EN;
	}
	if (save) {
		FILE *f = fopen(save, "wb");
		if (!f)
			return EN;
		for (size_t i = 0; i < max; i++) {
			if (fprintf(f, "%d\n", (int)m[i]) < 0)
				return EN;
		}
		if (fclose(f) < 0)
			return EN;
	}
	return 0;
}
