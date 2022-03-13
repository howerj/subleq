/* Author: Richard James Howe
 * Email: howe.r.j.89@gmail.com
 * Project: 16-bit SUBLEQ recompiler, a faster VM.
 *
 * This is a 16-bit SUBLEQ VM that attempts to optimize code
 * so it executes faster, finding common sequences of instructions
 * and turning them into a single instruction, it is brittle and
 * liable to break code (in part because any non trivial SUBLEQ
 * is highly self-modifying). Initially the idea was to make a Just
 * In Time compiler to recompile common expressions on the fly,
 * however, doing this ahead of time is simpler.
 *
 * TODO: Options, N-bit version, debugging options */
#include <stdint.h>
#include <stdio.h>
#include <stdarg.h>
#include <ctype.h>
#include <inttypes.h>
#define SZ   (32768)
#define L(X) ((X)%SZ)
//#define L(X) (X)
#define DEPTH (48)
enum {
	SUBLEQ, JMP, ADD, SUB, MOV, ZERO, PUT, GET, HALT,
	IJMP, ILOAD, ISTORE, INC, DEC, INV, /* MUL, DIV */

	MAX
};

static const char *names[] = {
	"SUBLEQ ", "JMP    ", "ADD    ", "SUB    ", "MOV    ", "ZERO   ", "PUT    ", "GET    ", "HALT   ",
	"IJMP   ", "ILOAD  ", "ISTORE ", "INC    ", "DEC    ", "INV    ", "MUL    ", "DIV    ", 
};

typedef struct {
	int instruction;
	uint16_t m, s, d;
} instruction_t;

typedef struct {
	int matches[MAX];
	int set[9];
	uint16_t v[9];
	unsigned char z_reg[SZ], one_reg[SZ], neg1_reg[SZ];
} optimizer_t;

static int match(optimizer_t *o, uint16_t *n, int sz, uint16_t pc, const char *s, ...) {
	va_list ap;
	int r = 0, i = 0, j = 0;
	for (int i = 0; i < 9; i++) {
		o->set[i] = 0;
		o->v[i] = 0;
	}
	va_start(ap, s);
	for (i = 0, j = 0; s[j] && i < sz; j++) {
		switch (s[j]) {
		case '0': case '1': case '2': case '3':
		case '4': case '5': case '6': case '7':
		case '8': case '9': {
			int p = s[j] - '0';
			if (o->set[p]) {
				if (n[i] != o->v[p]) goto end;
			} else {
				o->set[p] = 1;
				o->v[p] = n[i];
			}
			i++;
			break;
		}
		/* NB. Memory location zero must be zero in SUBLEQ image! */
		case 'Z': if (n[i] != 0) goto end; i++; break; 
		case 'N': if (n[i] != 65535) goto end; i++; break;
		case '>': if (n[i] != (pc + i + 1)) goto end; i++; break;
		case '%': {
			int q = va_arg(ap, int);
			if (n[i] != q)
				goto end;
			i++;
		} break;
		case '!': {
			  uint16_t *p = va_arg(ap, uint16_t*);
			  *p = n[i];
			  i++; 
		} break;
		case '?': i++; break;
		case ' ': case '\t': case '\n': case '\r': break;
		default: r = -1; goto end;
		}
	}
	while (isspace(s[j]))
		j++;
	r = (s[j] == 0) && (i <= sz);
end:
	va_end(ap);
	return r;
}

static long get(optimizer_t *o, char var) {
	if (var < '0' || var > '9' || o->set[var - '0'] == 0)
		return -1;
	return o->v[var - '0'];
}

/* This section pattern matches the code finding sequences of SUBLEQ
 * instructions against known instruction macros. It is essentially
 * a disassembler. It is liable not to work for every case, but will
 * do so for the code that *I* want to speed up. */
static int optimizer(optimizer_t *o, instruction_t *m, uint16_t pc) {

	for (uint16_t i = 0; i < pc; i++) {
		switch (m[i].m) {
		case 0: o->z_reg[i] = 1; break;
		case 1: o->one_reg[i] = 1; break;
		case 0xFFFF: o->neg1_reg[i] = 1; break;
		}
	}

	for (uint16_t i = 0; i < pc; i++) {
		uint16_t q0 = 0, q1 = 0;
		uint16_t n[DEPTH] = { 0, };

		for (size_t j = 0; j < DEPTH; j++)
			n[j] = m[L(i + j)].m;

		/* Largest instructions *must* go first */

		if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ> 11> ?Z> Z1> ZZ> 22> ?Z> Z2> ZZ> 33> !Z> Z3> ZZ>", &q0, &q1) == 1
			&& get(o, '0') == (i+(3*12)) && get(o, '1') == (i+(3*12)+1)) { // && n[3*14] == (i+(3*12)+7)) {
			m[L(i)].instruction = ISTORE;
			m[L(i)].d = L(q0);
			m[L(i)].s = L(q1);
			o->matches[ISTORE]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ> 11> ?Z> Z1> ZZ>", &q0) == 1 && get(o, '0') == (i + 15)) {
			m[L(i)].instruction = ILOAD;
			m[L(i)].d = L(get(o, '1'));
			m[L(i)].s = L(q0);
			o->matches[ILOAD]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ> ZZ>", &q0) == 1 && get(o, '0') == (i + (3*4) + 2)) {
			m[L(i)].instruction = IJMP;
			m[L(i)].d = L(q0);
			o->matches[IJMP]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ>", &q0) == 1) {
			m[L(i)].instruction = MOV;
			m[L(i)].d = L(get(o, '0'));
			m[L(i)].s = L(q0);
			o->matches[MOV]++;
			continue;
		}

		// TODO: (should match 1)
		if (match(o, n, DEPTH, i, "00> 10> 00> ?Z> Z0> ZZ> 21>") == 1) {
			//m[L(i)].instruction = INV;
			//m[L(i)].s = L(q0);
			o->matches[INV]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "!Z> Z!> ZZ>", &q0, &q1) == 1) {
			m[L(i)].instruction = ADD;
			m[L(i)].d = L(q1);
			m[L(i)].s = L(q0);
			o->matches[ADD]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "00>") == 1) {
			m[L(i)].instruction = ZERO;
			m[L(i)].d = L(get(o, '0'));
			o->matches[ZERO]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "ZZ!", &q0) == 1 && q0 >= SZ) {
			m[L(i)].instruction = HALT;
			o->matches[HALT]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "00!", &q0) == 1) {
			m[L(i)].instruction = JMP;
			m[L(i)].d = q0;
			m[L(i)].s = L(get(o, '0'));
			o->matches[JMP]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "N!>", &q0) == 1) {
			m[L(i)].instruction = GET;
			m[L(i)].d = L(q0);
			o->matches[GET]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "!N>", &q0) == 1) {
			m[L(i)].instruction = PUT;
			m[L(i)].s = L(q0);
			o->matches[PUT]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1 && o->neg1_reg[L(q0)]) {
			m[L(i)].instruction = INC;
			m[L(i)].d = L(q1);
			o->matches[INC]++;
			continue;
		}

		if (match(o, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1 && o->one_reg[L(q0)]) {
			m[L(i)].instruction = DEC;
			m[L(i)].d = L(q1);
			o->matches[DEC]++;
			continue;
		}
	
		if (match(o, n, DEPTH, i, "!!>", &q0, &q1) == 1 && q0 != q1) {
			m[L(i)].instruction = SUB;
			m[L(i)].d = L(q1);
			m[L(i)].s = L(q0);
			o->matches[SUB]++;
			continue;
		}

		o->matches[SUBLEQ]++;
	}
	return 0;
}

int main(int s, char **v) {
	static instruction_t m[SZ];
	static optimizer_t o = { .matches = { 0, }, };
	int64_t cnt[MAX] = { 0, };
	uint16_t pc = 0;
	const int dbg = 0, optimize = 1, stats = 1;
	for (int i = 1, d = 0; i < s; i++) {
		FILE *f = fopen(v[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%d", &d) > 0)
			m[L(pc++)].m = d;
		if (fclose(f) < 0)
			return 2;
	}

	if (optimize)
		if (optimizer(&o, m, pc) < 0)
			return 1;

	for (pc = 0; pc < SZ;) {
		const int instruction = m[pc].instruction;
		const uint16_t s = m[pc].s, d = m[pc].d;
		if (dbg) {
			if (fprintf(stderr, "{%ld:%d}", (long)pc, m[pc].instruction) < 0)
				return 1;
		}
		cnt[instruction % MAX]++;
		switch (instruction) {
		case SUBLEQ: {
			uint16_t a = m[pc++].m, b = m[L(pc++)].m, c = m[L(pc++)].m;
			if (a == 65535) {
				m[L(b)].m = getchar();
			} else if (b == 65535) {
				if (putchar(m[L(a)].m) < 0)
					return 3;
				if (fflush(stdout) < 0)
					return 4;
			} else {
				uint16_t r = m[L(b)].m - m[L(a)].m;
				if (r & 32768 || r == 0)
					pc = c;
				m[L(b)].m = r;
			}
			} 
			break;
		/* NB. We might be able to run more programs correctly
		 * if we disable these instructions if a write occurs
		 * within the bounds of an instruction macro, this would
		 * slow things down however. */
		case JMP: pc = d; m[s].m = 0; break;
		case MOV: m[d].m  = m[s].m; m[0].m = 0; pc += 12; break;
		case ADD: m[d].m += m[s].m; m[0].m = 0; pc += 9; break;
		case SUB: m[d].m -= m[s].m; pc += 3; break;
		case ZERO: m[d].m = 0; pc += 3; break;
		case IJMP: pc = m[d].m; m[0].m = 0; break;
		case ILOAD: m[d].m = m[L(m[s].m)].m; pc += 24; break;
		case ISTORE: m[L(m[d].m)].m = m[s].m; pc += 48; break;
		case PUT:
			if (putchar(m[L(m[pc].s)].m) < 0)
				return 3;
			if (fflush(stdout) < 0)
				return 4;
			pc += 3;
			break;
		case GET: m[m[pc].d].m = getchar(); pc += 3; break;
		case HALT: goto done;
		case INC: m[d].m++; pc += 3; break;
		case DEC: m[d].m--; pc += 3; break;
		case INV: m[d].m = ~m[d].m; pc += 21; break;
		default:
			return 5;
		}
	}
done:
	if (stats) {
		int64_t total = 0;
		for (int i = 0; i < MAX; i++)
			total += cnt[i];
		if (fprintf(stderr, "Total  ( Match) = % 10"PRId64"\n\n", total) < 0)
			return 1;
		for (int i = 0; i < MAX; i++)
			if (fprintf(stderr, "%s(% 6d) = % 10"PRId64"\t%.1f%%\n", names[i], o.matches[i], cnt[i], 100.0*((float)cnt[i])/(float)total) < 0)
				return 1;

	}
	return 0;
}
