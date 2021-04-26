#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>

#define ESCAPE (27)
#define DELETE (127)
#define BACKSPACE (8)

#ifdef __unix__
#include <unistd.h>
#include <termios.h>
static struct termios oldattr, newattr;

static void restore(void) {
	tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
}

static int setup(void) {
	tcgetattr(STDIN_FILENO, &oldattr);
	newattr = oldattr;
	newattr.c_iflag &= ~(ICRNL);
	newattr.c_lflag &= ~(ICANON | ECHO);
	newattr.c_cc[VMIN]  = 0;
	newattr.c_cc[VTIME] = 0;
	tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
	atexit(restore);
	return 0;
}

static int getch(void) {
	static int init = 0;
	if (!init) {
		setup();
		init = 1;
	}
	unsigned char r = 0;
	if (read(STDIN_FILENO, &r, 1) != 1)
		return -1;
	return r;
}

static int putch(int c) {
	int res = putchar(c);
	fflush(stdout);
	return res;
}

static void sleep_ms(unsigned ms) {
	usleep((unsigned long)ms * 1000);
}
#else
#ifdef _WIN32

extern int getch(void);
extern int putch(int c);
static void sleep_ms(unsigned ms) {
	usleep((unsigned long)ms * 1000);
}
#else
static int getch(void) {
	return getchar();
}

static int putch(const int c) {
	return putchar(c);
}

static void sleep_ms(unsigned ms) {
	(void)ms;
}
#endif
#endif /** __unix__ **/

static int wrap_getch(void) {
	const int ch = getch();
	if (ch == EOF) {
		sleep_ms(1);
	}
	if (ch == ESCAPE)
		exit(0);
	return ch == DELETE ? BACKSPACE : ch;
}

#define SZ   (32768)
#define L(X) ((X)%SZ)
int main(int s, char **v)
{
	static uint16_t m[SZ];
	uint16_t pc = 0;
	for (int i = 1, d = 0; i < s; i++) {
		FILE *f = fopen(v[i], "r");
		if (!f)
			return 1;
		while (fscanf(f, "%d", &d) > 0)
			m[L(pc++)] = d;
		if (fclose(f) < 0)
			return 2;
	}
	for (pc = 0; !(pc & 32768);) {
		uint16_t a = m[L(pc++)], b = m[L(pc++)], c = m[L(pc++)];
		if (a == 65535) {
			m[L(b)] = wrap_getch();
		} else if (b == 65535) {
			if (putch(m[L(a)]) < 0)
				return 3;
		} else {
			uint16_t r = m[L(b)] - m[L(a)];
			if (r & 32768 || r == 0)
				pc = c;
			m[L(b)] = r;
		}
	}
	return 0;
}
