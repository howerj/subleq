#include <stdio.h>
#include <stdint.h>
#include <stdlib.h>
/* TODO: Remove this junk */
#if defined(unix) || defined(__unix__) || defined(__unix) || (defined(__APPLE__) && defined(__MACH__))
#include <unistd.h>
#include <termios.h>
int getch(void) {
	struct termios oldattr, newattr;
	if (tcgetattr(STDIN_FILENO, &oldattr) < 0)
		goto fail;
	newattr = oldattr;
	newattr.c_iflag &= ~(ICRNL);
	newattr.c_lflag &= ~(ICANON | ECHO);
	if (tcsetattr(STDIN_FILENO, TCSANOW, &newattr) < 0)
		goto fail;
	const int ch = getchar();
	if (tcsetattr(STDIN_FILENO, TCSANOW, &oldattr) < 0)
		goto fail;
	if (ch == 0x1b) 
		exit(0);
	return ch == 127 ? 8 : ch;
fail:
	exit(1);
	return -1;
}

int putch(int c) { 
	if (putchar(c) < 0) return -1; 
	if (fflush(stdout) < 0) return -1; 
	return 0;
}
#elif defined(_WIN32)
extern int getc(void);
extern int putch(int c);
#else
int getc(void) { return getchar(); }
int putch(int c) { 
	if (putchar(c) < 0) return -1; 
	if (fflush(stdout) < 0) return -1; 
	return 0;
}
#endif
int main(int argc, char **argv) { /* 16-bit SUBLEQ OISC */
	static uint16_t m[UINT16_MAX], pc = 0;
	for (int i = 1, d = 0, j = 0; i < argc; i++) {
		FILE *f = fopen(argv[i], "r");
		if (!f) return 1;
		while (fscanf(f, "%d", &d) > 0)
			m[j++ % UINT16_MAX] = d;
		if (fclose(f) < 0) return 2;
	}
	do {
		const uint16_t a = m[pc++], b = m[pc++], c = m[pc++];
		if (a == 0xFFFFu) { m[b] = (int16_t)getch(); }
		else if (a == 0xFFFE) { m[b] = m[b] >> 1; } /* TODO: Remove temporary instruction */
		else if (a == 0xFFFD) { m[b] = m[b] << 1; } /* TODO: Remove temporary instruction */
		else if (a == 0xFFFC) { m[b] = m[b] & 0x8000 ? 0xFFFF : 0; } /* TODO: Remove temporary instruction */
		else if (a == 0xFFFB) { m[b] = m[b] & 0x0001 ? 0xFFFF : 0; } /* TODO: Remove temporary instruction */
		else if (b == 0xFFFFu) { if (putch(m[a]) < 0) return 3; }
		else {
			const uint16_t r = m[b] - m[a];
			if (r & 0x8000u || r == 0u) pc = c;
			m[b] = r;
		}
	} while (!(pc & 0x8000u));
	return 0;
}
