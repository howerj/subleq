#include <stdio.h>

int main(void) {
	int d = 0;
	while (scanf("%d", &d) > 0)
		printf("%04x\n", d & 0xFFFF);
	return 0;
}
