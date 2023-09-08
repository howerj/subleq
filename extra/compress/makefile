CFLAGS=-Wall -Wextra -pedantic -O2 -std=c99
VER=2

.PHONY: all run test forth clean

all run test: ${VER}c ${VER}d subleq.dec
	./${VER}c subleq.dec ${VER}c.dec
	./${VER}d ${VER}c.dec ${VER}d.dec
	diff -w subleq.dec ${VER}d.dec

forth: subleq subleq.dec
	./subleq subleq.dec

clean:
	git clean -dffx
