CFLAGS=-std=c11 -Wall -Wextra -pedantic -O3 -march=native
IMAGE=../../subleq.dec
VER=2
TARGET=o${VER}
TIME=5000
SHELL=/bin/bash
.PHONY: all test run speed forth

all run test: speed

speed: ${TARGET} ${IMAGE}
	time -p (echo -e "${TIME} ms\nbye\n" | ./${TARGET} ${IMAGE})

%.s: %.c
	${CC} -S -masm=intel -Og -fverbose-asm $< -o $@

forth: ${TARGET} ${IMAGE}
	./${TARGET} ${IMAGE}

clean:
	git clean -dffx


