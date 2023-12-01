CFLAGS=-Wall -Wextra -pedantic -O2 -std=c99
VER=4
IMAGE=../../subleq.dec
VMSRC=../../subleq.c
VMOBJ=${VMSRC:.c=.o}

.PHONY: all run test forth clean

all run test: ${VER} ${IMAGE}
	./${VER} compress ${IMAGE} ${VER}c.dec
	./${VER} decompress ${VER}c.dec ${VER}d.dec
	diff -w ${IMAGE} ${VER}d.dec

${VER}: ${VER}.c c.o
	${CC} ${CFLAGS} $^ -o $@

subleq: ${VMSRC}
	${CC} ${CFLAGS} $^ -o $@

forth: subleq ${IMAGE}
	./subleq ${IMAGE}

clean:
	git clean -dffx
