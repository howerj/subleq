CFLAGS=-Wall -Wextra -pedantic -O2 -std=c99
VER=4
IMAGE=../../subleq.dec
VMSRC=../../subleq.c
VMOBJ=${VMSRC:.c=.o}

.PHONY: all run test forth clean

all run test: ${VER}c ${VER}d ${IMAGE}
	./${VER}c ${IMAGE} ${VER}c.dec
	./${VER}d ${VER}c.dec ${VER}d.dec
	diff -w ${IMAGE} ${VER}d.dec

subleq: ${VMSRC}
	${CC} ${CFLAGS} $^ -o $@

forth: subleq ${IMAGE}
	./subleq ${IMAGE}

clean:
	git clean -dffx
