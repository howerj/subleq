CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3 -g
IMAGE=../../subleq.dec
SELF=../self/self.dec

.PHONY: all try run clobber clean

all: 1

run try: 1 ${IMAGE}
	./1 16 ${IMAGE}

self: 1 ${SELF} ${IMAGE}
	./1 16 ${SELF} ${IMAGE}

clobber clean:
	rm -f 1 2 3 *.exe

NBIT=1
width: ${IMAGE} ${NBIT}
	./${NBIT}  8 $<
	./${NBIT}  9 $<
	./${NBIT} 15 $<
	./${NBIT} 17 $<
	./${NBIT} 18 $<
	./${NBIT} 30 $<
	./${NBIT} 31 $<
	./${NBIT} 32 $<
	echo ".( Ahoy, World! ) cr bye " | ./${NBIT} 16 $<

#!/bin/sh
# Portability concerns:
# - Not tested a machine where sizeof(int)==16
# - Stack usage is determined by number of arguments passed in
# - Loading a program is done via the number of arguments passed
# via the command line, large programs may cause problems
CSPG=$(shell cat ${IMAGE} | tr '\n\t ' ',')

3: 3.c
	${CC} ${CFLAGS} -Du="${CSPG}" $< -o $@

