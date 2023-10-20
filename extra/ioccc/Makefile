CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3
IMAGE=../../subleq.dec
SELF=../self/self.dec

.PHONY: all try run clobber clean

all: prog

run try: prog ${IMAGE}
	./prog 16 ${IMAGE}

self: prog ${SELF} ${IMAGE}
	./prog 16 ${SELF} ${IMAGE}

clobber clean:
	rm -f prog 

NBIT=prog
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
PROG=$(shell cat ${IMAGE})

single: oneline
	@echo "SINGLE LINE RUNNING - PROG SZ:" $(shell tr -d '\n' < oneline.c | wc | awk '{print $$3}')
	@./oneline ${PROG}
	@echo BYE
