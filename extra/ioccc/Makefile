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
