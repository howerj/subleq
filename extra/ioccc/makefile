CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3 -g
IMAGE=../../subleq.dec
SELF=../self/self.dec
CSPG=$(shell cat ${IMAGE} | tr '\n\t ' ',')

.PHONY: all try run clobber clean width

all: 1

run try: 1 ${IMAGE}
	./1 16 ${IMAGE}

self: 1 ${SELF} ${IMAGE}
	./1 16 ${SELF} ${IMAGE}

clean clobber:
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


2: 2.c
	${CC} ${CFLAGS} -Du="${CSPG}" $< -o $@

3: 3.c
	${CC} ${CFLAGS} -Du="${CSPG}" $< -o $@

