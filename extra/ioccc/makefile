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

PROG=".( Ahoy, World! ) cr bye "
NBIT=1
width: ${IMAGE} ${NBIT}
	echo ${PROG} | ./${NBIT}  8 $<
	echo ${PROG} | ./${NBIT}  9 $<
	echo ${PROG} | ./${NBIT} 15 $<
	echo ${PROG} | ./${NBIT} 17 $<
	echo ${PROG} | ./${NBIT} 18 $<
	echo ${PROG} | ./${NBIT} 30 $<
	echo ${PROG} | ./${NBIT} 31 $<
	echo ${PROG} | ./${NBIT} 32 $<
	echo ${PROG} | ./${NBIT} 16 $<


2: 2.c
	${CC} ${CFLAGS} -Du="${CSPG}" $< -o $@

3: 3.c
	${CC} ${CFLAGS} -Du="${CSPG}" $< -o $@

