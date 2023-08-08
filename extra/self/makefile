.PHONY: all run run-orig clean
CFLAGS=-std=c99 -Wall -Wextra -pedantic -O2
MAINDIR=../..
VM=${MAINDIR}/subleq
OPTS=
IMAGE=${MAINDIR}/subleq.dec
DEBUG=${MAINDIR}/debug

all: self.dec

run: self.dec ${VM} ${IMAGE}
	${VM} ${OPTS} self.dec ${IMAGE} 

run-orig: ${VM} ${IMAGE}
	${VM} ${OPTS} ${IMAGE}

double: self.dec ${VM} ${IMAGE}
	${VM} ${OPTS} self.dec self.dec ${IMAGE}


%.dec: %.asq ${DEBUG}
	${DEBUG} -a $< > $@

${DEBUG}:
	make -C ${MAINDIR} debug

${IMAGE} ${VM}:
	make -C ${MAINDIR} subleq.dec subleq

run-all: all.dec ${VM} ${IMAGE}
	${VM} ${OPTS} all.dec ${IMAGE}

clean:
