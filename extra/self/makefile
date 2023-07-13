.PHONY: all run run-orig clean
CFLAGS=-std=c99 -Wall -Wextra -pedantic -O2
VM=../subleq
OPTS=
IMAGE=../subleq.dec

all: self.dec

run: self.dec ${VM} ${IMAGE}
	${VM} ${OPTS} self.dec ${IMAGE} 

run-orig: ${VM} ${IMAGE}
	${VM} ${OPTS} ${IMAGE}

double: self.dec ${VM} ${IMAGE}
	${VM} ${OPTS} self.dec self.dec ${IMAGE}


%.dec: %.asq ../debug
	../debug -a $< > $@

../debug:
	make -C .. debug

../subleq.dec ../subleq:
	make -C .. subleq.dec subleq

run-all: all.dec ${VM} ${IMAGE}
	${VM} ${OPTS} all.dec ${IMAGE}

clean:
