.PHONY: all run run-orig clean

all: self.dec

run: self.dec ../subleq ../subleq.dec
	../subleq self.dec ../subleq.dec 

run-orig: ../subleq ../subleq.dec
	../subleq ../subleq.dec

double: self.dec ../subleq ../subleq.dec
	../subleq self.dec self.dec ../subleq.dec

%.dec: %.asq ../debug
	../debug -a $< > $@

../debug:
	make -C .. debug

../subleq.dec ../subleq:
	make -C .. subleq.dec subleq

clean:
