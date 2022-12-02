.PHONY: all run run-orig clean

all: self2.dec self3.dec

run: self3.dec ../subleq ../subleq.dec
	make -C .. subleq subleq.dec
	../subleq self3.dec ../subleq.dec 

run-orig: ../subleq ../subleq.dec
	../subleq ../subleq.dec

%.dec: %.asq sqasm
	./sqasm < $< > $@

sqasm: sqasm.cpp


../subleq ../subleq.dec:
	make -C .. subleq subleq.dec

clean:
	rm sqasm
