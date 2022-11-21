.PHONY: all run clean

all: self2.dec self3.dec

run: self3.dec
	make -C .. subleq subleq.dec
	../subleq self3.dec ../subleq.dec 

%.dec: %.asq sqasm
	./sqasm < $< > $@

sqasm: sqasm.cpp

clean:
	rm sqasm
