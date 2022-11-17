all: self2.dec self3.dec

%.dec: %.asq sqasm
	./sqasm < $< > $@

sqasm: sqasm.cpp

clean:
	rm sqasm
