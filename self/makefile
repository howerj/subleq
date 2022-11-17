all: self2.dec

%.dec: %.hsq sqasm
	./sqasm < $< > $@

sqasm: sqasm.cpp

clean:
	rm sqasm
