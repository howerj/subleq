CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3

.PHONY: all try run clobber clean test

all: prog

try: prog ../subleq.dec
	./prog 16 ../subleq.dec

self: prog ../self/self.dec ../subleq.dec
	./prog 16 ../self/self.dec ../subleq.dec

run: try

clobber: clean

clean:
	rm -f prog 


