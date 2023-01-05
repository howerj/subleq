CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3

.PHONY: all try run clobber clean test

all: prog

try: prog ../subleq.dec
	./prog 16 ../subleq.dec

run: try

clobber: clean

clean:
	rm -f prog 


