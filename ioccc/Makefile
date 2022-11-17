CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3

.PHONY: all try run clobber clean test

all: prog

try: prog data.dec
	./prog 16 data.dec

run: try

clobber: clean

clean:
	rm -f prog 

data.dec: ioccc.fth
	gforth $< > $@

1.dec: prog data.dec ioccc.fth
	./prog 16 data.dec < ioccc.fth > $@

2.dec: prog 1.dec ioccc.fth
	./prog 16 1.dec < ioccc.fth > $@

test: 2.dec
