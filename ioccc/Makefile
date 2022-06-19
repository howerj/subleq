CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3

all: prog

try: prog data.dec
	./prog 16 data.dec

run: try

clobber: clean

clean:
	rm -f prog 

data.dec: ioccc.fth
	gforth $< > $@
