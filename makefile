CFLAGS=-std=c99 -Wall -Wextra -pedantic -O3

.PHONY: all clean test run

all: subleq

run: subleq subleq.dec
	./subleq subleq.dec

test: subleq
	./subleq subleq.dec < subleq.fth > 1.dec
	./subleq      1.dec < subleq.fth > 2.dec
	diff -w 1.dec 2.dec

gforth.dec: subleq.fth
	gforth $< > $@

gforth: subleq gforth.dec
	./subleq gforth.dec

clean:
	rm -fv subleq 1.dec 2.dec *.exe

