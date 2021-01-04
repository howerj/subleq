CFLAGS=-std=c99 -Wall -fwrapv -Wextra -std=c99 -pedantic -O2

.PHONY: all clean test run

all: subleq.dec subleq

test: run

run: subleq subleq.dec
	./subleq subleq.dec

subleq.dec: subleq.fth
	gforth subleq.fth

clean:
	rm -fv subleq *.dec *.hex *.exe

