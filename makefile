CFLAGS=-std=c99 -Wall -Wextra -pedantic -O3

.PHONY: all clean test run

all: subleq.dec subleq

test:
	./t

run: subleq subleq.dec
	./subleq subleq.dec

subleq.dec: subleq.fth
	gforth subleq.fth

clean:
	rm -fv subleq *.hex *.exe

