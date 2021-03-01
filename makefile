CFLAGS=-std=c99 -Wall -Wextra -pedantic -O3

.PHONY: all clean test meta run

all: subleq.dec subleq

test:
	./t

run: subleq subleq.dec
	./subleq subleq.dec

meta: subleq meta.fth
	./subleq subleq.dec < meta.fth > meta.dec

subleq.dec: subleq.fth
	gforth subleq.fth

clean:
	rm -fv subleq *.hex *.exe

