CFLAGS=-std=c99 -Wall -fwrapv -Wextra -std=c99 -pedantic -O2

.PHONY: all clean test run

all: run

test: run

run: subleq subleq.bin
	./subleq subleq.bin

subleq.bin: subleq.fth
	gforth subleq.fth

clean:
	rm -fv subleq *.bin *.hex

