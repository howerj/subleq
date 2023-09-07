CFLAGS=-Wall -Wextra -pedantic -O2 -std=c99

.PHONY: all run test forth clean

all run test: 1c 1d subleq.dec
	./1c subleq.dec 1c.dec
	./1d 1c.dec 1d.dec
	diff -w subleq.dec 1d.dec

forth: subleq subleq.dec
	./subleq subleq.dec

clean:
	git clean -dffx
