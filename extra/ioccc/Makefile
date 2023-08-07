CFLAGS=-Wall -Wextra -std=c11 -pedantic -O3
EFORTH=eforth.dec
SELF=../self/self.dec

.PHONY: all try run clobber clean

all: prog

run try: prog ${EFORTH}
	./prog 16 ${EFORTH}

self: prog ${SELF} ${EFORTH}
	./prog 16 ${SELF} ${EFORTH}

clobber clean:
	rm -f prog 


