CFLAGS=-Wall -Wextra -std=c99 -pedantic -O2

.PHONY: all dump clean

#all:
#	gforth ffs.fth

all: ffs

dump: ffs.fb
	hexdump -C $<

ffs.fb: ffs.fth
	gforth ffs.fth < /dev/null

format: ffs
	rm -fv file.fb
	./ffs format file.fb 1 128
	hexdump -C file.fb

clean:
	git clean -dffx
