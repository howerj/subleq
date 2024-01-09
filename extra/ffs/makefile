
.PHONY: all dump

all:
	gforth ffs.fth

dump: ffs.fb
	hexdump -C $<

ffs.fb: ffs.fth
	gforth ffs.fth < /dev/null

clean:
	git clean -dffx
