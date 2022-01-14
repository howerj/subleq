CFLAGS=-std=c99 -Wall -Wextra -pedantic -O3

.PHONY: all clean test run gforth

all: subleq

run: subleq subleq.dec
	./subleq subleq.dec

1.dec: subleq subleq.dec subleq.fth
	./subleq subleq.dec < subleq.fth > $@

2.dec: subleq 1.dec subleq.fth
	./subleq 1.dec < subleq.fth > $@

test: 1.dec 2.dec
	diff -w 1.dec 2.dec

gforth.dec: subleq.fth
	gforth $< > $@

gforth: subleq gforth.dec
	./subleq gforth.dec

subleq.md: convert subleq.fth
	./convert < subleq.fth > $@

subleq.htm: subleq.md
	markdown $< > $@

subleq.pdf: subleq.md
	pandoc -V cover-image=img/subleq-ebook.png --toc $< -o $@

subleq.epub: subleq.md
	pandoc \
		--epub-cover-image=img/subleq-ebook.png \
		--metadata=title:"SUBLEQ eForth" \
		--metadata=author:"Richard James Howe" \
		--metadata=lang:"en-US" \
		--toc \
		$< -o $@

clean:
	git clean -dffx

