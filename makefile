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

subleq.md: subleq.fth subleq 1.dec convert.fth
	cat convert.fth subleq.fth | ./subleq 1.dec > $@
	dos2unix $@
	echo "## Full eForth Image:" >> $@
	echo >> $@
	echo "A complete eForth image for reference (make sure not to copy page numbers):" >> $@
	echo >> $@
	cat 1.dec | tr '\n' ' ' | fmt -w 48 | sed 's/^/\t/' >> $@
	echo >> $@
	echo "## Source code without (major) comments:" >> $@
	echo >> $@
	grep '^[^\\]' subleq.fth | sed 's/^/\t/' >> $@
	echo >> $@

subleq.htm: subleq.md
	markdown $< > $@

META=--metadata=title:"SUBLEQ eForth" --metadata=author:"Richard James Howe" --metadata=lang:"en-US"
IMAGES=img/flow.png img/dictionary.png

subleq.pdf: subleq.md ${IMAGES}
	pandoc ${META} -V cover-image=img/subleq-ebook.png --toc $< -o $@

%.png: %.dia
	dia -e $@ $<


subleq.epub: subleq.md ${IMAGES}
	pandoc --epub-cover-image=img/subleq-ebook.png ${META} --toc $< -o $@

eforth.c: 1.dec
	rm -f $@
	echo "#include <stdio.h> /* eForth for 16-bit SUBLEQ */" >> $@
	echo "int main(void){short p=0,m[65536] = {" >> $@
	sed 's/$$/,/' $^ | fmt -w 80 | sed 's/ //g' >> $@
	echo "}; while(p>=0){int a=m[p++],b=m[p++],c=m[p++];" >> $@
	echo "a<0?m[b]=getchar():b<0?putchar(m[a]):(m[b]-=m[a])" >> $@
	echo "<=0?p=c:0;}}" >> $@

clean:
	git clean -dffx

