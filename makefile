CFLAGS=-std=c99 -Wall -Wextra -pedantic -O3

.PHONY: all clean test run gforth width extra

all: subleq

run: subleq subleq.dec
	./subleq subleq.dec

1.dec: subleq subleq.dec subleq.fth
	./subleq subleq.dec < subleq.fth > $@

2.dec: subleq 1.dec subleq.fth
	./subleq 1.dec < subleq.fth > $@

test: 1.dec 2.dec nbit
	diff -w 1.dec 2.dec

width: gforth.dec nbit 
	./nbit  8 $<
	./nbit  9 $<
	./nbit 15 $<
	./nbit 17 $<
	./nbit 18 $<
	./nbit 32 $<
	./nbit 63 $<
	./nbit 64 $<
	echo ".( Ahoy, World! ) cr bye " | ./nbit 16 $<

gforth.dec: subleq.fth
	gforth $< > $@

gforth: subleq gforth.dec
	./subleq gforth.dec

subleq.md: subleq.fth subleq 1.dec convert.fth
	rm -f $@
	echo "---" >> $@
	echo "title: \"SUBLEQ eForth Meta-Compilation\"" >> $@
	echo "author: [Richard James Howe]" >> $@
	echo "date: \"2022-03-01\"" >> $@
	echo "subject: \"SUBLEQ eForth\"" >> $@
	echo "keywords: [SUBLEQ, Forth]" >> $@
	echo "subtitle: \"Forth Meta-Compilation for a SUBLEQ machine\"" >> $@
	echo "lang: \"en\"" >> $@
	echo "titlepage: true," >> $@
	echo "titlepage-rule-color: \"360049\"" >> $@
	#echo "titlepage-background: \"img/subleq-ebook.png\"" >> $@
	echo "---" >> $@
	cat convert.fth subleq.fth | ./subleq 1.dec >> $@
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

IMAGES=img/flow.png img/dictionary.png

subleq.pdf: subleq.md ${IMAGES} eisvogel.tex
	pandoc ${META} --template eisvogel.tex -V fontsize=8pt -V book -V code-block-font-size=8pt --toc $< -o $@

%.png: %.dia
	dia -e $@ $<


EPUB=--metadata=title:"SUBLEQ eForth" --metadata=author:"Richard James Howe" --metadata=lang:"en-US"
subleq.epub: subleq.md ${IMAGES}
	#pandoc --epub-cover-image=img/subleq-ebook.png ${EPUB} --toc $< -o $@
	pandoc --epub-cover-image=img/subleq-ebook.png ${EPUB} --toc $< -o $@

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

