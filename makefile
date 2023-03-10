CFLAGS=-std=c99 -Wall -Wextra -pedantic -O3
FORTH=subleq.fth
IMAGE=subleq.dec

.PHONY: all clean test run gforth width extra

all: subleq

run: subleq ${IMAGE}
	./subleq ${IMAGE}

1.dec: subleq ${IMAGE} ${FORTH}
	./subleq ${IMAGE} < ${FORTH} > $@

2.dec: subleq 1.dec ${FORTH}
	./subleq 1.dec < ${FORTH} > $@

test: 1.dec 2.dec nbit
	diff -w 1.dec 2.dec

eforth.fth: subleq.fth
	sed -e 's/^\\.*//' -e '/^$$/d' < $< > $@

width: ${IMAGE} nbit 
	./nbit  8 $<
	./nbit  9 $<
	./nbit 15 $<
	./nbit 17 $<
	./nbit 18 $<
	./nbit 32 $<
	./nbit 63 $<
	./nbit 64 $<
	echo ".( Ahoy, World! ) cr bye " | ./nbit 16 $<

subleq.bin: subleq ${IMAGE}
	echo "0 here : ddd 1- for dup c@ emit 1+ next drop ; ddd bye " | ./subleq ${IMAGE} > $@

dump.dec:
	echo "0 here dump bye" | ./subleq ${IMAGE} > $@

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

%.cma: %.dec
	sed 's/$$/,/' $^ | fmt -w 80 | sed 's/ //g' >> $@

self.dec:
	make -C self self.dec
	cp self/self.dec .
	sed -i 's/ /\n/g' $@
	sed -i '/^$$/d' $@

debug.o: debug.c subleq.cma
	${CC} -std=gnu99 -Wall -Wextra -pedantic $< -c -o $@

float: gforth.dec
	cat self/float.fth /dev/stdin | ./subleq $<

debug: debug.o

clean:
	git clean -dffx

