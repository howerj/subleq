# makefile for <https://github.com/howerj/subleq>, see help
# target for more information.
default all: help

.PHONY: all clean test run gforth width help speed count

CFLAGS=-std=c99 -fwrapv -Wall -Wextra -pedantic -O3
#CFLAGS+=-fsanitize=undefined 
FORTH=subleq.fth
IMAGE=subleq.dec

help:
	@echo
	@echo "Project: 16-bit SUBLEQ VM and Forth image"
	@echo "Author:  Richard James Howe"
	@echo "License: The Unlicense (code only, not comments/book)"
	@echo "Repo:    https://github.com/howerj/subleq"
	@echo "Email:   howe.r.j.89@gmail.com"
	@echo
	@echo "Parameters:"
	@echo
	@echo "	CFLAGS : ${CFLAGS}"
	@echo "	IMAGE  : ${IMAGE}"
	@echo "	FORTH  : ${FORTH}"
	@echo
	@echo "Targets:"
	@echo
	@echo "	subleq   : build executable SUBLEQ VM"
	@echo "	run      : run ${IMAGE} with SUBLEQ VM"
	@echo "	clean    : remove build files using 'git clean -dffx'"
	@echo "	test     : run meta-compilation tests"
	@echo "	help     : display this help message"
	@echo "	speed    : perform basic speed test"
	@echo "	gforth   : compile and run ${FORTH} with gforth"
	@echo "	width    : peform VM cell width tests"
	@echo " self     : run ${IMAGE} under 'self-interpreter'"
	@echo "	eforth.c : make SUBLEQ VM with built-in Forth"
	@echo "	subleq.{pdf,epub.htm} : make documentation"
	@echo
	@echo "Consult subleq.fth for more information along"
	@echo "with the project "readme.md" file."
	@echo
	@echo "Happy Hacking!"
	@echo

run: subleq ${IMAGE}
	./subleq ${IMAGE}

1.dec: subleq ${IMAGE} ${FORTH}
	./subleq ${IMAGE} < ${FORTH} > $@

2.dec: subleq 1.dec ${FORTH}
	./subleq 1.dec < ${FORTH} > $@

test: 1.dec 2.dec
	diff -w 1.dec 2.dec

eforth.fth: subleq.fth
	sed -e 's/^\\.*//' -e '/^$$/d' < $< > $@

NBIT=extra/nbit
width: ${IMAGE} ${NBIT}
	./${NBIT}  8 $<
	./${NBIT}  9 $<
	./${NBIT} 15 $<
	./${NBIT} 17 $<
	./${NBIT} 18 $<
	./${NBIT} 32 $<
	./${NBIT} 63 $<
	./${NBIT} 64 $<
	echo ".( Ahoy, World! ) cr bye " | ./${NBIT} 16 $<

gforth.dec: ${FORTH}
	gforth $< > $@

gforth: subleq gforth.dec
	./subleq gforth.dec

subleq.md: subleq.fth subleq 1.dec extra/convert.fth extra/self/self.dec extra/self/self.asq
	rm -f $@
	echo "---" >> $@
	echo "title: \"SUBLEQ eForth Meta-Compilation\"" >> $@
	echo "author: [Richard James Howe]" >> $@
	echo "date: \"2023-07-04\"" >> $@
	echo "subject: \"SUBLEQ eForth\"" >> $@
	echo "keywords: [SUBLEQ, Forth]" >> $@
	echo "subtitle: \"Forth Meta-Compilation for a SUBLEQ machine\"" >> $@
	echo "lang: \"en\"" >> $@
	echo "titlepage: true," >> $@
	echo "titlepage-rule-color: \"360049\"" >> $@
	#echo "titlepage-background: \"img/subleq-ebook.png\"" >> $@
	echo "---" >> $@
	cat extra/convert.fth subleq.fth | ./subleq 1.dec >> $@
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
	echo "## Self Interpreter (source)" >> $@
	cat extra/self/self.asq >> $@
	echo >> $@
	echo "## Self Interpreter (data)" >> $@
	cat extra/self/self.dec | tr '\n' ' ' | fmt -w 48 | sed 's/^/\t/' >> $@
	echo >> $@

subleq.htm: subleq.md
	markdown $< > $@

IMAGES=extra/img/flow.png extra/img/dictionary.png

subleq.pdf: subleq.md ${IMAGES} extra/eisvogel.tex
	pandoc ${META} --template extra/eisvogel.tex -V fontsize=8pt -V book -V code-block-font-size=8pt --toc $< -o $@

%.png: %.dia
	dia -e $@ $<

EPUB=--metadata=title:"SUBLEQ eForth" --metadata=author:"Richard James Howe" --metadata=lang:"en-US"
subleq.epub: subleq.md ${IMAGES}
	#pandoc --epub-cover-image=extra/img/subleq-ebook.png ${EPUB} --toc $< -o $@
	pandoc --epub-cover-image=extra/img/subleq-ebook.png ${EPUB} --toc $< -o $@

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


count: ${IMAGE}
	awk '{h[$$1]++}END{for (k in h){print k ",",h[k]}}' ${IMAGE} | sort -n -k 2

%.bin: %.dec ${IMAGE} extra/dump.fth subleq
	cat extra/dump.fth $< | ./subleq ${IMAGE} > $@

dump.dec:
	echo "0 here dump bye" | ./subleq ${IMAGE} > $@

self.dec:
	make -C extra/self self.dec
	cp extra/self/self.dec .

self: self.dec ${IMAGE} subleq
	./subleq self.dec ${IMAGE}

debug.o: extra/debug.c subleq.cma
	${CC} -I. -std=gnu99 -Wall -Wextra -pedantic $< -c -o $@

debug: debug.o

clean:
	git clean -dffx

TIME=1000
SHELL=/bin/bash
speed: subleq gforth.dec
	time -p (echo -e "${TIME} ms\nbye\n" | ./subleq gforth.dec)

