# makefile for <https://github.com/howerj/subleq>, see help
# target for more information.
default all: help

.PHONY: all clean test run gforth width help speed

CFLAGS=-std=c99 -fwrapv -Wall -Wextra -pedantic -O3
#CFLAGS+=-fsanitize=undefined 
FORTH=subleq.fth
IMAGE=subleq.dec

help:
	@echo
	@echo "Project: 16-bit SUBLEQ VM and Forth image"
	@echo "Author:  Richard James Howe"
	@echo "License: The Unlicense (code only)"
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
	@echo "	eforth.c : make SUBLEQ VM with build in Forth"
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

subleq.md: subleq.fth subleq 1.dec extra/convert.fth self/self.dec self/self.asq
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
	cat self/self.asq >> $@
	echo >> $@
	echo "## Self Interpreter (data)" >> $@
	cat self/self.dec | tr '\n' ' ' | fmt -w 48 | sed 's/^/\t/' >> $@
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


#%.hex: %.dec makefile
#	awk '{l=$$1;s="";if(l<0){l=-l;s="-";};printf "%s%04x\n", s, l}' < $< > $@
#
## xxd '-e' switch does not work with '-r' and '-p' unfortunately, hence sed is
## used to switch the bytes.
#%.bin %.big: %.hex makefile
#	xxd -r -p -g 0 < $< > %.big
#	sed 's/\(.\)\(.\)/\2\1/g' < %.big > $@
#
#subleq.bin: subleq ${IMAGE}
#	echo "0 here : ddd 1- for dup c@ emit 1+ next drop ; ddd bye " | ./subleq ${IMAGE} > $@
#

%.bin: %.dec subleq.dec extra/dump.fth subleq
	cat extra/dump.fth subleq.dec | ./subleq subleq.dec > $@

dump.dec:
	echo "0 here dump bye" | ./subleq ${IMAGE} > $@

self.dec:
	make -C self self.dec
	cp self/self.dec .
	sed -i 's/ /\n/g' $@
	sed -i '/^$$/d' $@

debug.o: debug.c subleq.cma
	${CC} -std=gnu99 -Wall -Wextra -pedantic $< -c -o $@

debug: debug.o

clean:
	git clean -dffx

TIME=1000
SHELL=/bin/bash
speed: subleq gforth.dec
	time -p (echo -e "${TIME} ms\nbye\n" | ./subleq gforth.dec)
