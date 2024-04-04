# 16-bit SUBLEQ eForth

* Edition: 1.1.0
* Author: Richard James Howe
* Email: <mailto:howe.r.j.89@gmail.com>
* Repo: <https://github.com/howerj/subleq>
* License: The Unlicense / Public domain for code only, all 
rights reserved for comments, the book, diagrams and pictures.

This project contains a working (self-hosting) Forth
interpreter that runs on top of a SUBLEQ 16-bit machine. SUBLEQ
machines belong to the class of One Instruction Set Computers,
they only execute a single instruction but are still Turing
Complete. The Forth system, specifically a variant of eForth,
is provided as [subleq.dec](subleq.dec), passing this image to
the tiny (~ 700 bytes) [SUBLEQ C virtual machine](subleq.c)
allows you to run eForth on the machine. For a list of
commands type "words" and hit enter, numbers are entered
using Reverse Polish Notation, eg. "2 2 + . cr" prints "4",
and new functions can be defined like so:

	: hello cr ." Hello, World" ;

Be careful with the spaces, they matter, after typing that
in, type "hello" and hit enter. A Forth tutorial will not be
provided here. Many Forth words are defined *including the
bitwise words*.

To build and run you will need a C compiler and Make, type
"make run", failing that:

	cc subleq.c -o subleq
	./subleq subleq.dec

The system is self hosting, that is it can generate new eForth
images using the current eForth image and the eForth source
code. This is done like so:

	./subleq subleq.dec < subleq.fth > new-image.dec

There is a website available that runs an interactive
SUBLEQ interpreter in the browser in case you do
not want to both compiling things, it is available at
<https://github.com/howerj/subleq-js>. Or if you just want
to try it out directly <https://howerj.github.io/subleq.htm>.

Happy hacking, and a shiny penny for anyone that manages to
do something useful with this project!

## Other SUBLEQ projects

* <https://github.com/pbrochard/subleq-eForthOS>
* <https://github.com/howerj/subleq-python>
* <https://github.com/howerj/subleq-perl>
* <https://github.com/howerj/subleq-js>
* <https://github.com/howerj/subleq-forth>
* <https://github.com/howerj/subleq-vhdl>
* <https://github.com/howerj/muxleq>

## References

* <https://en.wikipedia.org/wiki/Forth_(programming_language)>
* <https://en.wikipedia.org/wiki/One-instruction_set_computer>
* <https://github.com/howerj/bit-serial>
* <https://github.com/howerj/embed>
* <https://github.com/howerj/forth-cpu>
* <https://rosettacode.org/wiki/Subleq>
* <https://stackoverflow.com/questions/2982729>
* <https://stackoverflow.com/questions/34120161>
* <https://web.ece.ucsb.edu/~parhami/pubs_folder/parh88-ijeee-ultimate-risc.pdf>
* <https://eigenratios.blogspot.com/2006/09/mark-ii-oisc-self-interpreter.html>

