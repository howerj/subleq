* Implement optional "defer"/"is":

	: defer create 0 , does> @ ?dup 0= -9 and throw execute ;
	: is ' >body ! ; immediate

* Write about alternate about subjects below

# Notes on the SUBLEQ Machine and alternatives

## Turing Completeness

What is Turing Completeness and is this SUBLEQ Turing Complete?

## Is SUBLEQ a single instruction

There are three special cases relating to input, output, and
halting when it comes to the SUBLEQ instruction. It could be
argued that the SUBLEQ instruction is no longer a single instruction
machine, but instead instead a three or four instruction machine,
which will be argued against with examples of alternate SUBLEQ
machines.

## Purpose Build Forth Cores

There are a few Forth cores, some implemented as integrated circuits,
others in the logic of an FPGA, that are optimized for executing Forth.

### J1 / H2

* <https://github.com/howerj/forth-cpu>
* <https://excamera.com/files/j1.pdf>

### Multiple instructions per cell

For a 16-bit Forth you could create an encoding that fit multiple Forth
instructions per cell, if each instruction was 4-bits you could fit four
instructions for example. What those instructions should be and if their
should be special cases could be discussed.


