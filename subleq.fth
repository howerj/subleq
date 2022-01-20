defined eforth [if] ' nop <ok> ! [then] ( Turn off ok prompt )
\ # Introduction
\
\ * Project: Cross Compiler / eForth for a SUBLEQ CPU
\ * License: The Unlicense
\ * Author:  Richard James Howe
\ * Email:   howe.r.j.89@gmail.com
\ * Repo:    <https://github.com/howerj/subleq>
\
\ TODO Section
\
\	- Add assembled version of image to appendix
\	- Publish on Amazon
\	- Separate SUBLEQ assembler Tutorial
\	- Other SUBLEQ projects and programs
\	- Uses; learning, puzzles, games
\	- Sokoban, floating point, file system,
\	allocate/free, ...
\	- About the Author, Other projects, etcetera.
\	- Possible optimizations; merge exit with last word
\	- Configuring the option bit in the image
\	- Make sure that words cannot be longer than 32
\	characters in length.
\	- Talk about variable bit width and how that could
\	be implemented, for a 32 or 64, or even N-bit version.
\	- Note that "0 here dump" can be used to save a
\	running image.
\	- Make diagrams in a proper image editor instead
\	of having ASCII diagrams (copies of the diagram could
\	go in the appendix).
\ 
\ ## NOTES (Write about them in the book)
\
\ - The eForth image could determine the SUBLEQ machine size,
\ and adjust itself accordingly, it would not even require a
\ power-of-two integer width. Another interesting concept would
\ be to adapt this eForth to a SUBLEQ machine that used bignums
\ for each cell, this would require re-engineering functions
\ like bitwise AND/OR/XOR as they require a fixed cell width to
\ work efficiently.
\ - A website with an interactive simulator is available at:
\   <https://github.com/howerj/subleq-js>
\ - It would be nice to make a 7400 Integrated Circuit board
\ that could run and execute this code, or a project in VHDL
\ for an FPGA that could do it.
\ - The virtual machine could be sped up with optimization
\ magic
\ - Half of the memory used is just for the virtual machine
\ that allows Forth to be written.
\ - Testing, why is there no test bench -> recompilation is
\ the test bench

\	-----------------------------------------------

\
\ This file contains an assembler for a SUBLEQ CPU, a virtual
\ machine capable of running Forth built upon that assembler,
\ a cross compiler capable of targeting the Forth VM, and
\ finally the Forth interpreter itself, based on the eForth
\ family of the Forth programming language. This system is
\ self-hosted, which means it can be used to create new,
\ modified systems. Also contained is a description of how this
\ system works, how the internals of a Forth interpreter works,
\ and the difficulties and trade-offs involved in targeting
\ such an anemic CPU, called SUBLEQ, which is an esoteric,
\ impractical, single instruction CPU. If you can port a Forth
\ to it, then you can port a Forth implementation to anything.
\
\ This program is written in Forth entirely, and should
\ compile both under gforth (tested with version 0.7.3) and
\ by executing it against a pre-generated eForth image
\ running on the SUBLEQ machine. The file is formatted so that
\ no line is longer than 64 bytes (or characters, it should be
\ encoded as ASCII) long, this is so that source code could
\ potentially be stored in something called "Forth Blocks".
\
\ This program and explanation is for an esoteric, oddball,
\ system, it is quite likely it will never be useful for
\ anything. It will also not designed for beginner programmers,
\ it would very much help if you had some understanding of
\ Forth and Assembly before hand.
\
\ ## Building This Image
\
\ To build the image you will need either gforth, or perhaps
\ easier for you, a working SUBLEQ implementation in your
\ favorite language, one is in the appendix and another will
\ be described shortly, both written in C. The C versions will
\ obviously require a C compiler.
\
\ Anyway, to build, on a Unix system:
\
\	gcc subleq.c -o subleq
\	./subleq subleq.dec < subleq.fth > new-image.dec
\
\ And on Windows:
\
\	gcc subleq.c -o subleq.exe
\	subleq.exe subleq.dec < subleq.fth > new-image.dec
\
\ And with gforth on Unix and Windows:
\
\	gforth subleq.fth > new-image.dec
\
\ And to run the new image:
\
\	./subleq new-image.dec
\
\ The command shells redirection facilities are used to make
\ up for a lack of input/output mechanisms within the SUBLEQ
\ machine, discussed later.
\
\ Most modern complex programs have a system of unit tests,
\ this would have been useful in a few places, however instead
\ the meta-compilation system is used as a giant unit test
\ framework. If the system can compile and image "A" which can
\ compile another image "B" that is byte for byte as image "A",
\ then we can be pretty confident that the new image is
\ correct, it is at least correct enough to compile itself.
\
\ This is done with the following commands on a Unix system:
\
\	./subleq subleq.dec < subleq.fth > 1.dec
\	./subleq 1.dec < subleq.fth > 2.dec
\	diff -w 1.dec 2.dec
\
\ You may wonder how the original "subleq.dec" image was
\ created, it was done using gforth to make the first viable
\ eForth for the SUBLEQ machine, once that done it was a matter
\ of modifying it so it could compile itself. The gforth
\ interpreter is no longer needed, just kept around because it
\ is *much* faster than using SUBLEQ eForth to compile a new
\ image.
\
\ The image generated by gforth should be the same as the one
\ generated by SUBLEQ eForth given the same "subleq.fth" file.
\
\ ## Markdown and Formatting
\
\ The comments in this file, the long descriptions at least,
\ not the short stack effect comments, are written with the
\ intention that they are extracted from the original
\ "subleq.fth" source and turned into a markdown file, a well
\ known format, that can be used to generate a book. If you are
\ viewing the original "subleq.fth" file then the comments will
\ contain markdown formatting, do less than or greater than
\ characters will have to be escaped, as will asterisks. If
\ you are viewing the book format (epub, PDF, ...) then the
\ formatting for *bold* words should appear, and characters
\ like '\*' will not have to be escaped with a backslash.
\
\ The source code is meant to be written in a "literate
\ programming" style, although semi-literate might be more
\ appropriate as it does not fully implement everything from
\ the paradigm. The literate programming paradigm, introduced
\ and advocated for by Donald Knuth, is a great solution for
\ programmers writing textbooks, such as myself, but is not
\ a good general purpose idea.
\
\ The "subleq.fth" file is formatted so it is at maximum 64
\ characters wide, and uses fairly conventional and regular
\ naming and coding styles for Forth, with the exception that
\ all word definitions are lower case. The 64 character limit
\ is imposed to accommodate the formatting of Forth blocks,
\ there would be more to do to make it fully Forth block
\ compliant, but it is a start.
\
\ You will notice that many word definitions have a regular
\ formatting when it comes to the comments, these "stack effect
\ comments" have a common structure that is sometimes broken
\ or bent where it makes sense.
\
\ An example of a stack effect comment, on a Forth definition:
\
\	: square dup * ; ( n -- n : square a number )
\
\ The stack effect comment is between "(" and ")", it has
\ the general form of:
\
\	( stack input -- stack output : description )
\
\ The stack refers to the variable stack, and to the number of
\ items the word consumes or produces. The description is
\ usually quite laconic, as there is little room for it, the
\ word name itself should offer some clue as to what it does,
\ and the word should be short, ideally a single line, so it
\ can be inspected to see what it *actually* does.
\
\ There is a common naming system for the arguments:
\
\	n - A signed number, takes up a single cell on the
\	    stack.
\	u - Unsigned single cell number.
\	c - A single character or byte which will take up a
\	    a single cell on the stack.
\	xt - an execution token, this points to a function
\	    which can be executed later.
\	a - A cell address, it should be aligned to a 2 byte
\	   boundary on this system.
\	b - An address that points to bytes, it will not
\	   be aligned.
\	f - a Forth flag, 0 = false, -1 = true.
\	x y z - Used when the position of the before and
\	   after picture for the cells matters more than the
\	   interpretation of the cells themselves.
\	d - A signed double cell (32-bit) value.
\	ud - An unsigned double cell value.
\	wid - An address of a vocabulary or word-list.
\
\ There are a few other conventions surrounding stack effect
\ comments.
\
\ For the word definition "\>r", pronounced "To R", the stack
\ effect comment shows the movement of a single cell from the
\ variable to the return stack with "R:".
\
\	( n --, R: -- n )
\
\ Parsing words use "name" to indicate they accept a word of
\ from the input stream to use, like ":" or "variable":
\
\	( --, "name" )
\
\ Some words return a variable number (or even more rarely
\ accept a variable number) of cells, this can be indicated
\ with "|", for example "key?":
\
\	( -- c 0 | -1 )
\
\ Returns either a single character and a zero *or* negative
\ one.
\
\ This Forth uses terms like "wordlist" and "vocabulary"
\ interchangeably, sometimes "dictionaries" is used for the
\ same term, all refer to a data structure containing a list
\ of words.
\
\ In the code all values are hexadecimal, in the comments it
\ will usually be in decimal unless a "\$" is prefixed to the
\ number.
\
\ The Forth code itself has minimal guidelines for formatting
\ it, try to keep it short and clean, use two spaces where
\ possible per indentation level when it is needed, try to
\ keep definitions on a single line, lower case names, and
\ add a stack effect comment.
\
\ ## eForth
\
\ The image we will create is a variant of Forth called
\ "eForth", what the "e" stands for is up for debate. There
\ are a few differences between this eForth implementation and
\ standard ANS Forth implementations, such as the lack of the
\ "do...loop" construct and variants, do not be surprised if
\ some standard words are missing or have different semantics.
\
\ The idea of eForth was to have a system that required only
\ a handful (about 30) primitives to be encoded in assembly
\ in order to create a highly portable Forth with reasonable
\ efficiency. The original eForth was coded by Bill Muench,
\ with many variants and improvements subsequently being made
\ by C.H. Ting. The link <http://forth.org/eforth.html> has
\ more information.
\
\ ## A little about SUBLEQ
\
\ So the goal is to port a system capable of compiling itself
\ to the SUBLEQ machine, what is SUBLEQ and why is porting to
\ it difficult? As mentioned it is a single instruction
\ machine, each instruction consisting of three operands; "A",
\ "B", and "C". Each operand is stored in a single cell, in
\ this implementation the cell size is 16-bits, which will be
\ important later. A SUBLEQ machine that uses sizes other than
\ this, or bignums, will not work.
\
\ The single instruction that is executed is:
\
\ 1. Load the contents of the cell pointed to by A
\ 2. Load the contents of the cell pointed to by B
\ 3. Subtract A from B, and store the result back to cell B.
\    If the result is less than or equal to zero, jump to the
\    location pointed to by C. Otherwise advance the program
\    counter to the next location and execute from there.
\
\ This machine does not specify I/O, it could be memory mapped
\ in a few ways, the most common is the following, by modifying
\ the above instruction slightly to deal with some special
\ values:
\
\ 1. If A is -1, or all bits set, then get a byte from the
\ input channel (AKA read a character from the keyboard) and
\ store it in the cell pointed to by B. Operand C is ignored,
\ and no jumps occur for either I/O instruction.
\ 2. If B is -1, then load the contents pointed to by A and
\ write a byte to the output channel (or display a single
\ character), again no jump is performed.
\
\ Another special case is that if the program counter goes
\ outside memory then the machine halts.
\
\ This sort of makes most SUBLEQ implementations three
\ instruction machines, with the instructions SUBLEQ, INPUT
\ and OUTPUT, and perhaps HALT. However, that is sort of
\ being pedantic.
\
\ A SUBLEQ machine that has and can address infinite memory
\ memory is Turing complete, our implementation, as all real
\ world implementations of any system, does not have infinite
\ memory, but it can be imagined to be Turing complete.
\
\ Assuming a twos compliment machine and 16-bit shorts, then
\ this tiny C program will execute the image we will make:
\
\	#include <stdio.h>
\	int main(int x, char **v)
\	{
\		FILE *f=fopen(v[1], "r");
\		short p=0, m[1<<16], *i=m;
\		while (fscanf(f, "%hd", i++) > 0) ;
\		for (; p>=0;) {
\			int a=m[p++],b=m[p++],c=m[p++];
\			a<0 ? m[b]=getchar() :
\			b<0 ? putchar(m[a]) :
\			(m[b]-=m[a]) <= 0 ? p=c :
\			0;
\	}}
\
\ Or, alternatively you can test out the system online here:
\
\ <https://howerj.github.io/subleq.htm>
\
\ Also note, the SUBLEQ machine has no way of saving to disk,
\ and the only peripherals it offers are inputting and
\ outputting a single byte. This does not hold us back during
\ cross compilation, as you will see.
\
\ The image is passed to the program simulating the machine
\ (which will not be referred to as a Virtual Machine (VM) to
\ distinguish it from the VM we are constructing within the
\ SUBLEQ machine) in the first argument to the program, with
\ each cell value stored as a signed number with spaces to
\ delimit it. We will eventually generate an image which
\ contains a full Forth interpreter, however here is an example
\ program that prints out "Hi" and then exits:
\
\	9 -1 3 10 -1 6 0 0 -1 72 105 0
\
\ You can see that this CPU architecture is barren, spartan, it
\ does not offer what we usually expect from a processor. It
\ has no native way to left shift, multiple, divide, it has
\ no interrupts, traps, nor a memory management unit, it cannot
\ load or store without doing a subtraction. The instruction it
\ can do will have to be manipulated so it can do just a store,
\ without the subtraction. Addition will have to be
\ synthesized, as will every other useful instruction.
\
\ Also of note, we have no stack, no function calls and
\ returns, no indirect load or stores. These will all require
\ self-modifying code to implement.
\
\ An example of this are the following, an unconditional Jump
\ instruction, or "JMP", can be made in the following fashion,
\ using "A", "B", and "C" for the operands as we did before:
\
\ 	JMP c
\		subleq Z, Z, c
\
\ A jump can be implemented in a single instruction, the "Z"
\ in this instruction is the location of a cell which should
\ contain zero, it may get temporarily modified so it does not
\ contain zero, but at the end of the sequence it should always
\ be returned to zero. The notation above shows that operands
\ "A" and "B" should point to the location of that zero
\ containing cell. The jump location is just put in the third
\ operand. As subtracting zero from zero is zero, then the
\ jump to location C is always executed.
\
\ "NOP" can be coded as:
\
\	NOP
\		subleq Z, Z
\
\ Note that the third operand is omitted, we take this to
\ mean that the jump location should be the location of the
\ next instruction.
\
\ A non-trivial instruction is "ADD":
\
\	ADD a, b
\
\		subleq a, Z
\ 		subleq Z, b
\		subleq Z, Z
\
\ "ADD" stores a temporary result in "Z", but it should
\ start off as zero as always, it effectively negates what is
\ loaded from "a", storing it in "Z", then subtracts and stores
\ the result in "b". However "Z" contains an unknown, possible
\ non-zero value, so the third SUBLEQ subtracts "Z" from itself
\ and thus restores it's zero status at the end of the "ADD"
\ instruction.
\
\ We will see more of this later, along with "MOV", indirect
\ loads and stores, branches, and conditional jumps that we
\ will use to build our virtual machine we can execute Forth
\ on.
\
\ This should give you some idea of how the SUBLEQ machine
\ works, but it is a long journey to get anything useful built
\ upon it.
\
\ ## Meta-compilation (Cross compilation with Forth)
\
\ To recap how this tool-chain works:
\
\ 1. An assembler for the SUBLEQ machine is made.
\ 2. A virtual machine is made with that assembler that can
\    support higher level programming constructs, specifically,
\    the easy execution of Forth.
\ 3. Forth word definitions are built up, which are then used
\    to construct a full Forth interpreter.
\ 4. The resulting image is output to standard out.
\
\ Check the references for more detailed examples for other
\ systems. Some keywords that will help are; Forth,
\ Meta-compilation, Cross compiler, eForth, 8086 eForth,
\ JonesForth, j1eforth. It is quite a complex system to
\ understand in one go and looking at other Forth
\ implementations will certainly help.
\
\ There are a few different ways of bringing up a Forth system,
\ for different environments. The environments are:
\
\ * An interpreter designed to execute Forth directly written
\ in another language, usually C. This can either be on the
\ bare metal or hosted.
\ * A physical machine such as a Z80, a x86, or an ARM CPU.
\ This can be with or without an operating system.
\ * A physical machine designed to execute Forth directly,
\ such as the H2, the J1, or various other. This environment
\ usually does not have an operating system.
\
\ We can also bring up the initial system by either writing
\ in assembly for the target, or by writing a Forth program
\ called a "meta-compiler" (similar but distinct from the
\ more well known computer science concept). The meta-compiler
\ is more akin to a special cross-compiler written in Forth
\ that uses the capabilities of the Forth language well.
\
\ Forth it quite a difficult language to use, but it is quite
\ good at making assemblers and cross compilers for bringing
\ up a Forth system on a new platform. It has niches in which
\ it works quite well (unfortunately nearly none of which are
\ applicable to modern systems).
\
\ The system contained within targets a physical machine
\ not designed to execute Forth (SUBLEQ) and we will be
\ making a meta-compiler to build upon that machine. The
\ techniques used to target the different systems have some
\ similarities.
\
\ We still start by describing the meta-compiler words,
\ assembler, and support words used to make this Forth.
\
\ One thing that needs to be said about eForth compared to
\ other Forth systems is that it is *case sensitive*,
\ historically the majority of Forth implementations were
\ case insensitive and had upper case as the default. The
\ ANS Forth standard (which is not used for this interpreter
\ unless convenient to do so) requires that Forth
\ implementations that follow it must at least allow the user
\ to type in upper case versions of each Forth word. I think
\ case insensitivity is a very poor idea, and that lower case
\ is better than upper case, so this is one area where this
\ Forth implementation makes a break from then norm, like other
\ eForth implementations.
\
\ The astute reader will notice that this document does not
\ start with comments, but with a line of Forth code. Usually
\ Forth programs print "ok" and a newline after each correctly
\ parsed and executed Forth line, however because the output
\ image it spat out to the normal output stream we want to
\ silence all extraneous noise on it.
\
\ After this, we will want to get the interpreter into a
\ known good state with the following line:
\
only forth definitions hex

\ If you are not familiar with these words, it would help
\ that you do, as we will be using the Forth vocabulary
\ word set to create words for the meta-compiler, and for
\ the target dictionary and assembler.
\
\ This is one of the advantages of explaining a Forth written
\ in pure assembler as opposed to one written in a Forth
\ meta-compiler. Nearly every competent programmer is capable
\ or understanding assembler programs to a degree even if they
\ are not intimately familiar with the platform. However a
\ Forth cross compiler requires an understanding of Forth and
\ how vocabularies work, along with search orders, and the
\ like. It is possible to come back to these things.
\
\ The words "(order)", "-order", "+order" are defined as the
\ built in words for manipulating Forth vocabularies are
\ appalling. "+order" and "-order" allow us to add and remove
\ a Forth word-list to the search order easily, "+order" adds
\ a word-list so long as it has not already been added.
\
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;

\ You will notice some code is only executed for gforth,
\ which does not by default have the word "eforth" defined,
\ and some for the eForth interpreter running on the SUBLEQ
\ machine. The structure:
\
\ 	defined eforth [if]
\		( CODE EXECUTED IN EFORTH )
\	[else]
\		( CODE EXECUTED IN GFORTH )
\	[then]
\
\ Is used, seldomly. Here it is used because "wordlist" is not
\ defined in the base eForth image, but is in gforth. To bring
\ them up to parity, "wordlist" is defined for the eForth
\ image.
\

defined eforth [if]
  : wordlist here cell allot 0 over ! ;
[then]

\ We then define the following wordlists, "meta.1" is used for
\ the meta-compiler, words for image generation, some
\ constants, for making word definitions go into specific
\ vocabularies, go here. Some example words include "t@", for
\ fetching a value from the generated target image, or "t:"
\ for switching to the "target.1" vocabulary and defining new
\ words in it. "target.1" contains words that refer to the
\ target vocabulary, that is words that have been defined
\ within the new eForth image we are creating. We will want
\ to refer to them, such that when we use "+" we will want
\ as some point for that "+" to refer to a location within
\ the target image. "assembler.1" is for words relating to
\ the assembler that we use to tame the SUBLEQ machine, and
\ "target.only.1" is not strictly necessary, but it is to
\ refer to define words that only exist in the target
\ dictionary.
\

wordlist constant meta.1        ( meta-compiler word set )
wordlist constant target.1      ( target eForth word set )
wordlist constant assembler.1   ( assembler word set )
wordlist constant target.only.1 ( target only word set )

\ New definitions will now go into the "meta.1" vocabulary.

defined eforth [if] system +order [then]
meta.1 +order definitions

\ Some system constants are defined, "=cell" is the size of
\ a cell in bytes, for out 16-bit machine it is "2", if we
\ want to allocate a cell within the image, we will need to
\ refer to this constant.

   2 constant =cell  \ Target cell size
4000 constant size   \ Size of image working area
 100 constant =buf   \ Size of various buffers in target
 100 constant =stksz \ Size of return and variable stacks
0008 constant =bksp  \ Backspace character value
000A constant =lf    \ Line feed character value
000D constant =cr    \ Carriage Return character value
007F constant =del   \ Delete character
FC00 constant =stack-start \ Initial start of stack

\ Create an area to store the new image in, called "tflash",
\ and clear it. The image generate is not that big. We will
\ need under 16KiB (although we are cutting it close).
\

create tflash tflash size cells allot size erase

\ "tdp" is the target dictionary pointer, "there" will return
\ the contents of it when it is defined. "t" is usually used
\ as a prefix to mean "target".
\
\ "tlast" is a pointer to the last defined word in the target.
\
\ "tlocal" is used to allocate room within a thread local
\ storage system, which is used to build cooperative
\ multitasking into the system.
\

variable tdp 0 tdp !
variable tlast 0 tlast !
variable tlocal 0 tlocal !

\ We will be switching between vocabularies later, by using
\ word pairs like ":m"/";m" and ":t"/";t" we can define words
\ and put them in the "meta.1" or "target.1" word sets
\ automatically.
\

: :m meta.1 +order definitions : ; ( --, "name" )
: ;m postpone ; ; immediate ( -- )

\ ";m" is a no-op, as we never want to remove "meta.1" from
\ the search order.
\
\ "tcell", "there", "tc!", "tallot", and the following words
\ all share a similar theme, they are the same as the Forth
\ words without the "t" prefix except they are used to
\ manipulate the generated target image stored in "tflash"
\ instead of arbitrary blocks of memory.
\
\ For example "tc!" is just "c!", but writes to "tflash".
\ "t!" also writes to "tflash", but only a single target cells
\ worth of data.
\

:m tcell 2 ;m ( -- 2 : bytes in a target cell )
:m there tdp @ ;m ( -- a : target dictionary pointer value )
:m tc! tflash + c! ;m ( c a -- : target write char )
:m tc@ tflash + c@ ;m ( a -- c : target get char )
:m t! over FF and over tc! swap 8 rshift swap 1+ tc! ;m
:m t@ dup tc@ swap 1+ tc@ 8 lshift or ;m ( a -- u : target @ )
:m taligned dup 1 and + ;m ( u -- u : align target pointer )
:m talign there 1 and tdp +! ;m ( -- : align target dic. ptr. )
:m tc, there tc! 1 tdp +! ;m ( c -- : write char to targ. dic.)
:m t, there t! 2 tdp +! ;m ( u -- : write cell to target dic. )
:m tallot tdp +! ;m ( u -- : allocate bytes in target dic. )

\ "tpack" is used to copy a string into the target image.
\ Useful if we want to define strings in the target, which we
\ will when defining new words in the target header.
\
\ "limit" is used on systems which are not 16-bit (ie. Gforth)
\ to limit the maximum value of a number to be modulo 2 raised
\ to the 16. It does nothing on SUBLEQ machine.
\

defined eforth [if]
  :m tpack dup tc, for aft count tc, then next drop ;m
  :m parse-word bl word ?nul count ;m
  :m limit ;m
[else]
  :m tpack talign dup tc, 0 ?do count tc, loop drop ;m
  :m limit FFFF and ;m
[then]

\ "\$literal" is defined now, but will not be of much use
\ until much later, when we can compile strings into the
\ target dictionary, until then it cannot be used.
\
\ "\$literal" parses input until a double quote is reached
\ and then compiles that string into the dictionary. It also
\ takes care of alignment, given its description in this
\ paragraph, why can we not use it? Because there is no code
\ yet written for the target to print out the strings that
\ "\$literal" can compile into the target dictionary, until
\ that is made we will not be able to make the meta-compiler
\ words that will utilize the target string printing words.
\

:m $literal talign [char] " word count tpack talign ;m

\ Some more conditional compilation, this time it is because
\ of the potential differences in arithmetic between the
\ gforth implementation and the SUBLEQ eForth (which is always
\ 16-bit). The image saving routine need to print out a 16-bit
\ signed value, so for gforth implementations that are likely
\ to be 32 or even 64 bit, the value to be printed out will
\ need to be sign-extended from a 16-bit signed value to a
\ 32/64 bit signed value, for the SUBLEQ eForth, that does
\ not need to be done. Both versions of "#dec" print out
\ 16-bit signed values is the important take away.
\

defined eforth [if]
  :m #dec dup 0< if [char] - emit then (.) ;m ( n16 -- )
[else]
  :m #dec dup 8000 u>= if negate limit -1 >r else 0 >r then
     0 <# #s r> sign #> type ;m ( n16 -- )
[then]

\ Some of the last words called to make the image, "mdump"
\ dumps a section of memory in the format that we would like,
\ a single signed sixteen bit decimal value per line with
\ a Unix line ending after it.
\
\ A line ending is used instead of a space as it makes the
\ diff tools recognize similar sections in the SUBLEQ image,
\ instead of a single line changing if only a tiny bit of has
\ changed.
\
\ "save-target" actually calls "mdump", making sure we are in
\ decimal base before hand, which the output format requires.
\
\ Due to defining our own versions of "only", "forth",
\ "definitions" and "hex" we will define a word called ".end"
\ that will restore the interpreter to the correct state.
\
0 constant cgen
cgen [if] :m msep 2C emit ;m [else] :m msep A emit ;m [then]
:m mdump taligned
  begin ?dup
  while swap dup @ limit #dec msep tcell + swap tcell -
  repeat drop ;m
:m save-target decimal tflash there mdump ;m
:m .end only forth definitions decimal ;m

\ "atlast" retrieves the last defined word, or a pointer to
\ to it. It will be used to set the variable which will
\ contain the Forth vocabulary, and the "{last}" variable,
\ which contains the last defined word when running the target
\ eForth.

:m atlast tlast @ ;m ( -- a )

\ "lallot" allocates space for a USER variable, which is just
\ an offset into the task thread, each thread has a 1024 byte
\ block used to store the tasks variable and return stacks,
\ buffers, and also USER variables - task specific or thread
\ local storage. "lallot" keeps track of an offset into thread
\ local storage ("tlocal"). "tuser" can be used to allocate
\ a cell in local storage space, and assign a name for that
\ space in the meta-compiler, which when run will compile that
\ offset into the target image. There is quite a limited amount
\ of space within the thread, so many variables should not be
\ allocated.
\
\ "tvar" is a more conventional variable, however, much like
\ "tuser" the name is not copied into the target dictionary.
\ It creates a global variable instead of a thread local one.
\
\ "label:" will create a label, which is just used within the
\ assembler. The label will push the location of where it was
\ made onto the variable stack during meta-compilation.
\
\ "tdown" aligns a cell downwards, ignoring the byte bit.
\
\ "tnfa" and "tcfa" move to the Name Field Address and the
\ Code Field Address in the given target word. They are much
\ like "nfa" and "cfa", except they are meant to run during
\ cross-compilation and not in the target.
\
\ This is an odd collection of words, but they will all be
\ used during meta-compilation.
\

:m lallot >r tlocal @ r> + tlocal ! ;m
:m tuser ( --, "name", Created-Word: -- u )
  get-current >r meta.1 set-current create r>
  set-current tlocal @ =cell lallot , does> @ ;m
:m tvar get-current >r ( --, "name", Created-Word: -- a )
     meta.1 set-current create
   r> set-current there , t, does> @ ;m
:m label: get-current >r ( --, "name", Created-Word: -- a )
     meta.1 set-current create
   r> set-current there ,    does> @ ;m
:m tdown =cell negate and ;m ( a -- a : align down )
:m tnfa =cell + ;m ( pwd -- nfa : move to name field address )
:m tcfa tnfa dup c@ 1F and + =cell + tdown ;m ( pwd -- cfa )

\ "compile-only" and "immediate" set flags in the last defined
\ word. In a quest to make the meta-compiled Forth look like
\ normal Forth code as much as possible they have not been
\ given new names, like "mimmediate", to indicate they are part
\ of the meta-compiler. A similar effort is made with other
\ parts of the meta-compiler, such as strings, to make the
\ code as Forth like as possible.
\
\ The format of the header will be described shortly, we will
\ have to make new headers for words ourselves in the new
\ Forth interpreter, the format will then be re-described later
\ when making the words that create functions in the target
\ image such as ":" and "create".
\

:m compile-only tlast @ tnfa t@ 20 or tlast @ tnfa t! ;m ( -- )
:m immediate    tlast @ tnfa t@ 40 or tlast @ tnfa t! ;m ( -- )

\ "half" and "double" are just synonyms for "2/" and "2\*", it
\ is much easier to know you are calling the correct version of
\ the words when they have different names. We will need to
\ convert from Forth to VM addresses with these two when
\ setting various execution tokens.
\

:m half dup 1 and abort" unaligned" 2/ ;m ( a -- a )
:m double 2* ;m ( a -- a )

\ Some more conditional code due to the differences between the
\ implementations of the single quote, "'", on the two
\ different Forth implementations used to compile this program.
\
\ "\>body" works (correctly) only on words defined with
\ "create". It moves an execution token pointer to point to the
\ body of (hence the name) a created Forth word, which will
\ contain the data contained within that word. In our case,
\ a pointer which is usually compiled into the target with
\ the "does\>" section of the Forth word.
\
\ "t'" and "to'" search "target.1" and "target.only.1"
\ vocabularies for a word which should have been defined with
\ the meta-compiler version of ":". It then uses "\>body" as
\ mentioned above. These two are usually used when we want to
\ compile a literal containing the address, instead of a call,
\ to an address, so at run time the address of a function is
\ pushed to the stack instead of being run.
\

defined eforth [if]
:m (') bl word find ?found cfa ;m
:m t' (') >body @ ;m ( --, "name" )
:m to' target.only.1 +order (') >body @ target.only.1 -order ;m
[else]
:m t' ' >body @ ;m ( --, "name" )
:m to' target.only.1 +order ' >body @ target.only.1 -order ;m
[then]

\ "tcksum" is used to calculate the checksum over the part of
\ the image that is checked. At the end of the meta-compilation
\ process this value is calculated and poked into the target
\ image. The corresponding word in the target is called
\ "cksum". The algorithm only uses addition, which makes for a
\ very weak form of checksum, but it is easy to compute.
\
:m tcksum taligned dup C0DE - FFFF and >r
   begin ?dup
   while swap dup t@ r> + FFFF and >r =cell + swap =cell -
   repeat drop r> ;m ( a u -- u : compute a checksum )
:m mkck dup there swap - tcksum ;m ( -- u : checksum of image )


\ "postpone" is used much like the normal Forth version of
\ "postpone", but within the context of the cross-compilation
\ process it compiles a reference to a word from the target
\ instead of executing a meta-compiler word. The words that
\ are in "target.only.1" are usually the ones we want to
\ "postpone". It is not always needed to use "postpone" on
\ some of the words it is called on, however to make the cross
\ compiled code more similar to actual Forth code it is
\ executed on "immediate" words being compiled in the target,
\ even though their immediateness only applies to it when the
\ target is running, which it is not.
\

:m postpone ( --, "name" )
   target.only.1 +order t' target.only.1 -order 2/ t, ;m


\ The dictionary in Forth is a list of all of the "words", or
\ functions, available to the system, the "words" each have a
\ header, and the dictionary and headers looks like this:
\
\	+--------------------+
\	| Vocabulary Pointer |
\	+--------------------+
\	    |
\	   \|/
\	    .
\	+------+----+-----------+--------...
\	| Prev | CB | Word Name | Code...
\	+------+----+-----------+--------...
\	    |
\	   \|/
\	    .
\	+------+----+-----------+--------...
\	| Prev | CB | Word Name | Code...
\	+------+----+-----------+--------...
\	    |
\	   \|/
\	    .
\	+-------+
\	| NULL  |
\	+-------+
\
\	Prev: Pointer to previous word in linked list
\	CB:   Part of the Name Field Address along with
\	      the first byte of "Word Name", it contains
\	      word header flags, as well as the length of
\	      "Word Name", which is a variable length string.
\	Word Name: The name of the Forth word, it is a variable
\	      length string. The length of which is stored in
\	      the lowest five bits of Cb.
\	Code...: A variable length series cells that form the
\	      code portion of the Forth word, usually ending
\	      in an exit/return instruction.
\	NULL: The end of the linked list.
\	Vocabulary Pointer: The handle used to identify the
\	      vocabulary, a pointer to the start of the linked
\	      list of words.
\
\ The word "thead" makes a header for a word in the target, it
\ writes a pointer to the previously defined word making a link
\ in the dictionary linked list, parses the next word in the
\ input stream, and copies that name into the target dictionary
\ with "tpack", making sure to align things for code generation
\ stage. "thead" is not called directly, but is called by
\ "header", which saves and restores the "\>in" variable, this
\ is done so that the name we just parsed from the input stream
\ is parsed *again* as we want to be able to create a word in
\ "target.1" that when referred to compiles a pointer to the
\ word header we just made. That is, we want to create *two*
\ words, one in the target image and one in the cross compilers
\ dictionary that we can use for compilation. If this seems
\ confusing, that is probably because it is.
\
\ The word ":ht" is used to create a word in the cross
\ compiler that when called compiles a pointer to the Code
\ Field Address (the code) of a word. It is reliant on "header"
\ resetting the input stream as mentioned. It can also be used
\ to create a word in the target with no header, if used by
\ itself without calling "header", this is done to save space
\ in the target.
\
\ ":t" is used to create a normal word definition in the
\ target, that is one with a header in the target image, it
\ just calls "header" and then ":ht".
\
\ ";t", used to terminate a word definition, is defined much
\ later, we will need to compile a "return" or an "exit" into
\ the word, however that requires us making one, which will
\ be done in the Forth Virtual Machine.
\

:m thead talign there tlast @ t, tlast !
   parse-word talign tpack talign ;m ( --, "name" )
:m header >in @ thead >in ! ;m ( --, "name" )
:m :ht ( "name" -- : forth routine, no header )
  get-current >r target.1 set-current create
  r> set-current BABE talign there ,
  does> @ 2/ t, ;m

:m :t header :ht ;m ( "name" -- : forth routine )

\ ":to" is similar to ":t", it makes a header in the target,
\ but instead of putting word definitions in the cross compiler
\ into "target.1" it puts them into "target.only.1", so they
\ can be accessed if needed, but only with special effort. This
\ is used because sometimes, especially when making
\ meta-compiler words, we will need to do some computation or
\ stack manipulation and we do not want to call the target
\ version of, say "dup", or "2/", but the normal Forth version
\ to actually perform a duplication or division *now* on an
\ actual stack item.
\
\ ":a" is similar to ":to", except it puts things into the
\ assembler vocabulary which we will be using first. ";a" is
\ defined later on, but instead of compiling a return it will
\ compile a jump to the Forth Virtual Machine we will be making
\ in assembly.
\
\ Notice the compiler security, ":a", for example, pushes the
\ hexadecimal value "D00D" and "(a);" (which ";a" will call)
\ throws an exception if that value is not on the stack when
\ it is called. This catches quite a few errors, using the
\ wrong word definitions from the wrong vocabulary, not
\ compiling literals correctly, etc.
\

:m :to ( "name" -- : forth, target only routine )
  header
  get-current >r
    target.only.1 set-current create
  r> set-current
  BABE talign there ,
  does> @ 2/ t, ;m
:m :a ( "name" -- : assembly routine, no header )
  D00D target.1 +order definitions
  create talign there , assembler.1 +order does> @ 2/ t, ;m
:m (a); D00D <>
   if abort" unstructured" then assembler.1 -order ;m

\ We no longer need the system vocabulary, if we are running
\ the cross-compiler on the eForth image (we never did if
\ running on gforth), so to keep things clean we will remove
\ it from the search order.

defined eforth [if] system -order [then]

\ # Forth Virtual Machine
\
\ This section contains the Forth Virtual Machine; a tiny
\ VM that does the bare minimum surround by about forty or
\ so instructions which when implemented can be used to make
\ porting relatively trivial.
\
\ Implementing and debugging the Forth VM was the hardest task
\ of porting this Forth to the SUBLEQ platform, usually it is
\ not that that difficult, and a Forth can be ported quite
\ rapidly by a skilled programmer familiar with the language,
\ perhaps in a week or two, with a few more for bugs, given
\ that they are familiar with the target. However with only
\ a single instruction it is incredibly difficult to perform
\ even the most basic tasks you expect from a CPU. Once the
\ VM was finished (it was more of an iterative process between
\ implementing the Forth VM and the Forth Code) porting the
\ Forth interpreter itself was relatively trivial as this
\ eForth is written mostly in Forth.
\
\ The memory layout of the Forth Virtual Machine is as follows:
\
\	0: Zero Register.
\ 	1: Zero Register.
\ 	2: Jump to Forth Virtual Machine Start.
\	3: Options variable.
\       3 to W-1: System variables.
\	...
\	W: label start; The Forth VM entry point.
\	X: label vm; The Forth VM.
\	Y: Forth VM instruction implementation.
\	Z: Forth Code that uses Forth VM instructions.
\
\ Cells 0, 1, and 2 also form the first SUBLEQ instruction,
\ and the first two cells must be zero, as mentioned
\ previously in the SUBLEQ introduction section, the first
\ location forms the "Z" register.
\
\ The options variable is designed to be modified, by hand
\ if needed, to create custom images. It contains behavior
\ related to things that might change in the hardware or the
\ system it is running on, like for example whether it should
\ echo terminal output or not. The defaults should be fine if
\ the SUBLEQ machine is the example C program running under a
\ normal operating system.
\
\ Now to define a few macros that will allow us to perform
\ addition, loads, stores, indirect loads and stores, moves,
\ some input and output, and a few other things.
\
\ The following words could also go into the target assembler,
\ which is usually part of a vocabulary called "assembler",
\ and can be activated to create words written in assembler
\ with the words "CODE" and "END-CODE", those will not be
\ defined in this eForth, there would also need to be a
\ mechanism to call Forth words written in assembler as well.
\
\ Onto the words themselves, they all write instructions into
\ the target image with "t,".
\
\ "Z" is the first word defined. It writes a zero into a cell,
\ it is meant to be used as a register location that starts
\ off as zero, as mentioned, and should end backup as zero
\ by the and of the instruction. It is not an instruction, but
\ is used to build one. The same with "NADDR", which compiles
\ the next memory location into the current cell.
\
\ "HALT" is our first instruction, "0 0 -1" will always halt
\ the machine. This will be used to implement the Forth word
\ "bye", but it is useful for debugging the lower levels when
\ the Forth VM had to be built.
\
\ "JMP" accepts a Forth Address, converts it to a Cell Address,
\ and when run, it will unconditionally jump to that location.
\
\ "ADD" is more complex than "SUB", they both have the same
\ form:
\
\	source destination instruction
\
\ Where both "source" and "destination" are Forth byte
\ addresses (which should always be aligned), for "SUB" the
\ "source" will subtracted from the "destination" and then
\ stored in the "destination". For "ADD", it will be added.
\
\ "NOOP" is the "no-operation" instruction, because we need
\ to write self-modifying code it is sometimes useful to write
\ no-op that will get modified later, which can be seen in
\ "iJMP".
\
\ "ZERO" zeros a memory location by subtracting that location
\ from itself, it then jumps to the next memory location in
\ case its test passes, if it fails, it will advance to the
\ next location anyway.
\
\ "GET" and "PUT" are built into the SUBLEQ machine, and are
\ simple to implement, just put the "-1" in the correct place.
\ They both accept a memory location to get data from or write
\ to. They are used to get and put a single byte of input or
\ output.
\
\ "MOV" copies the location of a cell to another one, it
\ requires for individual SUBLEQ instructions to implement.
\ "a b MOV" would copy "a" to "b", first "b" is zeroed, then
\ "a" is negated and stored in "Z", then "Z" which now contains
\ the negated "a" is subtracted from "b", moving the contents
\ stored a "a" to "b". Finally "Z" is returned to its original
\ state by subtracting "Z" from itself, so it now contains
\ zero as it should. Four instructions.
\
\ "iLOAD" does an indirect load, "a b iLOAD" would use "b"
\ as an address to do an indirect load through and store the
\ result in "a". It does this by using the "MOV" instruction
\ to modify a second "MOV" instruction to point to a different
\ location. Notice how the number of SUBLEQ multiplies quite
\ quickly to achieve anything, two moves is eight instructions,
\ "iSTORE" is even worse at four "MOV" instructions.
\
\ "iJMP" is simpler than "iLOAD", it just has to modify a
\ "NOOP" instruction to point to a different location, changing
\ the final cell in the "NOOP", turning it in effect into a
\ unconditional jump.
\
\ "iSTORE" is the most complex of all of these single
\ instruction macros, as mentioned, it contains four "MOV"
\ instructions, each containing four SUBLEQ instructions,
\ meaning twelve SUBLEQ instructions, or thirty six cells just
\ to perform an indirect store. It requires three "MOV"
\ instructions in order to modify three locations in a
\ final "MOV", leaving the last instruction of the modified
\ "MOV" the same, to zero out the "Z" register.
\
\ There are remarkably few macro instructions in the base;
\ I/O, Loads/Stores, and arithmetic. The conditional 
\ instruction macros will be defined in the next section. 
\ 

:m Z 0 t, ;m ( -- : Address 0 must contain 0 )
:m NADDR there 2/ 1+ t, ;m ( --, jump to next cell )
:m HALT 0 t, 0 t, -1 t, ;m ( --, Halt but do not catch fire )
:m JMP 2/ Z Z t, ;m ( a --, Jump to location )
:m ADD swap 2/ t, Z NADDR Z 2/ t, NADDR Z Z NADDR ;m
:m SUB swap 2/ t, 2/ t, NADDR ;m ( a a -- : subtract )
:m NOOP Z Z NADDR ;m ( -- : No operation )
:m ZERO dup 2/ t, 2/ t, NADDR ;m ( a -- : zero a location )
:m PUT 2/ t, -1 t, NADDR ;m ( a -- : put a byte )
:m GET 2/ -1 t, t, NADDR ;m ( a -- : get a byte )
:m MOV 2/ >r r@ dup t, t, NADDR 2/ t, Z  NADDR r> Z  t, NADDR
   Z Z NADDR ;m
:m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m ( a a -- )
:m iJMP there 2/ E + 2* MOV NOOP ;m ( a -- )
:m iSTORE ( a a -- )
   swap >r there 2/ 24 + 2dup 2* MOV 2dup 1+ 2* MOV 7 + 2* MOV
   r> 0 MOV ;m

\ To simplify program flow the normal control structures
\ will be defined, they work a little differently than the
\ Forth ones, as they must be given an address instead of
\ taking items off of the stack, however they make everything
\ a lot easier to do and provide us with the basics from the
\ structured programming paradigm. Programming in straight
\ assembler sucks.
\
\ The assembler versions of the control structures will work
\ a little differently from the way they do in Forth. In Forth
\ "if" pulls an item off of the variable stack to test again,
\ however the assembler version of "if" instead reads (and does
\ not modify) a memory location, so it must be used like so:
\
\	<location> if ... then
\
\ The same goes for "until", and "while", "+if", and "-if".
\ "+if" and "-if" are also new, they will execute if the
\ memory location provided contains a positive or a negative
\ value respectively, which is easy to compute in SUBLEQ.
\
\ TODO: More description
\

assembler.1 +order definitions
: begin talign there ;
: again JMP ;
: mark there 0 t, ;
: if ( NB. "if" does not work for 8000 )
   2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t,
   mark ;
: until 2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t,
   Z Z NADDR Z t, 2/ t, ;
: +if   Z 2/ t, mark ;
: -if 2/ t, Z there 2/ 4 + t, Z Z there 2/ 4 + t, Z Z mark ;
: then begin 2/ swap t! ;
: while if swap ;
: repeat JMP then ;
assembler.1 -order
meta.1 +order definitions

\ # The System Variables
\
\ There is not a lot in this section in terms logic, it nearly
\ all allocation of variables. They will be described, however
\ they are mostly registers, and memory locations that will
\ not make sense until later, it might be best just to refer
\ back to this chapter later on.
\
\ This section also contains the very first instruction to
\ be executed, which is formed by three "t," statements.
\
\ Initially "0 t, 0 t, -1 t," is written into the target
\ image. This might seem like the first instruction would
\ halt the interpreter, and it would, were it not for the
\ fact that the third value is overwritten later with the
\ location of the Forth Virtual Machine entry point.
\
\ The first two memory locations must both be zero, the
\ second memory location must be zero so it does not interfere
\ with the first jump, the same for the first instruction,
\ however the first location is also used as the "Z" location
\ in SUBLEQ instructions.
\

  0 t, 0 t,        \ both locations must be zero
label: entry       \ used to set entry point in next cell
  -1 t,            \ system entry point
  B tvar {options} \ bit #1=echo off, #2 = checksum on,
                   \ #4=info, #8=die on EOF
  0 tvar primitive \ any address lower must be a VM primitive
  8000 tvar hbit   \ must contain 8000
  =stksz half tvar stacksz \ must contain 80
  -2   tvar ntwo   \ must contain -2
  -1 tvar neg1     \ must contain -1
  1 tvar one       \ must contain  1
  2 tvar two       \ must contain  1
 10 tvar bwidth    \ must contain 16
  0 tvar INVREG    \ temporary register used for inversion only
  0 tvar w         \ working pointer
  0 tvar x         \ working pointer
  0 tvar t         \ temporary register for Virtual Machine
  0 tvar bl1       \ bitwise extra register
  0 tvar bl2       \ bitwise extra register
  0 tvar bt        \ bitwise extra register

  0 tvar h         \ dictionary pointer
  =stack-start half tvar {up} \ Current task addr. (Half size)
  F00 tvar {ms}    \ delay loop calibration variable
  0 tvar check     \ used for system checksum

\ These variables will described at a later point, there is
\ no point in going over them again, but they are the memory
\ locations that are referred by words without the "{}"
\ brackets, so for example the word "cold", defined later on,
\ is defined as:
\
\	:t cold {cold} lit @ execute ;t
\
\ It just refers to "{cold}", what "cold" does will be
\ described later at a more appropriate juncture.
\

  0 tvar {context} E tallot \ vocabulary context
  0 tvar {current}  \ vocabulary to add new definitions to
  0 tvar {forth-wordlist} \ forth word list (main vocabulary)
  0 tvar {editor}   \ editor vocabulary
  0 tvar {root-voc} \ absolute minimum vocabulary
  0 tvar {system}   \ system functions vocabulary
  0 tvar {cold}     \ entry point of VM program, set later on
  0 tvar {dirty}    \ is block dirty?
  0 tvar {last}     \ last defined word
  0 tvar {cycles}   \ number of times we have switched tasks
  0 tvar {single}   \ is multi processing off?
  2F tvar {blk}     \ current loaded block
  2F tvar {scr}     \ last viewed screen
  0 tvar padding    \ BUG: Getting during image generation

\ Most of these are thread local variables, with the exception
\ of "ip" and "tos", the stack variables "{rp}" and "{sp}" and
\ the initial stack positions "{rp0}" and "{rp0}", which are
\ only stored in thread local storage on a task switch, all
\ words defined with "tuser" are locations of memory relative
\ to thread local storage.
\
\ The stacks pointers "{rp}" and "{sp}" themselves point into
\ a stack location.
\
\ There are lots of interesting variables here, but they will
\ be described later.
\
\ Part of the initial stack is also set up here, there is a
\ word called "task-init", defined later, which initializes
\ new tasks. It sets up where the locations of variable
\ and return tasks as well as initial execution hooks with
\ that task. However, for the first task the stacks and input
\ buffers need to be set up manually.
\

  \ Thread variables, not all of which are user variables
  0 tvar ip        \ instruction pointer
  0 tvar tos       \ top of stack
  =stack-start =stksz        + half dup tvar {rp0} tvar {rp}
  =stack-start =stksz double + half dup tvar {sp0} tvar {sp}
  200 constant =tib \ Start of terminal input buffer
  380 constant =num \ Start of numeric input buffer
  tuser {next-task} \ next task in task list
  tuser {ip-save}   \ saved instruction pointer
  tuser {tos-save}  \ saved top of variable stack
  tuser {rp-save}   \ saved return stack pointer
  tuser {sp-save}   \ saved variable stack pointer
  tuser {base}      \ input/output radix
  tuser {dpl}       \ number of places after fraction
  tuser {hld}       \ hold space pointer
  tuser {in}        \ position in query string
  tuser {key}       \ execution vector for key?
  tuser {emit}      \ execution vector for emit
  tuser {literal}   \ execution vector for literal
  tuser {ok}        \ execution vector for .ok
  tuser {echo}      \ execution vector for echo
  tuser {state}     \ compiler state
  tuser {handler}   \ throw/catch handler
  tuser {sender}    \ multitasking; msg. send, 0 = no message
  tuser {message}   \ multitasking; the message itself
  tuser {id}        \ executing from block or terminal?
  tuser {tib}       \ terminal input buffer: cell 1,
  =cell lallot      \ terminal input buffer: cell 2

\ Ideally these meta-compiler macros would be defined along
\ with the other meta-compiler words, however they require
\ the locations of constants to be know, such as "one" or
\ "neg1". This will happen throughout, the definitions of
\ meta-compiler words that require knowledge of a Forth
\ function that has yet to be defined, or of a new constant,
\ will require a non-contiguous grouping of these functions.
\
\ "INC" and "DEC" are self explanatory, they increment or
\ decrement a memory location by subtracting negative one or
\ subtracting one from it. They can be implemented in a single
\ instruction.
\
\ The words "++sp"/"--sp", and "++rp"/"--rp" are for
\ incrementing and decrementing the data and return stacks
\ of the Forth interpreter. They move in opposite directions,
\ the data stack growing down and the return stack growing up,
\ allowing them to use the memory area used to store both
\ stacks more efficiently.
\
\ "a-optim" is used to perform a minor optimization in our
\ SUBLEQ assembly routines, if we have an arbitrary assembly
\ instruction that always jumps to the next instruction after
\ it, and if that next instruction is an unconditional jump,
\ we replace the arbitrary assembly instructions jump location
\ to be the same as the unconditional jump, potentially saving
\ a single cycles execution. It may help to illustrate this:
\
\	subleq ?, ?
\	subleq Z, Z, c
\
\ The first "subleq" instruction always branches to next one,
\ the second one always branches to "c". We can replace this
\ with the equivalent:
\
\	subleq ?, ?, c
\	subleq Z, Z, c
\
\ The first instruction *may* branch to "c", the second one
\ always will. It is a minor optimization that is easy to
\ implement, so it might as well be done.
\
\ The word "INV" assembles a bitwise invert, the only bitwise
\ operation we can perform easily. It uses the fact that
\ a subtraction using twos-compliment arithmetic is equivalent
\ to the following:
\
\	b - a = b + ~a + 1
\
\ If we would like to invert "a", we must get rid of "b" and
\ the +1 terms. We perform the subtraction, zeroing "b" first
\ (which is "INVREG" in the code), then just subtract one.
\
\ The other bitwise operations will be much more difficult
\ to implement. When making the system for the first time,
\ getting those operators correct was the most onerous task
\ of bringing the system up, as a bug in those operators makes
\ everything else more difficult to debug.
\

:m INC 2/ neg1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ one  2/ t, t, NADDR ;m ( b -- )
:m INV ( b -- : invert NB. b - a = b + ~a + 1 )
  INVREG ZERO dup INVREG SUB dup INVREG swap MOV DEC ;m
:m ++sp {sp} DEC ;m ( -- : grow variable stack )
:m --sp {sp} INC ;m ( -- : shrink variable stack )
:m --rp {rp} DEC ;m ( -- : shrink return stack )
:m ++rp {rp} INC ;m ( -- : grow return stack )
:m a-optim >r there =cell - r> 2/ t! ;m ( a -- )

\ # The Core Forth Virtual Machine
\
\ The core of the Forth Virtual Machine starts here, it is
\ quite short and is executed for each Virtual Machine
\ instruction, so any optimization here would make a huge
\ difference. It only has a few tasks and the Wikipedia page
\ on Threaded Code Interpreters would help in understanding
\ how it works.
\
\ The entry point is the label "start", which is executed
\ on startup naturally, it just sets up the initial stack
\ positions in "{sp}" and "{rp}" for the data and return stacks
\ respectively. It also sets the first instruction to be
\ executed by moving the contents of "{cold}" into "ip", the
\ Instruction Pointer.
\
\ The "vm" label does all the work. It implements a small
\ "virtual machine", a Threaded Code Interpreter, also known
\ as the Forth Inner Interpreter.
\
\ It only has two things it does; either execute a primitive
\ operation (or a VM instruction) or perform a call into
\ Forth code (which in turn can either perform calls or jump
\ to VM instructions).
\
\ The VM has a number of registers that are well known across
\ different Forth implementations, such as "ip" (the
\ Instruction Pointer), "w" (the Working Pointer). Also
\ required is a stack to place the calls, which uses "{rp}"
\ (the Return stack Pointer).
\
\ The way the Forth VM determines whether an instruction is a
\ call or a VM instruction to jump to is by assuming any
\ address bellow a value contained in "primitive" (which will
\ be set later) is a VM instruction, anything about that should
\ be something that can be called instead. This is done because
\ all that is needed to test this is a subtraction and jump,
\ all of which the SUBLEQ machine can do easily.
\
\ There are three main types of threaded code interpreters;
\ direct, indirect and subroutine. There are other kinds, but
\ they will not be considered.
\
\ We can imagine a Forth program as mostly consisting of a
\ series of calls, with a few primitives and mechanisms thrown
\ in. For example, let us define a function that computes
\ the square root of "a" squared plus "b" squared (given an
\ already defined "isqrt" for computing the integer square
\ root.
\
\	: square dup * ;
\ 	: pythagoras square swap square + isqrt ;
\
\ In the above example, and in this interpreter, "dup", "*",
\ and "swap" are primitives, "square", "pythagoras" and "isqrt"
\ are functions. What would the code potentially look like?
\ The Forth compiler, an interactive and lightweight compiler,
\ must generate code, there are several ways it could make it.
\
\ One way would be something like this:
\
\
\	+:
\		Assembly instructions...
\		exit
\	*:
\		Assembly instructions...
\		exit
\	DUP:
\		Assembly instructions...
\		exit
\	SWAP:
\		Assembly instructions...
\		exit
\ 	SQUARE:
\		call dup
\		call *
\		exit
\	PYTHAGORAS:
\		call square
\		call swap
\		call square
\		call +
\		call isqrt
\		exit
\
\ Where "call", "exit", and the assembly instructions are
\ native instructions in the machine. This is known as
\ subroutine threaded code. However, our SUBLEQ machine does
\ not have those instructions built in. If we want to perform
\ a call or return then we must implement that in our VM.
\
\	+:
\		Assembly instructions...
\		Jump to VM
\	*:
\		Assembly instructions...
\		Jump to VM
\	DUP:
\		Assembly instructions...
\		Jump to VM
\	SWAP:
\		Assembly instructions...
\		Jump to VM
\	EXIT:
\		Assembly instructions...
\ 		Jump to VM
\	\ ----- END OF VM INSTRUCTIONS -----
\ 	SQUARE:
\		Address of dup
\		Address of *
\		Address of exit
\	PYTHAGORAS:
\		Address of square
\		Address of swap
\		Address of square
\		Address of +
\		Address of isqrt
\		Address of exit
\
\ In this version, our compiled words consist of addresses of
\ functions and VM instructions, instead of raw calls to
\ functions. The virtual machine defined later on at the
\ label "vm" is jumped to at the end of virtual machine
\ instructions.
\
\ Let us say that the entry point of the program is the
\ "PYTHAGORAS" function, and that the numbers "3" and "4" are
\ on the data stack.
\
\ The virtual machine register "ip" will point to the first
\ address in "PYTHAGORAS", which will contain "address of
\ square". The VM copies this into "w", the working register,
\ does an indirect load storing the result back into "w",
\ and then increments "ip" so that it points to the next
\ address in "PYTHAGORAS", "address of swap".
\
\ In this interpreter, the VM determines whether to perform
\ a call or a jump by seeing if the address belongs to one
\ of the built in VM instructions by comparing to see if the
\ address if less than the address contained in "primitive",
\ a unique feature of this VM. It is not, as "SQUARE" is not
\ a primitive, it is a defined function. As this is the case,
\ the return stack point "{rp} is incremented, "ip" (which
\ contains the next address to be executed) is pushed onto the
\ return stack with a "iSTORE" (indirect store), and the "w"
\ becomes the new "ip" value. We have just performed a "call"
\ with the VM.
\
\ Our "ip" now points to the first address of "SQUARE", which
\ the address of "DUP", this is not a defined word, it is a
\ VM primitive or instruction. This means we do not call it,
\ we do an indirect jump to it. We still copy "ip" to "w",
\ indirect load through "w", and increment "ip" after. However,
\ instead of doing a simulated call, we do an indirect jump
\ through the contents of "w", a double indirect through "w".
\
\ At the end of the assembler routine that implements "DUP"
\ the last instruction is a unconditional jump back to the
\ VM. As the "ip" is already pointing at the next instruction
\ in "SQUARE", it proceeds along as usual, executing the
\ multiplication in the same fashion, until it comes to the
\ special VM instruction "EXIT", this VM instruction performs
\ a return, popping off a value off the return stack and
\ putting into the "ip" register. The value is the address
\ of the second instruction in "PYTHAGORAS", which contains
\ the address of "SWAP". You can see how this will proceed.
\

assembler.1 +order
label: start         \ System Entry Point
  start 2/ entry t!  \ Set the system entry point
  {sp0} {sp} MOV     \ Setup initial variable stack
  {rp0} {rp} MOV     \ Setup initial return stack
  {cold} ip MOV      \ Get the first instruction to execute
  ( fall-through )
label: vm ( Forth Inner Interpreter )
  ip w MOV           \ Move Instruction Pointer To Working Ptr.
  ip INC             \ IP now points to next instruction!
  t w iLOAD          \ Get actual instruction to execute
  t w MOV            \ Copy it, as SUB is destructive
  primitive t SUB    \ Check if it is a primitive
  t -if w iJMP then  \ Jump straight to VM functions if it is
  ++rp               \ If it wasn't a VM instruction, inc {rp}
  ip {rp} iSTORE     \ and store ip to return stack
  w ip MOV vm a-optim \ "w" becomes our next instruction
  vm JMP             \ Ad infinitum...
assembler.1 -order

\ This meta-compiler word, ";a", used to end the definition of
\ assembly word, requires that the VM has been implemented
\ first, as it jumps back to it.
\

:m ;a (a); vm a-optim vm JMP ;m

\ # Forth Virtual Machine Instructions
\
\ Our Forth Virtual Machine would be useless without any
\ instructions to execute, those instructions are defined here.
\
\ The choice of what functionality to implement as virtual
\ machine primitives and what to implement in the higher
\ level Forth is partially a matter of style, and partially
\ a matter of necessity.
\
\ The SUBLEQ machine is slow, no matter how we optimize it,
\ however it can become usable or unusable depending on what
\ we implement where. It is also more expensive in terms of
\ memory to implement an instruction here than it is in a
\ higher level Forth, a single jump here, or an increment
\ takes up three cells. In higher level Forth it is just
\ one cell. So there is a trade-off, we cannot put everything
\ in assembler to make things faster, as the resulting image
\ would be too big. It is also harder to code in assembly, than
\ Forth. The eForth model just has about thirty primitives,
\ which we will aim to do, other Forth implementations have
\ hundreds, some pedagogical ones have fewer.
\
\ If this was on a real machine, we would want about thirty
\ to implement a reasonably efficient machine, we would want
\ assembly routines for things like multiplication, division,
\ basic stack manipulation, and the like.
\
\ If we implement a multiplication routine in Forth it will
\ have to go through the virtual machine, if we do it in
\ assembly it will not, it will still be relatively slow as
\ we are implementing it in terms of addition, however it
\ be much fast than the Forth version. The number of
\ instructions in the VM is sort of hidden from the view
\ of the programmer, for example the "MOV" macro word is
\ comprised of four SUBLEQ instructions, an indirect load
\ more, each VM cycle takes many of those macros, you end
\ up spending more and more time in the VM than you should have
\ to.
\
\ Other routines like "pause", "exit", or "opEmit", have to
\ implemented as VM instructions as doing so otherwise would
\ be too tricky or impossible.
\
\ We have 42 primitives, more than is ideal, but it is
\ necessary given the nature of this system. Many of them
\ have to be implemented in terms of primitives otherwise the
\ system would be far too slow. For example "lsb" is a
\ primitive that gets the Least Significant Bit, an expensive
\ operation that would normally be implemented by "1 and",
\ but for performance reasons it is not. The same goes for
\ the multiplication and division routines, as well as "1-"
\ and "1+".
\
\ It is partly a matter of philosophy and part a matter of
\ engineering concerns as to what goes where. It is best to
\ keep it as minimal as possible, as if this Forth were to
\ be ported to a new platform, all of these routines would
\ have to be rewritten. One of the reasons eForth was so
\ portable is because there were very few primitive words in
\ it, most of eForth was written in eForth, a higher level and
\ more portable language than assembly.
\
\ ## The Assembly Primitives in Detail
\
\ Each of the virtual machine instructions written in assembly,
\ or primitives, will be described, some are trivial, others
\ less so. They will mostly be acting upon the data or variable
\ stacks and the "tos" register, which is an optimization. That
\ variable contains the top of the variable stack. It means
\ fewer loads and stores have to be done to access the values
\ on the variable stack.
\
\ "bye", "1-", "1+" and "invert" are nothing special, they are
\ backed by simple assembly instructions. "bye" halts the
\ SUBLEQ machine, "1-" decrements the top of the stack, you
\ can work out the other two.
\
:a bye HALT ;a
:a 1- tos DEC ;a
:a 1+ tos INC ;a
:a invert tos INV ;a

\ The following two functions are use to build "@" and "!",
\ however they deal with cell addresses and not with byte
\ addresses. The SUBLEQ machine labels the first 16-bits as
\ cell 0, and the next 16-bits as cell 1, however "@" and
\ "!" require byte addressing, that is, address 0 should refer
\ to the first 8-bits and not the first 16-bit, address 1
\ to the next 8-bits, and so on.
\
\ That will be corrected later, but these two form the core
\ of those words. It does mean that the Forth implementation
\ can address less memory than is available is potentially
\ available to the SUBLEQ machine. The SUBLEQ machine can
\ address 65536 cells, or 128kiB, however the Forth can address
\ 65536 bytes, or 64kiB.
\
:a [@] tos tos iLOAD ;a
:a [!] w {sp} iLOAD w tos iSTORE --sp tos {sp} iLOAD --sp ;a

\ "opEmit" and "opKey" perform the I/O functions, of which
\ there are only two, there are Forth functions which wrap
\ this functions but the basic I/O is done by these two. They
\ both operate on single bytes pulled or pushed to the stack.
\
\ "opEmit", called by "emit", outputs a single byte. It is
\ always blocking. "opKey" accepts a single byte of input
\ and returns negative on error. Depending on the
\ implementation the negative can mean "no byte has been
\ received yet" (it is non-blocking) or it can mean "End Of
\ Input". There are option bits in the "{options}" variable
\ for controlling the behavior of "key", defined later on,
\ which uses this instruction.
\
\ These two instructions provide the only real interaction
\ with the outside world, and you can do a lot with just that.
\
:a opEmit tos PUT tos {sp} iLOAD --sp ;a
:a opKey ++sp tos {sp} iSTORE tos GET ;a

\ We need a way of pushing a literal value, a number, onto
\ the variable stack, "opPush" does that.
\
\ In order to push a literal value onto the variable stack
\ the instruction "opPush" has to manipulate the instruction
\ pointer. Remember "ip" points to the next cell, if we store
\ the number in the next cell when compiling the number into
\ a word definition, then once we have loaded it via "ip", we
\ just need to increment "ip" in order put it back onto a
\ valid instruction.
\
\ As an example, the following word "x", pushes "2", then "3"
\ onto the variable stack:
\
\	: x 2 3 ;
\
\ This gets compiled to:
\
\	PUSH:
\		opPush
\		2
\		opPush
\		3
\		exit
\
:a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a

\ The "opUp" function is similar to the "opPush" function,
\ however it is used to access USER variables instead, that
\ is variables stored in thread local store, or to think of
\ them another way, variables stored relative to a tasks
\ memory area. They are part of the cooperative multitasking
\ system. This instruction should be understood in conjunction
\ with "tuser", "pause" (defined later on in this section) and
\ the multitasking words defined towards the end of this
\ document.
\
\ Where this function differs to "opPush" is that instead of
\ pushing a literal, it loads a literal, adds it to the current
\ task address stored in "{up}", and pushes that onto the
\ stack. The address should be cell and not byte address, so
\
\	opUp 0
\
\ Would refer to the first thread local cell and push the
\ address of it onto the stack.
\
\ "tuser" is used to reserve those thread local variables by
\ the meta-compiler, which has already been described.
\
:a opUp ++sp tos {sp} iSTORE tos ip iLOAD ip INC
   {up} tos ADD {up} tos ADD ;a

\ The following instructions are nothing special, just simple
\ stack manipulation functions, they implement the following
\ Forth functions, as is to be expected:
\
\	opSwap  -> swap ( x y -- y x )
\	opDup   -> dup  ( x -- x x )
\	opOver  -> over ( x y -- x y x )
\	opDrop  -> drop ( x -- )
\	opToR   -> >r   ( x --, R: -- x )
\	opFromR -> r>   ( -- x, R: x -- )
\
\ There is nothing much to say about them, but see if you can
\ understand how they work. The stack effect comments describe
\ them in their entirety.
\
:a opSwap tos w MOV tos {sp} iLOAD w {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a
:a opOver w {sp} iLOAD ++sp tos {sp} iSTORE w tos MOV ;a
:a opDrop tos {sp} iLOAD --sp ;a
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a

\ "opExit", which is used to make "exit", deserves some
\ explanation, it implements a "return", as part of the
\ call/returns you find in most instruction sets. It restores
\ execution to the cell after the call was made.
\

:a opExit ip {rp} iLOAD --rp ;a

\ Subtraction and addition need no real explanation, just note
\ that they are quite fast to execute.
\
:a - w {sp} iLOAD tos w SUB w tos MOV --sp ;a
:a + w {sp} iLOAD w tos ADD --sp ;a

\ These are various stack manipulation words that warrant a
\ little more explanation than the implementations of
\ instructions like "swap", mainly due to what they are used
\ for.
\
\ "r@" is especially useful, it is used often in "for...next"
\ loops to fetch the loop counter (stored on the return stack),
\ "r@" fetches the top most return stack value, pushing it to
\ the return stack. It is a copy, so it does not modify the
\ return stack.
\
\ "rdrop" just drops the topmost return stack variable.
\
\ It should be noted that the instructions that modify the
\ return stack should be that, instructions, and not function
\ calls, as if you called then the call location would be
\ in the way of their operations.
\
\ "rp@" and "rp!" can get and set the return stack pointer
\ directly, they are most useful for the throw/catch mechanism
\ described later on.
\

:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a
:a rdrop --rp ;a
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a
:a rp! tos {rp} MOV tos {sp} iLOAD --sp ;a

\ "sp@" and "sp!" do for the variable stack what "rp@" and
\ "rp!" do for the return stack, they can set them to arbitrary
\ locations, a minor complication is that "sp@" must push
\ the location of the stack before it is called, but that
\ is not much of a problem.
\
\ "sp@" is more useful of the two, they are both used in
\ throw/catch, but "sp@" is also used for words like "pick"
\ and for checking the depth of the variable stack, useful
\ for error checking and debugging purposes.
\

:a sp@ ++sp tos {sp} iSTORE {sp} tos MOV tos INC ;a
:a sp! tos {sp} MOV ;a

\ "opNext" is the only other control structure instruction
\ that is needed, apart from "opJump" and "opJumpZ". It should
\ be noted that the exception mechanism of throw/catch does
\ not need to be implemented as an instruction, but for the
\ sake of efficiency, "opNext" is implemented as an
\ instruction. It is used as part of the primary definite
\ looping mechanism available in eForth, which is the
\ "for...next" construct, it is odd construct, but easy to
\ implement and relatively fast. Much like "opJump" a
\ jump destination follows the "opNext" instruction.
\
\ The "for...next" loop if given N will run for N+1 times,
\ counting backwards from N until 0 is reached on the final
\ loop.
\
\ "for" does very little, "next" does all of the work, it
\ jumps back to after the "for".
\
\ As an example:
\
\	: example for r@ . next ;
\
\ Would compile to something like this:
\
\	0: opToR
\	1: r@ instruction
\	2: address of .
\	3: opNext 1
\
\ And if run with 2, like "2 example", would produce the output
\ "2 1 0".
\

:a opNext w {rp} iLOAD
   w if w DEC w {rp} iSTORE t ip iLOAD t ip MOV vm JMP then
   ip INC --rp ;a

\ "lsb" is similar to the expression "1 and", but with an
\ added "0= 0=", that is it gets the Least Significant Bit and
\ turns it into a Forth boolean (0 or false, -1 for true).
\
\ It is used in functions later on that need to be fast and do
\ the same operation, for example the "c@", or "c!", which both
\ need to test the lowest bit.
\
\ "lsb" is not usually a Forth word because "1 and" is usually
\ fast.
\
\ The instruction works by repeatedly doubling the input number
\ until the lowest bit has been shifted into the highest bit
\ location and then testing whether it is set or not.
\
:a lsb
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD
    tos w MOV 0 tos MOV w if neg1 tos MOV then ;a

\ "opJump" and "opJumpZ" implement unconditional and
\ conditional jumps respectively. The actual jump is performed
\ by "ip ip iLOAD", as "ip" has been increment before the
\ instruction has been called it points to the next cell,
\ which is used to store the jump location, much like with
\ "opPush". These instructions are used to implement the
\ control structures "if", "begin", "again", "until", "repeat",
\ along with a few other conditional statements and patching.
\
\ "opPushZ" does the same as "opJump" but it does it
\ conditionally, it pulls a value off of the variable stack
\ and performs the jump if it zero.
\
\ As an example,
\
\	: example if 2 2 + . then 3 3 + . ;
\
\ Will be compiled to something that looks like this:
\
\	0: opJumpZ 5
\	1: opPush 2
\	2: opPush 2
\	3: address of +
\	4: address of .
\	5: opPush 3
\	6: opPush 3
\	7: address of +
\	8: address of .
\
\ Where the address along the side are cell addresses.
\
\ The way code is generated in Forth is slightly unique and
\ interesting, and will be covered in the control statement
\ chapter.
\

:a opJump ip ip iLOAD ;a
:a opJumpZ
  tos w MOV 0 t MOV
  w if neg1 t MOV then w DEC w +if neg1 t MOV then
  tos {sp} iLOAD --sp
  t if ip INC vm JMP then w ip iLOAD w ip MOV ;a

\ The comparison operators are quite tricky to get right,
\ and are still not quite right, however they do a good
\ enough job. The built in operators are all for signed
\ comparison. Unsigned is synthesized in Forth code later
\ on. They use some of the "if" statement constructs made
\ in the assembler section to shunt the right constant in
\ if the statement is true or false. Forth booleans are
\ different from booleans in language like C, in C true is
\ non-zero but comparison operators yield 1 on true, on false
\ zero is produced. In Forth zero is produced on false, whilst
\ for true -1 is produced, this might seem odd, but -1 is
\ "all bits set", and allows the boolean to be used with
\ the bitwise logical operators as a mask.
\
\ "op0>" is used to make the Forth word "0>", and ">" the
\ Forth word ">", and so on. They could also be defined in
\ terms of each other, however for efficiencies sake they are
\ not.
\
\ An example of this is the "mux" word, this can be used to
\ select the bits from either "x1" or "x2" depending on a
\ mask. It could be used like "2dup > mux" to perform the
\ functionality of max, or "2dup < mux" to perform the
\ functionality of min, both of which are contingent on the
\ comparison operators returning a Forth, and not a C, boolean.
\
\	: mux ( x1 x2 mask -- x )
\		dup >r and swap r> invert and or ;
\
\ "min" and "max" will not be defined like this later, as
\ bitwise operators are expensive on the SUBLEQ machine.
\
\ NB. There are some bugs with the comparison operators "op<"
\ and "op>" when they deal with extreme values like
\ "\$8000 1 <", "\$8002 1 <" works fine.
:a op0> tos w MOV 0 tos MOV w +if neg1 tos MOV then ;a
:a op0=
   tos w MOV neg1 tos MOV
   w if 0 tos MOV then w DEC w +if 0 tos MOV then ;a
:a op0<
   tos w MOV 0 tos MOV
   w -if neg1 tos MOV then w INC w -if neg1 tos MOV then ;a
:a op< w {sp} iLOAD --sp tos w SUB 0 tos MOV
   w -if neg1 tos MOV then ;a
:a op> w {sp} iLOAD --sp tos w SUB 0 tos MOV
   w +if neg1 tos MOV then ;a

\ "op2/" is used to implement "2/", it is common for Forth
\ implementations to implement "2/" incorrectly and this one
\ is no exception, it is however done deliberately.
\
\ It is meant to be a fast division by two, which is not the
\ same as a right shift by one. It can also differ subtly from
\ an arithmetic right shift by one, which it is also sometimes
\ implemented as. Either way, we will need a fast logical
\ right shift, and left shift, by one, to convert from
\ different types of addresses understood by the Forth
\ interpreter and the underlying SUBLEQ machine.
\
\ "op2/" is more complex than "op2*", as "op2*" just adds the
\ "tos" register to itself, a doubling is equivalent to a
\ left shift by one. One some platforms that might not be
\ the case as the carry flag might be affected, not on this
\ one however.
\
\ As is common for all bitwise operations on the SUBLEQ
\ machine, it is expensive to compute. If the SUBLEQ machine
\ could have any extra instructions a bitwise AND and left
\ and right shifts would be it. You could gain back quite a lot
\ in terms of efficiency just from those three extra additions.
\
\ "op2/" works by looping for each bit in the 16-bit machine
\ less on, and it tests whether the topmost bit is set (a
\ relatively cheap operation on twos compliment SUBLEQ
\ machines, as the top bit is set when the value is negative).
\
\ If the topmost bit is set, then one is added to an
\ accumulator register ("x" in this case), "x" will be our
\ result, and it is built up bit by bit in reverse, on the
\ next loop "x" is doubled, which will shift it left.
\
\ We can illustrate this, however we will only do it with
\ four bits to save space, let us imagine we want to shift
\ the binary string "1010" right by one, for our four bit
\ machine we will only need to loop three times. After each
\ iteration the "x" will be as shown, given the "tos":
\
\	tos      x
\	         0000 ( x starts as 0 )
\	1010     0001
\	0100     0010
\	1000     0101
\
\ In each cycle, "x" is first doubled, but as it starts as
\ zero, this is has no effect, if the top most bit of "tos"
\ is non-zero then 1 is added to "x", then "tos" is doubled.
\
\ The algorithms for "AND", "OR", and "XOR" are similar.
\ As well as for left and right shifts by many places.
\

:a op2* tos tos ADD ;a
:a op2/
  bwidth w MOV
  x ZERO
  begin w DEC w while
    x x ADD
    tos bt MOV 0 bl1 MOV
    bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
    bl1 if x INC then
    tos tos ADD
  repeat
  x tos MOV ;a

\ "rshift" is implemented as a virtual machine instruction,
\ but "lshift" is not as it can be implemented in Forth
\ with little performance loss compared to "rshift", division
\ by even a power of two is slow on a SUBLEQ machine.
\
\ It works bit by bit, much like "op2/", but it shifts right
\ by a variable instead of fixed number of bits, it still
\ builds up the result by adding in one, and doubling the
\ "x" and "tos" registers.
\
\ It needs to pull the number to shift by off the stack but
\ there is no real complication.
\
\ A consequence of how this "rshift" works is that it is faster
\ the more bits it shifts by, it takes the longest to shift by
\ a single bit as the result is produced in reverse order.
\

:a rshift
  bwidth w MOV
  tos w SUB
  tos {sp} iLOAD --sp
  x ZERO
  begin w while
    x x ADD
    tos bt MOV 0 bl1 MOV
    bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
    bl1 if x INC then
    tos tos ADD
    w DEC
  repeat
  x tos MOV ;a

\ The logical operators, OR, XOR, and AND, have the same
\ pattern to how they work, and the borrow from how "op2/" and
\ "rshift" work. They both work by testing if the highest bit
\ is set and doubling both inputs and the output in order to
\ shift bits.
\
\ If we have the following table of bits, if we add those
\ bits together we can then use the comparison operators to
\ determine what the new bit should be in the output.
\
\	0 + 0 = 0
\       0 + 1 = 1
\       1 + 0 = 1
\       1 + 1 = 2
\
\ None of the operators output should be 1 when the result is
\ zero, XOR should 1 when the result is equal to zero, OR when
\ it is greater or equal to 1, and AND should only be 1 when
\ the output is 2. Otherwise the output should be 0 for the new
\ bit.
\
\ The following Stack Overflow question goes over this in more
\ detail <https://stackoverflow.com/questions/34120161>.
\
\ Making these operators fast is especially important, in all
\ other Forth systems they have the reasonable expectation that
\ these operators are fast, that is that they operate in a
\ single clock cycle. The bitwise operators usually are, but
\ for SUBLEQ the opposite is the case. Subtraction and addition
\ are, so some words later on have be rewritten to use
\ arithmetic instead.
\
:a opOr
  bwidth w MOV
  x ZERO
  t {sp} iLOAD
  --sp
  begin w while
   x x ADD
   tos bt MOV 0 bl1 MOV
   bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
   t   bt MOV 0 bl2 MOV
   bt -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD bl2 if x INC then
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a
:a opXor
  bwidth w MOV
  x ZERO
  t {sp} iLOAD
  --sp
  begin w while
   x x ADD
   tos bt MOV 0 bl1 MOV bt
   -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
   t   bt MOV 0 bl2 MOV bt
   -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD bl2 INC one bl1 MOV
   bl2 if 0 bl1 MOV then bl1 x ADD
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a
:a opAnd
  bwidth w MOV
  x ZERO
  t {sp} iLOAD
  --sp
  begin w while
   x x ADD
 tos bt MOV 0 bl1 MOV bt
   -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
   t   bt MOV 0 bl2 MOV bt
   -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD two bl2 ADD one bl1 MOV
   bl2 if 0 bl1 MOV then bl1 x ADD
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a

\ "opDivMod" is purely here for efficiency reasons, it really
\ improves the speed at which numbers can be printed, which
\ would be very slows if "um/mod" was used. Printing numbers
\ greatly increases the compilation speed as a large list of
\ them has to be printed out at the end of the image
\ generation. This is not a concern when "gforth" is used to
\ compile the image, but is when the eForth interpreter running
\ under SUBLEQ is.
\
\ "opDivMod" is used by "(.)" later on. It computes both
\ the quotient and the remainder of one number divided by
\ the other, and pushes the results back to the stack. It
\ does no error checking, so a division by 0 is not checked
\ for. It is not designed to be used by the user, but only by
\ the implementation.
\
\ The algorithm for division is a slow one, repeatedly
\ subtracting until the variable used to store the number
\ to be divided goes negative. It assumes its inputs are
\ positive and will not work for negative numbers, much like
\ restoring division. It then fixes up the result after the
\ loop has finished and pushes the result.
\

:a opDivMod
  w {sp} iLOAD
  t ZERO
  begin
    one x MOV
    w -if 0 x MOV then
    x
  while
    t INC
    tos w SUB
  repeat
  tos w ADD
  t DEC
  t tos MOV
  w {sp} iSTORE ;a

\ "pause" is quite simple, but explaining its usage is more
\ complex, it is a word that is implemented in assembly
\ because it needs to be. It is at the core of the cooperative
\ multitasking system that this implementation of Forth has.
\
\ Despite the underlying SUBLEQ machine being quite spartan,
\ it is possible to implement or approximate many things. We
\ will see that later on with the delay loop "ms", and with
\ fake Forth Block system. This however, implements a fairly
\ usable multitasking system of a type which is more common
\ in embedded control systems than on desktop computers. As
\ it is not preemptive we do not need a way of doing
\ interrupts. The multitasking system has interactions with
\ the Input/Output layer which is blocking in the default C
\ implementation of the SUBLEQ machine, and it also interacts
\ with the USER variables.
\
\ Each Forth task, of which there is guaranteed to be at least
\ one, has its own area in which is can store thread local
\ variables. The system setups the first thread on boot, and
\ more can be added later. Each task consists of a 1024 byte
\ thread local storage area, which contain the buffers used
\ for the terminal input, the return and variable stacks,
\ the numeric formatting buffer, and an area for the USER
\ variables and some registers.
\
\ The job of "pause" is to switch from one task to another,
\ if another task exists. Task switching can be disabled by
\ setting the variable "{single}" to non-zero as well.
\
\ A good description of this concurrency model is here:
\
\ <https://www.bradrodriguez.com/papers/mtasking.html>
\
\ Which formed the inspiration for this implementation of the
\ multitasking word-set.
\
\ The tasks exist as a linked list, and there is no priority,
\ it is the job of each task within the scheduler list to yield
\ to the next one. If it does not, it can lock up the system,
\ or too much time could be spent in one task. This is why
\ cooperative multithreading is not used in modern operating
\ systems (but might be used within programs running on those
\ systems, or in embedded real-time systems).
\
\ The I/O functions defined later on all call "pause", this
\ is so that they can yield to the next task as soon as
\ possible, but also in case the non-blocking versions of the
\ I/O functions are implemented in the SUBLEQ machine. This
\ allows the I/O functions to wait for the reception of a
\ character without holding up the rest of the system.
\
\ It is also caused within the "ms" function (which causes
\ problems in its current implementation), and in the Forth
\ Block words (which usually would perform I/O, however the
\ block system is virtual and not backed by massed storage).
\
\ With this VM instruction, USER variables, and a few other
\ variables, it is possible to make a viable, if primitive,
\ threading model. Making this preemptive would require
\ hardware support and greatly complicate the system.
\

:a pause
  {single} if vm JMP then
  w {up} iLOAD \ load next task pointer from user storage
  w if
    {cycles} INC        \ increment "pause" count
    {up} t MOV  t INC   \ load TASK pointer, skip next task loc
      ip t iSTORE t INC \ save registers to current task
     tos t iSTORE t INC
    {rp} t iSTORE t INC
    {sp} t iSTORE t INC
        w {rp0} MOV stacksz {rp0} ADD \ change {rp0} to new loc
    {rp0} {sp0} MOV stacksz {sp0} ADD \ same but for {sp0}
     w {up} MOV w INC  \ set next task
      ip w iLOAD w INC \ reverse of save registers
     tos w iLOAD w INC
    {rp} w iLOAD w INC
    {sp} w iLOAD w INC \ we're all golden
  then ;a

\ And to finish off this section, and the Forth Virtual
\ Machine, is this line, setting the "primitive" value to
\ the current address in the image, allowing the VM to
\ determine which addresses belong to Forth words that can
\ be called, and which ones belong to the Virtual Machine
\ instructions that we have just defined. Anything lower than
\ the current address must be a VM instruction, anything higher
\ a Forth word.

there 2/ primitive t!

\ # More Meta-Compiler words
\
\ Now we have defined our base system we will define some more
\ meta-compiler words that will use that base to perform
\ actions like creating new words in the target dictionary,
\ for compiling "if...else...then" and numbers into Forth
\ words, and such.
\
\ The first set of words we will define are for creating new
\ words in the target dictionary, but for multiple different
\ vocabularies, for the default Forth vocabulary (which
\ contains the standard Forth words), for the root vocabulary
\ (also contains standard Forth words, but the minimal set of
\ words to get back to a "normal" system), for the System
\ vocabulary (containing non-standard words and words used
\ for building the internals), and for the block editor,
\ described in a chapter on it.
\
\ Here are the words and their usage:
\
\ ":t"/";t" - Define a word in the target dictionary
\ ":to"     - Define a word in the target dictionary, but
\             do not make it available during meta-compilation,
\             the word is instead put into the "target.only.1"
\             dictionary of the meta-compiler.
\ ":s"/";s" - Define a word in the system dictionary
\ ":so"     - The same as ":to", except there is no
\             corresponding "system.only.1" dictionary and it
\             works with the system dictionary.
\ ":r"/";r" - Define a word in the root dictionary.
\ ":e"/";e" - Define a word in the editor dictionary.
\
\ The words that define new words in the target dictionary
\ may only be used with the corresponding target dictionary
\ terminating word, that is ":t" and ":to" can only be used
\ with ";t" and not with ";s". This is done with a Forth
\ concept called "compiler" security, which are primitive but
\ largely effective run time checks to make sure control
\ structures match up, or that there are enough items on the
\ stack before executing an operation.
\
\ An example of this is ":s", it pushes the hexadecimal
\ constant "F00D" onto the stack, which ";s" checks for, if
\ it is not present, it prints a message and calls abort.
\
\ ":t" does the same thing with a different constant, so a
\ mismatch can be detected. This also detects situation where
\ too many items on the stack have been consumed by words
\ within meta-compiler definitions.
\
\ An example of this is "lit", which pulls a value off of the
\ stack and compiles into a word definition. We can use this
\ in an example:
\
\	:t example 2 lit 2 lit + . cr ;t
\
\ Which defines a word in the target which will print "4",
\ if instead we defined:
\
\	:t example 2 2 lit + . cr ;t
\
\ Or:
\
\	:t example lit 2 lit + . cr ;t
\
\ The corresponding correct constant would be given to ";t"
\ and an error would be detected. "lit" and how it works will
\ be defined shortly, as well as its shortcomings and possible
\ workarounds.
\
\ The newly defined defining words all call ":t" (defined much
\ earlier) to do their job. (or ":to" if they do not want to
\ put the word name into the meta-compilers dictionary).
\ Likewise the words to terminated a word definition all call
\ ";t". The defining words need to modify the meta-compilers
\ view of the target dictionaries so the word is put into the
\ right location.
\
\ ":t" creates the word header.
\
\ ";t" compiles "opExit" at the end of a word definition.
\

:m ;t BABE <> if abort" unstructured" then
  talign opExit target.only.1 -order ;m
:m :s tlast @ {system} t@ tlast ! F00D :t drop 0 ;m
:m :so  tlast @ {system} t@ tlast ! F00D :to drop 0 ;m
:m ;s drop BABE ;t F00D <> if abort" unstructured" then
  tlast @ {system} t! tlast ! ;m
:m :r tlast @ {root-voc} t@ tlast ! BEEF :t drop 0 ;m
:m ;r drop BABE ;t BEEF <> if abort" unstructured" then
  tlast @ {root-voc} t! tlast ! ;m
:m :e tlast @ {editor} t@ tlast ! DEAD :t drop 0 ;m
:m ;e drop BABE ;t DEAD <> if abort" unstructured" then
  tlast @ {editor} t! tlast ! ;m


\ As ":t" and ";t" are the default actions we want when we
\ are defining new words in the target dictionary, we can make
\ it so that the normal Forth words for defining new functions
\ are what we use instead of ":t" and ";t". Note that ";" is
\ not defined as immediate, this is because we are not doing
\ normal Forth compilation, but are just running exclusively
\ in command mode, except when defining new meta-compilation
\ words with ":m" and ";m". That is ":t" does not change the
\ state variable. We want to make the Forth code we are
\ compiling for the target look like normal Forth code, but
\ the process is not quite the same.
\

:m : :t ;m ( -- ???, "name" : start cross-compilation )
:m ; ;t ;m ( ??? -- : end cross-compilation of a target word )


\ "lit" is used to compile a literal into the dictionary, which
\ will be pushed when run. Note, we are not in command mode
\ when in between ":t" and ";t", and even if we were, it would
\ not do the correct action, the Forth we are running this
\ meta-compiler under would attempt to compile a number given
\ to it into the dictionary, but into its dictionary, and in
\ a way that would not work in the target. We cannot modify
\ the internals of the Forth used to compile this program,
\ well, not the gforth interpreter and not in a portable way,
\ so we are left with having to call "lit" after each number
\ we want to compile into a target word.
\

:m lit         opPush t, ;m ( n -- )
:m up          opUp   t, ;m ( n -- )
:m [char] char opPush t, ;m ( --, "name" )
:m char   char opPush t, ;m ( --, "name" )

\ It is interesting to see just how simple and easy it is
\ to define a set of words for creating control structures.
\ Making a compiler is actually quite easy, making a good
\ one however, an industrial grade one (which this incremental
\ compiler certainly is not) is an open ended tasks taking
\ hundreds of thousands of man-hours to complete.
\
\ The meta-compiler defined control structures here are
\ similar to the ones defined in the assembler, and they will
\ again be similar to the ones defined further along that
\ will exist in the target dictionary. It is possible to make
\ your own control structures, perhaps an "if" that only
\ executes its clause when negative numbers are encountered,
\ or one with an inverted test called "unless".
\
\ Note that none of these definitions of control structures
\ are immediate words, because the entirety of the meta
\ compiler runs in command mode. The control structures which
\ will be defined within the target dictionary will require
\ their immediate flag to be set in their word headers.
\
\ TODO: Describe how these work, again

:m begin talign there ;m
:m until talign opJumpZ 2/ t, ;m
:m again talign opJump  2/ t, ;m
:m if opJumpZ there 0 t, ;m
:m mark opJump there 0 t, ;m
:m then there 2/ swap t! ;m
:m else mark swap then ;m
:m while if ;m
:m repeat swap again then ;m
:m aft drop mark begin swap ;m
:m next talign opNext 2/ t, ;m
:m for opToR begin ;m

\ The following words will be useful for defining control
\ structures within the newly made Forth interpreter image,
\ as they will need to compile in jump instructions into newly
\ defined words. Note that they use the cell address.
\

:m =push   [ t' opPush  half ] literal ;m ( -- a )
:m =jump   [ t' opJump  half ] literal ;m ( -- a )
:m =jumpz  [ t' opJumpZ half ] literal ;m ( -- a )
:m =unnest [ t' opExit  half ] literal ;m ( -- a )
:m =>r     [ t' opToR   half ] literal ;m ( -- a )
:m =next   [ t' opNext  half ] literal ;m ( -- a )
:m =up     [ t' opUp    half ] literal ;m ( -- a )

\ To avoid conflicts with meta-compiled words and words
\ compiled in the target dictionary some instructions have
\ been given "op" as a prefix. We could have done this with
\ dictionary manipulation, but this is easier. We no longer
\ need the word "dup" to actually duplicate the top word on
\ a stack, instead the new "dup" should compile a reference
\ to "opDup" in newly defined words.
\

:m dup opDup ;m ( -- : compile opDup into the dictionary )
:m drop opDrop ;m ( -- : compile opDrop into the dictionary )
:m over opOver ;m ( -- : compile opOver into the dictionary )
:m swap opSwap ;m ( -- : compile opSwap into the dictionary )
:m >r opToR ;m ( -- : compile opTorR into the dictionary )
:m r> opFromR ;m ( -- : compile opFromR into the dictionary )
:m 0> op0> ;m ( -- : compile op0> into the dictionary )
:m 0= op0= ;m ( -- : compile op0= into the dictionary )
:m 0< op0< ;m ( -- : compile op0< into the dictionary )
:m < op< ;m ( -- : compile op< into the dictionary )
:m > op> ;m ( -- : compile op> into the dictionary )
:m or opOr ;m ( -- : compile opOr into the dictionary )
:m xor opXor ;m ( -- : compile opXor into the dictionary )
:m and opAnd ;m ( -- : compile opAnd into the dictionary )
:m exit opExit ;m ( -- : compile opExit into the dictionary )
:m 2/ op2/ ;m ( -- : compile op2/ into the dictionary )

\ This complete most of the meta-compiler words, a few new
\ ones will be defined later, but that is most of it. The
\ next section we will bring this all together to start to
\ actually define new Forth words within the target Forth
\ image.
\

\ # The Forth (section of the) Image
\
\ We are now ready to define actual Forth words, everything up
\ until now has just been to make an environment that is
\ capable of hosting a Forth system, and about half the size
\ of the generated image is used to create a those conditions.
\
\ The first words will mostly be generic utility functions,
\ words the embody a virtual machine instruction, and a few
\ other miscellaneous things.
\
\ To both save space, and because using "lit" as a postfix is
\ annoying, for the most common constants; 0, 1, and -1, we
\ will define words for them and place them in the system
\ vocabulary with ":s".
\
\ Compiling a number into a word definition takes up two
\ cells, one for "opPush" and another for the value. A
\ reference to a word only takes up one cell, hence the saving.
\ The trade off is that it takes longer to execute and space
\ must be reserved for the words that push those constants.
\
:s #0 0 lit ;s ( -- 0 : push the number zero onto the stack )
:s #1 1 lit ;s ( -- 1 : push one onto the stack )
:s #-1 -1 lit ;s ( -- -1 : push negative one onto the stack )
\ The next section adds all the words that are implemented in
\ a single virtual machine instruction.
\
\ We want to make sure that we use references to the virtual
\ machine instructions when we call things like "1+" or "xor"
\ when we use them later in the program, and references to
\ these words, so we put those words in the "target.only.1"
\ dictionary to make sure they will not be found.
\
\ The definitions looks odd, for example:
\
\	:to 1+ 1+ ;
\
\ Looks like it would be a recursive function that never
\ terminates, however due to the way ":to" defines words the
\ new word definition is not visible when we are filling out
\ the word body, so the second "1+" refers to the assembly
\ definition we defined with ":a", or the VM instruction for
\ "1+".
\
\ The reason they are placed in the "target.only.1" is because
\ of speed, it is better to call the VM instruction directly
\ than to call a function that will just call the VM
\ instruction.
\
\ There is not much else to say about these words. Some of them
\ go into the system vocabulary, but should also not be
\ referenced moving on.
\

:to 1+ 1+ ; ( n -- n : increment a number by one )
:to 1- 1- ; ( n -- n : decrement a number by one )
:to + + ; ( n n -- n : addition )
:to - - ; ( n1 n2 -- n : subtract n2 from n1 )
:to invert invert ; ( u -- u : bitwise logical negate )
:to bye bye ; ( -- : halt the system )
:to dup dup ; ( n -- n n : duplicate top of variable stack )
:to drop opDrop ; ( n -- : drop top of variable stack )
:to over opOver ; ( x y -- x y x : get over it! )
:to swap opSwap ; ( x y -- y x : swap two variables on stack )
:to rshift rshift ; ( u n -- u : logical right shift by "n" )
:so [@] [@] ;s ( vma -- : fetch -VM Address- )
:so [!] [!] ;s ( u vma -- : store to -VM Address- )
:so lsb lsb ;s ( u -- f : get least significant bit )
:to sp@ sp@ ; ( -- a : get the variable stack location )
:to sp! sp! ; ( a -- ??? : set the variable stack location )
:to 0> op0> ; ( n -- f : signed greater than zero )
:to 0= op0= ; ( n -- f : equal to zero )
:to 0< op0< ; ( n -- f : signed less than zero )
:to < op< ; ( n1 n2 -- f : signed less than )
:to > op> ; ( n1 n2 -- f : signed greater than )
:to 2/ op2/ ; ( u -- u : div by two, equivalent to 1 rshift )
:to or opOr ; ( u u -- u : bitwise or )
:to xor opXor ; ( u u -- u : bitwise xor )
:to and opAnd ; ( u u -- u : bitwise and )
:so pause pause ;s ( -- : pause current task, task switch )

\ "nop" stands for 'no-operation', it is useful for some of
\ the hooks we have. It does nothing.
\
: nop ; ( -- : do nothing! )

\ Notice how "@" and "!" divide the address by two, which drops
\ the very lowest bit that is used to select the upper or lower
\ byte in a word. Division is expensive to compute, so it is
\ better to use "\[@\]" and "\[!\]" where possible.
\
\ However, from the point of view of the developer, using
\ "@" and "!" is easier.
\
: @ 2/ [@] ; ( a -- u : fetch a cell to a memory location )
: ! 2/ [!] ; ( u a -- : write a cell to a memory location )

\ These words are just used to push the address of a variable
\ onto the stack. Some of them are local variables (using "up")
\ and others are global addresses (using "lit").
\
\ Note which variables are USER variables and which are just
\ normal memory locations.
\
\ "<ok>", "<emit>", "<echo>", "<literal>" and "<cold>" are used
\ for system hooks. This allows the words they are used in to
\ change at run time. For example, "<ok>" contains the
\ execution token used to print the "ok" prompt, which we have
\ already encountered. By changing that execution token we
\ can change that prompt. For example:
\
\	: prompt cr ." ok>" ;
\	' prompt <ok> !
\
\ Will print a prompt that says "ok>" after each line is
\ executed.
\
\ "<cold>" is a normal variable and not a USER variable as it
\ needs to be available before the first thread is set up, it
\ contains the execution token of the first Forth word to be
\ executed.
\
\ "<ok> is in the normal Forth vocabulary despite it being a
\ non-standard word, this is because we need to silence the ok
\ prompt at the start of the script to ensure our output is
\ not corrupted.
\
\ It is not a hard rule, but usually hook variables have the
\ "<>" brackets enclosing them, and the functionality is
\ provided by a word with a name like "(name)", the word that
\ executes the hook will be called just "name".
\
\ NB. "{cold}" contains a cell address, not a Forth address!
\
\ That is,
\
\	' (cold) <cold> !
\
\ Will not work, this works for setting most execution vectors,
\ but not this one, "{cold}" needs to be executed by the
\ Forth Virtual Machine, it cannot divide a number by two
\ efficiently, so it only operates on cell address. This is
\ a minor quirk of this implementation and should not be the
\ case on other systems.
\
\ The following should work:
\
\	' (cold) 2/ <cold> !
\
: <ok> {ok} up ; ( -- a )
:s <emit> {emit} up ;s ( -- a )
:s <key>  {key} up ;s ( -- a )
:s <echo> {echo} up ;s ( -- a )
:s <literal> {literal} up ;s ( -- a )
:s <cold> {cold} lit ;s ( -- a )

\ There are quite a lot of variables in this part of the code,
\ much like at the beginning of the image, and they will all
\ be explained again.
\
\ "current" and "root-voc" are to do with word vocabularies.
\ The "root-voc" contains the minimal Forth vocabulary, hence
\ why "root" is part of its name. It contains only several
\ words such as "eforth", "words", "only", "forth", "system",
\ "forth-wordlist" and "set-order".
\
\ "current" contains the vocabulary for which newly defined
\ words are to be added to. It is used to place words in
\ the "target.1", "meta.1", "assembler.1" and "target.only.1"
\ vocabularies. It is not usually used directly however, but
\ is used by words like "definitions".
\

: current {current} lit ; ( -- a : get current vocabulary )
: root-voc {root-voc} lit ; ( -- a : get root vocabulary )

\ The word "this" allows us to access the USER task area,
\ it pushes the pointer to that area onto the stack. The
\ USER task is a 1024 byte block of memory, as mentioned, that
\ has multiple stacks, buffers and variables in it.
\
\ It can be use to get the task ID (which is the same as the
\ tasks address), or for the implementation, which knows that
\ the "pad area" is located 960 bytes into the task area, which
\ is used for the word "pad". The pad location is meant to be
\ a programmers utility, not meant for serious use, that
\ contains a *small* section of memory used as a temporary
\ "pad".
\
: this 0 up ; ( -- a : address of task thread memory )
: pad this 3C0 lit + ; ( -- a : index into pad area )

\ More vocabulary words, "#vocs" contains the maximum number
\ of possible vocabularies in the vocabulary list, whilst
\ "context" gets a pointer to the area used to store the
\ vocabulary array, the first cell will contain the first
\ vocabulary in the word list (or the wordlist that will get
\ searched for first).
\
: #vocs 8 lit ; ( -- u : number of vocabularies )
: context {context} lit ; ( -- a )

\ These words just push variable locations, or their values,
\ some of them will need an explanation, like "dpl", but that
\ is best done in the appropriate section (for "dpl" it is in
\ the section dealing with numeric input and output).
\
\ You should already be familiar with "here" and the "h"
\ variable, the "h" variable contains the dictionary pointer,
\ used to keep track of where newly compiled values will get
\ compiled to in memory.
\
\ "blk" and "scr" are covered in the section on block storage,
\ "state" is best dealt with when talking about the interpreter
\ loop.
\
\ "hld" also belongs with numeric I/O with "dpl", ">in" with
\ the parsing words.
\
\ "base" controls the input and output radix, if you want
\ hexadecimal output or input, or binary output, or octal,
\ or decimal, this is the variable you need to change. Be aware
\ of which base you are in whilst setting the new base.
\
\ Valid bases range from 2 to 36, if a number higher than 10
\ is used as a base the uppercase alphabet from A-Z will be
\ used along with the numbers from 0-9.
\
\ There is no way to set the input base without setting the
\ output base, however it is possible to change the base
\ temporarily. "." is used to print signed numbers, if you
\ wanted to make a word that always printed out signed numbers
\ in hexadecimal, regardless of what base you are in, you could
\ use the following word definition:
\
\	decimal
\	: .hex base @ >r 16 base ! . r> base ! ;
\
\ It is common to see the expressions "base @ \>r" and
\ "r\> base !" in Forth words definitions that deal with
\ number printing.
\

: here h lit @ ; ( -- u : push the dictionary pointer )
: base {base} up ; ( -- a : push the radix for numeric I/O )
: dpl {dpl} up ; ( -- a : decimal point variable )
: hld {hld} up ; ( -- a : index to hold space for num. I/O)
: state {state} up ; ( -- f : interpreter state )
:s calibration {ms} lit ;s ( -- a : "ms" calibration var )
: blk {blk} lit ; ( -- a : latest loaded block )
: scr {scr} lit ; ( -- a : last view block )
: >in {in} up ; ( -- a : input buffer position var )
: bl 20 lit ; ( -- 32 : push space character )
: cycles {cycles} lit ; ( -- a : number of "cycles" ran for )

\ To make switching bases easier the words "hex" and "decimal"
\ are made available, which set the numeric input and output
\ radix (also known as a base) to sixteen and ten respectively.
\
\ One problem with setting the base is knowing what base you
\ are in at the time, if you want to print out a number in
\ hexadecimal you could use the expression:
\
\	16 base !
\
\ However, this will not necessarily work when you are
\ interactively entering commands and you have lost track of
\ what base you are in. The number "16" is sixteen when you
\ are already in base ten, however it is twenty two if you
\ are operating in a hexadecimal base. You could always
\ attempt to view the value in "base", however the value in
\ base will always be "10"! It is much easier just to type
\ "hex" or "decimal" to make sure you are operating in the
\ correct base when you are unsure.
\
\ Also commonly defined:
\
\	decimal
\	: octal 8 base ! ;
\	: binary 2 base ! ;
\
\ You could use higher bases as a data interchange format,
\ allowing binary data to be transferred as text with a
\ processing and storage overhead. Bases 32 and 36 have
\ advantages, as does base 16. The lower the base the less
\ dense the resulting string, which means more overhead.
\
\ Base-64 is the most common way of encoding binary strings
\ as text, but base cannot be set that high as there is no
\ real sensible encoding that follows from going higher than
\ the ten digits plus the alphabet, there would be
\ ambiguities if we did. You could add it as a special case,
\ if you so desired.
\
\ Base-32 has the advantage that it is a power two base, so
\ a CODEC for it can be more efficiently implemented, Base-36
\ has the advantage that is the most dense encoding, but
\ slower to implement a fast CODEC.
\
\ Base-16 is less dense than Base-32 but is easier to process,
\ as two sets of 4 bits fit into a byte, so there are no dead
\ bits as well, but it is much less efficient overall.
\
\

: hex  10 lit base ! ; ( -- : change to hexadecimal base )
: decimal A lit base ! ; ( -- : change to decimal base )

\ "\]" and "\[" are two, very simple words, that require a
\ lot more context to understand properly. Note one of them
\ is immediate as well, that's important!
\
\ The words are often used within other word definitions to
\ get back into command mode temporarily. For example:
\
\	: x 2 2 + . cr ;
\	: y [ 2 2 + ] literal . cr ;
\
\ Both print the same result, however "y" is smaller and
\ executes faster (not that it matters for such a short word).
\
\ That is because "y" computes the value it will print out
\ at compile time, and "x" does it at run time. They will
\ look like this:
\
\
\	X:
\		opPush 2
\		opPush 2
\		address of +
\		address of .
\		address of cr
\		opExit
\
\	Y:
\		opPush 4
\		address of .
\		address of cr
\		opExit
\
\ This is why it is important "\[" is immediate, but "\]" does
\ not need to be. It needs to immediately switch from compile
\ mode to command in the middle of a word definition. It allows
\ one to do arbitrary computation during the compilation of
\ a Forth word.
\
\ The "state" variable is used to control whether the
\ interpreter is in command mode (state is zero) or compile
\ mode (state is non-zero). Command mode immediately executes
\ words, whilst compile mode compiles non-immediate words
\ and numbers into the current word definition, but executes
\ immediate ones.
\
\ This will be shown in more detail in the capture containing
\ the word "interpret", where naturally we go into more
\ detail about how the interpreter internals work.
\

: ] #-1 state ! ; ( -- : return to compile mode )
: [ #0  state ! ; immediate ( -- : initiate command mode )

\ "many" is an interesting word, it is in the system vocabulary
\ despite not being used, but I like the word so I have
\ included it here, it allows a line of code to be executed
\ an infinite number of times by postfixing it to the end of
\ the command. That is less interesting compared to the way it
\ does it, by manipulating the input line to make it so it is
\ executed again, the line is then re-parsed and executed
\ again, including "many", which triggers another re-parsing
\ and so on. It is a neat little word.
\
:s many #0 >in ! ;s ( -- : repeat current line )

\ These words should be familiar to any Forth programmer,
\ they are often defined in assembly for speed reasons, but
\ most of the time they are not the choke-point in an
\ application.
\
\ "?dup" is one of the rare Forth words that leave a differing
\ amount of items on the stack, it is useful for testing loop
\ conditions in "begin...while...repeat" statements, and
\ sometimes before an "if...then" clause. It only duplicates
\ non-zero values.
\
\ "rot" and "-rot" are used, and provided, but should be
\ avoided, if there are three or more items on the stack
\ at anyone time it can get confusing. Consider refactoring
\ if you use them.
\
\ For the other words, "nip", "tuck", "2drop" and "2dup",
\ nothing really needs to be said about them. Their stack
\ effect describes them perfectly.
\

: nip swap drop ; ( x y -- y : remove second item on stack )
: tuck swap over ; ( x y -- y x y : save item for rainy day )
: ?dup dup if dup then ; ( x -- x x | 0 : conditional dup )
: rot >r swap r> swap ; ( x y z -- y z x : "rotate" stack )
: -rot rot rot ; ( x y z -- z x y : "rotate" stack backwards )
: 2drop drop drop ; ( x x -- : drop it like it is hot )
: 2dup  over over ; ( x y -- x y x y )

\ The comparison or test words are defined in relation to
\ to each other, which is no surprise. Given one comparison
\ operator it is easy enough to produce the rest, ideally
\ the underlying system would provide at least one signed
\ and one unsigned method of comparison. This is usually done
\ on most real architectures by performing a subtraction and
\ checking what flags are set in the CPU, such as the borrow
\ flag, negative, overflow, and the zero flag. Different
\ combinations of these flags allow the language implementer
\ to perform signed and unsigned comparisons of equal, not
\ equal, less than, greater than, and less than and equal or
\ greater than or equal, all in one or two instructions.
\
\ As we do not have those facilities we make use of the
\ comparisons operators we made for the Forth Virtual Machine.
\ Using the signed comparison operators to do unsigned
\ comparison is a little more involved, and is doing the
\ opposite, however if we stay within one class it is easy
\ to construct all operators given a signed less than or
\ a signed greater than using just swapping the arguments
\ around, or inverting the boolean result.
\
\ "=" can be defined with "xor" instead of "-", but "-" is
\ cheaper on SUBLEQ machines.
\

: 0<= 0> 0= ; ( n -- f )
: 0<> 0= 0= ; ( n -- f : not equal to zero )
: = - 0= ; ( u1 u2 -- f : equality )
: <> = 0= ; ( u1 u2 -- f : inequality )
: >= < 0= ; ( u1 u2 -- f : greater than or equal to )
: <= > 0= ; ( u1 u2 -- f : less than or equal to )
: 0>= 0< 0= ; ( u1 u2 -- f )

\ The unsigned words are defined in terms of each other once
\ of them has been defined, they are a bit awkward as they
\ depend on the signed comparison words to do the work, which
\ is not normal, but necessary given the constraints of the
\ system. Unsigned comparison is used less than signed, so
\ speed is not too much of a concern, they are largely here
\ for completeness sake.
\

: u< 2dup 0< 0= swap 0< 0= <> >r < r> <> ; ( u1 u2 -- f )
: u> swap u< ; ( u1 u2 -- f )
: u>= u< 0= ; ( u1 u2 -- f )
: u<= u> 0= ; ( u1 u2 -- f )

\ A few miscellaneous arithmetic words need defining:
\
\ "negate", which is simple enough, it negates a value, most
\ Forth standards take after the C standard and do not
\ specify how signed numbers are encoded, they both came from
\ a time when the hardware had not settled down on some basic
\ features we now take for granted and everything was more
\ experimental. However, twos compliment is the norm, and
\ "negate" does a twos compliment negation.
\
\ "s\>d" turns a signed number and turns it into a double cell
\ number, which we will encounter more later on when talking
\ about the more complex arithmetic operators.
\
\ "abs" gets the absolute value of a number, note, like most
\ "abs" functions on twos compliment machines getting the
\ function is only properly defined within the range of
\ Minimum Signed Value + 1 to Maximum Signed Value, if you
\ entered the Minimum Signed Value ($8000 or -32768) you will
\ get back the same number. This is common for many
\ implementations of "abs" and is a consequence of twos
\ compliment arithmetic having one more negative numbers than
\ than positive non-zero numbers.
\
\ "2\*" just doubles a number, it should be familiar by now.
\

: negate 1- invert ; ( n -- n )
: s>d dup 0< ; ( n -- d )
: abs s>d if negate then ; ( n -- u )
: 2* op2* ; ( u -- u )

\ The cell word-set allows portable code to be written that
\ does have to worry about how many bytes are in a cell on
\ a given architecture. There are a few words within this
\ implementation that assume a cell size of 2 for optimization
\ reasons, which is frowned up, such as "aligned", but they
\ are kept to a minimum.
\
\ - "cell" just pushes the size of a single cell in bytes.
\ - "cell+" is used to increment an address to the next cell,
\ without any care for cell alignment.
\ - "cells" is used to convert a number of cells into the
\ number of bytes those cells take up.
\

: cell 2 lit ; ( -- u )
: cell+ cell + ; ( a -- a )
: cells op2* ; ( u -- u )

\ "execute" takes an "execution token", which is just a fancy
\ name for an address of a function, and then executes that
\ function. It is used within the interpreter quite a lot,
\ and is very useful for executing the contents of "hooks",
\ that is, variables that contain an execution token that
\ can be changed so the underlying functionality of a word
\ can be changed. They are also known as execution vectors
\ and changing them is "vectoring".
\
\ The implementation of "execute" is incredibly simple, as it
\ is on many Forth implementations, it converts a Forth address
\ to a cell address then puts the value onto the return stack,
\ when the word "execute" returns by calling exit (compiled
\ into the definition by ";t", it will cause a jump to the
\ execution vector.
\

: execute 2/ >r ; ( xt -- )

\ "key?" is a word that interfaces with the input channel
\ an attempts to get a single character, or byte, of input
\ from it. It *attempts* to do so. Depending on the underlying
\ implementation of the SUBLEQ machine it might block until
\ a character is available (that is, it will wait until the
\ user presses a key) and if there is no more input signal
\ a negative value (all character values are between 0 and
\ 255, so a negative value is treated as an error) known as
\ End Of Input, or on a machine with a non-blocking character
\ input a negative value will mean that it has not received
\ a character just yet and calling the get-character function
\ might succeed later.
\
\ As there is no way of determining which has occurred
\ (perhaps they could return different error codes, but they
\ do not), what the "key?" implementation does is instead
\ configurable. By default it will assume there is a blocking
\ character input, and when a negative value is encountered it
\ will halt the machine by calling "bye".
\
\ If a non-blocking implementation is configured, it will
\ instead return -1 on error. The word "key?" is a rare word
\ that returns a differing number of arguments, words that
\ accept or return a differing number of arguments depending
\ on circumstance are discouraged, but this word seems to be
\ the exception in many implementations.
\
\ "key?" will return the character retrieved and a zero
\ on successful reception of a character in either
\ implementation.
\
\ "key?" does not call "pause" either.
\

: key? opKey s>d ( -- c 0 | -1 : get single byte of input )
   if
     {options} lit @ 8 lit and if bye then drop #0 exit
   then #-1 ;

\ "key" pauses (allows other threads to run) and repeatedly
\ calls "key?" until the operation succeeds. This is a version
\ of "key?" that always blocks until a character is received.
\
\ Note that what is said is not quite correct, it does not
\ call "key?", however the default execution token stored in
\ "\<key\>" is "key?", you can change it to be whatever you
\ want, so long as it has the same stack effects as "key?",
\ allowing you to directly take input from another input
\ source, say a string, or a new peripheral you have added
\ to the SUBLEQ machine.
\

: key begin pause <key> @ execute until ; ( -- c )

\ A side note about randomness, we lack sources of entropy
\ within the virtual machine, we have two possible sources,
\ and a few other natural sources of seeds (but which cannot
\ be classified as sources of entropy given that they are
\ constant and only varying between different versions of the
\ base image) such as the image checksum. The two possible
\ sources of entropy are the timing of key presses and the
\ key presses themselves, as they constitute the only entropy
\ in the system, if the input blocks then only the values of
\ the key presses can be used as an entropy source.
\
\ If we integrated entropy collection into the "key" word,
\ then we could use this to create a higher quality (but
\ still fairly bad, it would still not be suitable as a
\ source for a Cryptographically Secure Random Number 
\ Generator). Adding entropy collection along with implementing
\ a reasonable Pseudo Random Number Generator algorithm such
\ as "XORSHIFT+" would allow a word usually called "random"
\ to be added, that returns a random number from zero the
\ maximum allowed by the machines cell width.
\

\ "emit" is the counter part to "key", it always blocks until
\ it succeeds (and it is assumed that outputting a character
\ is a fast operation that takes little time), it does call
\ "pause" before it outputs a character.
\
\ "emit" does not get the character itself, it is expected
\ that the execution vector stored in "\<emit\>" will do that.
\ It is possible to make the output go to wherever you want,
\ for example you could duplicate what is send to the
\ programmer and write it to a section of memory as a temporary
\ log, or search for strings within the output and process
\ those.
\

: emit pause <emit> @ execute ; ( c -- : output byte )

\ "cr" emits a newline, DOS style newlines are used instead
\ of Unix style newlines, although that can be configured by
\ removing the right constant and emit words.
\
\ * Unix systems use "lf"
\ * DOS and Windows uses "cr" then "lf"
\
\ There are other systems that use other crazy characters,
\ but no one cares about them.
\

: cr =cr lit emit =lf lit emit ; ( -- : emit new line )

\ There is nothing special about these three words, they are
\ common, standard, convince words for fetching and storing
\ variables in the system.
\
\ "get-current"/"set-current" get the dictionary or vocabulary
\ that is currently being used to append new word definitions
\ to, which does not have to be one in the search order. "last"
\ gets the location of the last defined word in the dictionary.
\
\ See also "definitions".
\

: get-current current @ ; ( -- wid )
: set-current current ! ; ( -- wid )
:s last get-current @ ;s ( -- wid )

\ "pick" is a word whose use is discouraged in Forth, if
\ you find yourself using it your code is likely to be
\ too complex and "un-forth-like". It is included because it
\ is useful for implementing the word ".s". It might not
\ be possible to implement this word on all Forth systems
\ efficiently (such as those with hardware stacks that cannot
\ be indexed like an array), however that is not a problem
\ on this system.
\
\ The word "pick" fetches the n-th item on the stack, using
\ zero indexing, so for example:
\
\ 	50 40 30 20 10 0 pick . ( prints 10 )
\ 	50 40 30 20 10 1 pick . ( prints 20 )
\ 	50 40 30 20 10 2 pick . ( prints 30 )
\
\ That is, the first element on the stack is at position 0,
\ the second at position 1, and so on. Not the positions are
\ calculated before you put the index onto the stack and not
\ after.
\
\ For systems that do not have a stack that can be indexed,
\ the following version of "pick" that shifts values between
\ the data and return stack can be defined:
\
\	: pick ?dup if swap >r 1- pick r> swap exit then dup ;
\
\ Be warned though, this version of "pick" is slower but the
\ main concern is its high stack usage, especially as systems
\ with hardware stacks are likely to have small stacks of
\ just a few, perhaps even as low as eight, values. This
\ version of "pick" will quickly eat through that, overflow,
\ and give incorrect results.
\

: pick sp@ + [@] ; ( nu...n0 u -- nu )

\ "+!" is a useful utility word, often found with "1+!" and
\ "1-!", neither of which are defined here as they are not
\ used, "+!" is however. It is used to add a number to a
\ value stored at an address, not replace, it stores the new
\ number back to the address.
\
\ It is a common enough operation that it deserves its own
\ word.
\
\ "1+!" and "1-!" are easy enough to define yourself:
\
\	: 1+!  1 swap +! ;
\	: 1-! -1 swap +! ;
\
\ If you need them. Do not forget to change the ":" and ";"
\ into ":t" and ";t", as well as changing "1" to "#1" and "-1"
\ to "#-1" if you want to add them to this cross compiled
\ program instead of typing them in at the command prompt.
\

: +! 2/ tuck [@] + swap [!] ; ( u a -- )

\ "lshift" is much faster to compute than "rshift" as it
\ "2\*" is faster than "2/". That is why it is done in Forth
\ as opposed to assembly.

: lshift begin ?dup while 1- swap 2* swap repeat ; ( n u -- n )

\ The SUBLEQ machine can only do cell aligned loads and stores,
\ as such we need to it manually, for speed reasons this would
\ be better done in assembly, but because of the complexity
\ these words have been left as pure Forth.
\
\ "c@" fetches a single character from an address, and "c!"
\ stores a single character to an address.
\
\ As mentioned, the SUBLEQ machine can address 65536 cells
\ (less one for the peripheral address), the Forth can only
\ address 65536 bytes as it uses the lowest bit for the byte
\ index, this sacrifice allows our Forth to behave like a
\ normal Forth, and for us to easily be able to manipulate
\ bytes and strings, however it means there are two types of
\ address, cell address used by the SUBLEQ machine, and Forth
\ addresses used by Forth interpreter. We must always be aware
\ which address types we should use under what circumstances.
\ If you see a multiplication or division by 2, it is most
\ likely because the address type needs converting for an
\ operation.
\
\ "c@" is the simpler of the two operations, it performs a load
\ and then just selects the high or low byte.
\
\ "c!" must shift the input byte into the right position,
\ high or low, perform a load of an address, mask off the
\ high or low byte, and then merge in the new byte, and then
\ write the result back to the same address.
\
\ Both operations are more expensive to do than just doing
\ a load with "@" and a store with "!". Even quicker is using
\ "\[@\]" and "\[!\]", which do not have to perform the
\ conversion from a Forth to a cell address, which requires
\ a division by two.
\
\ These words are also partially responsible for the Endianess
\ of the system, the Endianess of how the meta-compiler writes
\ values also needs to be the same as the target.
\

: c@ dup @ swap lsb if 8 lit rshift else FF lit and then ;
: c!  swap FF lit and dup 8 lit lshift or swap ( c a -- )
   tuck dup @ swap lsb 0= FF lit xor
   >r over xor r> and xor swap ! ;

\ "min" and "max" could have been written with "mux", as
\ described, but on this Forth this is the fasted way of
\ implementing them. "min" and "max" operate on *signed*
\ values, given two numbers they leave the minimum or maximum
\ number on the stack, dropping the other one.
\
\ A non-destructive version that sorts the items on the stack
\ by ascending or descending might be useful, but I cannot
\ think of a purpose for them.
\
\ Note, that "min" and "max" operate on *signed* values, not
\ unsigned ones, for unsigned values sometimes "umin" and
\ "umax" are defined, all you have to do is replace the
\ relevant comparison operator with its unsigned version.
\

: max 2dup < if nip else drop then ; ( n1 n2 -- n )
: min 2dup > if nip else drop then ; ( n1 n2 -- n )

\ "source-id" allows us to determine what the current input
\ source is. If it is 0 we are reading input from the terminal,
\ if it is non-zero then we are executing from a string or a
\ block.
\

: source-id {id} up @ ; ( -- u : input type )

\ If we want to set two, often related, cell values at once,
\ we can use "2!" and "2@", "2!" will store the top cell on the
\ stack in the first memory cell given, increment the cell
\ address, and store the second value in the second cell.
\
\ Within this interpreter the words are used for setting and
\ getting the values of the Terminal Input Buffer, which
\ consists of two contiguous cells that deal with the position
\ and maximum size of that buffer.
\
\ They often find use in code for walking more complex data
\ structures.
\

: 2! tuck ! cell+ ! ; ( u1 u2 a -- )
: 2@ dup cell+ @ swap @ ; ( a -- u1 u2 )

\ These two words are like "2!" and "2@", but for shunting
\ two numbers between the return and data stacks. Note that
\ the implementation of "2\>r" *cannot* just be:
\
\	: 2>r >r >r ;
\
\ As the return stack is used to store the position of
\ where a function was called from, if we implement as above
\ we will clobber that position and return not to whence we
\ came but it will do a fandango on the core instead. The
\ same goes for the word "2r\>".
\
\ Note that the flag "compile-only" is set at well, this word
\ will not do anything sensible if run as a command, so we make
\ it so the interpreter will throw an error if it encounters
\ the word in command mode.
\

: 2>r r> swap >r swap >r >r ; compile-only ( n n --,R: -- n n )
: 2r> r> r> swap r> swap >r ; compile-only ( -- n n,R: n n -- )

\ "tup" gets the address of the Terminal Input Buffer
\ variables, which point to the Terminal Input Buffer itself,
\ whilst "source" gets the contents of what is stored at
\ "{tib}".
\
\ These words are using for parsing, which is done later on.
\
: tup {tib} up ; ( -- a )
: source tup 2@ ; ( -- a u : get terminal input source )

\ "aligned" is one of those words that has been implemented
\ in a non-portable way, so would have to change if the cell
\ size did.
\
\ It takes an address and aligns that address to the next
\ address on a two byte boundary (and on a 32-bit system it
\ would align on a four byte boundary, on 64-bit, 8 bytes).
\
\ For some example mappings:
\
\	0 aligned -> 0
\	1 aligned -> 2
\	2 aligned -> 2
\	3 aligned -> 4
\	4 aligned -> 4
\
\ "align" does the same for "aligned", but operates on the
\ dictionary pointer. It is common to want to align the
\ dictionary pointer after writing a string into the
\ dictionary.
\

: aligned dup lsb 0<> #1 and + ; ( u -- u : align up ptr. )
: align here aligned h lit ! ; ( -- : align up dict. ptr. )

\ "allot" allocates memory in the dictionary, it accepts a
\ signed number of bytes to allocate, so it is possible to
\ move the dictionary pointer backwards, which is occasionally
\ useful, however it is usually used by the user to allocate
\ space for an array, or a string, like so:
\
\	decimal
\	create example 100 allot
\
\ Which creates a word which when run returns an address, that
\ address has 100 *bytes*, not cells, allocated to it.
\
\ "," is used to write a value into the dictionary, a cell,
\ it naturally uses a cells worth of memory. It makes sure to
\ call "align" before hand. It can also be used to allocate
\ memory in the dictionary:
\
\	create example 1 , 2 , 3 ,
\	example @ .
\	example cell+ @ .
\	example cell+ cell+ @ .
\
\ This creates a word "example" which returns an address when
\ run which points the dictionary space where we have just
\ written 1, 2, and then 3, into three consecutive cells.
\
\ On some platforms "," can be used to write an execution token
\ into a word definition, but that is not portable, an
\ execution token does not have to be the same as the number
\ that represents a call to a word within a word definition.
\
\ On this platform, using "," will fail, and will most likely
\ cause a crash or a reset. Instead "compile," is provided,
\ which can convert in a platform specific way the execution
\ token into a function call for that platform and reserve
\ enough space for the value for the call it will write into
\ the dictionary.
\
\ For example:
\
\	: x 2 2 [ ' + compile, ] . cr ;
\	: y 2 2 [ ' + , ] . cr ;
\
\ The function "x" will work, but "y" will not. "compile,"
\ is defined later.
\
: allot aligned h lit +! ; ( n -- : take dictionary space )
: , align here ! cell allot ;

\ "count" and "+string" are two words used for string
\ manipulation. "count" is named because it is often used with
\ counted strings, a counted string consists of a single byte
\ for the length of the string, followed by the rest of the
\ string. That is the traditional mechanism Forth used for
\ strings, but it limits those strings to only containing
\ 256 bytes, not a problem on the memory constrained 16-bit
\ bit microcomputers that Forth grew up on, and not a problem
\ here either.
\
\ "count" can be used to extract the length of a string, but
\ also for moving down that string, or any byte array.
\
\ An example for doing a byte-wise memory dump using "count":
\
\	: cdump for aft count . then next ;
\
\ And a common idiom for printing out counted strings:
\
\	count type
\
\ Which uses "type", a word we will encounter next.
\

: count dup 1+ swap c@ ; ( b -- b c )
: +string #1 over min rot over + rot rot - ; ( b u -- b u )

\ "type" is used to print out a string. It is not complicated,
\ it does not make an effort to not print out non-graphic
\ ASCII characters, so if you print out binary data you will
\ get garbage. It is easy enough to make a version of "type"
\ that did this filter, which would be useful for displaying
\ Forth Blocks with "list".
\

: type ( a u -- : print out a string )
  begin dup while swap count emit swap 1- repeat 2drop ;

\ "fill" is used to fill a section of memory with a byte,
\  hence the name. More frequently "erase" is used, which does
\ a fill but the byte being zero.
\
\ "cmove" is used to move blocks of memory to another section
\ of memory.
\
\ Both functions operate on bytes, not cells. "cmove" is often
\ used for strings however, and "fill" (in the form of "blank"
\ which fills a section of memory with spaces) is used in the
\ block text editor.
\
\ "fill" is equivalent to the C standard library function
\ "memset", and "cmove" to "memcpy".
\

: cmove ( b1 b2 u -- )
   for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;
: fill ( b u c -- )
   swap for swap aft 2dup c! 1+ then next 2drop ;
: erase #0 fill ; ( NB. blank is bl fill )

\ The following words, and two new meta-compiler words, allow
\ us to define two types of counted strings, and use them
\ in our meta-compiled program. The word "do\$" does most of
\ the work, it is a little complex as it has to do some
\ return stack manipulation for the word that calls it, not
\ just for itself.
\
\ * "do\$" pushes the address of a string compiled into the
\ dictionary, the string needs to be placed after the word
\ that *calls* "do\$".
\ * "($)" is used to make counted strings that push the address
\ of the string onto the stack so it can be used elsewhere.
\ * ".\$" is used to make counted strings that are always
\ printed out.
\
\ The two meta-compiler words, use "($)" and ".\$" then call
\ "\$literal" to grab the string from the input stream. Two
\ words similar to the meta-compiler versions will be defined
\ later, after the parsing words have been made.
\
\ How does "do\$" work? Let us see a compiled string:
\
\	: x ." HELLO" cr ;
\
\ This will be compiled to something like this, with one 16-bit
\ cell per line:
\
\	X:
\		address of .$
\		5,'H'
\		'E','L'
\		'L','O'
\		address of exit
\
\ Note that the word '."' does not appear in the compiled
\ program! It compiles ".\$" and the string into the
\ dictionary.
\
\ The word ".\$" must somehow print out the string "HELLO" and
\ then skip over the string in order to continue execution
\ after the string, which happens to be an exit.
\
\ The word ".\$" calls "do\$" which does the finding of the
\ string and the return address fixed up. However, before that
\ we the first thing we call is ".\$", when we call something
\ we push the address of the next cell onto the return stack
\ so when we return, we return to the address after the
\ function just called. In this case when we call ".\$" it will
\ contain the address of the string we want to print out,
\ however, when we call "do\$" from within ".\$" it will have
\ the address of "count" on the return stack (which we will
\ want to execute) and the address of the string after that.
\ "do\$" needs to extract the string address from return stack,
\ copy it as it is meant to leave a copy on the state stack,
\ calculate the next address after the string, and then
\ replace the string address on the return stack with the
\ address of the place after the string (aligned up of course).
\ It also needs to leave the address of "count" on the return
\ stack so when we return from "do\$" it will still call
\ "count", however after fixing up the return address of
\ the string when ".\$" returns, it returns to the place after
\ the string.
\
\ It does all this in quite a short amount of code. It is not
\ as complex as it sounds, it is more of a trick. "do\$" also
\ has to convert to and from cell address, with "2*" and "2/",
\ not difficult.
\
\ "do\$" is not a general purpose word, it should not be used
\ in any "normal" code, nor is the technique it uses a general
\ or good one. It does make for some nicely compact code
\ however, and is one way of doing introspection.
\
\ Another example of messing around with the return stack
\ to achieve greatness is:
\
\	: ?exit if rdrop then ; compile-only
\
\ Which will conditionally return from the *caller* of the
\ function, an example usage:
\
\	: x ." Executed." cr ?exit ." Conditionally Exec." cr ;
\	0 x
\	1 x
\

:s do$ r> r> 2* dup count + aligned 2/ >r swap >r ;s ( -- a  )
:s ($) do$ ;s           ( -- a : do string NB. )
:s .$ do$ count type ;s ( -- : print string in next cells )
:m ." .$ $literal ;m ( --, ccc" : compile string )
:m $" ($) $literal ;m ( --, ccc" : compile string )

\ "space" emits a space. This is all. I will not write an
\ essay to describe this.
\

: space bl emit ; ( -- : emit a space )

\ # Exception Mechanism: Catch and Throw
\
\ For exceptional circumstances Forth provides the words
\ "catch" and "throw", usually the author does not like 
\ exceptions in languages other than Forth (specifically C like 
\ languages, although the higher level the language is, the 
\ more acceptable they are), I instead prefer other mechanisms,
\ or even just returning error codes.
\
\ As an aside about languages and error handling:
\
\ One of the only languages that gets error handling right,
\ from what I have seen, is Rust, however that language is
\ the opposite of what Forth is. That is not a bad thing, in
\ fact it is a far more usable programming language than Forth
\ is, however no individual programmer could make a Rust
\ implementation, it is an industrial project. It is possible
\ for an individual programmer to make C compiler, or more
\ easily a Pascal compiler, and use that to build a toy
\ operating system akin to the original Unix, an individual has
\ very little possibility without dedicating a massive fraction
\ of their lives to making a Rust, or a C++ compiler, that
\ is compliant. A Forth system can built and understood, from
\ machine to user interface, by a single person. It is a short
\ and sweet language.
\
\ Java uses exceptions because it believes programmers are too
\ lazy to check return codes, which is true, but just leads to
\ programmers being lazy with exceptions (and catching too
\ many of them). Adding exceptions to a language does not solve
\ the lazy programmer problem, making it easier to check an
\ error than to ignore it does, which is what Rust does.
\ C++ is best left not talked about, which I find is true in
\ general.
\
\ Forth does use exceptions, but uses them sparingly, usually
\ only when something has gone very wrong. It is also a pain
\ to pass up error codes in Forth programs as everything is
\ passed via the stack. For truly exceptional circumstances,
\ where calling "abort" might be appropriate, an exception
\ can be thrown instead. It does not encourage their use and
\ one does not find themselves asking "what exceptions can this
\ possibly throw?", and they are usually left uncaught except
\ by the outer interpreter, which has an exception handler of
\ last resort.
\
\ The appendix contains a list of error codes that are thrown
\ by this interpreter.
\
\ While it can be tricky to get catch/throw correct, their
\ operation in Forth is quite simple due to the execution model
\ of Forth. It is a dual stack system where the stacks are
\ easily accessible.
\
\ The first word defined "catch" accepts an execution token,
\ it will execute that token, and in doing so return an error
\ code. It will return zero if nothing has been thrown, or
\ a non-zero number if an exception has been thrown by "throw"
\ in the token just executed.
\
\ A common idiom not used in this Forth, because the bitwise
\ operators are expensive and branching is cheap, is to use
\ the following construct to conditionally throw:
\
\	0= -1 and throw
\
\ Or more generally:
\
\	<conditional> <error-code> and throw
\
\ Which will throw an error if a condition is true, which
\ replaces:
\
\	<conditional> if <error-code> throw then
\
\ "if...then" generates slightly larger code on most Forth
\ implementations, and branches, than using "and". The source
\ code is slightly larger, although immediately easier to read.
\
\ Using "and" works in Forth as conditionals return Forth
\ booleans where true is all bits set, and zero, or all bits
\ clear when false. Using this in conjunction with "and" on
\ the error code means that the code will be preserved on
\ failure (conditional is true) and replaced with zero on
\ success (conditional is false). "throw" only throws an
\ exception when given a non-zero error code. There is synergy
\ between the words.
\
\ The implementation of "catch" and "throw" are close the
\ the example implementations in various standards and notes,
\ such as in the ANS Forth standard
\ <http://lars.nocrew.org/forth2012/exception/THROW.html> and
\ and the application note "Catch and Throw" by
\ Michael Milendorf, Sun Microsystems from EuroForth 98.
\
\ The idea is in "catch" we store the data stack pointer on 
\ the return stack saving our data stack position, along with
\ the previous exception handler, we then store the current
\ return stack position in the handler and then execute the
\ token given. If the execution token throws an exception 
\ then the rest of "catch" after "execute" is not executed,
\ only when no exception is thrown is it executed, it just
\ has to restore the variable stack pointer and the previous
\ exception handler and push "zero" indicating no error.
\
\ "throw" restores the previous stack pointer and handler,
\ but leaves a non-zero error code on the stack for future
\ consumption.
\

: catch        ( xt -- exception# | 0 \ return addr on stack )
   sp@ >r                ( xt )   \ save data stack pointer
   {handler} up @ >r     ( xt )   \ and previous handler
   rp@ {handler} up !    ( xt )   \ set current handler
   execute               ( )      \ execute returns if no throw
   r> {handler} up !     ( )      \ restore previous handler
   rdrop                 ( )      \ discard saved stack ptr
   #0 ;                 ( 0 )     \ normal completion

: throw ( ??? exception# -- ??? exception# )
    ?dup if              ( exc# )     \ 0 throw is no-op
      {handler} up @ rp! ( exc# )     \ restore prev ret. stack
      r> {handler} up !  ( exc# )     \ restore prev handler
      r> swap >r         ( saved-sp ) \ exc# on return stack
      sp! drop r>        ( exc# )     \ restore stack
    then ;

\ Now we have "catch" and "throw", we can use them, the next
\ chapter defines the more advanced arithmetic words and
\ division will need a way to throw if provided numbers outside
\ the range for which the function is valid (ie. if we try to
\ divide by zero).
\
\ But there are a few short words that we will define first
\ that are also useful. Those are "abort" and "?depth".
\
\ "abort" throws -1, which should not be caught by the
\ exception handler of last resort in the "interpret" word, but
\ will cause "interpret" to halt the system after printing
\ " -1?". "(abort)" is used in a similar function later on
\ that prints out a string, but is also conditional, only
\ aborting and printing out the error message if given a non
\ zero number, that will be defined later, but the previously
\ defined words for string handling "do\$" should be understood
\ to work out how this function will operate.
\
\ "depth" calculates the current number of items on the
\ stack, it gives its answer in the number of cells consumed,
\ not the number of bytes. This is then immediately used to
\ make the word "?depth", which throws an exception if there
\ are not enough items on the stack.
\

: abort #-1 throw ; ( -- : Time to die. )
:s (abort) do$ swap if count type abort then drop ;s ( n -- )
:s depth {sp0} lit @ sp@ - 1- ;s ( -- n )
:s ?depth depth >= if -4 lit throw then ;s ( ??? n -- )

\ # Advanced Arithmetic
\
\ The Forth arithmetic word-set attempts to solve the most
\ complex arithmetic problem, and in doing so make the simpler
\ problems trivial. This is not usually a viable method to
\ take, greater complexity just usually leads to greater
\ complexity, however here it works well.
\
\ A note about Forth, when using the term "double" it does
\ not refer to the floating point number, it refers to numbers
\ which are twice the normal width of a number, so on a 16-bit
\ system a 32-bit value stored as two integers on the stack
\ would be a "double". It comes from "double width" or
\ "double precision". Many Forth implementations do not define
\ the floating point word-set as Forth originated on quite
\ limited systems, systems that might not have had floating
\ point numbers and it would have been quite the slow down to
\ implement them in software (and the routines would have taken
\ up precious space). A lot of the decisions that went into
\ Forth are a consequence of the limited hardware on
\ microcomputers available in the 1980s. If starting from
\ scratch software wise, but with modern hardware, you would
\ not create a language like Forth. It is a product of its
\ time, which in a way makes it magical.
\
\ As the author was not sentient in the 1980s, it makes them
\ nostalgic for a time that they were not part of, a kind of
\ false nostalgia. One with simpler problems, no internet,
\ just man and the machine, a machine that could be understood
\ by a single person and does not collect telemetry, spy on
\ you, advertise to you, or reboot when it wants.
\
\ Anyway...
\
\ By implementing "um+", "um\*", "um/mod", and "m/mod", the
\ other arithmetic operators are much easier to implement.
\ It is also much easier to implement "/", the Forth word
\ for division, in terms of "um/mod", than it is to implement
\ "um/mod" in terms of "/".
\
\ The numeric tower of words is all built upon "um+", this
\ word performs an addition with carry, which many
\ systems have instructions specifically for. This one does
\ not, so it has to compute the carry itself, we already have
\ addition thankfully.
\
\ It takes two single cell numbers and adds them together,
\ the result of the addition and then the carry is pushed to
\ the stack. Using this we can construct some double cell
\ words, "um+" is a mixed word, it effectively produces a
\ double cell word, the name of the word closely follows the
\ naming convention for Forth words of these type, it says
\ "unsigned mixed addition", in effect.
\
\ The double cell words all have "d" in them, for example
\ "dnegate", or "d+", the double cell words are usually treated
\ as signed words, with the highest 16-bits stored in the
\ topmost cell on the stack.
\
\ "um*\", unsigned mixed multiply, takes two single cell
\ numbers and multiplies them together producing a double
\ cell number.
\
\ Note the algorithm used for multiplication, one naive way
\ to do multiplication of two numbers, say "m" and "n" is
\ to repeatedly add "m" to a register "n" times, it works but
\ is very slow. "um\*" only loops for 16 times however, on this
\ 16-bit platform, one for each bit, and always loops for the
\ same number of times, so it cannot logically be using that
\ algorithm to perform multiplication.
\
\ TODO: Describe multiplication algorithm.
\
\ Given "um\*", "\*" can be coded with:
\
\	: * um* drop ;
\
\ Rendering its implementation trivial. To really improve
\ the speed of this implementation, and any similar eForth 
\ implementation which uses "um+" for multiplication, "um+" 
\ should be made to be as fast as possible.
\

: um+ 2dup + >r r@ #0 >= >r ( u u -- u carry )
   2dup and 0< r> or >r or 0< r> and invert 1+ r> swap ;
: dnegate invert >r invert #1 um+ r> + ; ( d -- d )
: d+ >r swap >r um+ r> + r> + ;         ( d d -- d )
: um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) F lit
  for
    dup um+ >r >r dup um+ r> + r>
    if >r over um+ r> + then
  next rot drop ;
: * um* drop ;
: um/mod ( ud u -- ur uq : unsigned double cell div/mod )
  ?dup 0= if -A lit throw then
  2dup u<
  if negate F lit
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> ( or -> ) 0<> swap 0<> +
      if >r drop 1+ r> else drop then r>
    next
    drop swap exit
  then 2drop drop #-1 dup ;
: m/mod ( d n -- r q : floored division )
  s>d dup >r
  if
    negate >r dnegate r>
  then
  >r s>d if r@ + then r> um/mod r>
  if swap negate swap exit then ;
: /mod over 0< swap m/mod ; ( u1 u2 -- u1%u2 u1/u2 )
: mod  /mod drop ; ( u1 u2 -- u1%u2 )
: /    /mod nip ; ( u1 u2 -- u1/u2 )

\ We can implement some of the signed double cell arithmetic
\ words if we need them as well, with:
\
\	: 2swap >r -rot r> -rot ;        ( w x y z -- y z w x )
\	: d< rot 2dup >                    ( d -- f )
\	  if = nip nip if 0 exit then -1 exit then
\	  2drop u< ;
\	: d>  2swap d< ;                   ( d -- t )
\	: du> 2swap du< ;                  ( d -- t )
\	: d=  rot = -rot = and ;           ( d d -- t )
\	: d- dnegate d+ ;                  ( d d -- d )
\	: dabs  s>d if dnegate exit then ; ( d -- ud )
\
\ But by default they will not be included as they are easy
\ enough to define if we need them and we want to keep the
\ interpreter nice and slim.
\

\ # Terminal Input and Word Parsing
\
\ We have a pretty solid base, we will not move onto the next
\ stage, getting a line of input, and parsing that into a
\ stream of words.
\
\ The terminal line handling should work over a UART, and also
\ interact correctly when typing in commands as a program
\ running in a virtual terminal. Usually the operating system
\ handles line discipline, however in case it does not these
\ words handle it as well, there is no conflict between the
\ two.
\
\ The line parsing routines culminate in the construction of
\ "query", the word parsing words with "parse", both will
\ require a few useful words which we will make on the way.
\
\ We need to construct another word, synonymous with "emit",
\ but uses a different variable as an execution vector, we
\ will sometimes want to silence the output from "echo", but
\ not from "emit". We will also need to make the word that
\ is the execution vector for both "emit" and "echo", which we
\ have not done so far, that word will be called "(emit)".
\
\ All "(emit)" has to do is called "opEmit", if we want to
\ replace the execution vector with something that does nothing
\ then we cannot use "nop", as that would leave an item on the
\ stack where one should not be, it will be replaced with a
\ "drop" instead.
\
\ The default execution vectors are set later on during the
\ "task-init" function and depend partially on the "{options}"
\ flags to enable or disable echoing with "echo".
\
\

:s (emit) opEmit ;s ( c -- : output byte to terminal )
: echo <echo> @ execute ; ( c -- : emit a single character )

\ "tap" and "ktap" are both used by "accept", they are both
\ given four items on the stack, and return three, which is
\ quite a lot. The arguments given are as follows:
\
\ 	bot - Bottom Of Text
\ 	eot - End Of Text
\ 	cur - Current Text Position
\	c   - The character to process.
\
\ The input buffer and our position in it is represented by
\ the first three arguments, "bot", "eot", and "cur". We are
\ given a new character to process and decide on what to do
\ with.
\
\ "accept" is the word that calls these two words and will
\ finish processing when "cur" is equal to "eot", which would
\ mean the input buffer is full. "ktap" takes advantage of that
\ and uses that to force "accept" to exit when it encounters a
\ newline.
\
\ "bot" is needed because when we delete characters we need to
\ move the "cur" value backwards, however if we do it too much
\ then we will end up before the buffer, so we must prevent
\ "cur" going lower than this value.
\
\ "tap" echos a character given to it and then writes it to
\ the buffer. It also advances the string given to it. "accept"
\ calls "tap" for normal characters that do not need special
\ processing, like "a", or space, or 0.
\
\ "ktap" processes control characters, the Delete character,
\ the backspace and newlines. None of the characters given
\ to "ktap" should be written to the buffer given to accept
\ but should trigger different behaviors.
\
\ As mentioned, if "c" is a newline character "ktap" causes
\ "cur" to equal "eot", causing accept to exit.
\
\ The backspace is treated the same as the delete character,
\ some systems use one or the other. It deletes a single
\ character from the input buffer by manipulating the "cur"
\ position (it adds the result of a boolean to "cur", which is
\ negative if we are processing a delete character and we are
\ not at the bottom of the input buffer). It also emits the
\ following sequence; a backspace, a space, and another
\ backspace, in an attempt to erase the previous character on
\ the screen if the same conditions are met to remove the
\ previous character.
\
\ The tests could be factored out, and the delete functionality
\ "=bksp lit dup echo bl echo echo" are sometimes factored out.
\
\ If "ktap" does not know what to do with the control character
\ then it replaces it with a space and calls "tap".
\
\ This is a complex word, quite optimized, to handle terminal
\ input handling.
\

:s tap dup echo over c! 1+ ;s ( bot eot cur c -- bot eot cur )
:s ktap ( bot eot cur c -- bot eot cur )
  dup dup =cr lit <> >r  =lf lit <> r> and if ( Not EOL? )
    dup =bksp lit <> >r =del lit <> r> and if ( Not Del Char? )
      bl tap ( replace any other character with bl )
      exit
    then
    >r over r@ < dup if             ( if not at start of line )
      =bksp lit dup echo bl echo echo ( erase char )
    then
    r> + ( add 0/-1 to cur )
    exit
  then drop nip dup ;s ( set cur = eot )

\ "accept" is a useful word on its own, it gets a line of
\ input and stores it a specified location, it returns the
\ length and location of the accepted string.
\
\ An example usage:
\
\	pad 20 accept
\	HELLO WORLD
\	.s
\	type
\
\ Which will accept a line of text, "HELLO WORLD", and store
\ it into the pad area, then print what "accept" returns,
\ and then regurgitates the input string.
\
\ We will not use "accept" directly, but use it to make
\ "query", it does more work though, "query" just calls
\ "accept" on some internal buffers.
\
\ "accept" must make sure it does not overrun the input
\ buffer, it also must handle control characters. Note that
\ there is no part of the loop that says "terminate this
\ function when a newline is occurred", it only stops the
\ loop when the current cursor position
\
\ "tib" is a convenience word accessing the Terminal Input
\ buffer. It is immediately used by "query", note that query
\ also defines what our maximum length of a line is, this
\ itself could be made into a variable so it can be changed,
\ but we gain little from that on such a limited system.
\
\ After query is completed it sets the index into the line
\ being parsed to zero, so the parser starts from the
\ beginning, and also sets the length of the line just parsed.
\
\ After calling query we can split up the line.
\

: accept ( b u -- b u : read in a line of user input )
  over + over begin
    2dup <>
  while
    key dup bl - 5F lit u< if tap else ktap then
  repeat drop over - ;
: tib source drop ; ( -- b )
: query tib =buf lit accept tup ! drop #0 >in ! ; ( -- )

\ "-trailing" removes the trailing white-space from an input
\ string, it does this non-destructively leaving the original
\ string intact, it just modifies the string length of an
\ address-length pair. It is used to post process the line
\ given to us by query, it is not appropriate to call it
\ in query to make it as reusable as possible.
\
: -trailing for aft ( b u -- b u : remove trailing spaces )
     bl over r@ + c@ < if r> 1+ exit then
   then next #0 ;

\ "look" is a moderately complex word, it takes a string,
\ a character to parse until, and an execution token.
\ The execution token is for a function that takes two
\ characters and should return a boolean indicating when
\ to stop looking within a word.
\
\ The character is stored in the topmost return stack position
\ for easy access with "r@" and dropped before exit.
\
\ White-space is treated specially by "look", if given "bl"
\ it will directly affect the "unmatch" and "match" words.
\ Also of note is that "ktap" converts control characters and
\ other no-graphic characters to space, or "bl", characters,
\ meaning they be parsed as white-space.
\
\ "parse" is more fiddly than complex, it creates the string
\ variables from which it will work from the Terminal Input
\ Buffer and the ">in" variable, then uses two calls to
\ "look" to find the start and end with "unmatch" and "match"
\ respectively. It then calculates a delta between the unmatch
\ and match which it adds to ">in", meaning subsequent calls
\ to "parse" will skip over the section we have just parsed.
\
\ After that it retrieves the beginning of the "unmatch"
\ where our parsed word resides, it also trims and white-space
\ from it if we are parsing white-space.
\
\ In short "parse" does the following:
\
\ 1. Retrieve memory area to parse from.
\ 2. Finds the beginning of the match.
\ 3. Finds the end of the match.
\ 4. Calculates the distance between these two in bytes.
\ 5. Fixes up the results.
\
\ It also has to deal with the situation of not finding
\ anything, in which case the string length it returns will
\ be zero length.
\
\ With "parse" defined it feels like we could now complete
\ the interpreter, however there are a series of other words
\ we will have to make, some for numeric input, others for
\ searching for words in the dictionary, before we can
\ utilize "parse", it is just one piece of the puzzle.
\

:s look ( b u c xt -- b u : skip until *xt* test succeeds )
  swap >r rot rot
  begin
    dup
  while
    over c@ r@ - r@ bl = 4 lit pick execute
    if rdrop rot drop exit then
    +string
  repeat rdrop rot drop ;s
:s unmatch if 0> exit then 0<> ;s ( c1 c2 -- t )
:s match unmatch invert ;s        ( c1 c2 -- t )
: parse ( c -- b u ; <string> )
  >r tib >in @ + tup @ >in @ - r@ ( get memory to parse )
  >r over r> swap >r >r
  r@ t' unmatch lit look 2dup ( look for start of match )
  r> t' match   lit look swap ( look for end of match )
    r> - >r - r> 1+ ( b u c -- b u delta : compute match len )
  >in +!
  r> bl = if -trailing then
  #0 max ;

\ # Numeric Input and Output
\
\ Numeric Input and Output deserves its own section, Forth
\ provides a flexible way to format numeric output, less so
\ for parsing numbers, but does provide some words. The way
\ printing numbers is done is a little different from other
\ implementations of eForth purely for speed purposes, the
\ word "." needs to be as fast as possible so does not use
\ "um/mod" to extract digits from numbers. Unsigned number
\ printing does, so is slower, but that is less of a concern.
\
\ The core of the numeric output system are the words
\ "\<#", "#" and "#\>", for numeric input the main word
\ is "\>number", which does a lot, and then the secondary
\ word "number?" which pre and post processes the results of
\ "\>number".
\
\ All of the numbers are affected by the "base" variable,
\ which controls which bases are allowed in when parsing, and
\ what base they are printed out as.
\
\ The output triplet words, "\<#", "#" and "#\>" operated on
\ what is known as "hold-space", an area available (one for
\ each thread, much like the base variable) for formatting
\ output strings. Both the hold space and the base variable
\ make numeric I/O thread safe but not reentrant, a flaw, but
\ not a big one given the nature of Forth.
\

\ "banner" is a word I keep defining, but it is not a standard
\ word so is kept in the system vocabulary, it prints a
\ character "n" number of times, it accepts a signed number,
\ and does not print out characters if "n" is negative.
\
\ It is included as it is a factor of "u.r". It was used
\ for a more fancy version of "list", but the complexity of
\ that word has been reduced.
\
:s banner ( +n c -- )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ;s

\ "hold" adds a number to hold space, this version does not
\ check that it overflows hold space, so the programmer must be
\ careful not to do this. As mentioned, hold space is a per
\ thread area where numeric output strings are formatted.

: hold ( c -- : save character in hold space )
  #-1 hld +! hld @ c! ;

\ "#\>" is the last of the three (four including "\#s") words
\ that form the core of the numeric output words, although it
\ is defined first. It removes a number from the stack and
\ returns a string and string length to the string we have just
\ formed in hold space by using various combinations of "\<#", 
\ "\#" and "hold".
\
: #> 2drop hld @ this =num lit + over - ; ( u -- b u )

\ "extract" extracts (hence the name) a single number from
\ a double cell, it should this with the divide/modulo number,
\ if we divide a number by the base we are operating in then
\ we can extract a single digit from that number in the
\ returned remainder argument, the quotient is reused until
\ it is zero and no more digits can be extracted from the
\ number.
\
:s extract ( ud ud -- ud u : extract digit from number )
  dup >r um/mod r> swap >r um/mod r> rot ;s

\ "digit" converts a single digit, which might be from 0 to
\ 35 depending on the base, into an ASCII character which
\ represents that digit, it does this without branching
\ using the behavior of the comparison and logical operators
\ to do this.
\
:s digit 9 lit over < 7 lit and + [char] 0 + ;s ( u -- c )

\ "\#" extracts a single digits and adds it to hold space.
\ "\#s" continues to do this until the number is zero, which is
\ the most commonly desired option.
\
\ "\<#" resets hold space for use by "\#" and "#\>".
\
\ If you wanted to make a number with N leading zeros, or with
\ period between each character, or whatever is required for
\ the output, you can use these set of words to perform that.
\
: #  2 lit ?depth #0 base @ extract digit hold ; ( d -- d )
: #s begin # 2dup ( d0= -> ) or 0= until ;       ( d -- 0 )
: <# this =num lit + hld ! ;                     ( -- )

\ "sign" is a adds a "-" character to hold space if the
\ provided number is negative. It is used by ".".
\
: sign 0< if [char] - hold then ;                ( n -- )

\ "u.r" and "u." print out unsigned numbers, "u." prints out an
\ unsigned number with a single space before it, "u.r" allows
\ a variable number of spaces to be printed out before the
\ number to be printed out. This allows for the creation of
\ aligned numeric output.
\
: u.r >r #0 <# #s #>  r> over - bl banner type ; ( u r -- )
: u.     #0 <# #s #> space type ; ( u -- )

\ "(.)" bypasses all of the standard Forth mechanisms for
\ formatting Forth words, the reason for doing this is speed,
\ complex operations like division, as implemented by "um/mod",
\ are slow on this SUBLEQ machine, in order to output the
\ image quickly once meta-compiled we need a fast way to
\ format and print numbers, faster than using the more generic
\ "um/mod" and numeric output words.
\
\ "(.)" is recursive, note that the word "recurse" does not
\ need to be used, as when we are between ":s" and ";s" we
\ might be between a meta-compiled Forth definition for the
\ target, but we are not between a Forth definition in the
\ host, and the host doing the meta-compilation is still in
\ command mode.
\
:s (.) abs base @ opDivMod ?dup if (.) then digit emit ;s

\ "." uses "(.)", it just needs to check if the number to print
\ is negative, if so, it emits a single "-" character.
\
: . space dup 0< if [char] - emit then (.) ;

\ "\>number" is a large but not terribly complex word, it
\ is however a bit unwieldy to use, it operates on double
\ cell numbers instead of single cell ones. By solving the
\ problem of parsing double cell numbers instead of single
\ cell numbers a more generic word that can be used to solve
\ both can be made. This is much like the arithmetic word-set,
\ solving the more complex problem to allow for a more generic
\ system.
\
\ The word operates on double cell words and a string, it
\ consumes characters in a given base determined by the "base"
\ variable. The function stops when it encounters the first
\ non-digit character leaving the remaining string on the
\ stack (this can be used to determine whether a string
\ contains a number, or a number and other characters).
\
\ The character is converted to a number, for example "A"
\ becomes the decimal digit 10 in bases higher than ten. The
\ digit is then accumulated into a double cell, the double
\ cell is multiplied by the base before the digit is added. By
\ providing the initial double cell the word is made to be
\ slightly more flexible than if it just pushed a double cell
\ zero onto the stack itself.
\

: >number ( ud b u -- ud b u : convert string to number )
  begin
    2dup >r >r drop c@ base @        ( get next character )
    ( digit? -> ) >r [char] 0 - 9 lit over <
    if 7 lit - dup A lit < or then dup r> u< ( c base -- u f )
    0= if                            ( d char )
      drop                           ( d char -- d )
      r> r>                          ( restore string )
      exit                           ( finished...exit )
    then                             ( d char )
    swap base @ um* drop rot base @ um* d+ ( accumulate digit )
    r> r>                            ( restore string )
    +string dup 0=                   ( advance, test for end )
  until ;

\ "number?" is much easier to use than "\>number" and handles
\ several extensions to the numeric input handling in Forth,
\ which are; handling negative and hexadecimal numbers, as well
\ as double cell numbers which have a fixed point.
\
\ This Forth uses the prefix "\$" to indicate that the number
\ is hexadecimal, regardless of what base we are currently in,
\ the equivalent in C being the prefix "0x". In some Forth
\ interpreters, albeit not this one, "\#" is used to indicate
\ the number is always decimal. These prefixes seem natural to
\ use for these purposes that is hard to quantify, much like
\ using is sometimes used "\$" to indicate something is a
\ string, or "/" should be used as a path separator. Perhaps
\ it just part of the culture of computers and programming, but
\ it seems using something like "@" to indicate something is
\ hexadecimal just seems *wrong*.
\
\ For fixed numbers "dpl" is used, some explanation of it is
\ required. When processing a numeric string like "123" it
\ will be converted to a single 16-bit number, it can be
\ signed, such as "-123", and it is processed in the current
\ base. However, 16-bit numbers are often not enough, we might
\ need the range 32-bit numbers give us, but we will need a
\ syntax in order to enter them. This is done by entering the
\ number with a period, for example:
\
\	123.
\	1.23
\	.123
\
\ Are all valid Forth numbers, but when view with ".s" on the
\ stack they all produce the same result when entered at the
\ Forth prompt, "123 0". There is a difference between the two
\ however, the "dpl" variable will be set with different
\ values.
\
\	123    -> dpl is -1 (single cell number was entered)
\	123.   -> dpl is 0
\	1.23   -> dpl is 2
\	.123   -> dpl is 3
\	0.123  -> dpl is 3
\	0.0123 -> dpl is 4
\
\ Note that this is used for fixed point, although with the
\ right floating point words some Forth implementations have
\ used it for input of floats.
\
\ By examining the "dpl" variable after we can determine
\ whether the number inputted is a single or double cell
\ number, and if it is a double cell number, where the decimal
\ point lies.
\
\ A peculiarity of the implementation is that "1.2.3" is a
\ valid number, with only the last ".3" having an effect on
\ the "dpl" value.
\
: number? ( a u -- d -1 | a u 0 : easier to use than >number )
  #-1 dpl !
  base @ >r
  over c@ [char] - = dup >r if     +string then
  over c@ [char] $ =        if hex +string then
  >r >r #0 dup r> r>
  begin
    >number dup
  while over c@ [char] . <>
    if rot drop rot r> 2drop #0 r> base ! exit then
    1- dpl ! 1+ dpl @
  repeat
  2drop r> if dnegate then r> base ! #-1 ;

\ ".s" is a useful debugging word that requires that "." is
\ implemented. It displays the contents of the variable stack
\ without modifying or dropping numbers from it.
\
\ It would have been quite useful to have ".s" and "." when
\ debugging the bringing up of the Forth virtual machine,
\ however as we have seen these words both require the Forth
\ VM to be operation, and a lot of other Forth words to be
\ written before they can be implemented. Having a way to print
\ out numbers is useful, and in fact basic, necessity in
\ debugging a platform. One way this was achieved before ".s"
\ and "." could be supported is by implementing more primitive
\ versions of the numeric output words, it is quite easy to
\ print numbers in hexadecimal with only shift rights,
\ bitwise "AND" and addition.
\
: .s depth for aft r@ pick . then next ; ( -- : show stack )

\ # Search Order Words
\
\ The search order words allow us to find words in the
\ dictionary, which is composed of at least one but possibly
\ multiple vocabularies.
\
\ These words need intimate knowledge of the word header layout
\ which is not that complex, but the words are not portable
\ between different Forth implementations, just because this
\ Forth does it one way, does not mean others do. Some Forth
\ implementations use a hash to map word names to function
\ bodies, but it is traditional to use one of the simplest
\ data structures, the linked list.
\
\ A vocabulary is a linked list of words, and a dictionary is
\ an array containing multiple pointers to those linked lists.
\
\ This Forth keeps its word header in the same place at it
\ does the function body, this is not traditional, and limits
\ us in some ways, but it is easier to implement and more
\ compact. One advantage of keeping the word header separate
\ from the function is that you can erase all of the headers
\ and the code will still work, this is useful for systems
\ where you want to save room or deter reverse engineering,
\ such as programmings running unattended on embedded micro
\ controllers, where the word header is not needed for
\ interactive compilation and typing in commands. The word
\ headers are only needed for compilation, not for running
\ the code, so they could be erased. This is not possible
\ in this Forth (although the meta-compiler can elide word
\ headers in the generated Forth code) because of the word
\ header is stored intertwined with the code.
\
\ "#vocs" is the constant that tells us how many dictionaries
\ we can keep at one time, eight is usually the minimum, and on
\ this system that is the case, we can have a total of eight
\ and minimum of zero vocabularies active at any given period.
\
\ "(search)" will traverse a vocabulary looking for a word
\ until one is found, or exiting without finding one, and
\ "(find)" applies "(search)" to the array of vocabularies
\ with the same exit conditions.
\
\ The two words are actually very simple; look through a
\ linked list, look through an array, essentially.
\
\ A few other words for dealing with the word headers are also
\ made, "nfa" and "cfa" (although "cfa" is not needed here,
\ it makes sense to define it in the same place as "nfa").
\
\ The word "compare" for testing string equality, required
\ by "(search)", is also defined here.
\

\ "compare" exits early if the strings are not equal, otherwise
\ it compares the strings byte for byte and exits if they are
\ not equal. To keep the number of items on the stack small
\ we can use "for" to traverse both strings, given we now know
\ they are of equal length, and then use "count" to move the
\ string along and extract a byte. We use subtraction to test
\ whether the two are equal or not, and return that test value
\ much like the C "strcmp".
\

: compare ( a1 u1 a2 u2 -- n : string equality )
  rot
  over - ?dup if >r 2drop r> nip exit then
  for ( a1 a2 )
    aft
      count rot count rot - ?dup
      if rdrop nip nip exit then
    then
  next 2drop #0 ;

\ "nfa" stands for "Name Field Address", it takes a pointer
\ to the start of a words header and moves that pointer to
\ the field containing the start of the name of that word.
\
\ "cfa" uses "nfa", and then moves over the name returning
\ a pointer to the Code Field Address, which is an executable
\ address equivalent to an execution token. "cfa" has a
\ slightly harder job as it must retrieve the name length,
\ mask off the length as the upper bits are used for other
\ purposes and increment past that, and then align that
\ pointer.
\

: nfa cell+ ; ( pwd -- nfa : move word ptr to name field )
: cfa ( pwd -- cfa : move to Code Field Address )
  nfa dup c@ 1F lit and + cell+ cell negate and ;

\ "(search)" and "(find)" are made to be as generic as
\ possible, they are not complex words, but they do a lot.
\
\ Both words return the same thing, they both also accept
\ a counted string to look for, only "(search)" has an
\ extra input argument, the word list to look in. "(find)"
\ uses the global array of wordlists so it does not need to
\ be passed in as an argument.
\
\ They both return one of three things, two of which can occur
\ on a match, the third on no match. The stack comment shows
\ the three possible sets of returned values:
\
\	PWD PWD   1 : Word found, word is
\	PWD PWD  -1 : Word found, word is
\	  0   a   0 : Word not found, returns name in "a"
\
\ "a", the word passed in, is returned when the word is not
\ found to make displaying an error message indicating that
\ that word has not been found, if so desired, easier to
\ implement.
\
\ The words are used normally to find words during execution
\ and compilation of those words, however because of what it
\ returns it can also be used during decompilation, where we
\ want to determine the last cell within word definition.
\
\ The pair of words return two "PWD" fields when a word is
\ found, the address of the previous word in the linked list
\ which points to found word and the word that has been found.
\
\ This can help during decompilation to determine the last
\ cell in a word, the previous word in the linked list of words
\ starts (assuming no "allot" happens in between the word
\ definitions) where the last one ended. As a last resort the
\ "here" pointer can be used as a maximum value to not
\ decompile past. In short, it can be used to find the start
\ an end point of a word definition which is useful for
\ decompilation.
\

:s (search) ( a wid -- PWD PWD 1 | PWD PWD -1 | 0 a 0 )
  \ Search for word "a" in "wid"
  swap >r dup
  begin
    dup
  while
    dup nfa count 9F lit ( $1F:word-length + $80:hidden )
    and r@ count compare 0=
    if ( found! )
      rdrop
      dup ( immediate? -> ) nfa 40 lit swap @ and 0<>
      #1 or negate exit
    then
    nip dup @
  repeat
  rdrop 2drop #0 ;s
:s (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word )
  >r
  context
  begin
    dup @
  while
    dup @ @ r@ swap (search) ?dup
    if
      >r rot drop r> rdrop exit
    then
    cell+
  repeat drop #0 r> #0 ;s
: search-wordlist ( a wid -- PWD 1|PWD -1|a 0 )
   (search) rot drop ;
: find ( a -- pwd 1 | pwd -1 | a 0 : find word in dictionary )
  (find) rot drop ;

\ # The Interpreter

\ The main word defined in this section is called "interpret",
\ when given a counted string it will attempt to execute that
\ string, and throw an error if it cannot. It must deal with
\ compile only words (not shown the diagram below), immediate
\ words, numbers, and compile and command mode.
\
\
\
\	   .--------------------------.    .--------------.
\	.->| Get Next Word From Input |<---| Error: Throw |
\	|  .--------------------------.    .--------------.
\	|    |                                           ^
\	|   \|/                                          | (No)
\	|    .                                           |
\	^ .-----------------.(Not Found) .--------------------.
\	| | Search For Word |----------->| Is token a number? |
\	| .-----------------.            .--------------------.
\	|    |                                       |
\	^   \|/ (Found)                             \|/ (Yes)
\	|    .                                       .
\	|  .-------------------------.  .---------------------.
\	|  | Are we in command mode? |  |  In command mode?   |
\	^  .-------------------------.  .---------------------.
\	|    |               |            |                 |
\	|   \|/ (Yes)        | (No)      \|/ (Yes)     (No) |
\	|    .               |            .                 |
\	|  .--------------.  |   .------------------------. |
\	.--| Execute Word |  |   | Push Number onto Stack | |
\	|  .--------------.  |   .------------------------. |
\	|    ^              \|/                |            |
\	^    |               .                 |           \|/
\	|    | (Yes)   .--------------------.  |            .
\	|    ----------| Is word immediate? |  | .------------.
\	|              .--------------------.  | | Compile num|
\	^                        |             | | into next  |
\	|                       \|/ (No)       | | location in|
\	|                        .             | | dictionary |
\	|  .--------------------------------.  | .------------.
\	|  |    Compile Pointer to word     |  |            |
\	.--| in next available location in  |  |            |
\	|  |         the dictionary         | \|/          \|/
\	|  .--------------------------------.  .            .
\	|                                      |            |
\	.----<-------<-------<-------<-------<------<------<.
\
\ Whilst the diagram shows the overall flow of "interpret", it
\ leaves out a few things, the "compile-only" words is one as
\ mentioned, another is double cell words which are a less
\ known feature of Forth.
\
\ On to the words themselves.
\
\ "(literal)" is to determine whether we should compile a
\ number or push it on to the stack given the state, we do not
\ need to make a version of double cell numbers as we can just
\ call it twice. It is the word that will go into the execution
\ vector, in "literal", which is what "interpret" actually
\ uses. "literal" is an immediate word, as it is often used
\ within word definitions to compile a number computed within
\ that word definition, like this:
\
\	: example [ 2 2 + ] literal . cr ;
\
\ Which is equivalent to:
\
\	: example 2 2 + . cr ;
\
\ Except that the computation of adding two numbers together is
\ done in the first during compilation and not during runtime,
\ making the first more efficient and smaller.
\
\ The reason that "literal" is vectored is so that when writing
\ meta-compilers it is useful to be able to replace the systems
\ version of "literal" with one that writes into the target
\ image instead of one that pushes a number onto the stack or
\ compiles in a non-meta-compiled word definition.
\
\ We can see that it is annoying in this meta-compiled program
\ to always have to type "lit" to compile a number into a
\ definition defined in the target eForth image. We could
\ replace the systems version of "literal" in ":t" and restore
\ it in ";t". However, gforth does not have a mechanism for
\ this, and to be portable between our SUBLEQ eForth and gforth
\ we have to use the "lit" mechanism, which is unfortunate but
\ not world ending.
\
:s (literal) state @ if =push lit , , then ;s
: literal <literal> @ execute ; immediate ( u -- )

\ "compile," is a version of "," which can turn an execution
\ vector into something that will perform a call when compiled
\ into the word. One some platforms execution vectors and calls
\ might have the same numeric format, however on this one they
\ do not, there is a relationship that can be computed (and
\ reversed) but you cannot directly call an execution vector,
\ it must be converted first before it can be compiled into the
\ words body.
\
: compile, 2/ align , ;  ( xt -- )

\ "?found" is called at the end of "interpret" if a word has
\ not be found in the dictionary or is not a number, it prints
\ the string that could not be found in the dictionary and then
\ throws an error. The function either returns the given string
\ or it throws an error.
\

:s ?found if exit then ( b f -- b | ??? )
   space count type [char] ? emit cr -D lit throw ;s

\ The "interpret" diagram above describes the word, mostly, it
\ would be best to lay out the description in listed form as
\ well.
\
\ 1. Interpret is called for a word, given as a found string.
\ 2. Interpret attempt to find the string in the dictionary,
\    if it is found it must be a word, "find" returns a pointer
\    to the found word. We must then ask, what is the
\    interpreter state? If we are in...
\    a) Command Mode - We find out if the word is
\       "compile-only", if it is then we throw an error,
\       otherwise we execute the word and then exit "interpret"
\    b) Compile Mode - If the found word is an immediate word
\       we execute it anyway, otherwise we compile a reference
\       to it into the dictionary.
\ 3. If we could not find the word, we attempt to parse it as
\    a number, if it is a number and we are in...
\    a) Command Mode - We push the number to the stack and
\       exit interpret.
\    b) Compile Mode - We compile the number into the
\       dictionary.
\    For numbers there are two types, single or double cell
\    numbers, we can tell if a double cell number has been
\    parsed as "dpl" is set to a non-negative (inclusive of
\    zero) if a period, indicating a double cell number, is
\    present in the input. We either compile or push either
\    the double or single cell number depending on the
\    interpreter state as mentioned.
\ 4. If it is not a number, or a word, then we throw an error.
\
\ The order of that search matters, as it means that we could
\ define new words that are also numbers, but they would be
\ treated as words and not numbers, for example:
\
\	: 1 2 ;
\	1 ( pushes '2' )
\
\ Is valid, if useless and confusing, Forth code. If numbers
\ were dealt with first then it would still push "1".
\
\ We could make it so that "interpret" executes an execution
\ vector, or it executes one if neither a number or a word is
\ found. Either vector would make the interpreter more
\ flexible, but neither is done, flexibility is not always a
\ good thing.
\
: interpret ( b -- )
  find ?dup if
    state @
    if
      0> if cfa execute exit then \ <- execute immediate words
      cfa compile, exit \ <- compiling word are...compiled.
    then
    drop
    dup nfa c@ 20 lit and if -E lit throw then ( <- ?compile )
    \ if it's not compiling, execute it then exit *interpreter*
    cfa execute exit
  then
  \ not a word
  dup >r count number? if rdrop \ it is a number!
    dpl @ 0< if \ <- dpl is -1 if it's a single cell number
       drop     \ drop high cell from 'number?' for single cell
    else        \ <- dpl is not -1, it is a double cell number
       state @ if swap then
       postpone literal \ literal executed twice if # is double
    then \ NB. "literal" is state aware
    postpone literal exit
  then
  \ NB. Could vector ?found here, to handle arbitrary words
  r> #0 ?found ;

\ # The Root Vocabulary
\
\ The root vocabulary contains the minimal set of Forth words
\ needed to get the dictionary back into a normal state. A
\ Forth interpreter can contain hundreds of words, and have
\ several vocabularies, the root vocabulary is not only a
\ minimal one, but contains words for manipulating the
\ vocabularies themselves. Most of the words placed in this
\ section will be defined with ":r" to place them in the
\ root vocabulary.
\
\ The minimal word-set is:
\
\ * "set-order"
\ * "forth-wordlist"
\ * "system"
\ * "forth"
\ * "only"
\ * "words"
\ * "eforth" (defined much later)
\
\ The other words are needed, but not in the root word-set:
\
\ * "get-order"
\ * ".id"
\ * "definitions"
\
\ So long as we keep the root word-set in the list of
\ vocabularies that are active at any one time, we can always
\ type "only forth definitions" to get the word list into
\ a good, known, order.
\
\ Forth implementations sometimes only have one vocabulary,
\ especially the more amateur implementations. Vocabularies
\ are useful (especially for meta-compilation) but are not
\ necessary, so they are often viewed as unneeded.
\
\ When a Forth implementation does have vocabulary, it usually
\ has the following ones:
\
\ * "root", for the minimal set of Forth words.
\ * "forth", containing the standard Forth words.
\ * "assembler", containing words for the systems assembler.
\ * "editor", which has words for the block editor.
\
\ This eForth also has the "system" word-set, a lot of Forth
\ implementations dump all of the words defined for internal
\ use, which are non-standard, into the main Forth vocabulary,
\ which clutters the system and means that word names might
\ clash. One of the supposed advantages of the cluttered
\ approach is the logic that if those words are useful for
\ the implementation then they will most likely be useful for
\ the user of the implementation, so they should not be hidden
\ away.
\
\ The "assembler" word-set is one that is not implemented in
\ this Forth, it contains a set of system specific words
\ for making Forth definitions in assembly for that platform.
\ We have had to create an assembler in the meta-compiler, but
\ the assembler has not been made to be part of the base
\ image. Making assemblers for new platforms is actually one
\ of the areas Forth excels in, along with poking and playing
\ around with the underlying hardware.
\
\ If we wanted to, we could make an assembler with target
\ definitions of assembler words like "iJMP", "HALT", "LOAD",
\ and the like, along with the assembler versions of the
\ control structures.
\
\ We do implement an editor, for editing Forth blocks, in a
\ section towards the end of this book.
\
\ Now for the words themselves.
\
\ "get-order" and "set-order" do as their names suggests,
\ they retrieve and set the search order, or the vocabularies.
\ As the search order is an array containing up to eight items
\ (and could be higher on other Forth systems) the words
\ retrieve and accept a variable number of items on the stack,
\ along with a count of those items. Cells in the search order
\ with a zero value are treated as unused, so will not be
\ pushed onto the stack. "set-order" accepts a special value
\ for the number of items on the stack, if it is -1 then
\ "set-order" becomes the equivalent of the Forth word
\ "only", in that only the root vocabulary will be set.
\
\ Some example uses of "set-order":
\
\	-1 set-order
\	root-voc forth-wordlist 2 set-order
\
\ Note that "set-order" will not check whether or not you
\ have specified multiple vocabularies that happen to be
\ the same, so:
\
\	root-voc root-voc 2 set-order
\
\ Will add two copies of the root vocabulary into the search
\ order, which has negative utility. "words" will show two
\ copies of each word. There is no reason to do this, but
\ the situation is not checked for, which is why "+order"
\ and "-order" tend to be more useful, these are only defined
\ as part of the meta-compiler as they are non-standard words,
\ and putting them in the "system" vocabulary would defeat the
\ point, as you could not use them to access them.
\

: get-order ( -- widn...wid1 n : get current search order )
  context
   \ next line finds first empty cell
   #0 >r begin dup @ r@ xor while cell+ repeat rdrop
  dup cell - swap
  context - 2/ dup >r 1- s>d if -50 lit throw then
  for aft dup @ swap cell - then next @ r> ;
:r set-order ( widn ... wid1 n -- : set current search order )
  \ NB. Uses recursion, however the meta-compiler does not use
  \ the Forth compilation mechanism, so the current definition
  \ of "set-order" is available immediately.
  dup #-1 = if drop root-voc #1 set-order exit then
  dup #vocs > if -49 lit throw then
  context swap for aft tuck ! cell+ then next #0 swap ! ;r

\ "forth-wordlist" contains the standard Forth words,
\ excluding the root Forth words, as mentioned, and "system"
\ contains non-standard Forth words used in the implementation.
\

:r forth-wordlist {forth-wordlist} lit ;r ( -- wid )
:r system {system} lit ;r ( -- wid )

\ "forth" sets the search order to only include the root
\ and Forth vocabularies.
\

:r forth root-voc forth-wordlist 2 lit set-order ;r ( -- )

\ This version of "only" is implemented as "-1 set-order",
\ but it could be implemented other ways, such as
\ "root-voc 1 set-order", or by directly modifying the
\ search order array.
\
\ Typical usage as is mentioned:
\
\	only forth definitions
\
\ It is possible to call just "forth", instead of "only".
\

:r only #-1 set-order ;r ( -- : set minimal search order )

\ ".id", an internal word, is used to print out the word header
\ in a word definition. We cannot just use "count type" as we
\ need to mask off special word behavior bits stored in the
\ length byte. We usually only want to search for words, and
\ thus have the string we want with no need to print it out,
\ but for error handling and implementing the word "words", we
\ will need to print out this field.
\

:s .id ( pwd -- : print word )
  nfa count 1F lit and type space ;s

\ "words" loops through all of the vocabularies in the search
\ order, and then goes down the linked list in each of those
\ vocabularies, and prints out the words in those vocabularies.
\
\ The topmost bit in the length field is used as a "hidden"
\ flag, so if set we do not show the word.
\
\ The word is just one loop within the other, it uses
\ "get-order" to retrieve each of the vocabularies and the
\ number of them instead of examining the "context" array
\ itself.
\

:r words ( -- )
  cr get-order begin ?dup while swap ( dup u. ." : " ) @
  begin ?dup
  while dup nfa c@ 80 lit and 0= if dup .id then @
  repeat ( cr )
  1- repeat ;r

\ "definitions" make the first vocabulary in the search order
\ the one in which we add newly defined words to. It is
\ possible to add definitions into a vocabulary that is not
\ even on the search order by using "definitions" then removing
\ that vocabulary from the search order.
\
\ We might want to modify the search order without changing
\ which vocabulary we add definitions to, which is why this
\ word and mechanism is provided.
\
\ This also explains the usage of "definitions" in the phrase
\ "only forth definitions".
\
: definitions context @ set-current ; ( -- )

\ # Defining new words
\
\ Any programming language needs methods to create new
\ functions, and we now have the mechanisms to build a word
\ set that can do that.
\
\ Obviously we know that ":" and ";" can be used to create
\ new functions, you would not have been able to get very
\ far in this book having not know that. When we call ":" it
\ does several things, it parses the next word in the input
\ stream, creates a header for that word, and then makes it
\ so that everything we type in (baring "immediate" words) is
\ compiled into the dictionary instead of being executed.
\
\ The word ":" does not do the work of compiling, the
\ interpreter does, ":" does however signal to the interpreter
\ to go into compilation mode by setting the state variable
\ to -1, ";", an immediate word, sets it back.
\
\ ":" must be careful when adding a word into the dictionary,
\ more accurately, it is ";" that actually links the word into
\ the dictionary after ":" has created a header for it and left
\ the word address on the stack for ";" to pick up. The reason
\ is that when creating a new word, if there is a previously
\ defined word with the same name that is the one that must
\ be compiled into the dictionary, not the address of the new
\ word, so the new word definition cannot be visible to the
\ Forth interpreter until the conclusion of the word
\ definition. This seems like it would prevent recursion, which
\ is why the "recurse" word was made. ":" sets a variable
\ called "{last}" as well, this will be used in "recurse" as
\ it contains the location of the last defined word, well,
\ really it points to the word that is *currently being
\ defined*, knowing this, "recurse" can compile a jump to the
\ correct place.
\
\ A little compiler security is added, the constant $BABE is
\ pushed to the stack by ":" and checked by ";", if it is not
\ found then an error is thrown and the word is not linked into
\ the dictionary.
\
\ ";" must also terminate a word definition, it compiles an
\ exit instruction into the dictionary following the word
\ body. Note that it is also a compile only word, in some other
\ Forth implementations it has been given some command mode
\ semantics as well, but not in this one.
\
\ "?unique", "?nul", "?len" are three words that do some tests,
\ "?unique" is for a warning (the only one in this Forth
\ implementation) telling us if a word has been defined already
\ in the word-list or vocabulary (this guide uses the terms
\ interchangeably), "?nul" prevents zero length definition
\ words from being made. "?len" checks the length of a word to
\ make sure it is not too long, the length of a Forth word is
\ stored in the lower five bits of the first byte in the Name
\ Field, the other three being used for flags, if it is too
\ long, an exception is thrown.
\
\ With that compiler security and those words, most problems
\ can be found quite easily. They are not expensive to check
\ for.
\
\ "word" is general purpose parsing word that takes a character
\ to parse as an argument on the stack, and then parses out
\ that string from the input string. It then packs that string
\ into a counted string at the current dictionary location and
\ provides a pointer to it. This is useful for ":" and a few
\ other words, but we should be careful using it, it writes
\ to something that soon will be overwritten if we are not
\ careful.
\
\ ":noname" can be used to create anonymous functions,
\ functions with no-name and can only be referred to by a
\ execution token. ":noname" leaves an execution token on the
\ stack and it must cooperate with ";" so ";" does not attempt
\ to link something without a word header into the dictionary,
\ it does that by making sure ":noname" is given a 0 instead
\ of a word address which is treats specially and does not
\ link that into the dictionary. It must also make sure to
\ push the right constant to the stack as well. It is much
\ simpler than ":" as it does not have to deal with adding
\ new words into the dictionary.
\
\ Two new words which use "word" will also be defined, "char"
\ and "\[char\]", they both do a similar thing in that they
\ parse the next word in the input stream and extract the
\ first character from it, the rest is discard. "char" pushes
\ the value to the stack whilst "\[char\]" compiles a constant
\ containing the character into the dictionary. "\[char\]" is
\ immediate, and intended to be used from within a word
\ definition.
\
\ This short word-set gives us the power to define new words,
\ it took a fair amount to get us to this point, but this
\ is another milestone in making a Forth interpreter.
\

: word #1 ?depth parse here dup >r 2dup ! 1+ swap cmove r> ;
:s ?unique ( a -- a : warn if word definition is not unique )
 dup get-current (search) 0= if exit then space
 2drop {last} lit @ .id ." redefined" cr ;s ( b -- b )
:s ?nul dup c@ if exit then -10 lit throw ;s ( b -- b )
:s ?len dup c@ 1F lit > if -13 lit throw then ;s ( b -- b )
:to char bl word ?nul count drop c@ ; ( "name", -- c )
:to [char] postpone char =push lit , , ; immediate
:to ;
  BABE lit <> if -16 lit throw then ( check compile safety )
  =unnest lit ,                     ( compile exit )
  postpone [                        ( back to command mode )
  ?dup if                           ( link word in if non 0 )
    get-current ! exit              ( this inks the word in )
  then ; immediate compile-only
:to :   ( "name", -- colon-sys )
  align                 ( must be aligned before hand )
  here dup              ( push location for ";" )
  {last} lit !          ( set last defined word )
  last ,                ( point to previous word in header )
  bl word ?nul ?len ?unique ( parse word and do basic checks )
  count + h lit ! align ( skip over packed word and align )
  BABE lit              ( push constant for compiler safety )
  postpone ] ;         ( turn compile mode on )
:to :noname here #0 BABE lit ] ; ( "name", -- xt )

\ A few more words, "'" is an immediate word that attempts to
\ find a word in the dictionary (and throws an error if one
\ is not found), it then calls "literal" on the execution token
\ to resolve the compile/command behavior of the word. "'"
\ either pushes the execution token of a word when in command
\ mode, or compiles it into the dictionary in compile mode.
\
\ When in command mode, "2 2 ' + execute" is the same as
\ "2 2 +".
\
\ "compile" is a word that skips over the next compiled word
\ in the instruction stream and instead compiles that word into
\ the dictionary. It has some restrictions on what it can be
\ used on, for example it will not work on immediate words as
\ instead they will have already been executed, and it will not
\ work on literals, as they are not words (and they take up
\ two cells). "compile" does its job by looking at the return
\ stack value, copying it, and manipulating it.
\
\ A contrived example usage is:
\
\	: x compile + ; immediate
\	: y 2 2 + ;
\	: z 2 2 x ;
\
\ In this case "y" and "z" are equivalent words, they both
\ compute "4" at runtime. A simpler way of explaining "compile"
\ is "compile compiles the next word into another word via
\ return stack manipulation".
\
\ Some Forth implementations return a zero value when a word
\ is not found, which makes the word "defined" redundant,
\ however this behavior is not standard between all Forth
\ programs and should not be relied upon.
\
\ "recurse" is used to perform recursion. It does this by
\ looking at the last defined word and then compiling a
\ reference to it in the dictionary, which is why it needs
\ to be "immediate". Notice that it used "compile," and not
\ ",". The word "recurse" is needed because the definition
\ of a new word is not available until the definition of that
\ word is completed with a ";", meaning that word is
\ unavailable from within itself, unless by another mechanism.
\ We have already encountered that behavior, it allows the
\ redefinition of words, for example we could instead a
\ temporary debug version of "+" into source code with
\ ": + .s + ;" and the previous definition of "+" would be
\ used within the new definition.
\
\ "colorForth", a weird variation of Forth by the creator of
\ the language, Chuck Moore, uses recursion far more often. In
\ this Forth it is used sparingly, and during meta-compilation
\ the word is not needed at all (as the new definition of a
\ word is immediately available from within that word).
\
\ A word called "tail" is sometimes defined, which instead of
\ performing a call to word in which it is defined, does a
\ jump instead, named after a common optimization called
\ "tail call optimization". Some Forth implementations do
\ this automatically and in a more generic way, replacing
\ calls with jumps where possible, this means less stack is
\ used and jumps are often quicker than calls. One way Forth
\ implementations do this is by making a more intelligent
\ version of ";", which examines the previously compiled word
\ in the dictionary (and must be away of special case caused
\ by control structures and literals) and replaces a call to
\ a word with a jump to it.
\
\ "toggle" is a useful utility word, it toggles the bits at
\ an address given a bit mask. The word is defined only for
\ the next word, "hide". "hide" does what it says on the tin,
\ it hides a word given its name. A hidden word cannot be
\ currently unhidden, for that to be the case a reference
\ to the hidden word would need to be returned. "hide" can be
\ used to clean up the dictionary, it does not deleted a word
\ definition, so uses of that word will still work, but it
\ does render it invisible to the search routines. The topmost
\ bit of the NFA byte is reserved for the "hidden" bit, which
\ the search order words will take into account.
\
\ Some older Forth words had a word called "smudge", which
\ operated in a similar fashion, toggling a bit in the Name
\ Field Address, but operated on the previously defined word
\ like "recurse". This could be defined with:
\
\	:s smudge {last} lit @ nfa 80 lit swap toggle ;s
\
\ This was used to hide and unhide a word definition during
\ its creation to implement the word hiding feature described
\ with "recurse", this Forth implementation does it slightly
\ differently.
\

:to ' bl word find ?found cfa literal ; immediate
: compile r> dup [@] , 1+ >r ; compile-only ( -- )
:to recurse {last} lit @ cfa compile, ; immediate compile-only
:s toggle tuck @ xor swap ! ;s ( u a -- : toggle bits at addr )
:s hide bl word find ?found nfa 80 lit swap toggle ;s

\ # Control Structures
\
\ No programming language is complete without control
\ structures. Note that GOTO is missing, eForth also uses the
\ odd, but simple to implement, mechanism for definite loops
\ called "for...aft...then...next", which will need more
\ explanation. The other control structures are standard Forth
\ words. Also note that all the words defined here are
\ "immediate" and "compile-only" words. They push variables
\ to the variable stack during compilation, so the compiler
\ security within ":" and ";" can also detect mismatched
\ control structures.
\
\ Missing from this Forth are the looping mechanism "do...loop"
\ and its variants. This is one of the main stumbling blocks
\ for porting ANS Forth (one of the main Forth standards) code
\ to eForth.
\
\ Forth does not have a fixed grammar, it starts off with a
\ very simple one, but words can be added that do arbitrary
\ parsing, it is possible to add words that implement
\ arbitrary control structures as well, however the normal,
\ less arbitrary ones will do first. It should be said that
\ Forth is a very malleable language, however much less so
\ than Lisp, which offers a far more structured approach.
\
\ The constructs we will make are:
\
\	begin...again
\	begin...until
\	begin...while...repeat
\	if...then
\	if...else...then
\	for...next
\	for...aft...then...next
\
\ "if...then" and "begin...until" are some of the simplest
\ constructs, understanding these constructs really helps
\ in understanding how more complex compilers work, at least
\ for the code generation phase.
\
\ Both "if...then" and "begin...until" consist of a single
\ jump. Let us start with an example of "if":
\
\	: example if 1 . cr then ;
\
\ Which will compile to something like this:
\
\	EXAMPLE:
\		0: opJumpZ
\		1: 6
\		2: opPush
\		3: 1
\		4: address of "."
\		5: address of "cr"
\		6: opExit
\
\ The address written down the side are not realistic ones,
\ just illustrative ones. The purpose of this function is to
\ print out the number "1", followed by a new line, if, and
\ only if, the function is provided with a non-zero value.
\
\ The jump destination for "opJumpZ" is in the next cell,
\ it jumps to the exit, as there is nothing after the "then".
\
\ The "if" word, an immediate word that executes even in a
\ word definition, compiles the "opJumpZ" instruction into
\ the dictionary, but how does it know the jump destination?
\ We do not know where to jump to until the corresponding
\ "then" statement is reached, so "if" cannot know, and it
\ does not know. Instead it compiles a zero into the dictionary
\ after the "opJumpZ" which will later be *patched* by "then".
\ That means "then" needs to know the location that "if"
\ wrote a zero into the dictionary, to facilitate that, "if"
\ pushes the location to the stack, which "then" will consume.
\ All "then" has to do is patch the location, no extra code
\ is generated.
\
\ Thus, an "if" statement consists of one instruction, and
\ two cells, an "opJumpZ" and a jump destination. "opJumpZ"
\ is the exact instruction we need, it only jumps over the
\ if clause if given a zero.
\
\ Note in the definition of "then" that jump destination is
\ a cell address, not a byte address, in our example "6" is
\ a cell address, the Forth address would be "12". Hence the
\ division by 2.
\
\ Given the information about how "if...then" work, you can
\ then work out how the other, simple, constructs work, such
\ as "begin...until", and "begin...again", both of which have
\ an easier job than "if...then" as no patching of placeholders
\ is needed.
\
\ What is interesting is how "postpone" is used, and how new
\ words like "while" and "repeat" interact with the previously
\ defined words. "for...aft...then...next" will also need
\ explanation as it uses "opNext", which needs describing.
\
\ One common way in Forth to create new control structures,
\ which are all immediate words, is to call "postpone" on the
\ ones that already exist, these are often mixed with other
\ words that manipulate the locations those words push onto the
\ variable stack, or compile other words into their target.
\ This is a semi-portable method of doing this, as the Forth
\ implementations do not have to leave the same number of items
\ on the stack, nor do they have to have exactly the same
\ meaning.
\
\ TODO: More explanation
\

:to begin align here ; immediate compile-only
:to until =jumpz lit , 2/ , ; immediate compile-only
:to again =jump  lit , 2/ , ; immediate compile-only
:to if =jumpz lit , here #0 , ; immediate compile-only
:to then here 2/ swap ! ; immediate compile-only
:to while postpone if ; immediate compile-only
:to repeat swap postpone again postpone then ;
    immediate compile-only
:to else =jump lit , here #0 , swap postpone then ;
    immediate compile-only
:to for =>r lit , here ; immediate compile-only
:to aft drop =jump lit , here #0 , align here swap ;
    immediate compile-only
:to next =next lit , 2/ , ; immediate compile-only

\ # Create, DOES>, and other special Forth words
\
\ "create" and "does>" are sometimes called the jewels of
\ Forth, they allow the creation of words which can in turn
\ create new words. They are often used to make data-structure
\ related words, for example a word set for making and
\ manipulating arrays, which are not a native Forth construct.
\
\ "create"/"does\>" words are separated into an action 
\ performed during the creation of the new word, and the 
\ behavior of the new word at runtime. The word pair is used 
\ heavily during meta-compilation to make new words that do 
\ something with the target image. They are also some of the 
\ most difficult words to explain, as in this situation we are 
\ creating a set of words the will be used to create another 
\ set of words.
\
\ Included in this section is a word for manipulating words
\ defined with "create"/"does\>", called "\>body", and words
\ which use "create"/"does\>", such as "variable" and 
\ "constant".
\
\ When "create" is run it expects the name of a word to follow
\ in the input stream, it then creates a new word in the target
\ dictionary which when that newly created word is run pushes
\ a variable onto the stack which is the address of the 
\ location after the newly defined word, allowing you to
\ allocate memory after the word (for example, for an array)
\ after the newly defined word. The interesting thing about
\ words that have been created is that the default action to
\ push an address can be changed to something else with the
\ "does\>" word. There are a number of internal words which
\ are not very useful to anyone other than the person defining
\ the words here, such as "(var)" and "(const)".
\
\ 
\

:s (var) r> 2* ;s compile-only
:s (const) r> [@] ;s compile-only
:s (marker) r> 2* dup @ h lit ! cell+ @ get-current ! ;s
   compile-only
: create postpone : drop postpone [ compile (var)
   get-current ! ;
:to variable create #0 , ;
:to constant create cell negate allot compile (const) , ;

: >body cell+ ; ( a -- a : move to a create words body )
:s (does) r> r> 2* swap >r ;s compile-only
:s (comp) r> {last} lit @ cfa ! ;s compile-only
: does> compile (comp) compile (does) ;
   immediate compile-only

\ # Forgetting words
\
\ "marker" is a word with caveats aplenty, at least this
\ implementation of it, however it is a very useful word
\ when debugging new code interactively. It is a defining
\ word, so it requires a name of a new word, it then creates
\ a word that when called deletes every word in the dictionary
\ defined after and the new word and then deletes itself, it
\ reclaims the dictionary space.
\
\ This allows us to define a word, find problems with it,
\ and redefine it, without creating multiple definitions of
\ the same word.
\
\ An example usage might be:
\
\	marker xxx
\	: ahoy cr ." BYE" ;
\	words
\	xxx
\	words
\	: ahoy cr ." HELLO" ;
\
\ Creating a marker called "xxx", so if we make a mistake
\ making a new word, in this case making the string "ahoy"
\ print out the wrong message, then we can go back and change
\ it without cluttering up our environment.
\
\ Now for the caveats, the newly defined word should not be
\ used whilst changing the vocabularies used.
\

:to marker last here create cell negate allot compile
    (marker) , , ; ( --, "name" )


\ These target only words, defined with ":to", are both
\ immediate and compile-only. They are defined here because
\ the "compile" word needs defining first. They are all defined
\ this way because they manipulate the call stack, it is
\ possible to define a callable version of a word like "r>",
\ but it will be less efficient as it has to deal with its
\ own return value, there is also little value (negative even)
\ of being able to call these words in command mode, if they
\ are called in command mode the likely outcome is that the
\ system will crash.
\
\ All of these words compile a jump to a machine instruction
\ that does the appropriate action, which is the reason the
\ word needs to be immediate.
\
\ Remember how Forth vocabularies behave and whether the target
\ word is visible within its word definition. Recursion is not
\ being done here, this has been mentioned multiple times,
\ because it is important to understand and once it has, it
\ just clicked, beforehand it looks like the code could not
\ possibly work. I will be more explicit.
\
\	:to rp! compile rp! ; immediate compile-only
\           (1)         (2)
\
\	(1) : Newly defined header, not visible within the
\	      body of the newly defined word.
\	(2) : Points to the Forth Virtual Machine instruction,
\	      which has the following definition;
\	      ":a rp! tos {rp} MOV tos {sp} iLOAD --sp ;a"
\
\ As we have discussed how each of these instructions work,
\ we will not discuss what they do.
\

:to rp! compile rp! ; immediate compile-only
:to rp@ compile rp@ ; immediate compile-only
:to >r compile opToR ; immediate compile-only
:to r> compile opFromR ; immediate compile-only
:to r@ compile r@ ; immediate compile-only
:to rdrop compile rdrop ; immediate compile-only
:to exit compile opExit ; immediate compile-only

\ TODO: Describe string words

:to ." compile .$
  [char] " word count + h lit ! align ; immediate compile-only
:to $" compile ($)
  [char] " word count + h lit ! align ; immediate compile-only
:to abort" compile (abort)
  [char] " word count + h lit ! align ; immediate compile-only

\ These words add comments to the Forth interpreter, and a
\ word that prints out the contents of the comment. These are
\ parsing words, they modifying the input stream in someway,
\ these just modify it to discard it. They do it in different
\ ways.
\
\ "(" and ".(" both look for a single ")" character in the
\ input stream, which they use "parse" to do, feeding it a
\ ")" character to look for. "(" just drops the result of
\ parse, and ".(" uses "type" to print it out. It will not
\ work across lines, not in this Forth at least, in other
\ Forth implementations it might do, making it work for
\ multi-line comments would run its simplicity.
\
\ All of the comment words are immediate, so they can execute
\ within a word, otherwise "(" and ".(" would get compiled in
\ and the comment would as well (most likely it would cause
\ an error instead).
\
\ The word ")" does nothing, it is not even meant to be a
\ "nop", or No-Operation, it is meant to be compiled out,
\ this is because sometimes comments are used to comment out
\ code that is added back in for quick testing, for example:
\
\	: example ." HELLO " ( ." WORLD" ) ;
\
\ If we want to enable "WORLD" to be printed out, we need
\ to remove the "(":
\
\	: example ." HELLO " ." WORLD" ) ;
\
\ It is annoying however to remove the ")", especially if we
\ want to add the "(" back in after testing, but there is no
\ need to remove the ")" even temporarily as it does nothing
\ and is not compiled into the word "example".
\
\ "\\" works differently, it is meant to discard all the
\ input until the end of the line, and does this by
\ manipulating the "\>in" variable that the Forth interpreter
\ uses to keep track of where it is parsing in the input line,
\ by setting it to the end of the line, it means the rest of
\ the line is skipped. One extra complication is comments in
\ Forth blocks, a 1024 byte buffer broken into 16 lines of
\ 64 bytes, this is solved by in the implementation of "load",
\ which arranges for evaluation to occur on a line by line
\ basis, meaning "\\" does not have to be aware of the block
\ words.
\
\ A word not defined, but that you might find useful, is for
\ conditional compilation similar to
\ "\[if\]...\[else\]...\[then\]" but much simpler to implement.
\ That word is "?\\", it is a non-standard word, it can be
\ defined as follows:
\
\	: ?\ 0= if postpone \ then ; immediate
\
\ Usage:
\
\	0 ?\ .( ALPHA )
\	1 ?\ .( BRAVO )
\ 	( Only "BRAVO" is printed )
\
\

:to ( [char] ) parse 2drop ; immediate ( c"xxx" -- )
:to .( [char] ) parse type ; immediate ( c"xxx" -- )
:to ) ; immediate ( -- )
:to \ tib @ >in ! ; immediate ( c"xxx" -- )

\ "postpone" is a word that can be confusing, we have been
\ introduced to the concept of command mode and compile mode,
\ and know that in command mode numbers are pushed to the
\ stack and words are executed, but in compile mode numbers
\ and words are compiled into a word definitions body with
\ the exception of immediate words, which are executed anyway.
\
\ However the word "postpone" seems to carve an exception to
\ that exception, it takes the next work in the input stream
\ and compiles that word into the dictionary regardless of
\ whether it is an immediate or normal word. We have just
\ seen an example of this, in the definition of a new form
\ of conditional comment:
\
\	: ?\ 0= if postpone \ then ; immediate
\
\ "postpone" is called and compiles and instance of an
\ immediate word, "\\", into the dictionary. The reason this
\ can happen is two fold, only the Forth interpreter loop logic
\ itself takes into account the "immediate" status of a word,
\ which is done later on by the word "interpret", and
\ secondly, "postpone" itself is an immediate word, it looks
\ at the next word in the input stream, attempts to find that
\ word, and if it finds it, compiles that word into the
\ dictionary, it does not care about the immediate status of
\ word because it never checks it.
\
\ "postpone" is used to compile words that are usually
\ immediate into the dictionary, so that they can be executed
\ within the word definition.
\
\ We can use "postpone" to create new control structures in
\ a portable way across Forth implementations, for example.
\ Our Forth version of "if" uses the instruction "opJumpZ",
\ but other Forth implementations running on different
\ platforms might not have a similar instruction, so they could
\ implement "if" differently. So we do not have to take that
\ into account, if we want to use "if" we can call "postpone"
\ on it. Now let us give a concrete example, lets imagine we
\ want to make a control structure called "unless", it does
\ the exact opposite of "if", it executes its clause only
\ if given a zero. We can do this in the following way:
\
\ 	: unless compile 0= postpone if ; immediate
\
\ And it can be used with "then", just like "if":
\
\	: example unless ." HERE" then ;
\	0 example
\	( prints "HERE" )
\	1 example
\	( prints nothing )
\
\ In this example we do not need to know how "if" works on
\ this platform, but we can reuse it to do what we want.
\

:to postpone bl word find ?found cfa compile, ; immediate

\ "immediate" is a word that makes the last defined word
\ immediate, that is it will execute when in compile mode
\ instead of being compiled, as we know. All it has to do is
\ set a flag in the header for the last defined word to mark
\ it as being immediate, that is why it "immediate" goes after
\ the definition of a word, as it modifies the header of an
\ already existing word.
\
\ "last" gets us a pointer to the last defined word, naturally,
\ "nfa" moves us to the Name Field Address, which contains a
\ counted string with a modification, the byte used to indicate
\ the length of the Forth word is used for multiple purposes,
\ as word names can only be 32 characters in length on this
\ platform the upper bits of the count byte can be used for
\ flags. Care must be taken to mask off the lower bits when
\ word length is needed however. The 6th bit is used for
\ the "immediate" bit. It is the word "interpret" that looks
\ at this bit, "immediate" just needs to set it in the word
\ to make "interpret" treat it at immediate.
\
:to immediate last nfa @ 40 lit or last nfa ! ;

\ # Some Programmer Utilities
\
\ Nearly every Forth comes with a built in decompiler, that
\ can take a Forth word and produce and show how that Forth
\ word is put together. They can be quite advanced or be very
\ bare-bones, this is one of the more bare bones
\ implementations, it would not take that much to improve it,
\ but we are short of space.
\
\ It is interesting to think that if the decompiler was made
\ good enough then this source file could dispensed of.
\
\ The name of this decompiler is called "see", this one tries
\ to dump the numeric contents of each cell within a word, it
\ determines the end of a word when it either hits the value
\ for the exit function, or until it reaches the end of the
\ dictionary pointer.
\
\ A more advanced decompiler could look print out the word
\ header, whether the word is "compile-only", "immediate", or
\ even if it is hidden, analyze each cell to determine whether
\ it is a call or a VM instruction (and print out the word name
\ if it is a call), and decompile calls to "opJumpZ", "opUp",
\ "opNext", "opJump" and "opPush", all of which
\ contain data and not instructions in the cell after them.
\
\ However, as we have the source here, not much is gained
\ from this, it would just be an intellectual exercise.
\

:to see bl word find ?found cr ( --, "name" : decompile  word )
  begin dup @ =unnest lit <>
  while dup @ . cell+ here over < if drop exit then
  repeat @ u. ;

\ "dump" is another utility, it dumps out a section of memory
\ cell by cell. Much like the output from "see" the
\ implementations differ greatly in the quality of the output
\ they produce, some print a neat hex dump side by side with
\ the characters as well. This implementation however is as
\ simple as can be, it prints everything on a single row,
\ and unlike most implementations of "dump" it is done in the
\ current output radix, which means that the "base" variable
\ can used to change what output is displayed.
\
\ In this case the simplicity is a virtue, it means this
\ version of dump can be used for more purposes than most
\ ones designed to directly produce human readable output
\ in a neat, but limited, format. For example, it could be
\ used as the inner loop of a more complex dump program, or
\ for data exchange, for debugging sections of memory in
\ different bases such as binary, and more!
\
\ A program loader could be designed with a similar structure
\ to this word, one that accepted an address to write to and
\ a length and allowed a user to input numbers. The output
\ "." would have to be replaced with a word for getting a
\ number from the input and writing it to a memory location.
\

:to dump aligned ( a u -- : display section of memory )
  begin ?dup
  while swap dup @ . cell+ swap cell -
  repeat drop ;

\ "cksum" is used by the system to make sure the image has
\ not been corrupted or tampered with. It does it in a limited
\ way as self-modifying code means that it cannot make sure
\ that the Forth virtual machine has not been tampered with.
\ This is unfortunate, one way to get around this would be to
\ write this in assembly and do the checking much earlier in
\ the boot sequence. As this is not the case, only the Forth
\ code is checked. To make the check-sum quick enough on the
\ SUBLEQ machine no bitwise operations are used, just addition,
\ this makes for a pretty poor check-sum but is better than
\ nothing.
\
\ Another idea that could be combined with assembly version
\ of "cksum" that would run before the Forth VM is to make
\ the image decompress itself using an algorithm like
\ LZSS <https://en.wikipedia.org/wiki/LZSS>, the image is
\ highly compressible and LZSS is one of the simplest
\ dictionary compression based routines available. It is
\ doable and could save 30% or more on the image size, which
\ preliminary testing confirms.
\
\ Huffman coding might be simpler to achieve with a similar
\ result.
\
\ Image encryption or obfuscation could also be done, as an
\ anti-tampering measure, which might be useful to do if the
\ program was used as part of a game or a puzzle.
\
\ The meta-compiler calculates the checksum later on, and sets
\ a known location during boot to the checksum value. It is
\ checked as part of the initial Forth word "quit", which is
\ not standard behavior but this Forth does it.
\

:s cksum aligned dup C0DE lit - >r ( a u -- u )
  begin ?dup
  while swap dup @ r> + >r cell+ swap cell -
  repeat drop r> ;s

\ # Conditional Evaluation
\
\ The Forth input stream is malleable, we have seen with words
\ like "see", or "create", and in the parsing section, that
\ we can manipulate the program input at runtime, we can do
\ this in arbitrary ways, adding comments is one example. It
\ is technically possible to make a word that implements
\ another language, for example BASIC or C, that would still
\ technically be part of the Forth language, however we have
\ a far less ambitious goal - the creation of a word-set for
\ conditional evaluation of code.
\
\ This is will be like the "#ifdef" facility provided by the
\ C pre-processor, it will allow us to compile different code
\ for differing platforms, for example compiling one set of
\ words for gforth, as opposed to eForth (which we have already
\ seen in practice).
\
\ Another word will be defined which will help in the detection
\ of which system we are compiling for, that word is called
\ "defined". It parses the next word in the input stream and
\ pushes a boolean value, true if the word is defined, and
\ false if not. The word "eforth" is defined later, and is only
\ defined on this eForth system by default (of course, you
\ could define the word on a gforth or other Forth
\ implementation, but why would you?).
\
\ The words "\[if\]", "\[then\]" and "\[else\]" will be defined
\ for conditional compilation. They are analogous to the
\ "if...else...then" construct defined earlier.
\
\ Most of the work is done by "\[else\]", which "\[if\]" calls,
\ it parses tokens from the input stream and discards them
\ until "\[then\]", or another "\[else\]" is encountered.
\
\ They are all immediate words, so they can be used within
\ word definitions. "\[if\]" pulls a value off the stack, it
\ conditionally calls "\[else\]", if the value is false it
\ will skip to the corresponding "\[else\]", otherwise it
\ allow execution of its clause.
\
\ As "\[if\]...\[else\]...\[then\]" can span multiple lines
\ then the implementation of "\[else\]" must be sure to refill
\ the terminal input buffer once the end of the input line
\ has been reached. We do not do the same with "(", although
\ we could, for the sake of simplicity, it is also not
\ required.
\
\ To keep the implementation simple there is no compiler
\ security to ensure the construct is used in its intended
\ usage. For example:
\
\	[else] ( CODE ) [then]
\
\ Will not throw an error, the CODE section will not be
\ executed however.
\

: defined bl word find nip 0<> ; ( -- f )
:to [then] ; immediate
:to [else]
 begin
  begin bl word dup c@ while
   find drop cfa dup to' [else] lit = swap to' [then] lit = or
    if exit then repeat query again ; immediate
:to [if] if exit then postpone [else] ; immediate

\ # Time and Hacks
\
\ The Forth word "ms" is a standard extension word for delaying
\ for a specified number of milliseconds. The concept of
\ "sleep" is actually a complex one as it interacts with
\ operating systems, the threading model, I/O blocking, and
\ there are different optimizations that can be performed. The
\ concept of real-time systems must be introduced and hard
\ vs soft real-time systems. Saying "this function will sleep
\ for X milliseconds" does not fully answer questions around
\ sleep. There are also problems of jitter and drift, all
\ in all "sleep" is a complex topic.
\
\ However, given that the underlying SUBLEQ machine does not
\ have a method for determining the actual time (ie. A hardware
\ timer), how is it that this implementation provides an "ms"
\ implementation? The answer is by doing it poorly! There are
\ a few ways this implementation could be improved even on this
\ system, but there would still be trade-offs.
\
\ This implementation of "ms" uses busy waiting to approximate
\ a timer. The idea is simple, given a certain clock rate
\ which will assume is constant (it is not, unless you are
\ running a SUBLEQ machine on an FPGA or custom CPU) then
\ a busy loop will take a fixed amount of time to execute.
\
\ The time to execute depends on that clock rate, it will
\ vary from machine to machine, it will also vary depending
\ on how the busy loop is implemented, which we control. To
\ account for differences between machines a "calibration"
\ value is chosen that can be changed, on my machine a
\ calibration value of "F00" (Hex) causes the implementation
\ of "ms" when given 1000 (Decimal) to wait approximately one
\ second, which is correct. On your machine it could be vastly
\ different.
\
\ There is one other complication, this implementation of
\ "ms" calls "pause", if we are running other threads then
\ we will want them to proceed and do work whilst we are
\ waiting in this thread (if we do not want that behavior
\ we can always drop into single task mode with the word
\ "single".
\
\ This means our delay loop is actually going to count the
\ time executed in the thread it is run, and not the actual
\ time elapsed. There are ways of improving this, but the
\ easiest would be to implement a hardware time, to keep
\ things simple we will not do this.
\
\ You should be aware of this if you plan on using "ms" for
\ more serious timing needs.
\

: ms for pause calibration @ for next next ; ( ms -- )

\ The words "bell", "csi", "page" and "at-xy" make some
\ assumptions which may not be valid on all output devices,
\ it is assumed that if a user is typing commands into the
\ program from a terminal, that the terminal supports basic
\ ANSI terminal escape codes used to change things like
\ cursor position, or character color. "bell" outputs the
\ ASCII BEL character, which depending on your terminal, may
\ or may not do anything, it is meant to grab the users
\ attention.
\
\ "csi" is used to form CSI escape sequences, for example
\ the following word can be defined to change the color of
\ the terminal:
\
\	: color csi 0 u.r ." m" ;
\
\ Note that "csi" is in the system vocabulary, so it will
\ need to be loaded before this word can be defined.
\
\ The following words could then be defined:
\
\	decimal
\	: red 31 color ;
\	: green 32 color ;
\	: blue 34 color ;
\	: reset 0 color ;
\
\ This will most likely work under Unix systems, and quite
\ likely fail if done under Windows CMD.EXE (although a program
\ called ANSICON can remedy that), as windows terminal program
\ does not support ANSI escape codes, or it depends on the
\ version of windows and settings.
\
\ Standard Forths contain the words "at-xy" and "page" for
\ controlling the cursor position and clearing the screen
\ respectively, so these words are provided. Some interesting
\ games can be made just these two, limited terminal games,
\ but games nonetheless. If non-blocking input is implemented
\ games then a Tetris or Snake clone can be made.
\
\ The first column and row in "at-xy" is "1" and not "0".
\

: bell 7 lit emit ; ( -- : emit ASCII BEL character )
:s csi 1B lit emit 5B lit emit ;s ( -- : ANSI Term. Esc. Seq. )
: page csi ." 2J" csi ." 1;1H" ( csi ." 0m" ) ;
: at-xy base @ decimal ( x y -- : set cursor position )
   >r csi #0 u.r ." ;" #0 u.r ." H" r>
   base ! ;

\ # Forth Blocks
\
\ Forth blocks are a neat concept, they are a minimal way of
\ giving access to mass storage that any mass storage system
\ can provide, no file system is required, just the ability
\ to read and write blocks of data to non-volatile storage.
\
\ It is also a largely obsolete mechanism for interfacing with
\ mass-storage, even most Forth systems now provide a similar
\ interface to the C file words, "fopen", "fread", and the
\ like.
\
\ Unfortunately the SUBLEQ machine in a quest for simplicity
\ provides no mechanism for saving to non-volatile storage, it
\ would be easy enough to add and interface with, but that
\ will not happen, there is little need for it.
\
\ There are some Forth implementations that instead map the
\ memory available to the implementation to blocks. This is
\ what this implementation does, the idea behind the BLOCK
\ word-set is that the user does not have to worry about the
\ details of how it works, as far as the code is concerned, it
\ could be writing to flash, to a hard-drive, or just to
\ memory like this implementation does.
\
\ The block word-set has been added because it can be used as
\ a basis for text editing (see the block editor described
\ later) and if mass storage were to be added, this word-set
\ could be used to access it, the code using it would not have
\ to be modified, just the implementation of the word "block".
\
\ It would be interesting to build a simple file system on top
\ of the Forth Block mechanism, and a set of DOS like utilities
\ for accessing and executing files on it. But that is a
\ different story. It could also be used to simulate a simple
\ DOS like operating system within this Forth system. The
\ file system would also be quite portable, but limited.
\
\ It is possible to use the block system to map arbitrary bits
\ of memory to different things, for example, we could map
\ blocks 0-63 to our main memory, and 64-127 to EEPROM, if we
\ had some available, and 128-144 to ROM, if we had that
\ available. It is an abstraction that has many potential uses.
\
\ Anyway, on to the block word-set. A Block consists of a
\ 1024 byte chunk of memory stored in a buffer, for text
\ editing purposes it is usually treated as 16 lines each of
\ 64 bytes in length. The block can be transferred in and out
\ of memory, all systems with block storage have at least one
\ block buffer, most have two or more. Management of blocks
\ is semi-automatic. If a block is selected it might kick out
\ an existing buffer and reuse it, so all addresses to block
\ storage might become invalidated on any use of a word like
\ "block". If a block is marked as dirty and is to be evicted
\ from the block buffers it is written back to mass storage
\ before another block is loaded, otherwise the changes to
\ that block are discarded. Note that because this
\ implementation of the block word set just maps 1024 chunks
\ of memory to each block the changes are always reflected
\ and there are no block transfers.
\
\ That is the rough description of how things work in a normal
\ Forth block system. The three main words are "block",
\ "update" and "flush". The latter two words do nothing on
\ this system.
\
\ The word "block" is the core of the block system, given a
\ block number, "k", it will return an address to a block
\ buffer (or thrown an exception if there is an error), it
\ will perform the task of checking if the current block is
\ dirty, of potentially writing the changes back, of loading
\ the new block, of assigning the new block buffer. It does
\ all of the work. It also sets the variable "blk" to the
\ block buffer id given to "block", which contains the latest
\ loaded block. If the block number is the same as the one
\ loaded then it does nothing but return a pointer to the
\ already loaded block. This version of "block" also checks
\ that there is at least one value on the stack and it also
\ causes "pause", described in the multi-threading chapter.
\
\ The word "update" marks the last loaded block as dirty,
\ allowing the block system to determine what to write back,
\ and "flush" writes back all dirty buffers back to mass
\ storage and frees all buffers. As mentioned they do nothing
\ (or very little) on this system. They should still be used
\ within code that uses these block words for portability
\ reasons.
\
\ A few other words are defined in this chapter, "list",
\ "blank", "b/buf". It would also be a good time to describe
\ how the block system interacts with "source-id", when we
\ are executing code from a block (with "load", defined later)
\ "source-id" is set to -1 (it is set to -1 when any string
\ is being evaluated also), when reading from a terminal
\ "source-id" is set to zero.
\
\ "b/buf" is a constant which contains the number of bytes in
\ a block, on most Forth system it is 1024 bytes, it does
\ differ on some.
\
\ "list" takes a block number and displays that block. It
\ prints out a block in the default text format, with
\ 16 lines of text per block. This version of list also
\ prints out the line numbers for each line. It is used by
\ the block editor to display blocks.
\
\ "blank" is defined as a helper word, used to format blocks
\ for editing, it blanks a section of memory by writing spaces
\ (also known as "blanks") to that section of memory. It can
\ be used to format a block with the following expression:
\
\	47 block b/buf blank
\
\ If we wanted to format block "47" for editing.
\
: b/buf 400 lit ; ( -- u )
: block #1 ?depth dup blk ! A lit lshift pause ; ( k -- u )
: flush ( save-buffers empty-buffers ) ; ( -- )
: update #-1 {dirty} lit ! ; ( -- )
: blank bl fill ; ( a u -- : blank an area of memory )
: list ( k -- : list a block )
   page cr         ( clean the screen )
   dup scr ! block ( update "scr" and load block )
   F lit for       ( for each line in the block )
     F lit r@ - 3 lit u.r space    ( print the line number )
     3F lit for count emit next cr ( print line )
   next drop ;

\ # The Read-Eval-Loop
\
\ While there a few more sections of optional material, this
\ one finally puts everything together and produces the Forth
\ interpreter loop, were we read in a line, parse it, execute
\ it can catch any errors. A few new support words will be
\ defined, but the main ones are "evaluate", "load",
\ "task-init", "quit" and "(cold)". It has an odd name, but
\ traditionally the main interpreter loop is called "quit"
\ in Forth systems.
\
\ The goal of this section is to put the pieces together to
\ make that word "quit".
\
\ "get-input" and "set-input" are used in words that need
\ to change the input stream (for example, changing the
\ interpreter so it reads from the terminal, or so it can
\ evaluate a string). It puts an implementation defined number
\ of items on the stack, so do not rely on it putting the same
\ number of items on the stack between different Forth
\ implementations or even different versions of this Forth.
\

: get-input source >in @ source-id <ok> @ ; ( -- n1...n5 )
: set-input <ok> ! {id} up ! >in ! tup 2! ; ( n1...n5 -- )

\ "ok" is responsible for printing out the Forth prompt after
\ execution of each line, it will only print out if we are
\ in command mode. The "ok" prompt can be too noisy sometimes,
\ this Forth is careful not to print out a header, unless an
\ option is set to do so, which allows the system to be used
\ as a pipe in a Unix command line. The "ok" prompt can be
\ turned off before it outputs a single "ok", with the first
\ line in this file.
\
\	defined eforth [if] ' nop <ok> ! [then]
\
\ "ok" is not called directly, but is stored as an execution
\ token in "<ok>".
\

:s ok state @ 0= if ."  ok" cr then ;s ( -- )

\ Now we are getting somewhere, "eval" goes through each word
\ in a line until there are no more and executes "interpret"
\ for each word, it is sure to check the stack depth after
\ each call to "interpret" in an attempt to provide some kind
\ of limited error detection.
\
\ It also prints out "ok", by execution the contents of
\ "<ok>", as just mentioned.
\
:s eval
   begin bl word dup c@ while
     interpret #0 ?depth
   repeat drop <ok> @ execute ;s ( "word" -- )

\ "evaluate" takes a string and evaluates that string, it
\ is careful to preserve the input state prior to evaluation
\ so that it can be restored after, it also catches any error
\ that might occur before re-throwing them so it can make sure
\ to always restore that input state. "source-id" will be set
\ to -1 for the duration of the evaluation.
\
\ Now that we have "evaluate" we can use to extend the block
\ words...
: evaluate ( a u -- : evaluate a string )
  get-input 2>r 2>r >r        ( save the current input state )
  #0 #-1 t' nop lit set-input ( set new input )
  t' eval lit catch           ( evaluate the string )
  r> 2r> 2r> set-input        ( restore input state )
  throw ;                     ( throw on error )

\ "load" is one of the missing words needed by our block
\ wordlist, it evaluates a forth block, treating each 64 bytes
\ as a single line. That has the consequence that comments
\ end at those line boundaries.
\
\ * "line" is used to get a line from a block.
\ * "loadline" is a common factor of "load", used to evaluate
\   a single line.
\ * "load" evaluates an entire block.
\
\ The other two words missing from this version of forth, that
\ could easily be added later, are "thru" and "-->", "thru"
\ loads and executes a range of blocks inclusively, and "-->"
\ when used within a block discards the rest of the block from
\ being executed and continues execution from the next block,
\ which can be chained together.
\
:s line 6 lit lshift swap block + 40 lit ;s
:s loadline line evaluate ;s
: load #0 F lit for
   2dup 2>r loadline 2r> 1+ next 2drop ; ( k -- )

\ Two words that allow the user of the environment to get
\ information about the system, "eforth", which has uses in
\ conditional evaluation, pushes the version number of the
\ forth interpreter. The version number has the format of
\ MAJOR.MINOR version numbers, with the MAJOR number stored
\ in the upper byte, and the MINOR in the lower.
\
\ "info" is optionally printed out at start up, a bit has
\ to be enabled in the "{options}" variable for that to happen,
\ it is not necessary, but it means that information about
\ the project is stored with it.
:r eforth 0107 lit ;r ( --, version )
:s info cr ( --, print system info )
  ." Project: eForth v1.7 " ( here . ) cr
  ." Author:  Richard James Howe" cr
  ." Email:   howe.r.j.89@gmail.com" cr
  ." Repo:    https://github.com/howerj/subleq" cr
  ." License: The Unlicense / Public Domain" cr ;s

\ "task-init" is a poor Forth function, it really should be
\ split up into words that deal with the different aspects of
\ setting up a task.
\
\ The word must set up the user variables to point to the
\ default execution tokens, or to their default values. It
\ also sets the variables for the saved register locations,
\ and an initial execution token of "bye", which should be
\ replaced before the task is executed or the system will halt.
\
\ Note that this word also handles the "{options}" bit, the
\ first bit, which turns off echoing of the user input if
\ it is set, turning echoing on or off can be useful depending
\ on your terminal settings. If your terminal echos back what
\ you have written then you will want echoing off otherwise
\ each character written will be doubled up, if it does not
\ then you will want it on, otherwise your terminal will be
\ eerily silent and you will not be able to see what you have
\ typed in.
\
\ The original eForth system factored out setting up with
\ input/output hooks with different words, some of which were
\ used for file transfer or for normal input, which could be
\ replicated, but will not be for now. It also had a word
\ called "io!" for initializing the I/O channels, which might
\ be needed on some systems, but not this one.
\
\ This word should be treated as a list of things to be
\ initialized. That list includes:
\
\  1. Setting up the task linked list pointer.
\  2. Setting the initial execution token.
\  3. Setting up empty return and variable stacks.
\  4. Making the saved top of the stack empty for the task.
\  5. Setting the default input and output radix (to decimal).
\  6. Setting up the I/O behavior.
\  7. Making the input buffer position ">in" is zero.
\  8. Putting default values in a few variables like "dpl".
\  9. Making the terminal input buffer point to the right place
\     within the tasks memory.
\ 10. Putting the thread into the right "state", into command
\     mode by default, it is not expected that multiple threads
\     will be defining words (which will cause problems) or
\     executing commands (which may cause problems), but it is
\     setup just in case.
\
\ TODO: Talk about XIO, HAND, ...
\

\ h: quite? 4 cpu@ and 0<> ; ( -- t : in quite mode? )
\ : io! preset fallthrough;  ( -- : initialize I/O )
\ h: console ' rx? <key> ! ' tx! <emit> ! fallthrough;
\ h: hand
\   quite? 0= ' (ok) and
\   ' drop ' tap
\   raw? if 2drop
\     ' emit  ' ktap
\   then fallthrough;
\ h: xio  ' accept <expect> ! <tap> ! <echo> ! <ok> ! ;
\ h: pace 11 emit ;
\ : file ' pace ' drop ' ktap xio ;

:s task-init ( task-addr -- )
  {up} lit @ swap {up} lit !
  this 2/ {next-task} up !
  t' bye lit 2/ {ip-save} up ! ( Default execution token )
  this =stksz        lit + 2/ {rp-save} up !
  this =stksz double lit + 2/ {sp-save} up !
  #0 {tos-save} up !
  decimal
  t' key? lit <key> !
  t' (emit) lit <echo> ! ( Default: Echoing of input on )
  ( Turn off echoing if the 1st bit set )
  {options} lit @ lsb if to' drop lit <echo> ! then
  t' (emit) lit <emit> !
  t' ok lit <ok> !
  t' (literal) lit <literal> !
  #0 >in ! #-1 dpl !
  this =tib lit + #0 tup 2! \ Set terminal input buffer loc.
  postpone [
  {up} lit ! ;s

\ "ini" initializes the current task, the system brings itself
\ up at boot time, and where "ini" is executed is critical,
\ before it is called none of the words that rely on the
\ execution tokens being set can be executed without causes
\ the system to reboot. It is called from "(cold)", the first
\ forth word to run.
\
:s ini {up} lit @ task-init ;s ( -- : initialize current task )

\ "quit" is the heart of the Forth system, it is responsible
\ for fetching and evaluating each line of Forth code. It
\ also contains the error handler of last resort, which catches
\ all errors that have not been caught further up the call
\ stack and resets the system with some sensible defaults.
\ The only exception when it comes to exceptions is the ABORT
\ signal, which causes the interpreter to halt.
\
\ The actual interpreter loop is quite simple, get a line with
\ "query", execute the line with "eval" under "catch", and
\ handle any errors, loop until satisfied.
\

: quit ( -- : interpreter loop )
  begin
   query t' eval lit catch
   ?dup if
     dup space . [char] ? emit cr #-1 = if bye then ini then
  again ;

\ "(cold)" is the first word that gets executed, it performs
\ the task of continuing to setup the environment before
\ normal running. It must:
\
\ 1. Setup which word lists are used ("only forth definitions")
\ 2. Initialize the current task with "ini".
\ 3. Check the boot options in the "{options}" variable
\    a. The first bit is checked within "ini", the 4th within
\       the word "key".
\    b. Check 3rd bit and call "info" if set
\    c. Check 2nd bit and perform a checksum over the
\       Forth image, it then toggles the 2nd bit making
\       it so the image is not checked again (as adding
\       new definitions modifies the image, calling "(cold)"
\       again would mean the checksum would fail. If the
\       checksum fails it prints and error messages and halts.
\ 4. Calls "quit" to enter into the Forth interpreter loop.
\
\ And that completes the Forth boot sequence.
\
:s (cold) ( -- : Forth boot sequence )
  only forth definitions ( un-mess-up dictionary / set it )
  ini
  {options} lit @ 4 lit and if info then ( display info? )
  {options} lit @ 2 lit and if ( checksum on? )
    primitive lit @ 2* dup here swap - cksum  ( calc. cksum )
    check lit @ <> if ." cksum fail" bye then ( oops... )
    {options} lit @ 2 lit xor {options} lit ! ( disable cksum )
  then quit ;s ( call the interpreter loop AKA "quit" )

\ # Cooperative Multitasking
\
\ As mentioned before the best text on implementing
\ multitasking within Forth is available at:
\
\ <https://www.bradrodriguez.com/papers/mtasking.html>
\
\ Some of the concepts have already been talked about
\ previously, namely USER variables, the task area, the "pause"
\ word and "task-init". This section brings everything together
\ and makes those the multitasking usable. It adds words to
\ create new named tasks, to add that task to the list of tasks
\ to execute, to wait on a "signal" sent from another task,
\ to send messages between tasks, and to turn multitasking
\ on or off.
\
\ The I/O words "key", "emit", "block", and also "ms" all call
\ the word "pause", which should be bore in mind when
\ programming. "key?" does not call "pause" however, but its
\ blocking or non-blocking behavior should be considered.
\
\ So, what is the point of multithreading, there has been a lot
\ of talk about it within this document, allusions to it, but
\ no rationale. It might seem like quite the complication to
\ the implementation, and it certainly is if you never plan
\ on using it. It was actually relatively easy to implement,
\ especially compared to implementing the basic operators from
\ scratch with no debugging facilities. If you never use
\ another thread or never plan on using one, you can skip all
\ this and you do not need to concern yourself with it.
\
\ The point of multithreading is to divide up the processor
\ time so that multiple threads of execution can share the
\ same processor and pretend they are executing one a single
\ processor. Imagine if you have a computer that has multiple
\ users on it, you do not want to have to wait for one user
\ to log off the server before you can log into it, you both
\ want to log into the same server at the same time.
\
\ Another example is a computer game, you often have multiple
\ different input methods such as mouse, keyboard, the network,
\ and multiple different output methods, sound, graphics, and
\ again the network. You do not want the game to halt whilst
\ you play a sound, nor do you want the graphics subsystem to
\ stop when it is waiting for a keyboard press, you want
\ everything to appear so it is being computed all at the same
\ time, even on single CPU core systems (which are getting are
\ getting rarer nowadays even in the embedded computer space).
\
\ Threading and different threading models "solve" this, and
\ do so in different ways. The cooperative threading model is
\ the simplest, the easiest to use and the easiest to get
\ right. It does have a disadvantage in that a single thread
\ of execution can hold up and block the entire system from
\ running, and each thread must manually have "pause" functions
\ inserted in it for this to work.
\
\ As we control the virtual machine, it would be possible to
\ alleviate some of this by making it do the "pause"
\ functionality every X instructions completed, but that would
\ slow down normal execution.
\
\ An example program, that uses the tasks (make sure the system
\ vocabulary is loaded prior to executing this):
\
\ 	task: rx
\ 	task: tx1
\ 	task: tx2
\
\ 	: .tx1 begin [char] X rx send 100 ms again ;
\ 	: .tx2 begin [char] Y rx send 200 ms again ;
\ 	: .rx begin
\ 	  multi receive single . space emit cr again ;
\
\ 	single
\ 	' .tx1 tx1 activate
\ 	' .tx2 tx2 activate
\ 	' .rx rx activate
\ 	: schedule begin pause again ;
\ 	multi schedule
\
\ This creates three new threads, two of which transmit a
\ a single character to a third, "tx1" and "tx2" transmit
\ "X" and "Y" respectively to task "rx", which they do at
\ different rates. Notice that all three threads are executing
\ in an infinite loop, so without the threading mechanism
\ "rx" receiving and printing the characters would be
\ impossible. Also note, "ms", "emit" all call "pause",
\ allowing the threads to switch between themselves.
\
\ "multi" and "single" are used in the "rx" thread to
\ to prevent other I/O operations from interfering with
\ the output of the output of "rx". Note that ".", "space"
\ and "cr" all call "emit", so also yield to other threads.
\
\ The interpreter can run at the same time, and you can
\ type things into it, although the program will need
\ modification ("schedule" should be removed), as will the
\ "{options}" variable. It is also sensitive to the
\ environment, so it is best to leave the program as is.
\
\ To prevent activation of the threads immediately, single
\ mode is entered, and only when everything is setup do we
\ enter multi mode again, "schedule" is used to keep the
\ interpreter task busy.
\
\ Now for a more detailed description of the words themselves.
\
\ "task:" is used to create a named task, it allocates memory
\ for the tasks buffers, stacks, and USER variables. It does
\ little else however, and the task is not yet ready to run
\ as it has nothing to execute. It can now be referred to
\ however, allowing other tasks to send messages to it, or
\ at least the code written to do so as it will not be able
\ to receive them yet.
\
\ "activate" takes an execution token, initializes the task
\ memory, and assigns that execution token to the task, the
\ task will start running if we are in "multi" mode, so use
\ the word "single" prior to stop the task from immediately
\ launching.
\

:s task: ( create a named task )
  create here 400 lit allot 2/ task-init ;s
:s activate ( xt task-address -- : start task executing xt )
  dup task-init
  dup >r swap 2/ swap {ip-save} lit + ! ( set execution word )
  r> this @ >r dup 2/ this ! r> swap ! ;s ( link in task )

\ "wait" and "signal" belong as a pair, they do not require
\ a tasks to work, and work on arbitrary memory locations,
\ but are used to perform synchronization between tasks. One
\ task can "wait" on a variable to become non-zero, repeatedly
\ calling "pause" until it is (because another task or even
\ a hardware register has set to be non-zero).
\
\ "signal" is used to set that memory location to a non-zero
\ value, these two are wrapped up in functions because the
\ implementation of these two functions might be more complex
\ on multitasking systems that perform preemption, otherwise
\ "signal" could largely be replaced with "!". "signal" by
\ default sets the variable to be waited on to the current
\ task address (all task addresses being non-zero).
\

:s wait ( addr -- : wait for signal )
  begin pause dup @ until #0 swap ! ;s
:s signal this swap ! ;s ( addr -- : signal to wait )

\ "single" and "multi" turn off and on multitasking. This
\ can be used to prevent other tasks from interrupting the
\ currently running task, useful for preventing I/O from
\ multiple threads from interfering with each other.
\

:s single #1 {single} lit ! ;s ( -- : disable other tasks )
:s multi  #0 {single} lit ! ;s ( -- : enable multitasking )

\ "send" and "receive" are a pair of words like "wait" and
\ "signal", they are used to send a message from one thread
\ to another. "send" takes a message, which is a single cell,
\ it could be a pointer, a variable, an execution token, so
\ long as it is a single cell, it also takes a task address
\ to send to.
\
\ "receive" waits by repeatedly pausing until a task address
\ is written into its task inbox, "{sender}". It then retrieves
\ the value in "{sender}" and in "{message}".
\
\ This is a simple way to do inter-thread communication.
\

:s send ( msg task-addr -- : send message to task )
  this over {sender} lit +
  begin pause dup @ 0= until
  ! {message} lit + ! ;s
:s receive ( -- msg task-addr : block until message )
  begin pause {sender} up @ until
  {message} up @ {sender} up @
  #0 {sender} up ! ;s

\ That completes the multitasking section, there are only a
\ few words on top of the base that the Forth system provides,
\ but they are enough for most tasks.
\
\ Possibly missing are words to put to sleep and wake up a
\ task, if you need them, implement them yourself.
\

\ # Forth Text / Block Editor
\
\ I love Forth blocks, they are simple to implement, understand
\ and are very easy to implement. They are of course an
\ obsolete way of doing things, suitable for a bygone era only,
\ or perhaps on some limited embedded systems.
\
\ We can make a very small, and fairly usable, text editor
\ based on our "persistent" storage mechanism. It packs a lot
\ of functionality into a very short space, and it does this
\ by reusing a lot of functionality built into Forth. The
\ Forth Block mechanism takes care of storage, retrieval and
\ swapping in and out dirty blocks, the Forth interpreter
\ can handle command parsing and line based input. We just
\ need to meld these concepts into an editor. We even have
\ a word for executing Forth blocks.
\
\ For more information on Forth block editors the links:
\
\ - <http://tunes.org/wiki/block_20editor.html>
\ - <https://wiki.c2.com/?ForthBlocks>
\
\ Are useful, alternatively search for "FORTH BLOCK EDITOR",
\ in your favorite internet search engine.
\
\ To make sure that these newly defined commands do not
\ interfere with the main vocabulary we will put them in
\ a vocabulary called "editor". We will be defining short
\ one letter commands that are easy to type, so it is important
\ that we do this.
\
\ The editor we will be making will not be a normal text editor
\ like you will be familiar with, it will be simpler but more
\ wasteful.
\
\ We have been given a series of 1024 byte blocks to deal with,
\ we will treat each block as 16 lines, each line being 64
\ bytes long. The entire block when formatted for editing will
\ contain white-space. This makes the job of the editor much,
\ much easier. There are no variable length strings to make,
\ manage or delete, and all string handling routines will be
\ trivial.
\
\ The editor is only 16 lines long itself, so it will fit
\ perfectly within a single block.
\
\ To use the editor type "editor", this will replace the
\ current vocabulary so only the editor commands are visible,
\ they are; "q", "?", "l", "e", "ia", "i", "w", "s", "n", "p",
\ "r", "x", and finally "d". Some of the words are not
\ strictly necessary in this editor, but they are not large
\ and will be useful in other systems.
\
\ Here is a short description of each of the commands:
\
\ "editor", enter editor mode
\ "q", quit editor
\ "?", display current block number
\ "l", list current forth block
\ "e", execute current forth block
\ "ia", insert line of text into line at position
\ "i", insert line of text into line
\ "w", list commands
\ "s", save and flush
\ "n", go to next block and list it
\ "p", go to previous block and list it
\ "r", list a specific block
\ "x", erase the screen, replacing it with spaces
\ "d", delete a line, it takes a number as an argument
\
\ Missing are words to perform searching, replacing, swapping
\ lines. They are all easy to add, but are not necessary. The
\ fact that execute, "e", calls "q" might cause problems when
\ trying to put words into different vocabularies, but it is
\ not an insurmountable problem. Also "e" might clash with
\ the hexadecimal value for the number 14, in this Forth it is
\ not a problem as hexadecimal numbers use uppercase only, and
\ this Forth is case sensitive.
\
\ The editor can also be used to enter data with a series
\ of commands into blocks to create databases if file
\ redirection is used.
\
\ An example of its use:
\
\	editor
\	x
\	0 i ( HELLO WORLD PROGRAM VERSION 3.4 )
\	1 i : ahoy cr ." HELLO, WORLD" ;
\	2 i ahoy
\	l
\	e
\
\ This will make the block containing the following text:
\
\	( HELLO WORLD PROGRAM VERSION 3.4 )
\	: ahoy cr ." HELLO, WORLD" ;
\	ahoy
\
\ Which when run should print out:
\
\	HELLO, WORLD
\
\ It takes a little getting used to, but is not that difficult.
\ Feel free to edit the commands into something more suitable
\ to what you prefer, or add new ones.
\
\ The only complex word is "ia", which also forms the basis
\ for "i", it inserts a line of text into a line at a location,
\ after making sure there are at least two items on the stack,
\ it does not range checking on those variable unfortunately,
\ a common "feature" of Forth. It looks at the Terminal Input
\ Buffer, with ">in" and "tib", copying the results into the
\ location within the block specified, and then skips over
\ the line so it is not executed. It also calls "update",
\ which marks the current block as dirty (as it has just been
\ modified), this means the block is automatically saved when
\ moving to the next or previous block. Of course, as there
\ is no mass storage in this SUBLEQ machine, so nothing is
\ written to it, however it is a nice feature for portabilities
\ sake.
\
\ Most of the other commands are simple, they manipulate the
\ screen pointer by adding to it, or retrieving it.
\
\ A lot of the words call "l", to list the current screen,
\ so after modification or changing of the screen variable
\ "scr" the user does not have to type "l" themselves.
\

: editor {editor} lit #1 set-order ; ( Tiny BLOCK editor )
:e q only forth ;e ( -- : exit back to Forth interpreter )
:e ? scr @ . ;e ( -- : print block number of current block )
:e l scr @ list ;e ( -- : list current block )
:e e q scr @ load editor ;e ( -- : evaluate current block )
:e ia 2 lit ?depth 6 lit lshift + scr @ block + tib >in @ +
   swap source nip >in @ - cmove tib @ >in ! update l ;e
:e i #0 swap ia ;e ( line --, "line" : insert line at )
:e w words ;e ( -- : display block editor commands )
:e s update flush ;e ( -- : save edited block )
:e n  #1 scr +! l ;e ( -- : display next block )
:e p #-1 scr +! l ;e ( -- : display previous block )
:e r scr ! l ;e ( k -- : retrieve given block )
:e x scr @ block b/buf blank l ;e ( -- : erase current block )
:e d #1 ?depth >r scr @ block r> 6 lit lshift + 40 lit
   blank l ;e ( line -- : delete line )

\ # Last word defined
\
\ Finally "cold" is defined, the last word we will define,
\ it deserves its own section. The word "cold" restarts the
\ system by executing the execution token stored in "{cold}".
\
\ Note the "2\*"! Cold has to be executed by the Forth VM
\ as well, so it contains a cell address, meaning it has to
\ be multiplied by two before hand.
\

: cold {cold} lit 2* @ execute ; ( -- )

\ # Image Generation
\
\ Everything is done! No more Forth to write! We just need to
\ set up a few hooks, make sure the dictionary is in the right
\ state, output the image, and exit the interpreter. That
\ can be done in a few lines.
\
\ The word "save-target" is called, this spits the output onto
\ the standard output stream as a series of space delimited
\ 16-bit signed numbers.
\
\ ".end" puts the Forth interpreter back into a normal state,
\ so we can then call "bye", that "bye" will be the one which
\ exits the interpreter and not the new one we have just
\ defined. That completes everything.
\
\ As the SUBLEQ machine has no way of writing files to anything
\ and only one method of input or output, the build process
\ for a new image looks like this (on a Unix system):
\
\	./subleq old-image.dec < subleq.fth > new-image.dec
\
\ "old-image.dec" contains an eForth interpreter, "subleq.fth"
\ contains this file, and "new-image.dec" is the new eForth
\ image we have produced in this file.
\

t' (cold) half {cold} t!      \ Set starting Forth word
atlast {forth-wordlist} t!    \ Make wordlist work
{forth-wordlist} {current} t! \ Set "current" dictionary
there h t!                    \ Assign dictionary pointer
primitive t@ double mkck check t! \ Set checksum over VM
atlast {last} t!              \ Set last defined word
save-target                   \ Output target

\ With a slightly modified version of "save-target" and
\ the Forth word ".\\" which echos only the current line it
\ is possible to output a C program that contains the entire
\ interpreter, this would look something like this:
\ 
\ 	.\ #include <stdio.h> /* eForth for 16-bit SUBLEQ */
\ 	.\ int main(void){short p=0,m[65536] = {
\	save-target  \ Output target
\ 	.\ }; while(p>=0){int
\ 	.\ a=m[p++],b=m[p++],c=m[p++];
\ 	.\ a<0?m[b]=getchar():b<0?putchar(m[a]):(m[b]-=m[a])
\ 	.\ <=0?p=c:0;}}
\
\ The process could also be done as part of the build system
\ as well.
\
\

.end                          \ Get back to normal Forth
bye                           \ Auf Wiedersehen

As we have called "bye", we can write what we want here without
it being run.

\
\ # References:
\
\ - <https://esolangs.org/wiki/Subleq>
\ - <https://wikipedia.org/wiki/Forth_(programming_language)>
\ - <https://wikipedia.org/wiki/Threaded_code>
\ - <https://github.com/howerj/embed>
\ - <https://github.com/howerj/forth-cpu>
\ - <https://github.com/samawati/j1eforth>
\ - <https://www.bradrodriguez.com/papers/>
\ - 8086 eForth 1.0 by Bill Muench and C. H. Ting, 1990
\ - <https://www.bradrodriguez.com/papers/mtasking.html>,
\   For multitasking support
\ - <https://forth-standard.org/standard/block>,
\   For the block word-set, which is partially implemented.
\ - <https://github.com/howerj/subleq-js>
\

\ # Appendix
\
\ ## About the author
\ 
\ Hello! Instead of writing about myself awkwardly in the
\ third person with words such as "The author has..." I
\ have decided to down a more informal route. I, Richard
\ James Howe, have been an embedded software engineer in the
\ automotive sector writing safety critical code in C and
\ tooling in Perl/C#, I currently work in the smart energy
\ sector. I have a degree in electronic engineering and have
\ had a few internships related to that. I also studied in
\ Germany for a year as part of the Erasmus program.
\ 
\ I have interests in programming languages, operating
\ systems, starting businesses, travelling, going to galleries
\ and pretending I know about art, reading and would like to
\ branch out into other hobbies such as wood-working and
\ web-development (unrelated).
\ 
\ I have never programmed Forth (or VHDL) professionally,
\ only having done so fun.
\ 
\ I currently reside in the UK.
\
\ The book was written for fun, it has next to no real 
\ technical value whatsoever but the entire SUBLEQ eForth
\ system was a lovely puzzle to crack. My next big project will
\ probably be my own Unix operating system in a Pascal like
\ language of my own design.
\
\ ## Fully Portable SUBLEQ machine written in C
\
\ This section contains a fully portable version of SUBLEQ
\ machine, written in C, it is not minified, or obfuscated, but
\ designed to as portable as possible. It does not even rely
\ on twos compliment, doing all arithmetic with unsigned
\ numbers instead. It should work on all platforms that have
\ the fixed width typedefs.
\
\	#include <stdint.h>
\	#include <stdio.h>
\	#define SZ   (32768)
\	#define L(X) ((X)%SZ)
\	int main(int s, char **v) {
\		static uint16_t m[SZ];
\		uint16_t pc = 0;
\		for (int i = 1, d = 0; i < s; i++) {
\			FILE *f = fopen(v[i], "r");
\			if (!f)
\				return 1;
\			while (fscanf(f, "%d", &d) > 0)
\				m[L(pc++)] = d;
\			if (fclose(f) < 0)
\				return 2;
\		}
\		for (pc = 0; pc < SZ;) {
\			uint16_t a = m[L(pc++)];
\			uint16_t b = m[L(pc++)];
\			uint16_t c = m[L(pc++)];
\			if (a == 65535) {
\				m[L(b)] = getchar();
\			} else if (b == 65535) {
\				if (putchar(m[L(a)]) < 0)
\					return 3;
\				if (fflush(stdout) < 0)
\					return 4;
\			} else {
\				uint16_t r = m[L(b)] - m[L(a)];
\				if (r & 32768 || r == 0)
\					pc = c;
\				m[L(b)] = r;
\			}
\		}
\		return 0;
\	}
\
\
\ ## Non-blocking input SUBLEQ machine written in C
\
\ Unfortunately there is no way to do non-blocking Input and
\ Output in pure C with the standard input and output streams,
\ to remedy that a SUBLEQ machine that can run on most systems
\ has been provided. It should work under most Unix systems,
\ and under Windows. However, it has only been tested on Linux
\ and Windows.
\
\ It is largely the same as the portable version but provides
\ a function for enabling and performing a non-blocking read
\ on the standard input channel, or on Windows a read from
\ the terminal input. The character retrieval function now
\ returns negative if there is no input, instead of on End
\ Of File.
\
\ Consult the "{options}" variable for how this interacts with
\ the eForth image, as the default image exits when negative
\ is returned from the character input.
\
\ Non-blocking output is not done (it is assumed output happens
\ instantly).
\
\ One other change is that hitting ESC causes the interpreter
\ to exit, CTRL-D will no longer work nor can End-Of-File be
\ detected any more.
\
\	#include <stdint.h>
\	#include <stdio.h>
\	#include <stdlib.h>
\
\	#define ESCAPE (27)
\	#define DELETE (127)
\	#define BACKSPACE (8)
\
\	#ifdef __unix__
\	#include <unistd.h>
\	#include <termios.h>
\	static struct termios oldattr, newattr;
\
\	static void restore(void) {
\		tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
\	}
\
\	static int setup(void) {
\		tcgetattr(STDIN_FILENO, &oldattr);
\		newattr = oldattr;
\		newattr.c_iflag &= ~(ICRNL);
\		newattr.c_lflag &= ~(ICANON | ECHO);
\		newattr.c_cc[VMIN]  = 0;
\		newattr.c_cc[VTIME] = 0;
\		tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
\		atexit(restore);
\		return 0;
\	}
\
\	static int getch(void) {
\		static int init = 0;
\		if (!init) {
\			setup();
\			init = 1;
\		}
\		unsigned char r = 0;
\		if (read(STDIN_FILENO, &r, 1) != 1)
\			return -1;
\		return r;
\	}
\
\	static int putch(int c) {
\		int res = putchar(c);
\		fflush(stdout);
\		return res;
\	}
\
\	static void sleep_ms(unsigned ms) {
\		usleep((unsigned long)ms * 1000);
\	}
\	#else
\	#ifdef _WIN32
\
\	extern int getch(void);
\	extern int putch(int c);
\	static void sleep_ms(unsigned ms) {
\		usleep((unsigned long)ms * 1000);
\	}
\	#else
\	static int getch(void) {
\		return getchar();
\	}
\
\	static int putch(const int c) {
\		return putchar(c);
\	}
\
\	static void sleep_ms(unsigned ms) {
\		(void)ms;
\	}
\	#endif
\	#endif /** __unix__ **/
\
\	static int wrap_getch(void) {
\		const int ch = getch();
\		if (ch == EOF) {
\			sleep_ms(1);
\		}
\		if (ch == ESCAPE)
\			exit(0);
\		return ch == DELETE ? BACKSPACE : ch;
\	}
\
\	#define SZ   (32768)
\	#define L(X) ((X)%SZ)
\	int main(int s, char **v)
\	{
\		static uint16_t m[SZ];
\		uint16_t pc = 0;
\		for (int i = 1, d = 0; i < s; i++) {
\			FILE *f = fopen(v[i], "r");
\			if (!f)
\				return 1;
\			while (fscanf(f, "%d", &d) > 0)
\				m[L(pc++)] = d;
\			if (fclose(f) < 0)
\				return 2;
\		}
\		for (pc = 0; !(pc & 32768);) {
\			uint16_t a = m[L(pc++)];
\			uint16_t b = m[L(pc++)];
\			uint16_t c = m[L(pc++)];
\			if (a == 65535) {
\				m[L(b)] = wrap_getch();
\			} else if (b == 65535) {
\				if (putch(m[L(a)]) < 0)
\					return 3;
\			} else {
\				uint16_t r = m[L(b)] - m[L(a)];
\				if (r & 32768 || r == 0)
\					pc = c;
\				m[L(b)] = r;
\			}
\		}
\		return 0;
\	}
\
\
\ ## Error Code list
\
\ This is a list of Error codes, not all of which are used by
\ the application.
\
\ 	| Hex  | Dec  | Message                               |
\ 	| ---- | ---- | ------------------------------------- |
\ 	| FFFF |  -1  | ABORT                                 |
\ 	| FFFE |  -2  | ABORT"                                |
\ 	| FFFD |  -3  | stack overflow                        |
\ 	| FFFC |  -4  | stack underflow                       |
\ 	| FFFB |  -5  | return stack overflow                 |
\ 	| FFFA |  -6  | return stack underflow                |
\ 	| FFF9 |  -7  | do-loops nested too deeply            |
\ 	| FFF8 |  -8  | dictionary overflow                   |
\ 	| FFF7 |  -9  | invalid memory address                |
\ 	| FFF6 | -10  | division by zero                      |
\ 	| FFF5 | -11  | result out of range                   |
\ 	| FFF4 | -12  | argument type mismatch                |
\ 	| FFF3 | -13  | undefined word                        |
\ 	| FFF2 | -14  | interpreting a compile-only word      |
\ 	| FFF1 | -15  | invalid FORGET                        |
\ 	| FFF0 | -16  | attempt to use 0-len string as a name |
\ 	| FFEF | -17  | pictured numeric output str. overflow |
\ 	| FFEE | -18  | parsed string overflow                |
\ 	| FFED | -19  | definition name too long              |
\ 	| FFEC | -20  | write to a read-only location         |
\ 	| FFEB | -21  | unsupported operation                 |
\ 	| FFEA | -22  | control structure mismatch            |
\ 	| FFE9 | -23  | address alignment exception           |
\ 	| FFE8 | -24  | invalid numeric argument              |
\ 	| FFE7 | -25  | return stack imbalance                |
\ 	| FFE6 | -26  | loop parameters unavailable           |
\ 	| FFE5 | -27  | invalid recursion                     |
\ 	| FFE4 | -28  | user interrupt                        |
\ 	| FFE3 | -29  | compiler nesting                      |
\ 	| FFE2 | -30  | obsolescent feature                   |
\ 	| FFE1 | -31  | >BODY used on non-CREATEd definition  |
\ 	| FFE0 | -32  | invalid name argument (e.g., TO xxx)  |
\ 	| FFDF | -33  | block read exception                  |
\ 	| FFDE | -34  | block write exception                 |
\ 	| FFDD | -35  | invalid block number                  |
\ 	| FFDC | -36  | invalid file position                 |
\ 	| FFDB | -37  | file I/O exception                    |
\ 	| FFDA | -38  | non-existent file                     |
\ 	| FFD9 | -39  | unexpected end of file                |
\ 	| FFD8 | -40  | wrong BASE in floating point convert  |
\ 	| FFD7 | -41  | loss of precision                     |
\ 	| FFD6 | -42  | floating-point divide by zero         |
\ 	| FFD5 | -43  | floating-point result out of range    |
\ 	| FFD4 | -44  | floating-point stack overflow         |
\ 	| FFD3 | -45  | floating-point stack underflow        |
\ 	| FFD2 | -46  | floating-point invalid argument       |
\ 	| FFD1 | -47  | compilation word list deleted         |
\ 	| FFD0 | -48  | invalid POSTPONE                      |
\ 	| FFCF | -49  | search-order overflow                 |
\ 	| FFCE | -50  | search-order underflow                |
\ 	| FFCD | -51  | compilation word list changed         |
\ 	| FFCC | -52  | control-flow stack overflow           |
\ 	| FFCB | -53  | exception stack overflow              |
\ 	| FFCA | -54  | floating-point underflow              |
\ 	| FFC9 | -55  | floating-point unidentified fault     |
\ 	| FFC8 | -56  | QUIT                                  |
\ 	| FFC7 | -57  | exception in tx or rx a character     |
\ 	| FFC6 | -58  | [IF], [ELSE], or [THEN] exception     |
\
\ <http://www.forth200x.org/throw-iors.html>
\
\ 	| Hex  | Dec  | Message                               |
\ 	| ---- | ---- | ------------------------------------- |
\ 	| FFC5 | -59  | ALLOCATE                              |
\ 	| FFC4 | -60  | FREE                                  |
\ 	| FFC3 | -61  | RESIZE                                |
\ 	| FFC2 | -62  | CLOSE-FILE                            |
\ 	| FFC1 | -63  | CREATE-FILE                           |
\ 	| FFC0 | -64  | DELETE-FILE                           |
\ 	| FFBF | -65  | FILE-POSITION                         |
\ 	| FFBE | -66  | FILE-SIZE                             |
\ 	| FFBD | -67  | FILE-STATUS                           |
\ 	| FFBC | -68  | FLUSH-FILE                            |
\ 	| FFBB | -69  | OPEN-FILE                             |
\ 	| FFBA | -70  | READ-FILE                             |
\ 	| FFB9 | -71  | READ-LINE                             |
\ 	| FFB8 | -72  | RENAME-FILE                           |
\ 	| FFB7 | -73  | REPOSITION-FILE                       |
\ 	| FFB6 | -74  | RESIZE-FILE                           |
\ 	| FFB5 | -75  | WRITE-FILE                            |
\ 	| FFB4 | -76  | WRITE-LINE                            |
\
\
\ ## Advanced version of "see"
\
\ This is a more advanced version of "see", it is a much better
\ decompiler, but much more complex. It could still use more
\ work, but then again any decompiler that cannot reproduce the
\ source code it is decompiling could use more work.
\
\ We start by defining "ndrop" and "within", "ndrop" is a non
\ standard but common word for removing a variable number of
\ items from the variable stack. "within" is a standard word
\ for determining whether a variable is within a certain range.
\
:s ndrop for aft drop then next ;s ( x0...xn n -- )
: within over - >r - r> u< ; ( u lo hi -- f )

\ "validate" takes a pointer to a prospective match, the "pwd"
\ *could* be a pointer to the word that CFA belongs to, if
\ we can move from the "pwd" to the "cfa" and the two addresses
\ are equal, then we have a match, that CFA belongs to that
\ PWD. If it does, then we move to the name field prior,
\ otherwise zero is returned.
\
:s validate ( pwd cfa -- nfa | 0 )
   over cfa <> if drop #0 exit then nfa ;s

\ "cfa?" goes through the linked list of words in a vocabulary
\ and attempts to find a pair of pointers for which the Code
\ Field Address (CFA) it is given that lies between those two
\ pointers. As there might be other words defined with other
\ vocabularies within that pointer range, it must then validate
\ the match by calling "validate. If found, it returns the
\ Name Field Address (NFA) of the word which matches the given
\ CFA, otherwise it returns zero.
\
:s cfa? ( wid cfa -- nfa | 0 : search for CFA in a wordlist )
  cells >r
  begin
    dup
  while
    dup @ over r@ -rot within
    if dup @ r@ validate ?dup if rdrop nip exit then then
    @
  repeat rdrop ;s

\ "name" attempts to find a Code Field Address within the
\ dictionary, it is just a loop over all of the vocabularies
\ in the dictionary for which it calls "cfa?". It uses "ndrop"
\ to get rid of the remaining items vocabularies returned by
\ "get-order" if the CFA has been found.
:s name ( cwf -- a | 0 : search for CFA in the dictionary )
  >r
  get-order
  begin
    dup
  while
    swap r@ cfa? ?dup if
      >r 1- ndrop r> rdrop exit then
  1- repeat rdrop ;s

\ "decompile" takes an instruction "u" and an address of where
\ the decompilation is currently taking place, "a". It decides
\ what to do based on the current instruction, and it might
\ modify the address give, for example by moving over an
\ operand of the given instruction. The default behavior is
\ to print the instruction out as a number and do nothing with
\ the address (the address given should already point to the
\ cell after "u").
\
\ The word is just a table actions to take if specific
\ instructions are found. For example if "=push", for "opPush",
\ is found, it must print out a string containing "push" and
\ then the contents of the next cell. Strings are only a little
\ more complex, it must print out the string and skip over that
\ string. If none of those actions match, then it attempt to
\ look up what the possible word definition is with "name", and
\ as mentioned if that fails, it just prints out "u".
\
:s decompile ( a u -- a )
  dup =jumpz lit = if drop ."  jumpz " cell+ dup @ . exit then
  dup =jump  lit = if drop ."  jump  " cell+ dup @ . exit then
  dup =push  lit = if drop ."  push  " cell+ dup @ . exit then
  dup =next  lit = if drop ."  next  " cell+ dup @ . exit then
  dup =up    lit = if drop ."  up    " cell+ dup @ . exit then
  dup to' .$ half lit = if drop ."  ." [char] " emit space
    cell+ count 2dup type [char] " emit + aligned
  exit then
  dup to' ($) half lit = if drop ."  $" [char] "
  emit space
    cell+ count 2dup type [char] " emit + aligned
  exit then
  primitive lit @ over > if ."  VM    " else
    dup name ?dup if space count 1F lit and type drop exit then
  then
  . ;s

\ These two words, "compile-only?" and "immediate?" tell us
\ whether a word is compile-only or immediate respectively,
\ all they do is analyze a specific bit a pointer to a given
\ word header. Nothing complex.
\
:s compile-only? nfa 20 lit swap @ and 0<> ;s ( pwd -- f )
:s immediate? nfa 40 lit swap @ and 0<> ;s ( pwd -- f )

\ This is the more complex version of "see", it will attempt
\ to print the code in a form that resembles the source as
\ much as possible, but it falls short of that quite a bit.
\
\ We could improve the stop conditions by using "(find)"
\ as it gives us a better range for the likely positions of
\ when a word starts and ends.
:to see bl word dup ." : " count type cr find ?found
  dup >r cfa
  begin dup @ =unnest lit <>
  while dup @ decompile cr cell+ here over < if drop exit then
  repeat drop ."  ;"
  r> dup immediate? if ."  immediate" then
  compile-only? if ."  compile-only" then cr ;


