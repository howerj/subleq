defined eforth [if] ' ) <ok> ! [then] ( Turn off ok prompt )
\ # Information
\
\ * Edition: X.X.0
\ * Project: Cross Compiler / eForth for a SUBLEQ CPU
\ * Author:  Richard James Howe
\ * Email:   howe.r.j.89@gmail.com
\ * Repo:    <https://github.com/howerj/subleq>
\ * License: The Unlicense / Public domain for code only, all
\ rights reserved for comments, the book, diagrams
\ and pictures.
\
\ # ***THIS FILE NEEDS EDITING***
\
\ THIS FILE NEEDS TO GO THROUGH THE EDITING PROCESS TO MAKE
\ A SECOND EDITION OF THIS BOOK.
\
\ ***THIS FILE NEEDS EDITING***
\ ***THIS FILE NEEDS EDITING***
\ ***THIS FILE NEEDS EDITING***
\ ***THIS FILE NEEDS EDITING***
\
\ TODO: Make "\[" and "\]" target only, use them when using
\ literals to make the code much more Forth like, e.g.
\ "2 lit" should become "\[ 2 \] literal".
\
\ # Dedication and Foreword
\
\ This book is dedicated to my wife Molly Barton, my Moll,
\ my tiny girl Persephone and has also introduced me to
\ her wonderful dog Poppy.
\
\ Please feel free to contact the author about thoughts,
\ feedback, and for any corrections.
\
\ # Introduction
\
\ This book contains an assembler for a SUBLEQ CPU, a virtual
\ machine capable of running Forth built upon that assembler,
\ a cross compiler capable of targeting the Forth VM, and
\ finally the Forth interpreter itself, based on the eForth
\ family of the Forth programming language. This system is
\ self-hosted, which means it can be used to create new,
\ modified, systems. Also contained is a description of how
\ this system works, how the internals of a Forth interpreter
\ works, and the difficulties and trade-offs involved in
\ targeting such an anemic CPU architecture, called SUBLEQ,
\ which is an esoteric, impractical, single instruction CPU. If
\ you can port a Forth to SUBLEQ, then you can port a Forth
\ implementation to anything. There is a saying about Forth,
\ "Forth is Sudoku for programmers", I think it sums up my
\ relationship with the language perfectly and this project.
\
\ This program is written in Forth entirely, and should
\ compile both under gforth (tested with version 0.7.3, under
\ both Linux and Windows) and by executing it against a
\ pre-generated eForth image running on the SUBLEQ machine.
\
\ This program and explanation is for an esoteric, oddball,
\ system, it is likely it will never be useful for
\ anything. It will also not designed for beginner programmers,
\ it would help if you had some understanding of Forth and
\ Assembly before you read this file.
\
\ The explanation for this system is tied into the order in
\ which each subsystem is defined, which I find understandable
\ but others may not.
\
\ This tutorial will use a 16-bit SUBLEQ VM written in C.
\ To interact with just the eForth interpreter running itself
\ no installation is required as there is a web-based version
\ written in JavaScript available here:
\
\ * <https://howerj.github.io/subleq.htm>
\ * <https://github.com/howerj/subleq>
\
\ ## Building This Image
\
\ To build the image you will need either gforth, or perhaps
\ easier for you, a working SUBLEQ implementation in your
\ favorite language, one is in the appendix and another will
\ be described shortly, both written in C. The C versions will
\ obviously require a C compiler.
\
\ First you will need to clone the repository available at
\ <https://github.com/howerj/subleq>.
\
\       git clone https://github.com/howerj/subleq
\       cd subleq
\
\ To build, on a Unix system:
\
\        cc subleq.c -o subleq
\        ./subleq subleq.dec < subleq.fth > new-image.dec
\
\ And on Windows:
\
\        cc subleq.c -o subleq.exe
\        subleq.exe subleq.dec < subleq.fth > new-image.dec
\
\ And with gforth on Unix and Windows:
\
\        gforth subleq.fth > new-image.dec
\
\ And to run the new image:
\
\        ./subleq new-image.dec
\
\ The command shells redirection facilities are used to make
\ up for a lack of input/output mechanisms within the SUBLEQ
\ machine, which will be discussed later.
\
\ Most modern complex programs have a system of unit tests,
\ this would have been useful in places, however instead
\ the meta-compilation system is used as a giant unit test
\ framework. If the system can compile an image "A" which can
\ compile another image "B" that is byte for byte the same as
\ image "A", then we can be pretty confident that the new
\ image is correct, or it is at least correct enough to compile
\ itself.
\
\ This is done with the following commands on a Unix system:
\
\        ./subleq subleq.dec < subleq.fth > 1.dec
\        ./subleq 1.dec < subleq.fth > 2.dec
\        diff -w 1.dec 2.dec
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
\ ## Glossary of Terms
\
\ Whilst you should be familiar with common Forth and
\ programming terms, the following terms should be describe
\ in more detail to avoid confusion.
\
\ * "Forth", the programming language that will be used
\ for cross compilation onto the target, and the language
\ that will be running on the target.
\ * "eForth", the specific version of Forth that this version
\ of Forth most resembles.
\ * "SUBLEQ machine"/"SUBLEQ VM", this is the machine that
\ the system will be running on.
\ * "VM"/"Forth VM", this is a set of functions and a small
\ bit of code that will be compiled and run upon the SUBLEQ
\ machine that will allow a Forth implementation to be hosted
\ upon the SUBLEQ machine.
\ * "compiler-security", a term used for some run time checks
\ present when compiling Forth words.
\ * "meta-compilation", another word for cross-compilation
\ but using Forth instead, distinct from the more widely
\ known Computer Science term. The reason for this difference
\ is that Forth evolved within the microcomputer scene, which
\ was very distinct from the academic scene in the 1980s and
\ prior. The term seems to have been mistranslated somewhat.
\ * "word" in the context of Forth means "A forth function",
\ the term "word" is used because a Forth function consists of
\ space delimited characters that usually form a single
\ descriptive word as a name. Words form vocabularies and
\ vocabularies form the dictionary.
\ * "cell" is used as a term because "word" is already used
\ for Forth functions. It is used to refer to a memory location
\ that is of the computers natural width, on a 16-bit machine a
\ single cell is a 16-bit location, aligned on a 2 byte
\ boundary. On this SUBLEQ machine the smallest addressable
\ unit is the cell, not the byte, byte access will have to be
\ simulated.
\ * "Target Dictionary", the dictionary in the image we will
\ be assembling, this is distinct from the words used for
\ meta-compilation, and the words made in the meta-compiler
\ used to refer to memory locations of defined words within
\ in the Target Dictionary.
\
\ The most important terms to differentiate between are
\ what is the Forth VM and what is the SUBLEQ machine. The
\ SUBLEQ machine implementation is itself a Virtual Machine
\ written in C, when using the term "VM", it will not refer
\ to the SUBLEQ machine however, but the Forth VM.
\
\ On a grammatical manner this document uses "ones compliment"
\ and "twos compliment" without the apostrophe, which is
\ perhaps incorrect, but it shall be done consistently.
\
\ ## Markdown and Formatting
\
\ The comments in this file, the long descriptions at least,
\ and not the short stack effect comments, are written with the
\ intention that they are extracted from the original
\ "subleq.fth" source and turned into a markdown file, a well
\ known format, that can be used to generate a book. If you are
\ viewing the original "subleq.fth" file then the comments will
\ contain markdown formatting, eg. less than or greater than
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
\ characters wide, and uses conventional and regular
\ naming and coding styles for Forth, with the exception that
\ all word definitions are lower case. The 64 character limit
\ is imposed to accommodate the formatting of Forth blocks,
\ there would be more to do to make it fully Forth block
\ compliant, but it is a start.
\
\ You will notice that word definitions have a regular
\ formatting when it comes to the comments, these "stack effect
\ comments" have a common structure that is sometimes broken
\ or bent where it makes sense.
\
\ An example of a stack effect comment, on a Forth definition:
\
\        : square dup * ; ( n -- n : square a number )
\
\ The stack effect comment is between "(" and ")", it has
\ the general form of:
\
\        ( stack input -- stack output : description )
\
\ The stack refers to the variable stack, and to the number of
\ items the word consumes or produces. The description is
\ usually laconic, as there is little room on screen for it
\ given historical terminal character widths, the
\ word name itself should offer some clue as to what it does,
\ and the word should be short, ideally a single line, so it
\ can be inspected to see what it *actually* does.
\
\ There is a common naming system for the arguments:
\
\        n - A signed number, takes up a single cell on the
\            stack.
\        u - Unsigned single cell number.
\        c - A single character or byte which will take up a
\            a single cell on the stack.
\        xt - an execution token, this points to a function
\            which can be executed later.
\        a - A cell address, it should be aligned to a 2 byte
\           boundary on this system.
\        b - An address that points to bytes, it will not
\           need to be aligned.
\        f - a Forth flag, 0 = false, -1 = true.
\        x y z - Used when the position of the before and
\           after picture for the cells matters more than the
\           interpretation of the cells themselves.
\        d - A signed double cell (32-bit) value.
\        ud - An unsigned double cell value.
\        wid - An address of a vocabulary or word-list.
\
\ There are a other conventions surrounding stack effect
\ comments.
\
\ For the word definition "\>r", pronounced "To R", the stack
\ effect comment shows the movement of a single cell from the
\ variable to the return stack with "R:".
\
\        ( n --, R: -- n )
\
\ Parsing words use "name" to indicate they accept a word of
\ from the input stream to use, like ":" or "variable":
\
\        ( --, "name" )
\
\ Some words return a variable number (or even more rarely
\ accept a variable number) of cells, this can be indicated
\ with "|", for example "key?":
\
\        ( -- c 0 | -1 )
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
\ it:
\
\ * Try to keep word definitions short and clean.
\ * Use two spaces where possible per indentation level
\ * Try to keep definitions on a single line.
\ * Use lower case names.
\ * Add a stack effect comment if possible.
\
\ Notice the liberal use of the word "try", do what makes sense
\ first.
\
\ ## SUBLEQ History
\
\ SUBLEQ machines belong to a class of machines called "One
\ Instruction Set Computer(s)", they only have a single
\ instruction that allows them to compute anything that is
\ computable, albeit not efficiently. SUBLEQ is possibly the
\ original OISC, which was named "URISC". The idea was to
\ provide a simple platform for those students at university
\ learning computer engineering so that they could implement
\ their own instruction set in a single course and create
\ the microcode for that platform. The system was outlined
\ in the paper "URISC: The Ultimate Reduced Instruction Set
\ Computer"  by Mavaddat, F. and Parhami, B. published in
\ October 1988.
\
\ SUBLEQ is an OISC that belongs to the category of single
\ instruction set computers built around an arithmetic
\ operation (the other two main categories include bit
\ manipulation instructions and architectures build
\ around a MOVE instruction called Transport Triggered
\ Architectures). The arithmetic architecture feel closet to a
\ real machine whilst at the same time being far away from
\ them. As you will find out, it would not take much to
\ radically improve the efficiency of the system with extra
\ instructions for bit-manipulation, perhaps even as little as
\ one (such as an instruction set that could perform SUBLEQ and
\ a NAND, or SUBLEQ and a Right Shift).
\
\ Other OISCs can be found online, such as SUBNEG (subtract
\ and branch if negative), SUBLEQ with an accumulator, SBNZ
\ (Subtract and Branch if not zero) and others. They
\ are all difficult to use, some more than others. SUBLEQ
\ appears to be the more popular of the OISCs. Those
\ instructions are the universal ones (given infinite memory).
\ There are other trivial and useless OISC systems
\ out there, for example a machine consisting of just a single
\ No-operation or NOP instruction, which we will disregarded as
\ uninteresting.
\
\ SUBLEQ, and OISC systems generally, feel like they
\ are close to and perhaps belong to the same category
\ as Esoteric Programming Languages and Turing Tar-pits,
\ languages that are technically Turing complete,
\ but are incredibly difficult to use and are only ever used
\ as a puzzle to satisfy intellectual curiosity.
\
\ ## eForth
\
\ The image we will create is a variant of Forth called
\ "eForth", what the "e" stands for is up for debate. Perhaps
\ "embedded", although there are other contenders.
\
\ There are differences between this eForth implementation and
\ standard ANS Forth implementations, such as the lack of the
\ "do...loop" construct and variants. Do not be surprised if
\ some standard words are missing or have different semantics.
\
\ The idea of eForth was to have a system that required only
\ a handful (about 30) primitives to be encoded in assembly
\ in order to create a highly portable Forth with reasonable
\ efficiency. The original eForth was coded by Bill Muench,
\ with variants and improvements subsequently being made
\ by C.H. Ting. The link <http://forth.org/eforth.html> has
\ more information.
\
\ There have been debates about the minimal viable word-set
\ in Forth, and different schemes have been proposed.
\
\ Some schemes are:
\
\ * The 1992 entry "third" by buzzard
\ <https://www.ioccc.org/years.html>. Which makes an obfuscated
\ interpreter for a Forth like language in C which can
\ bootstrap itself. It has about 15 primitives.
\ * A 3-Instruction Forth <https://pygmy.utoh.org/3ins4th.html>
\ which takes the concept in a different direction and is
\ arguably not a Forth but a way of tethering a Forth for a
\ new system.
\ * "sectorforth" is a Forth interpreter in a (512 byte)
\ boot-sector, available at
\ <https://github.com/cesarblum/sectorforth> which is very
\ platform specific and aims at a minimal number of bytes and
\ not primitives.
\
\ The three schemes, whilst impressive, are a bit too spartan.
\ It is not possible to definitively say that X number of
\ primitives is the absolute minimum built-in primitives to
\ implement a Forth, and the above three schemes have different
\ caveats and limitations (such as are those implementations
\ *real* Forth implementations, are they *viable* and *useful*)
\ which can be argued back and forth to no effect and with
\ nothing learned.
\
\ eForth aims at a minimal and viable (runs fast enough)
\ solution.
\
\ ## A little about SUBLEQ
\
\ So the goal is to port a system capable of compiling itself
\ to the SUBLEQ machine, what is SUBLEQ and why is porting to
\ it difficult? As mentioned it is a single instruction
\ machine, each instruction consisting of three operands; "A",
\ "B", and "C". Each operand is stored in a single cell, in
\ this implementation the cell size is 16-bits, which is
\ important. A SUBLEQ machine that uses sizes other than
\ this, or bignums, will not work.
\
\ The SUBLEQ Virtual Machine is written in C, and one has also
\ been written in JavaScript. It would be easy to
\ write one in VHDL and run the system on an FPGA. It is even
\ possible to build a machine capable of running eForth made
\ from discrete 7400 series Integrated Circuits, EEPROM and
\ RAM chips, although that is beyond this project.
\
\ The single instruction that is executed is:
\
\ 1. Load the contents of the cell pointed to by A
\ 2. Load the contents of the cell pointed to by B
\ 3. Subtract A from B, and store the result back to cell B.
\    If the result is less than or equal to zero, jump to the
\    location pointed to by C. Otherwise advance the program
\    counter to the next instruction (or over the three
\    operands) and execute from there.
\
\ This machine does not specify I/O, it could be memory mapped
\ but the most common is the following, by modifying
\ the above instruction slightly to deal with some special
\ values:
\
\ 1. If A is -1, or all bits set, then get a byte from the
\ input channel (or read a character from the keyboard) and
\ store it in the cell pointed to by B. Operand C is ignored,
\ and no jumps occur for either I/O instruction beyond moving
\ to the next instruction.
\ 2. If B is -1, then load the contents pointed to by A and
\ write a byte to the output channel (or display a single
\ character), again no jump is performed.
\
\ Another special case is that if the program counter goes
\ outside memory then the machine halts. On some machines this
\ only happens if the program counter goes negative.
\
\ This sort of makes most SUBLEQ implementations three
\ instruction machines, with the instructions SUBLEQ, INPUT
\ and OUTPUT, and perhaps HALT. However, that is being too
\ pedantic.
\
\ A SUBLEQ machine that has and can address infinite memory
\ memory is Turing complete. Our implementation, as all real
\ world implementations of any system, does not have infinite
\ memory, but in practical terms it means we can solve any
\ programming problem on it (modulo memory and speed
\ constraints) even if it is not an ideal system to do it on.
\
\ Assuming a twos compliment machine and 16-bit shorts, then
\ this C program will execute the image we will make:
\
\        #include <stdio.h>
\        int main(int x, char **v) {
\                FILE *f=fopen(v[1], "r");
\                short p=0, m[1<<16], *i=m;
\                while (fscanf(f, "%hd,", i++) > 0) ;
\                for (; p>=0;) {
\                        int a=m[p++],b=m[p++],c=m[p++];
\                        a<0 ? m[b]=getchar() :
\                        b<0 ? putchar(m[a]) :
\                        (m[b]-=m[a]) <= 0 ? p=c :
\                        0;
\        }}
\
\ Or, alternatively you can test out the system online here:
\
\ <https://howerj.github.io/subleq.htm>
\
\ Also note, the SUBLEQ machine has no way of saving to disk
\ (or mass storage as it is sometimes known) and the only
\ peripherals it offers are inputting and outputting a single
\ byte. This does not hold us back during cross compilation,
\ as you will see.
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
\        9 -1 3 10 -1 6 0 0 -1 72 105 0
\
\ You can see that this CPU architecture is barren and spartan,
\ it does not offer what we usually expect from a processor. It
\ has no native way to shift, multiply, divide, it has
\ no interrupts, traps, nor a memory management unit, it cannot
\ load or store without doing a subtraction. The instruction it
\ can do will have to be manipulated so it can do just a store,
\ without the subtraction. Addition will have to be
\ synthesized, as will every other useful instruction.
\
\ Also of note, we have no stack, no function calls and
\ returns, no indirect load or stores. These will all require
\ self-modifying code to implement. The operand "c" cannot be
\ modified by the current instruction (so if "b" pointed to
\ the location of "c" the next time the instruction is run
\ it would have a new jump location), at least in most SUBLEQ
\ specifications.
\
\ On to defining our first instruction, an unconditional Jump
\ instruction, or "JMP", can be made in the following fashion,
\ using "A", "B", and "C" for the operands as we did before:
\
\         JMP c
\                subleq Z, Z, c
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
\        NOP
\                subleq Z, Z
\
\ Note that the third operand is omitted, we take this to
\ mean that the jump location should be the location of the
\ next instruction.
\
\ A non-trivial instruction is "ADD":
\
\        ADD a, b
\
\                subleq a, Z
\                subleq Z, b
\                subleq Z, Z
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
\ It is possible to implement a fully featured eForth in around
\ 8kiB with a hundred words, the image we will create
\ however is nearly double that in size as half the image will
\ be dedicated to implementing a virtual machine which eForth
\ can run on. On a more conventional architecture that VM could
\ be implemented in perhaps as little as a hundred bytes.
\
\ ### Detecting SUBLEQ virtual machine size
\
\ Although an account of how a SUBLEQ machine has been given,
\ there are other SUBLEQ machines that exist that will not
\ run this program, either they lack the memory, they do not
\ use twos-compliment arithmetic, they use bignums (or
\ arbitrary precision arithmetic) or even floating point
\ numbers for each cell, or, more likely they use a different
\ cell width than this one.
\
\ Instead of specifying an exact width it is most likely
\ that a SUBLEQ machine written in C would use "int" as the
\ storage type for each cell, which might be 16-bit but is
\ much more likely to be 32-bit or 64-bit. It would be possible
\ to create an image that is entirely agnostic to the cell
\ width and could perhaps patch itself up so it would run on
\ any bit width above a minimum, this image does not do that,
\ but it would be a interesting exercise, perhaps even
\ more difficult than constructing the original VM required to
\ support Forth. The cell width would not even necessarily
\ need to be 16, 32 or 64 bit, perhaps a 18 bit or 36 bit
\ system could be accommodated for.
\
\ A system that used bignums would be yet another difficulty,
\ arbitrary precision numbers do not overflow, and the fact
\ that integer overflow is used by this implementation to
\ implement a fast (for SUBLEQ machines) bitwise operations
\ causes problems for those bignum machine. Instead
\ the bitwise operations would have to be re-engineered around
\ multiplication and division instead of bit by bit testing
\ of the topmost bit (which indicates a number is negative,
\ an easy test for a SUBLEQ machine). It might instead be
\ easier to simulate a 16-bit SUBLEQ machine on an N-bit
\ architecture, where N is greater than 16, falling back to
\ direct evaluation if we detect we are running on a 16-bit
\ machine (and thus do not need the simulator), something
\ similar could be done for machines that used bignums.
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
\ JonesForth, j1eforth. It is a complex system to
\ understand in one go and looking at other Forth
\ implementations will certainly help.
\
\ There are different ways of bringing up a Forth system,
\ for different environments. The environments are:
\
\ * An interpreter designed to execute Forth directly written
\ in another language, usually C. This can either be on the
\ bare metal or hosted.
\ * A physical machine such as a Z80, a x86, or an ARM CPU.
\ Again, this can be bare metal or hosted.
\ * A physical machine designed to execute Forth directly,
\ such as the H2, the J1, or others. This environment
\ usually does not have an operating system.
\
\ We can also bring up the initial system by either writing
\ in assembly for the target, or by writing a Forth program
\ called a "meta-compiler" (similar but distinct from the
\ more well known computer science concept). The meta-compiler
\ is more akin to a special cross-compiler written in Forth
\ that uses the capabilities of the Forth language well.
\
\ Forth is a difficult language to use, but it is
\ good at making assemblers and cross compilers for bringing
\ up a Forth system on a new platform. It has niches in which
\ it works well (unfortunately nearly none of which are
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
\ case insensitivity is a poor idea, and that lower case
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

\ There are some options that can be configured at compile
\ time which, for example, can make the image smaller or
\ enable/disable certain features.
\
\ "opt.multi", "opt.editor", "opt.info" turn on/off groups
\ of words being compiled into the target image, they are all
\ optional and are not required for self-hosting the image.
\ "opt.control", "opt.float" and "opt.allocate" do the same.
\
\ "opt.generate-c" puts a comma in between the numbers in the
\ target image when it is outputted instead of a space, this
\ makes including the file in a C program easier.
\
\ "opt.sm-vm-err" makes for a smaller error message in the
\ Forth VM, where it detects the SUBLEQ machines width.
\
\ The "opt.sys" constant will be described more later, it
\ contains flags that control features in the system.
\
1 constant opt.multi      ( Add in large "pause" primitive )
1 constant opt.editor     ( Add in Text Editor )
1 constant opt.info       ( Add info printing function )
0 constant opt.generate-c ( Generate C code )
0 constant opt.better-see ( Replace 'see' with better version )
0 constant opt.control    ( Add in more control structures )
0 constant opt.allocate   ( Add in "allocate"/"free" )
0 constant opt.float      ( Add in floating point code )
0 constant opt.sm-vm-err  ( Smaller VM error message )

: sys.echo-off 1 or ; ( bit #1 = turn echoing chars off )
: sys.cksum    2 or ; ( bit #2 = turn checksumming on )
: sys.info     4 or ; ( bit #4 = print info msg on startup )
: sys.eof      8 or ; ( bit #8 = die if received EOF )

0 sys.cksum sys.eof sys.echo-off constant opt.sys

\ ## Order!
\
\ If you are not well versed with the vocabulary words, it
\ would help that you become so, as we will be using the Forth
\ vocabulary word set to create words for the meta-compiler,
\ and for the target dictionary and assembler.
\
\ This is one of the advantages of explaining a Forth written
\ in pure assembler as opposed to one written in a Forth
\ meta-compiler. Nearly every competent programmer is capable
\ or understanding assembler programs to a degree even if they
\ are not intimately familiar with the platform. However a
\ Forth cross compiler requires an understanding of Forth and
\ how vocabularies work, along with search orders, and the
\ like.
\
\ The words "(order)", "-order", "+order" are defined as the
\ built in words for manipulating Forth vocabularies are
\ appalling. "+order" and "-order" allow us to add and remove
\ a Forth word-list to the search order easily, "+order" adds
\ a word-list so long as it has not already been added.
\
\ If "(order)" is not defined then we assume "+order" and
\ "-order" are also not defined.
\

defined (order) 0= [if]
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
[then]

defined [unless] 0= [if]
: [unless] 0= postpone [if] ; immediate
[then]

\ You will notice some code is only executed for gforth,
\ which does not by default have the word "eforth" defined,
\ and some for the eForth interpreter running on the SUBLEQ
\ machine. The structure:
\
\         defined eforth [if]
\                ( CODE EXECUTED IN EFORTH )
\        [else]
\                ( CODE EXECUTED IN GFORTH )
\        [then]
\
\ Is used, seldomly. Here it is used because "wordlist" is not
\ defined in the base eForth image, but is in gforth. To bring
\ them up to parity, "wordlist" is defined for the eForth
\ image.
\

defined eforth [if]
  : wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )
[then]

\ We then define the following wordlists, "meta.1" is used for
\ the meta-compiler, words for image generation, some
\ constants, and for making word definitions go into specific
\ vocabularies. Some example words include "t@", for
\ fetching a value from the generated target image, or "t:"
\ for switching to the "target.1" vocabulary and defining new
\ words in it. "target.1" contains words that refer to the
\ target vocabulary, that is words that have been defined
\ within the new eForth image we are creating. We will want
\ to refer to them, such that when we use "+" we will want
\ at some point for that "+" to refer to a location within
\ the target image. "assembler.1" is for words relating to
\ the assembler that we use to tame the SUBLEQ machine, and
\ "target.only.1" is used for words that we do not normally
\ (but occasionally do) want to refer to when meta-compiling
\ but must also exist in the target dictionary. We might want
\ to use this "target.order.1" dictionary to define a target
\ version of ":" for example, we will want to use the
\ meta-compilers version of ":" when meta-compiling.
\

wordlist constant meta.1        ( meta-compiler word set )
wordlist constant target.1      ( target eForth word set )
wordlist constant assembler.1   ( assembler word set )
wordlist constant target.only.1 ( target only word set )

\ New definitions will now go into the "meta.1" vocabulary.

defined eforth [if] system +order [then]
meta.1 +order definitions

\ Some system constants are defined:

\ * "=cell" is the size of a cell in bytes, for our 16-bit
\  machine it is "2", if we want to allocate a cell within the
\ image, we will need to refer to this constant.
\ * "size" is the maximum size that our image we are generating
\ can be, which is plenty of room for a Forth image.
\ * "=buf" is the size of the textual input buffer used for
\ storing a line, this like the return and variable stacks are
\ stored within a threads memory area. Do not increase this
\ value without adjusting the rest of memory areas.
\ * "=stksz", each of the stacks, the variable and return
\ stacks are this size. It is stored within thread memory, one
\ of the stacks grows upwards and the other downwards and they
\ are arranged so it one of the stacks overflows it will end
\ up in the other stacks memory, this gives a little more stack
\ space to work with if one stack is consuming more than the
\ other, but both are quite small.
\ * "=thread", this contains the location of the first thread
\ we setup, there must always be at least one thread running.
\ Each thread is a 1024 byte block and is aligned (although it
\ does not have to be) on a 1024 block boundary. The first
\ thread is located at the end of main memory, which is a good
\ place to put it, but it interacts poorly with the SUBLEQ
\ machine in the appendix that saves the image after running
\ (it still works). A consequence of the thread size being
\ 1024 bytes and 1024 byte aligned is that if we had a block
\ system that could swap out to mass storage we could
\ potentially swap out unused threads. This goes for any
\ data structure with the same properties, the block system
\ is like the poor-mans Memory Management Unit (MMU).
\ * "=bksp", "=lf", "=cr", and "=del" contain the values for
\ the characters backspace, line feed, character return and
\ delete respectively.
\
\
   2 constant =cell   \ Target cell size
4000 constant size    \ Size of image working area
 100 constant =buf    \ Size of text input buffers in target
 100 constant =stksz  \ Size of return and variable stacks
FC00 constant =thread \ Initial start of thread area
0008 constant =bksp   \ Backspace character value
000A constant =lf     \ Line feed character value
000D constant =cr     \ Carriage Return character value
007F constant =del    \ Delete character

\ Now we need to create an area to store the new image in,
\ called "tflash", and clear it. The image generated is not
\ that big. We will need under 16KiB (although we are cutting
\ it close).
\

create tflash tflash size cells allot size erase

\ "tdp" is the Target Dictionary Pointer, "there" (pronounced
\ "T-here" for "target-here") will return the contents of it
\ when it is defined. "t" is usually used as a prefix to mean
\ "target" in this implementation.
\
\ "tlast" is a pointer to the last defined word in the target.
\
\ "tlocal" is used to allocate room within a thread local
\ storage system, which is used to build cooperative
\ multitasking into the system.
\

variable tdp 0 tdp ! ( target dictionary pointer )
variable tlast 0 tlast ! ( last defined target word pointer )
variable tlocal 0 tlocal ! ( local variable allocator )
variable voc-last 0 voc-last ! ( last defined in any vocab )

\ We will be switching between vocabularies later, by using
\ word pairs like ":m"/";m" and ":t"/";t" we can define words
\ and put them in the "meta.1" or "target.1" word sets
\ using those word pairs.
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
:m mdrop drop ;m ( u -- : always call drop )
:m mswap swap ;m ( u -- : always call swap )
:m mdecimal decimal ;m
:m mhex hex ;m
:m m.s ." STK> " .s cr ;m     ( ??? -- ??? : debugging print stack )
:m seem see cr ;m 

\ "tpack" is used to copy a string into the target image.
\ Useful if we want to define strings in the target, which we
\ will when defining new words in the target header.
\
\ "limit" is used on systems which are not 16-bit (i.e. Gforth)
\ to limit the maximum value of a number to be modulo 2 raised
\ to the 16. It does nothing on SUBLEQ eForth as that is a
\ 16-bit machine.
\

defined eforth [if]
  :m tpack dup tc, for aft count tc, then next drop ;m
  :m parse-word bl word ?nul count ;m ( -- a u )
  :m limit ;m ( u -- u16 : not needed on 16-bit systems )
[else]
  :m tpack talign dup tc, 0 ?do count tc, loop drop ;m
  :m limit FFFF and ;m ( u -- u16 : limit variable to 16 bits )
[then]

\ "\$literal" is defined now, but will not be of much use
\ until much later, when we can compile strings into the
\ target dictionary, until then it cannot be used.
\
\ "\$literal" parses input until a double quote is reached
\ and then compiles that string into the dictionary. It also
\ takes care of alignment. Given its description in this
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
\ 16-bit). The image saving routine needs to print out a 16-bit
\ signed value, so for gforth implementations that are likely
\ to be 32 or even 64 bit, the value to be printed out will
\ need to be sign-extended from a 16-bit signed value to a
\ 32/64 bit signed value, for the SUBLEQ eForth, that does
\ not need to be done. Both versions of "#dec" print out
\ 16-bit signed values is the important take away.
\

defined eforth [if]
\ If not using "(.)" and "opDivMod", then this can be used:
\
\        :m #dec dup >r abs 0 <# #s r> sign #> type ;
\
\ Otherwise:
:m #dec s>d if [char] - emit then (.) ;m ( n16 -- )
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
\ instead of a single giant line changing if only one or two
\ cells have changed. This makes an otherwise binary image
\ (albeit stored in a textual format) play better with version
\ control systems such as "git", or "svn".
\
\ "save-target" actually calls "mdump", making sure we are in
\ decimal base before hand, which the output format requires.
\
\ Due to defining our own versions of "only", "forth",
\ "definitions" and "hex" in the target image we will define a
\ word called ".end" that will restore the interpreter to a
\ standard, usable state, after compilation is complete.
\

opt.generate-c [if]
  :m msep 2C emit ;m ( -- : emit "," as separator )
[else]
  :m msep A emit ;m  ( -- : emit space as separator )
[then]
:m mdump taligned ( a u -- )
  begin ?dup
  while swap dup @ limit #dec msep tcell + swap tcell -
  repeat drop ;m
:m save-target decimal tflash there mdump ;m ( -- )
:m .end only forth definitions decimal ;m ( -- )

\ "atlast" retrieves the last defined word, or a pointer to
\ to it. It will be used to set the variable which will
\ contain the Forth vocabulary, and the "{last}" variable,
\ which contains the last defined word when running the target
\ eForth. The "{last}" variable and the Forth vocabulary,
\ which is stored in "forth-wordlist", is set at the end of
\ the image generation.
\

:m atlast tlast @ ;m ( -- a )

\ "lallot" allocates space for a USER variable, which is just
\ an offset into the task thread, each thread has a 1024 byte
\ block used to store the tasks variable and return stacks,
\ buffers, and also USER variables - task specific or thread
\ local storage. "lallot" keeps track of an offset into thread
\ local storage ("tlocal") for those USER variables. "tuser"
\ can be used to allocate a cell in local storage space, and
\ assign a name for that space in the meta-compiler, which when
\ run will compile that offset into the target image. There is
\ a limited amount of space within each thread, so user
\ variables should not be allocated freely.
\
\ USER variables are thread-local variables, there is an
\ instance of a USER variable for each thread that has been
\ created.
\
\ "local?" fetches the local value.
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

:m local? tlocal @ ;m
:m lallot >r tlocal @ r> + tlocal ! ;m ( u -- allot in target )
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
\ "voc-last" is used as "tlast" is swapped in and out depending
\ on the vocabulary used, whereas "voc-last" is not.
\
:m compile-only voc-last @ tnfa t@ 20 or voc-last @ tnfa t! ;m
:m immediate   voc-last @ tnfa t@ 40 or voc-last @ tnfa t! ;m

\ "half" and "double" are just synonyms for "2/" and "2\*", it
\ is much easier to know you are calling the correct version of
\ the words when they have different names. During
\ meta-compilation we will need to convert from Forth to VM
\ addresses using these two words when setting execution
\ tokens.
\

:m half dup 1 and abort" unaligned" 2/ ;m ( a -- a )
:m double 2* ;m ( a -- a )

\ Some more conditional code is needed due to the differences
\ between the implementations of the single quote, "'", on the
\ two different Forth implementations used to compile this
\ program.
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
\ "tcompile" forces compilation of the target version of
\ the Forth into the dictionary as an executable token, it
\ should be used within a new target word definition.
\

defined eforth [if]
:m (') bl word find ?found cfa ;m
:m t' (') >body @ ;m ( --, "name" )
:m to' target.only.1 +order (') >body @ target.only.1 -order ;m
[else]
:m t' ' >body @ ;m ( --, "name" )
:m to' target.only.1 +order ' >body @ target.only.1 -order ;m
[then]
:m tcompile to' half t, ;m
:m >tbody =cell + ;m

\ "tcksum" is used to calculate the checksum over the part of
\ the image that is checked. At the end of the meta-compilation
\ process this value is calculated and poked into the target
\ image. The corresponding word in the target used at runtime
\ is called "cksum". The algorithm only uses addition, which
\ makes for a weak form of checksum, but it is easy to compute.
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
\ used on "immediate" words being compiled in the target,
\ even though their immediateness only applies to it when the
\ target is running, which it is not when cross compiling.
\

:m postpone ( --, "name" )
   target.only.1 +order t' target.only.1 -order 2/ t, ;m

\ The dictionary in Forth is a list of all of the "words", or
\ functions, available to the system, the "words" each have a
\ header, and the dictionary and headers looks like this:
\
\ ![Dictionary Format](img/dictionary.png)
\
\ (See the appendix for the ASCII ART version of this diagram)
\
\ There is a difference between this Forth implementation and
\ other Forth implementations, it is the location of the where
\ the Forth word name fields are stored. In this Forth
\ implementation they are stored conjoined with the words
\ themselves, as with other Forth implementations such as
\ JonesForth. This simplifies the system conceptually and
\ means code and the names form one contiguous block, which has
\ some advantages.
\
\ However most Forth implementations have a separate Forth name
\ location and code location. This means that it is easier to
\ erase the names for words, which may no longer be needed,
\ it is even possible to erase all of the word names and keep
\ the code intact which is not possible in this implementation,
\ although separate storage is less compact than a conjoined
\ system. Both systems can be designed to use the same amount
\ of memory, with caveats.
\
\ The word "thead" makes a header for a word in the target, it
\ writes a pointer to the previously defined word making a link
\ in the dictionaries linked list, parses the next word in the
\ input stream, and copies that name into the target dictionary
\ with "tpack", making sure to align the Dictionary Pointer for
\ the code generation stage.
\
\ "thead" is not called directly, but is called by
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

:m thead talign there tlast @ t, dup tlast ! voc-last !
   parse-word talign tpack talign ;m ( --, "name" )
:m header >in @ thead >in ! ;m ( --, "name" )
:m :ht ( "name" -- : forth routine, no header )
  get-current >r target.1 set-current create
  r> set-current CAFE talign there ,
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
\ hexadecimal value "1234" and "(a);" (which ";a" will call)
\ throws an exception if that value is not on the stack when
\ it is called. This catches common errors such as using the
\ wrong word definitions from the wrong vocabulary, not
\ compiling literals correctly, etc.
\

:m :to ( "name" -- : forth, target only routine )
  header
  get-current >r
    target.only.1 set-current create
  r> set-current
  CAFE talign there ,
  does> @ 2/ t, ;m
:m :a ( "name" -- : assembly routine, no header )
  1234 target.1 +order definitions
  create talign there , assembler.1 +order does> @ 2/ t, ;m
:m (fall-through); 1234 <>
   if abort" unstructured" then assembler.1 -order ;m
:m (a); (fall-through); ;m

\ We no longer need the system vocabulary, if we are running
\ the cross-compiler on the eForth image (we never did if
\ running on gforth), so to keep things clean we will remove
\ it from the search order.

defined eforth [if] system -order [then]

\ # Forth Virtual Machine
\
\ This section contains the Forth Virtual Machine; a VM
\ that does the bare minimum by implementing about forty or
\ so instructions. If these instructions were implemented on
\ different platform this Forth would then be trivial to port
\ to it.
\
\ Implementing and debugging the Forth VM was the hardest task
\ of porting this Forth to the SUBLEQ platform, usually it is
\ not that that difficult, and a Forth can be ported
\ rapidly by a skilled programmer familiar with the language,
\ perhaps in a week or two, given that they are familiar with
\ the target.
\
\ However with only a single instruction it is incredibly
\ difficult to perform even the most basic tasks you expect
\ from a CPU. Once the VM was finished (it was more of an
\ iterative process between implementing the Forth VM and the
\ Forth Code) porting the Forth interpreter itself was
\ trivial as this eForth is itself written in Forth.
\
\ The memory layout of the Forth Virtual Machine is as follows:
\
\ * 0: Zero Register.
\ * 1: Zero Register.
\ * 2: Jump to Forth Virtual Machine Start.
\ * 3: Options variable.
\ * 3 to W-1: System variables.
\ * W: label start; The Forth VM entry point.
\ * X: label vm; The Forth VM.
\ * Y: Forth VM instruction implementation.
\ * Z: Forth Code that uses Forth VM instructions.
\ * End - 1024/$400 bytes: First thread area.
\
\ This is a rough layout only, 'W', 'X', 'Y' and 'Z' refer to
\ memory blocks that may vary in location and length, but are
\ in the same order as shown.
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
\ Now to define macros that will allow us to perform
\ addition, loads, stores, indirect loads and stores, moves,
\ some input and output, and other tasks.
\
\ ## The SUBLEQ Assembler
\
\ This section contains the assembler for the SUBLEQ machine.
\
\ The following words could also go into the target assembler,
\ which is usually part of a vocabulary called "assembler",
\ and can be activated to create words written in assembler
\ with the words "CODE" and "END-CODE", those will not be
\ defined in this eForth, there would also need to be a
\ mechanism to call Forth words written in assembler as well.
\
\ The influential Forth-83 standard defined the following
\ assembler related words (which will not be present in this
\ system), but would be added in to the target image on a
\ "real" system. One reason they are not added in is because
\ they would have to be redefined for "gforth" anyway, which
\ is used for fast compilation of the target image. The words
\ are:
\
\ * "ASSEMBLER": Adds the assembler word set into the search
\ order, in this system, if present, it would add the
\ control structures defined, and "PUT", "MOV", "JMP", and
\ the like, into the search order.
\ * "CODE": Along with "END-CODE", as mentioned, this is the
\ assembler version of ":", it sets up a new word in the
\ dictionary that will not be visible until the corresponding
\ "END-CODE" has been reached, it also adds in the "ASSEMBLER"
\ vocabulary into the dictionary, so the assembler words can
\ be found.
\ * ";CODE": Starts a section of assembler *within* a normal
\ word definition, which should be terminated with "END-CODE".
\ * "END-CODE": Terminates a run of assembly defined with
\ ";CODE" or "CODE".
\
\ Those are the only standard assembly words, naturally the
\ rest would be platform specific, the mnemonics for an
\ x86 or an ARM processor would be different for example.
\
\ Onto the SUBLEQ assembler words themselves.
\
\ The macros all write instructions into the target image
\ with "t,".
\
\ "Z" is the first word defined. It writes a zero into a cell,
\ it is meant to be used as a register location that starts
\ off as zero, as mentioned, and should end back up as zero
\ by the end of the instruction. It is not an instruction, but
\ is used to build one. The same with "NADDR", which compiles
\ the next memory location into the current cell. "A," is an
\ alias for "Z". "V," uses the next cell for the same purpose.
\
\ For each macro the precondition is that the first two
\ cells in the image are zero and as a postcondition the macro
\ must leave those cells as zero, being able to use them as
\ temporary storage within the macro.
\
\ "HALT" is our first instruction, "0 0 -1" will always halt
\ the machine. This will be used to implement the Forth word
\ "bye", but it is also useful for debugging the lower levels
\ when the Forth VM had to be built.
\
\ "JMP" accepts a Forth Address, converts it to a Cell Address,
\ and when run, it will unconditionally jump to that location.
\
\ "ADD" is more complex than "SUB", they both have the same
\ form:
\
\        source destination instruction
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
\ output respectively.
\
\ "MOV" copies the location of a cell to another one, it
\ requires four individual SUBLEQ instructions to implement.
\ "a b MOV" would copy "a" to "b". First "b" is zeroed, then
\ "a" is negated and stored in "Z", then "Z" which now contains
\ the negated "a" is subtracted from "b", moving the contents
\ stored in "a" to "b". Finally "Z" is returned to its original
\ state by subtracting "Z" from itself, so it now contains
\ zero as it should. Four instructions.
\
\ "iLOAD" does an indirect load, "a b iLOAD" would use "b"
\ as an address to do an indirect load through and store the
\ result in "a". It does this by using the "MOV" instruction
\ to modify a second "MOV" instruction to point to a different
\ location. Notice how the number of SUBLEQ instruction
\ increases quickly to achieve anything, two moves is eight
\ instructions, "iSTORE" is even worse at four "MOV"
\ instructions.
\
\ "iJMP" is simpler than "iLOAD", it just has to modify a
\ "NOOP" instruction to point to a different location, changing
\ the final cell in the "NOOP", turning it in effect into a
\ unconditional jump. Note that the final "Z Z NADDR" must come
\ after the "MOV", which also contains a "Z Z NADDR", both
\ must be present, the first to clear the "Z" register (which
\ may or may not perform the jump depending on what was in "Z")
\ and the second which will always jump, as the "Z" register
\ has been previously zeroed.
\
\ "iSTORE" is the most complex of all of these single
\ instruction macros, as mentioned, it contains four "MOV"
\ instructions, each containing four SUBLEQ instructions,
\ meaning twelve SUBLEQ instructions, or forty eight cells just
\ to perform an indirect store. It requires three "MOV"
\ instructions in order to modify three locations in a
\ final "MOV", leaving the last instruction of the modified
\ "MOV" the same, to zero out the "Z" register.
\
\ "iADD" and "iSUB" perform indirect add and subtract
\ respectively, both are shorter than an "iSTORE" and find
\ their use implementing "+" and "-" which operate on the
\ variable stack (and thus have to do indirection).
\
\ There are under twenty macro instructions in the base
\ assembler; I/O, Loads/Stores, and arithmetic. The conditional
\ instruction macros will be defined in the next section. That
\ the instruction number is so small is remarkable.
\

:m Z 0 t, ;m ( -- : Address 0 must contain 0 )
:m A, 0 t, ;m ( -- : Synonym for 'Z', temporary location )
:m V, 1 t, ;m ( -- : Address 1 also contains 0, temp location )
:m NADDR there 2/ 1+ t, ;m ( --, jump to next cell )
:m HALT 0 t, 0 t, -1 t, ;m ( --, Halt but do not catch fire )
:m JMP 2/ Z Z t, ;m ( a --, Jump to location )
:m ADD swap 2/ t, Z NADDR Z 2/ t, NADDR Z Z NADDR ;m
:m SUB swap 2/ t, 2/ t, NADDR ;m ( a a -- : subtract )
:m NOOP Z Z NADDR ;m ( -- : No operation )
:m ZERO dup 2/ t, 2/ t, NADDR ;m ( a -- : zero a location )
:m PUT 2/ t, -1 t, NADDR ;m ( a -- : put a byte )
\ :m GET 2/ -1 t, t, NADDR ;m ( a -- : get a byte )
:m MOV 2/ >r r@ dup t, t, NADDR 2/ t, Z  NADDR r> Z  t, NADDR
   Z Z NADDR ;m
:m iJMP there 2/ E + 2* MOV Z Z NADDR ;m ( a -- )
:m iADD ( a a -- : indirect add )
   2/ t, A, NADDR
   2/ t, V, NADDR
   there 2/ 7 + dup dup t, t, NADDR
   A,   t, NADDR
   V, 0 t, NADDR
   A, A, NADDR
   V, V, NADDR ;m
:m iSUB ( a a -- : indirect sub )
   2/ t, A, NADDR
   2/ >r
   there 2/ 7 + dup dup t, t, NADDR
   A,   t, NADDR
   r> t, 0 t, NADDR
   A, A, NADDR ;m
\ The previous version of "iSTORE" leveraged the "MOV" macro
\ and produced a larger indirect store.
\
\        :m iSTORE ( a a -- )
\          swap >r there 2/ 24 + 2dup 2* MOV
\          2dup 1+ 2* MOV 7 + 2* MOV r> 0 MOV ;m
\
\ The version below is smaller, and will execute faster,
\ even if the definition is larger.
\
:m iSTORE ( a a -- : Indirect Store )
  2/ t, A, NADDR
  there 2/ 3 4 * + dup t, t, NADDR
  there 2/ $A + dup t, t, NADDR
  A, there 2/ 5 + t, NADDR
  A, there 2/ 3 + t, NADDR
  0 t, 0 t, NADDR
  2/ t, V, NADDR
  there 2/ 7 + dup dup t, t, NADDR
  A,   t, NADDR
  V, 0 t, NADDR

  A, A, NADDR
  V, V, NADDR
  ;m

\ An attempt at a more efficient "iLOAD" was made, however it
\ is exactly the same size as the previous definition:
\
\       :m iLOAD
\          2/ t, A, NADDR
\          there 2/ 6 + dup t, t,  NADDR ( [q] = 0 )
\          A, there 2/ 2 + t, NADDR
\          0 t, V, NADDR
\          2/ dup dup t, t, NADDR
\          V, t, NADDR
\          A, A, NADDR
\          V, V, NADDR ;m
\
\ So original is kept in. The main problem lies in the fact
\ that we cannot load or store to a cell, we can only subtract
\ from it, thus we have to zero that cell before hand.
\

:m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m ( a a -- )

\ To simplify program flow some control structures
\ will be defined, they work a little differently than the
\ Forth ones, as they must be given an address instead of
\ taking items off of the stack, however they make everything
\ a lot easier to do and provide us with the basics from the
\ structured programming paradigm. Programming in straight
\ assembler sucks.
\
\ In Forth "if" pulls an item off of the variable stack to test
\ again, however the assembler version of "if" instead reads
\ (and does not modify) a memory location, so it must be used
\ like so:
\
\        <location> if ... then
\
\ The same goes for "until", and "while", "+if", and "-if".
\ "+if" and "-if" are also new, they will execute if the
\ memory location provided contains a positive or a negative
\ value respectively, which is easy to compute in SUBLEQ.
\
\ Here is a short overview of the constructs:
\
\ * "if...then", the standard "if" clause, if the value
\ provided in the given location is non-zero then the clause is
\ execute, otherwise execution begins after "then". "else" is
\ not provided here in the assembler because it is not needed.
\ * "-if...then", much like "if...then" but it executes the
\ clause if the given location contains a negative cell.
\ * "+if...then", like "-if" but only executes the clause if
\ given a positive cell.
\ * "begin...again", an infinite loop.
\ * "begin...until" is the simplest loop on most platforms,
\ on a SUBLEQ machine it is more complex, but kept in because
\ a lot of Forth code uses the loop. It continues to run
\ until the location given to "until" is non-zero.
\ * "begin..while...repeat" will repeat until the location
\ given to "while" is zero, it is easier to use than the
\ "begin...until" loop.
\
\ These constructs allow us to make a language that is not
\ raw assembler, and not really a high level language
\ either. The macros work by either pushing locations and/or
\ writing holes into the assembled image, or by patching up
\ those holes in different ways, or by manipulating
\ the memory locations already on the stack, some words do
\ a combination of those three options.
\
\ For example "mark" creates an empty location in the
\ dictionary and pushes that location onto the meta-compilers
\ variable stack. Later control structures can use that
\ location to patch up it up with a new jump destination.
\ "mark" should be used in place of the last operand of a
\ partially completed SUBLEQ instruction, as it is in the
\ definition of "if".
\
\ None of these words contain any compiler security, unlike
\ the control structure words defined in the target, or defined
\ in the meta-compiler, so use them carefully.
\

assembler.1 +order definitions
: begin talign there ; ( -- a )
: again JMP ; ( a -- )
: mark there 0 t, ; ( -- a : create hole in dictionary )
: if ( a -- a : NB. "if" does not work for 8000 )
   2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t,
   mark ;
: until 2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t,
   Z Z NADDR Z t, 2/ t, ; ( a -- a )
: +if   Z 2/ t, mark ; ( a -- a )
: -if 2/ t, Z there 2/ 4 + t, Z Z there 2/ 4 + t, Z Z mark ;
: then begin 2/ swap t! ; ( a -- )
: while if swap ; ( a a -- a a )
: repeat JMP then ; ( a a -- )
assembler.1 -order
meta.1 +order definitions

\ If we were to implement the ASSEMBLER words then we would put
\ all of the words defined in this section into an assembler
\ vocabulary in the target. An added complication is getting
\ the VM to JMP to the newly defined word because of how the
\ Forth VM works, which might require a new primitive to do so,
\ a hypothetical concern as we will not be implementing this
\ feature, and a concern that will make more sense later when
\ we see how the Virtual Machine is implemented.
\
\ # The System Variables
\
\ There is not a lot in this section in terms logic, it nearly
\ all allocation of variables. They will be described, however
\ they are registers for the VM and locations of Forth
\ variables, context is required to understand them.
\
\ This section also contains the first instruction to
\ be executed, which is formed by three "t," statements.
\
\ Initially "0 t, 0 t, -1 t," is written into the target
\ image. This might seem like the first instruction would
\ halt the interpreter, and it would, were it not for the
\ fact that the third value is overwritten later with the
\ location of the Forth Virtual Machine entry point.
\
\ The first three memory locations comprise a single SUBLEQ
\ instruction which has more than one use, the first two memory
\ locations must be zero, to ensure that the third operand
\ is jumped to (which will jump to the Forth Virtual Machine)
\ and because it is used as the location of the "Z" register,
\ which must contain zero as is used in subsequent SUBLEQ
\ instructions.
\
\ This section also contains constants ("one", "two", "bwidth"
\ and others), and virtual machine registers ("w", "x", and
\ others).
\
\ The variables that are worth noting now are; "h",
\ "primitive", "{options}", "{up}", and "check".
\
\ * "h" contains the dictionary pointer, as used by "here",
\ this will be used much later on.
\ * "primitive" is used by the virtual machine to determine
\ which instructions are VM instructions and which are calls
\ to Forth words.
\ * "{up}" is used for thread local storage, which will be
\ describe later, also known as user variables.
\ * "check" is used to hold a checksum over the Forth image.
\ * "{options}" is a variable near the beginning of the image
\ that can be manually edited to change fundamental behaviors
\ of the image. It deserves more explanation.
\
\ ## {options} variable
\
\ The "{options}" variable is used as a bit-field containing
\ 16-bits, of which only the lowest four are used. The options
\ are:
\
\ * bit 1: If set turn echoing of characters off when
\   processing input characters.
\ * bit 2: If set checksum is checked on startup.
\ * bit 3: If set a greeting is printed out on startup.
\ * bit 4: If set "bye" is called if an error or End-of-File
\   occurs when attempting to read a character from input.
\
\ The "{options}" variable is placed so near the beginning of
\ the generated file so it can be edited manually in generated
\ images if needed.
\
\ Bit options 1 and 4 are used for terminal input and output
\ and might need to change depending on how I/O is processed.
\
\ Using the C implementation that uses non-blocking input means
\ an error code is returned if there is no input, by clearing
\ bit 4 is means that instead of the system calling HALT it
\ passes the error code up. The default SUBLEQ implementation
\ does blocking input, so we want this bit set normally.
\
\ On some systems turning on non-blocking mode also enables
\ or disables other terminal operations, such as echoing
\ characters back when they are input by the user, hence why
\ bit 1 is also needed, to enable or disable echoing.
\
\ The checksum is checked when the image is first loaded but
\ not on subsequent boot operations, as the image might have
\ been modified since last boot. If set, it will be cleared on
\ a successful checksum calculation.
\
\ Some Forth implementations like to be noisy and print out
\ a banner, the behavior is obnoxious as it prevents the Forth
\ from being used like a standard Unix utility with commands
\ piped into the interpreter, and output printed to standard
\ output (which is how the meta-compilation works, almost)
\ unless extra complication is added to determine whether the
\ program is talking to an interactive terminal or not (on Unix
\ systems the function call "isatty" does this).
\
\ This complication is not needed if by default the verbose
\ banner is not printed out, which is the case here. If you
\ need or want it to be printed out, this option can
\ be enabled.
\

  0 t, 0 t,        \ both locations must be zero
label: entry       \ used to set entry point in next cell
  -1 t,            \ system entry point
opt.sys tvar {options} \ bit #1=echo off, #2 = checksum on,
                   \ #4=info, #8=die on EOF
  0 tvar primitive \ any address lower must be a VM primitive
  =stksz half tvar stacksz \ must contain $80
 -1 tvar neg1      \ must contain -1
  1 tvar one       \ must contain  1
  2 tvar two       \ must contain  2
 10 tvar bwidth    \ must contain 16
  0 tvar r0        \ working pointer 1 (register r0)
  0 tvar r1        \ register 1
  0 tvar r2        \ register 2
  0 tvar r3        \ register 3
  0 tvar r4        \ register 4
  0 tvar r5        \ register 5
  0 tvar r6        \ register 6
  0 tvar r7        \ register 7

  0 tvar h         \ dictionary pointer
  =thread half tvar {up} \ Current task addr. (Half size)
  0 tvar check     \ used for system checksum

\ The following will described at a later point, there is
\ no point in going over them again, but they are the memory
\ locations that are referred to by words without the "{}"
\ brackets, so for example the word "cold", defined later on,
\ is defined as:
\
\        : cold {cold} lit @execute ;
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
  0 tvar {last}     \ last defined word
  0 tvar {cycles}   \ number of times we have switched tasks
  0 tvar {single}   \ is multi processing off?
  0 tvar {user}     \ Number of locals assigned

\ Most of the following are thread local variables, with the
\ exception of "ip" and "tos", the stack variables "{rp}" and
\ "{sp}" and the initial stack positions "{rp0}" and "{rp0}",
\ which are only stored in thread local storage on a task
\ switch. All words defined with "tuser" are locations of
\ memory relative to thread local storage.
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
\ and return stacks as well as initial execution hooks with
\ that task. However, for the first task the stacks and input
\ buffers need to be set up manually.
\

  \ Thread variables, not all of which are user variables
  0 tvar ip         \ instruction pointer
  0 tvar tos        \ top of stack
  =thread =stksz        + half dup tvar {rp0} tvar {rp}
  =thread =stksz double + half dup tvar {sp0} tvar {sp}
  200 constant =tib \ Start of terminal input buffer
  380 constant =num \ Start of numeric input buffer
  tuser {next-task} \ next task in task list
  tuser {ip-save}   \ saved instruction pointer
  tuser {tos-save}  \ saved top of variable stack
  tuser {rp-save}   \ saved return stack pointer
  tuser {sp-save}   \ saved variable stack pointer
  tuser {handler}   \ throw/catch handler
  tuser {sender}    \ multitasking; msg. send, 0 = no message
  tuser {message}   \ multitasking; the message itself
  tuser {id}        \ executing from block or terminal?

\ # Meta-compiler Assembly Macros
\
\ Ideally these meta-compiler macros would be defined along
\ with the other meta-compiler words, however they require
\ the locations of constants which are in the target image to
\ be known, such as "one" or "neg1", which have only just been
\ defined. This will happen throughout, the definitions of
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
\        subleq ?, ?
\        subleq Z, Z, c
\
\ The first "subleq" instruction always branches to next one,
\ the second one always branches to "c". We can replace this
\ with the equivalent:
\
\        subleq ?, ?, c
\        subleq Z, Z, c
\
\ The first instruction *may* branch to "c", the second one
\ always will. It is a minor optimization that is easy to
\ implement, so it might as well be done.
\
:m INC 2/ neg1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ one  2/ t, t, NADDR ;m ( b -- )
:m ONE! dup ZERO INC ; ( a -- : set address to '1' )
:m NG1! dup ZERO DEC ; ( a -- : set address to '-1' )
:m ++sp {sp} DEC ;m ( -- : grow variable stack )
:m --sp {sp} INC ;m ( -- : shrink variable stack )
:m --rp {rp} DEC ;m ( -- : shrink return stack )
:m ++rp {rp} INC ;m ( -- : grow return stack )
:m a-optim drop ;m \ >r there =cell - r> 2/ t! ;m ( a -- )

\ # The Core Forth Virtual Machine
\
\ The core of the Forth Virtual Machine starts here, it is
\ short and is executed for each Virtual Machine
\ instruction, so any optimization here would make a large
\ difference. It only has a small set of things it needs to do.
\ Articles on Threaded Code Interpreters help in understanding
\ how the Virtual Machine works.
\
\ The entry point is the label "start", which is executed
\ on startup naturally, it sets up the initial stack
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
\ required is a stack to place the calls, which "{rp}"
\ (the Return stack Pointer) is used for, well, it is used as
\ a pointer to the call stack.
\
\ The way the Forth VM determines whether an instruction is a
\ call or a VM instruction to jump to is by assuming any
\ address bellow a value contained in a variable called
\ "primitive" (which will be set later) is a VM instruction,
\ anything above that is something that is called instead, as
\ it is a Forth function. This is done because all that is
\ needed to test this is a subtraction and jump, all of which
\ the SUBLEQ machine can do easily.
\
\ There are three main types of threaded code interpreters;
\ direct, indirect and subroutine. There are other kinds, but
\ they will not be considered. This system uses hybrid approach
\ on a custom VM designed to executed Forth.
\
\ We can imagine a Forth program as consisting of a
\ series of calls, with primitives and mechanisms for pushing
\ numbers and jumping thrown in. For example, let us define a
\ function that computes the square root of "a" squared plus
\ "b" squared (given an already defined "isqrt" for computing
\ the integer square root.
\
\        : square dup * ;
\        : pythagoras square swap square + isqrt ;
\
\ In the above example, and in this interpreter, "dup", "*",
\ and "swap" are primitives, "square", "pythagoras" and "isqrt"
\ are functions. What would the code potentially look like?
\ The Forth compiler, an interactive and lightweight compiler,
\ must generate the code on-the-fly. There are multiple ways
\ it could do this.
\
\ One way would be something like this:
\
\
\        +:
\                Assembly instructions...
\                exit
\        *:
\                Assembly instructions...
\                exit
\        DUP:
\                Assembly instructions...
\                exit
\        SWAP:
\                Assembly instructions...
\                exit
\        ISQRT:
\                Integer Square Root implementation...
\                exit
\        SQUARE:
\                call dup
\                call *
\                exit
\        PYTHAGORAS:
\                call square
\                call swap
\                call square
\                call +
\                call isqrt
\                exit
\
\ Where "call", "exit", and the assembly instructions are
\ native instructions to the machine. This is known as
\ subroutine threaded code. However, our SUBLEQ machine does
\ not a call instruction built in. If we want to perform
\ a call or return then we must implement that in our VM.
\
\        +:
\                Assembly instructions...
\                Jump to VM
\        *:
\                Assembly instructions...
\                Jump to VM
\        DUP:
\                Assembly instructions...
\                Jump to VM
\        SWAP:
\                Assembly instructions...
\                Jump to VM
\        EXIT:
\                Assembly instructions...
\                Jump to VM
\
\
\         -----  END OF VM INSTRUCTIONS  -----
\               SUPER IMPORTANT BARRIER!
\
\        Anything below this line must be a Forth
\        function and not an assembly instruction so
\        the Forth VM can decide it should jump to it
\        instead of calling it!
\
\
\        ISQRT:
\                Integer Square root implementation...
\                Address of exit
\        SQUARE:
\                Address of instruction dup
\                Address of instruction *
\                Address of instruction exit
\        PYTHAGORAS:
\                Address of Forth Function square
\                Address of instruction swap
\                Address of Forth Function square
\                Address of instruction +
\                Address of Forth Function isqrt
\                Address of instruction exit
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
\ address if less than the address contained in "primitive"
\ (a unique feature of this VM). It is not, as "SQUARE" is not
\ a primitive, it is a defined function. As this is the case,
\ the return stack point "{rp} is incremented, "ip" (which
\ contains the next address to be executed) is pushed onto the
\ return stack with a "iSTORE" (indirect store), and the "w"
\ becomes the new "ip" value. We have just performed a "call"
\ with the VM.
\
\ Our "ip" now points to the first address of "SQUARE", which
\ is the address of "DUP", this is not a defined word, it is a
\ VM primitive or instruction. This means we do not call it,
\ we do an indirect jump to it. We still copy "ip" to "w",
\ indirect load through "w", and increment "ip" after. However,
\ instead of doing a simulated call, we do an indirect jump
\ through the contents of "w", or a double indirect through
\ "w".
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
\ The VM also contains code to detect and prevent the image
\ from running on a SUBLEQ machine that is not a 16-bit one, as
\ the image will not work correctly if it is.
\
\ It detects the width of the VM using arithmetic
\ properties of the VM that can only be maintained given
\ a certain bit-width, such as adding 0x7FFF to 0x7FFF on
\ a 16-bit machine should result in a negative number, on a
\ 32-bit machine it will be positive.
\
\ The code will detect machine widths greater than 16-bits and
\ print this error message and exit, so the system fails
\ gracefully instead of just not working. It will detect 32-bit
\ SUBLEQ machines (the most common) as well as odd ones such
\ as 17-bit machines. The image will work on 16-bit machines so
\ this error message and exit will not be triggered, however
\ it will also not be triggered for machine widths lower than
\ 16-bits (such as an 8-bit or a 15-bit SUBLEQ machine), which
\ are far less common so it is not a worry.
\
\ Tests to determine if we are on a ones-compliment, sign
\ magnitude, or arbitrary precision machine are not performed
\ either, so this detection system might give false
\ positives (only twos-compliment is supported). These could
\ be tested for. To support those machines whilst keeping the
\ system mostly the same an emulator for a 16-bit twos
\ compliment SUBLEQ machine could be written and prepended
\ to the target image, this is a complication too far. Note,
\ on systems with fewer than 16-bit cells we can address
\ fewer bytes than might be necessary to do this.
\

opt.sm-vm-err [if]
( Smaller, more cryptic, error message string "Error" )
7 tvar err-str
  45 t, 72 t, 72 t, 6F t, 72 t, 0D t, 0A t,
[else]
( Error message string "Error: Not a 16-bit SUBLEQ VM" )
1F tvar err-str
  45 t, 72 t, 72 t, 6F t, 72 t, 3A t, 20 t, 4E t,
  6F t, 74 t, 20 t, 61 t, 20 t, 31 t, 36 t, 2D t,
  62 t, 69 t, 74 t, 20 t, 53 t, 55 t, 42 t, 4C t,
  45 t, 51 t, 20 t, 56 t, 4D t, 0D t, 0A t,
[then]

err-str 2/ tvar err-str-addr

\ This prints the error message if we are not on the
\ right machine width, 16-bit SUBLEQ machines allowed only.
\ The test and jump to here is in the "start" routine.
\

assembler.1 +order
label: die
   err-str-addr r1 MOV
   err-str r0 MOV
   begin
     r0
   while
     r0 DEC
     r1 INC
     r4 r1 iLOAD
     r4 PUT
   repeat
   HALT

\ Here is the "start" routine, we set the system entry point
\ to the location in the label, do the cell bit-width test
\ (and jump to the failure handler if we are a system with
\ the wrong bit-width), set up the stacks and get the first
\ Forth VM instruction to execute. We then fall into the
\ Forth VM proper, no need to jump to it as it is the next
\ instruction after "start" is finished.
\
\ The checksum code could be moved here to enable (nearly)
\ the entire image to be checked, it would not be too
\ difficult to do.
\

label: start         \ System Entry Point
  start 2/ entry t!  \ Set the system entry point

\ This routine doubles "w" until it becomes negative, which
\ will happen on twos compliment machines upon reaching the
\ maximum bit-width.
\
\ This routine really should be much closer to the beginning
\ of the file, before (many of, but not all of) the variable
\ declarations as we have a limited number of bytes to work
\ with (bytes that are addressable that is) when on machines
\ with smaller bit widths than 16.
\

  r1 ONE!
  r0 ONE!
label: chk16
  r0 r0 ADD                        \ r0 = r0 * 2
  r1 INC                           \ r1++
  r0 +if chk16 JMP then            \ check if still positive
  bwidth r1 SUB r1 if die JMP then \ r1 - bwidth should be zero

\ Back to setting up registers
\

  {sp0} {sp} MOV     \ Setup initial variable stack
  {rp0} {rp} MOV     \ Setup initial return stack
  {cold} ip MOV      \ Get the first instruction to execute
  ( fall-through )

\ This is the Forth Virtual Machine itself. A very short
\ routine. It just needs to call, or jump, depending on the
\ instruction it is given.
\

label: vm ( Forth Inner Interpreter )
  ip r0 MOV          \ Move Instruction Pointer To Working Ptr.
  ip INC             \ IP now points to next instruction!
  r1 r0 iLOAD        \ Get actual instruction to execute
  r1 r0 MOV          \ Copy it, as SUB is destructive
  primitive r1 SUB   \ Check if it is a primitive
  r1 -if r0 iJMP then \ Jump straight to VM functions if it is
  ++rp               \ If it wasn't a VM instruction, inc {rp}
  ip {rp} iSTORE     \ and store ip to return stack
  r0 ip MOV vm a-optim \ "w" becomes our next instruction
  vm JMP             \ Ad infinitum...

assembler.1 -order

\ This meta-compiler word, ";a", used to end the definition of
\ assembly word, requires that the VM has been implemented
\ first, as it jumps back to it, so we define it here instead
\ of using a forward reference (which we could have used, but
\ have studiously avoided as many forward references as is
\ possible).
\

:m ;a (fall-through); vm a-optim vm JMP ;m

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
\ in Forth. The eForth model just has about thirty primitives,
\ which we will aim to emulate, other Forth implementations
\ have hundreds, some pedagogical ones have fewer.
\
\ If this was on a more conventional architecture we would want
\ about thirty base primitive instruction to implement a
\ reasonably efficient Forth, we would want assembly routines
\ for things like multiplication, division, basic stack
\ manipulation, and the like. We will need more just to
\ maintain some level of speed. Although it should be noted
\ that eForth eschews implementing multiplication and division
\ in terms of assembly primitives to keep the system much more
\ portable (as those routines can be tricky to implemented
\ correctly), writing them instead in Forth.
\
\ If we implement a multiplication routine in Forth it will
\ have to go through the virtual machine, if we do it in
\ assembly it will not, it will still be slow as
\ we are implementing it in terms of subtraction, however it
\ be much fast than the Forth version. The number of
\ instructions in the VM is sort of hidden from the view
\ of the programmer, for example the "MOV" macro word is
\ comprised of four SUBLEQ instructions, an indirect load
\ more, each VM cycle takes multiples of those macros, so you
\ end up spending more time in the VM than you think you would.
\
\ Other routines like "pause", "exit", or "opEmit", have to be
\ implemented as VM instructions as doing so otherwise would
\ be too tricky or impossible.
\
\ We have around forty primitives, more than is ideal, but it
\ is necessary given the nature of this system. Performance
\ critical words have to be implemented as primitives
\ otherwise the system would be far too slow.
\
\ It is partly a matter of philosophy and partly a matter of
\ engineering concerns as to what goes where. It is best to
\ keep the set of primitives as minimal as possible, as if this
\ Forth were to be ported to a new platform, all of these
\ routines would have to be rewritten. (It "is best" because
\ the author values portability).
\
\ One of the reasons eForth was so portable is because there
\ were a small set of primitive words that it required, most
\ of eForth was written in eForth, a higher level and more
\ portable language than assembly is.
\
\ ## The Assembly Primitives in Detail
\
\ Each of the virtual machine instructions written in assembly,
\ or primitives, will be described, some are trivial, others
\ less so. They will be acting upon the data (or variable)
\ stacks and the "tos" register, which is an optimization. The
\ "tos" variable contains the top of the variable stack, or the
\ first item on it. It means fewer loads and stores have to be
\ done to access the values on the variable stack.
\
\ There are many ways in which these primitives could be
\ optimized, both in terms of size and speed, but they are
\ current in a "good enough" state.
\
\ "bye" is nothing special, it just calls "HALT".
\
:a bye HALT (a);    ( -- : HALT system )

\ The following instructions are nothing special, just simple
\ stack manipulation functions, they implement the following
\ Forth functions, and are as expected by a Forth programmer:
\
\        opSwap  -> swap ( x y -- y x )
\        opDup   -> dup  ( x -- x x )
\        opToR   -> >r   ( x --, R: -- x )
\        opFromR -> r>   ( -- x, R: x -- )
\        opDrop  -> drop ( x -- )
\
\ There is nothing much to say about them, but see if you can
\ understand how they work. The stack effect comments describe
\ them in their entirety.
\
:a opSwap tos r0 MOV tos {sp} iLOAD r0 {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a ( n -- n n )
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opToR ++rp tos {rp} iSTORE (fall-through);
:a opDrop tos {sp} iLOAD --sp ;a ( n -- )

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
\ can address less memory than is potentially available to the
\ SUBLEQ machine. The SUBLEQ machine can address approximately
\ 65536 cells, or 128kiB, however the Forth can address
\ 65536 bytes, or 64kiB. Note the word "approximately", some
\ addresses (such as "-1") are treated specially and are not
\ available as memory locations.
\
:a [@] tos tos iLOAD ;a
:a [!] r0 {sp} iLOAD r0 tos iSTORE --sp t' opDrop JMP (a);

\ "opEmit" (and formerly "opKey") perform the I/O functions,
\ of which there are only two support by the SUBLEQ
\ architecture, get an octet of input and output one. Only
\ one of those, output, is coded in assembly and that is
\ "opEmit". It is possible to implement input directly in
\ Forth with the "\[@\]" primitive.
\
\ "opEmit", called by "emit", outputs a single byte. It is
\ always blocking. "opKey" (no longer used) accepts a single
\ byte of input and returns negative on error. Depending on the
\ implementation the negative can mean "no byte has been
\ received yet" (it is non-blocking) or it can mean "End Of
\ Input". There are option bits in the "{options}" variable
\ for controlling the behavior of "key", defined later on,
\ which uses the instruction "opKey".
\
\ A non-blocking version of "opEmit" would require a virtual
\ machine change, not a radical one, but just information on
\ whether the operation succeeded. The change in the virtual
\ machine necessary to do this is present in the appendix
\ containing the virtual machine (at the end of the
\ non-blocking input version). This would also require
\ a change in "opEmit" and would render this SUBLEQ machine
\ different to the majority of SUBLEQ machines out there.
\
\ These two instructions provide the only real interaction
\ with the outside world, and you can do a lot with just that.
\
\ Note that "opDrop" is used by "opEmit" to save on some space
\ with an unconditional jump, "(a);" is used instead of ";a"
\ as ";a" writes in another unconditional jump which is no
\ longer needed.
\
\ "opKey" is commented out, as it is possible to implement it
\ in Forth code, which saves on space, by loading from address
\ "-1" with "\[@\]". The former assembly definition of "opKey"
\ is as follows:
\
\        :a opKey ++sp tos {sp} iSTORE tos GET ;a ) ( -- n )
\
:a opEmit tos PUT t' opDrop JMP (a);

\ "opExit", which is used to make "exit", deserves some
\ explanation, it implements a "return", as part of the
\ call/returns you find in most instruction sets. It restores
\ execution to the cell after the call was made by moving
\ what "{rp}" points to into "ip" and then decrementing the
\ return stack pointer for the next return or call.
\
\ Aliases for this word include "unnest", "exit", "return".
\
\ Some Forth implementations also do some optimizations when
\ compiling an exit into the dictionary (this one does not).
\ The H2 CPU from <https://github.com/howerj/forth-cpu> Forth
\ implementation does more advanced optimizations on exit
\ as allowed by the instruction set (such as merging exit with
\ the previous instruction if possible), however most Forth
\ implementations can at least perform a tail-call
\ optimization (again, this version does not do this for
\ simplicities sake). The optimization is simple, when
\ you have a Forth word like:
\
\        : x a b c ;
\
\ The words "a", "b" and "c" are all function calls, however
\ the last call to "c" could instead be made to be a simple
\ jump assuming that "c" is itself a normal Forth function,
\ "c" will instead call "opExit". This is sometimes faster
\ (a direct jump usually being faster than a function call)
\ but it always saves on return stack space as the return
\ site does not have to be pushed to the return stack.
\
\ The way this would be implemented is the word that will
\ compile "opExit" into the dictionary would need to look at
\ the previously compiled word (and perhaps some
\ meta-information) and check if it is a function call, if so,
\ it can change it to a jump. We will not do this optimization
\ in this Forth. The optimizer has to either be aware of cells
\ that might look like calls but are not (think compiled
\ numbers) and also be aware not to cross control structure
\ boundaries, or alternatively the programmer has to
\ selectively apply the optimization themselves which is
\ error prone.
\
\ These next words are for stack manipulation, mainly return
\ stack manipulation like "opExit", words that warrant more
\ explanation than the implementations of instructions like
\ "swap", mainly due to what they are used for.
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
\ calls, as if you called them the call location would be
\ in the way of their operations.
\
\ "rp@" and "rp!" can get and set the return stack pointer
\ directly, they are most useful for the throw/catch mechanism
\ described later on.
\
\ Note: "opExit" falls-through into "rdrop" to save on space.
\

:a opExit ip {rp} iLOAD (fall-through); ( R: a -- )
:a rdrop --rp ;a ( R: u -- )
:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a ( R: u --, -- u )
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a ( R: ???, -- u )
:a rp! tos {rp} MOV t' opDrop JMP (a); ( u -- R: ??? )

\ "sp@" and "sp!" do for the variable stack what "rp@" and
\ "rp!" do for the return stack, they can set them to arbitrary
\ locations. "sp@" can be defined in Forth, and is defined
\ later, "sp!" must be defined in assembly.
\
\ "sp@" is more useful of the two, they are both used in
\ throw/catch, but "sp@" is also used for words like "pick"
\ and for checking the depth of the variable stack, it is
\ also useful for error checking and debugging purposes.
\

:a sp! tos {sp} MOV ;a ( u -- ??? )

\ "opJump" and "opJumpZ" implement unconditional and
\ conditional jumps respectively. The actual jump is performed
\ by "ip ip iLOAD", as "ip" has been incremented before the
\ instruction has been called it points to the next cell,
\ which is used to store the jump location.
\
\ These instructions are used to implement the
\ control structures "if", "begin", "again", "until", "repeat",
\ along with other conditional statements. As shown later
\ patching the binary and computing addresses will need to be
\ done in order to make these instructions useful.
\
\ "opPushZ" does the same as "opJump" but it does it
\ conditionally, it pulls a value off of the variable stack
\ and performs the jump if it zero.
\
\ As an example,
\
\        : example if 2 2 + . then 3 3 + . ;
\
\ Will be compiled to something that looks like this:
\
\
\        <word header not shown>
\         0: opJumpZ
\         1: 12
\         2: (push)
\         3: 2
\         4: (push)
\         5: 2
\         6: address of +
\         7: address of .
\         8: (push)
\         9: 3
\        10: (push)
\        11: 3
\        12: address of +
\        13: address of .
\        14: opExit
\
\ Where the address along the side are cell addresses.
\
\ The way code is generated in Forth is slightly unique and
\ interesting, and will be covered in the control statement
\ chapter.
\
\ Note that opJumpZ falls-through to "opJump" (conditionally)
\ to save on space. "opNext" also uses "opJump", which will
\ be defined next.
\

:a opJumpZ ( u -- : Conditional jump on zero )
  r2 ZERO
  tos if r2 NG1! then tos DEC tos +if r2 NG1! then
  tos {sp} iLOAD --sp
  r2 if ip INC vm JMP then (fall-through);
:a opJump ip ip iLOAD ;a ( -- : Unconditional jump )

\ "opNext" is the only other control structure instruction
\ that is needed, apart from "opJump" and "opJumpZ". It should
\ be noted that the exception mechanism of throw/catch does
\ not need to be implemented as an instruction, which is also
\ part of the control structure word set even if it is not
\ thought as being so. For the sake of efficiency "opNext"
\ is implemented as an instruction. It could actually be
\ implemented in higher level Forth using return stack
\ manipulation.
\
\ It is used as part of the definite looping mechanism
\ available in eForth, which is the "for...next" construct, it
\ is odd construct, but easy to implement and fast. Much like
\ "opJump" a jump destination follows the "opNext" instruction.
\
\ The "for...next" loop if given N will run for N + 1 times,
\ counting backwards from N until 0 is reached on the final
\ loop.
\
\ "for" does little, "next" does all of the work, it jumps
\ back to after the "for".
\
\ As an example:
\
\        : example for r@ . next ;
\
\ Would compile to something like this:
\
\        0: opToR
\        1: r@ instruction
\        2: address of .
\        3: opNext 1
\
\ And if run with 2, such as "2 example", would produce the
\ output "2 1 0".
\
\ "opJump" is reused by "opNext" to save space.
\

:a opNext r0 {rp} iLOAD ( R: n -- | n-1 )
   r0 if r0 DEC r0 {rp} iSTORE t' opJump JMP then
   ip INC --rp ;a

\ The comparison operators are tricky to get right, we build
\ upon "leq0" and "op0=", which are relatively easy to get
\ correct, in the Forth code to fully implement "\<" and later
\ "u\<". Note that the assembly versions of "if" hide a version
\ of "0=" in them that does not quite work for all values, so
\ we have to correct for that in our definition of "op0=".
\
\ In Forth booleans are represented by "0" and "-1" (or all
\ bits-set) instead of "0" and "1" in languages like C. In
\ Forth this allows the returned boolean to be used as a mask
\ with the logical operators. "0=" returns "0" and "-1" as
\ it implements the standard Forth word "0=", "leq0" however
\ returns "0" and "1" as a non-standard Forth word, this is
\ so "leq0" can be used on the output of "leq0", which it could
\ not be if standard Forth booleans were returns by it.
\

:a op0= ( n -- f : not equal to zero )
   tos r0 MOV tos NG1!
   r0 if tos ZERO then r0 DEC r0 +if tos ZERO then ;a
:a leq0 ( n -- 0|1 : less than or equal to zero )
  Z tos 2/ t, there 2/ 4 + t,
  tos 2/ dup t, t, vm 2/ t,
  tos ONE! ;a

\ Subtraction and addition need no real explanation, just note
\ that they are relatively fast to execute on this machine.
\
\ The both jump to "opDrop" to finish the job.
\
:a - tos {sp} iSUB t' opDrop JMP (a);
:a + tos {sp} iADD t' opDrop JMP (a);

\ "rshift" is implemented as a virtual machine instruction,
\ but "lshift" is not as it can be implemented in Forth
\ with little performance loss compared to "rshift", division
\ by even a power of two is slow on a SUBLEQ machine.
\
\ It works bit by bit, shifting right by a variable number of
\ bits, building up the results one bit at a time and doubling
\ the "x" and "tos" registers.
\
\ A consequence of how this "rshift" works is that it is faster
\ the more bits it shifts by, it takes the longest to shift by
\ a single bit as the result is produced in reverse order.
\
\ As is common for all bitwise operations on the SUBLEQ
\ machine barring "invert", they are expensive to compute. If
\ the SUBLEQ machine could have any extra instructions a
\ bitwise multiplexor and left and right shifts would be them.
\ You could gain back a lot in terms of efficiency just from
\ those three extra additions (although another contender would
\ be load and store instructions).
\
\ "rshift" works by looping for each bit in a 16-bit value less
\ one bit, and it tests whether the topmost bit is set (a
\ cheap operation on twos compliment SUBLEQ machines, as the
\ top bit is set when the value is negative).
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
\        tos      x
\                 0000 ( x starts as 0 )
\        1010     0001
\        0100     0010
\        1000     0101
\
\ In each cycle, "x" is first doubled, but as it starts as
\ zero, this is has no effect initially, if the top most bit of
\ "tos" is non-zero then 1 is added to "x", then "tos" is
\ doubled, until completion.
\
\ The algorithms for "AND", "OR", and "XOR" are similar.
\ As are left and right shifts by "N" places, although they are
\ now included in the appendix, "opMux" however uses the same
\ tricks.
\

:a rshift
  bwidth r0 MOV
  tos r0 SUB
  tos {sp} iLOAD --sp
  r1 ZERO
  begin r0 while
    r1 r1 ADD
    tos r2 MOV r3 ZERO
    r2 -if r3 NG1! then r2 INC r2 -if r3 NG1! then
    r3 if r1 INC then
    tos tos ADD
    r0 DEC
  repeat
  r1 tos MOV ;a

\ This single primitive, "opMux", implements bitwise
\ multiplexing, also known as "mux". It is a Universal Gate,
\ much like NAND and NOR are (well, it is universal if we have
\ access to the constants zero and one).
\
\ Previous iterations of this project implement the bitwise
\ operators in full, in SUBLEQ assembly, and those
\ implementations along with the description are still
\ available in the appendix.
\
\ However implementing "and", "or" and "xor" takes up a lot
\ of space. We could implement all the other logical operators
\ in terms of "and" or "or" in combination with "invert", but
\ a more elegant way (in the authors opinion) is to use "mux".
\
\ The Forth code that implements those operators using "mux"
\ is shown later on, this section will describe how "mux" is
\ implement, and what it does. Unlike the normal boolean
\ operators multiplex takes three arguments, which we will
\ call "a", "b" and "sel" ("sel" being short for select).
\
\ The "mux" operator is applied to each bit, much like the
\ bitwise operators.
\
\ The following truth table describes the "mux" operator:
\
\      +-----+-----+-----+-----+
\      |  A  |  B  | SEL | OUT |
\      +-----+-----+-----+-----+
\      |  0  |  0  |  0  |  0  |
\      |  0  |  1  |  0  |  1  |
\      |  1  |  0  |  0  |  0  |
\      |  1  |  1  |  0  |  1  |
\      |  0  |  0  |  1  |  0  |
\      |  0  |  1  |  1  |  0  |
\      |  1  |  0  |  1  |  1  |
\      |  1  |  1  |  1  |  1  |
\      +-----+-----+-----+-----+
\
\ It is possible to describe a "mux" where "sel" is inverted:
\
\      +-----+-----+-----+-----+
\      |  A  |  B  | SEL | OUT |
\      +-----+-----+-----+-----+
\      |  0  |  0  |  0  |  0  |
\      |  0  |  1  |  0  |  0  |
\      |  1  |  0  |  0  |  1  |
\      |  1  |  1  |  0  |  1  |
\      |  0  |  0  |  1  |  0  |
\      |  0  |  1  |  1  |  1  |
\      |  1  |  0  |  1  |  0  |
\      |  1  |  1  |  1  |  1  |
\      +-----+-----+-----+-----+
\
\ It behaves much the same.
\
\ "opMux" is used to build "mux", which is a non-standard Forth
\ word and thus is put in the "system" vocabulary, you must
\ load the "system" vocabulary before using the "mux" word.
\
\ Some examples of the word:
\
\        hex
\        5 A F mux ( returns 5 )
\        5 A 0 mux ( returns A )
\
\ You can see that the operator is quite simple, and it is
\ descriptive, it multiplexes between two values.
\
\ Multiplexors are more familiar to electronic engineers than
\ to programmers, and especially those familiar with the
\ primitive 7400 series logic devices or FPGAs.
\
\ You can implement the other logical operators as mentioned,
\ for example you can implement "or" by setting the "sel"
\ input to be a duplicate of one of the inputs, this is all
\ shown later.
\
\ On to how the code works, much like "rshift", it operates
\ bit by bit, checking the topmost bit, if the value is
\ negative then we known that the top bit is set (or the
\ entire value is zero, which we must account and correct for).
\
\ We can shift bits to the left by adding the value we want to
\ shift to itself, which doubles it.
\
\ Using the topmost bit check and the doubling technique we
\ have a way accomplish what we need to do. Although this will
\ only work on SUBLEQ machines that implements twos compliment
\ arithmetic (it may fail on other signed arithmetic machines,
\ and it will fail on machines with arbitrary precision
\ arithmetic).
\

:a opMux
  bwidth r0 MOV
  r1 ZERO
  r3 {sp} iLOAD --sp
  r4 {sp} iLOAD --sp
  begin r0 while
    r1 r1 ADD

    tos r5 MOV r6 ZERO
    r5 -if r6 NG1! then r5 INC r5 -if r6 NG1! then

    r6 -if
      r4 r7 MOV r5 ZERO
      r7 -if r5 ONE! then r7 INC r7 -if r5 ONE! then
      r5 r1 ADD
    then
    r6 INC
    r6 if
      r3 r7 MOV r5 ZERO
      r7 -if r5 ONE! then r7 INC r7 -if r5 ONE! then
      r5 r1 ADD
    then

    tos tos ADD
    r3 r3 ADD
    r4 r4 ADD
    r0 DEC
  repeat
  r1 tos MOV ;a

\ "opDivMod" is purely here for efficiency reasons, it really
\ improves the speed at which numbers can be printed, which
\ would be too slow if "um/mod" was used. Printing numbers
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
\ loop has finished and pushes the result. Although the
\ algorithm itself is slow, the assembly version is much
\ faster than "um/mod".
\
\ Removing "opDivMod" does save on some space, as would
\ removing "rshift", and implementing their functionality in
\ pure Forth, but there is a significant slow down, it might
\ be best to allow them to be compiled out with a meta-compile
\ time flag.
\
\ This could be rewritten to use long division, which would
\ be larger, but faster, with the pseudo-code:
\
\        if D = 0 then throw() end
\        Q := 0 -- Initialize quotient and remainder to zero
\        R := 0
\        -- Where n is the number of bits in N
\        for i := n - 1 .. 0 do
\          R := R << 1 -- Left-shift R by 1 bit
\          -- Set the least-significant bit of R
\          -- equal to bit i of the numerator
\          R(0) := N(i)
\          if R >= D then
\            R := R - D
\            Q(i) := 1
\          end
\        end
\
\ (from <https://en.wikipedia.org/wiki/Division_algorithm>)
\
\

:a opDivMod
  r0 {sp} iLOAD
  r2 ZERO
  begin
    r1 ONE!
    r0 -if r1 ZERO then
    r1
  while
    r2 INC
    tos r0 SUB
  repeat
  tos r0 ADD
  r2 DEC
  r2 tos MOV
  r0 {sp} iSTORE ;a

\ ### PAUSE
\
\ "pause" is simple, but explaining its usage is more
\ complex, it is a word that is implemented in assembly
\ because it needs to be. It is at the core of the cooperative
\ multitasking system that this implementation of Forth has.
\
\ Despite the underlying SUBLEQ machine being spartan,
\ it is possible to implement or approximate peripherals
\ and other systems.
\
\ We will see that later on with the delay loop based "ms", and
\ with the fake Forth Block system. "pause" and the eForth
\ system however implements a usable multitasking system of a
\ type which is more common in embedded control systems than on
\ desktop computers.
\
\ As the multitasking is not preemptive we do not need a way
\ of handling interrupts. The multitasking system has
\ interactions with the Input/Output layer which is blocking in
\ the default C implementation of the SUBLEQ machine, and it
\ also interacts with the USER variables.
\
\ Each Forth task, of which there is guaranteed to be at least
\ one, has its own area in which is can store thread local
\ variables. The system sets up the first thread on boot, and
\ more can be added later. Each task consists of a 1024 byte
\ thread local storage area, which contain the buffers used
\ for the terminal input, the return and variable stacks,
\ the numeric formatting buffer, and an area for the USER
\ variables and some registers.
\
\ The job of "pause" is to switch from one task to another,
\ if another task exists. Task switching can be disabled by
\ setting the variable "{single}" to non-zero.
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
\ It is also called within the "ms" function (which causes
\ problems in its current implementation, the "ms" function
\ can vary depending on settings and only times the amount
\ of time executed within a single thread), and in the Forth
\ Block words (which usually would perform I/O, however the
\ block system is virtual and not backed by massed storage).
\
\ With this VM instruction, USER variables, and other
\ variables, it is possible to make a viable, if primitive,
\ threading model. Making this preemptive would require
\ hardware support and greatly complicate the system.
\
\ To save on space it might be worth rewriting this VM
\ instruction to only do the bare minimum that *has* to be
\ done as a VM instruction, instead performing as many loads
\ and stores in Forth code (which is far more compact) instead.
\

opt.multi [if]
:a pause ( -- : pause and switch task )
  {single} if vm JMP then \ Do nothing if single-threaded mode
  r0 {up} iLOAD \ load next task pointer from user storage
  r0 if
    {cycles} INC        \ increment "pause" count
    {up} r2 MOV  r2 INC \ load TASK pointer, skip next task loc
      ip r2 iSTORE r2 INC \ save registers to current task
     tos r2 iSTORE r2 INC
    {rp} r2 iSTORE r2 INC
    {sp} r2 iSTORE r2 INC
       r0 {rp0} MOV stacksz {rp0} ADD \ change {rp0} to new loc
   {rp0} {sp0} MOV stacksz {sp0} ADD \ same but for {sp0}
       r0 {up} MOV r0 INC  \ set next task
      ip r0 iLOAD r0 INC \ reverse of save registers
     tos r0 iLOAD r0 INC
    {rp} r0 iLOAD r0 INC
    {sp} r0 iLOAD r0 INC \ we're all golden
  then ;a
[else]
:m pause ;m ( -- [disabled] )
[then]

\ The following assembly routine is one way adding support for
\ assembly routines callable from within the Forth interpreter
\ and without relying on hacks. This is the absolute bare
\ minimum, but it would help if the meta-compilation assembly
\ words such as "iLOAD", "ADD", and the like, along with the
\ assembly control structures like "-if", were placed in an
\ assembly vocabulary in the target system. One thing that you
\ see in Forth systems from the 1980s is the liberal use of
\ assembly (usually for the 6502 or Z80 processors) for
\ routines that needed speed, usually control structures or
\ arithmetic operations such as division.
\
\        :a opAsm
\          tos r0 MOV
\          tos {sp} iLOAD --sp
\          r0 iJMP (a);
\
\ The way the VM instruction works is to pull an address off
\ of the stack and perform an indirect jump to it. We cannot
\ directly call assembly routines in our Forth code because
\ of how the Forth VM works, if we compiled an assembly
\ routine into the Forth dictionary the Forth VM would attempt
\ to execute the assembly as a list of addresses of Forth
\ functions and VM routines to jump to, with disastrous
\ results.
\
\ We may want to add some house-keeping to the routine, but as
\ it stands "opAsm" is usable.
\
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
\ words in the target dictionary, for multiple different
\ vocabularies, for the default Forth vocabulary (which
\ contains the standard Forth words), for the root vocabulary
\ (also contains standard Forth words, but the minimal set of
\ words to get back to a "normal" system), for the System
\ vocabulary (containing non-standard words and words used
\ for building the internals), and for the block editor.
\
\ Here are the words and their usage:
\
\ * ":t"/";t":  Define a word in the target dictionary.
\ * ":to": Define a word in the target dictionary, but
\ do not make it available directly by putting it in the search
\ order during meta-compilation, the word is instead put into
\ the "target.only.1" dictionary of the meta-compiler.
\ * ":s"/";s": Define a word in the system dictionary
\ * ":so": The same as ":to", except there is no
\ corresponding "system.only.1" dictionary and it
\ works with the system dictionary.
\ * ":r"/";r": Define a word in the root dictionary.
\ * ":e"/";e": Define a word in the editor dictionary.
\ * "system\[/\]system": Put a group of words within the
\ system vocabulary.
\ * "root\[","\]root": Put a group of words within the
\ root vocabulary.
\
\ The words that define new words in the target dictionary
\ may only be used with the corresponding target dictionary
\ terminating word, that is ":t" and ":to" can only be used
\ with ";t" and not with ";s". This is done with a Forth
\ concept called "compiler security", which are primitive but
\ effective run time checks to make sure control
\ structures match up, or that there are enough items on the
\ stack before executing an operation.
\
\ An example of this is ":s", it pushes the hexadecimal
\ constant "F00D" onto the stack, which ";s" checks for, if
\ it is not present, it prints a message and calls abort. It
\ is unlikely, although possible, that this value will be
\ produced by accident. It is not an actual security mechanism,
\ just a way of detecting programming errors.
\
\ ":t" does the same thing with a different constant, so a
\ mismatch can be detected. This also detects situation where
\ numbers that have meant to compiled are instead left on
\ the stack, or the incorrect number is compiled into a word
\ definition by the meta-compiler.
\
\ An example of this is "lit", which pulls a value off of the
\ stack and compiles into a word definition. We can use this
\ in an example:
\
\        :t example 2 lit 2 lit + . cr ;t
\
\ Which defines a word in the target which will print "4",
\ if instead we defined:
\
\        :t example 2 2 lit + . cr ;t
\
\ Or:
\
\        :t example lit 2 lit + . cr ;t
\
\ The corresponding correct constant would not be given to ";t"
\ and an error would be detected. "lit" and how it works will
\ be defined shortly, as well as its shortcomings and possible
\ workarounds.
\
\ The newly defined defining words all call ":t" (defined much
\ earlier) to do their job. (or ":to" if they do not want to
\ put the word name into the meta-compilers target dictionary).
\ Likewise the words to terminated a word definition all call
\ ";t". The defining words need to modify the meta-compilers
\ view of the target dictionaries so the word is put into the
\ right location.
\
\ And the other words:
\
\ * ":t" creates the word header.
\ * ";t" compiles "opExit" at the end of a word definition.
\

:m munorder target.only.1 -order talign ;m
:m (;t)
   CAFE <> if abort" Unstructured" then
   munorder ;m
:m ;t (;t) opExit ;m
:m :s tlast @ {system} t@ tlast ! F00D :t drop 0 ;m
:m :so  tlast @ {system} t@ tlast ! F00D :to drop 0 ;m
:m ;s drop CAFE ;t F00D <> if abort" unstructured" then
  tlast @ {system} t! tlast ! ;m
:m :r tlast @ {root-voc} t@ tlast ! BEEF :t drop 0 ;m
:m ;r drop CAFE ;t BEEF <> if abort" unstructured" then
  tlast @ {root-voc} t! tlast ! ;m
:m :e tlast @ {editor} t@ tlast ! DEAD :t drop 0 ;m
:m ;e drop CAFE ;t DEAD <> if abort" unstructured" then
  tlast @ {editor} t! tlast ! ;m
:m system[ tlast @ {system} t@ tlast ! BABE ;m
:m ]system BABE <> if abort" unstructured" then
   tlast @ {system} t! tlast ! ;m
:m root[ tlast @ {root-voc} t@ tlast ! D00D ;m
:m ]root D00D <> if abort" unstructured" then
   tlast @ {root-voc} t! tlast ! ;m

\ As ":t" and ";t" are the default actions we want when we
\ are defining new words in the target dictionary, we can make
\ it so that the normal Forth words for defining new functions
\ are what we use instead of ":t" and ";t". Note that ";" is
\ not defined as immediate, this is because we are not doing
\ normal Forth compilation, but are just running exclusively
\ in command mode, except when defining new meta-compilation
\ words with ":m" and ";m". That is ":t" does not change the
\ state variable in the host Forth.
\

:m : :t ;m ( -- ???, "name" : start cross-compilation )
:m ; ;t ;m ( ??? -- : end cross-compilation of a target word )

\ It is interesting to see just how simple and easy it is
\ to define a set of words for creating control structures.
\ Making a compiler is actually easy, making a good
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
\ or one with an inverted test called "unless" like in Perl.
\
\ Note that none of these definitions of control structures
\ are immediate words, because the entirety of the meta
\ compiler runs in command mode.
\
\ The stack effects for these words describe the meta-compiler
\ behavior and not their runtime one, the runtime behavior
\ should be the same as the usual Forth control structures of
\ the same name. The items pushed on to the stack are either
\ addresses to jump to or holes in the dictionary that need to
\ be patched with the correct addresses.
\
\ There are more constructs here than in the assembler
\ versions, but constructs likes "+if...then" and "-if...then"
\ are missing as they are not standard Forth control
\ structures.
\
\ The "for...next" and "for...aft...then...next" control
\ structures are more common in eForth, most Forth
\ implementations use "do...loop" and "do...+loop" which are
\ more difficult to implement, slower, and use more items
\ on the return stack, but easier to use.
\
\ The "for...next" construct keeps a loop counter on the
\ return stack ("for" compiles "\>r" into the dictionary
\ and a jump location after the "\>r"), "opNext" has already
\ been encountered in the Virtual Machine instruction section,
\ but it decrements and tests that loop counter and jumps
\ back to instruction after "\>r" if the counter is positive,
\ if zero or negative it removes the counter from the return
\ stack and continues on after the "next" statement.
\
\ These meta-compiled words do not take locations at cross
\ compile time like the assembler versions do, instead taking
\ variables to test off of the variable stack at run time.
\
\ If you understood the assembly versions, you will understand
\ these versions. Note that these implementations are
\ portable, or have the ability to be ported easy. Other
\ platforms might not need the "2\/" but they all follow the
\ same general pattern; compile in a conditional jump or
\ hole in the dictionary, modify it later on, push locations
\ on to the stack for a word defined later to act on.
\
\ The section on Control Structures has more detail as to how
\ the compilation of these is achieved.
\

:m begin talign there ;m ( -- a )
:m until talign opJumpZ 2/ t, ;m  ( a -- )
:m again talign opJump  2/ t, ;m ( a -- )
:m if opJumpZ there 0 t, ;m ( -- a )
:m tmark opJump there 0 t, ;m ( -- a )
:m then there 2/ swap t! ;m ( a -- )
:m else tmark swap then ;m ( a -- a )
:m while if ;m ( -- a )
:m repeat swap again then ;m ( a a -- )
:m aft drop tmark begin swap ;m ( a -- a a )
:m next talign opNext 2/ t, ;m ( a -- )
:m for opToR begin ;m ( -- a )

\ The following words will be useful for defining control
\ structures within the newly made Forth interpreter image,
\ as they will need to compile in jump instructions into newly
\ defined words. Note that they use the cell address.
\

:m =jump   [ t' opJump  half ] literal ;m ( -- a )
:m =jumpz  [ t' opJumpZ half ] literal ;m ( -- a )
:m =unnest [ t' opExit  half ] literal ;m ( -- a )
:m =>r     [ t' opToR   half ] literal ;m ( -- a )
:m =next   [ t' opNext  half ] literal ;m ( -- a )

\ To avoid conflicts with meta-compiled words and words
\ compiled in the target dictionary some instructions have
\ been given "op" as a prefix. We could have avoided this with
\ dictionary manipulation, but this is easier. We no longer
\ need the word "dup" to actually duplicate the top word on
\ a stack, instead the new "dup" should compile a reference
\ to "opDup" in newly defined words, so we are safe to define
\ a new "dup". The same goes for the rest of these new words.
\

:m dup opDup ;m ( -- : compile opDup into the dictionary )
:m drop opDrop ;m ( -- : compile opDrop into the dictionary )
:m swap opSwap ;m ( -- : compile opSwap into the dictionary )
:m >r opToR ;m ( -- : compile opTorR into the dictionary )
:m r> opFromR ;m ( -- : compile opFromR into the dictionary )
:m 0= op0= ;m ( -- : compile op0= into the dictionary )
:m mux opMux ;m ( -- : compile opMux into the dictionary )
:m exit opExit ;m ( -- : compile opExit into the dictionary )

\ This complete most of the meta-compiler words, new
\ words will be defined later, but that is most of it. The
\ next section we will bring this all together to start to
\ actually define new Forth words within the target Forth
\ image.
\

\ # The Forth (section of the) Image
\
\ We are now ready to define actual Forth words, everything up
\ until now has just been to make an environment that is
\ capable of hosting a Forth system, and about half the size
\ of the generated image is used to create those conditions.
\
\ The first words will be generic utility functions,
\ words the embody a virtual machine instruction, and
\ other miscellaneous things.
\
\ The next section adds all the words that are implemented in
\ a single virtual machine instruction.
\
\ We want to make sure that we use references to the virtual
\ machine instructions when we call functions such as
\ "+" or "xor" when we use them later in the program, so we
\ put those words in the "target.only.1" dictionary to make
\ sure they will not be found, but are still accessible if we
\ need them.
\
\ The definitions looks odd, for example:
\
\        :to + + ;
\
\ Looks like it would be a recursive function that never
\ terminates, however due to the way ":to" defines words the
\ new word definition is not visible when we are filling out
\ the word body, so the second "+" refers to the assembly
\ definition we defined with ":a", or the VM instruction for
\ "+".
\
\ The reason they are placed in the "target.only.1" is because
\ of speed, it is better to call the VM instruction directly
\ than to call a function that will just call the VM
\ instruction.
\
\ There is not much else to say about these words. Some of them
\ go into the system vocabulary, but should also not be
\ referenced later on either.
\

:to + + ; ( n n -- n : addition )
:to - - ; ( n1 n2 -- n : subtract n2 from n1 )
:to bye bye ; ( -- : halt the system )
:to dup dup ; ( n -- n n : duplicate top of variable stack )
:to drop opDrop ; ( n -- : drop top of variable stack )
:to swap opSwap ; ( x y -- y x : swap two variables on stack )
:to rshift rshift ; ( u n -- u : logical right shift by "n" )
:so [@] [@] ;s ( vma -- : fetch -VM Address- )
:so [!] [!] ;s ( u vma -- : store to -VM Address- )
:to sp! sp! ; ( a -- ??? : set the variable stack location )
:to 0= op0= ; ( n -- f : equal to zero )
:so leq0 leq0 ;s ( n -- 0|1 : less than or equal to zero )
:so mux opMux ;s ( u1 u2 sel -- u : bitwise multiplex op. )
:so pause pause ;s ( -- : pause current task, task switch )

\ If "opAsm" is defined, so should this:
\
\        :so asm opAsm ;s
\
\ However, if that is defined, then an entire assembly word-set
\ should be as well, along with easily accessible constants
\ such as the value of "opExit", and the locations of registers
\ should as "tos", "ip", and ways of accessing the stack
\ pointers from assembly.
\

\ One of the earliest tricks I remember being taught when
\ learning to program is shift left by one place is equivalent
\ to adding that number to itself, "2\*" does this, "2\/" will
\ be defined later, it is slightly more complex.
\
: 2* dup + ; ( u -- u : multiply by two )

\ We need a way of pushing a literal value, a number, onto
\ the variable stack, "(push)" does that, formerly "opPush"
\ which was written in SUBLEQ assembly did that, which
\ was defined as so:
\
\        :a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a
\
\ As usual, to save space, "opPush" was recoded in Forth, at
\ the expense of some speed.
\
\ In order to push a literal value onto the variable stack
\ the instruction "(push)" has to manipulate the return stack.
\ Remember when calling a function the topmost return stack
\ variable points to the next cell from which the word
\ was stored. If we store the number in the next cell when
\ compiling the number into a word definition, then by
\ manipulating the return stack value to load from that address
\ and then placing that return address back but incremented
\ over the address of the number we have just loaded, we will
\ have loaded the right number and ensured execution continues
\ from the correct place.
\
\ As an example, the following word "x", pushes "2", then "3"
\ onto the variable stack:
\
\        : x 2 3 ;
\
\ This gets compiled to:
\
\        PUSH:
\                (push)
\                2
\                (push)
\                3
\                exit
\
\ Some other important meta-compiler words will be defined ones
\ for making constants, variables and thread-local variables
\ (called user variables) can now be defined. We will first
\ define three words that will be used by the meta-compiler
\ words we will define (they will also be used much later
\ by the equivalent words defined in the target, in the
\ section on "create" and "does\>"). Those words are "(var)",
\ "(const)" and "(user)", which are used by "variable",
\ "constant" and "user" respectively.
\
\ The bracket words work by manipulating the return stack value
\ doing three things when they are compiled into a new word by
\ "variable", "constant" or "user".
\
\ 1. Getting the address of a cell in which a special value
\ is stored.
\ 2. Optionally doing *something* with that address, such
\ as fetching the contents as with "(const)".
\ 3. Exiting to the caller instead of continue execution, that
\ means these words do not exit their definitions like normal
\ words using "opExit".
\
\ "(up)" does a similar function to "(user)", and is then
\ used in the meta-compiler word "up". It is also similar
\ to "(push)", however it is used to access USER variables
\ instead, that is variables stored in the thread local store,
\ or to think of them another way, variables stored relative to
\ a tasks memory area. They are part of the cooperative
\ multitasking system. This instruction should be understood in
\ conjunction with "tuser", "pause" (defined later on in this
\ section) and the multitasking words defined towards the end
\ of this document.
\
\ Where this function differs to "(push)" is that instead of
\ pushing a literal, it loads a literal, adds it to the current
\ task address stored in "{up}", and pushes that onto the
\ stack. The address should be cell and not byte address, so
\
\        (up) 0
\
\ Would refer to the first thread local cell and push the
\ address of it onto the stack.
\
\

:s (const) r> [@] ;s compile-only ( R: a --, -- u )
:m constant :t mdrop (const) t, munorder  ;m

\ To both save space, and because using "lit" as a postfix is
\ annoying, for the most common constants; 0, 1, and -1, we
\ will define words for them and place them in the system
\ vocabulary with ":s".
\
\ Compiling a number into a word definition takes up two
\ cells, one for "(push)" and another for the value. A
\ reference to a word only takes up one cell, hence the saving.
\ The trade off is that it takes longer to execute and space
\ must be reserved for the words that push those constants.
\
system[
 0 constant #0  ( --  0 : push the number zero onto the stack )
 1 constant #1  ( --  1 : push one onto the stack )
-1 constant #-1 ( -- -1 : push negative one onto the stack )
 2 constant #2  ( --  2 : push two onto the stack )
]system

\ "1+" and "1-" do what they say, increment and decrement
\ respectively. They are defined here as they are needed
\ for the next few definitions.

\ "1+" is needed for our definition "(push)" so is defined
\ here, it also needs the constant '#1' to be defined, as
\ does "1-".
\
: 1+ #1 + ; ( n -- n : increment value in cell )
: 1- #1 - ; ( n -- n : decrement value in cell )

\ "lit" is used to compile a literal into the dictionary, which
\ will be pushed when run. Note again, we are not in command
\ mode when in between ":t" and ";t", and even if we were, it
\ would not do the correct action, the Forth we are running
\ this meta-compiler under would attempt to compile a number
\ given to it into the dictionary, but into its dictionary, and
\ in a way that would not work in the target. We cannot modify
\ the internals of the Forth used to compile this program,
\ (or not the gforth interpreter and not in a portable way)
\ so we are left with having to call "lit" after each number
\ we want to compile into a target word.
\
\ "up" takes a number representing an index into the thread
\ local storage and compiles an instruction into the dictionary
\ that will push a number on to the stack at run time that
\ represents the address of that thread local variable.
\
\ "\[char\]" and "char" have different uses in a running Forth,
\ when meta-compiling there is no command vs compile mode,
\ we use meta-compiler commands to compile into the target
\ always, so for the meta-compiler the words both do the same
\ thing, which is to get a single character from the input
\ stream and compile into the dictionary instructions which
\ push the number representing that character on to the
\ variable stack. The reason both are defined is so that they
\ can be used in the appropriate places where they would be
\ used in normal Forth code, so as to make the code look more
\ like normal Forth code.
\
\ All of these operations take up two cells in the dictionary
\ as they are not just a simple call, but a VM instruction and
\ then the compiled number.
\
\ "variable" like "constant" should be familiar to all Forth
\ programmers, "user" is more esoteric but is the thread local
\ equivalent of allocating a variable, note that these
\ definitions are for the meta-compiler, they will be
\ redefined in the target dictionary using ":to" much later
\ in the section on "create" and "does\>".
\

:s (push) r> dup [@] swap 1+ >r ;s ( -- n : inline push value )
:m lit (push) t, ;m ( n -- : compile a literal )

:s (up) r> dup [@] {up} half lit [@] 2* + swap 1+ >r ;s
  compile-only ( -- n : user variable implementation word )
:s (var) r> 2* ;s compile-only ( R: a --, -- a )
:s (user) r> [@] {up} half lit [@] 2* + ;s compile-only
  ( R: a --, -- u )

:m up (up) t, ;m ( n -- : compile user variable )
:m [char] char (push) t, ;m ( --, "name" : compile char )
:m char   char (push) t, ;m ( --, "name" : compile char )
:m variable :t mdrop (var) 0 t, munorder ;m ( --, "name": var )
:m user :t mdrop (user) local? =cell lallot t, munorder ;m

\ ")" is defined here, it can be used as a "no-operation"
\ instruction. This word will be better described later on.

:to ) ; immediate ( -- : NOP, terminate comment )

\ We need "over" (formerly a VM instruction) to implement
\ some of the logical operators using "mux".
\

: over swap dup >r swap r> ; ( n1 n2 -- n1 n2 n1 )

\ A common expression I find myself typing is "dup \>r",
\ we could define a word that does this using "over" and
\ some return stack manipulation, or with some of the other
\ stack words, however it is more clear (but results in a
\ slightly larger image) to type out the expression in its
\ entirety. There is an element of style into what gets turned
\ into its own word and what does not. As the author cannot
\ think of a good name for the non-standard word
\ ("dupr", or "dup\>r", both of which are not clear) the word
\ will be left out. The names of many of the Forth words
\ could have the same things said against them, it is not
\ clear what they do from their name, however they are standard
\ and only have to be learned once, the same cannot be said
\ for non-standard words meant only for compressing the size
\ of the dictionary, they impose a cognitive burden that may
\ not be worth the trade-off in size.
\
\ All three binary logical operators, excluding "invert",
\ are implementing using "mux" ("invert" could be implemented
\ with "mux" if we needed it to be to save space). This is
\ done by duplicating inputs, inverting them, swapping them,
\ and then feeding them into "mux", it is a very versatile
\ instruction.
\
\ The operation "invert" performs a bitwise invert, the only
\ bitwise operation we can perform easily. It uses the fact
\ that a subtraction using twos-compliment arithmetic is
\ equivalent to the following:
\
\        b - a = b + ~a + 1
\
\ If we would like to invert "a", we must get rid of "b" and
\ the "+1" terms. We perform the subtraction, zeroing "b"
\ first, then we just subtract one.
\
\ The other bitwise operations will be much more difficult
\ to implement. When making the system for the first time,
\ getting those bitwise operators correct was the most onerous
\ task of bringing the system up, as a bug in those operators
\ makes everything else more difficult to debug.
\

: invert #0 swap - 1- ;           ( u -- u : bitwise invert )
: xor >r dup invert swap r> mux ; ( u u -- u : bitwise xor )
: or over mux ;                   ( u u -- u : bitwise or )
: and #0 swap mux ;               ( u u -- u : bitwise and )

\ "2/", or dividing by two, is a very common operation, so it
\ useful to have a word that does this. This Forth implements
\ it by a simple right shift by one, which is not standards
\ compliant (there is a different between right shift,
\ arithmetic right shift and division that many do not care
\ about and get confused between).
\
\ "2/" is used to convert from a SUBLEQ VM address to an
\ address understandable in Forth.
\
\ "2\*" has a much simpler definition, instead of being defined
\ in terms of "lshift" (which has not yet been defined) it
\ just adds the number to be doubled to itself, as previously
\ shown.
\
: 2/ #1 rshift ; ( u -- u : divide by two )

\ Notice how "@" and "!" divide the address by two, which drops
\ the lowest bit that is used to select the upper or lower
\ byte in a word. Division is expensive to compute, so it is
\ better to use "\[@\]" and "\[!\]" where possible as the calls
\ to them will faster and use less return stack space.
\
\ However, from the point of view of the developer, using
\ "@" and "!" is easier and more portable.
\
: @ 2/ [@] ; ( a -- u : fetch a cell to a memory location )
: ! 2/ [!] ; ( u a -- : write a cell to a memory location )

\ A non-destructive version of '@', it preserves the address
\ it uses. Many Forth words would benefit from having non
\ destructive analogues that keep their input arguments intact,
\ such as "if".
\
:s @+ dup @ ;s ( a -- a u : non-destructive load )

\ ### Hook Variables
\
\ These words are just used to push the address of a variable
\ onto the stack. Some of them are local variables (using "up")
\ and others are global addresses (using "lit").
\
\ Note which variables are USER variables and which are just
\ normal memory locations.
\
\ "\<ok\>", "\<emit\>", "\<echo\>", "\<literal\>" and
\ "\<cold\>" are used for system hooks. This allows the words
\ they are used in to change at run time. For example, "\<ok\>"
\ contains the execution token used to print the "ok" prompt,
\ which we have already encountered. By changing that execution
\ token we can change that prompt. For example:
\
\        : prompt cr ." ok>" ;
\        ' prompt <ok> !
\
\ Will print a prompt that says "ok\>" after each line is
\ executed, regardless of the interpreter state, other prompts
\ include:
\
\        : prompt source type ."  ok" cr ;
\        : prompt .s ."  ok" cr ;
\
\ The first will print out each line that is input which can
\ be useful for debugging errors when text is redirected into
\ the interpreter, and the second will print out the variable
\ stack at the end of each line, another useful debugging aid.
\
\ "\<cold\>" is a normal variable and not a USER variable as it
\ needs to be available before the first thread is set up, it
\ contains the execution token of the first Forth word to be
\ executed.
\
\ "\<ok\> is in the normal Forth vocabulary despite it being a
\ non-standard word, this is because we need to silence the ok
\ prompt at the start of the script to ensure our output is
\ not corrupted, gforth does not need this at it detects
\ whether it is reading from a terminal that a user is typing
\ into or a file (such cleverness is not necessarily a good
\ thing).
\
\ It is not a hard rule, but usually hook variables have the
\ "\<\>" brackets enclosing them, and the functionality is
\ provided by a word with a name like "(name)", the word that
\ executes the hook will be called just "name".
\
\ NB. "{cold}" should contain a cell address, not a Forth
\ address!
\
\ That is,
\
\        ' (cold) <cold> !
\
\ Will not work, this works for setting most execution vectors,
\ but not this one, "{cold}" needs to be executed by the
\ Forth Virtual Machine, it cannot divide a number by two
\ efficiently (or at all, and would slow everything down if
\ implemented), so it only operates on cell address. This is
\ a minor quirk of this implementation and should not be the
\ case on other systems.
\
\ The following should work:
\
\        ' (cold) 2/ <cold> !
\

user <ok> ( -- a : okay prompt xt loc. )

system[
  user <emit>    ( -- a : emit xt loc. )
  user <key>     ( -- a : key xt loc. )
  user <echo>    ( -- a : echo xt loc. )
  user <literal> ( -- a : literal xt loc. )
  user <tap>     ( -- a : tap xt loc. )
  user <expect>  ( -- a : expect xt loc. )
  user <error>   ( -- a : <error> xt container. )
]system

:s <cold> {cold} lit ;s ( -- a : cold xt loc. )

\ ### Forth Variables
\
\ There are a lot of variables in this part of the code,
\ much like at the beginning of the image, and they will all
\ be explained again.
\
\ "current" and "root-voc" are to do with word vocabularies.
\ The "root-voc" contains the minimal Forth vocabulary, hence
\ why "root" is part of its name. It contains only the
\ words "eforth", "words", "only", "forth", "system",
\ "forth-wordlist" and "set-order".
\
\ "current" contains the vocabulary for which newly defined
\ words are to be added to. An example usage is in the meta
\ compiler, It is used to place words in the "target.1",
\ "meta.1", "assembler.1" and "target.only.1" vocabularies.
\ "current" is not usually used directly however, but is used
\ by words like "definitions".
\

: current {current} lit ; ( -- a : get current vocabulary )
: root-voc {root-voc} lit ; ( -- a : get root vocabulary )

\ The word "this" allows us to access the USER task area,
\ it pushes the pointer to that area onto the stack. The
\ USER task is a 1024 byte block of memory, mentioned
\ previously, that has multiple stacks, buffers and variables
\ in it.
\
\ "this" can be use to get the task ID (which is the same as
\ the tasks address), it can also be used for the
\ implementation of the word "pad". The pad location is an area
\ 960 bytes into the task area which is meant as a programmer
\ utility that should contain a *small* section of memory for
\ temporary usage.
\
: this 0 up ; ( -- a : address of task thread memory )
: pad this 3C0 lit + ; ( -- a : index into pad area )

\ More vocabulary words up next, "#vocs" contains the maximum
\ number of possible vocabularies in the vocabulary list, while
\ "context" gets a pointer to the area used to store the
\ vocabulary array, the first cell will contain the first
\ vocabulary in the word list (or the wordlist that will get
\ searched in first).
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
\ "hld" also belongs with numeric I/O with "dpl", "\>in" with
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
\        decimal
\        : .hex base @ >r 16 base ! . r> base ! ;
\
\ It is common to see the expressions "base @ \>r" and
\ "r\> base !" in Forth words definitions that deal with
\ number printing.
\
\

variable blk ( -- a : loaded block )
variable scr ( -- a : latest listed block )

2F t' scr >tbody t!

user base  ( -- a : push the radix for numeric I/O )
user dpl   ( -- a : decimal point variable )
user hld   ( -- a : index to hold space for num. I/O)
user state ( -- f : interpreter state )
user >in   ( -- a : input buffer position var )
user span  ( -- a : number of chars saved by expect )

$20 constant bl ( -- 32 : push space character )

system[
       h constant h?  ( -- a : push the location of dict. ptr )
{cycles} constant cycles ( -- a : number of "cycles" ran for )
    {sp} constant sp ( -- a : address of v.stk ptr. )
  {user} constant user? ( -- a : )
         variable calibration F00 t' calibration >tbody t!
]system

:s radix base @ ;s ( -- u : retrieve base )
: here h? @ ; ( -- u : push the dictionary pointer )

\ As mentioned, "sp@" is defined in Forth, and it is defined
\ here. It retrieves the variable stack position, and pushes
\ it on to the variable stack.
\
: sp@ sp @ 1+ ; ( -- a : Fetch variable stack pointer )

\ To make switching bases easier the words "hex" and "decimal"
\ are made available, which set the numeric input and output
\ radix (also known as a base) to sixteen and ten respectively.
\
\ One problem with setting the base is knowing what base you
\ are in at the time, if you want to print out a number in
\ hexadecimal you could use the expression:
\
\        16 base !
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
\        decimal
\        : octal 8 base ! ;
\        : binary 2 base ! ;
\
\ You could use higher bases as a data interchange format,
\ allowing binary data to be transferred as text with a
\ processing and storage overhead. Bases 32 and 36 have
\ advantages, as does base 16, explained in a little bit. The
\ lower the base the less dense the resulting string, which
\ means more overhead.
\
\ Base-64 is the most common way of encoding binary strings
\ as text, but "base" cannot be set that high as there is no
\ real sensible encoding that follows from going higher than
\ the ten digits plus the alphabet, there would be
\ ambiguities if we did. You could add it as a special case,
\ if you so desired.
\
\ Base-32 has the advantage that it is a power two base, so
\ a CODEC for it *could* be more efficiently implemented with
\ just shifts and masking (no multiplication is required),
\ Base-36 has the advantage that is the most dense encoding
\ allowed in this scheme, but the CODEC will always be slower.
\
\ Base-16 is less dense than Base-32 but is easier to process,
\ as two sets of 4 bits fit into a byte, meaning no dead bits,
\ but it is much less space efficient overall. It is the least
\ dense still viable encoding scheme.
\

: hex  $10 lit base ! ; ( -- : change to hexadecimal base )
: decimal $A lit base ! ; ( -- : change to decimal base )

\ ## Command and Compile with "\[" and "\]"
\
\ "\]" and "\[" are two, simple, words, that require a
\ lot more context to understand properly. Note one of them
\ is immediate as well, that's important!
\
\ The words are often used within other word definitions to
\ get back into command mode temporarily. For example:
\
\        : x 2 2 + . cr ;
\        : y [ 2 2 + ] literal . cr ;
\
\ Both print the same result, however "y" is smaller and
\ executes faster (not that it matters for such a short word).
\
\ That is because "y" computes the value it will print out
\ at compile time, and "x" does it at run time. They will
\ look like this:
\
\
\        X:
\                opPush 2
\                opPush 2
\                address of +
\                address of .
\                address of cr
\                opExit
\
\        Y:
\                opPush 4
\                address of .
\                address of cr
\                opExit
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
\ Other eForth models instead store execution vectors for
\ compilation and interpreting within the "state" variable,
\ this does make the system more flexible, but is not done here
\ for the sake of size.  "\<interpret\>" and "\<compile\>" are
\ two hooks, including others, that have been cut from this
\ implementation of eForth.
\
\ There is a trade-off, you could potentially put a
\ hook in for every single defined word, but that would be too
\ cumbersome, if we had more room then those two hooks would
\ be added, along with modifications to "evaluate" so they
\ are used. -1 and 0 would still need to be stored in the
\ "state" variable, as that variable is part of the standard
\ word set, and must contain only "-1" and "0" depending on the
\ interpreter state.
\

: ] #-1 state ! ; ( -- : return to compile mode )
: [  #0 state ! ; immediate ( -- : initiate command mode )

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
\ "shed" is a Forth word of my own design, by searching for
\ synonyms for "drop" I decided upon this word, which removes
\ the third item on the stack. It currently uses as much space
\ as it saves, so it is kept in.
\

: nip swap drop ;   ( x y -- y : remove second item on stack )
: tuck swap over ;  ( x y -- y x y : save item for rainy day )
: ?dup dup if dup then ; ( x -- x x | 0 : conditional dup )
: rot >r swap r> swap ; ( x y z -- y z x : "rotate" stack )
: -rot rot rot ; ( x y z -- z x y : "rotate" stack backwards )
: 2drop drop drop ; ( x x -- : drop it like it is hot )
: 2dup  over over ; ( x y -- x y x y )
:s shed rot drop ;s ( x y z -- y z : drop third stack item )

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
\
\ Using the signed comparison operators to do unsigned
\ comparison is a little more involved, as is doing the
\ opposite, however if we stay within one class it is easy
\ to construct all operators given a signed less than or
\ a signed greater than using just swapping the arguments
\ around, or inverting the boolean result.
\
\ "=" can be implemented with "xor" instead of "-", but "-" is
\ cheaper on SUBLEQ machines. We still need to turn the result
\ into a boolean with "0=", for some tests "-" or "xor" could
\ be used without turning the result into a boolean. Many CPUs
\ implement various comparisons by checking flags set after
\ subtraction.
\
\ The first operator we will implement is "\<", it is
\ equivalent to the C code:
\
\        int leq0(uint16_t a) {
\          return ((int16_t)a) <= (int16_t)0;
\        }
\
\        int s_less(uint16_t a, uint16_t b) {
\          const int a0 = leq0(a);
\          const int b0 = leq0(b);
\          if (a0 && !b0)
\            return 1;
\          if (!a0 && b0)
\            return 0;
\          if (a0 && b0) {
\            if (!leq0(a + 1) && leq0(b + 1))
\              return 0;
\          }
\          const int l = leq0((uint16_t)(a - b));
\          return l ? leq0((uint16_t)((a + 1) - b)) : 0;
\        }
\
\ There are many corner cases that have to be dealt with. It
\ is easy to perform a Less-Than-Or-Equals-To-Zero on a SUBLEQ
\ machine, and to perform subtraction. It would seem like
\ implementing the basic comparison operators with those
\ operators would be the fastest, simplest and smallest
\ solution. While it is all three, it is also not correct,
\ failing for comparing large negative values.
\
\ The simple but buggy solution is:
\
\        : > - 0> ;    ( n1 n2 -- f : signed greater than )
\        : < swap > ;  ( n1 n2 -- f : signed less than )
\
\ With "0\>" being implemented as a VM instruction (as the
\ inverse of Less-Than-Or-Equal-To-Zero, which is easy to
\ compute). Instead we implement "leq0" as a VM instruction
\ and build upon that.
\

: <
   2dup leq0 swap leq0 if
     if
       2dup 1+ leq0 swap 1+ leq0
       if drop else if 2drop #0 exit then then
     else 2drop #-1 exit then \ a0 && !b0
   else
     if 2drop #0 exit then \ !a0 && b0
   then
   2dup - leq0 if
     swap 1+ swap - leq0 if #-1 exit then
     #0 exit
   then
   2drop #0 ;

: = - 0= ;     ( u1 u2 -- f : equality )
: <> = 0= ;    ( u1 u2 -- f : inequality )
: > swap < ;   ( n1 n2 -- f : signed greater than )
: 0> leq0 0= ; ( n -- f : greater than zero )
: 0< #0 < ;
: 0<> 0= 0= ;  ( n -- f : not equal to zero )
: 0<= 0> 0= ;  ( n -- f : less than or equal to zero )
: 0>= 0< 0= ;  ( n1 n2 -- f : greater or equal to zero )
: >= < 0= ;    ( n1 n2 -- f : greater than or equal to )
: <= > 0= ;    ( n1 n2 -- f : less than or equal to )

\ The unsigned words are defined in terms of each other once
\ one of them has been defined, they are a bit awkward as they
\ depend on the signed comparison words to do the work, which
\ is not normally how they are implemented, but necessary given
\ the constraints of the system. Unsigned comparison is used
\ less than signed, so speed is not too much of a concern, they
\ are here for completeness sake.
\
\ An alternative implementation of "u\<" is as follows:
\
\        : u< 2dup xor 0< if nip 0< exit then - 0< ;
\
\ However, this uses "xor".
\
: u< 2dup 0>= swap 0>= <> >r < r> <> ; ( u1 u2 -- f )
: u> swap u< ; ( u1 u2 -- f : unsigned greater than )
: u>= u< 0= ; ( u1 u2 -- f )
: u<= u> 0= ; ( u1 u2 -- f )

\ "within" is a word that is used for bounds checking. Whether
\ to include this word or not in the base system is up for
\ debate, it is not used within it at the time of writing this
\ sentence, however it is used quite a lot in extensions.
\
\ The word calculates:
\
\          lower bound <= u < upper bound
\
\ Note that the first comparison differs from the second!
\
\ You can test this yourself with:
\
\         0 1 10 within . ( 0 )
\         1 1 10 within . ( -1 )
\         5 1 10 within . ( -1 )
\         9 1 10 within . ( -1 )
\        10 1 10 within . ( 0 )
\
: within over - >r - r> u< ; ( u lo hi -- f )

\ Miscellaneous arithmetic words need defining:
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
\ "abs" functions on twos compliment machines the
\ function is only properly defined within the range of
\ Minimum Signed Value + 1 to Maximum Signed Value, if you
\ entered the Minimum Signed Value ($8000 or -32768) you will
\ get back the same number. This is common to the majority of
\ implementations of "abs" and is a consequence of twos
\ compliment arithmetic having one more negative numbers than
\ positive non-zero numbers. We could call "throw" in this
\ condition, but the vast majority of "abs" functions in all
\ programming languages do not do this.
\

: negate 1- invert ; ( n -- n : twos compliment negation )
: s>d dup 0< ; ( n -- d : signed to double width cell )
: abs s>d if negate then ; ( n -- u : absolute value )

\ The cell word-set allows portable code to be written that
\ does have to worry about the number of bytes that are in a
\ cell on a given architecture. There are words within this
\ implementation that assume a cell size of 2 for optimization
\ reasons such as "aligned" but they are kept to a minimum,
\ that assumption is frowned upon because it negatively affects
\ portability.
\
\ - "cell" just pushes the size of a single cell in bytes.
\ - "cell+" is used to increment an address to the next cell,
\ without any care for cell alignment.
\ - "cells" is used to convert a number of cells into the
\ number of bytes those cells take up.
\ - "cell-" is used to decrement an address by a single cell,
\ it used to be missing, but
\
\ The words are trivial, "cell-" is missing because it is not
\ used but is easy enough to define.
\

: cell #2 ;   ( -- u : push bytes in cells to stack )
: cell+ cell + ; ( a -- a : increment address by cell width )
: cells 2* ;     ( u -- u : multiply # of cells to get bytes )
: cell- cell - ;

\ "execute" takes an "execution token", which is just a fancy
\ name for an address of a function, and then executes that
\ function. It is used within the interpreter a lot,
\ and is useful for executing the contents of "hooks",
\ that is, variables that contain an execution token that
\ can be changed so the underlying functionality of a word
\ can be changed. They are also known as execution vectors
\ and changing them is "vectoring".
\
\ The implementation of "execute" is incredibly simple, as it
\ is on other Forth implementations, it converts a Forth
\ address to a cell address then puts the value onto the return
\ stack, when the word "execute" returns by calling exit
\ (compiled into the definition by ";t") it will cause a jump
\ to the execution vector.
\
\ "@execute" is a merger of the common expression "@ execute"
\ into one word, it saves a little space. In the original
\ eForth it would perform the tests which are currently
\ commented out, that is "@execute" would check if the
\ execution token was not null before executing the token. This
\ is useful for some execution tokens, but is just as liable
\ to cause problems with others, consider execution tokens
\ that expect or produce values, the checks would not help
\ there. It would be better to throw an error if the execution
\ token is zero, a check which could be added in (and would
\ involve moving the definition of "@execute" to after "throw".
\

: execute 2/ >r ; ( xt -- : execute an execution token )
:s @execute ( ?dup 0= ?exit ) @ execute ;s ( xt -- )

\ "?exit" will conditionally return from the *caller* of the
\ function, an example usage:
\
\        : x ." Executed. " ?exit ." Conditionally Exec." cr ;
\        0 x ( prints "Executed. Conditionally Exec." )
\        1 x ( prints "Executed. " )
\

: ?exit if rdrop then ; compile-only ( u --, R: -- |??? )

\ "key?" is a word that interfaces with the input channel
\ and attempts to get a single character, or byte, of input
\ from it. It *attempts* to do so. Depending on the underlying
\ implementation of the SUBLEQ machine it might block until
\ a character is available (that is, it will wait until the
\ user presses a key) and if there is no more input signal
\ a negative value (all character values are between 0 and
\ 255, so a negative value is treated as an error) known as
\ End Of Input, or on a machine with a non-blocking
\ input a negative value will mean that it has not received
\ a character just yet and calling the get-character function
\ might succeed later.
\
\ As there is no way of determining which has occurred
\ (perhaps they could return different error codes, but they
\ do not), what the "key?" implementation does is instead
\ configurable. By default it will assume there is blocking
\ character input, and when a negative value is encountered it
\ will halt the machine by calling "bye".
\
\ If a non-blocking implementation is configured, it will
\ instead return -1 on error. The word "key?" is a rare word
\ that returns a differing number of arguments, words that
\ accept or return a differing number of arguments depending
\ on circumstance are discouraged, but this word seems to be
\ the exception in other implementations.
\
\ "key?" will return the character retrieved and a zero
\ on successful reception of a character in either
\ implementation.
\
\ "key?" does not call "pause" either.
\
\ Note that "key?" does not call a VM primitive to perform
\ an input operation, it instead performs a load from a
\ special address that triggers an input (*sort of* like
\ memory mapped Input and Output).
\
\ This is done with the line:
\
\        #-1 [@] negate
\
\ Performing a load of address "#-1 [@] negate", which fetches
\ a byte from input as a side effect of a load.
\

: key? #-1 [@] negate ( -- c 0 | -1 : get byte of input )
   s>d if
     {options} lit @ 8 lit and if bye then drop #0 exit
   then #-1 ;

\ "key" pauses (allows other threads to run) and repeatedly
\ calls "key?" until the operation succeeds. This is a version
\ of "key?" that always blocks until a character is received.
\
\ Note that what is said is not correct, it does not
\ call "key?", however the default execution token stored in
\ "\<key\>" is "key?", you can change it to be whatever you
\ want, so long as it has the same stack effects as "key?",
\ allowing you to directly take input from another input
\ source, say a string, or a new peripheral you have added
\ to the SUBLEQ machine.
\

: key begin pause <key> @execute until ; ( -- c )

\ A side note about randomness, we lack sources of entropy
\ within the virtual machine, we have two possible sources,
\ and other natural sources of seeds (but which cannot
\ be classified as sources of entropy given that they are
\ constant and only varying between different versions of the
\ base image) such as the image checksum. The two possible
\ sources of entropy are the timing of key presses and the
\ key presses themselves, as they constitute the only entropy
\ in the system, if the input blocks then only the values of
\ the key presses can be used as an entropy source.
\
\ If we integrated entropy collection into the "key" word,
\ then we could use this to create a higher quality PRNG (but
\ still bad, it would still not be suitable as a
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
\ for example you could duplicate what is sent to the
\ programmer and write it to a section of memory as a temporary
\ log, or search for strings within the output and process
\ those.
\

: emit pause <emit> @execute ; ( c -- : output byte )

\ "cr" emits a newline, DOS style newlines are used instead
\ of Unix style newlines, although that can be changed by
\ removing the right constant and emit words. This could be
\ made to be an option, set in the "{options}" field, but is
\ not at present as there is little need.
\
\ * Unix systems use "lf".
\ * DOS and Windows uses "cr" then "lf".
\
\ There are other systems that use other crazy characters for
\ newlines, but no one cares about them.
\
\ "cr" would be slightly smaller if we stored the output
\ characters as a string. A minor optimization.
\

: cr =cr lit emit =lf lit emit ; ( -- : emit new line )

\ There is nothing special about these three words, they are
\ common, standard, convenience words for fetching and storing
\ variables in the system.
\
\ "get-current"/"set-current" get the dictionary or vocabulary
\ that is currently being used to append new word definitions
\ to, which does not have to be one in the search order, but
\ usual is. "last" gets the location of the last defined word
\ in the dictionary.
\
\ See also "definitions".
\

: get-current current @ ; ( -- wid : get definitions vocab. )
: set-current current ! ; ( -- wid : set definitions vocab. )
:s last get-current @ ;s ( -- wid : get last defined word )

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
\         50 40 30 20 10 0 pick . ( prints 10 )
\         50 40 30 20 10 1 pick . ( prints 20 )
\         50 40 30 20 10 2 pick . ( prints 30 )
\
\ That is, the first element on the stack is at position 0,
\ the second at position 1, and so on. Note the positions are
\ calculated before you put the index onto the stack and not
\ after.
\
\ For systems that do not have a stack that can be indexed,
\ the following version of "pick" that shifts values between
\ the data and return stack can be defined:
\
\        : pick ?dup if swap >r 1- pick r> swap exit then dup ;
\
\ Be warned though, this version of "pick" is slower but the
\ main concern is its high stack usage, especially as systems
\ with hardware stacks are likely to have small stacks,
\ perhaps with even as low as eight values. The version of
\ "pick" above will quickly eat through that, overflow,
\ and give incorrect results when the index is too high, the
\ one below does not have that limitation as the stack can be
\ indexed into and directly accessed.
\
\ "roll" is not defined here, but in an ancillary section not
\ included in the meta-compiled image. It is usually found with
\ "pick" and likewise discouraged from use. It allows a varying
\ number of items on the stack to be rotated.
\

: pick sp@ + [@] ; ( nu...n0 u -- nu : pick item on stack )

\ "+!" is a useful utility word, often found with "1+!" and
\ "1-!", neither of which are defined here as they are not
\ used, "+!" is however. It is used to add a number to a
\ value stored at an address, storing the result back to the
\ same cell.
\
\ It is a common enough operation that it deserves its own
\ word.
\
\ "1+!" and "1-!" are easy enough to define yourself:
\
\        : 1+!  1 swap +! ;
\        : 1-! -1 swap +! ;
\
\ If you need them. Do not forget to change the ":" and ";"
\ into ":t" and ";t", as well as changing "1" to "#1" and "-1"
\ to "#-1" if you want to add them to this cross compiled
\ program instead of typing them in at the command prompt.
\

: +! 2/ tuck [@] + swap [!] ; ( u a -- : add val to cell )

\ "lshift" is much faster to compute than "rshift" as
\ "2\*" is faster than "2/". As "2\*" is much faster we can
\ instead write "lshift" in Forth, instead of assembly, saving
\ on space.
\
\ "rshift" can be defined like so:
\
\         : rshift begin ?dup while 1- swap 2/ swap repeat ;
\
\ However in this Forth we implement "2/" with "rshift", and
\ as you know we have implemented "rshift" in assembly instead.
\
: lshift begin ?dup while 1- swap 2* swap repeat ; ( n u -- n )

\ ### Character Load / Store
\
\ The SUBLEQ machine can only do cell aligned loads and stores,
\ This means we will have to make words that can perform
\ character or byte based access. For speed reasons this would
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
\ address, cell addresses used by the SUBLEQ machine, and Forth
\ addresses used by Forth interpreter. We must always be aware
\ which address types we should use under what circumstances.
\ If you see a multiplication or division by 2, it is most
\ likely because the address type needs converting for an
\ operation. Other systems sometimes have complications when
\ it comes to addressing which are more often present in 16-bit
\ systems of in the embedded software space including near and
\ far pointers, or separate address spaces for RAM, ROM and
\ EEPROM.
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
\ values into the target also needs to be the same as the
\ target.
\

: c@ @+ swap #1 and if 8 lit rshift exit then FF lit and ;
: c! swap FF lit and dup 8 lit lshift or swap
   tuck @+ swap #1 and 0= FF lit xor
   >r over xor r> and xor swap ! ; ( c a -- character store )

\ "c@+" is another space saving measure like "@+", but is also
\ useful to have around, and non-standard.
\
:s c@+ dup c@ ;s ( b -- b u : non-destructive 'c@' )

\ "min" and "max" are more traditionally written as:
\
\        : max 2dup < if nip else drop then ; ( n1 n2 -- n )
\        : min 2dup > if nip else drop then ; ( n1 n2 -- n )
\
\ And that is the fastest way of writing them in this system,
\ it is not the smallest, instead we can use the multiplexing
\ word "mux" and a property of Forth booleans (false being zero
\ and true being all bits set) to implement them. Speed is not
\ of too much of a concern here as the two words should not be
\ on critical path when it comes to performance.
\
\ Note that these version of "min" and "max" return the minimum
\ and maximum of two *signed* values respectively, dropping the
\ other value. "umin" and "umax" operate on unsigned values,
\ but they are not needed.
\
\ A non-destructive version that sorts the items on the stack
\ by ascending or descending might be useful, but I cannot
\ think of a purpose for them in this Forth.

: max 2dup > mux ; ( n1 n2 -- n : highest of two numbers )
: min 2dup < mux ; ( n1 n2 -- n : lowest of two numbers )

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

: 2! tuck ! cell+ ! ; ( u1 u2 a -- : store two cells )
: 2@ dup cell+ @ swap @ ; ( a -- u1 u2 : fetch two cells )

\ These two words are like "2!" and "2@", but for shunting
\ two numbers between the return and data stacks. Note that
\ the implementation of "2\>r" *cannot* just be:
\
\        : 2>r >r >r ;
\
\ As the return stack is used to store the position of
\ where a function was called from, if we implement as above
\ we will clobber that position and return not to whence we
\ came but we will do a fandango on the core instead. The
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
\ These words are using for parsing, which is defined later on.
\
system[ user tup =cell tallot ]system
: source tup 2@ ; ( -- a u : get terminal input source )

\ "aligned" is one of those words that has been implemented
\ in a non-portable way, so would have to change if the cell
\ size did.
\
\ It takes an address and aligns that address to the next
\ address on a two byte boundary (and on a 32-bit system this
\ standard Forth word would align to a four byte boundary, on
\ 64-bit, 8 bytes).
\
\ For some example mappings:
\
\        0 aligned -> 0
\        1 aligned -> 2
\        2 aligned -> 2
\        3 aligned -> 4
\        4 aligned -> 4
\
\ "align" does the same as "aligned", but operates on the
\ dictionary pointer. It is common to want to align the
\ dictionary pointer after writing a string into the
\ dictionary or byte oriented data of a varying length.
\

: aligned dup #1 and 0<> #1 and + ; ( u -- u : align up ptr. )
: align here aligned h? ! ; ( -- : align up dict. ptr. )

\ "allot" allocates memory in the dictionary, it accepts a
\ signed number of bytes to allocate, so it is possible to
\ move the dictionary pointer backwards, which is occasionally
\ useful, however it is usually used by the user to allocate
\ space for an array, or a string, like so:
\
\        decimal
\        create example 100 allot
\
\ Which creates a word which when run returns an address, that
\ address has 100 *bytes*, not cells, allocated to it.
\
\ "," is used to write a value into the dictionary, a cell,
\ it naturally uses a cells worth of memory. It makes sure to
\ call "align" before hand. It can also be used to allocate
\ memory in the dictionary:
\
\        create example 1 , 2 , 3 ,
\        example @ .
\        example cell+ @ .
\        example cell+ cell+ @ .
\
\ This creates a word "example" which returns an address when
\ run which points the dictionary space where we have just
\ written 1, 2, and then 3, into three consecutive cells.
\
\ On some platforms "," can be used to write an execution token
\ into a word definition, but that is not portable, an
\ execution token does not have the same representation as the
\ number that represents a call to a word that is compiled
\ within a word definition.
\
\ On this platform, using "," will fail when compiling
\ execution tokens into the dictionary, and will most likely
\ cause a crash or a reset. Instead "compile," is provided,
\ which can convert in a platform specific way the execution
\ token into a function call for that platform and reserve
\ enough space for the value for the call it will write into
\ the dictionary.
\
\ For example:
\
\        : x 2 2 [ ' + compile, ] . cr ;
\        : y 2 2 [ ' + , ] . cr ;
\
\ The function "x" will work, but "y" will not. "compile,"
\ is defined later.
\
\ "c," is analogous to "," except it writes single characters
\ into the dictionary, it does not align the dictionary after
\ writing (which would defeat the purpose of the word). It
\ advances the dictionary pointer by one byte and not the size
\ of a cell, like "," does. In this Forth two characters, or
\ bytes, fit into a single cell.
\
: allot h? +! ; ( n -- : allocate space in dictionary )
: , align here ! cell allot ; ( u -- : write value into dict. )
: c, here c! #1 allot ; ( c -- : write character into dict. )

\ "count" and "+string" are two words used for string
\ manipulation. "count" is named because it is often used with
\ counted strings, a counted string consists of a single byte
\ for the length of the string, followed by the rest of the
\ string. That is the traditional mechanism Forth used for
\ strings, but it limits those strings to only containing
\ 255 bytes plus the length byte, not a problem on the memory
\ constrained 16-bit microcomputers that Forth grew up on,
\ and not a problem here either.
\
\ "count" can be used to extract the length of a string, but
\ also for moving down that string, or any byte array.
\
\ An example for doing a byte-wise memory dump using "count":
\
\        : cdump for aft count . then next ; ( b u -- )
\
\ And a common idiom for printing out counted strings:
\
\        count type
\
\ Which uses "type", a word we will encounter next.
\

: count dup 1+ swap c@ ; ( b -- b c : advance string )
: +string #1 over min rot over + -rot - ; ( b u -- b u )

\ "type" is used to print out a string. It is not complicated,
\ and does not make an effort to not print out non-graphic
\ ASCII characters, so if you print out binary data you will
\ get garbage. It is easy enough to make a version of "type"
\ that did this filtering, which would be useful for displaying
\ Forth Blocks with "list".
\
\ Another way of dealing with this would be to replace the
\ execution vector in "emit" with one that pre-processes the
\ character, replacing non-graphic characters with ".", for
\ example.
\
\ As with the numeric output words, it may be useful to call
\ "single" and "multi" at the start and end of this procedure,
\ to help prevent garbled output in a multithreading
\ environment.
\

: type ( a u -- : print out a string )
  begin dup while swap count emit swap 1- repeat 2drop ;

\ "fill" is used to fill a section of memory with a repeated
\ byte, hence the name. More frequently "erase" is used, which
\ does a fill but the byte being zero.
\
\ "cmove" is used to move one section of memory to another
\ section of memory. "move" is a cell by cell equivalent, but
\ is missing as it is not used within this Forth.
\
\ Both functions operate on bytes, not cells. "cmove" is often
\ used for strings however, and "fill" (in the form of "blank"
\ which fills a section of memory with spaces) is used in the
\ block text editor.
\
\ "fill" is equivalent to the C standard library function
\ "memset", and "cmove" to "memcpy".
\
\ "blank", if needed, can be defined as:
\
\        : blank bl fill ; ( b u -- : write spaces to buffer )
\
\ "blank" is sometimes used in Forth block editors.
\

: cmove ( b1 b2 u -- : move character blocks around )
   for aft >r c@+ r@ c! 1+ r> 1+ then next 2drop ;
: fill ( b u c -- : write byte 'c' to array 'b' of 'u' length )
   swap for swap aft 2dup c! 1+ then next 2drop ;
: erase #0 fill ; ( b u -- : write zeros to array )

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
\ The two meta-compiler string words '."' and '$"', use
\ "($)" and ".\$" then call "\$literal" to grab the string from
\ the input stream. Two words similar to the meta-compiler
\ versions will be defined later, after the parsing words have
\ been made.
\
\ How does "do\$" work? Let us see a compiled string:
\
\        : x ." HELLO" cr ;
\
\ This will be compiled to something like this, with one 16-bit
\ cell per line:
\
\        X:
\                address of .$
\                5,'H'
\                'E','L'
\                'L','O'
\                address of exit
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
\ the first word we call is ".\$", when we call something
\ we push the address of the next cell onto the return stack
\ so when we return, we return to the address after the
\ function just called. In this case when we call ".\$" it will
\ contain the address of the string we want to print out,
\ however, when we call "do\$" from within ".\$" it will have
\ the address of "count" on the return stack (which we will
\ want to execute) and the address of the string after that.
\ "do\$" needs to extract the string address from return stack,
\ copy it as it is meant to leave a copy on the return stack,
\ calculate the next address after the string, and then
\ replace the string address on the return stack with the
\ address of the place after the string (aligned up of course).
\ It also needs to leave the address of "count" on the return
\ stack so when we return from "do\$" it will still call
\ "count", however after fixing up the return address of
\ the string when ".\$" returns, it returns to the place after
\ the string.
\
\ It does all this in a short amount of code. It is not
\ as complex as it sounds, it is more of a trick that has to
\ be learned. "do\$" also has to convert to and from cell
\ addresses with "2*" and "2/".
\
\ "do\$" is not a general purpose word, it should not be used
\ in any "normal" code, nor is the technique it uses a general
\ or good one. It does make for some nicely compact code
\ however, and is one example of doing introspection in Forth.
\
\ Another example of messing around with the return stack
\ to achieve greatness is "?exit", previously defined.
\
:s do$ 2r> 2* dup count + aligned 2/ >r swap >r ;s ( -- a  )
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
\ more acceptable they are), The author instead prefer other
\ mechanisms, or even just returning error codes.
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
\ little possibility without dedicating a massive fraction
\ of their lives to making a Rust, or a C++ compiler, that
\ is compliant. A Forth system can built and understood, from
\ machine to user interface, by a single person. It is a short
\ and sweet language.
\
\ Java uses exceptions because it believes programmers are too
\ lazy to check return codes, which is true, but just leads to
\ programmers being lazy with exceptions (and catching
\ ones they should not be).
\
\ Adding exceptions to a language does not solve
\ the lazy programmer problem, making it easier to check an
\ error than to ignore it does, which is what Rust does.
\ C++ is best left not talked about, which I find is true in
\ general.
\
\ Forth does use exceptions, but uses them sparingly, usually
\ only when something has gone wrong. It is also a pain
\ to pass up error codes in Forth programs as everything is
\ passed via the stack. For truly exceptional circumstances,
\ where calling "abort" might be appropriate, an exception
\ can be thrown instead. Forth does not encourage their use and
\ one does not find themselves asking "what exceptions can this
\ possibly throw?", and they are usually left uncaught except
\ by the outer interpreter, which has an exception handler of
\ last resort.
\
\ The appendix contains a list of standard error codes some of
\ which are thrown by this interpreter.
\
\ While it can be tricky to get catch/throw correct, their
\ operation in Forth is simple due to the execution model
\ of Forth. It is a dual stack system where the stacks are
\ easily accessible.
\
\ The first word defined "catch" accepts an execution token,
\ it will execute that token, and in doing so return an error
\ code. It will return zero if nothing has been thrown, or
\ a non-zero number if an exception has been thrown by "throw"
\ in the token just executed.
\
\ A common idiom used in this Forth, even though bitwise
\ operators are expensive and branching is cheap, is to use
\ the following construct to conditionally throw:
\
\        0= -1 and throw
\
\ Or more generally:
\
\        <conditional> <error-code> and throw
\
\ Which will throw an error if a condition is true, which
\ replaces:
\
\        <conditional> if <error-code> throw then
\
\ "if...then" generates slightly larger code on most Forth
\ implementations, and also branches, than if "and" were used.
\ The source code is slightly larger, although immediately
\ easier to read.
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
\ the example implementations in the standards and notes,
\ such as in the ANS Forth standard (available at
\ <http://lars.nocrew.org/forth2012/exception/THROW.html>) and
\ and the application note "Catch and Throw" by
\ Michael Milendorf, Sun Microsystems from EuroForth 98.
\
\ The idea is in "catch" we store the data stack pointer on
\ the return stack saving our data stack position, along with
\ the previous exception handler, we then store the current
\ return stack position in the handler and then execute the
\ given token. If the execution token throws an exception
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
   #0 ;                  ( 0 )    \ normal completion

: throw ( ??? exception# -- ??? exception# )
    ?dup if              ( exc# )     \ 0 throw is no-op
      {handler} up @ rp! ( exc# )     \ restore prev ret. stack
      r> {handler} up !  ( exc# )     \ restore prev handler
      r> swap >r         ( saved-sp ) \ exc# on return stack
      sp! drop r>        ( exc# )     \ restore stack
    then ;

\ Now we have "catch" and "throw", we can use them. The next
\ chapter defines the more advanced arithmetic words of
\ division is one such word. Division will need a way to throw
\ if provided numbers outside the range for which the function
\ is valid (ie. if we try to divide by zero).
\
\ But there are short words that we will define first
\ that are also useful. Those are "abort" and "?depth".
\
\ "abort" throws -1, which should not be caught by the
\ exception handler of last resort in the "interpret" word, but
\ will cause "interpret" to halt the system after printing
\ -1 followed by a question mark.
\
\ "(abort)" is used in a similar function later on
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
:s ?depth depth >= -4 lit and throw ;s ( ??? n -- )

\ # Advanced Arithmetic
\
\ The Forth arithmetic word-set attempts to solve the most
\ complex arithmetic problem, and in doing so make the simpler
\ problems trivial. This is not usually a viable method to
\ take, greater complexity usually just leads to greater
\ complexity, however here it works well.
\
\ A note about Forth, when using the term "double" it does
\ not refer to floating point numbers, it refers to numbers
\ which are twice the normal width of a number, so on a 16-bit
\ system a 32-bit value stored as two integers on the stack
\ would be a "double". It comes from "double width" or
\ "double precision".
\
\ It is usual for Forth implementations that target small
\ or embedded system to not define the floating point word-set.
\ The original Forth implementations predated widely available
\ floating point units integrated into the CPU, and
\ implementing floating point arithmetic in software on a
\ 16-bit system would be slow and take up limited memory
\ space.
\
\ A lot of the decisions that went into Forth are a consequence
\ of the limited hardware on microcomputers available in the
\ 1980s and earlier. If starting from scratch software wise,
\ but with modern hardware, you would not create a language
\ like Forth. It is a product of its time, which in a way makes
\ it magical and timeless, but dated.
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
\ word performs an addition with carry, which most modern
\ systems have instructions specifically for. This one does
\ not, so it has to compute the carry itself, we already have
\ addition thankfully.
\
\ "um+" takes two single cell numbers and adds them together,
\ the result of the addition and then the carry is pushed to
\ the stack. Using this we can construct some double cell
\ words, "um+" is a mixed word, it effectively produces a
\ double cell number given two single cell numbers, the name of
\ the word closely follows the naming convention for Forth
\ words of these type, "um+" says "unsigned mixed addition", in
\ effect.
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
\ is slow. "um\*" only loops for 16 times however, on this
\ 16-bit platform, one for each bit, and always loops for the
\ same number of times, so it cannot logically be using that
\ algorithm to perform multiplication, instead it uses a shift
\ and add, doubling the multiplier and taking care with the
\ overflow each cycle to process each bit within it.
\
\ Given "um\*", "\*" can be coded with:
\
\        : * um* drop ;
\
\ Rendering its implementation trivial. To really improve
\ the speed of this implementation, and any similar eForth
\ implementation which uses "um+" for multiplication, "um+"
\ should be made to be as fast as possible.
\

: um+ 2dup + >r r@ 0>= >r ( u u -- u carry )
  2dup and 0< r> or >r or 0< r> and negate r> swap ;
: dnegate invert >r invert #1 um+ r> + ; ( d -- d )
: d+ >r swap >r um+ r> + r> + ;         ( d d -- d )
: um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) F lit
  for
    dup um+ >r >r dup um+ r> + r>
    if >r over um+ r> + then
  next shed ;
: * um* drop ; ( n n -- n : multiply two numbers )
: um/mod ( ud u -- ur uq : unsigned double cell div/mod )
  ?dup 0= -A lit and throw
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
  if negate >r dnegate r> then
  >r s>d if r@ + then r> um/mod r>
  if swap negate swap then ;
: /mod over 0< swap m/mod ; ( u1 u2 -- u1%u2 u1/u2 )
: mod  /mod drop ; ( u1 u2 -- u1%u2 )
: /    /mod nip ; ( u1 u2 -- u1/u2 )

\ We can implement some of the signed double cell arithmetic
\ words if we need them as well, with:
\
\        : 2swap >r -rot r> -rot ;       ( w x y z -- y z w x )
\        : d< rot 2dup >                    ( d -- f )
\          if = nip nip if 0 exit then -1 exit then
\          2drop u< ;
\        : d>  2swap d< ;                   ( d -- t )
\        : du> 2swap du< ;                  ( d -- t )
\        : d=  rot = -rot = and ;           ( d d -- t )
\        : d- dnegate d+ ;                  ( d d -- d )
\        : dabs  s>d if dnegate then ;      ( d -- ud )
\
\ But by default they will not be included as they are easy
\ enough to define if we need them and we want to keep the
\ interpreter nice and slim.

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
\ words handle it as well.
\
\ The line parsing routines culminate in the construction of
\ "query", which parses words with "parse", both will
\ require other words which we will make on the way.
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
\ then we cannot use ")" as that would leave an item on the
\ stack where one should not be, it will be replaced with a
\ "drop" instead.
\
\ The default execution vectors are set later on during the
\ "task-init" function and depend partially on the "{options}"
\ flags to enable or disable echoing with "echo".
\
\

:s (emit) opEmit ;s ( c -- : output byte to terminal )
: echo <echo> @execute ; ( c -- : emit a single character )

\ "tap" and "ktap" are both used by "accept", they are both
\ given four items on the stack, and return three, which is
\ a lot for a Forth word. The arguments given are as follows:
\
\         bot - Bottom Of Text.
\         eot - End Of Text.
\         cur - Current Text Position, or cursor.
\         c   - The character to process.
\
\ The input buffer and our position in it is represented by
\ the first three arguments, "bot", "eot", and "cur". We are
\ given a new character to process and decide on what to do
\ with.
\
\ "accept" is the word that calls these two words, "tap" and
\ "ktap", and will finish processing when "cur" is equal to
\ "eot", which would mean the input buffer is full. "ktap"
\ takes advantage of that and uses that to force "accept" to
\ exit when it encounters a newline.
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
\ to "ktap" should be written to the buffer given to "accept"
\ but should trigger different behaviors.
\
\ As mentioned, if "c" is a newline character "ktap" causes
\ "cur" to equal "eot", causing "accept" to exit.
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
\ "=bksp lit dup echo bl echo echo" is sometimes factored out
\ into a word called "^h".
\
\ If "ktap" does not know what to do with the control character
\ then it replaces it with a space and calls "tap".
\
\ This is a complex word needed to handle terminal input.
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
\        pad 20 accept
\        HELLO WORLD
\        .s
\        type
\
\ Which will accept a line of text, "HELLO WORLD", and store
\ it into the pad area, then print what "accept" returns,
\ and then regurgitates the input string.
\
\ We will not use "accept" directly, but use it to make
\ "query", it does more little work though, "query" just calls
\ "accept" on some internal buffers and sets some variables.
\
\ "accept" must make sure it does not overrun the input
\ buffer, it also must handle control characters. Note that
\ there is no part of the loop that says "terminate this
\ function when a newline has occurred", it only stops the
\ loop when the current cursor position exceeds the buffer
\ length.
\
\ "tib" is a convenience word used for accessing the Terminal
\ Input Buffer. It is immediately used by "query", note that
\ "query" also defines what our maximum length of a line is,
\ this itself could be made into a variable so it can be
\ changed, but we gain little from that on such a limited
\ system.
\
\ After "query" is completed it sets the index into the line
\ being parsed to zero, so the parser starts from the
\ beginning, and also sets the length of the line just parsed.
\
\ After calling "query" we can then split up the line.
\

: accept ( b u -- b u : read in a line of user input )
  over + over begin
    2dup <>
  while
    key dup bl - 5F lit u< if tap else <tap> @execute then
  repeat drop over - ;
: expect <expect> @execute span ! drop ; ( a u -- )
: tib source drop ; ( -- b )
: query tib =buf lit <expect> @execute tup ! drop #0 >in ! ;

\ "-trailing" removes the trailing white-space from an input
\ string, it does this non-destructively leaving the original
\ string intact, it just modifies the string length in an
\ address-length pair. It is used to post process the line
\ given to us by "query", it is not appropriate to call it
\ in "query" to make "query" as reusable as possible.
\
: -trailing for aft ( b u -- b u : remove trailing spaces )
     bl over r@ + c@ < if r> 1+ exit then
   then next #0 ;

\ This section will culminate in the word "parse", which will
\ allow us to split up lines provided by "query" in to
\ individual words. We will start by describing a component of
\ "parse" called "look".
\
\ "look" is a moderately complex word, it takes a string,
\ a character to parse until, and an execution token.
\ The execution token is for a function that takes two
\ characters and should return a boolean indicating when
\ to stop looking within a word. Note that parsing space is
\ treated as a special case (partially because when splitting
\ up a word we do not just want to split on the space character
\ but also on tabs, control characters, NULs and the like,
\ which should be treated as space).
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
\ Buffer and the "\>in" variable, then uses two calls to
\ "look" to find the start and end of a word with "unmatch" and
\ "match" respectively. It then calculates a delta between the
\ unmatch and match which it adds to "\>in", meaning subsequent
\ calls to "parse" will skip over the section we have just
\ parsed.
\
\ After that it retrieves the beginning of the "unmatch"
\ where our parsed word resides, it also trims any white-space
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
\ "parse" also has to deal with the situation of not finding
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
  swap >r -rot
  begin
    dup
  while
    over c@ r@ - r@ bl = 4 lit pick execute
    if rdrop shed exit then
    +string
  repeat rdrop shed ;s
:s unmatch if 0> exit then 0<> ;s ( c1 c2 -- t )
:s match unmatch invert ;s        ( c1 c2 -- t )
: parse ( c -- b u ; <string> )
  >r tib >in @ + tup @ >in @ - r@ ( get memory to parse )
  >r over r> swap 2>r
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
\ is "\>number", which does the heavy lifting, and then the
\ secondary word "number?" which pre and post processes the
\ results of "\>number".
\
\ All of the numeric I/O is affected by the "base" variable,
\ which controls which base or radix we are operating in when
\ parsing numbers, and what base numbers are printed out in.
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
\ For the I/O words it might be worth calling the
\ multithreading words "single" and "multi" to disable
\ multithreading and re-enable it after the output is complete,
\ which could also be done for "type", to help prevent mangled
\ output. This is not done however.
\
:s banner ( +n c -- )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ;s

\ "hold" adds a number to hold space, this version does not
\ check that it overflows hold space, so the programmer must be
\ careful not to do this. As mentioned, hold space is a per
\ thread area where numeric output strings are formatted.
\ Characters in hold space are also stored in reverse order!

: hold #-1 hld +! hld @ c! ; ( c -- : save char in hold space )

\ "#\>" is the last of the three (four including "\#s") words
\ that form the core of the numeric output words, although it
\ is defined first. It removes a number from the stack and
\ returns a string and string length to the string we have just
\ formed in hold space by using combinations of "\<#",
\ "\#" and "hold".
\
: #> 2drop hld @ this =num lit + over - ; ( u -- b u )

\ "extract" extracts (hence the name) a single number from
\ a double cell, it should do this with the divide/modulo
\ number word called "um/mod", if we divide a number by the
\ base we are operating in then we can extract a single digit
\ from that number in the returned remainder argument, the
\ quotient is reused until it is zero and no more digits can be
\ extracted from the number.
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
\ a period between each character, or whatever is required for
\ the output, you can use these set of words to achieve that.
\
: #  #2 ?depth #0 radix extract digit hold ; ( d -- d )
: #s begin # 2dup ( d0= -> ) or 0= until ;   ( d -- 0 )
: <# this =num lit + hld ! ;                 ( -- )

\ "sign" adds a "-" character to hold space if the
\ provided number is negative. It is used by ".".
\
: sign 0>= ?exit [char] - hold ; ( n -- )

\ "u.r" and "u." print out unsigned numbers, "u." prints out an
\ unsigned number with a single space before it, "u.r" allows
\ a variable number of spaces to be printed out This allows for
\ the creation of right aligned numeric output.
\
\ A version of "u.r" that would allow a custom character to
\ be passed to it instead of printing "bl" would allow for
\ more formatting options, such as displaying leading zeros,
\ useful for the "dump" word defined later.
\
\ The ".r" word is missing, this word acts the same as
\ "u.r" but displays signed values.
\
: u.r >r #0 <# #s #> r> over - bl banner type ; ( u r -- )
: u. space #0 u.r ; ( u -- : unsigned numeric output )

\ "(.)" bypasses all of the standard Forth mechanisms for
\ formatting Forth numbers, the reason for doing this is speed,
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
:s (.) abs radix opDivMod ?dup if (.) then digit emit ;s

\ "." uses "(.)", it just needs to check if the number to print
\ is negative, if so, it emits a single "-" character.
\
\ The normal definition of ".", without using "(.)", is:
\
\        : . space dup >r abs #0 <# #s r> sign #> type ;
\
\
: . space s>d if [char] - emit then (.) ; ( n -- )

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
\ becomes the decimal number 10 in bases higher than ten. The
\ number is then accumulated into a double cell, the double
\ cell is multiplied by the base before the number is added. By
\ providing the initial double cell the word is made to be
\ slightly more flexible than if it just pushed a double cell
\ zero onto the stack itself.
\

: >number ( ud b u -- ud b u : convert string to number )
  dup 0= ?exit
  begin
    2dup 2>r drop c@ radix           ( get next character )
    ( digit? -> ) >r [char] 0 - 9 lit over <
    if 7 lit - dup A lit < or then dup r> u< ( c base -- u f )
    0= if                            ( d char )
      drop                           ( d char -- d )
      2r>                            ( restore string )
      exit                           ( finished...exit )
    then                             ( d char )
    swap radix um* drop rot radix um* d+ ( accumulate digit )
    2r>                              ( restore string )
    +string dup 0=                   ( advance, test for end )
  until ;

\ "number?" is much easier to use than "\>number" and handles
\ extensions to the numeric input handling in Forth,
\ which are; handling negative and hexadecimal numbers, as well
\ as double cell numbers which have a fixed point.
\
\ This Forth uses the prefix "\$" to indicate that the number
\ is hexadecimal, regardless of what base we are currently in,
\ the equivalent in C being the prefix "0x". In some Forth
\ interpreters, albeit not this one, "\#" is used to indicate
\ the number is always decimal. These prefixes seem natural to
\ use for these purposes that is hard to quantify, much like
\ using "\$" is sometimes used to indicate something is a
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
\        123.
\        1.23
\        .123
\
\ Are all valid Forth numbers, but when view with ".s" on the
\ stack they all produce the same result when entered at the
\ Forth prompt, "123 0". There is a difference between the two
\ however, the "dpl" variable will be set with different
\ values.
\
\        123    -> dpl is -1 (single cell number was entered)
\        123.   -> dpl is 0
\        1.23   -> dpl is 2
\        .123   -> dpl is 3
\        0.123  -> dpl is 3
\        0.0123 -> dpl is 4
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
\ The separator is a single dot, currently there is no way
\ of changing this, to do this a variable could be introduced,
\ or perhaps a string containing characters to allow as
\ separators.
\
: number? ( a u -- d -1 | a u 0 : easier to use than >number )
  #-1 dpl !
  radix >r
  over c@ [char] - = dup >r if         +string then
  over c@ [char] $ =        if hex     +string then
( over c@ [char] # =        if decimal +string then )
  2>r #0 dup 2r>
  begin
    >number dup
  while over c@ [char] . <>
    if shed rot r> 2drop #0 r> base ! exit then
    1- dpl ! 1+ dpl @
  repeat
  2drop r> if dnegate then r> base ! #-1 ;

\ ".s" is a useful debugging word that requires that "." is
\ implemented. It displays the contents of the variable stack
\ without modifying or dropping numbers from it.
\
\ It would have been useful to have ".s" and "." when
\ debugging the bringing up of the Forth virtual machine,
\ however as we have seen these words both require the Forth
\ VM to be operational, and a lot of other Forth words to be
\ written before they can be implemented. Having a way to print
\ out numbers is useful, and in fact basic, necessity in
\ debugging a platform. One way this was achieved before ".s"
\ and "." could be supported is by implementing more primitive
\ versions of the numeric output words, it is easy to
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
\ This Forth keeps its word header in the same place as it
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
\ "#vocs" is the constant that tells us the number of
\ dictionaries we can load and search through at one time,
\ eight is usually the minimum, and on this system that is the
\ case, we can have a total of eight and minimum of zero
\ vocabularies active at any given period. It might be useful
\ to have zero vocabularies loaded to prevent the entering of
\ any words whilst executing a non-interactive program for
\ instance.
\
\ "(search)" will traverse a vocabulary looking for a word
\ until one is found, or exiting without finding one, and
\ "(find)" applies "(search)" to the array of vocabularies
\ with the same exit conditions.
\
\ The two words are simple; look through a linked list,
\ and look through an array, essentially.
\
\ Other words for dealing with the word headers are also
\ made, "nfa" and "cfa" (although "cfa" is not needed here,
\ it makes sense to define it in the same place as "nfa").
\
\ The word "compare" for testing string equality, required
\ by "(search)", is also defined here.
\
\ "compare" exits early if the strings are not of equal length,
\ otherwise it compares the strings byte for byte and exits if
\ they are not equal. To keep the number of items on the stack
\ small we can use "for" to traverse both strings, given we now
\ know they are of equal length, and then use "count" to move
\ the string along and extract a byte. We use subtraction to
\ test whether the two are equal or not, and return that test
\ value much like the C "strcmp" function.
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
  nfa c@+ 1F lit and + cell+ cell negate and ;

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
\        PWD PWD   1 : Word found, word is immediate
\        PWD PWD  -1 : Word found, word is not immediate
\          0   a   0 : Word not found, returns name in "a"
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
\ which points to the found word and the word that has been
\ found.
\
\ This can help during decompilation to determine the last
\ cell in a word, the previous word in the linked list of words
\ starts (assuming no "allot" happens in between the word
\ definitions) where the last one ended. As a last resort the
\ "here" pointer can be used as a maximum value to not
\ decompile past. This can be used to find the start an end
\ point of a word definition which is useful for decompilation.
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
    nip @+
  repeat
  rdrop 2drop #0 ;s
:s (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word )
  >r
  context
  begin
    @+
  while
    @+ @ r@ swap (search) ?dup
    if
      >r shed r> rdrop exit
    then
    cell+
  repeat drop #0 r> #0 ;s
: search-wordlist ( a wid -- PWD 1|PWD -1|a 0 )
   (search) shed ;
: find ( a -- pwd 1 | pwd -1 | a 0 : find word in dictionary )
  (find) shed ;

\ # The Interpreter

\ The main word defined in this section is called "interpret",
\ when given a counted string it will attempt to execute that
\ string, and throw an error if it cannot. It must deal with
\ compile only words (not shown in the diagram below),
\ immediate words, numbers, and compile and command mode.
\
\ ![Interpreter Control Flow](img/flow.png)
\
\ (See the Appendix for ASCII Art Version)
\
\ Whilst the diagram shows the overall flow of "interpret", it
\ leaves out a few states, the "compile-only" words is one as
\ mentioned, another is processing double cell words which are
\ a less well known feature of Forth.
\
\ On to the words themselves.
\
\ "(literal)" is to determine whether we should compile a
\ number or push it on to the variable stack given the state of
\ the interpreter, we do not need to make a version of
\ "(literal)" for double cell numbers as we can just call it
\ twice. "(literal)" is the word that will go into the
\ execution vector in "literal", "literal" is the word that
\ "interpret" actually uses. "literal" is an immediate word, as
\ it is often used within word definitions to compile a number
\ computed within that word definition, like this:
\
\        : example [ 2 2 + ] literal . cr ;
\
\ Which is equivalent to:
\
\        : example 2 2 + . cr ;
\
\ Except that the computation of adding two numbers together is
\ done in the first example during compilation and not during
\ runtime, making the first example more efficient and smaller
\ than the second example.
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
\ In order to define "(literal)" we will need to define
\ "compile".
\
\ "compile" is a word that skips over the next compiled word
\ in the instruction stream and instead compiles that word into
\ the dictionary. It has some restrictions on what it can be
\ used on, for example it will not work on immediate words on
\ this platform as instead they will have already been
\ executed, and it will not work on literals, as they are not
\ words (and they take up two cells). "compile" does its job by
\ looking at the return stack value, copying it, and
\ manipulating it.
\
\ A contrived example usage is:
\
\        : x compile + ; immediate
\        : y 2 2 + ;
\        : z 2 2 x ;
\
\ In this case "y" and "z" are equivalent words, they both
\ compute "4" at runtime. A more succinct way of explaining
\ "compile" is "compile compiles the next word into another
\ word via return stack manipulation".
\

: compile r> dup [@] , 1+ >r ; compile-only ( -- )
:s (literal) state @ if compile (push) , then ;s
: literal <literal> @execute ; immediate ( u -- )

\ "compile," is a version of "," which can turn an execution
\ vector into something that will perform a call when compiled
\ into the word. On some platforms execution vectors and calls
\ might have the same numeric format, however on this one they
\ do not, there is a relationship that can be computed (and
\ reversed) but you cannot directly call an execution vector,
\ it must be converted first before it can be compiled into the
\ words body.
\
: compile, ( align <- called by "," ) 2/ , ;  ( xt -- )

\ "?found" is called at the end of "interpret" if a word has
\ not be found in the dictionary or is not a number, it prints
\ the string that could not be found in the dictionary and then
\ throws an error. The function either returns the given string
\ or it throws an error.
\

:s ?found ?exit ( b f -- b | ??? )
   space count type [char] ? emit cr -D lit throw ;s

\ The "interpret" diagram describes the word, with
\ some minor details missing that will be describe later, it
\ would be best to lay out the description in listed form as
\ well.
\
\ 1. Interpret is called for a word, given as a counted string.
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
\    a number, if it is numeric and we are in...
\    a) Command Mode - We push the number to the stack and
\       exit "interpret".
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
\    It would be ideal if the error could be vectored.
\
\ The order of that search matters, as it means that we could
\ define new words that are also numbers, but they would be
\ treated as words and not numbers, for example:
\
\        : 1 2 ;
\        1 ( pushes '2' )
\
\ Is valid, if useless and confusing, Forth code. If numbers
\ were dealt with first then it would still push "1".
\
\ We could make it so that "interpret" executes an execution
\ vector, or it executes one if neither a number or a word is
\ found. Either vector would make the interpreter more
\ flexible, but neither is done, due to limitations on the
\ image size. In the newer standards of Forth there is a push
\ to incorporate something called "recognizers" that would
\ allow the interpreter to be extended arbitrarily, while a
\ neat idea, they seem to go against the spirit of Forth by
\ being too generic and complex, this is a matter of personal
\ taste however.
\

: interpret ( b -- )
  find ?dup if
    state @
    if
      0> if cfa execute exit then \ <- execute immediate words
      cfa compile, exit \ <- compiling word are...compiled.
    then
    drop
    dup nfa c@ 20 lit and 0<> -E lit and throw ( <- ?compile )
    \ if it's not compiling, execute it then exit *interpreter*
    cfa execute exit
  then
  \ not a word
  dup >r count number? if rdrop \ it is numeric!
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
\ more than one vocabulary, the root vocabulary is not only a
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
\ The other words which are needed by the system, but are not
\ needed in the root word-set are:
\
\ * "get-order"
\ * ".id"
\ * "definitions"
\
\ So long as we keep the root word-set in the list of
\ vocabularies that are active at any one time, we can always
\ type "only forth definitions" to get the dictionary into
\ a good, known, order.
\
\ Forth implementations sometimes only have one vocabulary,
\ especially the more amateur implementations. Vocabularies
\ are useful (especially for meta-compilation) but are not
\ necessary, so they are often viewed as superfluous.
\
\ When a Forth implementation does have multiple vocabularies,
\ it usually has the following ones:
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
\ away. This approach has the feeling of laziness to it and
\ seems more like an excuse.
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
\        -1 set-order
\        root-voc forth-wordlist 2 set-order
\
\ Note that "set-order" will not check whether or not you
\ have specified multiple vocabularies that happen to be
\ the same, so:
\
\        root-voc root-voc 2 set-order
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
   #0 >r begin @+ r@ xor while cell+ repeat rdrop
  dup cell - swap
  context - 2/ dup >r 1- s>d -$32 lit and throw
  for aft @+ swap cell - then next @ r> ;
:r set-order ( widn ... wid1 n -- : set current search order )
  \ NB. Uses recursion, however the meta-compiler does not use
  \ the Forth compilation mechanism, so the current definition
  \ of "set-order" is available immediately.
  dup #-1 = if drop root-voc #1 set-order exit then
  dup #vocs > -$31 lit and throw
  context swap for aft tuck ! cell+ then next #0 swap ! ;r

: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r ( recurse -> ) (order) over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;

\ "forth-wordlist" contains the standard Forth words,
\ excluding the root Forth words, as mentioned, and "system"
\ contains non-standard Forth words used in the implementation.
\
\ As an aside we could save space by creating a special word
\ for variables that could be used in the meta-compiler, here
\ we push a memory location for where the variable is
\ stored in both "forth-wordlist" and "system", instead we
\ could use the cell used to store the address of a variable
\ as the variable, the new utility word would then need to
\ push the location of that memory location and exit the word
\ for this to work, it would be slightly slower but more
\ memory efficient. The current setup works however.
\

root[
  {forth-wordlist} constant forth-wordlist ( -- wid )
          {system} constant system         ( -- wid )
]root

\ "forth" sets the search order to include both the root
\ and Forth vocabularies.
\

:r forth root-voc forth-wordlist #2 set-order ;r ( -- )

\ This version of "only" is implemented as "-1 set-order",
\ but it could be implemented other ways, such as
\ "root-voc 1 set-order", or by directly modifying the
\ search order array.
\
\ Typical usage as is mentioned:
\
\        only forth definitions
\
\ It is possible to call just "forth", instead of "only".
\

:r only #-1 set-order ;r ( -- : set minimal search order )

\ ".id", an internal word, is used to print out the name of a
\ word in a word's header. We cannot just use "count type" as
\ we need to mask off special word behavior bits stored in the
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
\ flag, so if set we do not show the word. It might be better
\ to use the 6th bit instead (which we use for other purposes
\ at the moment) to speed up word searching slightly as words
\ can only be 32 characters in length, if we mask off the
\ lowest 6-bits and use "compare" a hidden word will always
\ return false at it will be longer than 32 characters. A
\ slight efficiency gain.
\
\ The word is just one loop within the other, it uses
\ "get-order" to retrieve each of the vocabularies and the
\ number of them instead of examining the "context" array
\ itself.
\

:r words ( -- )
  cr get-order
  begin ?dup while swap ( dup u. ." : " ) @
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
\ new functions, you would not have been able to get
\ far in this book having not know that. When we call ":" it
\ does the following things, it parses the next word in the
\ input stream, creates a header for that word, and then
\ makes it so that everything we type in (barring "immediate"
\ words) is compiled into the dictionary instead of being
\ executed.
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
\ definition. This seems like it would prevent recursion and it
\ does, which is why the "recurse" word was made. ":" sets a
\ variable called "{last}" as well, this will be used in
\ "recurse" as it contains the location of the last defined
\ word, well, really it points to the word that is *currently
\ being defined*, knowing this, "recurse" can compile a call
\ to the correct place (and it must be a call, not a jump,
\ unless certain constraints are met which are not checked for
\ in this implementation). These problems do not apply to the
\ meta-compiler, we can define recursive words without the
\ usual Forth semantics getting in our way.
\
\ A little compiler security is added, the constant $CAFE is
\ pushed to the stack by ":" and checked by ";", if it is not
\ found then an error is thrown and the word is not linked into
\ the dictionary.
\
\ ";" must also terminate a word definition, it compiles an
\ exit instruction into the dictionary following the word
\ body. Note that it is also a compile only word, in some other
\ Forth implementations it has been given some command mode
\ semantics as well that are not related to its usual function,
\ but not in this one.
\
\ "?unique", "?nul", "?len" are three words that do some tests,
\ "?unique" is for a warning (the only one in this Forth
\ implementation) telling us if a word has been defined already
\ in the word-list or vocabulary (this guide uses the terms
\ interchangeably). "?nul" prevents zero length definition
\ words from being made. "?len" checks the length of a word to
\ make sure it is not too long, the length of a Forth word is
\ stored in the lower five bits of the first byte in the Name
\ Field, the other three being used for flags, if the name is
\ too long, an exception is thrown.
\
\ Other eForth implementations provided "!csp" and "?csp":
\
\        variable csp
\        : !csp sp@ csp ! ;
\        : ?csp sp@ csp @ <> abort" stack mismatch" ;
\
\ They are useful for testing new words but not for embedding
\ in word definitions as there is only one "csp" location
\ available. The word "?csp" calls abort if there is a mismatch
\ in the current stack pointer and the pointer stored by
\ "!csp", which can be used to check whether words consume
\ and produce the right number of arguments.
\
\ With that compiler security and potentially the "csp"
\ words, most problems can be found easily. They are not too
\ expensive to check for.
\
\ "word" is general purpose parsing word that takes a character
\ to parse as an argument on the stack, and then parses out
\ that string from the input string. It then packs that string
\ into a counted string at the current dictionary location and
\ provides a pointer to it. This is useful for ":" and
\ other words, but we should be careful using it, it writes
\ to a location that soon will be overwritten if we are not
\ careful.
\
\ ":noname" can be used to create anonymous functions,
\ functions with no-name, that can only be referred to by an
\ execution token. ":noname" leaves an execution token on the
\ stack and it must cooperate with ";" so ";" does not attempt
\ to link something without a word header into the dictionary,
\ it does that by making sure ":noname" is given a 0 instead
\ of a word address which is treated specially by ';' and does
\ not link the zero address into the dictionary. ":noname" must
\ also make sure to push the right compiler security constant
\ to the stack as well. It is much simpler than ":" as it does
\ not have to deal with adding new words into the dictionary.
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
\ "token" takes care of the most common use of "word", the
\ short phrase "bl word".
\
\ This short word-set gives us the power to define new words,
\ it took a fair amount to get us to this point, but this
\ is another milestone in making a Forth interpreter.
\

: word ( -- b )
  #1 ?depth parse here aligned dup >r 2dup ! 1+ swap cmove r> ;
:s token bl word ;s ( -- b )
:s ?unique ( a -- a : warn if word definition is not unique )
 dup get-current (search) 0= ?exit space
 2drop {last} lit @ .id ." redefined" cr ;s ( b -- b )
:s ?nul c@+ ?exit -$10 lit throw ;s ( b -- b )
:s ?len c@+ 1F lit > -$13 lit and throw ;s ( b -- b )
:to char token ?nul count drop c@ ; ( "name", -- c )
:to [char] postpone char compile (push) , ; immediate
:to ;
  CAFE lit <> -$16 lit and throw ( check compile safety )
  =unnest lit ,                     ( compile exit )
  postpone [                        ( back to command mode )
  ?dup if                           ( link word in if non 0 )
    get-current !                   ( this inks the word in )
  then ; immediate compile-only
:to :   ( "name", -- colon-sys )
  align                 ( must be aligned before hand )
  here dup              ( push location for ";" )
  {last} lit !          ( set last defined word )
  last ,                ( point to previous word in header )
  token ?nul ?len ?unique ( parse word and do basic checks )
  count + h? ! align    ( skip over packed word and align )
  CAFE lit              ( push constant for compiler safety )
  postpone ] ;          ( turn compile mode on )
:to :noname align here #0 CAFE lit ] ; ( "name", -- xt )

\ "'" is an immediate word that attempts to
\ find a word in the dictionary (and throws an error if one
\ is not found), it then calls "literal" on the execution token
\ to resolve the compile/command behavior of the word. "'"
\ either pushes the execution token of a word when in command
\ mode, or compiles it into the dictionary in compile mode.
\
\ When in command mode, "2 2 ' + execute" is the same as
\ "2 2 +".
\
\ "compile" belongs with these words, but is needed earlier
\ on.
\
\ Some Forth implementations of "'" return a zero value (and
\ thus invalid) execution token when a word is not found,
\ which makes the word "defined" redundant, however this
\ behavior is not standard between all Forth programs and
\ should not be relied upon.
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
\ redefinition of words using the previous definition, for
\ example we could introduce a temporary debug version of "+"
\ into source code with ": + .s + ;" and the previous
\ definition of "+" would be used within the new definition,
\ being unaffected by the new definition.
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
\ in the dictionary (and must be aware of special case caused
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
\ the search order words will take into account. Using bit-6
\ might make more sense, as explained previously.
\
\ Some older Forth words had a word called "smudge", which
\ operated in a similar fashion, toggling a bit in the Name
\ Field Address, but operated on the previously defined word
\ like "recurse". This could be defined with:
\
\        :s smudge {last} lit @ nfa 80 lit swap toggle ;s
\
\ "smudge" was used to hide and unhide a word definition during
\ its creation to implement the word hiding feature described
\ in the section on "recurse", this Forth implementation does
\ not use "smudge" and implements compilation slightly
\ differently.
\

:to ' token find ?found cfa literal ; immediate
:to recurse {last} lit @ cfa compile, ; immediate compile-only
:s toggle tuck @ xor swap ! ;s ( u a -- : toggle bits at addr )
:s hide token find ?found nfa 80 lit swap toggle ;s

\ # Control Structures
\
\ No programming language is complete without control
\ structures. Note that GOTO is missing, eForth also uses the
\ odd, but simple to implement, mechanism for definite loops
\ called "for...aft...then...next", which will need more
\ explanation. The other control structures are standard Forth
\ words. Also note that all the words defined here are
\ "immediate" and "compile-only" words. They push variables
\ to the variable stack during compilation containing memory
\ locations to be patched or used, and also push known
\ constants, so the compiler security within ":" and ";" can
\ also detect mismatched control structures.
\
\ Missing from this Forth is the looping mechanism "do...loop"
\ and its variants. This is one of the main stumbling blocks
\ for porting ANS Forth (one of the main Forth standards) code
\ to eForth. I, personally, dislike the "do...loop" mechanism
\ and avoid its use within my own code, preferring the simpler
\ but less well known "for...next" loops. There is an
\ implementation of the "do...loop" mechanism in the appendix,
\ which is not compiled into the target.
\
\ Forth does not have a fixed grammar, it starts off with a
\ simple one, but words can be added that do arbitrary
\ parsing, it is possible to add words that implement
\ arbitrary control structures as well, however the normal,
\ less arbitrary ones will do first. It should be said that
\ Forth is a malleable language, however much less so
\ than Lisp, which offers a far more structured approach.
\
\ The constructs we will make are:
\
\        begin...again
\        begin...until
\        begin...while...repeat
\        if...then
\        if...else...then
\        for...next
\        for...aft...then...next
\
\ "if...then" and "begin...until" are some of the simplest
\ constructs, understanding these constructs really helps
\ in understanding how more complex compilers work, at least
\ for the code generation phase.
\
\ Both "if...then" and "begin...until" consist of a single
\ jump. Let us start with an example of "if":
\
\        : example if 1 . cr then ;
\
\ Which will compile to something like this:
\
\        EXAMPLE:
\                0: opJumpZ
\                1: 6
\                2: opPush
\                3: 1
\                4: address of "."
\                5: address of "cr"
\                6: opExit
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
\ a cell address, the Forth address would be "12".
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
\ explanation as it uses "opNext", which is more complex.
\
\ One common way in Forth to create new control structures,
\ which are all immediate words, is to call "postpone" on the
\ ones that already exist, these are often mixed with other
\ words that manipulate the locations those words push onto the
\ variable stack, or compile other words into their target.
\ This is a semi-portable method of doing this, as the Forth
\ implementations do not have to leave the same number of items
\ on the stack, nor do they have to have exactly the same
\ meaning. "unless" is an example of this, shown later.
\
\ ## FOR...NEXT
\
\ The "for...next" loop is given a counter which it decrements
\ until it is equal to zero, when it then stops. It loops
\ for N + 1 times, so given 9 it will loop 10 times, or 1 it
\ will loop 2 times. This is not always desired, so the
\ "for...aft...then...next" construct can be used to loop
\ fewer times, the construct is more flexible than we generally
\ needed it, when using "aft...then" we normally just want to
\ loop one fewer times, however the full behavior is:
\
\ * "for...aft" is run on the first loop iteration and
\ every time "aft...then" is.
\ * "aft...then" is the main part of the loop body, it runs
\ for N times, if given N as a loop counter to "for". It
\ is entirely optional.
\ * "then...next" is run every loop iteration.
\
\ This means "for" must put the loop counter onto the return
\ stack, as with a normal "for...next" loop, however
\ "aft...then" must make an unconditional jump over it, and
\ "next" must be made to point to after "aft".
\
\ The example words:
\
\        : fn for r@ . cr next ;
\        : fatn
\           for
\             ." A:" r@ . cr
\           aft
\             ." B:" r@ . cr
\           then
\             ." C:" r@ . cr
\           next ;
\
\ Shows the (convoluted) control structure in its full gory
\ (glory?). One of the costs of reusing existing mechanisms
\ and not coming up with a new one is this complex "aft"/"then"
\ modifier, the "for...next" loop is simple and easy to use,
\ but remembering all the details of a
\ "for...aft...then...next" can be confusing. Instead it is
\ usually just used in the following manner:
\
\        : example for aft (code) then next ;
\
\ Which executes "(code)" N times instead of N + 1 times.
\
\ The text detailing "opNext" contains more of a description
\ of how it works, which is shown in the previous section on
\ VM instructions.
\

:s mark here #0 , ;s compile-only
:to begin here ; immediate compile-only
:to if =jumpz lit , mark ; immediate compile-only
:to until 2/ postpone if ! ; immediate compile-only
:to again =jump lit , compile, ; immediate compile-only
:to then here 2/ swap ! ; immediate compile-only
:to while postpone if ; immediate compile-only
:to repeat swap postpone again postpone then ;
    immediate compile-only
:to else =jump lit , mark swap postpone then ;
    immediate compile-only
:to for =>r lit , here ; immediate compile-only
:to aft drop =jump lit , mark here swap ;
    immediate compile-only
:to next =next lit , compile, ; immediate compile-only

\ # Create, DOES>, and other special Forth words
\
\ "create" and "does\>" are sometimes called the jewels of
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
\ a variable onto the variable stack which is the address of
\ the location after the newly defined word, allowing you to
\ allocate memory after the word (for example, for an array).
\
\ The interesting thing about words that have been created is
\ that the default action to push an address can be changed to
\ something else with the "does\>" word.
\
\ There are also a number of internal words which
\ are not useful to anyone other than the person defining
\ the words here, such as "(var)" and "(const)", which are
\ used in "variable" and "constant", the standard Forth words
\ for creating variables and constants respectively. "variable"
\ and "constant" could also be defined as:
\
\       : constant create , does> @ ;
\       : variable create 0 , does> ;
\
\ But we do not have the runtime version of "create" and
\ "does\>" available, hence the usage of "(var)", and
\ "(const)". "(marker)" will be used in the next section, it
\ is used to retrieve stored information required by the word
\ "marker", it is similar in to "(var)" and "(const)".
\
\ We have already encountered their use in the meta-compiler.
\
\ The explanation of "does\>" will require more effort, it
\ modifies the created word so that it now executes a different
\ set of instructions, the ones pointing to after the "does\>"
\ in the current defining word. "create" can be execute outside
\ of a word definition, and it makes sense and can be valid
\ to do so, "does\>" needs to be done with a word definition.
\
\ The following peculiar behavior is also standard for
\ "create...does\>" constructs:
\
\        : x create does> ." Hello" does> ." World" ;
\        x y
\        y ( prints "Hello" )
\        y ( prints "World", behavior has changed )
\        y ( prints "World", behavior has now settled )
\
\ Which has limited uses, but is more of a curiosity than
\ anything. "does\>" compiles two words into the creating word
\ that we are making, one to patch up the created word called
\ "(comp)" to point to the second compiled in word "(does)"
\ which we use to redirect execution to continue after the
\ "does\>".
\
\ "(comp)" can look at the last defined word and move to the
\ code field in order to rewrite it by looking at the "{last}"
\ variable. It pops off the topmost return stack variable,
\ this is the location of "(does)" which will be written in
\ after "(comp)", by popping the return stack it means that
\ when "(comp)" exits it will return to the callers caller,
\ ensuring that "(does)" not get executed prematurely.
\
\ "(does)" gets executed in the newly created word, it gets
\ called from it, which means that it can look at its return
\ stack to see where it was called from and pass a copy of that
\ pointer to the code after it which will get executed.
\
\ That should give an overview of how "create...does\>" works,
\ the easiest way to truly understand it is to reimplement
\ it yourself.
\

:s (marker) r> 2* @+ h? ! cell+ @ get-current ! ;s compile-only
: create state @ >r postpone : drop r> state ! compile (var)
   get-current ! ;
:to variable create #0 , ;
:to constant create cell negate allot compile (const) , ;
:to user create cell negate allot compile (user)
   cell user? +! user? @ , ;

: >body cell+ ; ( a -- a : move to a create words body )
:s (does) r> r> 2* swap >r ;s compile-only
:s (comp)
  r> {last} lit @ cfa
  ( check we are running does> on a created word )
  @+ to' (var) half lit <> -$1F lit and throw
  ! ;s compile-only
: does> compile (comp) compile (does) ;
   immediate compile-only

\ # Forgetting words
\
\ "marker" is a word with caveats aplenty, at least this
\ implementation of it. It is however a useful word
\ when debugging new code interactively. It is a defining
\ word, so it requires a name of a new word, it then creates
\ a word that when called deletes every word in the dictionary
\ defined after the new word and then deletes itself. It can
\ be used to reclaim dictionary space and names from the
\ dictionary.
\
\ This allows us to define a word, find problems with it,
\ and redefine it, without creating multiple definitions of
\ the same word.
\
\ An example usage might be:
\
\        marker xxx
\        : ahoy cr ." BYE" ;
\        words
\        xxx
\        words
\        : ahoy cr ." HELLO" ;
\
\ This creates a marker called "xxx", so if we make a mistake
\ making a new word, in this case making the word "ahoy"
\ print out the wrong message, then we can go back and change
\ it without cluttering up our environment.
\
\ Now for the caveats, the newly defined word should not be
\ used whilst changing the vocabularies in play, this can
\ cause memory corruption issues, nor should any memory or
\ variable be referred to in the section of memory just
\ deallocated for the same reason.
\

:to marker last align here create cell negate allot compile
    (marker) , , ; ( --, "name" )

\ # Return Stack Words
\
\ These target only words, defined with ":to", are both
\ immediate and compile-only. They are defined here because
\ the "compile" word needs defining first. They are all defined
\ this way because they manipulate the call stack, it is
\ possible to define a callable version of a word like "r\>",
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
\ being done here, which has been mentioned multiple times,
\ because it is important to understand and once it has, it
\ will become obvious (which is almost tautological, "once
\ you understand it, your will understand it", but the concept
\ will just "click"), beforehand it looks like the code could
\ not possibly work. To be be more explicit:
\
\            (1)         (2)
\        :to rp! compile rp! ; immediate compile-only
\
\        (1) : Newly defined header, not visible within the
\              body of the newly defined word because the
\              meta-compiler ":to" word is used.
\        (2) : Points to the Forth Virtual Machine instruction,
\              which has the following definition;
\              ":a rp! tos {rp} MOV t' opDrop JMP (a);"
\              (equivalent to):
\              ":a rp! tos {rp} MOV tos {sp} iLOAD --sp ;a"
\
\ As we have discussed how each of these instructions work
\ previously in the VM instructions section, we will not
\ discuss what they do here.
\

:to rp! compile rp! ; immediate compile-only
:to rp@ compile rp@ ; immediate compile-only
:to >r compile opToR ; immediate compile-only
:to r> compile opFromR ; immediate compile-only
:to r@ compile r@ ; immediate compile-only
:to rdrop compile rdrop ; immediate compile-only
:to exit compile opExit ; immediate compile-only

\ # Meta-compiler string words
\
\ These words are target only words, as the meta-compiler has
\ versions of these words as well that are used (except for
\ *abort"*, however we still would not want to use this version
\ as these words only work correctly on a running target and
\ not in the meta-compiler).
\
\ These words are all string related words, all strings are
\ in the range of 0 to 255 bytes in length as the length is
\ stored in a single byte, like all Forth strings traditionally
\ were. Who needs more than 255 bytes for a string!?
\
\ The words do the following:
\
\        ."      Parse until ", compile a string, at run time
\                The string is printed
\        $"      Parse until ", compile a string, at run time
\                a pointer to a counted string is pushed onto
\                the stack.
\        abort"  Parse until ", compile a string, at run time
\                pop an item off of the stack, if the number is
\                non-zero then print out the string and call
\                abort, otherwise, continue on execution.
\
\ This handles most of string handling needs for Forth. They
\ are all immediate words as they have to parse a string at
\ compile time from the input stream and compile a word into
\ the dictionary.
\
\ "(s)" is common to all of these words, it is similar to
\ "$literal", both non-standard Forth words. As the word might
\ be useful it placed within the system vocabulary, but not
\ the main Forth vocabulary. "(s)" parses a string from the
\ input string, which is stored in the correct dictionary
\ location already by "word", so all it needs to do is allocate
\ the length of the newly parsed string.
\
\ Most languages allow for escape sequences within strings,
\ Forth being the exception. Escape strings are useful (as
\ are "printf" format string present in most languages, also
\ missing from Forth) and would simplify some code within the
\ meta-compiler (although to remain portable we would not be
\ able to use them). For example with the ANSI terminal
\ escape words being able to enter an Escape character into
\ a string by specifying its numeric value, like so:
\
\        :s csi ." \x1B\x5B" ;s
\
\ Would shorten the definition (skip ahead to see it). Adding
\ this capability in would complicate the implementation for
\ little gain however.
\
\ Other de facto standard characters for new-lines, return
\ characters and so-forth would be useful as well. De Facto
\ not for Forth but for nearly every single post-C language.
\
\ A word which printed out a string followed by a new-line
\ would be useful, but would be non-standard.
\

:s (s) align [char] " word count nip 1+ allot align ;s
:to ." compile .$ (s)  ; immediate compile-only
:to $" compile ($) (s)  ; immediate compile-only
:to abort" compile (abort) (s) ; immediate compile-only

\ # Comments
\
\ These words add comments to the Forth interpreter, and a
\ word that prints out the contents of the comment. These are
\ parsing words, they modifying the input stream in someway,
\ these just modify it to discard it. They do it in different
\ ways.
\
\ "(" and ".(" both look for a single ")" character in the
\ input stream, which they use "parse" to do, feeding it a
\ ")" character to look for. "(" just drops the result of
\ parse, and ".(" uses "type" to print it out. They will not
\ work across lines, not in this Forth at least, in other
\ Forth implementations they might do, making them work for
\ multi-line comments would ruin their simplicity.
\
\ All of the comment words are immediate, so they can execute
\ within a word, otherwise "(" and ".(" would get compiled in
\ and the comment would as well (most likely it would cause
\ an error instead).
\
\ The word ")" does nothing, it is not even meant to be a
\ No-Operation, it is meant to be compiled out,
\ this is because sometimes comments are used to comment out
\ code that is added back in for quick testing, for example:
\
\        : example ." HELLO " ( ." WORLD" ) ;
\
\ If we want to enable "WORLD" to be printed out, we need
\ to remove the "(":
\
\        : example ." HELLO " ." WORLD" ) ;
\
\ It is annoying however to remove the ")", especially if we
\ want to add the "(" back in after testing, but there is no
\ need to remove the ")" even temporarily as it does nothing
\ and is not compiled into the word "example" as it is an
\ immediate word that does nothing. The word ")" does actually
\ have a use, as a "no operation" word, which is sometimes
\ useful if we want to set an execution vector (that takes
\ and returns no arguments) to do nothing.
\
\ "\\" works differently, it is meant to discard all the
\ input until the end of the line, and does this by
\ manipulating the "\>in" variable that the Forth interpreter
\ uses to keep track of where it is parsing in the input line.
\ By setting ">in" to the end of the line "query" will skip
\ the rest of the line. One extra complication is comments in
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
\        : ?\ 0= if postpone \ then ; immediate
\
\ Usage:
\
\        0 ?\ .( ALPHA )
\        1 ?\ .( BRAVO )
\         ( Only "BRAVO" is printed )
\
\ NB. ")" is defined earlier so it can be used as a
\ "no-operation" word.
\

:to ( [char] ) parse 2drop ; immediate ( c"xxx" -- )
:to .( [char] ) parse type ; immediate ( c"xxx" -- )
:to \ tib @ >in ! ; immediate ( c"xxx" -- )

\ "postpone" is a word that can be confusing, we have been
\ introduced to the concept of command mode and compile mode,
\ and know that in command mode numbers are pushed on to the
\ stack and words are executed, but in compile mode numbers
\ and words are compiled into a word definitions body with
\ the exception of immediate words, which are executed anyway.
\
\ The word "postpone" carves out an exception to that
\ exception, it takes the next work in the input stream
\ and compiles that word into the dictionary regardless of
\ whether it is an immediate or normal word. We have just
\ seen an example of this, in the definition of a new form
\ of conditional comment:
\
\        : ?\ 0= if postpone \ then ; immediate
\
\ "postpone" is called and compiles and instance of an
\ immediate word, "\\", into the dictionary. The reason this
\ can happen is two fold, only the Forth interpreter loop logic
\ itself takes into account the "immediate" status of a word,
\ which we can see later on in the word "interpret", and
\ secondly, "postpone" itself is an immediate word, it looks
\ at the next word in the input stream, attempts to find that
\ word, and if it finds it, compiles that word into the
\ dictionary, it does not care about the immediate status of
\ word because it never checks it.
\
\ "postpone" is used to compile words that are usually
\ immediate into the dictionary, so that they can be executed
\ within the word definition, but works on any word.
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
\         : unless compile 0= postpone if ; immediate
\
\ And it can be used with "then", just like "if":
\
\        : example unless ." HERE" then ;
\        0 example
\        ( prints "HERE" )
\        1 example
\        ( prints nothing )
\
\ In this example we do not need to know how "if" works on
\ this platform, but we can reuse it to do what we want.
\

:to postpone token find ?found cfa compile, ; immediate

\ "immediate" is a word that makes the last defined word
\ immediate, that is it will execute when in compile mode
\ instead of being compiled, as we know. All it has to do is
\ set a flag in the header for the last defined word to mark
\ it as being immediate, that is why it "immediate" goes after
\ the definition of a word, as it modifies the header of an
\ already existing word.
\
\ "last" gets us a pointer to the last defined word, naturally.
\ "nfa" moves us to the Name Field Address, which contains a
\ counted string with a modification, the byte used to indicate
\ the length of the Forth word is used for multiple purposes,
\ as word names can only be 32 characters in length on this
\ platform the upper bits of the count byte can be used for
\ flags. Care must be taken to mask off the lower bits when
\ word length is needed however. The 6th bit is used for
\ the "immediate" bit. It is the word "interpret" that looks
\ at this bit, "immediate" just needs to set it in the latest
\ defined words header to make "interpret" treat it
\ as immediate.
\
\ "compile-only" works in the same way, except it sets a
\ different bit. The flag is tested in "interpret".
\
\ "(nfa)" allows one to toggle any of the Name-Field-Address
\ bits, it should be "set" and not "toggle" (that is, a
\ version of "toggle" that uses "or" and not "xor"), but
\ we should never call "immediate" or "compile-only" on a
\ word twice anyway.
\
\ Note, that in order to save space we could have used the
\ definition:
\
\        :to compile-only bl (nfa) ;
\
\ To save two bytes, as "bl" has the hexadecimal value 20,
\ or 32 in decimal, but that is confusing, anyone reading the
\ code by itself may think to themselves "why has the space
\ character got anything to do with the word header?". So
\ we have not done this.
\

:s (nfa) last nfa toggle ;s ( u -- )
:to immediate 40 lit (nfa) ; ( -- : mark prev word as immed. )
:to compile-only 20 lit (nfa) ; ( -- )

\ # Some Programmer Utilities (Decompilation and Dump)
\
\ Nearly every Forth comes with a built in decompiler that
\ can take a Forth word and produce and show how that Forth
\ word is put together. They can be advanced or be
\ bare-bones, this is one of the more bare bones
\ implementations, it would not take that much to improve it,
\ but we are short of space in the target image.
\
\ It is interesting to think that if the decompiler was made
\ good enough then this source file could dispensed of,
\ although the comments would not be reproduced.
\
\ The name of this decompiler is called "see", this one tries
\ to dump the numeric contents of each cell within a word, it
\ determines the end of a word when it either hits the value
\ for the exit instruction or until it reaches the end of the
\ dictionary pointer.
\
\ A more advanced decompiler could print out the word
\ header, whether the word is "compile-only", "immediate", or
\ even if it is a hidden word, analyze each cell to determine
\ whether it is a call or a VM instruction (and print out the
\ word name if it is a call), and decompile calls to "opJumpZ",
\ "(up)", "(user)", "(var)", "(const)" and "(push)", all of
\ which contain data and not instructions in the cell after
\ them.
\
\ However, as we have the source here, not much is gained
\ from this, it would just be an intellectual exercise.
\
\ There is a better decompiler in the appendix where some of
\ the ideas described here are dealt with in more detail.
\

opt.better-see [unless]
:to see token find ?found cr ( --, "name" : decompile  word )
  begin @+ =unnest lit <>
  while @+ . cell+ here over < if drop exit then
  repeat @ u. ;
[then]

\ ## Advanced version of "see"
\
\ This is a more advanced version of "see", it is a much better
\ decompiler, but also much more complex. It could still use
\ more work, but then again any decompiler that cannot
\ reproduce the source code it is decompiling could use more
\ work.
\
\ We start by defining "ndrop",  "ndrop" is a non standard but
\ common word for removing a variable number of items from the
\ variable stack.
\

opt.better-see [if] ( Start conditional compilation )

:s ndrop for aft drop then next ;s ( x0...xn n -- )

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
\ Field Address (CFA) it is given lies between those two
\ pointers. As there might be other words defined with other
\ vocabularies within that pointer range, it must then validate
\ the match by calling "validate". If found, it returns the
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
\
\ The original eForth had a different name for "cfa?" and
\ "name", one was called "\>name" which was most analogous to
\ "cfa?".
\
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
  dup =jumpz lit = if
    drop ."  jumpz " cell+ dup @ 2* u. exit
  then

  dup =jump  lit = if
    drop ."  jump  " cell+ dup @ 2* u. exit
  then

  dup =next  lit = if
    drop ."  next  " cell+ dup @ 2* u. exit
  then

  dup to' (up) half lit = if drop
     ."  (up) " cell+ dup @ u. exit
  then

  dup to' (push) half lit = if drop
     ."  (push) " cell+ dup @ u. exit
  then

  dup to' (user) half lit = if drop
     ."  (user) " cell+ @ u. drop $7FFF lit exit
  then

  dup to' (const) half lit = if drop
     ."  (const) " cell+ @ u. drop $7FFF lit exit
  then

  dup to' (var) half lit = if drop
     ."  (var) " cell+ dup u. ."  -> " @ . $7FFF lit exit
  then

  dup to' .$ half lit = if drop ."  ." [char] " emit space
    cell+ count 2dup type [char] " emit + aligned
  exit then

  dup to' ($) half lit = if drop ."  $" [char] "
  emit space
    cell+ count 2dup type [char] " emit + aligned
  exit then
  primitive lit @ over u> if ."  VM    " 2* else
    dup name ?dup if space count 1F lit and type drop exit then
  then
  u. ;s

\ These two words, "compile-only?" and "immediate?" tell us
\ whether a word is compile-only or immediate respectively,
\ all they do is analyze a specific bit a pointer to a given
\ word header. Nothing complex.
\
:s compile-only? nfa 20 lit swap @ and 0<> ;s ( pwd -- f )
:s immediate? nfa 40 lit swap @ and 0<> ;s ( pwd -- f )

\ This is the more complex version of "see", it will attempt
\ to print the code in a form that resembles the source as
\ much as possible, but it falls short of that a bit.
\
\ We could improve the stop conditions by using "(find)"
\ as it gives us a better range for the likely positions of
\ when a word starts and ends.
:to see token dup ." : " count type cr find ?found
  dup >r cfa
  begin dup @ =unnest lit <>
  while
    dup dup 5 lit u.r ."  | "
    @ decompile cr cell+ here over u< if drop exit then
  repeat drop ."  ;"
  r> dup immediate? if ."  immediate" then
  compile-only? if ."  compile-only" then cr ;

[then] ( End conditional compilation of entire section )

\
\ ## Dump
\
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
\ Due to the simplicity of the implementation, the following
\ phrase:
\
\        0 here dump
\
\ Can be used to spit out the entire dictionary ready for it
\ to be backed up, this could be used to create a new
\ "turn-key" application by replacing the execution vector
\ for "cold" with a newly defined word and then saving the
\ entire image to disk, which when next executed would jump
\ straight to the new definition, or just to save newly defined
\ words into a new image. This might be easier for some than
\ meta-compiling a new image.
\
\ "." is used to print out the value stored at an address,
\ which is signed, but using:
\
\        6 lit u.r
\
\ Makes for a more ordered dump (although it prints unsigned
\ values), the number '6' should be replaced depending on what
\ base you are using (and can be calculated by performing the
\ integer logarithm in the base you are using on the maximum
\ value stored in a single cell, 65535, and adding one for
\ extra padding, and another if dealing with signed values).
\

: dump aligned ( a u -- : display section of memory )
  begin ?dup
  while swap @+ . cell+ swap cell -
  repeat drop ;

\ ## Check sums over the image
\
\ "cksum" is used by the system to make sure the image has
\ not been corrupted or tampered with. It does it in a limited
\ way as self-modifying code means that it cannot make sure
\ that the Forth virtual machine has not been tampered with.
\ This is unfortunate. One way to get around this would be to
\ write this in assembly and do the checking much earlier in
\ the boot sequence. As this is not the case, only the Forth
\ code is checked. To make the check-sum quick enough on the
\ SUBLEQ machine no bitwise operations are used, just addition,
\ this makes for a pretty poor check-sum but is better than
\ nothing.
\
\ Another idea that could be combined with an assembly version
\ of "cksum" that would be run before the Forth VM is to make
\ the image decompress itself using an algorithm like
\ LZSS (see <https://en.wikipedia.org/wiki/LZSS>), the image is
\ highly compressible and LZSS is one of the simplest
\ dictionary compression based routines available. It is
\ doable and could save 30% or more on the image size, which
\ preliminary testing (not part of this document) confirms.
\
\ A better scheme might be to use LZSS with a different
\ set of instructions, much like:
\ <https://richg42.blogspot.com/2022/01/lzxor.html>. Which
\ extends LZSS to include a XOR command (LZSS has a "literal"
\ and "copy" command). This command XORs a block with another
\ block which may be either new or in the sliding dictionary
\ (determined by the location of where to begin XOR'ing),
\ allowing partial matches to be encoded, placing a lot
\ of work on the encoder. Instead of using XOR, which is
\ expensive to compute on this system, a Subtract could be
\ used instead.
\
\ Huffman coding might be yet another alternate way to achieve
\ this similar results.
\
\ Image encryption or obfuscation could also be done, as an
\ anti-tampering measure, which might be useful to do if the
\ program was used as part of a game or a puzzle.
\
\ The meta-compiler calculates the checksum later on, and sets
\ a known location to the checksum value. It is checked as
\ part of the initial Forth word "(cold)".
\

:s cksum aligned dup C0DE lit - >r ( a u -- u )
  begin ?dup
  while swap @+ r> + >r cell+ swap cell -
  repeat drop r> ;s

\ # Conditional Evaluation
\
\ The Forth input stream is malleable, we have seen with words
\ like "see", or "create", and in the parsing section, that
\ we can manipulate the program input at runtime. We can do
\ this in arbitrary ways, adding comments is one example. It
\ is possible to make a word that implements another language,
\ for example BASIC or C, that would still *technically* be
\ part of the Forth language, however we have a far less
\ ambitious goal - the creation of a word-set for conditional
\ evaluation of code.
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
\ could define the word on a gforth or another Forth
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
\ we could, but for the sake of simplicity, it is not done
\ nor is it required.
\
\ To keep the implementation simple there is no compiler
\ security to ensure the construct is used in its intended
\ usage. For example:
\
\        [else] ( CODE ) [then]
\
\ Will not throw an error and the CODE section will not be
\ executed.
\
\ Instead of defining these words it would have been possible
\ to extend "if...else...then" to have run-time behavior
\ outside of compilation that performed the same as
\ "\[if\]...\[else\]...\[then\]". There are very good reason
\ for not doing so however. Changing the behavior of a word
\ depending on the compiler state, whilst sometimes necessary,
\ is in the general case *bad*. It limits the use of the word
\ and can complicate understanding the word and code that uses
\ that word, what seems like something quite elegant at first
\ creates complications.
\
\ N.B. These words do not deal with nested conditional 
\ "\[if\]...\[else\]...\[then\]" statements!
\

: defined token find nip 0<> ; ( -- f )
:to [then] ; immediate ( -- )
:to [else]
 begin
  begin token c@+ while
   find drop cfa dup to' [else] lit = swap to' [then] lit = or
    ?exit repeat query drop again ; immediate
:to [if] ?exit postpone [else] ; immediate

\ # Time and Hacks
\
\ The Forth word "ms" is a standard extension word for delaying
\ for a specified number of milliseconds. The concept of
\ "sleep" is actually a complex one as it interacts with
\ operating systems, the threading model, I/O blocking, and
\ there are different optimizations that can be performed. The
\ concept of real-time systems must be introduced and the
\ difference between hard and soft real-time systems, which we
\ will not cover. Saying "this function will sleep for X
\ milliseconds" does not fully answer questions around sleep.
\ There are also problems of jitter and drift, "sleep" is
\ actually quite a complex topic.
\
\ However, given that the underlying SUBLEQ machine does not
\ have a method for determining the actual time (ie. A hardware
\ timer), how is it that this implementation provides an "ms"
\ implementation? The answer is by doing it poorly! There are
\ ways this implementation could be improved even on this
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
\ easiest would be to implement a hardware timer, to keep
\ things simple we will not do this. This behavior might be
\ desired but it most likely is not.
\
\ You should be aware of all the potential problems of "ms"
\ if you plan on using "ms" for more serious timing needs.
\

: ms for pause calibration @ for next next ; ( ms -- )

\ ## ANSI Terminal Words
\
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
\        : color csi 0 u.r ." m" ;
\
\ Note that "csi" is in the system vocabulary, so it will
\ need to be loaded before this word can be defined.
\
\ The following words could then be defined:
\
\        decimal
\        : red 31 color ;
\        : green 32 color ;
\        : blue 34 color ;
\        : reset 0 color ;
\
\ This will most likely work under Unix systems, and
\ likely fail if done under Windows CMD.EXE (although a program
\ called ANSICON can remedy that), as Windows' terminal program
\ does not support ANSI escape codes, or more accurately it
\ depends on the version of windows and settings.
\
\ Standard Forths contain the words "at-xy" and "page" for
\ controlling the cursor position and clearing the screen
\ respectively, so these words are provided. Some interesting
\ games can be made using just these two, limited terminal
\ words, but games nonetheless, games such as Hack, Chess,
\ Checkers, Sokoban, Conways Game Of Life, 2048, and
\ Minesweeper. If non-blocking input is implemented
\ games then a Tetris, Space Invaders, Pac-Man, Pong or Snake
\ clones can be made.
\
\ The first column and row in "at-xy" is "1" and not "0".
\ Although in this case "0" is equivalent to "1".
\
\ This is the alternative definition of "csi", it is slightly
\ shorter than the one given but less portable and more
\ difficult to understand.
\
\        :s csi .$ 2 tc, 1B tc, 5B tc, 0 tc, ;s
\
\ So instead of shaving bytes off of the current implementation
\ (which can be done elsewhere), the simpler version is kept
\ in.
\

: bell 7 lit emit ; ( -- : emit ASCII BEL character )
:s csi 1B lit emit 5B lit emit ;s ( -- : ANSI Term. Esc. Seq. )
: page csi ." 2J" csi ." 1;1H" ( csi ." 0m" ) ;
: at-xy radix decimal ( x y -- : set cursor position )
   >r csi #0 u.r ." ;" #0 u.r ." H" r> base ! ;

\ # Forth Blocks
\
\ Forth blocks are a neat concept, they are a minimal way of
\ giving access to mass storage that even the most spartan
\ system can provide, no file system is required, just the
\ ability to read and write blocks of data to non-volatile
\ storage.
\
\ Apart from in embedded systems, or as a building block for
\ other words, the Forth block system is a semi-obsolete way of
\ interacting with mass-storage, as Forth kernels have moved
\ on to providing a File Access Word set that is similar in
\ design to the C file functions, "fopen", "fread", etcetera.
\
\ Unfortunately the SUBLEQ machine in a quest for simplicity
\ provides no mechanism for saving to non-volatile storage, it
\ would be easy enough to add an interface which provides
\ for this but that will not happen as there is little need for
\ it.
\
\ There are some Forth implementations that instead map the
\ memory available to the implementation to blocks. This is
\ what this implementation does, the idea behind the BLOCK
\ word-set is that the user does not have to worry about the
\ details of how it works, as far as the code is concerned, it
\ could be writing to flash, to a hard-drive, or just to
\ memory like this implementation does. Some block systems
\ map contiguous sections of blocks to different types of
\ memory.
\
\ The block word-set has been added because it can be used as
\ a basis for text editing (see the block editor described
\ later) and if mass storage were to be added, this word-set
\ could be used to access it, the code using it would not have
\ to be modified, just the implementation of the word "block".
\
\ It would be interesting to build a simple file system on top
\ of the Forth Block mechanism, and a set of DOS like utilities
\ for accessing and executing files on it.  It could also be
\ used to simulate a simple DOS like operating system within
\ this Forth system. The file system would also be portable,
\ but limited. These possibilities will be described in more
\ detail in the appendix, along with the possible of adding
\ peripheral support for mass storage.
\
\ It is possible to use the block system to map arbitrary bits
\ of memory to different things, for example, we could map
\ blocks 0-63 to our main memory, and 64-127 to EEPROM, if we
\ had some available, and 128-144 to ROM, if we had that
\ available. It is a powerful abstraction that can hide the
\ implementation details whilst providing a high degree of
\ utility.
\
\ Another advantage of the block system is that it allows a
\ constrained 16-bit system to potentially address much more
\ memory than a 16-bit address would allow, up to 65536 blocks
\ of 1024 bytes.
\
\ On to the block word-set itself. A Block consists of a
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
\ that block are discarded.
\
\ Note that because this implementation of the block word set
\ just maps 1024 chunks of memory to each block the changes
\ are always reflected and there are no block transfers, this
\ is one feature of the system that could be changed to make
\ the behavior more consistent with other implementations,
\ however that this would require memory to store at least one,
\ preferably two, block buffers. It would not require any new
\ peripheral support. We could also use the block system to
\ access the full 65536 cells that are available to the SUBLEQ
\ VM, using "\[@\]", transferring the block from memory not
\ normally accessible to the Forth implementation with "@"
\ alone.
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
\ all of the work. 
\
\ If the block number is the same as the one
\ loaded then it does nothing but return a pointer to the
\ already loaded block. This version of "block" also checks
\ that there is at least one value on the stack and it also
\ calls "pause", described in the multi-threading chapter.
\
\ The word "update" marks the last loaded block as dirty,
\ allowing the block system to determine what to write back,
\ and "flush" writes back all dirty buffers back to mass
\ storage and frees all buffers. As mentioned they do nothing
\ (or little) on this system. They should still be used
\ within code that uses these block words for portability
\ reasons.
\
\ The words "list" "blank", "b/buf" are defined in this
\ chapter. It would also be a good time to describe
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
\        47 block b/buf blank
\
\ If we wanted to format block "47" for editing.
\
\ Replacing "emit" with ".emit" in "list":
\
\         : within over - >r - r> u< ; ( u lo hi -- f )
\         : .emit ( c -- )
\           dup bl 7F lit within 0= if drop [char] . then
\           emit ;
\
\ Renders "list" a little more forgiving when printing
\ binary data. You will need to add back in the line with
\ "emit" and remove the line with "type".
\
\ It might be worth having a compile time switch for more
\ or less complicated versions of "block", the simplest and
\ least functional block implementation is:
\
\        : block 1024 * ;
\
\ A little too spartan. Switching between this version of
\ "block" and one that can access all memory along with proper
\ block buffers would be useful, however.
\
\ "thru" and "screens" for loading a block range and listing
\ a block range are listed in the appendix.
\
\ Of note, assuming a proper buffered "block" implementation
\ it is allowable by the ANS Forth standard for the system
\ to have only one block buffer, this means to copy one
\ block to another the following code:
\
\
\        : copy-block ( from to -- )
\          swap block swap buffer 1024 move update ;
\
\ From:
\
\ <https://groups.google.com/g/comp.lang.forth/c/f5-xM_cl2S8>
\
\ Will *not* work on all systems, instead a block of memory
\ not under managed of the block system needs to be used, like
\ so:
\
\        1024 buffer: buf
\
\        : copy-block ( u.block-from u.block-to -- )
\          swap block buf 1024 move
\          buf swap buffer 1024 move
\          update
\          save-buffers ( optional )  ;
\
\ With "buffer:" being defined as:
\
\        : buffer: create allot ; ( u "<name>" -- ; -- addr )
\
\ And "buffer" is similar to "block" except (on systems with
\ mass storage) no transfer from mass storage is done when
\ loading a block into a block buffer (this means if you just
\ want to write to a block with no need to read from it, you
\ should use "buffer" instead of "block", "buffer" is sometimes
\ used to implement parts of the functionality of "block".
\

( system[ variable dirty ]system )
: b/buf 400 lit ; ( -- u : size of the block buffer )
: block ( k -- u )
   #1 ?depth 1- ( dup blk ! ) A lit lshift pause ; 
: flush ( dirty @ if save-buffers empty-buffers then ) ;
: update ( #-1 dirty ! ) ; ( -- : mark cur. buf. as dirty )
: blank bl fill ; ( a u -- : blank an area of memory )
: list ( k -- : display a block )
   page cr         ( clean the screen )
   dup scr ! block ( update "scr" and load block )
   F lit for       ( for each line in the block )
     F lit r@ - 3 lit u.r space    ( print the line number )
     40 lit 2dup type cr +         ( print line )
   ( 3F lit for count emit next cr \ print line )
   next drop ;

\ # The Read-Eval-Loop
\
\ Whilst there are more sections of optional material, this
\ one finally puts everything together and produces the Forth
\ interpreter loop, where we read in a line, parse it, execute
\ it and catch any errors. New support words will be
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
\ line in this file being the following as we have seen:
\
\        defined eforth [if] ' ) <ok> ! [then]
\
\ "ok" is not called directly, but is stored as an execution
\ token in "\<ok\>".
\

:s ok state @ ?exit ."  ok" cr ;s ( -- : okay prompt )

\ "eval" goes through each word in a line until there are no
\ more and executes "interpret" for each word, it is sure to
\ check the stack depth after each call to "interpret" in an
\ attempt to provide some kind of limited error detection.
\
\ It also prints out "ok", by executing the contents of
\ "\<ok\>", as just mentioned.
\
:s eval ( "word" -- )
   begin token c@+ while
     interpret #0 ?depth
   repeat drop <ok> @execute ;s

\ ## Evaluate
\
\ "evaluate" takes a string and evaluates that string, it
\ is careful to preserve the input state prior to evaluation
\ so that it can be restored after, it also catches any error
\ that might occur before re-throwing them so it can make sure
\ to always restore that input state. "source-id" will be set
\ to -1 for the duration of the evaluation.
\
\ Now that we have "evaluate" we can use it to extend the block
\ words...
\

: evaluate ( a u -- : evaluate a string )
  get-input 2>r 2>r >r        ( save the current input state )
  #0 #-1 to' ) lit set-input  ( set new input )
  t' eval lit catch           ( evaluate the string )
  r> 2r> 2r> set-input        ( restore input state )
  throw ;                     ( throw on error )

\ "load" is one of the missing words needed by our block
\ word set, it evaluates a forth block, treating each 64 bytes
\ as a single line. That has the consequence that comments
\ end at those line boundaries.
\
\ * "line" is used to get a line from a block.
\ * "loadline" is a common factor of "load", used to evaluate
\   a single line.
\ * "load" evaluates an entire block.
\
\ The other two words missing from this version of forth, that
\ could easily be added later, are "thru" and "--\>", "thru"
\ loads and executes a range of blocks inclusively, and "--\>"
\ when used within a block discards the rest of the block from
\ being executed and continues execution from the next block,
\ which can be chained together. The next block is the block
\ with the current block number plus one.
\
:s line 6 lit lshift swap block + 40 lit ;s ( k l -- a u )
:s loadline line evaluate ;s ( k l -- ??? : execute a line! )
: load ( k -- : exec blk )
   blk @ >r dup blk ! #0 F lit for
   2dup 2>r loadline 2r> 1+ next 2drop r> blk ! ; 

\ Two words that allow the user of the environment to get
\ information about the system, "eforth", which has uses in
\ conditional evaluation, pushes the version number of the
\ forth interpreter. The version number has the format of
\ MAJOR.MINOR version numbers, with the MAJOR number stored
\ in the upper byte, and the MINOR in the lower.
\
\ "info" is optionally printed out at start up, a bit has
\ to be enabled in the "{options}" variable for that to happen,
\ it is not needed, but it means that information about
\ the project is stored within it.
\
\ It might be to refactor "info" into multiple words (if we
\ had the space) to break down the information provided into
\ a word for "author", and "version", such as this:
\
\        :s project $" eForth v1.8" ;s
\        :s author $" Richard James Howe" ;s
\        :s email $" howe.r.j.89@gmail.com" ;s
\        :s repo $" https://github.com/howerj/subleq" ;s
\        :s license $" The Unlicense / Public Domain" ;s
\        :s line count type cr ;s ( a -- )
\        :s info project line author line email line
\           repo line license line ;s
\
\ Which makes the word more Forth-like.
\
\ Also note that the license applies to the Forth image and
\ not the book!
\
:r eforth 0109 lit ;r ( --, version )

opt.info [if]
  :s info cr ( --, print system info )
    ." eForth v1.9, Public Domain,"  here . cr
    ." Richard James Howe, howe.r.j.89@gmail.com" cr
    ." https://github.com/howerj/subleq" cr ;s
[else]
  :s info ;s ( --, [disabled] print system info )
[then]

\ ## Task Initialization
\
\ "task-init" is a poor Forth function, it really should be
\ split up into words that deal with the different aspects of
\ setting up a task but we lack space.
\
\ The word must set up the user variables to point to the
\ default execution tokens or to their default values. It
\ also sets the variables for the saved register locations,
\ and an initial execution token of "bye", which should be
\ replaced before the task is executed or the system will halt.
\
\ Note that this word also inspects the "{options}" bit, the
\ first bit, which turns off echoing of the user input if
\ it is set, turning echoing on or off can be useful depending
\ on your terminal settings. If your terminal echos back what
\ you have written then you will want echoing off otherwise
\ each character written will be doubled up, if it does not
\ then you will want it on, otherwise your terminal will be
\ eerily silent and you will not be able to see what you have
\ typed in.
\
\ This word should be treated as a list of things to be
\ initialized. That list includes:
\
\  1. Setting up the task linked list pointer, linking in
\     the task to the list of all running tasks.
\  2. Setting the initial execution token.
\  3. Setting up return and variable stacks.
\  4. Making the saved top of the stack empty for the task.
\  5. Setting the default input and output radix (to decimal).
\  6. Setting up the I/O behavior.
\  7. Making the input buffer position "\>in" zero.
\  8. Putting default values in variables like "dpl".
\  9. Making the terminal input buffer point to the right place
\     within the tasks memory.
\ 10. Putting the thread into the right "state", into command
\     mode by default, it is not expected that multiple threads
\     will be defining words (which will cause problems) or
\     executing commands (which may cause problems), but it is
\     setup just in case.
\
\ ### "xio" and related words
\
\ The "xio" function and associated words are used to change
\ the Input and Output characteristics of the system in a
\ decomposed manner, allowing a more fine grained control of
\ behavior.
\
\ This is best shown with a description of the words
\ themselves, and what they are and can potentially be used
\ for.
\
\ * "io!", or Initialize I/O, sets up the Input/Output for the
\ system, for example on some eForth system it might setup a
\ UART, its baud rate, and other characteristics, on other
\ hosted systems it might be a No-Operation, in this one it
\ just calls "console", another word defined slightly later
\ on.
\ * "xio", or Exchange I/O, takes three executions tokens and
\ sets the vectors for "\<echo\>", "\<ok\>", "\<tap\>" and
\ "\<expect\>" (it sets "\<expect\>" to the default word
\ "accept". This does not change where the input comes from
\ or goes to, but terminal (processing backspace characters
\ or not, for example) behavior, the prompt (usually printing
\ out "ok" after each line), and how input is acquired given
\ an input stream and processed into a line.
\ * "hand" provides default execution vectors for the okay
\ prompt, terminal input handling with ktap, and then calls
\ "xio".
\ * "console" sets the default input and output vectors for
\ "\<key\>" and "\<emit\>" to read from and output to
\ the terminal, or console, hence the name. It then
\ in turn calls "hand", which in turn calls "xio". This can
\ be used to get the interpreter back into a "normal" state.
\ * "pace" emits a single ASCII character, a vertical tab,
\ or the 11th character in the table of ASCII characters.
\ * "file" sets up I/O for a file transfer, in the original
\ eForth this requires support from the word "quit", which
\ would detect if the execution vector for the okay prompt was
\ not the one that printed "ok", if so, on error it would emit
\ an ASCII escape character instead of an error message. "file"
\ also set the execution vector of "\<ok\>" to "pace" and
\ disabled echoing of characters back to the user, this allowed
\ file redirection to give meaningful feedback to the user
\ without overwhelming them on a DOS prompt. The interpreter
\ loop in this version of Forth does not do that.
\
\ It is not difficult to imagine a better version of "file",
\ or at least a more feature rich version, perhaps one that
\ could be used as a basis of networking between different
\ eForth systems with access to a common bus, one that would
\ leverage the Data-Is-Code approach of Forth (opposite to the
\ Code-Is-Data approach that Lisp takes) and existing Forth
\ mechanism for reading and executing words like the Forth
\ block editor does.
\
\ This would in effect make two of the text interpreters the
\ programmer normally uses talk to each other in an automated
\ manner, with little difference between typing the commands
\ in and executing them. The line input mechanism could be
\ extended with a line length and checksum prefix to ensure
\ data integrity over a potentially unreliable communications
\ medium (which means "accept" would have to be vectored to a
\ different word that implemented this), and acknowledgement
\ of success echoed back via "ok".
\
\ This is all hypothetical, but is one reason for vectoring
\ I/O, another is so that you can read directly from a file
\ system without much difficulty given the facilities for file
\ access.
\
:s xio t' accept lit <expect> ! <tap> ! <echo> ! <ok> ! ;s
:s hand t' ok lit
    t' (emit) lit ( Default: echo on )
    {options} lit @ #1 and if drop to' drop lit then
    t' ktap lit postpone [ xio ;s ( -- )
:s pace B lit emit ;s ( -- : emit pacing character )
:s file t' pace lit to' drop lit t' ktap lit xio ;s ( -- )
:s console t' key? lit <key> ! t' (emit) lit <emit> ! hand ;s
:s io! console ;s ( -- : setup system I/O )

\ NB. It might be slightly more efficient to copy a block of
\ constants into the new task, patching things up after.
:s task-init ( task-addr -- : initialize USER task )
  {up} lit @ swap {up} lit !
  this 2/ {next-task} up !
  to' bye lit 2/ {ip-save} up ! ( Default execution token )
  this =stksz        lit + 2/ {rp-save} up !
  this =stksz double lit + 2/ {sp-save} up !
  #0 {tos-save} up !
  decimal
  io!
  t' (literal) lit <literal> !
  to' bye lit <error> !
  #0 >in ! #-1 dpl !
  this =tib lit + #0 tup 2! \ Set terminal input buffer loc.
  {up} lit ! ;s

\ "ini" initializes the current task, the system brings itself
\ up at boot time, and where "ini" is executed is critical,
\ before it is called none of the words that rely on the
\ execution tokens being set can be executed without causing
\ the system to reboot. It is called from "(cold)", the first
\ forth word to run.
\
:s ini {up} lit @ task-init ;s ( -- : initialize current task )

\ ## Quit and Cold
\
\ "quit" is the heart of the Forth system, it is responsible
\ for fetching and evaluating each line of Forth code. It
\ also contains the error handler of last resort, which catches
\ all errors that have not been caught further up the call
\ stack and resets the system with some sensible defaults.
\ The only exception when it comes to exceptions is the ABORT
\ signal, which causes the interpreter to halt.
\
\ The actual interpreter loop is simple, get a line with
\ "query", execute the line with "eval" under "catch", and
\ handle any errors, loop until satisfied. It also sets up the
\ default error handler, replacing the error handler set in
\ "task-init" with one that prints out an error message instead
\ of calling "bye".
\
\ The name "quit" certainly is an odd one, and despite being
\ a poor name for the top interpreter loop (it would be better
\ suited for a word that halted the machine), the name comes
\ from the fact that calling "quit" makes the Forth system
\ quit what it is doing and continue on executing Forth if it
\ is called.
\
\ "(error)" is the default error handling word, it will be
\ called from via the "\<error\>" execution vector. "(error)"
\ prints out the error number, then a "?" (inspired by the
\ wonderful error handling of the text editor "ed", expounded
\ upon in the article "Ed is the standard text editor"), and
\ calls "bye" if the error was "-1", which is "abort". If not
\ it calls "ini", but makes sure it will be called in case of
\ an error next time instead of "bye", as "ini" sets
\ "\<error\>" to "bye". Another candidate for error handling is
\ the "\<ok\>" vector, instead of having a separate prompt
\ vector, that could also act as an error handler, as we never
\ want to print out "ok" when an error has occurred. To keep
\ things conceptually simple, they are in separate vectors.
\

:s (error) ( u -- : quit loop error handler )
   dup space . [char] ? emit cr #-1 = if bye then
   ini t' (error) lit <error> ! ;s

: quit ( -- : interpreter loop )
  t' (error) lit <error> !         ( set error handler )
  begin                            ( infinite loop start... )
   query t' eval lit catch         ( evaluate a line )
   ?dup if <error> @execute then   ( error? )
  again ;                          ( do it all again... )

\ "(cold)" is the first word that gets executed, it performs
\ the task of continuing to setup the environment before
\ normal running. It must:
\
\ 1. Setup which word lists are used ("forth definitions")
\ 2. Initialize the current task with "ini".
\ 3. Check the boot options in the "{options}" variable
\    a. The first bit is checked within "ini", the 4th within
\       the word "key".
\    b. Check 3rd bit and call "info" if set
\    c. Check 2nd bit and perform a checksum over the
\       Forth image, it then toggles the 2nd bit making
\       it so the image is not checked again (as adding
\       new definitions modifies the image, calling "(cold)"
\       again after defining new words would mean the checksum
\       would fail. If the checksum fails it prints an error
\       messages and halts.
\ 4. Calls "quit" to enter into the Forth interpreter loop.
\
\ And that completes the Forth boot sequence.
\
:s (cold) ( -- : Forth boot sequence )
  forth definitions ( un-mess-up dictionary / set it )
  ini ( initialize the current thread correctly )
  {options} lit @ 4 lit and if info then ( display info? )
  {options} lit @ #2 and if ( checksum on? )
    primitive lit @ 2* dup here swap - cksum  ( calc. cksum )
    check lit @ <> if ." bad cksum" bye then ( oops... )
    {options} lit @ #2 xor {options} lit ! ( disable cksum )
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
\ and makes multitasking usable. It adds words to
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
\ no rationale. It might seem like a complication to
\ the implementation, and it certainly is if you never plan
\ on using it. It was actually easy to implement,
\ especially compared to implementing the basic operators from
\ scratch with no debugging facilities. If you never use
\ another thread or never plan on using one, you can skip all
\ this and you do not need to concern yourself with it.
\
\ The point of multithreading is to divide up the processor
\ time so that multiple threads of execution can share the
\ same processor and pretend they are executing on a single
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
\ everything to appear as if it is being computed all at the
\ same time, even on single CPU core systems (which are getting
\ are getting rarer nowadays even in the embedded computer
\ space).
\
\ Threading and different threading models "solve" this, and
\ do so in different ways. The cooperative threading model is
\ the simplest, the easiest to use and the easiest to get
\ right. It does have a disadvantage in that a single thread
\ of execution can hold up and block the entire system from
\ running, and each thread must manually have "pause" functions
\ inserted in it for it to work.
\
\ As we control the virtual machine, it would be possible to
\ alleviate some of this by making it do the "pause"
\ functionality every X instructions completed, but that would
\ slow down normal execution.
\
\ An example program, that uses the tasks (make sure the system
\ vocabulary is loaded prior to executing this):
\
\         task: rx
\         task: tx1
\         task: tx2
\
\         : .tx1 begin [char] X rx send 100 ms again ;
\         : .tx2 begin [char] Y rx send 200 ms again ;
\         : .rx begin
\           multi receive single . space emit cr again ;
\
\         single
\         ' .tx1 tx1 activate
\         ' .tx2 tx2 activate
\         ' .rx rx activate
\         : schedule begin pause again ;
\         multi schedule
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
\ If a thread returns an error condition exists, each thread
\ should never return. It would be possible to make it so
\ that by default each thread when initialized pushes a word
\ onto the return stack that removes the current thread from
\ execution, calling "bye" when no words are left, but this
\ is not done.
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
\ This section itself could be expanded upon greatly, but it
\ would become a book in of itself, threading and scheduling
\ are the domain of operating systems. Books that deal with
\ operating systems such as Unix, or real-time embedded systems
\ are best consulted to get an understanding of this deep
\ topic. Different approaches have different trade-offs, a
\ scheduler for a real time system is very different from a
\ preemptive one, likewise for single and multicore systems.
\ And an operating system that deals with a Memory Management
\ Unit is also very different to a system that uses real
\ addresses exclusively like this one.
\
\ One type of cooperative multithreading system that I am fond
\ of using in the embedded realm is to use cooperative
\ multithreading and only fire an interrupt to signal an error
\ if the allotted amount of time given to a task is exceeded,
\ this cannot be done in this system because there is both no
\ concept of time nor an interrupt mechanism.
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

opt.multi [if]
:s task: ( "name" -- : create a named task )
  create here b/buf allot 2/ task-init ;s
:s activate ( xt task-address -- : start task executing xt )
  dup task-init
  dup >r swap 2/ swap {ip-save} lit + ! ( set execution word )
  r> this @ >r dup 2/ this ! r> swap ! ;s ( link in task )
[then]

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
\ "signal" could be replaced with "!". "signal" by
\ default sets the variable to be waited on to the current
\ task address (all task addresses being non-zero). This is a
\ debugging aid so you can tell which task has acquired a
\ resource.
\

opt.multi [if]
:s wait ( addr -- : wait for signal )
  begin pause @+ until #0 swap ! ;s
:s signal this swap ! ;s ( addr -- : signal to wait )
[then]

\ "single" and "multi" turn off and on multitasking. This
\ can be used to prevent other tasks from interrupting the
\ currently running task, useful for preventing I/O from
\ multiple threads from interfering with each other.
\

opt.multi [if]
:s single #1 {single} lit ! ;s ( -- : disable other tasks )
:s multi  #0 {single} lit ! ;s ( -- : enable multitasking )
[then]

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
\ This is a simple way to do limited inter-thread
\ communication.
\

opt.multi [if]
:s send ( msg task-addr -- : send message to task )
  this over {sender} lit +
  begin pause @+ 0= until
  ! {message} lit + ! ;s
:s receive ( -- msg task-addr : block until message )
  begin pause {sender} up @ until
  {message} up @ {sender} up @
  #0 {sender} up ! ;s
[then]

\ That completes the multitasking section, there are only a
\ handful of words on top of the base that the Forth system
\ provides, but they are enough for most tasks.
\
\ Possibly missing are words to put to sleep and wake up a
\ task, if you need them, implement them yourself.
\
\ # Forth Text / Block Editor
\
\ I love Forth blocks, they are simple to implement and
\ simple to understand. They are of course an obsolete way of
\ doing things, suitable for a bygone era, or perhaps on some
\ limited embedded systems.
\
\ We can make a small, and usable, text editor
\ based on our "persistent" storage mechanism. It packs a lot
\ of functionality into a small space, and it does this
\ by reusing a lot of functionality built into Forth. The
\ Forth Block mechanism takes care of storage, retrieval and
\ swapping in and out "dirty" blocks, the Forth interpreter
\ can handle command parsing and line based input. We just
\ need to meld these concepts into an editor. We even have
\ a word for executing Forth blocks as part of the base system.
\
\ For more information on Forth block editors the following
\ links are useful:
\
\ - <http://tunes.org/wiki/block_20editor.html>
\ - <https://wiki.c2.com/?ForthBlocks>
\
\ Alternatively search for "FORTH BLOCK EDITOR" in your
\ favorite internet search engine.
\
\ To make sure that these newly defined commands do not
\ interfere with the main vocabulary we will put them in
\ a vocabulary called "editor". We will be defining short
\ one letter commands that are easy to type, so it is important
\ that we do this so as to not clutter up the base system.
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
\ they are; "q", "?", "l", "x", "ia", "a", "w", "s", "n", "p",
\ "r", "z", and finally "d". Some of the words are not
\ strictly necessary in this editor, but they are not large
\ and are useful.
\
\ Here is a short description of each of the commands:
\
\ * "editor", enter editor mode
\ * "q", quit editor
\ * "?", display current block number
\ * "l", list current forth block
\ * "x", execute current forth block
\ * "ia", insert line of text into line at position
\ * "a", insert line of text into line
\ * "w", list commands
\ * "s", save and flush
\ * "n", go to next block and list it
\ * "p", go to previous block and list it
\ * "r", list a specific block
\ * "z", erase the screen, replacing it with spaces
\ * "d", delete a line, it takes a number as an argument
\
\ Missing are words to perform searching, replacing, and
\ swapping lines. A rudimentary help message might be useful.
\
\ A command to enter a mode to enter 16 consecutive lines
\ would aid in editing larger amounts of text. Perhaps empty
\ lines would exit this mode early.
\
\ They are all easy to add, but are not necessary. The
\ fact that execute, "x", calls "q" might cause problems when
\ trying to put words into different vocabularies, but it is
\ not an insurmountable problem. 

\ Also "d" might clash with the hexadecimal value for the 
\ number 13, in this Forth it is not a problem as hexadecimal 
\ numbers use uppercase only, and this Forth is case sensitive.
\
\ The editor can also be used to enter data with a series
\ of commands into blocks to create databases if file
\ redirection is used.
\
\ An example of its use:
\
\        editor
\        z
\        0 a ( HELLO WORLD PROGRAM VERSION 3.4 )
\        1 a : ahoy cr ." HELLO, WORLD" ;
\        2 a ahoy
\        l
\        x
\
\ This will make the block containing the following text:
\
\        ( HELLO WORLD PROGRAM VERSION 3.4 )
\        : ahoy cr ." HELLO, WORLD" ;
\        ahoy
\
\ Which when run should print out:
\
\        HELLO, WORLD
\
\ It takes a little getting used to, but is not that difficult.
\ Feel free to edit the commands into something more suitable
\ to what you prefer, or add new ones.
\
\ The only complex word is "ia", which also forms the basis
\ for "i", it inserts a line of text into a line at a location
\ after making sure there are at least two items on the stack.
\ The word "ia" does not do range checking on those variables
\ unfortunately, a common "feature" of Forth.
\
\ It looks at the Terminal Input Buffer, with "\>in" and "tib",
\ copying the results into the location within the block
\ specified, and then skips over the line so it is not
\ executed. It also calls "update", which marks the current
\ block as dirty (as it has just been modified), this means the
\ block is automatically saved when moving to the next or
\ previous block. Of course, as there is no mass storage in
\ this SUBLEQ machine, so nothing is written to it, however it
\ is a nice feature for portabilities sake.
\
\ Most of the other commands are simple, they manipulate the
\ screen pointer by adding to it, or retrieving it.
\
\ A lot of the words call "l", to list the current screen,
\ so after modification or changing of the screen variable
\ "scr" the user does not have to type "l" themselves. On
\ slow connections (imagine you are talking to this Forth over
\ a 300 baud modem) "l" should be removed, and it would be
\ worth rewriting the code to draw and redraw only what is
\ necessary, complications which are not needed.
\

opt.editor [if]
: editor {editor} lit #1 set-order ; ( Micro BLOCK editor )
:e q only forth ;e ( -- : exit back to Forth interpreter )
:e ? scr @ . ;e ( -- : print block number of current block )
:e l scr @ list ;e ( -- : list current block )
:e x q scr @ load editor ;e ( -- : evaluate current block )
:e ia #2 ?depth 6 lit lshift + scr @ block + tib >in @ +
   swap source nip >in @ - cmove tib @ >in ! update l ;e
:e a #0 swap ia ;e ( line --, "line" : insert line at )
:e w words ;e ( -- : display block editor commands )
:e s update flush ;e ( -- : save edited block )
:e n  #1 scr +! l ;e ( -- : display next block )
:e p #-1 scr +! l ;e ( -- : display previous block )
:e r scr ! l ;e ( k -- : retrieve given block )
:e z scr @ block b/buf blank l ;e ( -- : erase current block )
:e d #1 ?depth >r scr @ block r> 6 lit lshift + 40 lit
   blank l ;e ( line -- : delete line )
[then]

\ # Extra Control Structures
\
\ This section contains extra control structures not usually
\ present in eForth, and some are not standard Forth constructs 
\ at all. They include the "do...loop", case-statements, macros
\ and "many".
\

opt.control [if]

: rpick rp@ swap - 1- 2* @ ; ( n -- u, R: ??? -- ??? )

\ "many" is an interesting word, it allows a line of code to be
\ executed an infinite number of times by postfixing it to the
\ end of the command. That is less interesting compared to the
\ way it does it, by manipulating the input line to make it so
\ it is executed again, the line is then re-parsed and executed
\ again, including "many", which triggers another re-parsing
\ and so on. It is a neat little word.
\
\ Extensions to the word could include executing the same
\ line X amount of times, or quitting on a key press or
\ specific key.
\
: many #0 >in ! ; ( -- : repeat current line )

\ Case Statements:
\
\ Usage:
\
\        : x case
\          1 of ." one" endof
\          2 of ." two" endof
\          ." default"
\          endcase ;
\

:s (case) r> swap >r >r ;s compile-only
:s (of) r> r@ swap >r = ;s compile-only
:s (endcase) r> r> drop >r ;s

: case compile (case) $1E lit ; compile-only immediate
: of compile (of) postpone if ; compile-only immediate
: endof postpone else $1F lit ; compile-only immediate
: endcase
   begin
    dup $1F lit =
   while
    drop
    postpone then
   repeat
   $1E lit <> -$16 lit and throw ( abort" Bad case construct!" )
   compile (endcase) ; compile-only immediate

:s r+ 1+ ;s ( NB. Should be cell+ on most platforms )
:s (unloop) r> rdrop rdrop rdrop >r ;s compile-only
:s (leave) rdrop rdrop rdrop ;s compile-only
:s (j) 4 lit rpick ;s compile-only
:s (k) 7 lit rpick ;s compile-only
:s (do) r> dup >r swap rot >r >r r+ >r ;s compile-only
:s (?do)
   2dup <> if
     r> dup >r swap rot >r >r r+ >r exit
   then 2drop ;s compile-only
:s (loop)
  r> r> 1+ r> 2dup <> if
    >r >r 2* @ >r exit \ NB. 2* and 2/ cause porting problems
  then >r 1- >r r+ >r ;s compile-only
:s (+loop)
   r> swap r> r> 2dup - >r
   #2 pick r@ + r@ xor 0>=
   3 lit pick r> xor 0>= or if
     >r + >r 2* @ >r exit
   then >r >r drop r+ >r ;s compile-only

: unloop compile (unloop) ; immediate compile-only
: i compile r@ ; immediate compile-only
: j compile (j) ; immediate compile-only
: k compile (k) ; immediate compile-only
: leave compile (leave) ; immediate compile-only
: do compile (do) #0 , here ; immediate compile-only
: ?do compile (?do) #0 , here ; immediate compile-only
: loop
  compile (loop) dup 2/ ,
  compile (unloop)
  cell- here cell- 2/ swap ! ; immediate compile-only
: +loop
  compile (+loop) dup 2/ ,
  compile (unloop)
  cell- here cell- 2/ swap ! ; immediate compile-only

\ This is a late binding macro system, it makes a macro out
\ of a name and the rest of the current line.
\
\ Usage:
\
\        macro square dup *
\        : foo 5 square . ;
\ 
\ Note that:
\
\        : * ." ???" ;
\        : foo 5 square ;
\
\ Prints out:
\
\        ??? 5
\
\ This is due to the fact, already mentioned, that this macro
\ system is *late binding* and not *early binding*.
\
\ Another version using "sliteral", not yet implemented in
\ this system, is:
\
\   : macro 
\     : char parse postpone sliteral postpone evaluate
\     postpone ; immediate ;
\
\ It has a slightly different syntax:
\
\        macro square " dup * "
\
\ And the same problems.
\

:s scopy ( b u -- b u : copy a string into the dictionary )
  align here >r aligned dup allot
  r@ swap dup >r cmove r> r> swap ;s
:s (macro) r> 2* 2@ swap evaluate ;s

\ If "does\>" worked at time of meta-compilation we could
\ do the following:
\
\       : macro ( c" xxx" --, : create a late-binding macro )
\         create postpone immediate align here #2 cells + ,
\         #0 parse dup , scopy 2drop
\         does> 2@ swap evaluate ;
\
\ Instead we have to manipulate the dictionary and compile
\ a word "(macro)" into the correct place.
\

: macro ( c" xxx" --, : create a late-binding macro )
  create postpone immediate 
  cell negate allot compile (macro)
  align here #2 cells + ,
  #0 parse dup , scopy 2drop ;

[then] ( opt.control )


\ # Dynamic Memory Allocation
\
\ This section contains an optional dynamic memory allocator,
\ that has been adapted to compile under the meta-compiler, it
\ is complicated code that is worth not implementing yourself
\ but borrowing it from somewhere else, this allocator came
\ from here:
\
\        Dynamic Memory Allocation package
\        this code is an adaptation of the routines by
\        Dreas Nielson, 1990; Dynamic Memory Allocation;
\        Forth Dimensions, Volume. XII, No. 3, pp. 17-27
\        See: <http://www.forth.org/fd/FD-V12N4.pdf>.
\
\ There is a lot of good Forth code in the publications
\ "Forth Dimensions" (and the German "Vierte Dimensions"). See
\ <http://www.forth.org/fd/FDcover.html> for more information.
\
\ This code is not an example of "good" Forth code as it keeps
\ many items on the stack and uses "pick" to access those
\ items, however it works, is compact, easy to integrate and
\ portable. For those reasons even those it eschews good Forth
\ practice it is good code.
\
\ As there is a default arena that will be setup if none is
\ specified you can use "allocate", "free" and "resize" without
\ running any initialization code.
\
\ There are a few things that could be improved with this
\ implementation; putting the "freelist" variables within the
\ arena, allowing the user to get more information about the
\ allocation arena and already allocated pointers, an option
\ to zero all allocated (but not resized!) pointers and
\ of course more documentation and testing.
\
\ By default this code will not be compiled in to the target
\ image to save on space as it is not needed for
\ meta-compilation, and most Forth programs do not use these
\ routines.
\
\ A rough overview of the dynamic memory allocation subsystem
\ is that memory is managed by a free-list, with adjacent free
\ blocks merged together when possible in "free". The variable
\ "freelist" is a special case, it contains a node not within
\ the arena being used for allocations.
\
\ Note that the memory allocator is global, does not call
\ "pause" or interact with the multi-threading library in
\ any way, nor does it need to in a cooperative multithreading
\ environment.
\
opt.allocate [if]

system[

\ Here we define some helper words, most of which were not
\ in the original system, "freelist", a variable, was however.
\
\ Nodes in the "freelist" are stored as a pointer to the next
\ item in the list and a length of the free block, we also
\ store the length of the entire allocated block after the
\ two cells in "freelist", which can be accessed with
\ "\>length".
\
\ * "pool" is the location of the default pool, located at
\ address "$F800" (just before the first or main thread of
\ execution) and is $400 bytes in size (or 1024 bytes).
\ * "arena?" is used to check whether a pointer is within
\ the given arena. There are other checks that could be
\ done, for example it is possible to determine whether a
\ pointer is within the list of items in the free-list (and
\ hence not a valid pointer). It is possible to add these
\ features in, which are not present in the Standard C library
\ functions for memory management (which is a little too
\ spartan, it is lacking much in the way of functionality
\ such as being able to use custom allocators).
\ * "\>size", after performing a basic check with "arena?"
\ "\>size" gets the size of a pointer previously allocated
\ with "allocate"/"(allocator)" (or "resize"/"(resize)". This
\ is another feature missing from the standard C allocation
\ functions.
\ * "arena!" is used to initialize an arena and "freelist"
\ variable so it can be used with the allocation functions.
\

  ( pointer to beginning of free space )
variable freelist 0 t, 0 t, ( 0 t' freelist t! )

: >length #2 cells + ; ( freelist -- length-field )
: pool $F800 lit $400 lit ; ( default memory pool )
: arena! ( start-addr len -- : initialize memory pool )
  >r dup $80 lit u< if -B lit throw then ( arena too small )
  dup r@ >length !
  2dup erase
  over dup r> ! #0 swap ! swap cell+ ! ;
: arena? ( ptr freelist -- f : is "ptr" within arena? )
  dup >r @ 0= if rdrop drop #0 exit then
  r> swap >r dup >r @ dup r> >length @ + r> within ;
: >size ( ptr freelist -- size : get size of allocated ptr )
  over swap arena? 0= if -3B lit throw then
  cell - @ cell - ;

\ "(allocate)", "(free)" and "(resize)" are defined, they
\ perform the same functionality as "allocate", "free", and
\ "resize" do, apart from the fact that it is possible to pass
\ in a custom arena to allocate memory in.
\
\ Allocate "u" bytes, return pointer to block and result flag,
\ zero for success, check to see if pool has been initialized.
: (allocate) ( u -- addr ior : dynamic allocate of 'u' bytes )
  >r
  aligned
  r@ @ 0= if pool r@ arena! then ( init to default pool )
  dup 0= if rdrop drop #0 -3B lit exit then ( not allowed )
  cell+ r@ dup
  begin
  while dup @ cell+ @ #2 pick u<
    if
      @ @ dup ( get new link )
    else
      dup @ cell+ @ #2 pick - #2 cells max dup #2 cells =
      if
        drop dup @ dup @ rot
        ( prevent freelist address from being overwritten )
        dup r@ = if rdrop 2drop 2drop #0 -3B lit exit then
        !
      else
        2dup swap @ cell+ ! swap @ +
      then
      2dup ! cell+ #0 ( store size, bump pointer )
    then              ( and set exit flag )
  repeat
  rdrop nip dup 0= -3B lit and ;

\ Free space at "ptr", return status, zero for success.

: (free) ( ptr freelist -- ior : free pointer from "allocate" )
  >r
  dup 0= if rdrop #0 exit then
  dup r@ arena? 0= if rdrop drop -3C lit exit then

  cell- dup @ swap 2dup cell+ ! r> dup
  begin
    dup 3 lit pick u< and
  while
    @ dup @
  repeat

  dup @ dup 3 lit pick ! ?dup
  if
    dup 3 lit pick 5 lit pick + =
    if
      dup cell+ @ 4 lit pick + 3 lit pick cell+ ! @ #2 pick !
    else
      drop
    then
  then
  dup cell+ @ over + #2 pick =
  if
    over cell+ @ over cell+ dup @ rot + swap ! swap @ swap !
  else
    !
  then
  drop #0 ;

\ "(resize)" is used to implement "resize", except it takes
\ a "freelist" as a parameter. It is similar to the C
\ function "realloc", it can allocate new memory or
\ alternatively free or resize an already allocated block of
\ memory depending on the arguments given to it.
\
\ "(resize)" should free a block of memory if the new size
\ is some fraction of what the currently allocated block is,
\ but above some threshold to prevent needless reallocations.
: (resize) ( a-addr1 u freelist -- a-addr2 ior )
  >r
  dup 0= if drop r> (free) exit then
  over 0= if nip r> (allocate) exit then
  2dup swap r@ >size u<= if drop #0 exit then
  r@ (allocate) if drop -3D lit exit then
  over r@ >size
  #1 pick 3 lit pick >r >r cmove r> r> r>
  (free) if drop -3D lit exit then #0 ;

]system

\ "allocate", "free", and "resize" are wrappers around
\ "(allocate)", "(free)" and "(resize)", just passing those
\ routines the address of the default memory arena. A larger
\ system would perhaps allow for custom allocators using
\ execution vectors instead of "(allocate)" or "(free)"
\ directly. One reason to do this is to allow for faster
\ and more efficient allocations where you know what the
\ allocation patterns of some code but do not want to modify
\ it.
\
\ For example, the following system:
\
\
\        variable arena
\        here constant start
\        create $400 allot
\        start arena !
\
\        : allocate ( u -- ptr ior )
\           aligned arena @ >r arena +! r> #0 ;
\        : free drop #0 ; ( u -- ior )
\
\ Will work fine given the following *assumption* that you do
\ not need to allocate more than 1024 ($400 in hex) bytes
\ before the task you need to complete is done. This special
\ purpose allocator wastes less space and is faster to both
\ allocate and free memory. Once the task is complete the
\ arena can be reset, freeing all memory, by pointing the
\ "arena" variable back to the "start" constant. It has
\ obvious, serious, downsides, in that memory is not actually
\ freed and that there is no error checking (allocations always
\ succeed).
\

: allocate freelist (allocate) ; ( u -- ptr ior )
: free freelist (free) ; ( ptr -- ior )
: resize freelist (resize) ; ( ptr u -- ptr ior )

[then]

opt.float [if] ( Large section of optional code! )

\ # Floating Point Package (and more)
\
\ TODO: Testing!
\ TODO: Handle different bases?
\ TODO: Handle under/overflow?
\
\ This is a Forth Floating point package *for 16-bit systems*.
\
\ It has been extended and modified from the original adding 
\ many of the standard Forth floating point words as well as 
\ making it work on a more modern Forth system as the original 
\ Floating Point package, published in the 1980s, targeted 
\ Forth-83. It was published in Vierte Dimensions (Vol.2, 
\ No.4 1986) with the article and code being written by Robert 
\ F. Illyes.
\
\ It uses non-standard Floating Point numbers (i.e. not 
\ IEEE 754 floats, first published in 1985 just before the
\ publication of the article). It uses a 16-bit value for the
\ mantissa and another for the exponent and contains no special
\ values like "NaN" (Not a number) or +/- "INF" (Infinity).
\
\ Overflow or Underflow is not caught. These properties could
\ be added in if needed.
\
\ The original system was quite spartan, it had a system for
\ entering floating point numbers and printing them, 
\ converting between single/double integers and floats,
\ a few comparison operators, floating point addition,
\ subtraction, division and multiplication, and a few other
\ useful floating point words like "fexp" and "exp". However
\ it lacked many of the floating point manipulation words like
\ "ftuck", as well as nearly all of the transcendental 
\ functions, which have been added in.
\
\ The library is to show that it is possible to add floating
\ point functionality to a Forth, and so have not been used
\ in anger. It is quite some of the functions are incorrect.
\
\ The ones that are correct likely have poor accuracy over
\ some or all of their input domains.
\
\ This floating point system uses the variable stack to store
\ floating point numbers. A separate floating point stack,
\ holding say 8 floating point values, could be made. It could
\ even be made into a compile time option, using the words that
\ store their items on the stack and redefining them. If this
\ option were to be chosen the floating point stack should be
\ made to be thread local. This would require a minor rewrite.
\
\ Floating point input is finicky, and requires "dpl" to be 
\ set, so "fone e 1" will not work, but "1.0 e 1" will. 
\ Likewise with "1.0 f" and "fone f", the former working, and 
\ the latter not. The problem is that the number parsing
\ routines need modifying, which for an add-on component is
\ not possible to do in a standard way.
\
\ Another note, negative zero is not handled in all cases.
\
\ Some references and links that are useful:
\
\ * <https://stackoverflow.com/questions/3581528/>
\ * <https://stackoverflow.com/questions/4541130/>
\ * <https://forth-standard.org/standard/float/>
\ * <https://stackoverflow.com/questions/42537957>
\ * FORTH-83 Floating Point by Robert F. Illyes, Vierte 
\ Dimensions Vol.2, No.4 1986.
\ * <https://en.wikibooks.org/wiki/Digital_Circuits/CORDIC>
\ 
\ Some floating point discussions on comp.lang.forth:
\
\ * <https://groups.google.com/g/comp.lang.forth/c/H8Bs-5JSArc> 
\ * <https://groups.google.com/g/comp.lang.forth/c/pMl8Vzr00X0>
\

\ : undefined? bl word find nip 0= ; ( "name", -- f )
\ : defined? undefined? 0= ; ( "name", -- f: word defined ? )
\ : ?\ 0= if postpone \ then ; ( f --, <string>| : cond comp. )
\ undefined? rdup ?\ : rdup r> r> dup >r >r >r ;

: 2+ #2 + ;
: 2- #2 - ;
: 1+! #1 swap +! ;

: /string ( b u1 u2 -- b u : advance string u2 )
  over min rot over + -rot - ;

system[
( 1 cells 8 * -> ) $10 constant #bits
( 1 #bits 1- lshift -> ) $8000 constant #msb
]system

:m 2variable :t mdrop mswap (var) t, t, munorder ;m
:m 2literal mswap lit lit ;m
:s (2const) r> 2* 2@ ;s compile-only ( R: a --, -- u )
:m 2constant :t mdrop (2const) t, t, ;m
:m mcreate :t mdrop (var) munorder ;m ( --, "name": var )

: spaces bl banner ; ( +n  -- : print space 'n' times )
: convert count >number drop ; ( +d1 addr1 -- +d2 addr2 )
: arshift ( n u -- n : arithmetic right shift )
  2dup rshift >r swap #msb and
  if $10 lit swap - #-1 swap lshift else drop #0 then r> or ;
: d2* over #msb and >r 2* swap 2* swap r> if #1 or then ;
: d2/ dup   #1 and >r 2/ swap 2/ r> if #msb or then swap ;
: d- dnegate d+ ; ( d d -- d : double cell subtraction )
: d= rot = -rot = and ; ( d d -- f : double cell equal )
: d0= or 0= ;     ( d -- f : double cell number equal to zero )
: d0<> d0= 0= ;   ( d -- f : double not equal to zero )
: 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
: dabs s>d if dnegate then ; ( d -- ud )
: 2over ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
  >r >r 2dup r> swap >r swap r> r> -rot ;
: 2, , , ; ( n n -- : write to values into dictionary )

\ As "does\>" does not work during meta-compilation we cannot
\ defined "2constant" as:
\
\        :to 2constant create 2, does> 2@ ; ( d --, Run: -- d )
\
\ Instead, we use the same trick we used to define "constant".

:to 2constant create cell negate allot compile (2const) 2, ;
:to 2variable create #0 , #0 , ; \ does> ; ( d --, Run: -- a )
:to 2literal swap postpone literal postpone literal ; immediate
:s +- 0< if negate then ;s ( n n -- n : copy sign )

\ Some more arithmetic words need to be defined for this 
\ floating point package, less common ones. It is amazing how
\ involved simple operations like division and multiplication
\ can be.
\

: m* ( n n -- d : single to double cell multiply [16x16->32] ) 
  2dup xor 0< >r abs swap abs um* r> if dnegate then ; 
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r         ( dl dh nn -- dl dh,   R: -- dh nn )
  dabs r@ abs um/mod ( dl dh    -- rem quo, R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : double prec. intermediate val )
  >r m* r> sm/rem ;

\ ## CORDIC CODE
\
\ NB. This CORDIC code could be extended to perform many more
\ functions, not just sine and cosine, examples of this are
\ in <https://github.com/howerj/q> and 
\ <https://en.wikibooks.org/wiki/Digital_Circuits/CORDIC>,
\ using "Universal CORDIC". It is possible to compute sine,
\ cosine, atan, atan(x/y), sqrt(x^2 + y^2), y/x, x*z,
\ hyperbolic sine and cosine and some other functions.
\
\ The code has a few weaknesses, it should be rewritten if
\ possible not to use "variable" which it unfortunately does.
\ Doing this would mean using "pick" and stack juggling a lot
\ and would make for a poor Forth word anyway, but eliminating 
\ global state is preferable to using "pick" in my view.
\
\ Using "variable" would create problems on a preemptive
\ multitasking Forth system (this system uses cooperative
\ threading and CORDIC does not call "pause" anywhere). A
\ simple fix would be to use "user" instead of "variable" to
\ make all of these variables thread-local, however there are
\ a lot of variables declared here and thread local storage is 
\ in short supply.
\
\

system[

mcreate lookup ( 16 values )
$3243 t, $1DAC t, $0FAD t, $07F5 t, 
$03FE t, $01FF t, $00FF t, $007F t,
$003F t, $001F t, $000F t, $0007 t, 
$0003 t, $0001 t, $0000 t, $0000 t,

$26DD constant cordic_1K 
$6487 constant hpi

variable tx variable ty variable tz
variable cx variable cy variable cz 
variable cd variable ck

]system

( CORDIC: valid in range -pi/2 to pi/2, arguments in fixed )
( point format with 1 = 16384, angle is given in radians.  )

: cordic ( angle -- sine cosine | x y -- atan sqrt )
  cz ! cordic_1K cx ! #0 cy ! #0 ck !
  $10 lit begin ?dup while
    cz @ 0< cd !
    cx @ cy @ ck @ arshift cd @ xor cd @ - - tx !
    cy @ cx @ ck @ arshift cd @ xor cd @ - + ty !
    cz @ ck @ cells lookup + @ cd @ xor cd @ - - tz !
    tx @ cx ! ty @ cy ! tz @ cz !
    ck 1+!
    1-
  repeat 
  cy @ cx @ ;

: sin cordic drop ; ( rad/16384 -- sin : fixed-point sine )
: cos cordic nip ;  ( rad/16384 -- cos : fixed-point cosine )

\ ## Core Floating Point Code

\ This floating point library has been adapted from one found 
\ in Vierte Dimensions Vol.2, No.4 1986, it should be free to 
\ use so long as the following copyright is left in the code:
\ 
\            FORTH-83 FLOATING POINT.
\       ----------------------------------
\       COPYRIGHT 1985 BY ROBERT F. ILLYES
\
\             PO BOX 2516, STA. A
\             CHAMPAIGN, IL 61820
\             PHONE: 217/826-2734 
\
\ only forth definitions system +order

: fabs $7FFF lit and ; ( f -- f )

system[
\ TODO: make a user variable, it's not currently working!
\ user (precision) \ 4 (precision) !
variable (precision)

4 t' (precision) >tbody t!

\ TODO: Replace this table.
mdecimal
mcreate ftable
         0.001 t, t,       0.010 t, t,
         0.100 t, t,       1.000 t, t,
        10.000 t, t,     100.000 t, t,
      1000.000 t, t,   10000.000 t, t,
    100000.000 t, t, 1000000.000 t, t,
mhex

]system

:s null ( f -- f : zero exponent if mantissa is )
  over 0= if drop #0 then ;s
:s norm >r 2dup or  ( normalize input float )
  if begin s>d invert
    while d2* r> 1- >r
    repeat swap 0< - ?dup
    if r> else #msb r> 1+ then
  else r> drop then ;s
:s lalign $20 lit min for aft d2/ then next ;s
:s ralign 1- ?dup if lalign then #1 #0 d+ d2/ ;s
:s tens 2* cells ftable + 2@ ;s ( a -- d )
:s shifts 
   fabs $4010 lit - s>d invert if -$2B lit throw then negate ;s
:s base? ( -- : check base )
  base @ $A lit <> -40 lit and throw ;s
:s unaligned? dup #1 and = -9 lit and throw ;s ( -- : chk ptr )
:s -+ drop swap 0< if negate then ;s

\ "fdepth" is standards compliant, but pretty useless because
\ there is no separate floating point stack.

: fcopysign #msb and nip >r fabs r> or ; ( r1 r2 -- r1 )
: floats 2* cells ;    ( u -- u )
: float+ 4 lit ( [ 1 floats ] literal ) + ; ( a -- a )
: set-precision ( +n -- : set FP decimals printed out )
dup #0 5 lit within if (precision) ! exit then -$2B lit throw ; 
: precision (precision) @ ; ( -- u : precision of FP values )
\ : precision 
\  (precision) @ #0 5 lit within if (precision) @ exit then
\  4 lit set-precision (precision) @ ; ( -- u : precision of FP values )
: f@ unaligned? 2@ ;   ( a -- r : fetch FP value )
: f! unaligned? 2! ;   ( r a -- : store FP value )
: f, 2, ; ( r -- : write float into dictionary )
: falign align ;       ( -- : align the dict. to store a FP )
: faligned aligned ;   ( a -- a : align point for FP )
: fdepth depth 2/ ;    ( -- n : number of floats, approximate )
: fdup #2 ?depth 2dup ; ( r -- r r : FP duplicate )
: fswap 4 lit ?depth 2swap ; ( r1 r2 -- r2 r1 : FP swap )
: fover 4 lit ?depth 2over ; ( r1 r2 -- r1 r2 r1 )
: f2dup fover fover ;  ( r1 r2 -- r1 r2 r1 r2 )
: ftuck fdup 2>r fswap 2r> ; ( r1 r2 -- r2 r1 r2 )
: frot 2>r fswap 2r> fswap ; ( r1 r2 r3 -- r2 r3 r1 )
: -frot frot frot ;    ( r1 r2 r3 -- r3 r1 r2 )
: fdrop #2 ?depth 2drop ; ( r -- : floating point drop )
: f2drop fdrop fdrop ; ( r1 r2 -- : FP 2drop )
: fnip fswap fdrop ;   ( r1 r2 -- r2 : FP nip )
: fnegate #msb xor null ;  ( r -- r : FP negate )
: fsign fabs over 0< if >r dnegate r> #msb or then ;
: f2* #2 ?depth 1+ null ; ( r -- r : FP times by two )
: f2/ #2 ?depth 1- null ; ( r -- r : FP divide by two )
: f*  ( r r -- r : FP multiply )
   4 lit ?depth rot + $4000 lit - >r um* r> norm ; 
: fsq fdup f* ;        ( r -- r : FP square )
: f0= fabs null d0= ; ( r -- r : FP equal to zero [incl -0.0] )
: um/ ( ud u -- u : ud/u and round )
  dup >r um/mod swap r> over 2* 1+ u< swap 0< or - ; 

: f/ ( r1 r2 -- r1/r2 : floating point division )
  4 lit ?depth
  fdup f0= -$2A lit and throw
  rot swap - $4000 lit + >r
  #0 -rot 2dup u<
  if  um/ r> null 
  else >r d2/ fabs r> um/ r> 1+
  then ;

: f+ ( r r -- r : floating point addition )
  4 lit ?depth
  rot 2dup >r >r fabs swap fabs - 
  dup if s>d
    if rot swap negate
      r> r> swap >r >r 
    then #0 swap ralign
  then swap #0 r> r@ xor 0< 
  if r@ 0< if 2swap then d-
    r> fsign rot swap norm 
  else d+ if 1+ 2/ #msb or r> 1+
    else r> then then ;

0 0 2constant fzero 

: f- fnegate f+ ; ( r1 r2 -- t : floating point subtract )
: f< f- 0< nip ; ( r1 r2 -- t : floating point less than )
: f> fswap f< ;  ( r1 r2 -- t : floating point greater than )
: f>= f< 0= ;    ( r1 r2 -- t : FP greater or equal )
: f<= f> 0= ;    ( r1 r2 -- t : FP less than or equal )
: f= d= ;        ( r1 r2 -- t : FP exact equality )
: f0> fzero f> ; ( r1 r2 -- t : FP greater than zero )
: f0< fzero f< ; ( r1 r2 -- t : FP less than zero )
: f0<= f0> 0= ;  ( r1 r2 -- t : FP less than or equal to zero )
: f0>= f0< 0= ;  ( r1 r2 -- t : FP more than or equal to zero )
: fmin f2dup f< if fdrop exit then fnip ; ( r1 r2 -- f : min )
: fmax f2dup f> if fdrop exit then fnip ; ( r1 r2 -- f : max )
: fwithin ( r1 r2 r3 -- f : r2 <= r1 < r3 )
  frot ftuck f>= >r f<= r> and ; 
: d>f $4020 lit fsign norm ;  ( d -- r : double to float )
: s>f s>d d>f ;           ( n -- r : single to float )

: f# 
  base?
  >r precision tens drop um* r> shifts
  ralign precision ?dup if for aft # then next
  [char] . hold then #s rot sign ;
: f.r >r tuck <# f# #> r> over - spaces type ; ( f +n -- )
: f. space #0 f.r ; ( f -- : output floating point )

( N.B. 'f' and 'e' require "dpl" to be set correctly! )

: f ( n|d -- f : formatted double to float )
   base?
   dpl @ 0< if ( input was single number )
     #1 ?depth s>d #0 dpl ! 
   else ( else a double )
     #2 ?depth
   then 
   d>f dpl @ tens d>f f/ ;    

:to fconstant ( "name", r --, Run Time: -- r ) 
  f postpone 2constant ; 
:to fliteral ( r --, Run: -- r : compile a literal in a word ) 
  f postpone 2literal ; immediate 
: fix tuck #0 swap shifts ralign -+ ; ( r -- n : f>s rounding )
: f>s tuck #0 swap shifts lalign -+ ; ( r -- n : f>s truncate )
: floor  f>s s>f ; ( r -- r )
: fround fix s>f ; ( r -- r )
: fmod f2dup f/ floor f* f- ; ( r1 r2 -- r )

\ 1.0 fconstant fone decimal ( = $8000 $4001 )
$8000 $4001 2constant fone

: f1+ fone f+ ; ( r -- r : increment FP number )
: f1- fone f- ; ( r -- r : decrement FP number )
: finv fone fswap f/ ; ( r -- r : FP 1/x )

: exp ( r -- r : raise 2.0 to the power of 'r' )
  2dup f>s dup >r s>f f-     
  f2* $E1E5 $C010 2literal ( [ -57828.0 ] fliteral )
  2over fsq $FA26 $400B 2literal ( [ 2001.18 ] fliteral ) f+ f/
  2over f2/ f- $8AAC $4006 2literal ( [ 34.6680 ] fliteral ) 
  f+ f/ f1+ fsq r> + ;
: fexp  ( r -- r : raise e to the power of 'r' )
  \ 1.4427 = log2(e)
  $B8AA $4001 2literal ( [ 1.4427 ] fliteral ) f* exp ; 
: falog ( r -- r ) 
   $D49A $4002 2literal ( [ 3.3219 ] fliteral ) f* exp ; 
: get ( "123" -- : get a single signed number )
  bl word dup 1+ c@ [char] - = tuck -
  #0 #0 rot convert drop ( should throw if not number... ) -+ ;

: fexpm1 fexp fone f- ; ( r1 -- r2 : e raised to 'r1' less 1 )
: fsinh fexpm1 fdup fdup f1+ f/ f+ f2/ ; ( r -- fsinh : h-sin )
: fcosh fexp fdup fone fswap f/ f+ f2/ ; ( r -- fcosh : h-cos )
: fsincosh fdup fsinh fswap fcosh ; ( f -- sinh cosh )
: ftanh fsincosh f/ ; ( f -- ftanh : hyperbolic tangent )

mdecimal
: e.r ( r +n -- : output scientific notation )
  >r
  tuck fabs 16384 lit tuck -
  4004 lit 13301 lit */mod >r
  s>f 4004 lit s>f f/ exp f*
  2dup fone f<
  if 10 lit s>f f* r> 1- >r then
  <# r@ abs #0 #s r> sign 2drop
  [char] e hold f# #> r> over - spaces type ;
: e ( f "123" -- usage "1.23 e 10", input scientific notation )
  f get >r r@ abs 13301 lit 4004 lit */mod
  >r s>f 4004 lit s>f f/ exp r> +
  r> 0< if f/ else f* then ;
mhex
: e. space #0 e.r ;

( Define some useful constants )
$C911 $4002 2constant fpi \ Pi = 3.14159265 fconstant fpi )
$C911 $4001 2constant fhpi \ 1/2pi = 1.57079632 fconstant fhpi
$C911 $4003 2constant f2pi \ 2pi = 6.28318530 fconstant f2pi
$ADF8 $4002 2constant fe \ e = 2.71828182 fconstant fe
$B172 $4000 2constant fln2 \ ln[2] = 0.69314718 fconstant fln2
$935D $4002 2constant fln10 \ ln[10] 2.30258509 fconstant fln10

: fdeg ( rad -- deg )
  f2pi f/ $B400 $4009 2literal ( [ 360.0 ] fliteral ) f* ; 
: frad ( deg -- rad )
  $B400 $4009 2literal ( [ 360.0 ] fliteral ) f/ f2pi f* ; 

:s >cordic ( f -- n )
   $8000 $400F 2literal ( [ 16384.0 ] fliteral ) f* f>s ;s 
:s cordic> ( n -- f )
   s>f $8000 $400F 2literal ( [ 16384.0 ] fliteral ) f/ ;s     

:s quadrant 
  fdup fhpi f< if fdrop #0 exit then 
  fdup  fpi f< if fdrop #1 exit then 
      $96CD $4003 2literal ( [ fpi fhpi f+ ] 2 literal ) f< 
      if #2 exit then 
  3 lit ;s
:s >sin #2 4 lit within if fnegate then ;s
:s >cos #1 3 lit within if fnegate then ;s
:s scfix >r 
  r@ #1 = if fnegate fpi f+ rdrop exit then
  r> 3 lit = if fnegate f2pi f+ then ;s

:s (fsincos) fhpi fmod >cordic cordic >r cordic> r> cordic> ;s

: fsincos ( rads -- sin cos )
   fdup f0< >r
   fabs
   f2pi fmod fdup quadrant dup >r scfix (fsincos) 
   r@ >cos fswap r> >sin fswap
   r> if fswap fnegate fswap then ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )
: ftan fsincos f/ ; ( rads -- tan )

\ only forth definitions system +order decimal

\ A poor standard Forth word, with three comparison operators
\ rolled into one.
\
\  r3 > 0 -> |r1 - r2| < r3
\  r3 = 0 -> r1 = r2 [should also deals with negative zero...]
\  r3 < 0 -> |r1 - r2| < |r3| * (|r1| + |r2|)

: f~ ( r1 r2 r3 -- flag )
  fdup f0> if 2>r f- fabs 2r> f< exit then
  fdup f0= if fdrop f= exit then
  fabs 2>r f2dup fabs fswap fabs f+ 2r> f* 2>r f- fabs 2r> f< ;

: fsqrt ( r -- r : square root of 'r' )
  fdup f0< if fdrop -$2E lit throw then
  fdup f0= if fdrop fzero exit then
  fone 
  $10 lit for aft 
    f2dup fsq fswap f- fover f2* f/ f-
  then next
  fnip ;

: filog2 ( r -- u : Floating point integer logarithm )
  null
  fdup fzero f<= -$2E lit and throw
  ( norm ) nip $4001 lit - ;

: fhypot f2dup f> if fswap then ( a b -- c : hypotenuse )
  fabs 2>r fdup 2r> fswap f/ fsq f1+ fsqrt f* ;

: sins
  f2pi fnegate
  begin
    fdup f2pi f<
  while
    fdup fdup f. [char] , emit space fsincos 
    fswap f. [char] , emit space f. cr
    $80AF $3FFE 2literal ( [ f2pi 50.0 f f/ ] 2literal ) 
    f+
  repeat fdrop ;

\ We use the Arithmetic Geometric Mean, see
\ <en.wikipedia.org/wiki/Arithmetic%E2%80%93geometric_mean>,
\ to calculate a natural logarithm.
\
: agm f2dup f* fsqrt 2>r f+ f2/ 2r> fswap ; ( r1 r2 -- r1 r2 )

\ ln(x) = (pi / (2*M(1, (2^(2-m))/ x))) - (m*ln(2))
\
\ m = number of steps in M
\ M = Arithmetic-Geometric Mean, defined as:
\
\     a0 = x
\     g0 = y
\     a_(n+1) = 1/2 * (a_n + g_n)
\     g_(n+1) = sqrt(a_n * g_n)
\
\ As m -> INF, a_n = g_n.
\
\ 12 = Number of steps
\
: fln ( r -- r : natural logarithm )
  $8000 $3FF7 2literal ( [ 2 12 - s>f exp ] 2literal ) fswap f/ 
  fone fswap 
  12 lit for aft agm then next f+ fpi 
  fswap f/ $8516 $4004 2literal ( [ 12 s>f fln2 f* ] 2literal ) 
  f- ;
: flnp1 fone f+ fln ; ( r -- r )

\ Once we have a function for calculating the natural logarithm
\ we can use this to calculate logarithms in other bases, by
\ either dividing by the natural logarithm of the target base
\ (which can be precomputed) or by multiplying by the inverse
\ of that.
\
\ An Alternate "flog2":
\
\        : flog2
\          [ 2 12 - s>f exp ] 2literal fswap f/ 
\          fone fswap 
\          12 for aft agm then next f+ fln2 f* fpi 
\          fswap f/ [ 12 s>f ] 2literal f- ;
\

: flog2 fln fln2 f/ ; ( r -- r : base  2 logarithm )
: flog fln fln10 f/ ; ( r -- r : base 10 logarithm )
: f** fswap flog2 f* exp ; ( r1 r2 -- r : pow[r1, r2] )

: fatanh ( r1 -- r2 : atanh, -1 < r1 < 1 )
  fdup f1+ fswap fone fswap f- f/ fln f2/ ;
: facosh ( r1 -- r2 : acosh, 1 <= r1 < INF )
  fdup fsq f1- fsqrt f+ fln ; 
: fasinh fdup fsq f1+ fsqrt f+ fln ; ( r -- r )

\ N.B This is a better version of atan where x > 1, but it
\ is larger and uses global variables:
\
\        2variable fatan.cnt
\        2variable fatan.sqr
\        2variable fatan.x 
\        variable fatan.dir
\         
\        : fatan-hi ( r -- r )
\          fdup fsq fatan.sqr 2! fatan.x 2! fone fatan.cnt 2! 
\          1 fatan.dir ! 
\          fhpi
\          10 for aft 
\            fatan.x 2@ fatan.cnt 2@ f* finv 
\            fatan.dir @ if f- 0 fatan.dir !
\            else f+ 1 fatan.dir ! then
\            fatan.x 2@ fatan.sqr 2@ f* fatan.x 2!
\            fatan.cnt 2@ [ 2.0 f ] 2literal f+ fatan.cnt 2!
\          then next ;
\
\ This version of "atan" is much faster, It approximates atan 
\ in range (0, 1], with a fair bit of error. We can then use
\ that to make an atan which deals with values greater than
\ one.
\
\ See <https://stackoverflow.com/questions/42537957/>
\

:s fatan-lo fdup fsq fdup 
\ [  0.07765095 ( = A ) ] fliteral f* = $9F08 $3FFD 
   $9F08 $3FFD 2literal f* 
\ [ -0.28743447 ( = B ) ] fliteral f+ f* = $932B $BFFF 
   $932B $BFFF 2literal f+ f* 
\ [  0.99518168 ( pi/4 - A - B ) ] fliteral f+ f* ;= $FEC5 $4000 
   $FEC5 $4000 2literal f+ f* ;s

\ atan(x) = pi/2 - atan(1/x)
:s fatan-hi finv fatan-lo fhpi fswap f- ;s

: fatan fdup fabs fone f> if fatan-hi exit then fatan-lo ;

\ atan(y/x)
\
\ x  > 0         =>  arctan(y/x)
\ y >= 0, x < 0  =>  arctan(y/x) + pi
\ y  < 0, x < 0  =>  arctan(y/x) - pi
\ y  > 0, x = 0  =>  pi/2
\ y  < 0, x = 0  => -pi/2
\ y  = 0, x = 0  =>  undefined
: fatan2 ( r1=y r2=x -- r3 )
  fdup f0> if f/ fatan exit then
  fdup f0< if 
     fover f0< 
     if   f/ fatan fpi f+
     else f/ fatan fpi f- then
     exit
  then
  fdrop
  fdup f0> if fdrop fhpi exit then
  fdup f0< if 
    fdrop $C911 $C001 2literal ( [ fhpi fnegate ] 2literal ) 
    exit then
  -$2E lit throw ;

: fasin fdup fsq fone fswap f- fsqrt f/ fatan ; ( r -- r )
: facos fasin fhpi fswap f- ; ( r -- r )

[then] ( opt.float )


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
\ The word name comes from "cold start", as opposed to a "warm
\ start", from the automotive terms of starting an engine
\ in a cold state relative to its normal operating temperature,
\ the term has caught on in computer science circles to mean
\ to start a system or program from the beginning.
\
\ We could define a "warm" word with the following
\ definition:
\
\        : warm 0 >r ;
\
\ This definition performs a "warm start" of sorts, and does
\ so in a non-portable manner. The VM handles the return stack
\ jumps and this just happens to work on this VM. It should
\ not be relied upon, it causes the VM to jump to the first
\ instruction, which in turn jumps into the reset vector for
\ the system. (All address lower than the value stored in
\ "primitive" are treated as jump locations by the VM).
\

: cold {cold} lit 2* @execute ; ( -- )

\ # Image Generation
\
\ Everything is done! No more Forth to write! We just need to
\ set up hooks, make sure the dictionary is in the right
\ state, output the image, and exit the interpreter. That
\ can be done fewer than ten lines.
\
\ The word "save-target" is called, this spits the output onto
\ the standard output stream as a series of space delimited
\ 16-bit signed numbers.
\
\ ".end" puts the Forth interpreter back into a normal state,
\ so we can then call "bye", that "bye" will be the one which
\ exits the interpreter and not the new one we have just
\ defined in the target. That completes everything.
\
\ As the SUBLEQ machine has no way of writing files to anything
\ and only one method of input or output, the build process
\ for a new image looks like this (on a Unix system):
\
\        ./subleq old-image.dec < subleq.fth > new-image.dec
\
\ "old-image.dec" contains an eForth interpreter, "subleq.fth"
\ contains this file, and "new-image.dec" is the new eForth
\ image we have produced in this file.
\

t' (cold) half {cold} t!      \ Set starting Forth word
atlast {forth-wordlist} t!    \ Make wordlist work
{forth-wordlist} {current} t! \ Set "current" dictionary
there h t!                    \ Assign dictionary pointer
local? {user}  t!             \ Assign number of locals
primitive t@ double mkck check t! \ Set checksum over Forth
atlast {last} t!              \ Set last defined word
save-target                   \ Output target

\ With a slightly modified version of "save-target" and
\ the Forth word ".\\" (which echos only the current line) it
\ is possible to output a C program that contains the entire
\ interpreter, this would look something like this:
\
\         .\ #include <stdio.h> /* eForth for 16-bit SUBLEQ */
\         .\ int main(void){short p=0,m[1<<16] = {
\        save-target  \ Output target
\         .\ }; while(p>=0){int
\         .\ a=m[p++],b=m[p++],c=m[p++];
\         .\ a<0?m[b]=getchar():b<0?putchar(m[a]):(m[b]-=m[a])
\         .\ <=0?p=c:0;}}
\
\ The process could also be done as part of the build system
\ as well.
\
\

.end                          \ Get back to normal Forth
bye                           \ Auf Wiedersehen

As we have called "bye", we can write what we want here without
it being run.

\ ## Further Programs
\
\ Even given such a limited environment it is possible to
\ create complex programs, as demonstrated. It might seem
\ necessary to include more memory, or more peripherals,
\ however only the surface has been scratched on what is
\ possible with what is available, even if the limit lies
\ somewhere on the same level as the micro-computing systems
\ of the 1980s. Still, that level of system would allow us to
\ make in memory database systems and file systems, perhaps
\ based on Forth blocks, to implement a Floating Point Word
\ Set, or ALLOCATE and FREE for dynamic memory allocation.
\
\ It might not seem possible to extend this Forth
\ implementation so it supports floating point numbers, or
\ dynamic memory allocation, but those features can be written
\ in pure Forth, special hardware is not required to implement
\ those features, only code.
\
\ For example, <https://wilsonminesco.com/Forth/ALLOC.html>
\ implements ALLOCATE and FREE in Forth. There are other
\ pure Forth implementations of these words about.
\
\ An article in "Vierte Dimension Vol.2, No.4 1986" titles
\ "A FAST HIGH-LEVEL FLOATING POINT" by "Robert F. Illyes"
\ implements a floating point system optimized for the 16-bit
\ Forth implementations that were common at the time.
\
\ Simple games could be made that only require a terminal,
\ such as Sokoban, 2048, Conway's Game Of Life, or Minesweeper.
\ More dynamic games like Space Invaders or Tetris would
\ require non-blocking input, which is possible to add to
\ the virtual machine without breaking any Forth code.
\
\ It would be nice to see this system in use elsewhere, perhaps
\ integrated into a game, or another system, impractical as it
\ is. Please contact the author if you find a use for this
\ system, other than for pedagogical purposes or as an
\ interesting puzzle to solve.
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
\ - URISC, the original OISC, a SUBLEQ machine:
\  Mavaddat, F.; Parhami, B. (October 1988). "URISC: The
\  Ultimate Reduced Instruction Set Computer".
\ - <https://en.wikipedia.org/wiki/Turing_tarpit>, which
\ SUBLEQ could be argued to be one.
\ - For other Single Instruction Set Computers:
\ <https://en.wikipedia.org/wiki/One-instruction_set_computer>
\ - For the Forth-83 Standard:
\ <http://forth.sourceforge.net/standard/fst83/>
\ <http://forth.sourceforge.net/standard/fst83/FORTH-83.PRN>
\ - DPANS84 FORTH standard:
\ <http://forth.sourceforge.net/std/dpans/>
\
\
\ # Appendix
\
\ ## About the author
\
\ Hello! Instead of writing about myself awkwardly in the
\ third person with words such as "The author has..." I
\ have decided to go down a more informal route. I, Richard
\ James Howe, have been an embedded software engineer in the
\ automotive sector writing safety critical code in C and
\ tooling in Python/Perl/C#, I currently work in the smart
\ energy sector. I have a degree in electronic engineering and
\ have had internships related to that. I also studied in
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
\ commercial or professional value whatsoever but the entire
\ SUBLEQ eForth system was a lovely puzzle to crack. My next
\ big project will probably be my own Unix operating system in
\ a Pascal like language of my own design, or starting a
\ business.
\
\ ## Fully Portable SUBLEQ machine written in C
\
\ This section contains a fully portable version of the SUBLEQ
\ machine, written in C, it is not minified, or obfuscated, but
\ designed to as portable as possible. It does not even rely
\ on signed arithmetic, doing all arithmetic with unsigned
\ numbers instead. It should work on all platforms that have
\ the fixed width typedefs present in "\<stdint.h\>".
\
\        #include <stdint.h>
\        #include <stdio.h>
\        #define SZ   (1<<16)
\        #define L(X) ((X)%SZ)
\        int main(int s, char **v) {
\                static uint16_t m[SZ];
\                uint16_t pc = 0;
\                for (int i = 1, d = 0; i < s; i++) {
\                        FILE *f = fopen(v[i], "r");
\                        if (!f)
\                                return 1;
\                        while (fscanf(f, "%d,", &d) > 0)
\                                m[L(pc++)] = d;
\                        if (fclose(f) < 0)
\                                return 2;
\                }
\                for (pc = 0; !(pc & 0x8000u);) {
\                        uint16_t a = m[L(pc++)];
\                        uint16_t b = m[L(pc++)];
\                        uint16_t c = m[L(pc++)];
\                        if (a == 65535) {
\                                m[L(b)] = getchar();
\                        } else if (b == 65535) {
\                                if (putchar(m[L(a)]) < 0)
\                                        return 3;
\                                if (fflush(stdout) < 0)
\                                        return 4;
\                        } else {
\                                uint16_t r = m[L(b)]-m[L(a)];
\                                if (r & 32768 || r == 0)
\                                        pc = c;
\                                m[L(b)] = r;
\                        }
\                }
\                return 0;
\        }
\
\ Note that it is fairly easy to change this virtual machine
\ so that is can either compute different OISC instructions
\ or so it can perform SUBLEQ with different representations
\ for negative numbers. An example of this would be to change
\ the conditional test:
\
\        if (r & 32768 || r == 0)
\                pc = c;
\
\ To:
\
\        if (r & 32768)
\                pc = c;
\
\ This would give us a 16-bit SUBNEG (or Subtract and branch
\ if Negative) machine, a similar machine to SUBLEQ but one
\ that would require subtly different algorithms in the base
\ image.
\
\ Making a ones compliment SUBLEQ machine would not be much
\ harder.
\
\ The machine can address (almost) 65536 16-bit values, or
\ 128KiB. The Forth machine however wastes the bottom bit so
\ character addressing can be used. There is no reason to
\ limit the SUBLEQ VM for this implementation specifically and
\ there are a few advantages to using 128KiB instead of 64KiB,
\ one is that the compiler can eliminate the boundary checks
\ done with the "L" function when using 128KiB of memory,
\ another is that a few SUBLEQ programs other than this of the
\ authors do use this memory.
\
\ Another limitation is that SUBLEQ code can only be executed
\ in the lower half of the address space, as the machine will
\ exit for the high bit of the program counter is set. This is
\ not much of a limitation, but it should be mentioned.
\
\ ### SUBLEQ VM File Format
\
\ The arguments passed to the program are meant to be file
\ names containing space delimited decimal values, one value
\ for each cell. Multiple programs can be concatenated into
\ one image which is then run, or programs and data. This
\ behavior has some utility, but the implementation is this
\ way as it is easy to extend the program to do this than any
\ other reason.
\
\ We could complicate the program by adding in command line
\ parsing so options and file input and output could be
\ selected. Potentially useful options include:
\
\ * A file to save the image to.
\ * Whether to enter a debug mode and command line.
\ * How many cycles to run for.
\ * A help message and program version number.
\ * The bit-width of the SUBLEQ machine (actually covered in
\ a program displayed further on).
\ * How much memory to allocate to the SUBLEQ machine, which
\ is currently fixed.
\ * The format of the input file.
\ * Whether to enable variations on the SUBLEQ machine (such
\ as SUBNEG).
\
\ All of these options *could* be implemented but would more
\ than double the size of the simple C program that currently
\ implements the SUBLEQ VM, in fact it would be many multiples
\ of its current size. All for something that is *potentially*
\ useful. It is better to keep things simple.
\
\ As mentioned, the file format consists of space delimited
\ decimal values, each value being placed sequentially in each
\ cell, with no checking for overflow.
\
\ The following program, shown before, prints "Hi" and then
\ exits:
\
\        9 -1 3 10 -1 6 0 0 -1 72 105 0
\
\ The spaces can be replaced with new lines and it will
\ still work:
\
\        9
\        -1
\        3
\        10
\        -1
\        6
\        0
\        0
\        -1
\        72
\        105
\        0
\
\ This has one advantage that if the program is modified and
\ stored in version control many of the lines are likely to
\ be the same, most version control systems calculate
\ differences line by line, and not within lines, so the
\ "diffs" will be smaller.
\
\ Most SUBLEQ systems load their values with a "fscanf"
\ format string, or language equivalent, of the following
\ statement:
\
\       fscanf(f, "%d", &d)
\
\ With the addition of a single character we can parse both
\ the white-space delimited format, and formats that contain
\ numbers separated white space and commas:
\
\       fscanf(f, "%d,", &d)
\
\ This helps when generating SUBLEQ programs that are to be
\ embedded in other languages as an array.
\
\ The file format is quite inefficient but the main reason
\ that this format is chosen by many implementations is that
\ most SUBLEQ programs are quite short, featuring just tens
\ of instructions, and are shared on websites as part of a
\ page viewed by a human. For this purpose the format is
\ adequate.
\
\ Using hexadecimal would have on space, as would using a
\ binary format, but it would make sharing more difficult and
\ tie the program to this implementation.
\
\ ## SUBLEQ machine with automatic save feature
\
\ This version machine is has a very different flavor compared
\ to the previous one, this one automatically saves the memory
\ of the device upon exit. This means interactive Forth
\ sessions can be saved and new images prepared. Care has to
\ be taken not to corrupt the image, because the system will
\ always save on exit!
\
\ Care is taken to try not to save memory that does not need
\ to be saved because it has not been initialized with the
\ images given on the command line or that has not been
\ written to, which is what the variable "max" is for. There
\ are some changes in the Forth system that could be made to
\ improve this which have not been undertaken (such as making
\ system memory be contagious with the Forth image).
\
\        #include <stdint.h>
\        #include <stdio.h>
\        #define SZ (1<<16)
\        #define L(X) ((X)%SZ)
\        #define MAX(X, Y) ((X) < (Y) ? (Y) : (X))
\        int main(int s, char **v) {
\                static uint16_t m[SZ];
\                uint16_t pc = 0, max = 0;
\                for (int i = 1, d = 0; i < s; i++) {
\                        FILE *f = fopen(v[i], "r");
\                        if (!f)
\                                return 1;
\                        while (fscanf(f, "%d,", &d) > 0)
\                                m[(pc++)%SZ] = d;
\                        if (fclose(f) < 0)
\                                return 2;
\                }
\                max = pc;
\                for (pc = 0; !(pc & 0x8000);) {
\                        uint16_t a = m[pc++];
\                        uint16_t b = m[pc++];
\                        uint16_t c = m[pc++];
\                        if (a == 65535) {
\                                m[b] = getchar();
\                        } else if (b == 65535) {
\                                if (putchar(m[a]) < 0)
\                                        return 3;
\                                if (fflush(stdout) < 0)
\                                        return 4;
\                        } else {
\                                uint16_t r = m[b] - m[a];
\                                max = MAX(max, b);
\                                if (r & 32768 || r == 0)
\                                        pc = c;
\                                m[b] = r;
\                        }
\                }
\                if (s <= 1)
\                        return 0;
\                FILE *f = fopen(v[s - 1], "w");
\                if (!f)
\                        return 5;
\                for (int i = 0; i <= max; i++) {
\                        const int p = (short)m[i];
\                        if (fprintf(f, "%d\n", p) < 0) {
\                                (void)fclose(f);
\                                return 6;
\                        }
\                }
\                if (fclose(f) < 0)
\                        return 7;
\                return 0;
\        }
\
\ The last argument given is the one that is written to (it
\ will also be read from). It would be wise to backup any
\ images before use.
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
\ It is the same as the portable version but provides
\ a function for enabling and performing a non-blocking read
\ on the standard input channel, or on Windows a read from
\ the terminal input. The character retrieval function now
\ returns negative if there is no input, instead of on End
\ Of File, which can no longer be detect.
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
\ detected any more, as mentioned.
\
\        #include <stdint.h>
\        #include <stdio.h>
\        #include <stdlib.h>
\
\        #define ESCAPE (27)
\        #define DELETE (127)
\        #define BACKSPACE (8)
\
\        #ifdef __unix__
\        #include <unistd.h>
\        #include <termios.h>
\        static struct termios oldattr, newattr;
\
\        static void restore(void) {
\                tcsetattr(STDIN_FILENO, TCSANOW, &oldattr);
\        }
\
\        static int setup(void) {
\                tcgetattr(STDIN_FILENO, &oldattr);
\                newattr = oldattr;
\                newattr.c_iflag &= ~(ICRNL);
\                newattr.c_lflag &= ~(ICANON | ECHO);
\                newattr.c_cc[VMIN]  = 0;
\                newattr.c_cc[VTIME] = 0;
\                tcsetattr(STDIN_FILENO, TCSANOW, &newattr);
\                atexit(restore);
\                return 0;
\        }
\
\        static int getch(void) {
\                static int init = 0;
\                if (!init) {
\                        setup();
\                        init = 1;
\                }
\                unsigned char r = 0;
\                if (read(STDIN_FILENO, &r, 1) != 1)
\                        return -1;
\                return r;
\        }
\
\        static int putch(int c) {
\                int res = putchar(c);
\                fflush(stdout);
\                return res;
\        }
\
\        static void sleep_ms(unsigned ms) {
\                usleep((unsigned long)ms * 1000);
\        }
\        #else
\        #ifdef _WIN32
\
\        extern int getch(void);
\        extern int putch(int c);
\        static void sleep_ms(unsigned ms) {
\                usleep((unsigned long)ms * 1000);
\        }
\        #else
\        static int getch(void) {
\                return getchar();
\        }
\
\        static int putch(const int c) {
\                return putchar(c);
\        }
\
\        static void sleep_ms(unsigned ms) {
\                (void)ms;
\        }
\        #endif
\        #endif /** __unix__ **/
\
\        static int wrap_getch(void) {
\                const int ch = getch();
\                if (ch == EOF) {
\                        sleep_ms(1);
\                }
\                if (ch == ESCAPE)
\                        exit(0);
\                return ch == DELETE ? BACKSPACE : ch;
\        }
\
\        #define SZ   (1<<16)
\        #define L(X) ((X)%SZ)
\        int main(int s, char **v) {
\                static uint16_t m[SZ];
\                uint16_t pc = 0;
\                for (int i = 1, d = 0; i < s; i++) {
\                        FILE *f = fopen(v[i], "r");
\                        if (!f)
\                                return 1;
\                        while (fscanf(f, "%d,", &d) > 0)
\                                m[L(pc++)] = d;
\                        if (fclose(f) < 0)
\                                return 2;
\                }
\                for (pc = 0; !(pc & 32768);) {
\                        uint16_t a = m[L(pc++)];
\                        uint16_t b = m[L(pc++)];
\                        uint16_t c = m[L(pc++)];
\                        if (a == 65535) {
\                                m[L(b)] = wrap_getch();
\                        } else if (b == 65535) {
\                                if (putch(m[L(a)]) < 0)
\                                        return 3;
\                        } else {
\                                uint16_t r = m[L(b)]-m[L(a)];
\                                if (r & 32768 || r == 0)
\                                        pc = c;
\                                m[L(b)] = r;
\                        }
\                }
\                return 0;
\        }
\
\ Note that the machine could be changed so that output could
\ be non-blocking as well, "putch" would have to be changed
\ so it attempted to output the character, and would instead
\ feedback the information to the program running on the VM
\ in this manner:
\
\        m[L(a)] = putch(m[L(a)]);
\
\ It would be up to the running program to handle this
\ correctly, and retry any failed operations if necessary.
\
\ ## N-Bit SUBLEQ machine
\
\ This C program implements a SUBLEQ machine with a variable
\ width for the SUBLEQ cell, it can be used to simulated fixed
\ width SUBLEQ machines from 8 to 64 bits inclusive, but only
\ twos compliment machines, it could be extended if needs be
\ to deal with different number representations (such as ones
\ compliment or sign magnitude representation).
\
\        #include <stdint.h>
\        #include <stdio.h>
\        #include <stdlib.h>
\        #include <inttypes.h>
\        #define SZ     (1<<16)
\        #define L(X)   ((X)%SZ)
\        #define HI(X)  (1ull << ((X) - 1))
\
\        static inline uint64_t msk(int n) {
\          return n < 64 ?
\            (1ull << n) + 0xFFFFFFFFFFFFFFFFull :
\            0xFFFFFFFFFFFFFFFFull;
\        }
\
\        int main(int s, char **v) {
\          if (s < 2)
\            return 1;
\          static uint64_t m[SZ];
\          uint64_t pc = 0, N = atoi(v[1]);
\          if (N < 8 || N > 64)
\            return 2;
\          for (long i = 2, d = 0; i < s; i++) {
\            FILE *f = fopen(v[i], "r");
\            if (!f)
\              return 3;
\            while (fscanf(f, "%ld,", &d) > 0)
\              m[L(pc++)] = ((int64_t)d) & msk(N);
\            if (fclose(f) < 0)
\              return 4;
\          }
\          for (pc = 0; pc < SZ;) {
\            uint64_t a = m[L(pc++)],
\               b = m[L(pc++)],
\               c = m[L(pc++)];
\            if (a == msk(N)) {
\              m[L(b)] = getchar() & msk(N);
\            } else if (b == msk(N)) {
\              if (putchar(m[L(a)]) < 0)
\                return 5;
\              if (fflush(stdout) < 0)
\                return 6;
\            } else {
\              uint64_t r = m[L(b)] - m[L(a)];
\              r &= msk(N);
\              if (r & HI(N) || r == 0)
\                pc = c;
\              m[L(b)] = r;
\            }
\          }
\          return 0;
\        }
\
\ This program is useful for testing the error messages printed
\ out when the eForth image detects the wrong SUBLEQ cell width
\ is in use.
\
\ ## Recompiling Virtual Machine; The "Recompiler"
\
\ This is a 16-bit SUBLEQ VM that attempts to optimize the
\ code that it is to execute so it runs much faster, it finds
\ common sequences of instructions and turns them into a single
\ instruction. The system is brittle and liable to break code
\ as SUBLEQ code tends to be highly self-modifying. Initially
\ the idea was to make a Just In Time compiler to recompile
\ common expressions on the fly, however doing this ahead of
\ time is simpler. This could be turned into an N-bit version,
\ but execution speed and not being generic is the goal.
\
\ The core of the system is a pattern matching mini-language
\ that can match on sequences of SUBLEQ instructions and
\ extract values for further comparison. This is embodied in
\ the function "match", which is used by "optimizer".
\
\ It must be emphasized again, that this program is *brittle*,
\ it works perfectly on the "eforth.dec" image that is
\ generated but it will fail on arbitrary SUBLEQ programs.
\
\ It is meant to match on these instruction macros:
\
\ ======================= TODO ================================
\
\        :m Z 0 t, ;m ( -- : Address 0 must contain 0 )
\        :m NADDR there 2/ 1+ t, ;m
\        :m HALT 0 t, 0 t, -1 t, ;m
\        :m JMP 2/ Z Z t, ;m ( a --, Jump to location )
\        :m ADD swap 2/ t, Z NADDR Z 2/ t, NADDR Z Z NADDR ;m
\        :m SUB swap 2/ t, 2/ t, NADDR ;m ( a a -- : subtract )
\        :m NOOP Z Z NADDR ;m ( -- : No operation )
\        :m ZERO dup 2/ t, 2/ t, NADDR ;m
\        :m PUT 2/ t, -1 t, NADDR ;m ( a -- : put a byte )
\        :m GET 2/ -1 t, t, NADDR ;m ( a -- : get a byte )
\        :m MOV 2/ >r r@ dup t, t, NADDR 2/ t, Z  NADDR
\           r> Z  t, NADDR Z Z NADDR ;m
\        :m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m
\        :m iJMP there 2/ E + 2* MOV Z Z NADDR ;m
\        :m iSTORE ( a a -- )
\           swap >r there 2/ 24 + 2dup 2* MOV 2dup 1+ 2* MOV
\           7 + 2* MOV r> 0 MOV ;m
\
\ If the above section differs in your version of the assembler
\ it is quite likely the reason it is failing. This may have
\ happened because those macros have been optimized and this
\ program not updated.
\
\ On some systems this speeds execution up, on others it seems
\ to slow it down. There is a lot in this program that could
\ itself be optimized, it was written to demonstrate the
\ concept and not for efficiency and speeds sake in of itself.
\
\
\       #include <stdint.h>
\       #include <stdio.h>
\       #include <stdarg.h>
\       #include <ctype.h>
\       #include <inttypes.h>
\       #include <time.h>
\       #define SZ   (1<<16)
\       #define L(X) ((X)%SZ)
\       #define DEPTH (3*64)
\       enum {
\         SUBLEQ, JMP, ADD, SUB, MOV,
\         ZERO, PUT, GET, HALT,
\         IJMP, ILOAD, ISTORE, INC, DEC,
\         INV, DOUBLE, LSHIFT,
\
\         MAX
\       };
\
\       static const char *names[] = {
\         "SUBLEQ ", "JMP    ", "ADD    ", "SUB    ",
\         "MOV    ", "ZERO   ", "PUT    ", "GET    ",
\         "HALT   ", "IJMP   ", "ILOAD  ", "ISTORE ",
\         "INC    ", "DEC    ", "INV    ", "DOUBLE ",
\         "LSHIFT ",
\       };
\
\       typedef struct {
\         int instruction;
\         uint16_t m, s, d;
\       } instruction_t;
\
\       typedef struct {
\         int matches[MAX];
\         int set[9];
\         uint16_t v[9];
\         unsigned char z_reg[SZ], one_reg[SZ], neg1_reg[SZ];
\         clock_t start, end;
\         int64_t cnt[MAX];
\       } optimizer_t;
\
\       static int match(optimizer_t *o, uint16_t *n,
\         int sz, uint16_t pc, const char *s, ...) {
\         va_list ap;
\         int r = 0, i = 0, j = 0;
\         for (int i = 0; i < 9; i++) {
\           o->set[i] = 0;
\           o->v[i] = 0;
\         }
\         va_start(ap, s);
\         for (i = 0, j = 0; s[j] && i < sz; j++) {
\           switch (s[j]) {
\           case '0': case '1': case '2': case '3':
\           case '4': case '5': case '6': case '7':
\           case '8': case '9': {
\             int p = s[j] - '0';
\             if (o->set[p]) {
\               if (n[i] != o->v[p]) goto end;
\             } else {
\               o->set[p] = 1;
\               o->v[p] = n[i];
\             }
\             i++;
\             break;
\           }
\           /* Mem location 0 must be 0 in SUBLEQ image! */
\           case 'Z': if (n[i] != 0) goto end; i++; break;
\           case 'N': if (n[i] != 65535) goto end; i++; break;
\           case '>': if (n[i] != (pc + i + 1)) goto end; i++;
\            break;
\           case '%': {
\             int q = va_arg(ap, int);
\             if (n[i] != q)
\               goto end;
\             i++;
\           } break;
\           case '!': {
\               uint16_t *p = va_arg(ap, uint16_t*);
\               *p = n[i];
\               i++;
\           } break;
\           case '?': i++; break;
\           case ' ': case '\t':
\           case '\n': case '\r': break;
\           default: r = -1; goto end;
\           }
\         }
\         while (isspace(s[j]))
\           j++;
\         r = (s[j] == 0) && (i <= sz);
\       end:
\         va_end(ap);
\         return r;
\       }
\
\       static long get(optimizer_t *o, char var) {
\         if (var < '0' || var > '9' || o->set[var - '0'] == 0)
\           return -1;
\         return o->v[var - '0'];
\       }
\
\       /* This section pattern matches the code finding
\        * sequences of SUBLEQ instructions against known
\        * instruction macros.  It is essentially a
\        * disassembler. It is liable not to work for every
\        * case, but will do so for the code that *I* want to
\        * speed up. */
\       static int optimizer(optimizer_t *o,
\           instruction_t *m, uint16_t pc) {
\
\         for (uint16_t i = 0; i < pc; i++) {
\           switch (m[i].m) {
\           case 0: o->z_reg[i] = 1; break;
\           case 1: o->one_reg[i] = 1; break;
\           case 0xFFFF: o->neg1_reg[i] = 1; break;
\           }
\         }
\
\         for (uint16_t i = 0; i < pc; i++) {
\           uint16_t q0 = 0, q1 = 0;
\           uint16_t n[DEPTH] = { 0, };
\
\           for (size_t j = 0; j < DEPTH; j++)
\             n[j] = m[L(i + j)].m;
\
\           /* Largest instructions *must* go first */
\
\           if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ> 11>\
\           ?Z> Z1> ZZ> 22> ?Z> Z2> ZZ> 33> !Z> Z3> ZZ>",
\           &q0, &q1) == 1
\             && get(o, '0') == (i+(3*12))
\             && get(o, '1') == (i+(3*12)+1)) {
\             m[L(i)].instruction = ISTORE;
\             m[L(i)].d = L(q0);
\             m[L(i)].s = L(q1);
\             o->matches[ISTORE]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ> 11>\
\           ?Z> Z1> ZZ>", &q0) == 1
\               && get(o, '0') == (i + 15)) {
\             m[L(i)].instruction = ILOAD;
\             m[L(i)].d = L(get(o, '1'));
\             m[L(i)].s = L(q0);
\             o->matches[ILOAD]++;
\             continue;
\           }
\
\           int shift = 0, l = 0, dest = 0;
\           for (l = 0; l < DEPTH; l += 9) {
\             if (match(o, n+l, DEPTH-l, i+l, "!Z>\
\                 Z!> ZZ>", &q0, &q1) == 1
\                 && q0 == q1) {
\               if (l == 0) {
\                 dest = q0;
\               } else {
\                 if (dest != q0) {
\                   break;
\                 }
\               }
\               shift++;
\             } else {
\               break;
\             }
\           }
\           if (shift >= 2) {
\             m[L(i)].instruction = LSHIFT;
\             m[L(i)].d = L(dest);
\             m[L(i)].s = shift;
\             o->matches[LSHIFT]++;
\             continue;
\           }
\
\
\           if (match(o, n, DEPTH, i, "00> 10> 11> 2Z>\
\               Z1> ZZ> !1>", &q0) == 1
\               && o->one_reg[q0]) {
\             m[L(i)].instruction = INV;
\             m[L(i)].d = L(get(o, '1'));
\             o->matches[INV]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ> ZZ>",
\           &q0) == 1
\               && get(o, '0') == (i + (3*4) + 2)) {
\             m[L(i)].instruction = IJMP;
\             m[L(i)].d = L(q0);
\             o->matches[IJMP]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "00> !Z> Z0> ZZ>",
\           &q0) == 1) {
\             m[L(i)].instruction = MOV;
\             m[L(i)].d = L(get(o, '0'));
\             m[L(i)].s = L(q0);
\             o->matches[MOV]++;
\             continue;
\           }
\
\           /* We should match multiple ones in a row and
\            * turn them into a left shift */
\           if (match(o, n, DEPTH, i, "!Z> Z!> ZZ>",
\           &q0, &q1) == 1
\               && q0 == q1) {
\             m[L(i)].instruction = DOUBLE;
\             m[L(i)].d = L(q1);
\             m[L(i)].s = L(q0);
\             o->matches[DOUBLE]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "!Z> Z!> ZZ>",
\           &q0, &q1) == 1) {
\             m[L(i)].instruction = ADD;
\             m[L(i)].d = L(q1);
\             m[L(i)].s = L(q0);
\             o->matches[ADD]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "00>") == 1) {
\             m[L(i)].instruction = ZERO;
\             m[L(i)].d = L(get(o, '0'));
\             o->matches[ZERO]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "ZZ!", &q0) == 1
\           && q0 >= SZ) {
\             m[L(i)].instruction = HALT;
\             o->matches[HALT]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "00!", &q0) == 1) {
\             m[L(i)].instruction = JMP;
\             m[L(i)].d = q0;
\             m[L(i)].s = L(get(o, '0'));
\             o->matches[JMP]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "N!>", &q0) == 1) {
\             m[L(i)].instruction = GET;
\             m[L(i)].d = L(q0);
\             o->matches[GET]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "!N>", &q0) == 1) {
\             m[L(i)].instruction = PUT;
\             m[L(i)].s = L(q0);
\             o->matches[PUT]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "!!>", &q0, &q1) == 1
\             && q0 != q1 && o->neg1_reg[L(q0)]) {
\             m[L(i)].instruction = INC;
\             m[L(i)].d = L(q1);
\             o->matches[INC]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "!!>", &q0, &q1) == 1
\             && q0 != q1 && o->one_reg[L(q0)]) {
\             m[L(i)].instruction = DEC;
\             m[L(i)].d = L(q1);
\             o->matches[DEC]++;
\             continue;
\           }
\
\           if (match(o, n, DEPTH, i, "!!>", &q0, &q1) == 1
\             && q0 != q1) {
\             m[L(i)].instruction = SUB;
\             m[L(i)].d = L(q1);
\             m[L(i)].s = L(q0);
\             o->matches[SUB]++;
\             continue;
\           }
\
\           o->matches[SUBLEQ]++;
\         }
\         return 0;
\       }
\
\
\       static int report(optimizer_t *o) {
\         double elapsed_s = (double)(o->end - o->start);
\         elapsed_s /= CLOCKS_PER_SEC;
\         int64_t total = 0, subs = 0;
\         FILE *e = stderr;
\         for (int i = 0; i < MAX; i++) {
\           total += o->cnt[i];
\           subs  += o->matches[i];
\         }
\         static const char *rep_div =
\         "+--------+--------+--------------+----------+\n";
\
\         if (fputs(rep_div, e) < 0)
\           return -1;
\         if (fprintf(e, "| Instr. | Subs.  | Instr. Cnt   |\
\        Instr. %% |\n") < 0)
\           return -1;
\         if (fputs(rep_div, e) < 0)
\           return -1;
\         for (int i = 0; i < MAX; i++)
\           if (fprintf(e, "| %s| % 6d | % 12"PRId64" |\
\        % 7.1f%% |\n",
\               names[i], o->matches[i], o->cnt[i],
\               100.0*((float)o->cnt[i])/(float)total) < 0)
\             return 1;
\         if (fputs(rep_div, e) < 0)
\           return -1;
\         if (fprintf(e, "| Totals | % 6d | % 12"PRId64" |\
\                 |\n",
\                    (int)subs, total) < 0)
\           return -1;
\         if (fputs(rep_div, e) < 0)
\           return -1;
\         if (fprintf(e, "|         EXECUTION TIME %.3f \
\       SECONDS      |\n",
\                     elapsed_s) < 0)
\           return -1;
\         if (fputs(rep_div, e) < 0)
\           return -1;
\         return 0;
\       }
\
\       int main(int s, char **v) {
\         static instruction_t m[SZ];
\         static optimizer_t o = { .matches = { 0, }, };
\         uint16_t pc = 0;
\         const int dbg = 0, optimize = 1, stats = 1;
\         for (int i = 1, d = 0; i < s; i++) {
\           FILE *f = fopen(v[i], "r");
\           if (!f)
\             return 1;
\           while (fscanf(f, "%d,", &d) > 0)
\             m[L(pc++)].m = d;
\           if (fclose(f) < 0)
\             return 2;
\         }
\
\         if (optimize)
\           if (optimizer(&o, m, pc) < 0)
\             return 1;
\         o.start = clock();
\         for (pc = 0; pc < SZ;) {
\           const int instruction = m[pc].instruction;
\           const uint16_t s = m[pc].s, d = m[pc].d;
\           if (dbg) {
\             if (fprintf(stderr, "{%ld:%d}",
\                  (long)pc, m[pc].instruction) < 0)
\               return 1;
\               /* Could return __LINE__ for simple debugging,
\                * but return val is limited to 255 usually */
\           }
\           if (stats) {
\             o.cnt[instruction/*% MAX*/]++;
\           }
\           switch (instruction) {
\           case SUBLEQ: { /* OG Instruction */
\             uint16_t a = m[pc++].m,
\                      b = m[L(pc++)].m,
\                      c = m[L(pc++)].m;
\             if (a == 65535) {
\               m[L(b)].m = getchar();
\             } else if (b == 65535) {
\               if (putchar(m[L(a)].m) < 0)
\                 return 3;
\               if (fflush(stdout) < 0)
\                 return 4;
\             } else {
\               uint16_t r = m[L(b)].m - m[L(a)].m;
\               if (r & 32768 || r == 0)
\                 pc = c;
\               m[L(b)].m = r;
\             }
\             }
\             break;
\           /* NB. We might be able to run more programs
\            * correctly if we disable these instructions if
\            * a write occurs within the bounds of an
\            * instruction macro, this would slow things down
\            * however. */
\           case JMP: pc = d; m[s].m = 0; break;
\           case MOV: m[d].m  = m[s].m; pc += 12; break;
\           case ADD: m[d].m += m[s].m; pc += 9; break;
\           case DOUBLE: m[d].m <<= 1; pc += 9; break;
\           case LSHIFT: m[d].m <<= s; pc += 9 * s; break;
\           case SUB: m[d].m -= m[s].m; pc += 3; break;
\           case ZERO: m[d].m = 0; pc += 3; break;
\           case IJMP: pc = m[d].m;  break;
\           case ILOAD: m[d].m = m[L(m[s].m)].m; pc += 24;
\             break;
\           case ISTORE: m[L(m[d].m)].m = m[s].m; pc += 48;
\             break;
\           case PUT:
\             if (putchar(m[L(m[pc].s)].m) < 0)
\               return 3;
\             if (fflush(stdout) < 0)
\               return 4;
\             pc += 3;
\             break;
\           case GET: m[m[pc].d].m = getchar(); pc += 3; break;
\           case HALT: goto done;
\           case INC: m[d].m++; pc += 3; break;
\           case DEC: m[d].m--; pc += 3; break;
\           case INV: m[d].m = ~m[d].m; pc += 21; break;
\           default:
\             return 5;
\           }
\         }
\       done:
\         o.end = clock();
\         if (stats)
\           if (report(&o) < 0)
\             return 1;
\         return 0;
\       }
\
\ A report is printed to standard error at the end of
\ execution.
\
\ ## Error Code list
\
\ This is a list of Standard Forth Error Codes, not all of
\ which are used by the application.
\
\         | Hex  | Dec  | Message                             |
\         | ---- | ---- | ----------------------------------- |
\         | FFFF |  -1  | ABORT                               |
\         | FFFE |  -2  | ABORT"                              |
\         | FFFD |  -3  | stack overflow                      |
\         | FFFC |  -4  | stack underflow                     |
\         | FFFB |  -5  | return stack overflow               |
\         | FFFA |  -6  | return stack underflow              |
\         | FFF9 |  -7  | do-loops nested too deeply          |
\         | FFF8 |  -8  | dictionary overflow                 |
\         | FFF7 |  -9  | invalid memory address              |
\         | FFF6 | -10  | division by zero                    |
\         | FFF5 | -11  | result out of range                 |
\         | FFF4 | -12  | argument type mismatch              |
\         | FFF3 | -13  | undefined word                      |
\         | FFF2 | -14  | interpreting a compile-only word    |
\         | FFF1 | -15  | invalid FORGET                      |
\         | FFF0 | -16  | attempt to use 0-len str. as a name |
\         | FFEF | -17  | pictured numeric out. str. overflow |
\         | FFEE | -18  | parsed string overflow              |
\         | FFED | -19  | definition name too long            |
\         | FFEC | -20  | write to a read-only location       |
\         | FFEB | -21  | unsupported operation               |
\         | FFEA | -22  | control structure mismatch          |
\         | FFE9 | -23  | address alignment exception         |
\         | FFE8 | -24  | invalid numeric argument            |
\         | FFE7 | -25  | return stack imbalance              |
\         | FFE6 | -26  | loop parameters unavailable         |
\         | FFE5 | -27  | invalid recursion                   |
\         | FFE4 | -28  | user interrupt                      |
\         | FFE3 | -29  | compiler nesting                    |
\         | FFE2 | -30  | obsolescent feature                 |
\         | FFE1 | -31  | >BODY used on non-CREATEd def.      |
\         | FFE0 | -32  | invalid name arg. (e.g., TO xxx)    |
\         | FFDF | -33  | block read exception                |
\         | FFDE | -34  | block write exception               |
\         | FFDD | -35  | invalid block number                |
\         | FFDC | -36  | invalid file position               |
\         | FFDB | -37  | file I/O exception                  |
\         | FFDA | -38  | non-existent file                   |
\         | FFD9 | -39  | unexpected end of file              |
\         | FFD8 | -40  | wrong BASE in float point convert   |
\         | FFD7 | -41  | loss of precision                   |
\         | FFD6 | -42  | floating-point divide by zero       |
\         | FFD5 | -43  | floating-point result out of range  |
\         | FFD4 | -44  | floating-point stack overflow       |
\         | FFD3 | -45  | floating-point stack underflow      |
\         | FFD2 | -46  | floating-point invalid argument     |
\         | FFD1 | -47  | compilation word list deleted       |
\         | FFD0 | -48  | invalid POSTPONE                    |
\         | FFCF | -49  | search-order overflow               |
\         | FFCE | -50  | search-order underflow              |
\         | FFCD | -51  | compilation word list changed       |
\         | FFCC | -52  | control-flow stack overflow         |
\         | FFCB | -53  | exception stack overflow            |
\         | FFCA | -54  | floating-point underflow            |
\         | FFC9 | -55  | floating-point unidentified fault   |
\         | FFC8 | -56  | QUIT                                |
\         | FFC7 | -57  | exception in tx or rx a character   |
\         | FFC6 | -58  | [IF], [ELSE], or [THEN] exception   |
\
\ <http://www.forth200x.org/throw-iors.html>
\
\         | Hex  | Dec  | Message                             |
\         | ---- | ---- | ----------------------------------- |
\         | FFC5 | -59  | ALLOCATE                            |
\         | FFC4 | -60  | FREE                                |
\         | FFC3 | -61  | RESIZE                              |
\         | FFC2 | -62  | CLOSE-FILE                          |
\         | FFC1 | -63  | CREATE-FILE                         |
\         | FFC0 | -64  | DELETE-FILE                         |
\         | FFBF | -65  | FILE-POSITION                       |
\         | FFBE | -66  | FILE-SIZE                           |
\         | FFBD | -67  | FILE-STATUS                         |
\         | FFBC | -68  | FLUSH-FILE                          |
\         | FFBB | -69  | OPEN-FILE                           |
\         | FFBA | -70  | READ-FILE                           |
\         | FFB9 | -71  | READ-LINE                           |
\         | FFB8 | -72  | RENAME-FILE                         |
\         | FFB7 | -73  | REPOSITION-FILE                     |
\         | FFB6 | -74  | RESIZE-FILE                         |
\         | FFB5 | -75  | WRITE-FILE                          |
\         | FFB4 | -76  | WRITE-LINE                          |
\
\


\ ## ASCII Art Diagram: Interpreter Control Flow
\
\ Interpreter control Flow as an ASCII art diagram:
\
\           .--------------------------.    .--------------.
\        .->| Get Next Word From Input |<---| Error: Throw |
\        |  .--------------------------.    .--------------.
\        |    |                                          ^
\        |    |                                          | (No)
\        |    v               (Not Found)                |
\        ^ .-----------------.           .--------------------.
\        | | Search For Word |---------->| Is token a number? |
\        | .-----------------.           .--------------------.
\        |    |                                       |
\        ^    | (Found)                               |  (Yes)
\        |    v                                       v
\        |  .------------------.  .---------------------.
\        |  | In command mode? |  |  In command mode?   |----.
\        ^  .------------------.  .---------------------.    |
\        |    |               |            |                 |
\        |    |  (Yes)        | (No)       |  (Yes)     (No) |
\        |    v               |            v                 |
\        |  .--------------.  |   .------------------------. |
\        |<-| Execute Word |  |   | Push Number onto Stack | |
\        |  .--------------.  |   .------------------------. |
\        |    ^               |                |             |
\        ^    |               v                |             |
\        |    | (Yes)   .--------------------. |             v
\        |    ----------| Is word immediate? | | .------------.
\        |              .--------------------. | | Compile num|
\        ^                        |            | | into next  |
\        |                        |  (No)      | | location in|
\        |                        v            | | dictionary |
\        |  .--------------------------------. | .------------.
\        |  |    Compile Pointer to word     | |            |
\        .--| in next available location in  | |            |
\        |  |         the dictionary         | |            |
\        |  .--------------------------------. v            v
\        |                                     |            |
\        .----<-------<-------<-------<-------<------<------<.
\
\ ## ASCII Art Diagram: Dictionary Structure
\
\ The dictionary structure as an ASCII art diagram:
\
\        +--------------------+
\        | Vocabulary Pointer |
\        +--------------------+
\            |
\           \|/
\            .
\        +------+----+-----------+--------...
\        | Prev | CB | Word Name | Code...
\        +------+----+-----------+--------...
\            |
\           \|/
\            .
\        +------+----+-----------+--------...
\        | Prev | CB | Word Name | Code...
\        +------+----+-----------+--------...
\            |
\           \|/
\            .
\        +-------+
\        | NULL  |
\        +-------+
\
\        Prev: Pointer to previous word in linked list
\        CB:   Part of the Name Field Address along with
\              the first byte of "Word Name", it contains
\              word header flags, as well as the length of
\              "Word Name", which is a variable length string.
\        Word Name: The name of the Forth word, it's a variable
\              length string. The length of which is stored in
\              the lowest five bits of "CB".
\        Code...: A variable length series cells that form the
\              code portion of the Forth word, usually ending
\              in an exit/return instruction.
\        NULL: The end of the linked list.
\        Vocabulary Pointer: The handle used to identify the
\              vocabulary, a pointer to the start of the linked
\              list of words.
\
\ ## Example Long Division C Code
\
\ This C code shows the example long division code that could
\ be used for "opDivMod".
\

#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdint.h>

static int long_division(uint32_t n, uint32_t d,
  uint32_t *quo, uint32_t *rem) {
  assert(quo);
  assert(rem);
  *quo = 0;
  *rem = 0;
  if (d == 0)
    return -1;
  uint32_t q = 0, r = 0;
  for (int i = 31; i >= 0; i--) {
    r <<= 1;
    r |= !!(n & (1ul << i));
    if (r >= d) {
      r -= d;
      q |= (1ul << i);
    }
  }
  *quo = q;
  *rem = r;
  return 0;
}

int main(int argc, char **argv) {
  if (argc != 3)
    return 1;
  unsigned long op = atol(argv[1]);
  unsigned long di = atol(argv[2]);
  uint32_t quo = 0, rem = 0;
  if (long_division(op, di, &quo, &rem) < 0)
    return 2;
  const char *fmt = "%lu / %lu = %lu rem: %lu\n";
  unsigned long q = quo, r = rem;
  int e = printf(fmt, op, di, q, r);
  return e < 0 ? 3 : 0;
}

\ ## Faster SUBLEQ Assembly Operations
\
\ Here is an alternate set of assembly instruction macros for
\ common operations, they could be integrated the system, some
\ are faster and shorter than the ones that the author has
\ come up with.
\
\ Taken from:
\
\ https://web.archive.org/web/20151121172708/\
\ http://www.sccs.swarthmore.edu/users/06/adem/engin/e25/finale
\
\ Some notes on the notation; "ZR" indicates the zero register
\ which should start out (and end up as zero), "TA"/"TB"/"TC"
\ are all temporary registers, "NR" should contain negative
\ one (or all bits set). If the third operand is missing the
\ jump destination is the next instruction, or in the notation
\ used here the jump address is "+1" (although that "+1" might
\ be converted to a "+3" by the assembler to convert to cell
\ address), the same goes for "+2"/"-2", they refer to the two
\ instructions forward or back, and not cell addresses. If only
\ one operand is present then the third operand is the usual
\ jump to next instruction, and the first operand is zeroed by
\ subtracting it from itself, eg. "subleq a" expands to
\ "subleq a a +1". The JMP instruction is the same as our JMP
\ instruction, ie. "JMP c" is "subleq Z Z c".
\
\ The code is not tested.
\
\ ### ADD a b c
\
\ Addition.
\
\        \ ADD a b c: m(c) = m(a) + m(b)
\        subleq a  ZR
\        subleq b  ZR    \ ZR = - a - b
\        subleq c  c     \ c  = 0
\        subleq ZR c     \ c  = a + b
\        subleq ZR ZR    \ ZR = 0
\
\ ### SUB a b c
\
\ Subtraction.
\
\        \ SUB a b c: m(c) = m(b) - m(a)
\        subleq TA TA    \ TA = 0
\        subleq TB TB    \ TB = 0
\        subleq a  TA
\        subleq b  TB
\        subleq TB TA    \ TA = b - a
\        subleq c  c
\        subleq TB TB
\        subleq TA TB    \ TB = a - b
\        subleq TB c     \ c = b - a
\
\ ### MOV a b
\
\ Move.
\
\        \ MOV a b: move m(a) to m(b)
\        subleq b  b
\        subleq a  ZR
\        subleq ZR b
\        subleq ZR ZR
\
\ ### JMPI a
\
\ Jump indirect.
\
\        \ JMPI a: jump to memory location stored in m(a)
\        subleq ZR a
\        subleq TA TA
\        subleq TA ZR
\        subleq ZR ZR TA
\
\ ### IFLE a b c (if less than or equal to)
\
\ If less than or equal to.
\
\        \ IFLE a b c: IF a<=b THEN JMP c
\        subleq TA TA
\        subleq a  TA    \ TA = -a
\        subleq TB TB
\        subleq TA TB    \ TB = a
\        subleq b  TB c  \ TB = a - b, if (a - b) <= 0 JMP c
\
\ ### IFGT a b c (if greater than)
\
\ If greater than.
\
\        \ IFGT a b c: IF a > b THEN JMP c
\        subleq TA TA
\        subleq a  TA    \ TA = -a
\        subleq TB TB
\        subleq b  TB    \ TB = -b
\        subleq TB TA    \ TA = b - a
\        subleq NR TB c  \ TB = b - a + 1, if (b-a+1)<=0 JMP c
\
\ ### DIV a b c
\
\ Division.
\
\        \ DIV a b c: m(c) = m(a) / m(b)
\        subleq TA
\        subleq TB
\        subleq TC
\
\        subleq b  TA
\        subleq TA TB    \ TB = b
\
\        subleq TA TA
\        subleq a  TA
\        subleq TA TC    \ TC = a
\
\        subleq c c
\
\        subleq NR c     \ c++
\        subleq TC TB +2 \ b -= a, if b <= 0 JMP 2
\        subleq ZR ZR -2 \ loop back to c++
\
\ ### MUL a b c
\
\ Multiply.
\
\        \ MUL a b c: m(c) = m(a) * m(b)
\        subleq TA
\        subleq TB
\        subleq TC
\
\        subleq b  TA
\        subleq TA TB    \ TB = b
\
\        subleq NR c     \ c = 1
\
\        subleq TA TA
\        subleq a  TA    \ TA = -a
\
\        subleq c c
\
\        \ sub (-a) from dest and b--, when b <=0 escape
\        subleq TA c
\        subleq TC TB +2
\        subleq ZR ZR -2 \ loop back
\
\ ## Obsolete Assembly Routines
\
\ This section contains old assembly routines that are useful
\ but have been replaced for various reasons.
\
\ ### Logical Operators in SUBLEQ
\
\ The logical operators, OR, XOR, and AND, have the same
\ pattern to how they work, and the borrow from how "rshift"
\ works. They both work by testing if the highest bit
\ is set and doubling both inputs and the output in order to
\ shift bits.
\
\ If we have the following table of bits, if we add those
\ bits together we can then use the comparison operators to
\ determine what the new bit should be in the output.
\
\       0 + 0 = 0
\       0 + 1 = 1
\       1 + 0 = 1
\       1 + 1 = 2
\
\ None of the operators output should be one when the result is
\ zero, XOR should output one when the result is equal to one,
\ OR when it is greater or equal to one, and AND should only be
\ one when the output is two. Otherwise the output should be
\ zero for the new bit.
\
\ The following Stack Overflow question goes over this in more
\ detail <https://stackoverflow.com/questions/34120161>.
\
\ Making these operators fast is especially important, in all
\ other Forth systems there is the reasonable expectation that
\ these operators are fast, and that they operate in a
\ single clock cycle, not so for the system. Subtraction and
\ addition are fast as usual, so some words later on have be
\ rewritten to use arithmetic instead.
\
\ It is possible to do all kinds of optimizations with what
\ are usually fast bitwise operations, for example, SWAR
\ operations or SIMD With A Register (SIMD stands for Single
\ Instruction Multiple Data).
\
\ The idea of SWAR optimizations is that you can process
\ multiple N-bit operations (say 8-bit) on a machine capable of
\ doing k\*N bit arithmetic (k = 2 for a 16-bit machine).
\
\ Unfortunately,
\ these optimizations tend to be bit-wise heavy and more
\ suitable for larger cell width systems anyway, but such
\ optimizations are worth knowing about. We can do a very
\ limited version of SWAR potentially in comparing Forth
\ strings (comparing them cell by cell instead of byte by
\ byte) if the strings are aligned (which they will be for
\ Forth words) and there is no junk at the end of the string.
\
\ *SWAR* operations, whilst well worth knowing about, are
\ not worth doing on this machine.
\
\ All of these operators could be replaced with a single
\ bitwise operator known as "mux", already shown. This could be
\ done to save on space without sacrificing speed too much.
\ These operators take up quite a lot of space, and there is
\ a lot that could be done to shrink the image.
\
:a opOr
  bwidth r0 MOV
  x ZERO
  r2 {sp} iLOAD
  --sp
  begin r0 while
    x x ADD
    tos r5 MOV r3 ZERO
    r5 -if r3 NG1! then r5 INC r5 -if r3 NG1! then
    r2   r5 MOV r4 ZERO
    r5 -if r4 NG1! then r5 INC r5 -if r4 NG1! then
    r3 r4 ADD r4 if x INC then
    r2 r2 ADD
    tos tos ADD
    r0 DEC
  repeat
  x tos MOV ;a
:a opXor
  bwidth r0 MOV
  x ZERO
  r2 {sp} iLOAD
  --sp
  begin r0 while
    x x ADD
    tos r5 MOV r3 ZERO r5
    -if r3 NG1! then r5 INC r5 -if r3 NG1! then
    r2   r5 MOV r4 ZERO r5
    -if r4 NG1! then r5 INC r5 -if r4 NG1! then
    r3 r4 ADD r4 INC r3 ONE!
    r4 if r3 ZERO then r3 x ADD
    r2 r2 ADD
    tos tos ADD
    r0 DEC
  repeat
  x tos MOV ;a
:a opAnd
  bwidth r0 MOV
  x ZERO
  r2 {sp} iLOAD
  --sp
  begin r0 while
   x x ADD
   tos r5 MOV r3 ZERO r5
   -if r3 NG1! then r5 INC r5 -if r3 NG1! then
   r2   r5 MOV r4 ZERO r5
   -if r4 NG1! then r5 INC r5 -if r4 NG1! then
   r3 r4 ADD two r4 ADD r3 ONE!
   r4 if r3 ZERO then r3 x ADD
   r2 r2 ADD
   tos tos ADD
   r0 DEC
  repeat
  x tos MOV ;a

\ ## Extra Code
\
\ This section of code can be fed to the interpreter with the
\ following command (on Unix systems):
\
\        cat extra.fth - | ./subleq subleq.dec
\
\ It will need to be adapted if it is to be meta-compiled. It
\ includes some missing standard Forth words, and the major
\ things that are missing - do loops and case.
\
\ ======================= TODO ================================
\ This should be a compile time option.
\
<ok> @ ' ) <ok> !
: debug source type ."  ok" cr ; ' debug <ok> !

 0 constant false
-1 constant true

\ A poorer, smaller, version of "random" can be defined as:
\
\        : random seed @ 31421 * 6927 + dup seed ! ; ( -- u )
\
\ We could mix this in with keyboard input, as we have no 
\ sources of actual entropy in this system, keyboard input
\ would be the closest thing.
\
variable seed here seed ! 
: random ( -- u : 16-bit xorshift )
  seed @ dup 0= if 0= then ( seed must not be zero )
  dup 13 lshift xor
  dup  9 rshift xor
  dup  7 lshift xor
  dup seed ! ;

: anonymous ( -- : make anonymous vocabulary and enable it )
  get-order 1+ here dup 1 cells allot 0 swap ! swap set-order ;

: umin 2dup swap u< if swap then drop ; ( u u -- u )
: umax 2dup      u< if swap then drop ; ( u u -- u )
: off false swap ! ; ( a -- )
: on true swap ! ; ( a -- )
: tab 9 emit ; ( -- : emit the tab character )
: spaces ( n -- : equiv. bl banner  )
    ?dup 0> if for aft space then next then ;
: 2+ 2 + ; ( u -- u : increment by two )
: 2- 2 - ; ( u -- u : decrement by two )
: not -1 xor ; ( u -- u : same as 'invert' in this Forth )
: binary $2 base ! ; ( -- : set numeric radix to binary )
: octal $8 base ! ; ( -- : set numeric base to octal )
: .base base @ dup decimal . base ! ; ( -- )
: also get-order over swap 1+ set-order ; ( -- )
: previous get-order nip 1- set-order ; ( -- )
\ : buffer block ; ( k -- a )
: enum dup constant 1+ ; ( n --, <string> )
: logical 0= 0= ; ( n -- f : turn a number into a 0 or -1 )
: limit rot min max ; ( n lo hi -- n )
: odd 1 and logical ; ( n -- f )
: even odd invert ; ( n -- f )
: nor or invert ; ( u u -- u )
: nand and invert ; ( u u -- u )
\ : under >r dup r> ; ( n1 n2 -- n1 n1 n2 )
: under over swap ; ( n1 n2 -- n1 n1 n2 )
: 2nip >r >r 2drop r> r> ; ( n1 n2 n3 n4 -- n3 n4 )
( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
: 2over >r >r 2dup r> swap >r swap r> r> -rot ;
: 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
: 2tuck 2swap 2over ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 n3 n4 )
: 4drop 2drop 2drop ; ( n1 n2 n3 n4 -- )
: trip dup dup ; ( n -- n n n : triplicate )
: 2pick dup >r pick r> 2+ pick swap ;
: log  >r 0 swap ( u base -- u : integer logarithm )
  begin swap 1+ swap r@ / dup 0= until drop 1- rdrop ;
: log2 0 swap ( u -- u : integer logarithm in base 2 )
  begin swap 1+ swap 2/ dup 0= until drop 1- ;
: average um+ 2 um/mod nip ; ( u u -- u )
: <=> 2dup > if 2drop -1 exit then < ;
: bounds over + swap ;
: 2, , , ; ( n n -- : write two numbers into the dictionary )
: d>s drop ; ( d -- n : convert dubs to single )
: d< rot 2dup >                    ( d -- f )
   if = nip nip if 0 exit then -1 exit then
   2drop u< ;
: dabs s>d if dnegate then ; ( d -- ud )
: d>= d< invert ;            ( d -- f )
: d>  2swap d< ;             ( d -- f )
: d<= d> invert ;            ( d -- f )
\ : du> 2swap du< ;          ( d -- f )
: d=  rot = -rot = and ;     ( d d -- f )
: d- dnegate d+ ;            ( d d -- d )
: d<> d= 0= ;                ( d d -- f )
: d0= or 0= ;                ( d -- f )
: d0<> d0= 0= ;              ( d -- f )
: d.r >r tuck dabs <# #s rot sign #> r> over - bl banner type ;
: ud.r >r <# #s #> r> over - bl banner type ;
: d. 0 d.r space ;
: ud. 0 ud.r space ;
: 2rdrop r> rdrop rdrop >r ; ( R: n n -- )
: 2. swap . . ; ( n n -- )
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ;
: */mod  >r m* r> m/mod ;  ( n n n -- r q )
: */  */mod nip ;          ( n n n -- q )
: holds begin dup while 1- 2dup + c@ hold repeat 2drop ;
: roll  dup 0> if swap >r 1- recurse r> swap else drop then ;
: signum s>d swap 0> 1 and xor ; ( n -- -1 | 0 1 : signum )
: >< dup 8 rshift swap 8 lshift or ; ( u -- u : swap bytes )
: #digits >r dup 0= if 1+ exit then r> log 1+ ; ( u b -- u )
: ** ( n u -- n : integer exponentiation )
  ?dup if
    over >r
    begin
      dup 1 >
    while
      swap r@ * swap 1-
    repeat rdrop drop
  else logical 1 and then ;

: ur. base @ >r base ! u. r> base ! ; ( u base -- )
: r. base @ >r base ! . r> base ! ; ( n base -- )

: b. $2 ur. ;  ( u -- )
: h. $10 ur. ; ( u -- )
: o. $8 ur. ;  ( u -- )
: d. $A r. ;   ( n -- )

\ : b. base @ swap 2 base ! . base ! ; ( u -- )
\ : h. base @ swap hex . base ! ;      ( u -- )
\ : o. base @ swap 8 base ! . base ! ; ( u -- )
\ : d. base @ swap decimal . base ! ;  ( n -- )


: @bits swap @ and ;                 ( a u -- u )
: ?\ if postpone \ then ; immediate
: ?( if postpone ( then ; immediate ( )
: ?if compile dup postpone if ; immediate
: screens ( k1 k2 -- : list blocks k1 to k2 )
  over -
  for
    dup . dup list 1+ key $D = if rdrop drop exit then
  next drop ;
: thru over - for dup >r load r> 1+ next drop ; ( k1 k2 -- )
: /string over min rot over + -rot - ;
: ndrop for aft drop then next ; ( 0...n n -- )
: unused $FFFF here - ; ( 65536 bytes available in this VM )
: char+ 1+ ; ( b -- b )
: str= compare 0= ;
: str< compare 0< ;

\ This version of Forth defines "mux" in SUBLEQ assembly,
\ previous versions did not and coded the bitwise routines in
\ assembly instead of using "mux", here "mux" is defined in
\ Forth. Note that the mask here is inverted from the built in
\ one.
\
\ You can use "mux" to define "or" and "and" as so:
\
\        : or dup mux ;
\        : and 0 -rot mux ;
\
: mux dup >r and swap r> invert and or ; ( x1 x2 mask -- x )

\
\ From: https://en.wikipedia.org/wiki/Integer_square_root
\ This function computes the integer square root of a number.
\
\ 'sc': unsigned small candidate
\ 'lc': unsigned large candidate
\
\ : square dup * ; ( n -- n : square a number )
\ : sqrt ( n -- u : integer square root )
\   #1 ?depth
\   s>d  if -$B lit throw then ( does not work for neg. values )
\   dup #2 < if exit then   ( return 0 or 1 )
\   dup                    ( u u )
\   #2 rshift sqrt ( recurse ) 2*    ( u sc )
\   dup                    ( u sc sc )
\   1+ dup square          ( u sc lc lc^2 )
\   >r rot r> <            ( sc lc bool )
\   if drop else nip then ; ( return small or large candidate )
\ 
\ : log ( u base -- u : the integer logarithm of u in 'base' )
\   >r
\   dup 0= -$B lit and throw ( logarithm of zero is an error )
\   #0 swap
\   begin
\     swap 1+ swap r@ / dup 0= ( keep dividing until 'u' is 0 )
\   until
\   drop 1- rdrop ;
\ 
: clz ( u -- : count leading zeros )
  ?dup 0= if $10 exit then
  $8000 0 >r begin
   2dup and 0=
  while
   r> 1+ >r 2/
  repeat
  2drop r> ;

( : log2 2 log ; ( u -- u : binary integer logarithm )
: log2 ( u -- u )
  ?dup 0= -$B lit and throw clz $10 swap - 1- ; 
\ 
\ <forth.sourceforge.net/algorithm/bit-counting/index.html>
: count-bits ( number -- bits )
  dup $5555 and swap 1 rshift $5555 and +
  dup $3333 and swap 2 rshift $3333 and +
  dup $0F0F and swap 4 rshift $0F0F and +
  $FF mod ;
\ 
\ \ <forth.sourceforge.net/algorithm/firstbit/index.html>
: first-bit ( number -- first-bit )
  dup   1 rshift or
  dup   2 rshift or
  dup   4 rshift or
  dup   8 rshift or
  dup $10 rshift or
  dup   1 rshift xor ;
 
: gray-encode dup 1 rshift xor ; ( gray -- u )
: gray-decode ( u -- gray )
\ dup $10 rshift xor ( <- 32 bit )
  dup   8 rshift xor 
  dup   4 rshift xor
  dup   2 rshift xor 
  dup   1 rshift xor ;

system -order
.( DONE ) cr

\ \ http://forth.sourceforge.net/word/n-to-r/index.html
\ \ Push n+1 elements on the return stack.
\ : n>r ( xn..x1 n -- , R: -- x1..xn n )
\   dup
\   begin dup
\   while rot r> swap >r >r 1-
\   repeat
\   drop r> swap >r >r ; \ compile-only
\ 
\ \ http://forth.sourceforge.net/word/n-r-from/index.html
\ \ pop n+1 elements from the return stack.
\ : nr> ( -- xn..x1 n, R: x1..xn n -- )
\     r> r> swap >r dup
\     begin dup
\     while r> r> swap >r -rot 1-
\     repeat
\     drop ; \ compile-only

\ : ?exit if rdrop exit then ;

\ $FFFE constant rp0
 
\ : +leading ( b u -- b u: skip leading space )
\     begin over c@ dup bl = swap 9 = or while 1 /string repeat ;

\ http://forth.sourceforge.net/word/string-plus/index.html
\ ( addr1 len1 addr2 len2 -- addr1 len3 )
\ append the text specified by addr2 and len2 to the text of length len2
\ in the buffer at addr1. return address and length of the resulting text.
\ an ambiguous condition exists if the resulting text is larger
\ than the size of buffer at addr1.
\ : string+ ( bufaddr buftextlen addr len -- bufaddr buftextlen+len )
\        2over +         ( ba btl a l bta+btl )
\        swap dup >r     ( ba btl a bta+btl l ) ( r: l )
\        move
\        r> + ;


\ ( addr1 len1 c -- addr1 len2 )
\ append c to the text of length len2 in the buffer at addr1.
\ Return address and length of the resulting text.
\ An ambiguous condition exists if the resulting text is larger
\ than the size of buffer at addr1.
\ : string+c ( addr len c -- addr len+1 )
\   dup 2over + c! drop 1+ ;

\ http://forth.sourceforge.net/algorithm/unprocessed/valuable-algorithms.txt
\ : -m/mod over 0< if dup    >r +       r> then u/mod ;         ( d +n - r q )
\ :  m/     dup 0< if negate >r dnegate r> then -m/mod swap drop ; ( d n - q )

\ From comp.lang.forth:
\ : du/mod ( ud1 ud2 -- udrem udquot )  \ b/d = bits/double
\   0 0 2rot b/d 0 do 2 pick over 2>r d2* 2swap d2* r>
\  0< 1 and m+ 2dup 7 pick 7 pick du< 0= r> 0< or if 5 pick
\  5 pick d- 2swap 1 m+ else 2swap then loop 2rot 2drop ; 


\ One possible word-set for structures in Forth (something
\ which Forth is really lacking in and is its major weakness).
\
\ These works could do with some compiler security, both with
\ minimum stack checks and magic numbers.
\
\ Example:
\
\ struct
\   byte: >b1
\   byte: >b2
\   cell: >w1
\   cell: >w2
\ size: /foo
\
\ struct
\   byte: >c1
\   7 unused
\   /foo field: >foo
\ ;struct
\
: struct 0 ;
: field:  ( offset size -- offset' )
  create over , +
  does> @ + ;
: byte:   1 field: ;
: cell:   2 field: ;
: long:   4 field: ;
: union:  0 field: ;
: unused  +  ;
: size:  constant ;
: ;struct drop ;

\ Make a word called "mark" which when called erases
\ everything after it has been defined.
: mark
  $" defined (mark) [if] (mark) [then] marker (mark) "
  count evaluate ;
mark
' ) <ok> !
.( LOADED EFORTH. ) cr
.( DICTIONARY: ) here . cr
.( EFORTH:     ) ' 1- u. cr
<ok> !
\
\ ## User Login System
\
\ This section contains a (insecure) login system with period
\ accurate cryptographic practices for the 1980s. It is
\ another show case in leveraging the built in capabilities
\ of Forth to accomplish a task. It has no utility.
\

<ok> @ ' ) <ok> !

system +order
: wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )

( NB. Bitwise ops must be masked off on non 16-bit machines )
: crc ( b u -- u : calculate ccitt-ffff CRC )
  $FFFF >r begin ?dup while
   over c@ r> swap
   ( CCITT polynomial $1021, or "x16 + x12 + x5 + 1" )
   over $8 rshift xor ( crc x )
   dup  $4 rshift xor ( crc x )
   dup  $5 lshift xor ( crc x )
   dup  $C lshift xor ( crc x )
   swap $8 lshift xor ( crc )
   >r +string
  repeat r> nip ;

( A primitive user login system [that is super insecure]. )
wordlist +order definitions
wordlist constant users
dup constant (prompt)
: conceal $1B emit ." [8m" ; ( NB. Could also override <emit> )
: reveal $1B emit ." [28m" ;
: secure users 1 set-order ;
: restore only forth definitions decimal (prompt) <ok> ! ;
: message ." user: " ;
: fail ." Invalid username or password" cr message ;
: success ." logged in." ;
: pass token count crc ; ( super-super-secure <_< )
: ask ." pass: " conceal query reveal ;

forth-wordlist +order definitions

: user:
  create pass ,
  does> ask @ pass = if restore success exit then fail ;
: login secure message ' ) <ok> ! ;
: .users get-order secure words set-order ;

users +order definitions
user: guest guest
user: admin password1
user: archer dangerzone
user: cyril figgis
user: lana stirling
users -order
.( EFORTH ONLINE ) cr
<ok> ! login

\ ## Sokoban
\
\ This is a game of Sokoban, to play, type:
\
\       ./embed sokoban.fth /dev/stdin
\       cat sokoban.fth /dev/stdin | ./subleq subleq.dec
\
\ On the command line. Four maps are provided, more can be
\ found online at <https://github.com/begoon/sokoban-maps>,
\ where the four maps were found.
\
\ * Author:  Richard James Howe
\ * License: The Unlicense (excluding the maps)
\

variable ook <ok> @ ook ! ' ( <ok> !
.( LOADING... ) cr

only forth definitions hex

variable sokoban-wordlist
sokoban-wordlist +order definitions

$20    constant maze
char X constant wall
char * constant boulder
char . constant off
char & constant on
char @ constant player
char ~ constant player+ ( player + off pad )
$10    constant l/b     ( lines   per block )
$40    constant c/b     ( columns per block )
     7 constant bell    ( bell character )

variable position  ( current player position )
variable moves     ( moves made by player )
variable lblk      ( last block loaded )

( used to store rule being processed )
create rule 3 c, 0 c, 0 c, 0 c,

: n1+ swap 1+ swap ; ( n n -- n n )
: match              ( a a -- f )
  n1+ ( replace with umin of both counts? )
  count
  for aft
    count rot count rot <> if 2drop rdrop 0 exit then
  then next 2drop -1 ;

: beep bell emit ; ( -- )
: ?apply           ( a a a -- a, R: ? -- ?| )
  >r over swap match if drop r> rdrop exit then rdrop ;

: apply ( a -- a )
 $" @ "  $"  @"  ?apply
 $" @."  $"  ~"  ?apply
 $" @* " $"  @*" ?apply
 $" @*." $"  @&" ?apply
 $" @&." $"  ~&" ?apply
 $" @& " $"  ~*" ?apply
 $" ~ "  $" .@"  ?apply
 $" ~."  $" .~"  ?apply
 $" ~* " $" .@*" ?apply
 $" ~*." $" .@&" ?apply
 $" ~&." $" .~&" ?apply
 $" ~& " $" .~*" ?apply beep ;

: pack ( c0...cn b n -- )
  2dup swap c! for aft 1+ tuck c! then next drop ;

: locate ( b u c -- u f )
  >r
  begin
    ?dup
  while
    1- 2dup + c@ r@ = if nip rdrop -1 exit then
  repeat
  rdrop
  drop
  0 0 ;

: relative swap c/b * + + ( $3ff and ) ; ( +x +y pos -- pos )
: +position position @ relative ; ( +x +y -- pos )
: double 2* swap 2* swap ;  ( u u -- u u )
: arena lblk @ block b/buf ; ( -- b u )
: >arena arena drop + ;     ( pos -- a )
: fetch                     ( +x +y -- a a a )
  2dup   +position >arena >r
  double +position >arena r> swap
  position @ >arena -rot ;
: rule@ fetch c@ rot c@ rot c@ rot ; ( +x +y -- c c c )
: 3reverse -rot swap ;               ( 1 2 3 -- 3 2 1 )
: rule! rule@ 3reverse rule 3 pack ; ( +x +y -- )
: think 2dup rule! rule apply >r fetch r> ; ( +x +y --a a a a )
: count! count rot c! ;              ( a a -- )

\ 'act' could be made to be more elegant, but it works, it
\ handles rules of length 2 and length 3

: act ( a a a a -- )
  count swap >r 2 =
  if
     drop swap r> count! count!
  else
     3reverse r> count! count! count!
  then drop ;

: #boulders ( -- n )
   0 arena
   for aft
     dup c@ boulder = if n1+ then
     1+
   then next drop ;
: .boulders  ." BOLDERS: " #boulders u. cr ; ( -- )
: .moves    ." MOVES: " moves    @ u. cr ; ( -- )
: .help     ." WASD - MOVEMENT" cr ." H    - HELP" cr ; ( -- )
: .maze lblk @ list ;                  ( -- )
: show ( page cr ) .maze .boulders .moves .help ; ( -- )
: solved? #boulders 0= ;               ( -- )
: finished? solved? if 1 throw then ; ( -- )
: instructions ;                      ( -- )
: where >r arena r> locate ;          ( c -- u f )
: player? player where 0= if drop player+ where else -1 then ;
: player! player? 0= throw position ! ; ( -- )
: start player! 0 moves ! ;           ( -- )
: .winner show cr ." SOLVED!" cr ;    ( -- )
: .quit cr ." Quitter!" cr ;          ( -- )
: finish 1 = if .winner exit then .quit ; ( n -- )
: rules think act player! ;           ( +x +y -- )
: +move 1 moves +! ;                  ( -- )
: ?ignore over <> if rdrop then ;     ( c1 c2 --, R: x -- | x )
: left  [char] a ?ignore -1  0 rules +move ; ( c -- c )
: right [char] d ?ignore  1  0 rules +move ; ( c -- c )
: up    [char] w ?ignore  0 -1 rules +move ; ( c -- c )
: down  [char] s ?ignore  0  1 rules +move ; ( c -- c )
: help  [char] h ?ignore instructions ; ( c -- c )
: end  [char] q ?ignore drop 2 throw ; ( c -- | c, R ? -- | ? )
: default drop ;  ( c -- )
: command up down left right help end default finished? ;
: maze! dup lblk ! block drop ; ( k -- )
: input key ;        ( -- c )

sokoban-wordlist -order definitions
sokoban-wordlist +order

: sokoban ( k -- )
  maze! start
  begin
    show input ' command catch ?dup
  until finish ;

decimal 47 maze!
only forth definitions decimal
editor z
 1 a            XXXXX
 2 a            X   X
 3 a            X*  X
 4 a          XXX  *XXX
 5 a          X  *  * X
 6 a        XXX X XXX X     XXXXXX
 7 a        X   X XXX XXXXXXX  ..X
 8 a        X *  *             ..X
 9 a        XXXXX XXXX X@XXXX  ..X
10 a            X      XXX  XXXXXX
11 a            XXXXXXXX
12 a
n z
 1 a       XXXXXXXXXXXX
 2 a       X..  X     XXX
 3 a       X..  X *  *  X
 4 a       X..  X*XXXX  X
 5 a       X..    @ XX  X
 6 a       X..  X X  * XX
 7 a       XXXXXX XX* * X
 8 a         X *  * * * X
 9 a         X    X     X
10 a         XXXXXXXXXXXX
11 a
n z
 1 a               XXXXXXXX
 2 a               X     @X
 3 a               X *X* XX
 4 a               X *  *X
 5 a               XX* * X
 6 a       XXXXXXXXX * X XXX
 7 a       X....  XX *  *  X
 8 a       XX...    *  *   X
 9 a       X....  XXXXXXXXXX
10 a       XXXXXXXX
n z
 1 a                     XXXXXXXX
 2 a                     X  ....X
 3 a          XXXXXXXXXXXX  ....X
 4 a          X    X  * *   ....X
 5 a          X ***X*  * X  ....X
 6 a          X  *     * X  ....X
 7 a          X ** X* * *XXXXXXXX
 8 a       XXXX  * X     X
 9 a       X   X XXXXXXXXX
10 a       X    *  XX
11 a       X **X** @X
12 a       X   X   XX
13 a       XXXXXXXXX
q

.( LOADED ) cr
.( Type '# sokoban' to play, where '#' is a block number ) cr
.( For example "47 sokoban" ) cr
.( Follow the on screen instructions to play a game. ) cr
ook <ok> !

\ ## Simple Bootloader
\
\ Calling this facility a bootloader might be overselling it,
\ the word "ingest" essentially performs the opposite of this
\ systems version of "dump". It retrieves a series of numbers
\ represented in the current base and stores them at
\ consecutive memory locations until either an error has
\ occurred or the number of cells it is meant to write has
\ been reached.
\
\ The word-set needed to achieve this is tiny and it leverages
\ the default Forth I/O mechanisms and the Forth language to
\ achieve what is needed.
\
\ There a few facilities that are missing which one would
\ expect from a bootloader program, but on second glance are
\ readily available. If we are boot-loading via the Forth
\ terminal we can either dump the written memory back out to
\ verify it is correct, or load a checksum algorithm, execute
\ it and then get then read the value back (if the checksum
\ algorithm fails to load due to line noise a UART as an
\ example, you will not get a valid checksum either way).
\
\ Forth code can be executed once loaded with the "execute"
\ instruction.
\
\ The words defined are:
\
\ * "nul?", This is used to check if we are at the end of
\ the current line, which could have either contained numbers
\ we have processed, or been empty. It accepts a counted
\ string.
\ * "grab", Get a word from the input stream, we cannot rely
\ on "word" to do this directly as we might be at the end of
\ the line, if we are it refills the line and attempts again,
\ it will do this until success.
\ * "integer", gets a space delimited word from the input
\ stream and then attempts to convert it into a number in the
\ current input radix and either returns that number and
\ signals success or it returns the word (as an address and
\ length) and a flag indicating failure. "integer" returns a
\ single cell number.
\ * "integer?", integer returns a single cell or it throws
\ an error. It could be extended to look for overflow, and
\ to check for double cell number entry, but it does not, it
\ just checks to make sure that input was a number.
\ * "ingest", what we are building towards, the inverse of
\ "dump", it takes a memory address and length and attempts
\ to populate it with a list of integers.
\
\ Note however how small the program is, and how much it
\ *does not* do. It simple, but can be used as a bootloader.
\

: nul? count nip 0= ; ( a -- f : is counted word empty? )
: grab ( <word> -- a : get word from input stream  )
  begin token dup nul? 0= ?exit drop query again ;
: integer grab count number? nip ; ( <num> -- n f : get int. )
: integer? integer 0= ( dpl @ 0>= or ) -$18 and throw ;
: ingest ( a u -- : opposite of 'dump', load nums into mem )
  cell / for aft integer? over ! cell+ then next drop ;

\ ## Floating Point Implementation
\
\ This section contains the original source for the floating
\ point module:
\
\ Vierte Dimension Vol.2, No.4 1986
\
\
\         A FAST HIGH-LEVEL FLOATING POINT
\
\                 Robert F. Illyes
\
\                      ISYS
\                PO Box 2516, Sta. A
\                Champaign, IL 61820
\                Phone: 217/359-6039
\
\ If binary normalization and rounding are used, a fast
\ single-precision FORTH floating point can be built with
\ accuracy adequate for many applications. The creation
\ of such high-level floating points has become of more
\ than academic interest with the release of the Novix
\ FORTH chip. The FORTH-83 floating point presented here
\ is accurate to 4.8 digits. Values may range from about
\ 9E-4933 to about 5E4931. This floating point may be
\ used without fee provided the copyright notice and
\ associated information in the source are included.
\
\ FIXED VS. FLOATING POINT
\
\ FORTH programmers have traditionally favored fixed over
\ floating point. A fixed point application is harder to
\ write. The range of each value must be known and
\ considered carefully, if adequate accuracy is to be
\ maintained and overflow avoided. But fixed point
\ applications are much more compact and often much
\ faster than floating point (in the absence of floating
\ point hardware).
\
\ The debate of fixed vs. floating point is at least as
\ old as the ENIAC, the first electronic digital
\ computer. John von Neumann used fixed point on the
\ ENIAC. He felt that the detailed understanding of
\ expressions required by fixed point was desirable, and
\ that fixed point was well worth the extra time (1).
\
\ But computers are no longer the scarce resource that
\ they once were, and the extra programming time is often
\ more costly than any advantages offered by fixed point.
\ For getting the most out of the least hardware,
\ however, fixed point will always be the technique of
\ choice.
\
\ Fixed point arithmetic represents a real number as a
\ ratio of integers, with an implicit divisor. This
\ implicit divisor may be thought of as the
\ representation of unity. If unity were represented by
\ 300, for example, 2.5 would be represented by 750.
\
\ To multiply 2.5 by 3.5, with all values representing
\ unity as ten, one would evaluate
\
\                      25 * 35
\                      -------
\                        10
\
\ The ten is called a scale factor, and the division by
\ ten is called a scaling operation. This expression is
\ obviously inefficient, requiring both a division and a
\ multiplication to accomplish a multiplication.
\
\ Most scaling operations can, however, be eliminated by
\ a little algebraic manipulation. In the case of the sum
\ of two squares, for example, where A and B are fixed
\ point integers and unity is represented by the integer
\ U,
\
\       A * A   B * B           (A * A)+(B * B)
\       ----- + -----    -->    ---------------
\         U       U                    U
\
\ In addition to the elimination of a division by U, the
\ right expression is more accurate. Each division can
\ introduce some error, and halving the number of
\ divisions halves the worst-case error.
\
\ DECIMAL VS. BINARY NORMALIZATION
\
\ A floating point number consists of two values, an
\ exponent and a mantissa. The mantissa may represent
\ either an integer or a fraction. The exponent and the
\ mantissa are related to each other in the same way as
\ the value and power of ten in scientific notation are
\ related.
\
\ A mantissa is always kept as large as possible. This
\ process is called normalization. The following
\ illustrates the action of decimal normalization with an
\ unsigned integer mantissa:
\
\          Value       Stack representation
\            4
\
\          5 * 10         50000  0  --
\                3
\            * 10          7000  0  --
\                3
\            * 10         50000 -1  --
\
\ The smallest the mantissa can become is 6554. If a
\ mantissa of 6553 is encountered, normalization requires
\ that it be made 65530, and that the exponent be
\ decremented. It follows that the worst-case error in
\ representing a real number is half of 1 part in 6554,
\ or 1 part in 13108. The error is halved because of the
\ rounding of the real number to the nearest floating
\ point value.
\
\ Had we been using binary normalization, the smallest
\ mantissa would have been 32768, and the worst case
\ error in representation would have been 1 part 65536,
\ or 1/5 that of decimal normalization. LOG10(65536) is
\ 4.8, the accuracy in decimal digits.
\
\ As with fixed point, scaling operations are required by
\ floating point. With decimal normalization, this takes
\ the form of division and multiplication by powers of
\ ten. Unlike fixed point, no simple algebraic
\ manipulation will partly eliminate the scale factors.
\ Consequently there are twice as many multiplications
\ and divisions as the floating point operators would
\ seem to imply. Due to the presence of scaling in 73% of
\ decimally normalized additions (2), the amount is
\ actually somewhat over twice.
\
\ With binary normalization, by contrast, this extra
\ multiplication effectively disappears. The scaling by
\ a power of two can usually be handled with a single
\ shift and some stack manipulation, all fast operations.
\
\ Though a decimally normalized floating point can be
\ incredibly small (3), a binary normalized floating
\ point has 1/5 the error and is about twice as fast.
\
\ It should be mentioned that the mantissa should be
\ multiples of 2 bytes if the full speed advantage of
\ binary normalization is to be available. Extra shifting
\ and masking operations are necessary with odd byte
\ counts when using the 2-byte arithmetic of FORTH.
\
\ NUMBER FORMAT AND ARITHMETIC
\
\ This floating point package uses an unsigned single
\ precision fraction with binary normalization,
\ representing values from 1/2 to just under 1. The high
\ bit of the fraction is always set.
\
\ The sign of the floating point number is carried in the
\ high bit of the single precision exponent, The
\ remaining 15 bits of the exponent represent a power of
\ 2 in excess 4000 hex. The use of excess 4000 permits
\ the calculation of the sign as an automatic outcome of
\ exponent arithmetic in multiplication and division.
\
\ A zero floating point value is represented by both a
\ zero fraction and a zero exponent. Any operation that
\ produces a zero fraction also zeros the exponent.
\
\ The exponent is carried on top of the fraction , so the
\ sign may be tested by the phrase DUP 0< and zero by the
\ phrase DUP 0= .
\
\ The FORTH-83 Double Number Extension Word Set is
\ required. Floating point values are used with the "2"
\ words: 2CONSTANT, 2@, 2DUP, etc.
\
\ There is no checking for exponent overflow or underflow
\ after arithmetic operation, nor is division by zero
\ checked for. The rationale for this is the same as with
\ FORTH integer arithmetic. By requiring that the user
\ add any such tests, 1) all arithmetic isn't slowed by
\ tests that are only sometimes needed and 2) the way in
\ which errors are resolved may be determined by the
\ user. The extremely large exponent range makes exponent
\ overflow and underflow quite unlikely, of course.
\
\ All of the arithmetic is rounding. The failure to round
\ is the equivalent of throwing a bit of accuracy away.
\ The representational accuracy of 4.8 digits will be
\ quickly lost without rounding arithmetic.
\
\ The following words behave like their integer
\ namesakes:
\
\      F+  F-  F*  F/  F2*  F2/  FABS  FNEGATE  F<
\
\ Single precision integers may be floated by FLOAT, and
\ produced from floating point by FIX and INT, which are
\ rounding and truncating, respectively. DFLOAT floats a
\ double precision integer.
\
\ NUMBER INPUT AND OUTPUT
\
\ Both E and F formats are supported. A few illustrations
\ should suffice to show their usage. An underscore
\ indicates the point at which the return key is pressed.
\ PLACE determines the number of places to the right of
\ the decimal point for output only.
\
\            12.34  F      F. _ 12.340
\            12.34  F      E. _ 1.234E1
\            .033 E -1002  E. _ 3.300E-1004
\
\            4 PLACES
\
\            2. F -3. F F/ F. _ -0.6667
\            2. F -3. F F/ E. _ -6.6667E-1
\
\ F and E will correctly float any input string
\ representing a signed double precision number. There
\ may be as many as 9 digits to the right of the decimal
\ point. Numbers input by E are accurate to over 4
\ digits. F is accurate to the full 4.8 digits if there
\ are no digits to the right of the decimal point.
\ Conversion is slightly less accurate with zeros to the
\ right of the decimal point because a division by a
\ power of ten must be added to the input conversion
\ process.
\
\ F and E round the input value to the nearest floating
\ point value. So a sixth digit will often allow a more
\ accurately rounded conversion, even thought the result
\ is only accurate to 4.8 digits. There is no advantage
\ to including trailing zeros, however. In many floating
\ points, this extra accuracy can only be achieved by the
\ inconvenient procedure of entering the values as
\ hexadecimal integers.
\
\ Only the leftmost 5 digits of the F. output are
\ significant. F. displays values from 32767 to -32768,
\ with up to 4 additional places to the right of the
\ decimal point. The algorithm for F. avoids double
\ rounding by using integer rather than floating point
\ multiplication to scale by a power of ten. This gives
\ as much accuracy as possible at the expense of a
\ somewhat limited range. Since a higher limit on size
\ would display digits that are not significant, this
\ range limit does not seem at all undesirable.
\
\ Like E input, E. is accurate to somewhat over 4 digits.
\ The principal source of inaccuracy is the function EXP,
\ which calculates powers of 2.
\
\ The following extended fraction is used by EXP. It
\ gives the square root of 2 to the x power. The result
\ must be squared to get 2 to the x.
\
\                       2x
\          1 + ---------------------------
\                               57828
\              34.668 - x - --------------
\                                         2
\                           2001.18 + (2x)
\
\ In order to do E format I/O conversion, one must be
\ able to evaluate the expressions
\
\          a   a/log10(2)        b    b*log10(2)
\        10 = 2           and   2 = 10
\
\ These log expressions may be easily evaluated with
\ great precision by applying a few fixed point
\ techniques. First, a good rational approximation to
\ log10(2) is needed.
\
\             log10(2)     = .3010299957
\             4004 / 13301 = .3010299978
\
\ The following code will convert an integer power of
\ ten, assumed to be on the stack, into a power of 2:
\
\                13301 4004 */MOD >R
\                FLOAT 4004 FLOAT F/ EXP
\                R> +
\
\ The first line divides the power of ten by log10(2) and
\ pushes the quotient on the return stack. The quotient
\ is the integer part of the power of two.
\
\ The second line finds the fractional part of the power
\ of two by dividing the remainder by the divisor. This
\ floating point fractional part is evaluated using EXP.
\
\ The third line adds the integer power of two into the
\ exponent of the floating point value of the fractional
\ part, completing the conversion.
\
\ The inverse process is used to convert a power of 2 to
\ a power of ten.
\
\ FORTH-83 LIMITATIONS
\
\ Perhaps the most serious deficiency in the FORTH-83
\ with extensibility as its pre-eminent feature, it is
\ surprisingly difficult to write standard code that will
\ alter the processing of numeric input strings by the
\ interpreter and compiler.
\
\ It is usually a simple matter to replace the system
\ conversion word (usually called NUMBER) with a routine
\ of ones choice. But there if no simple standard way of
\ doing this. The interpreter, compiler and abort
\ language are all interwoven, and may all have to be
\ replaced if a standard solution is sought.
\
\ This floating point package assumes that double
\ precision integers are generated if the numeric input
\ string contains a period, and that a word PLACES can be
\ written that will leave the number of digits to the
\ right of the period. This does not seem to be
\ guaranteed by FORTH-83, although it may be safely
\ assumed on most systems that include double precision.
\
\ If you know how this part of your system works, you
\ will probably want to eliminate the words E and F, and
\ instead force floating point conversion of any input
\ string containing a period. Double precision integers
\ could still be generated by using a comma or other
\ punctuation.
\
\ It is suggested that future FORTH standards include the
\ word NUMBER, which is a vector to the current input
\ numeric word.
\
\ It is also suggested that the Double Number Extension
\ Wordset specification include a requirement that the
\ interpreter and compiler be able to accept input
\ strings specifying double precision values.
\
\ COMMENTS ON THE FOLLOWING CODE
\
\ The words ". and "- leave the ASCII values for period
\ and minus, respectively. Replace these with whatever
\ language you prefer for insertion of ASCII values.
\
\ The size of F+ can be considerably reduced at the
\ expense of quite a lot of execution speed. Think twice
\ before you simplify it.
\
\ The normalizing word NORM expects the stack value under
\ the exponent to be a double precision signed integer.
\ It leaves a normalized floating point number, rounding
\ the double precision integer into the fraction.
\
\ ALIGN and RALIGN expect an integer shift count with an
\ unsigned double precision number beneath. They leave
\ double precision unsigned integer results. At least one
\ shift is always performed. RALIGN rounds after
\ alignment.
\
\ UM/ divides an unsigned double precision number by an
\ unsigned single precision number, and rounds the single
\ precision quotient.
\
\ ZERO forces a floating point value with a zero fraction
\ to also have a zero exponent.
\
\ FSIGN applies the sign of the stack value under the
\ exponent to the exponent. The double precision integer
\ under an exponent is left unsigned.
\
\ FEXP evaluates a power of e. It is included because it
\ is a trivial but useful application of EXP.
\
\ GET converts the next word in the input stream into a
\ single precision signed integer.
\
\ REFERENCES
\
\ 1. Von Neumann, J., John von Neumann Collected Works,
\ vol. 5, p.113.
\
\ 2. Knuth, D. K., The Art of Computer Programming,
\ second edition, vol. 2, pp. 238,9.
\
\ 3. Tracy, M., Zen Floating Point, 1984 FORML Conference
\ Proceedings, pp. 33-35.
\

( FORTH-83 FLOATING POINT.
  ----------------------------------
  COPYRIGHT 1985 BY ROBERT F. ILLYES

        PO BOX 2516, STA. A
        CHAMPAIGN, IL 61820
        PHONE: 217/826-2734  )     HEX

: ZERO  OVER 0= IF DROP 0 THEN ;
: FNEGATE 8000 XOR ZERO ;
: FABS  7FFF AND ;
: NORM  >R 2DUP OR
        IF BEGIN DUP 0< NOT
           WHILE D2* R> 1- >R
           REPEAT SWAP 0< - ?DUP
           IF R> ELSE 8000 R> 1+ THEN
        ELSE R> DROP THEN ;

: F2*   1+ ZERO ;
: F*    ROT + 4000 - >R UM* R> NORM ;
: FSQ   2DUP F* ;

: F2/   1- ZERO ;
: UM/   DUP >R UM/MOD SWAP R>
        OVER 2* 1+ U< SWAP 0< OR - ;
: F/    ROT SWAP - 4000 + >R
        0 ROT ROT 2DUP U<
        IF   UM/ R> ZERO
        ELSE >R D2/ FABS R> UM/ R> 1+
        THEN ;

: ALIGN 20 MIN 0 DO D2/ LOOP ;
: RALIGN 1- ?DUP IF ALIGN THEN
        1 0 D+ D2/ ;
: FSIGN FABS OVER 0< IF >R DNEGATE R>
        8000 OR THEN ;

: F+    ROT 2DUP >R >R FABS SWAP FABS -
        DUP IF DUP 0<
                IF   ROT SWAP  NEGATE
                     R> R> SWAP >R >R
                THEN 0 SWAP RALIGN
        THEN SWAP 0 R> R@ XOR 0<
        IF   R@ 0< IF 2SWAP THEN D-
             R> FSIGN ROT SWAP NORM
        ELSE D+ IF 1+ 2/ 8000 OR R> 1+
                ELSE R> THEN THEN ;

: F-    FNEGATE F+ ;
: F<    F- 0< SWAP DROP ;

( FLOATING POINT INPUT/OUTPUT ) DECIMAL

CREATE PL 3 , HERE  ,001 , ,   ,010 , ,
          ,100 , ,            1,000 , ,
        10,000 , ,          100,000 , ,
     1,000,000 , ,       10,000,000 , ,
   100,000,000 , ,    1,000,000,000 , ,

: TENS  2* 2* LITERAL + 2@ ;     HEX
: PLACES PL ! ;
: SHIFTS FABS 4010 - DUP 0< NOT
        ABORT" TOO BIG" NEGATE ;
: F#    >R PL @ TENS DROP UM* R> SHIFTS
        RALIGN PL @ ?DUP IF 0 DO # LOOP
        ". HOLD THEN #S ROT SIGN ;
: TUCK  SWAP OVER ;
: F.    TUCK <# F# #> TYPE SPACE ;
: DFLOAT 4020 FSIGN NORM ;
: F     DFLOAT POINT TENS DFLOAT F/ ;
: FCONSTANT F 2CONSTANT ;

: FLOAT DUP 0< DFLOAT ;
: -+    DROP SWAP 0< IF NEGATE THEN ;
: FIX   TUCK 0 SWAP SHIFTS RALIGN -+ ;
: INT   TUCK 0 SWAP SHIFTS  ALIGN -+ ;

1.      FCONSTANT ONE DECIMAL
34.6680 FCONSTANT X1
-57828. FCONSTANT X2
2001.18 FCONSTANT X3
1.4427  FCONSTANT X4

: EXP   2DUP INT DUP >R FLOAT F-
        F2* X2 2OVER FSQ X3 F+ F/
        2OVER F2/ F-     X1 F+ F/
        ONE F+ FSQ R> + ;
: FEXP  X4 F* EXP ;
: GET   BL WORD DUP 1+ C@ "- = TUCK -
        0 0 ROT CONVERT DROP -+ ;
: E     F GET >R R@ ABS 13301 4004 */MOD
        >R FLOAT 4004 FLOAT F/ EXP R> +
        R> 0< IF F/ ELSE F* THEN ;

: E.    TUCK FABS 16384 TUCK -
        4004 13301 */MOD >R
        FLOAT 4004 FLOAT F/ EXP F*
        2DUP ONE F<
        IF 10 FLOAT F* R> 1- >R THEN
        <# R@ ABS 0 #S R> SIGN 2DROP
        "E HOLD F# #>     TYPE SPACE ;


\ ## Future Direction and Additional tasks.
\
\ There is still a lot that could be done with this system,
\ there are many programs and extensions that could be written
\ for it. Some of them have already been mentioned.
\
\ For example any of these games could be ported to the system;
\ 2048, Minesweeper, Sokoban, Conways' Game of Life, Checkers,
\ perhaps a small chess engine, hangman, Sudoku, a rogue like
\ game similar to Hack/Nethack and more. These games all do
\ not require any non-blocking input and none of their game
\ elements require time as one of their variables.
\
\ The chess engine might seem impossible to implement in such
\ a small space, but there have been many that play on okay
\ game written for more constrained systems, see
\ <https://nanochess.org/chess.html> for more information.
\ Given 32k of code space, no doubt a decent engine could be
\ made, even if not by me.
\
\ Interactive games that would require non-blocking input and
\ perhaps a better timing mechanism include; Tetris, Snake,
\ Space Invaders, and Pacman. Programming a CHIP-8 emulator
\ would allow other, tiny, games to emulated (although the
\ slow-down of hosting an emulator on this SUBLEQ machine would
\ be horrendous).
\
\ Some of those games exist, written in Forth, in some of my
\ other projects, and some online. They are the type of game
\ that can be ported to these systems. A library of such games
\ written in a highly portable Forth dialect would be a nice
\ thing to have.
\
\ Games like these used to come as listing in microcomputer
\ magazines, usually written in a dialect of BASIC and
\ requiring only a terminal for the display of the games
\ graphics.
\
\ It would be quite easy to add in words for "free", "allocate"
\ and "resize", from
\ <http://lars.nocrew.org/forth2012/memory.html>. A simple
\ allocator only requires a handful of words and could be done
\ in two blocks of Forth code. We have seen though that you do
\ not need such an allocator package in order to produce
\ useful programs. In order to write an allocator you just need
\ a static section of memory (perhaps as little as 4KiB)
\ to divide up into sections, you do not need support from the
\ operating system or special system calls, this can be done in
\ pure Forth.
\
\ Again, links to a Floating Point implementation in Forth
\ have been given. However adding floating point words is
\ a more tasking operation, it interacts with more of the
\ system and there are design decisions that will have to be
\ made that mean one Floating Point system could differ greatly
\ from another. It could still be done in pure Forth and left
\ as an optional extra, however you must ask yourself if it
\ is worth the trouble. Forth is not good at making application
\ level programs, it is good at tinkering around with hardware,
\ with cross assemblers and the like, but anything that demands
\ floating points would be better either being rewritten to
\ used Fixed Point arithmetic, or being done in a different
\ language.
\
\ There are a number of optimizations, or just changes, that
\ could be done to the system. Removing SUBLEQ assembly
\ routines, or at least making them compile time switches,
\ and implementing the routine in Forth where doable is one
\ way to save space.
\
\ Writing an LZSS CODEC in pure Forth would be a utility that
\ could find use elsewhere, and compression routines in
\ general. As mentioned a substantial amount of room could be
\ saved if the image could be compressed during creation and
\ decompressed on the fly. For the system a tiny decompressor
\ written in SUBLEQ assembly would allow the greatest saving.
\ This might mean the compression algorithm would have to be
\ selected for execution speed on SUBLEQ with no other
\ concerns. There is a similarity between dictionary encoding
\ compression schemes, such as LZSS and the Forth concept of
\ "factoring". Factoring in Forth means finding common runs of
\ instruction sequences in words and creating a new word that
\ makes logical sense that can be applied to both words, or
\ finding the common factor of those words. Highly and well
\ factored Forth code is considered good Forth code. This is
\ quite similar to the automatic searching and replacing of
\ repeated sub-sequences in dictionary encoding.
\
\ The utility <https://github.com/howerj/ngram> can be used
\ to repeated sequences of words suitable for factoring.
\
\ An LZSS encoder for this system would best being adapted to
\ work on 16-bit data instead of bytes, perhaps using the top
\ most bit to determine whether compressed data is a run of
\ literals or a location in the dictionary. This might not
\ compress the data as well (or it might compress it better)
\ but it would be needed so byte access does not have used,
\ which is expensive under SUBLEQ.
\
\ The "recompiler", which matches known sequences of SUBLEQ
\ instructions and replaces them single instructions to run
\ on a modified virtual machine (under the "Recompiling Virtual
\ Machine" section) could also be used as the basis of
\ compression. Essentially the system is doing just that,
\ replacing sequences with known, shorter equivalents. The
\ CODEC for compression would not need to be general, much
\ like the "recompiler", it would only need to work for the
\ given, known input of this image. It would only succeed in
\ compressing the SUBLEQ virtual machine part of the generated
\ image, and not the compiled Forth section.
\
\ The main and most useful task would be the creation of an
\ MS-DOS like subsystem for this Forth.
\
\ DOS (Disk Operating System) is an incredibly
\ simple operating system, suitable for tiny systems and
\ embedded environment. It is basically a glorified program
\ loader married to an equally simple file system with a few
\ basic drivers. A DOS like operating system with a command
\ prompt could easily be implemented on top of the Forth block
\ system (and thus on top of this SUBLEQ machine). Using Forth
\ blocks would make it portable to the majority of Forth
\ implementations. Whether to implement a custom file system
\ or reimplement FAT-12, FAT-16 and/or FAT-32 would need to be
\ decided, they are all simple file systems but they are also
\ crufty. Commands for directory manipulation would need to
\ be implemented, the command line parsed, and functions for
\ opening, reading, and writing to files made.
\
\ Again, it should be emphasized that the retrieval mechanism
\ used should be Forth blocks in this DOS. If a custom file
\ system is made, it might be beneficial to create data
\ structures built around the 1024 byte block.
\
\ Making this DOS would mean the editor provided in this Forth,
\ a block editor, would need to be improved so it could work
\ on blocks that are not stored contiguously, or so it would
\ work on normal files.
\
\ Not many commands would need to be implemented; mkdir, edit,
\ chdir, pwd, rmdir, del, move, copy, rename, format, stat,
\ time, df, and a few others.
\
\ Programs could consist of Forth scripts, that could be
\ executed, with their input and output redirected to and from
\ files. Alternatively they could be SUBLEQ instructions.
\
\ A user login system for the operating system could take
\ advantage of Forth words and vocabularies. By defining a
\ word for each user name, and only having the user name
\ vocabulary loaded,and only having the user name
\ vocabulary loaded, password validation would have to be
\ done by the user name word, and the system would have to
\ take care that it could not get into a state where normal
\ Forth words could be executed without the correct password.
\
\ It would be a simple system suitable for the type of limited
\ operating system and not for serious use.
\
\ Another path for development would be to implement more
\ peripherals in the SUBLEQ machine, perhaps disk storage (the
\ above DOS would have to be stored within memory in this
\ system), a timer, and networking. All networking would have
\ to consist of is the ability to send UDP packets to another
\ SUBLEQ eForth system, which would be trivial to integrate.
\
\ For this image itself the best feature that could be added
\ would be to include a self-interpreter, which is mentioned
\ in the startup code. A correctly designed self-interpreter
\ would allow the system to execute (albeit much more slowly)
\ on systems that were the wrong bit-width, or even on systems
\ that used a different arithmetic (such as ones compliment,
\ or bignums) by simulating the correct system. Such self
\ interpreters have been written before, such as the one
\ from <https://eigenratios.blogspot.com/2006/08>.
\
\ There is a real "risk" of the eForth image being run on a
\ SUBLEQ machine that has the wrong bit-width, if that happens
\ then the image will not run (although the system does print
\ out an error message for width greater than 16-bits)
\ hampering use of the system.
\
\ Another project would be to implement a SUBLEQ machine in
\ hardware, either in 7400 series logic gates with a UART
\ for the interface and RAM chips for the main memory (loading
\ the eForth image into the RAM chip would need doing as well),
\ or making a SUBLEQ machine in VHDL and testing it on an FPGA,
\ or even going as far as to make an ASIC for the SUBLEQ
\ architecture! Making a SUBLEQ computer in 7400 series chips
\ would be rewarding and would teach the maker a lot about
\ electronics, in a way it would be more rewarding as producing
\ physical device is far more tangible than a software project
\ and there is less temptation to tinker after it is finished
\ as there is with software, in can be "complete" in a way
\ software is never complete (much like this project I
\ declared complete and after a while came back to it). The
\ initial design could be done one of the various electronics
\ simulators that are available.
\
\ There are some things that are missing in this Forth that
\ could be added, which are present in the ANS Forth standard,
\ some control structures ("case", "do" loops), the locals word
\ set which allows Forth words to create and use named
\ parameters (which would be too complex to implement), the
\ file access word-set (which requires a file system, although
\ the DOS extensions could be used as a basis to implement
\ them), words for dealing with structures (such as "+field",
\ "field:", "begin-structure", "end-structure"), and more. The
\ main aim implementing these words would be for portability
\ purposes.
\
\ What would the purpose of these little programs and
\ extensions however? A lot of effort could be dedicated to
\ this system and other Forth systems like it, for little
\ gain other than as an intellectual exercise. Code written for
\ this eForth could be ported to more viable and usable
\ systems, or this system could be a component in a virtual
\ world. One could imagine this eForth and SUBLEQ machine being
\ used to control a 3D model of a 1980s style microcomputer, or
\ to control virtual entities that could be programmed by the
\ game-player. Apart from this, eForth SUBLEQ is just a puzzle!
\
\ ## A Forth Manifesto
\
\ I deliberately eschew standards when it comes to Forth (apart
\ from the FORTH-83 and FORTH-79 standards), to me Forth is
\ best represented by eForth, or some of the long obsolete
\ Forth implementations and is only suitable for systems that
\ are well out to pasture. It represents a simpler time, one
\ that no longer exists. A time where the technology stack
\ employed by a programmer could be understood entirely by
\ them, and if there was any complexity involved it was the
\ individual at fault and not some impenetrably Byzantine mess
\ of leaky abstraction upon leaky abstraction that is
\ responsible for running internet, our operating systems and
\ the web, today (to be fair, that mess is awfully productive
\ and profitable).
\
\ So what should Forth be? To me it should be:
\
\ * Simple.
\ * Used only for Fun (and perhaps specialized niches).
\ * Understandable by one person.
\ * Not use "complex" (for Forth) data structures internally,
\ preferring the linked-list instead of a hash table for
\ dictionaries.
\ * Do minimal optimization.
\ * Usable on a 16-bit system, where Forth's home is.
\ * Forth code should not be afraid to use the capabilities
\ of the system, and non-standard ways of implementing words
\ the done thing.
\
\ I believe a lot of modern Forth implementations do not live
\ up to this, especially ones that adhere to the ANS Forth
\ specification (which does have uses, but it is not for me).
\
\ Happy hacking.
\
\ ## Licenses
\
\ The project as a whole comes with its own license, for the
\ code and generated SUBLEQ images they are released under
\ the public domain and/or the Unlicense. However the book
\ uses a LaTeX template with its own license, available from:
\ <https://github.com/Wandmalfarbe/pandoc-latex-template/>.
\

