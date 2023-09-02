# THE ULTIMATE RISCY IOCCC SUBMISSION

# USAGE

	./prog 16 eforth.dec # SLOW
	./prog 16 self.dec eforth.dec # SLOOOOOOW
	./prog 16 self.dec self.dec eforth.dec # DOUBLE PLUS SLOOOOOOW

Type "words" and then hit (or gently press) return.

The following should also work:

	: ahoy ." Ahoy there, World!" ;
	ahoy

	2 2 + . 

Using any value other than '16' (8-32 inclusive is accepted)
cause error messages to be printed out when the system is run,
although numbers lower than 16 may cause...other output as well.
Do not expect prepending the "self.dec" image to work with numbers
less than 16 as well.

Input files are stored as sequences of space delimited decimal 
numbers (ASCII only pls). A single comma may be placed after the
number as well.

# BUILD INSTRUCTIONS

To build this program from scratch:

1. First invent the universe.
2. Wait for a C compiler to evolve on a planet.
3. Capture a wild C compiler, *larger compilers are not
necessarily better*, you can only evaluate a C compiler
based off its excretions.
4. ${CC} -Wall -Wextra -pedantic -std=c11 -O3 prog.c -o prog

# PORTABILITY

There are no portability concerns, or at least am I not
concerned, it's not as if this is safety critical software
running a nuclear reactor is it? The program has only
been tested under twos compliment machines with a word width
greater or equal to 32 on Windows and Linux. It is written
in straightforward C and compiles with no warnings (on *my*
platform with the compilers I have, I don't really care about
*your* compilers because they're not warning *me*).

Unfortunately the code is not, as of yet, MISRA-C compliant.
It's something I'm working on.

This program has been tested on:

* Linux with tcc, gcc, and clang. Avoid using tcc (the code it
generates is punishingly slow, I blame the program that was fed
to it however).
  - gcc (Ubuntu 9.4.0-1ubuntu1~20.04.1) 9.4.0
  - clang version 10.0.0-4ubuntu1 / Target: x86_64-pc-linux-gnu
  - tcc version 0.9.27 (x86\_64 Linux)
* Windows (with GCC)
  - Whatever version I had lying around, use a proper OS kid!
  - With the -m32 flag as well.
 
# HINTS

* If you had to choose just one arithmetic operator, what would 
you choose?
* The "self.dec" image and the program are very similar.

# OBFUSCATION TECHNIQUES AND LAMENTATIONS

The following obfuscation techniques have been employed:

* De Morgan's Theorem.
* Alternative methods for addition.
* Alternative methods for performing logical operators.
* Lies.
* Renaming functions *may* break the program.
* Reformatting the code *may* break the program.
* Reformatting the code *may* cause it to fail to compile,
and then only with certain compilers.
* Structured programming using goto.
* Data driven programming.
* Studiously avoiding magic numbers (and strings).
* Following IOCCC best practices in relation to naming.

The big obfuscation technique is:

* What the data is. Understanding the program does not
help (much) in understanding what is an obfuscated platform
to begin with. This might make more sense with after the
spoilers.

# RETURN CODE

This program returns non-zero failure, and zero on success.

# LICENSE

Do what thou wilt shall be the whole of the Law.

Also whatever license the IOCCC requires.

# SPOILERS

	.------..------..------..------..------..------..------..------.
	|S.--. ||P.--. ||O.--. ||I.--. ||L.--. ||E.--. ||R.--. ||S.--. |
	| :/\: || :/\: || :/\: || (\/) || :/\: || (\/) || :(): || :/\: |
	| :\/: || (__) || :\/: || :\/: || (__) || :\/: || ()() || :\/: |
	| '--'S|| '--'P|| '--'O|| '--'I|| '--'L|| '--'E|| '--'R|| '--'S|
	`------'`------'`------'`------'`------'`------'`------'`------'


Avert thine eyes lest you be spoilt!

This program implements an N-Bit SUBLEQ VM, on top of that
two images are provided, one containing a complete Forth interpreter 
(most similar to the eForth variants of Forth which uses the 
"for...aft...then...next" looping construct instead of "do...loop") and
an interpreter for the SUBLEQ machine written in SUBLEQ which can
be prepended to any SUBLEQ image (even itself).

The Forth image is self-hosting, that is, it can be used to create
new Forth images (although the source for it may not be provided depending
on how mysterious I am feeling, it can be produced on request).

What is SUBLEQ? When is SUBLEQ? Who is SUBLEQ? Where was SUBLEQ
when J.F.K was shot?

SUBLEQ is the Ultimate RISC architecture (RISC-V is so hot right
now) consisting of just a single instruction. (SUB)tract and 
(B)ranch if (L)ess than or (EQ)ual to zero. There is more to it than 
this, but not much. There are many One Instruction Set Computers (OISCs)
but SUBLEQ is the most common (and perhaps the most easy to use).

Each SUBLEQ instruction consists of three operands, normally
called "a", "b" and rather inventively "c", the following is
performed:

	while (true == !false):
		var a = m[pc++], b = m[pc++], c = m[pc++]
		m[b] = m[b] - m[a]
		if (m[b] <= 0):
			pc = c;

This is enough to produce a Turing complete OISC. You will note
that this definition has no method for Input or Output and no
way of halting the machine, all of which might be considered to
be "useful" by *some* Maverick programmers.

In order to remedy this the following is usually done:

	while (pc >= 0):
		var a = m[pc++], b = m[pc++], c = m[pc++]
		if (a == -1):
			m[b] = getch()
		else if (b == -1):
			putch(m[a])
		else:
			m[b] = m[b] - m[a]
			if (m[b] <= 0):
				pc = c

Some might argue that this is no longer a single instruction
machine, but it is best not to argue with those types of people
and just get on with your life, ignore them if you can.

Real OISCs limit word length and the like, this program allows
SUBLEQ machines from 8-to-32 bit widths inclusive to be simulated.

You will note that the machine lacks and capability to:

* Perform indirect loads or stores.
* Do any bitwise operation.
* Obviously lacks any capability to perform multiplication
or division.
* Cannot address individual bytes.
* Mixes up subtraction, comparison, loading and storing
into one single instruction.
* Cannot do crazy things like MOV, at least not directly.

Some of things things can be achieved with self-modifying
code (which is used a lot), for example you can perform an 
indirect jump by modifying the jump destination with another 
SUBLEQ instruction. Indirect loads and stores are more
complex.

Broadly the strategy to implement the Forth interpreter on
such a spartan machine is the following:

1. Implement a virtual machine.
2. Make a few primitives for that virtual machine (in our
case some bitwise operators, arithmetic, conditional
jumping, cooperative multitasking instructions, I/O,
stack manipulation instructions, and some more).
3. Ignore the SUBLEQ machine and just target the VM we
just made, which is much, much easier.

The most difficult part of this task is implementing the
bitwise operators (and it turns out decent signed/unsigned
comparison operations as well).

In order to implement the bitwise operators (AND, OR, XOR,
INVERT, LEFT/RIGHT SHIFT) in the Forth image we can use the
property of signed twos compliment numbers that if the top
bit is set then the number is negative, this allows us to
to use the Less-Than-Or-Equal-To-Zero property of SUBLEQ
to test if that bit is set (we have to make sure the number 
is not zero at the same time). To save space in the target
image we just implement a multiplexing and a shift routine
from the bitwise routines.

About a third of the image is taken up by Forth VM primitives,
each OISC instruction takes up three machine words or 6 bytes,
and a single indirect load takes about 8 instructions. This
means speed and size are both concerns.

The Forth is fairly fully featured (and larger images can be
supplied that include more control structures, ALLOCATE/FREE,
better decompiler, and a floating point word-set (which uses 
someones code from the 1980s to implement the basic FP words, most 
of the transcendental functions I implemented myself)).

The following is implemented:

* A block editor, available in the word "editor", along with
a fake block word-set.
* Double cell input parsing.
* The vocabulary word-set is implemented.
* Super primitive decompiler in "see", if it can be called that.
* Cooperative multi-threading (along with "user" variables).
* A few hundred words.

Some limitations of this Forth:

* "page"/"at-xy" use ANSI terminal escape sequences.
* This Forth is case sensitive and uses lower case words.
* Numeric bases higher than 9 use upper case letters only.
* "ms" uses a calibrated delay loop. Calibrated to work on my
machine.
* "do...loop" and associated words are missing.
* The heat death of the universe will eventually render all
efforts useless.

