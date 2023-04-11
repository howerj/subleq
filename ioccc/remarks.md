# prog.c

To run:

	./prog 16 data.dec

The second argument may be between 8 and 32 inclusive, but
you'll only get a useful system when it is 16 (an error
message is printed out if it is not 16).

Once it is running, type "words". 

This:

	: hello ." Hello, World!" cr ;
	hello

Works as well.

The program will return zero on success and non-zero on
failure.

# Hints

* It should work fine without optimizations, but it helps
to have them on.
* If you had to chose an arithmetic operator, which would be your 
favorite?
* What the program does it actually *really* simple, however
that is only (much less than) half the battle in understanding 
the system, focus instead on the data.

# Portability and Features

* For the full range of features the int type should be at
least 32-bits in length, a 33-bit int should be fine. If "int"
is 16-bit, then the first argument should be restricted to be
less than or equal to 16.
* Twos compliment machines only guys, pls.
* Tested using clang/gcc/tcc on Linux/Windows and 64/32 bit. It
should be pretty portable.

# Banner indicating spoilers are ahead, so beware lest you be spoiled:

	.------..------..------..------..------..------..------..------.
	|S.--. ||P.--. ||O.--. ||I.--. ||L.--. ||E.--. ||R.--. ||S.--. |
	| :/\: || :/\: || :/\: || (\/) || :/\: || (\/) || :(): || :/\: |
	| :\/: || (__) || :\/: || :\/: || (__) || :\/: || ()() || :\/: |
	| '--'S|| '--'P|| '--'O|| '--'I|| '--'L|| '--'E|| '--'R|| '--'S|
	`------'`------'`------'`------'`------'`------'`------'`------'

# **SPOILERS**

The system contains a complete working Forth image, given
the paucity of instructions in this RISC-V (RISC-V is *so*
hot right now) contender the Forth image really has to be
creative in how the operators are implemented (including
the bitwise operators! You try doing AND with subtraction
and see how far you get!).

## Obfuscation techniques

The following obfuscation has been used:

- There are no numbers, magic or otherwise, used in the code.
- Replacing common bitwise operators with equivalents based on
multiplexing, despite the theme multiplexing is my favorite
operator.
- Using alternative methods for addition.
- Misleading comments (Someone wouldn't do that would they?
Go on the internet and lie?).
- Using the IOCCCs best practices when it comes to formatting
and variable naming.
- Structured programming using goto.
- Data driven programming.
- Why use equality when XOR will do?
- Synthesizing constants, and doing so with "\_\_LINE\_\_" and 
"\_\_func\_\_".

But the big one is:

- Obfuscating what is actually a really dead simple program,
deobfuscating gets you nowhere, you still do not have any
idea about how anything works unless you reverse engineer
the data as well (which was not trivial to produce!).

I toyed with the idea of filling the screen with a single
dense blob and going crazy with it, but I think it is better
as it is.

# **MAJOR SPOILERS**

The program implements an N-bit SUBLEQ machine, SUBLEQ is
a One Instruction Set Computer that is Turing Complete
(given the usual infinite memory blah blah blah). The
upshot is, if it is Turing complete and exists you can 
port a Forth interpreter to it, and here is a partial 
existence proof of that!

The Forth interpreter is split into two parts, a virtual
machine built up using only SUBLEQ (which necessitates
using self-modifying code to do crazy things like, oh,
I don't know, MOV, or indirect jumping), and the Forth
code itself. Decompiling the Forth code should be trivial,
but the underlying VM that is built much more difficult
to reverse engineer unless you *really*, *really*, like
subtraction.

As to how the bitwise operators are implemented, there 
are two main algorithms, both of which aim to test a
single bit. You can either implement the modulo operator
and do a modulo 2 to test the least significant bit, to
shift bits you then divide by two (implementing division
using subtraction should be fairly easy), or you can
test something is negative and this test the top bit
(and be careful to account for numbers that are all zero)
whilst using addition to double numbers and thus shift
bits into the highest bit position, which is far more
efficient but imposes more restrictions on the SUBLEQ
machine (for example, you cannot use arbitrary precision
arithmetic and must settle on a machine width to target,
you must also settle on a numeric representation for signed
numbers, which in this case is twos (sic) compliment).

The source code for the (self-hosting-) Forth interpreter can 
be produced on request, but I think the entry is nicer without 
it as it preserves an air of mystery and simplicity to it.

