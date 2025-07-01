# A FORTH TUTORIAL by RICHARD JAMES HOWE
# 2025 Jun 22nd

This small document contains a tutorial for SUBLEQ
eFORTH, it should be enough to help you understand how
the meta-compiler works. SUBLEQ eFORTH is available
at <https://github.com/howerj/subleq> and can be tried
online at <https://howerj.github.io/subleq.htm>. There
book that describes the Forth internals is available at
<https://www.amazon.com/SUBLEQ-EFORTH-Forth-Metacompilation-Machine/dp/B0B5KV548D>.

**TODO: Talk about**:

* Add questions, answers and exercises. This tutorial 
really needs more examples and problem sets.
* Global hyper static environment; Forths usage of globals
as well
* How do you get good at juggling stack items (answer;
practice, factoring, and you do not as it is difficult to
do which is why no one else does it).
* Good Forth practice; factoring
* Forth sucks at data structures, lisp has lists, Forth
has...?
* The highest tier of any pyramidal structure of knowledge,
topologies of ignorance, also Alpha Centauri quotes.
* Charles H. Moore
* Forth lore, C2 Wiki, comp.lang.forth.
* Forth wasn't discovered like some grandiose mathematical
or scientific theory.
* Speed and optimizations in Forth.
* C Forths Vs Assembly Vs meta-compilation.
* Introspection; see, dump, ...
* Forth execution model, diagram
* One does not want to use Forth, sometimes one has to,
much like multicore systems or FPGAs, or GPUs. (use example
of sound card going away).
* `words`, go through most common words.
* It might be nice to make some
example applications, such as converting
<https://github.com/izabera/bitwise-challenge-2048>,
or something else. Perhaps we could make an assembler.
Making an assembler for Forth and compiling actual exes
would neat.
* Missing is a really basic introduction to programming
for non-programmers. Starting with binary numbers, what
is computing and what is a computer, ...
* Interesting Forth resources; Zepto Forth TCP/IP, 
j1eforth, SUBLEQ eFORTH and my other Forths, ...

## Why learn Forth?

	Forth is Sudoku for programmers.

If this was the 1980s and the only exposure to programming
was a microcomputer running BASIC from a ROM then you
could quite reasonably say that Forth was the future of
computing. It is however not the 1980s, and for whatever
reason you have decided to pick up Forth. There are many
reasons to learn Forth, but not many practical ones,
whilst Forth has been used in industry its usage to
accomplish actual tasks of utility wanes each year. It
is often joked that there are more Forth implementations
than there are applications, this actually most like true,
hence the saying "If you have seen one Forth, you have
seen one Forth".

The previous paragraph was perhaps overly dismissive,
too harsh, but it is important not to lose sight of the
truth and promise things that cannot be delivered. Not
everything has to be done to maximize utility, the saying
that "Forth is Sudoku for programmers" is perhaps a more
apt one. Learning Forth for fun is a perfectly valid
reason to do so. It is certainly a language and system in
which one individual can master and understand completely,
to which there is a certain satisfaction from doing so.

That said, there are some areas, ecological niches, in
which Forth shines:

* Creating assemblers for unique systems.
* Understanding how a language is implemented.
* To broaden ones horizons, learning a language that
differs greatly from the mainstream C genealogy, languages
like APL, LISP, Prolog, fit this mold, letting you express
your thoughts in different ways.
* When incredibly compact code is needed that takes up
very little space (a Forth system can be made to fit in
kilo bytes of space).
* To interact with hardware directly.
* Nostalgia, or borrowed nostalgia if one did not live
through the microcomputer era, for a simpler time.
* As an artistic endeavor, to create programs and thoughts
in a deliberately constrained and contrarian manner.
* Forth is great at creating new Forth implementations.

Whilst it is possible to run Forth under a hosted
environment like any other programming language Forth
really feels like it belongs in a 16-bit environment,
an environment whose borders recede every year even in
the embedded programming space where 32-bit RISC cores
are now the norm.

Some people feel a sense of enlightenment when learning
how Forth is implemented, you are less likely to do so
if you know how compilers and interprets work, or if
you have implemented another programming language from
scratch yourself.

## What is Forth?

Forth is a stack oriented procedural programming language,
it features an incremental compiler that can be interacted
with, and uses Reverse Polish Notation (RPN) to enter
numbers and commands. It has been likened to an interactive
macro assembler, which does not quite capture what Forth
is, but has some truth to it.

It is a very simple language, much simpler than C, or Java,
or Haskell.  Simpler does not mean better, and by no means
is that an objective descriptor, however the weight of
the system as measured in bytes often speaks for itself,
a Forth system can fit comfortably, with no obfuscation or
missing features, in a dozens of kilobytes. SUBLEQ eForth
for example fits into about 20 kilo bytes with all options
enabled, and about 13 kilo bytes in the base system.

It is a language that is not memory safe and lacks
guardrails, it is very easy to break the system, relying on
programmer discipline and the reset button. This of course
negatively affects programmer productivity and program
correctness, the simplicity has a cost. It is often easier
to get programs running and correct in a language like C,
which is fraught with error, than in Forth.

Forth is often dismissed as a "Write Only Language", much
like Perl, one in which it is possible to create only
inscrutable programs that cannot be read by anyone else,
or frequently cannot be read by the very programmer that
wrote them in the first place.

To a certain extent this is true, there are features of
the language that make it difficult to read code such as
the lack of explicit named function parameters (which also
make Forth so compact). It is also in part because Forth
is very different from every other programming language out
there. It shares very little heritage with other languages
so there is no reference point that a programmer can use
as way to understanding Forth programs.

Generally speaking people do not want to program, they want
to solve problems they have and programming gets in the
way of that. They do not care of supposed enlightenment
acolytes achieve upon mastering an esoteric system, they
want results. If you are more interested in results of the
computation than the computation itself then you should
avoid Forth, there are more mainstream tools that can
achieve that for you.

If you have managed it this far, my ramblings have not
put you off. I shall continue on to do what the title of
the document claims it will do, act as a Forth tutorial.

## Forth Basics and Hello World

Some facts about Forth should be kept in mind when dealing
with Forth.

1) Forth is space delimited, although it has no fixed
grammar.
2) Forth programs are entered a line at a time.
3) Numbers and commands are entered using Reverse Polish
Notation.
4) Forth is a procedural language, it is possible to do
functional programming, it is not a strength.
5) Forth has two stacks. A data stack and a return stack,
which unlike in most programming languages is directly
interacted with.
6) The interpreter can either be in `compile mode` or
`interactive mode`.
7) There are several Forth standards (FORTH-79, ANS Forth,
and more) but each Forth is unique.
8) Forth features an incremental and interactive compiler.
9) Forth operates in a global hyper static environment, new
re-definitions of functions and variables do not replace
old ones and functions which used those old definitions
continue to do so when new definitions are entered.
10) Forth is untyped and is not memory safe.

Most Forth implementations are case insensitive (and hail
from an ASCII only era). SUBLEQ eFORTH is case sensitive.

The consequences of these bullet points is not immediately
apparent, and can only be understood by interacting with
a real system. It is often a mistake to think that just
because you have all of the facts about a thing you can
immediately deduce all consequences of those facts for
free and immediately.

There are several differences in nomenclature between
Forth, which formed in the more insular microcomputer
era, and the mainstream computing culture as derived from
academia and industry.

For example the following terms in Forth have their own
meaning:

* `double`: This does not refer to double precision
floating point numbers, for the early history of Forth
floating point functionality was not present in the
language, as it was not present in, nor capable of
being run by, the 8-bit systems to which Forth grew up
in. Instead this refers to double width integers, on a
16-bit system, a `double` would be a 32-bit integer.
* `word`: To most programmers, especially those familiar
with assembly or low level programming details, a `word`
is the natural machine width, on a 32-bit machine, a word
is 32-bits, it can also mean a 16-bit value, and `DWORD`
would be a 32-bit value. It is context dependent. In a
Forth, `word` is the name used for a function.
* `cell`: As `word` has already been taken, `cell` is used
in its place, this can refer to the natural machine width,
or on an 8-bit machine to a 16-bit value.
* `dictionary`: The sum total of all functions currently
compiled in the Forth system is called the `dictionary`,
called so because each function is known as a `word`. It
is usually implemented as a linked list.
* `meta-compiler`: This is a cross compiler written in
Forth that is used to create a new Forth, instead of the
usual meaning.
* `turn key`: A method for generating Forth programs or
systems that execute a given `word` on start up instead
of dropping into a prompt, it needs a term as there is no
standard method to do this, a deficiency of Forth.
* `compiler security`: methods which may or may not
be present depending on the Forth implementation
for performing basic sanity checks on programs, such
as checking that each "if" construct has a matching
"then". Taken for granted in most programming languages,
they were some times not implemented for size and
performance reasons, or just carelessness.

### Hello World

It is customary for the first program written in a tutorial
to be a "Hello World" program, a simple program that when
run prints out the string `Hello World`. Here is is in
all of it's glory:

	: hello cr ." Hello, World" ;

Once entered, to call the function type `hello` and hit
return. Note that Forth is a space delimited language,
every space matters, the following constructs will not
work, or will print out something slightly different:

	: hello cr ."Hello, World" ;
	:hello cr ."Hello, World";
	:hello cr ." Hello, World";
	: hello cr ."  Hello, World" ;

Going back to the working definition:

	: hello cr ." Hello, World" ;

The components of this definition is as follows:

1) `:` is a defining word, it takes a word from the input
stream and creates a new header in the dictionary for that
word, it is not yet linked into the dictionary.
2) `cr` prints a new line.
3) `."` parses the input stream until a `"`, compiling a
string into the dictionary which when the function is run
will print out the string. `"` cannot be escaped so `"`
cannot appear in a string, also the maximum string length
in SUBLEQ eFORTH (and many other Forth implementations)
is 255 bytes in size.
4) `;` ends the function definition, it is an immediate
word, it switches the interpreter back into command (or
interpret) mode and links the word definition into the
dictionary so it can be called. Until this is done the
word will not appear in the dictionary, and if there is
another word called `hello` then this version will be
called instead, even within the new definition.

### Forth's execution model

Forth has a simple execution model in which a line of text
is entered, then the line is split up from left to right
and as soon as a space delimited word is encountered it is
executed with semantics that depend on the compiler mode
and the type of word it is (whether it is immediate or
not). If an error is encountered the rest of the line is
discarded, along with some other error handling, if not an
"ok" is printed out after a successfully executed line if
the interpreter is in command mode.

You should type in the lines of code that do not work and
see what failures you get. The final line should compile,
but will print out an extra space preceding the "Hello,
World" string.

The Forth word `."` is a word, an ordinary function, it is
not *syntax*. In a language like C strings are syntax that
is part of the grammar for that programming language. In
Forth strings are words, ordinary functions, as are
comments, and "if" statements, and loops, and function
definitions. This should be taken as is for the moment,
the why and how will come later.

Next, let us perform some basic addition, to add two
numbers and print the result enter the following and
hit return:

	2 2 + .
	
This is equivalent to the expression "2+2". This method
of entering number is known as Reverse Polish Notation, or
RPN, it has been used by some historical and early pocket
calculators but has largely been forgotten about. It is
much simpler to implement and requires fewer resources
to parse and store partial expressions. There is also no
operator precedence rules to remember, or to implement.

The expression "2+(2\*3)" can be entered as so:

	2 3 * 2 + .

The rules for parsing lines and then words still apply
here. If you enter `2+` you will most likely get an error
in your Forth, `2+` is not the same as `2 +`.  Numbers of
usually handled as a special case, if after parsing a word
and that word is not found in the dictionary an attempt
is made to treat it as a number, if the number conversion
succeeds in the current input radix it is treated as
a number, what is done with that number depends on the
interpreter state (if in compile mode a number is compiled
into a word definition, if in command mode it is instead
pushed on to the variable stack).

As mentioned, Forth has two stacks, the stack you will
interact with the most is the variable (or data) stack. The
return stack is used for function calls and loop variables,
it will be discussed later. Data can be passed to and from
each stack, but the data stack will be used far more by
the programmer.

An output radix, or base, was mentioned. The base to which
numbers of entered or output can be changed by manipulating
the global `base` variable (it might be a thread local
variable in your Forth). Some Forth implementations default
to hexadecimal, but most use decimal.

By default the interpreter starts up in command mode,
when you enter a number it is pushed to the data stack,
when you enter a word it is executed. So when the line
`2 3 * 2 + .` is entered, the following happens.

1) `2` is pushed to the data stack.
2) `3` is pushed to the data stack.
3) `2` and `3` are popped off of the data stack, they are
multiplied together and the result is pushed to the data
stack. The order they are popped off does not matter for
a multiplication, but `3` would be popped off first before
`2` as it was pushed last, Last In First Out, LIFO.
4) `2` is pushed to the data stack.
5) `6` is popped off of the data stack along with `2`,
the two numbers are added together and the result is pushed
to the data stack.
6) `8` is popped off of the data stack and is printed out
as `.` prints out the top of the data stack. It does so
in the current output radix, as specified by `base`.
7) `ok` is printed out and the next line is read in.

Negative numbers can be entered in the usually fashion,
`-2` for example, again whitespace matters, `-2` is not
the same as `- 2`.

It is worth mentioning what happens when the data stack
is empty, or lacking the requisite number of variables
for a given operation. It entirely depends on the Forth
implementation, some Forth implementations will issue an
error, others will perform the operation with junk values,
some may only do error checking at certain points or when
executing specific words. It is up to the user to ensure
that operations are correct and have enough arguments.

As mentioned, numbers are treated as a special case by
the interpreter, but are parsed after words, there is
nothing preventing the user from redefining a number as a
function. This might not usually be a problem, but what
is a word or a number depends on the input radix! For
example `dead` might be a word, but it is also a valid
hexadecimal number.

Forth implementations usually allow hexadecimal numbers
to be specified directly by prefixing them with `$`. Bases
2-36 are usually valid. SUBLEQ eFORTH requires all digits
above 9 to be in upper case, as lower case is used for word
definitions (and SUBLEQ eForth is a case sensitive Forth).

The floating point word set is optional within Forth, it
does not have to be implemented by a Forth interpreter,
this is due to origins of Forth on limited systems that
lacked floating point hardware support and with limited
flash available. SUBLEQ eFORTH has a floating point word
set that can be enabled, but requires a new image to be
generated (there are also some core words that optional
in SUBLEQ eFORTH, such as the `do...loop` mechanism).

It is up to the Forth implementation as to whether the
floating point numbers, if implemented, are placed on
their own floating point stack, distinct from the data
and return stacks, or if they take up space on the data
stack. In SUBLEQ eFORTH they are placed upon the data
stack, and as it is a 16-bit Forth the floating point
numbers take up 2 slots on the data stack. This makes
their usage quite tricky.

More common in Forth than using floating point numbers
is using double cell, integers numbers that are twice the
width of the normal Forth numbers, which are at a minimum
16-bits in size (thus a double cell number is 32-bits in
size in a 16-bit Forth). As Forth grew up on microcomputers
using 16-bit arithmetic as default even on 8-bit machines,
the need for calculations with greater precision was always
apparent. There was more need to enter double cell integers
than floating point numbers, thus there is a syntax for it.

The expression:

	2.1

Does not push a floating point number on to the data stack,
it instead pushes a double cell number on to the data
stack, occupying two slots. `dpl`, a variable, is set
after each number is entered and it contains `-1` when
a single precision number is entered and a non-negative
number when a double precision number is entered.

We will introduce a few new words now, so we can inspect
what happens when various numbers are entered. We have
already encountered `.`, for popping the top of the
data stack (often just referred to as "the stack") and
printing it.

`dpl` as mentioned is a variable, we can access the
contents of a variable with `@`, called "at" or "load". `@`
takes the contents of the top of the stack and treats it
as an address, it replaces the top of the stack with the
contents it loads from that address. Most implementations
of `@` do no bounds checking, if you provide it with an
invalid address, `@` will attempt to load it, this may
cause a segmentation or bus fault, or load nonsense,
or trigger a hardware read, if you provide it with
an invalid address depending on your system and Forth
implementation. `.s` can be used to display the contents
of the data stack, it does not empty the data stack and
leaves the items as they are, unlike `.`. To drop an item
from the stack without doing anything we can call `drop`.

Try running the following code:

	.s
	1 2 3 .s
	drop .s
	drop drop .s
	2.1 .s dpl @ . drop drop
	2.10 .s dpl @ . drop drop
	2.01 .s dpl @ . drop drop

There exists a corresponding set of operators for dealing
with double cell, or double precision, numbers. If `+`
adds two numbers, the word `d+` if present in your Forth
will add two double cell numbers, like wise for `*` and
`d*`. `s>d` can be used to convert a signed single cell
number to a double cell one, and `d>s` can be used to
convert a double cell number back into a single cell one,
albeit with the possibility of losing information.

You can check what words are available within your Forth
by typing in the word `words`, you will see a list of
words like so:

	cold editor quit load evaluate set-input
	get-input list blank block buffer empty-buffers
	flush save-buffers update b/buf at-xy page
	bell ms [if] [else] [then] defined dump see
	compile-only immediate postpone \ .( ( abort"
	$" ." exit rdrop r> >r marker does> >body user
	constant variable create next aft for else repeat
	while then again until if begin recurse ' :noname
	: ; [char] char word definitions +order -order
	(order) get-order interpret compile, literal
	compile find search-wordlist cfa nfa compare
	.s number? >number . u. u.r sign <# #s # #>
	hold parse -trailing query tib expect accept
	echo / mod /mod m/mod um/mod * um* d+ dnegate
	um+ abort throw catch space erase fill cmove
	type +string count c, , allot align aligned
	source 2r> 2>r 2@ 2! source-id min max c! c@
	lshift +! pick set-current get-current cr emit
	key key? ?exit execute cell- cells cell+ cell
	abs s>d negate within u<= u>= u> u< <= >= 0>=
	0< > < 0<= 0<> 0> <> = 2dup 2drop -rot rot r@
	?dup tuck nip [ ] decimal hex rp! rp@ sp! sp@
	here bl span >in state hld dpl base scr blk
	context #vocs pad this root-voc current <ok>
	! @ 2/ and or xor invert over ) 1- 1+ 2* 0=
	rshift swap drop dup bye - + eforth words only
	forth system forth-wordlist set-order

Many of these words are standard, and only a few
non-standard words, are defined in the default vocabulary
in SUBLEQ eFORTH. Some words you will be able to ascertain
what they do from their name, for example `min` and `max`
will most likely find the minimum and maximum number if
given two numbers, and they do, you might not know how to
use them yet.

This brings us on to the topic of Forth naming conventions.

### Forth naming conventions: Part I

In the list of words you will find names of functions
that might be obvious what they do, and others that will
require knowledge of Forth, you might also notice some
patterns. For example `u` often appears as a prefix, along
with `2`. `\>` and `\<` appear quite often as well. These
pre- and post- fixes are part of a Forth naming convention
that is not always followed. There is also a convention
when it comes to the comments in word definitions. These
conventions will start to make sense when we see these
words in use, at the moment we will just note that they
exist and we will come back to them later and describe
them in detail.

In the next subsection we will describe words that
operate on the stacks. In a sense Forth more than other
languages such as C is defined as a language by the
functions which compose it. In a language like C there
is syntax and the ability to define new functions, you
can start with a completely blank slate, no functions
are needed, you can write your scaffolding in C and the
C compiler will compile it. Instead with Forth you start
with a set of pre defined functions interacting with an
interpreter loop and build up new functions with them,
a set of primitive functions forms the kernel of Forth
upon which everything else it built. You can technically
understand C without reference to the C standard library,
that is not the case in Forth. It is a mistake to think
that the starting functions in Forth are key words however,
in Forth there is no such thing as a key word, as there
is in C or Java, not even `if` is a reserved word.

### Stack Words

We have encountered words that use items on the stack,
such as `+`, `*`, and `.`, we have also seen a word that
inspects the stack, `.s`, and we have seen one word so far
that has an operation that is used purely for its effect
on the stack, that is `drop`. There are more stack words,
you must become intimately familiar with them when writing
Forth code, using them gets easier with practice, however
there is a reason nearly no other programming language
exposes the stacks they use internally as Forth does,
whilst it gets easier to manipulate the stack with time, it
is still difficult to do so. There are methods for coping,
but that is all they are, a coping mechanism for an error
prone method of dealing with data.

Now on to the first batch of words; `drop`, `dup`, `swap`,
`nip`, `over`, `rot`, and `-rot`. Their behavior can be
described entirely by the effect they have on the data
stack, they have no other side effects (apart from in error
conditions when not enough data is supplied to them, and
error checking for stack depth is available in the Forth
you are using).

* `drop`: Drop the topmost stack item.
* `dup`: Short for duplicate, duplicates the topmost stack
item leaving two copies of it on the stack.
* `swap`: Swap the top two items on the data stack.
* `nip`: Remove the second item from the stack, leaving
the topmost item.
* `over`: Copy the second item on the stack over the
first item.
* `rot`: Rotate the topmost three items on the stack.
* `-rot`: Rotate the topmost three items on the stack in
the opposite direction to `rot`.

As before, we can enter numbers and inspect the
results. Try the following, note that comments in Forth
are placed between `(` and `)`, a space must come after
`(` as it is a word, a plain function:
  
	.s         ( This should display no numbers )
	1 drop .s  ( This should do the same )
	1 dup .s   ( Displays `1 1` )
	1 2 drop . ( Displays `1` )
	1 2 .s     ( Note the order of `1 2` )
	1 2 . .    ( Note the order of `1 2`, `2` first )
	1 2 nip .  ( Displays `2` )
	1 2 over   ( Displays nothing yet... )
	.s         ( Displays `1 2 1` )
	drop drop drop ( Get rid of those numbers... )
	
Note in my description of `rot` and `-rot` there is
something missing, I have not described the direction to
which the topmost numbers are rotated. To determine that
you can play around with the words yourself. If you enter
`1 2 3` what will the effects be when you call `rot` and
display the results with `.s`, what about `1 2 3 rot rot`
versus `1 2 3 -rot`, or `1 2 3 rot . . .`?

Do not spend too long on this, but try to understand what
they do, you will memorize what they do through repeated
use if you decide to write any large program in Forth.

Previously it has been mentioned that there are two
stacks present in Forth, it is one of Forth's defining
characteristics. Whilst ordinary data is passed
to functions, words, in Forth via the data stack and
returned via the data stack a second stack, called the
return stack, is also present. Naturally there is a set of
words to manipulate the return stack, however those words
should only be use within a word definition and never in
command mode.

In SUBLEQ eFORTH if you attempt to use one of these words
in command mode, that is not in between `:` and `;` which
are used to define a function, you will get the cryptic
error message `-14?` displayed. You may have encountered
other such error numbers before, as space is limited in
SUBLEQ eFORTH, and in many Forth implementations of the
past, a set of standard error codes was agreed upon, such
as `-13` for an undefined word, `-4` for stack overflow,
and `-14` for executing a word that should only be used
at compile time, that is within a word definition.

The words are:

* `\>r`: To-R, this word moves a value from the data stack
to the return stack.
* `r\>`: From-R, this word moves a value from the return
stack to the data stack.
* `r@`: R-at, this value copies a value from the return
stack to the data stack, leaving the value it copies
in place.

Using these words must be done with caution, it is possible
to break the system by using them incorrectly quite easily.

As mentioned the return stack is used to store loop
variables (in an implementation defined way) and the
return value of function, or word, calls.  If you remove
the return address or replace it with the result of a
calculation then it is incredibly likely that you will
break the system. The reason these words can only be used
within word definitions and not in command mode is that
after a word like `\>r` is executed in command mode the
return address is immediately needed as it is being used
by the command loop, you cannot balance the usage of `\>r`
and `r\>` as you can within a word definition, and if you
are using those words you must do that - restore the return
stack to its original state before the word exits.

The primary uses for these return stack words is; using
the return stack as a temporary store for variables,
fetching loop counter values, and in some implementations
they are used in a non-portable manner to manipulate
where the program returns to (for example, exiting to the
callers caller).

For the moment, you should just use these as a temporary
store of variables if you have trouble juggling values on
the data stack.

Some examples:

	: .. >r . r> ; ( display the second value )
	: ... >r >r . r> r> ; ( display the third value )
	: ret? r@ . ; ( displays `ret?` return value )

Notice that when using `\>r` and `r\>` the number of them
is balanced, for each `\>r` there is a corresponding `r\>`.

Internally some of the stack words are defined in terms
of simpler ones, such as the following:

	: over swap dup >r swap r> ;
	: rot >r swap r> swap ;
	: tuck swap over ;
	: nip swap drop ;

Some questions for you: Is it possible to implement `over`
without the use of a second stack just using the words
`swap`, `dup` and `drop`? Is it possible if we were to
use a temporary variable for storage?

`pick` and `roll` are two words that allow powerful
manipulations of the data stack, but they are to be
avoided, instead you should try to structure your code so
you do not need them, which takes practice. Their usage
indicates that you are not writing very "Forth-like" code,
and the code is what is called poorly factored. Factoring
is the art of turning a Forth word definition into smaller,
preferably one line, easier to understand words that
can be used else where and have a descriptive name. It
is not always possible to do however, so you might have
to reach for these words. If you are juggling more than
three items and the stack it is a hint that you *might*
be doing something wrong, but it is not a hard rule.

`pick` selects an arbitrary item on the stack, and `roll`
rotates n-items on the stack. `pick` starts its count at 0,
0 is the first item on the stack, 1 is the second item on
the stack. Instead of a full description of both words,
why not try to use them:

	1 2 3 0 pick .
	1 2 3 1 pick .
	1 2 3 2 pick .

SUBLEQ eFORTH does not define `roll` by default, in some
Forth implementations, such as those that use hardware
stacks, it might be difficult to access and manipulate the
stacks. If `roll` has not been defined, you can define it
as so:

	: roll ?dup if swap >r 1- recurse r> swap then ; 

This uses `?dup`, `if`, `then` and `1-` (not `-1`!) which
we will encounter later. To perform recursion you must
use the `recurse` word instead of calling `roll`, this is
needed because the definition of `roll` is not linked into
the dictionary until `;`.

Try the following with either your systems defined version
of `roll`, or the one above.

	: 3drop drop drop drop ;
	1 2 3 0 roll .s 3drop
	1 2 3 1 roll .s 
	1 2 3 1 roll .s 3drop
	1 2 3 2 roll .s 
	1 2 3 2 roll .s 
	1 2 3 2 roll .s 3drop

What is `1 roll` equivalent to? What is `2 roll`
equivalent to?

`3drop` is defined as a convince word to empty the stack,
to prevent it becoming cluttered.

`3drop` is an uncommonly defined word, more common are
words that begin with the `2` prefix. If we wanted to
define a word that dropped two items from the stack,
we could define a word like so:

	: 2drop drop drop ;

`2drop` is usually part of the standard library. `2swap`
and `2dup` are also defined. `2swap` takes four items on
the stack an swaps the topmost two with the bottom most
two, `2dup` takes two items on the stack and duplicates
those two items. If you wanted to triplicate the topmost
stack item a rare word called `trip` is sometimes defined
as `: trip dup dup ;`, its usage is uncommon.

You should take the time to inspect how these words work:

	1 2 2dup .s 2drop .s 2drop
	1 2 3 4 2swap .s 2swap .s 2drop 2drop .s

Given the behavior of `2drop`, `2swap` and `2dup`, what
do you think `2nip`, and `2over` do? What about `2\>r`
and `2r\>`? How would you use them, and what would their
stack effects be?

How could you implement `2swap`, `2dup`, or `2over`
yourself? Try to implement them before looking at the
source code for them, you should be able to implement
them without using any temporary variables, just using
the words previously shown for manipulating both stacks.

### Arithmetic and Bitwise Words

You have encountered the arithmetic words `+` and `*`,
also defined are `-`, `mod` and `/`, there are several
bit wise operators such as `and`, `or`, `invert`, `xor`,
`lshift` and `rshift`. Forth hails from a time when there
was more experimentation with the very basics of computing
which we take for granted, such as whether or not to use
twos compliment arithmetic, much like C.

Not only this, there are different ways in which operators
like `mod` can be defined when it comes to taking the
modulo of negative numbers.

Whilst it should be clear what the operators from their
names, it will be spelled out for you. Note that some
operators require that their operands are in a certain
order, such as `-`, `/`, `mod`, `lshift`, and `rshift`,
whereas the rest do not. `invert` takes only one argument.

* `+`: Add to numbers together.
* `-`: Subtract the first item on the stack from the second.
* `*`: Multiply two numbers together.
* `/`: Divide the second item on the stack by the first.
* `mod`: Perform the modulo operator by dividing the second
item on the stack by the first item.
* `and`: Perform a bitwise AND of two items on the stack
and push the result.
* `or`: Perform a bitwise OR of two items on the stack
and push the result.
* `xor`: Perform a bitwise Exclusive OR of two items on
the stack and push the result.
* `invert`: Perform a bitwise invert on the topmost item
on the stack.
* `lshift`: Perform a left shift of the second item on
the stack by the number of places indicated by the first.
* `rshift`: Perform a right shift of the second item on the
stack by the number of places indicated by the first. This
zero fills from the left.

Some Forth specific operators are:

* `2/`: Divide the topmost item on the stack, depending on
your Forth this might be implemented in multiple different
ways, one is for this to be equivalent to `1 rshift`
(as SUBLEQ eFORTH defines it as), another is `2 /`, which
has different behavior when it comes to negative numbers.
* `2*`: Multiply the topmost item on the stack by two.
* `1+`: Increment the topmost item on the stack. 
* `1-`: Decrement the topmost item on the stack.

They are easy to define yourself, but are such common
factors that they are often defined.

Some uncommon operators are:

* `arshift`: An arithmetic right, similar to `rshift`,
this fills in ones from the left when shifting right if
the topmost bit is set, otherwise it fills in zero.

Note that some of these operators deal with signed values,
others it does not matter if the numbers are signed or
not. The sign matters for `mod`, `/`, `arshift`, and
depending on the implementation, `2/`.

We can try out the operators, but do not expect anything
unexpected!

	8 2 - .
	-1 -3 - .
	9 3 / .
	9 4 / .
	-1 $A5A5 and .
	$A5A5 $5A5A and .
	$A5A5 $5A5A or .
	$AA55 $5A5A xor .
	1 4 lshift .
	4 1 lshift .
	8192 2 rshift .
	2 8192 rshift .
	
One useful operator we can define is `mux`, short for
multiplex, which like NAND and NOR gates is a universal
function (so long as a source of truth and falsity is
provided).

	: mux dup >r and swap r> invert and or ;

This function can be used to select between two values,
using the comparison operators, shown in the next section.

### Comparison Words and Truth

Unlike most programming languages Forth returns all bits
set for true (although when checking for true non-zero
is checked against, not all bits set) and zero for false
as usual. There is a good reason for this, it allows the
bitwise operators to be used like the logical operators
are in C.

The comparison operators are:

* `=`: Are two numbers equal?
* `\<\>`: Are two numbers unequal?
* `\<`: Signed less than.
* `\>`: Signed more than.
* `\<=`: Signed less than or equal.
* `\>=`: Signed more than or equal.
* `0=`: Is value equal to zero?
* `0\<\>`: Is value not equal to zero?
* `0\<`: Is value less than zero?
* `0\>`: Is value more than zero?
* `0\<=`: Is value less than or equal to zero?
* `0\>=`: Is value more than or equal to zero?
* `u\<`: Unsigned less than.
* `u\>`: Signed more than.
* `u\<=`: Unsigned less than or equal to.
* `u\>=`: Unsigned more than or equal to.

Much like the arithmetic operators, there is nothing
special here apart from the return value for truth. Unlike
in the vast majority of programming languages the correct
operator is not selected, it is up to the programmer to
select the right one, this is due to the fact that Forth
is a type less language so there is no information upon
which the implementation can select differing comparison
operators. On a 16-bit Forth -1 and 65535 are the same
number, it might even be a pointer, the usage of the
number determines the numbers type, whether it is correct
to treat the number as signed, a boolean, a pointer, or
as something else entirely is up to the programmer. This
is another source of errors, and of simplicity. Simplicity
that foists greater complexity onto the programmer.

Now to test some of them out:

	1 2 < .
	1 2 > .
	1 2 u< .
	1 2 u> .
	-1 1 < .
	1 -1 < .
	-1 1 u< .
	1 -1 u< .
	0 0= .
	1 0= .
	-1 0= .

Note the differences between who the signed and unsigned
operators behave.

### Forth naming conventions: Part II

Now we have a feel for the words, it would help if we had
a way of describing the effects they have on the stack and
the types of argument they take and return. This is done
with stack effect comments, they are comments and are not
enforced by the Forth interpreter.

Take the `min` and `max` again, they take two signed
numbers and return a Forth boolean value (a 0 or -1). Let
us take a simple definition of them:

	: min 2dup > if swap then drop ;
	: max 2dup < if swap then drop ;

Notice that they use signed operators, to define minimum
and maximum words (called `umin` and `umax`, notice the `u`
prefix) we would use the corresponding unsigned comparison
operator. If we wanted to comment on the stack effects we
could do something like this:

	: min 2dup > if swap then drop ; ( n n -- f )
	: max 2dup < if swap then drop ; ( n n -- f )

In fact we can document some of the stack effects for the
operators we have been using:

	+ ( n n -- n )
	- ( n n -- n )
	and ( u u -- u )
	or ( u u -- u )
	u> ( u u -- f )
	> ( n n -- f )
	0< ( n -- f )

Although it does not matter the sign of the values passed
to `+` and `-`, we have used `n` to document that it takes
a signed value, for words like `u\>` and `\>` the sign
really does matter.

The following is a list of conventions used within stack
comments:

* `u`: used for unsigned values.
* `n`: signed values.
* `c`: A character, this is usually a single byte.
* "name": The word parses a word from the input stream.
Parsing words will need to be looked into more detail 
later.
* `a`: An address.
* `b`: A potentially unaligned address (for example it
points to a character stream).
* `f`: Not a floating point number, a flag.
* `d`: A double cell word, this will take up two places on
the stack.
* `ud`: An unsigned double cell word.
* `xt`: An execution token, a function pointer.

Words can move items between stacks as mentioned, here
is are examples of this:

	>r ( n --, R: -- n : move item to return stack )
	r> ( -- n, R: n -- : move item from return stack )

`?dup` is a word that duplicates the top of the stack if
it is non-zero, otherwise if it is zero it just returns
it. `?dup` has the following stack comment:

	?dup ( n -- n n | 0 -- 0 : conditionaly dup )

If is often used in looping constructs.

Words often have prefixes or post fixes as mentioned, here
are some:

* `u`: the number is unsigned.
* `2`: two, this often applies to stack operations, it
means it affects a group of two items, `3` would affect
a group of three items.
* `\>`: this means "to" as a prefix and "from" as a post
fix, as in convert to, and convert from.
* `.`: often used to mean that the word displays something,
as in the words `.`, `.s`, `.r`, `.u`, `u.r`.
* `+`: as a  prefix it is often added to words to indicate 
that the word adds something to a value, such as adding
a number of bytes as an offset into a structure.
* `@`: the word fetches something.
* `!`: the word stores something.
* `#`: often means "number of".

There are other conventions, and projects often establish
there own.

### Memory and Memory Operations

We have encountered `@`, which is used to fetch a value,
sometimes it is used to interact with hardware on some
Forth platforms, allowing us to read hardware registers.

`@` fetches a value that is the natural size for that
machine but always at least 16-bits in size. On a 32-bit
platform `@` with fetch a 32-bit value (and double cell
words occupy two 32-bit slots), on a 16-bit platform,
it will fetch a 16-bit value, and on an 8-bit platform
it will fetch a 16-bit value. Depending on your system
`@` may or may not be able to handle unaligned loads, so
you should be very careful when incrementing an address,
you will need to increment by the correct amount yourself.

`!` stores a value, it has the same size restrictions.

Their stack comments are:

	@ ( a -- n : fetch a value )
	! ( n a -- : store a value )

At the moment we have little to read and write from, there
are a number of variables defined within the system, and
constants as well. We can define our own with the words
`variable`, and `constant`, both of which are *defining
words*, that is they are words which define new words. This
concept will require its own chapter later.

The following defines the variables `x`, `y`, and `set`.

	variable x
	variable y
	255 constant set

It is important to note that the value of `x` and `y` are
not initialized, or at least not guaranteed to be 
initialized. Some variants of `variable` accept an 
initialization value, but that is non-standard.

To write a value to `x` we use `!` and to read from it
we use `y`.

	42 x ! .s
	x @ .

We can see what the address of `x` is:

	x .

Although the address will not mean much to you. 

Here is a slightly more advanced use:

	: our-swap x ! y ! x @ y @ ;
	: our-dup x ! x @ x @ ;
	: our-drop x ! ;

We can define our own (thread-unsafe) versions of `swap`,
`dup`, and `drop`. In Forth it possible to build up (or
rewrite) the system using more primitive methods.

We can test them out:

	1 2 .s
	swap .s
	our-swap .s
	our-swap .s

There is nothing that prevents us from reusing the name
for `swap` instead of `our-swap`, it can be useful to
redefine a built in word like `swap` for multiple reasons
such as for debugging (we could add stack depth checks if
they are missing) or meta-compilation (instead of calling
`swap` we could make a version of `swap` that compiles
a swap instruction into a target executable). These uses
are more advanced however.

To store or load byte quantities we can use `c@` and `c!`,
many 32-bit or 64-bit Forth implementations provide words
for storing and loading 16, 32, and 64 quantities as well,
often with prefixes like `q` for quad, or `w` for word (as
in machine word), on those platforms `!` and `@` will be
a synonym for one of those word pairs.

### Control Structures

We have already encountered `if` statements, although they
have not been explained. You may have noticed that `if`
statements in Forth are terminated with a `then`, which
takes some getting used to.

`if` takes an argument from the data stack and checks to 
see if it is zero, if it is zero then the code in between
`if` and `then` is skipped. Naturally, we also have `else`.

The other control structures in Forth a more particular to
Forth, and it is possible to make your own. As mentioned,
in Forth there are no key words, `if` is a simple function
much like `dup`, or a user defined function, albeit with
some special behavior, behavior allowed because `if` is
an immediate word, a topic covered later.

The control structures are:

* `if...then`
* `if...else...then`
* `begin...again`
* `begin...until`
* `begin...while...repeat`
* `do...loop`
* `do...+loop`
* `?do...loop`
* `?do...+loop`
* `for...next`
* `for...aft...then...next`

They must be used within a word definition, using them
outside of one will likely cause an error to be displayed.
The last two, `for...next` and `for...aft...then...next`
are rarer control structures but are much preferred by
eFORTH as they are simpler to implement than the `do`
loops.

If you want to use the `do` loops in SUBLEQ eFORTH you
will need to enable them and recompile the system if you
are using <https://github.com/howerj/subleq>, or you
can just use the online version at 
<https://howerj.github.io/subleq.htm> which has all
options enabled.

As we can only use these control structure words from
within new word definitions we will have to define words
for testing purposes.

`if...then` we have already encountered:

	: t1 if cr ." Clause executed" then ;
	: t2 
	  if 
	    cr ." Clause 1 executed" 
	  else
	    cr ." Clause 2 executed 
	then ;
	0 t1
	1 t1
	0 t2
	99 t2

`begin...again` loops forever, after running the test word
you will need to halt the interpreter manually, most likely
with CTRL-C, or to reset your Forth system.

	: t3 begin cr ." The pain never ends!" again ;
	t3

`for...next` loops are rarer, they execute N+1 times, and
never if provided a negative number. Their loop counter
can be accessed with `r@`. The following example uses
`depth`, which if defined in your Forth should provide you
with the number of items currently on the data stack prior
to calling `depth`. In SUBLEQ eFORTH `depth` is accessible
by loading the system vocabulary as it is a non-standard
word, this can be done by running `system +order` (which
will only work under SUBLEQ eFORTH).

The `for` takes a loop counter which it stores on the
return stack, `next` checks this counter and pops it off
when it reaches zero, otherwise it decrements it and jumps
back to just after the `for`.

	: empty depth 1- for drop next ;
	: ndrop 1- for drop next ;
	.s
	1 2 3 .s
	empty
	.s
	1 2 3 .s
	2 ndrop
	.s

`for...aft...then...next` is a similar construct that is
specific to eFORTH. The `for...aft` is executed only once
at the start of the loop, `aft...then` is executed N time,
`then...next` is executed N+1 times, to see how this works
the following test word should be defined and run.

	: t4 for 
	  cr ."  start " 
	  aft r@ . then 
	  ."  end " 
	  next ;
	9 t4

Note how many times `end` is printed and `start`, compare 
this to how many times the loop counter is printed with
`r@ .`. 

You Forth might lack `for` and `next`, if it is not an
eFORTH, it is highly likely that it lacks `aft`. It is
often used without anything in between `for...aft` and
`then...next`, just to execute the loop N times instead
of N+1 times, as `for...next` does, like so:

	: t5 for aft r@ . then next ;

`begin...until` and `begin...while...repeat` are commonly
available, but less commonly used, looping constructs. They
are very simple to implement and do not use a loop counter
unlike `for` and `do` loops. 

	: t6 begin 1- dup . ?dup 0= until ;
	9 t6

The loop continues until the exit condition is false.
Note that the loop body is always executed at least once.

An example using `begin...while...repeat`, which checks
the loop exit condition first:

	: t7 begin ?dup while 1- dup . repeat ;
	9 t7

The `do...loop` construct can be used for definite counted
loops that operating within a range, unlike `for` loops
which takes one argument they take two, and upper and a
lower bound, because of the poor way the behavior of `do`
was specified, a second loop initiator called `?do` was
made. We can increment by a number other than 1 by using
`+loop`, instead of `loop`, `+loop` pulls a value off of
the data stack.

	: t8 do i . loop ;
	: t9 do i . 2 +loop ;
	: t10 ?do i . loop ;
	: t11 ?do i . 2 +loop ;

	10 0 t8
	10 0 t9
	10 10 t8 ( Causes problems! )

`i` has been introduced without mention, it gets the loop
counter, to get the loop counters in containing loops you
will need to use `j` and `k`. Unfortunately there is no
standard way to extract the loop counter if you needed to
get it from nested loops more than three deep, although
you should rewrite your code if this occurs, it is an
arbitrary limitation for Forth.

It is possible to exit early from a loop, but you have to
use the correct word, `leave` for `do...loop` constructs
and their variants, and `rdrop exit` for `for...next`
constructs. You should never call `exit` from within a
`for..next` or `do...loop` directly. `begin...again`,
`begin...until` and `being...while...repeat` loops can
be exited from with `exit`. All of these complications,
whilst seemingly minor, contribute to the difficulty of
programming Forth, the fact that the programmer has to
select the right word for something so trivial and has
to do the work of the compiler for it means that the Forth
implementation can be simple.

So far we have not seen anything really special, there
have been hints here and there, but despite Forth having
severe limitations and drawbacks it does have some 
features, and not just its small size and simplicity of
implementation. It has methods of introspecting itself and
meta-programming capabilities that are seen rarely (mainly
in LISP, which does a better job of it than Forth). We
will eventually go on to those topics, for the moment Forth
just seems like a weird imperative programming language
with an odd syntax and an RPN interface. It will still
seem like that after, but with some redeeming features.

### Case Statements

**In SUBLEQ eFORTH this construct is optional and will
need to be enabled**.

`if` statements can get quite unwieldy when more than
one or two branches are requires, switch statements can
help alleviate that problem. The words used to a case
statement are as follows:

* `case`
* `of`
* `endof`
* `endcase`

`case` starts the switch statement, and is naturally ended
with `endcase`. 

Example:

	: ex
	  case
	    1 of ." one" endof
	    2 of ." two" endof
	    ." default"
	  endcase ;

**TODO**

### Conditional macros

`[if]`, `[else]` and `[then]` are used as conditional
macros, much like `#if` in C, except in Forth, everything
is an ordinary word, `[if]` is included in that.

To use them:

	0 [if] 2 2 + . [else] 9 9 * . [then]
	1 [if] 3 . [then]

They are often used with `defined`, which takes the next
word from the input stream and returns a boolean, true if
the word is defined and appears in the dictionary, and
false if not. For example:

	defined ?dup 0= [if]
		: ?dup dup if dup then ;
	[then]

### Comments

Comments are ordinary words in Forth as well. There are
two words that are standard comment words, `(` and `\\`,
both of which have been implemented in different ways which
can lead to problems for those that are unaware.

`\\` discards anything until the end of the line. `(`
discards everything until a corresponding `)`. They work
within and outside of word definitions as they are 
immediate words, which will be discussed in the chapter
on command and compile modes.

Depending on how `(` is implemented in might not work
over multiple lines, and it might require that that `)`
be a single character, for example:

	( This should work )
	( This might not work)
	(
		This might not work.
	)

There are two reasons for this, `(` may be implemented so
it parses words from the input stream, thus it looks for
`)` and not `foo)`, secondly the implementation of `(`
may not reload the input line. Forth input is line based,
a line of text is input and then processed, if `(` does
not call the appropriate function for reloading the line
if it has not encountered a `)` then some implementations
treat the end of line as a `)`.

`\\` should work within Forth blocks, which are encountered
later, where what a line of text is is slightly different.

## Allocating memory

One weakness of Forth is its lack of data structures, it
is possible to create words that perform arbitrary 
operations data structures, but that does not mean it is
good or easy to do so, and apart from the dictionary and 
the data stacks Forth has no real data-structures to speak 
of.

As mentioned, `@`, `!` can be used to load and store values
to and from memory. `variable` can be used to create a new
variable. We can see how much memory is allocated with the
word `here`, this returns the amount of memory used by the
dictionary. The dictionary is a linear array that grows
when we define new words or allocate memory manually.

The functions we define, the words, are located in the
dictionary. Traditionally the dictionary is a linked list
of words, and the names of the words are stored separately
from the definitions of the words. More modern Forth
implementations use hash tables for speed and some of the
simpler Forth implementations store the word names next
to the word code (SUBLEQ eFORTH uses a linked list and
stores word names and code together). The idea of storing
the names of the words separately from their definitions is
so that the word names can be erased and the space 
reclaimed if the application does not require the 
interactive Forth shell to be present.

We can allocate space in the dictionary with the word
`,`. This stores a single cell into the dictionary. We
can see this with the following:

	variable x  ( used to store a pointer )
	here x !    ( store the current position in x )
	here .      ( display the current position )
	99 ,        ( write `99` into the dictionary )
	here .      ( display new dictionary position )
	x @ @ .     ( retrieve `99` )

On a 16-bit Forth `,` will write a 16-bit value into the
dictionary. We can write bytes into the dictionary with
`c,`, much like writing bytes with `c!` and loading them
with `c@`. After using `c,` the dictionary pointer may not
be aligned, you can use `align` to align up the dictionary
pointer on your platform.

To allocate many bytes in the dictionary `allocate` can
be used. `char` is used to get the value of a character
(it actually parses a word and gets the value of the first
byte of that word), `count` is used to retrieve the byte
count in the string we are creating and returns the rest
of that string, `type` displays that string.

	here 6 allot align constant hi
	5 hi c!
	char H hi 1 + c!
	char e hi 2 + c!
	char l hi 3 + c!
	char l hi 4 + c!
	char o hi 5 + c!
	hi count type cr

It would have been simpler to type this as:

	here
	5 c,
	char H c,
	char e c,
	char l c,
	char l c,
	char o c,
	align
	constant hi
	hi count type cr

We must be sure to align the dictionary pointer after
using `c,`. 

`cell` can be used to get the size of a single cell, it
will be 2 on a 16-bit platform. `cells` can be used to
calculate the number of bytes needed to store N cells.

	1 cell .
	here .
	2 cells allot
	here .

`aligned` is like `align` except it accepts a pointer and
does not operate on the dictionary pointer like `align`
does:

	here .
	1 c,
	here .
	align
	here .
	0 aligned .
	1 aligned .
	7 aligned .
	2 aligned .
	4 aligned .
	8 aligned .

`allocate`, `free` and `resize` are used to manage memory
allocated on the heap. SUBLEQ eFORTH has this as an 
optional extra that must be enabled (and has only a small
1KiB heap). The words are roughly analogous to `malloc`, 
`free`, and `realloc` from C. 

The stack effects for these words are:

* `allocate` ( u -- a ior : allocate memory )
* `free` ( a -- ior : free memory )
* `resize` ( a u -- a ior : resize allocated pointer )

`free` and `resize` must only be called on pointers 
allocated by `allocate`. These functions return an `ior`,
and Input/Output error, if it is non-zero an error has
occurred (such as we have ran out of space). It is up to
the user to balance allocations and freeing the returned
memory.

These words are not often used and in smaller Forth
implementations might not even be present. If you find
yourself needing them it is a sign that you are either
doing things wrong, or trying to solve a problem which is
not suited to Forth (which is more likely).

### Execution Tokens, Hooks

An execution token is the Forth terminology for a
function pointer. It is possible to get the execution token
from a word with the word `'`, this can then be executed
with the word `execute`. For example:

	2 2 ' + execute .

Is equivalent to:

	2 2 + .

It is possible to store an execution token in a variable:

	variable <op>
	: op <op> @ execute ;
	' + <op> !
	2 2 op .

`<op>` is known as a hook, a variable used to store an
execution token so that functionality can be changed at a
later date, by convention hooks are sometimes bracketed
with `<` and `>` to differentiate them from ordinary
variables, with the word that performs the action, `op` in
this case, being the normal version without brackets. `op`
could perform a NULL check if needed.

Forth philosophy dictates that hooks are to be avoided,
instead you are meant to change the code to do what needs
to be done specifically. This is a poor way of doing things
when it comes to Forth implementations that are meant to be
used by other people, they often do not want to modify the
behavior of the base system, and providing a hook is not
always about deferring work but can add new functionality
(such as the ability to take input from different sources
as the need arises).

Typical system callbacks include:

* `<key>`: Called by `key`, this allows the user to control
where input is taken from, for example you might want to
take input from a file, but then restore the system to
take input from the keyboard.
* `<emit>`: Called by `emit`, this allows the user to
control where output goes to.
* `<cold>`: A word executed at or near when the Forth
interpreter boots. Forth does not have a standard way of
creating stand alone Forth executables, a weakness of the
Forth ecosystem (along with poor library support and other
problems), some Forth implementations at least allow an
execution vector to be changed so the starting word to be
executed is changed, booting into a user application 
instead of the usual Forth command line.
* `<literal>`: Useful for meta-compilation, this allows
the user to determine what happens with numbers when the
Forth interpreter encounters them, for example they could
define a function that compiles the numbers into a target
memory location during cross-compilation instead of 
compiling them into the hosts word definitions.

These are not standard and not likely to be defined, or
if they are, they might have different names. 

The above hooks are defined in the `system` vocabulary in
SUBLEQ eFORTH, which can be loaded with `system +order`,
they are kept in the system vocabulary as they are non
standard words.

### Defining `min` and `max`

This chapter contains multiple definitions of the same
words, done in different styles and for different reasons,
mainly for the purposes of micro optimizations which may
not be relevant for what you are doing or for the platform
you are on. However if you are programming in Forth then
for the type of situations Forth is good at, these type
of considerations and optimizations matter.

This is the slightly less optimal of defining `min` and
`max`.

	: min 2dup > if drop else nip then ;
	: max 2dup < if drop else nip then ;

The code for `min` this might look like this:

	X: `min` WORD HEADER
	0: call: 2dup
	1: call: >
	2: jump if zero to 5
	3: call: drop
	4: jump to 6
	5: call: nip
	6: exit

The following:

	: min 2dup > if swap then drop ;
	: max 2dup < if swap then drop ;

Like this:

	X: `min` WORD HEADER
	0: call: 2dup
	1: call: >
	2: jump if zero to 4
	3: call: swap
	4: call: drop
	5: exit

Which is slightly smaller.

The following is slightly small *on some platforms*, such
as the Forth available at 
<https://github.com/howerj/forth-cpu>. This Forth executes
on a CPU designed for the language, called the H2, it can 
execute some primitives in a single cycle. The cross 
compiler can also merge exits into some primitive 
constructs with a simple peep hole optimizer.

	: min 2dup > if drop exit then nip ;
	: max 2dup < if drop exit then nip ;

What is:

	X: `min` WORD HEADER
	0: call: 2dup
	1: call: >
	2: jump if zero to 5
	3: call: drop
	4: exit
	5: call: nip
	6: exit

On some platforms, becomes:

	X: `min` WORD HEADER
	0: call: 2dup
	1: call: >
	2: jump if zero to 4
	3: drop instruction + exit
	4: nip instruction + exit

This is smaller and faster.

SUBLEQ eFORTH has `mux` available as a relatively fast
assembly routine, we have already seen its definition. It
can be used like so:

	: min 2dup > mux ;
	: max 2dup < mux ;

And given `mux` already exists it is the smallest of the
definitions.

If branching is particularly expensive it is possible to
perform both `min` and `max` without branching another
way, in fact it also does not use the comparison operators
either. It might be best to inline the code on your 
platform:

	\ equivalent 32-bit C, needs arithmetic right shift
	\ return b + ((a-b) & (a-b)>>31); /* min */
	\ return a - ((a-b) & (a-b)>>31); /* max */
	1 cells 8 * constant #bits
	1 #bits 1- lshift constant #high
	: high? #high and 0<> ;
	: min 2dup nip - dup high? and + ;
	: max 2dup drop swap - dup high? and - ;
	\ 16-bit `high?` without using `and`:
	: high? #high + 0>= ;

Usually one would not have to concern oneself with such
micro optimizations as the compiler would take care of 
them, even having options to select code generation for
the smallest or alternatively the fastest code. Forth
implementations are usually quite primitive, so the
programmer often has to play the part of the compiler.

## Command Mode and Compile Mode

The Forth interpreter has two modes of operation, command
mode and compile mode. In command mode all words are 
executed and numbers are pushed to the data stack. In
compile mode words are compiled unless they are immediate,
immediate words are instead executed, and numbers are
compiled into the word definition. When the Forth 
interpreter searches for a word, if it is not found, an
attempt is made to treat it is a number, only if that fails
is an error raised.

You have encountered immediate words before hand, words
such as `(`, `if`, `for`, `loop`, and even `;`. Some
immediate words have behavior for both command and compile
mode, such as `(`, others will not work in command mode
such as `if` (as `if` is not only marked as being 
*immediate* it is also marked as being *compile-only*). 

Your Forth implementation may not do such error checking.

Note that `:` is not an immediate word, but `;` is. You
should only make a word immediate if it needs to be. 

Let us look at two lines of Forth code. If we are in 
command mode then the following code:

	2 2 + . cr

Does the obvious. After we enter this line of text and hit
enter, the Forth interpreter parses each word from left to
right. It first encounters `2`, it checks if this is a
defined word and it is not (at least not be default), so
it then tries to convert it as a number in the current
base, it succeeds, the interpreter then checks if we are
in compile or command mode, as we are in command mode it
then pushes the word to the data stack. It repeats this
for the next `2`, and when it gets to `+` it does the same
except this is a defined word, the interpreter then 
executes this word, and does the same for `.` and `cr`.
A full line of text is processed without error so an `ok` 
is printed, as we are in command mode.

For the second line, we start in command mode, and the
first word we encounter is `:`, this word is executed as
we are in command mode. `:` parses the next word in the
input stream, creates a header for that word in the
dictionary but does not link it in, and then switches the
interpreter to compile mode. In compile mode if we
encounter a `2` it is instead compiled into the dictionary
so that when the word `example` is run it pushes `2` onto
the data stack, the same is done for the second `2`, and
for the word `+`, `.`, and `cr`. Those last three words
are all not immediate words, so they are compiled in. The
final word, `;`, is an immediate word. When the interpreter
encounters an immediate word it is instead executed
regardless of what mode the interpreter is in. This word
links the word `example` into the dictionary so it can
be seen by the rest of the system (the linking only happens
at this step both to stop the word being linked in if there
is an error defining the word, and also so that any
previous definitions of `example` if they exist could be
called from the new one). `;` also compiles an `exit`
into the word being defined and lastly it switches the
interpreter back into command mode.

	: example 2 2 + . cr ;

If `;` was not immediate it would not be able to break
out of the compile mode. It needs to be executed instead of
compiled into the code.

Of note, if we instead typed in:

	: example
	  2 2
	  +
	  .
	  cr
	;

The usual `ok` prompt disappears, and returns when `;`
is entered. This is normal behavior, when in compile mode
the `ok` prompt is turned off, some Forth implementations
instead change the prompt to print `compiled` instead.

In our `example` word the compiled code will looks 
something akin to this:

	X: `example` word header
	0: Push 2
	1: Push 2
	2: Call `+`
	3: Call `.`
	4: Call `cr`
	5: Exit

We could make this slightly more efficient by calculating
the result of `2 + 2` and instead printing this out, it
would become:

	: example 4 . cr ;

And the code would look like this:

	X: `example` word header
	0: Push 4
	1: Call `+`
	2: Call `.`
	3: Call `cr`
	4: Exit

It is smaller and will run faster.

This trivial, contrived, example shows a fundamental
facet of Forth - you are expected to perform the role of
the compiler. Even the simplest compilers for a language
like C will partially evaluate expressions at compile time
and compile those results instead. The simplicity of Forth
is extreme.

For an expression like `2 + 2`, we would write `4` down
ourselves, but there are more complex expressions that
we might want to evaluate and compile in, ones in which
we do not want to calculate the results by hand but we
do want the result to be compiled in as it can be 
determined ahead of time. We will continue to use our
trivial example, but there are real world examples of
wanted to calculate tables programmatically at compile time
(such as CORDIC, CRC tables, and even sine and cosine 
tables).

We could do the following:

	2 2 + constant four
	: example four . cr ;

However the constant `four` takes up space in the
dictionary, space we might not want to use up, especially
if this constant only requires being defined once.

We know `,` can write numbers into the dictionary, but that
is not the same as compiling a literal, and besides, `,`
when called within `example` will compile a call to `,` and
not write the value we want into the dictionary.

Instead we need a way of switching back into command mode
whilst in the middle of a word definition. We know `;` that
is an immediate word that will switch back into command
mode, however it will also terminate a word definition, so
we cannot call it. Instead there are two words defined
expressly for this purpose, called `[` and `]`, `[` is
an immediate word whose only purpose is to switch the
interpreter back into command mode. `]` switches the 
interpreter back into compile mode, it is not an immediate
word as it does not need to be.

However, the following code **will not** work:

	2 2 +
	: example [ , ] . cr ;

For one reason already mentioned, `,` writes a value into
the dictionary, however when the Forth system executes that
`2` will not be a valid instruction. Instead we must 
compile a literal instead of writing one. `literal` can
do this, but there is another problem. The data stack is
used by the Forth compiler. `:` may push an arbitrary 
number of values to the data stack and `;` may check or
use these values. This means our `4` could be anywhere on
the data stack. Instead we need to do the following:

	: example [ 2 2 + ] literal . cr ;

Note that `literal` is outside of the `[` and `]` brackets,
this is because `literal` is an immediate word that
examines the interpreter state, if we are compile mode
`literal` compiles a value into the dictionary, in command
mode it leaves it there. If we run this version of 
`example` we get what we want.

`:` and `;` both use the data stack for their own purposes,
the data stack does not go away just because we in compile
mode, and we can still run Forth words in compile mode,
any Forth word, so long as it is immediate. This allows
us to create immediate words that act as control 
structures, `if`, `else`, `then`, `begin`, `until`, `for`,
and the like are all immediate words, they also all push,
consume or manipulate words on the data stack at compile
time.

`if` when called compiles a `branch if zero` instruction
into the dictionary, it does not have the target location
to jump yet, so it compile a "hole" after the jump (or
as part of it), the location of this "hole" is then pushed
onto the data stack. When `else` or `then` is called this
"hole" is written to with the correct location to jump
to. All of the control structures do something similar.

You may wonder what stops us from calling and `if` without
a `then`, and this is done with a notion called "compiler
security" in Forth. `if` not only pushes a location to
write to, but it also pushes a value which is checked for
by the loop or branch construct terminating word, `:`
and `;` will do something similar, if the right value is
not found then an error is raised. This allows us to
do some very simple syntax checking. Your Forth 
implementation may or may not implement compiler security
features.

It is possible to compile an immediate word into the
dictionary, this is often useful when we want to define
our own conditional statements. The word `postpone` can
be used, it is an immediate word that acts on words and
compiles them into the dictionary, as it is an immediate
word and is executed before the next word, whether the
next word is immediate or not does not matter, it will
take that word and compile it into the dictionary. The
word `compile` can be used on non-immediate words. The
word `immediate` is used after a word definition to make
the word just defined immediate. Using all this, try to
understand what is happening with the following:

	: -if compile 0< postpone if ; immediate 
	compile-only
	: test -if cr ." NEGATIVE" then ;
	0 test
	1 test
	-1 test

`compile-only` may not be present in your Forth, if it is
then it will make sure that `-if` can only be used within
a word definition.

The state of the compiler is stored in the variable
called `state`, it can be written to manipulate the
compiler state or read from. A non zero value indicates
we are in compile mode, and a zero indicates we are in
compile mode.

### Parsing Words: Defining new words

**TODO**

## Pictured Numeric Output

**TODO:**

* Numeric input vs output
* `base` and `dump`

## Recursion

Recursion has to be handled with a special word called
`recurse` in Forth. You cannot call a function within
the function definition as the definition has not yet been
linked into the dictionary (at least not in standard Forth
implementations). If you attempt to do this then the
function will either not be found, leading to an error
being raise or if there is a previous definition with the
same name then that version will be called instead.

	: x x ; ( fails if `x` is not defined )
	
	: y cr ." executing first y" ; 
	: y cr ." new y" y ;
	y

You may get a warning that `y` has been redefined, one of
the few warnings that Forth may issue.

Instead, to perform recursion, call `recurse`:

	: z ?dup if 1- dup . recurse then ;
	10 z
	1 z
	0 z

If you call `z` with a high value it may cause problems,
each call to `recurse` uses space on the return stack,
which only has a finite amount of space, the amount of
which is defined by the platform, it can be quite limited.

On some platforms the following will replace the recursion
with a jump to be beginning of the world:

	: z ?dup if 1- dup . recurse exit then ;

(This is not the case in SUBLEQ eFORTH). This optimization
turns the recursion into a tail call, which means that no
extra space is used on the return stack as the call is
replaced with a jump.

## Double Word Set

Double cell numbers, called doubles in Forth, have already
been mentioned. Each double cell number occupies two slots
on the data stack, hence the name. They are not related to
floating point numbers.

**Note that in SUBLEQ eFORTH many of these words are
not defined.** Instead they appear in an appendix of the
book and can be entered in if needed.

To enter a double cell number we enter it by point a 
decimal point in the number. To check if the number just
entered was a single or double cell number the variable
`dpl` can be checked (it is set to -1 after to converting
a single cell number and it set to zero or more to indicate
the position of the decimal point if a double cell number
was entered):

	2 dpl @ .s 2drop
	2.1 dpl @ .s drop 2drop
	20.1 dpl @ .s drop 2drop

We can convert to and from double and single cell integers
with the following words:

* `s>d` ( n -- d : convert signed cell to double cell )
* `d>s` ( d -- n : convert double cell to single cell )

`d>s` may cause information to be lost as a single cell
can only represent a subset of numbers possible in a
double cell integer.

`u>d` and `u>ud` are not usually needed, if you need to
convert an unsigned number to a signed or unsigned double
cell integer you can just push a `0` after the number, as
the high portion of the double cell integer is stored on
the topmost stack location.

The words for manipulating them are as follows:

* `d+` ( d d -- d : double cell add )
* `d-` ( d d -- d : double cell subtract )
* `dnegate` ( d -- d )
* `dlshift`, `drshift` ( d u -- d )
* `d2*`, `d2/` ( d -- d )
* `dabs` ( d -- ud )
* `d>`, `d<`, `d<=`, `d>=`, `d=`, `d<>` ( d d -- f )
* `ud>`, `ud<`, `ud<=`, `ud>=` ( ud ud -- f )
* `d0<`, `d0>`, `d0>=`, `d0<=`, `d0=`, `d0<>`. ( d -- f )
* `dmin`, `dmax`, ( d d -- d )

It is common to not define all of these words but only
a subset needed to get the base interpreter working. This
is done for space reasons. The words `2drop`, `2nip`, 
`2swap` and the like are used with these double cell
numbers to move them around the data stack.

If your system defines `d=` it may not define `ud=` as they
are equivalent words (at least where twos compliment
arithmetic is used).

* `d.` ( d -- : display a double cell number )
* `ud.` ( ud -- : display a unsigned double cell number )
* `d.r` ( ud +n -- : show double with +n leading spaces )
* `ud.r` ( d +n -- : show unsigned double with +n spaces )

`2.` is not the same as any of the above words if it is
defined, instead it will display two single cell words
one after the other.

These words operate on mixed quantities an either consume
or produce double cell numbers:

* `um+` ( u u -- ud )
* `um*` ( u u -- ud )
* `um/mod` ( ud u -- ur uq )
* `m/mod` ( d n -- r q )
* `m*` ( n n -- d )
* `*/` ( n n n -- q )
* `*/mod` ( n n n -- r q )

**TODO**

* Talk about fixed point

## `create` and `does>`

`create` and `does>` have been called the jewels of Forth,
is it perhaps because you have to sift through a large 
amount waste to get to them. Perhaps a better analogy would
be that they are like undigested corn.

Anyway...

`create` and `does>` can be used to make words that create
news, a quote from Nietzsche would be appropriate right
about now. Here it is:

"Companions the creator seeks. Not corpses, not herds and
believers. Fellow creators the creator seeks, those who
write new values on new tablets."

With `create` and `does>` you too can become the 
Uebermensch, or at least create a class of new words called
"defining words". We have seen them before with the words
`:`, `variable` and `constant`. Defining words can be
immediate, but they do not have to be, many are not.

We can define `variable` and `constant` if they are
lacking on your system like so:

	: constant create , does> @ ;
	: variable create 0 , does> ;

`create` makes a new word and links it into the dictionary,
by default a word made by `create` just returns a pointer
to the dictionary after the definition of said word. 
`does>` changes the behavior a created word, the code after
`does>` runs in the created word and not in the current
definition, it is still passed the address of the data 
after the created word. The code in between `create` and
`does>` runs after the word is created but is not part
of the newly defined word.

Words created with `does>` do have some strange behaviors
such as:

	: hello does> ." Hello" does> ." Good bye" ;
	hello
	hello
	hello
	hello

This is not portable (`does>` should only be run on a
created word), so you might need to do this:

	: test create does> ." Hello" does> ." Good bye" ;
	test hello
	hello
	hello
	hello
	hello

The internals of how these work are not relevant (and are
instead explained in book "SUBLEQ eFORTH: Forth 
Metacompilation for a SUBLEQ Machine".

### Forth data structures

It is clear with the new constructs that new data 
structure words can be made, but it is unclear just how
that should be done. `variable` creates new variables, a
simple data structure, much like `constant`. We can use
these words to create arrays, structures and enumerations,
which will be a little more awkward to use than their C
counterparts but still do work.

Using `create` directly and then `allot` we an allocate
and name sections of memory:

	create arr1 20 cells allot
	arr1 20 cells blank ( will array with spaces )
	arr1 20 cells dump ( dump cells )

Notice that we have to keep track of the size of the
array. Instead we could create our own array word that
returns the array size, as well as zeroing the array.

	: array create dup , here over allot swap erase
	  does> dup @ swap cell+ swap ;
	20 cells array arr2
	arr2 .s 2drop
	arr2 dump
	arr2 blank
	arr2 dump

Perhaps instead we want to create a lookup table from
some data:

	: lookup create dup , 1- for , next
	  does> dup >r @ mod cells r> + cell+ @ ;
	10 8 6 4 2 5 lookup dubs
	0 dubs .
	1 dubs .
	2 dubs .
	3 dubs .
	4 dubs .
	5 dubs .

Note that the data is entered backwards. There are a number
of conditions that could be tested for, potential errors,
for example when creating the lookup table negative numbers
are not checked for. When the lookup function is running
`mod` is used to limit the index, this will fail if the
table is of zero length. `mod` might also not be the 
desired functionality, we might want to instead check that
the index is within bounds and if it is not we could 
instead throw an error. It is entirely up to you and your
requirements.

If we wanted to create a lookup table in which we could
modify the entries we could just remove the final `@`
before `;`. If we wanted a table of function pointers
that get executed we could instead call `execute` after
the `@` but before `;`.

The function we have created is not generic, although it
will work if we want to store execution tokens or pointers
(as they must fit in a single cell) it will not work if we 
want to store double cell numbers, floating point numbers, 
or if we want to store characters, we would need to make a 
function specific to those types.

**TODO**

## USER words and cooperative multithreading

Forth, whilst a programming language, often took the place
of a primitive operating system. This was the case for many
microcomputers in the 1980s, albeit BASIC was far more
common. This meant that it was the responsibility of the
Forth implementation to abstract over the resources the
machine provided, much like a more traditional operating
system. Forth provides simple ways to do this, the BLOCK
word set, shown in a different chapter, abstracts over
mass storage, `key` and `emit` hide user input and output
routines, and the USER routines and mechanisms allow
cooperative multithreading.

Cooperative multithreading has advantages and 
disadvantages, it is simple to implement and easier to
get right, but any single thread can block progress of the
entire system, potentially locking it up. It is possible
to use different threading schemes together, but that will
not be covered here. No timer is needed, nor interrupts,
in a Cooperative multithreading model.

Modern multitasking systems, such as those used by Unix
kernels, are preemptive. A timer interrupt is setup to
fire after a set period after a thread is run, once the
timer fires the kernel, which the interrupt jumps to, 
decides on what task should be run, often with a very
complex algorithm to determine fairness, responsiveness,
and throughput. Multiple threads can run within the same
program, and often on modern CPUs threads can be moved
between cores, and threads within the same program may
run on different cores. This presents several problems
that must be solved, any instruction may be interrupted and
race conditions and corruption can abound if not carefully
prevented and managed.

Unfortunately the words described in this system are
non-standard, many of the newer Forth implementations do
not define the words described in this section, nor their
equivalents. This is due to the fact that many of the newer
Forth implementations are hosted, and written in C, or
they target a Virtual Machine designed to run in a hosted
environment, the main purposes of these newer interpreters
is as a playground, not to write new Forth programs in but
to learn how to make a Forth. This is not a criticism, but
an observation and an opinion. You only need multithreading
when interacting with real word systems, thus making these
words more common in embedded Forth systems designed to
run on micro controllers, or historical Forth 
implementations.

There are three key mechanisms, USER variables, `pause` and
the ability to create and manage tasks. There are some 
words used to communicate between threads, passing messages
back and Forth between threads, but they are not key to
understanding multitasking.

The multitasking system in SUBLEQ eFORTH is based off of 
the one described in this paper:

<https://www.bradrodriguez.com/papers/mtasking.html>,
"Forth Multitasking in a Nutshell", by Brad Rodriguez,
September 1992.

What is multitasking? It is the ability to partition the
a CPU so multiple programs can run on it, seemingly at
the same time, even when there is only one CPU core. If
we have only one CPU core, naturally only one program can
run on that core, however we can pause that program, run
another, then restore it at a later time. This can happen
hundreds of times per second, giving the illusion that
both programs are running at the same time.

We may also want to manage tasks, activating them, or
pausing them whilst they wait for input, output or other
facilities to become available, a thread containing the
Forth interpreter does not need to run if there is no
input to process as an example.

Each program executing is known as a thread, a thread of
execution, In SUBLEQ eFORTH there is at least one thread 
defined, the starting thread which begins executing the
interpreter loop. In SUBLEQ eFORTH each task is 1KiB in
size and contains both variable and return stack, a parse
buffer, saved registers for the virtual machine, and 
various USER variables. 

USER variables are thread local storage, each USER variable
is an index into this stack space. All variables declared
by `variable` are global, and exist in a global shared
space, `user` is used to declare a thread local variable.
As space is limited in each task it is wise to declare
`user` variables only as needed, or not at all.

We have encountered various USER variables before, although
they were not introduced as such, `base`, `hld`, `dpl` and
`state` are user variables, as well many of the variables
used as hooks. They are defined as USER variables so that 
different threads can change those variables without
affecting the other threads (for example, you might want
to print something out in one thread in hexadecimal, but
keep the interpreter thread in decimal).

The core word of the multitasking system is called `pause`,
this word saves the virtual machine state to the currently
executing thread, then loads the registers for the next 
thread in the linked list of threads to execute, after
doing so it resumes the execution of the next thread.
`pause` pauses the current thread and loads the next one,
if there is only one thread it does nothing, and if
multitasking is disabled, it does nothing.

`pause` is called by various words within the SUBLEQ
eFORTH interpreter, such as `key`, `ms`, `emit`, and
`block`, all of which are either input/output operations
or potentially long running operations.

Despite these calls the programmer who wishes to use
multiple threads must arrange themselves to call `pause`
in their own threads if these operations are not called
frequently enough. A balancing act must be reached between
calling `pause` too little and too much, there is no
fancy scheduler, all scheduling is done manually. Calling
`pause` is not free, it is an expensive operation and the
more it is called the more responsive the system can be
(each thread will be serviced more frequently) but the 
less work will get done (more time will be spent running
the code in `pause`).


**TODO**

* Mention `key?`
* Examples
* H2 interrupts

### Interrupts in H2 Forth

## Locals

**TODO:**

Do not bother.

## Block Word Set

### Forth Block Editor

## Vocabulary Words

The vocabulary word set is often ignored in Forth 
implementations, and too many Forth implementations pollute
the default search order with too many words. SUBLEQ
eFORTH makes an attempt to keep the non-standard words
in separate vocabularies, only leaving a few words that
are non-standard in the default vocabulary.

The vocabulary word set offers a way to make modules,
and also aids greatly when making cross compilers written
in Forth.

These words are commonly defined, although non-standard:

* `+order`: Add a vocabulary to the top of the search
order if it is not in the search order.
* `-order`: Remove a vocabulary from the top of the search
order if it is in the search order.
* `(order)`: A factor of `-order`.

If they are not then you can use the following:

	: (order) ( w wid*n n -- wid*n w n )
	  dup if
	    1- swap >r recurse over r@ xor
	    if 1+ r> -rot exit then rdrop
	  then ;
	: -order get-order ( wid -- )
	  (order) nip set-order ; 
	: +order dup >r -order  ( wid -- )
	  get-order r> swap 1+ set-order ;

The standard words for manipulating vocabularies, `also`
and `previous`, are simplistic and poor for their purpose.
They are so poor that despite being standard words, and
the non-order words are non-standard, `also` and `previous`
are not defined in SUBLEQ eFORTH, whilst `+order` and
`-order` are defined.

* `wordlist`: This word is used in conjunction with
`constant` to create a named wordlist. It reserves enough
space in the dictionary for the word list and returns a
pointer to it.
* `get-order`: Get the search order and put it on the
data stack, the topmost items contains the number of
vocabularies on the stack, and the rest of the items are
vocabulary pointers.
* `set-order`: Given a number of the vocabularies on the
data stack and the vocabularies themselves this will set
the search order. There are some special cases, 
`-1 set-order` will loads the minimal root search order,
and `0 set-order` will mean no vocabularies are loaded,
all searches will fail (although entering numbers will
still work).
* `words`: This displays the list of words that are
currently loaded, it will do this in an implementation
defined manner.
* `only`: Equivalent to `-1 set-order`.
* `forth-wordlist`: The vocabulary that contains all
of the normally defined Forth words.
* `root-voc`: The minimal root vocabulary, a small
vocabulary containing words needed to get the system
back into a normal state. `root-voc` is SUBLEQ eFORTH
specific, your Forth may not have one, or it may be
equivalent to `forth-wordlist` (i.e. On your Forth the
minimal set of Forth words is the same as all of the
defined Forth words, as `minimal set` is not defined in the
ANS Forth standard).
* `forth`: This sets the vocabularies so that 
`forth-wordlist` and `root-voc` are loaded. It is
equivalent to `forth-wordlist root-voc 2 set-order` in
SUBLEQ eFORTH.
* `#vocs`: A non-standard word, this is a constant that
represents the maximum number of vocabularies that can
be loaded.
* `definitions`: This changes the vocabulary into which
new words are added to, the topmost vocabulary in the
vocabulary stack is anointed as the one to review new
word definitions.

This list of words will not give you a sense of how to
use vocabularies, instead you will need to experiment with
them. 

It is helpful to think of the system like this:

1) You have words in Forth which are functions.
2) A linked list of words form a vocabulary.
3) A group of vocabularies form a dictionary, also known
as the "search order".
4) The order in which vocabularies are loaded matters. If
you have multiple definitions in different search orders
then the vocabulary which is loaded first will be found
first.

This allows us to create a powerful module system with
limited primitives but it can be confusing, if the word
`x` is defined in multiple vocabularies it can be difficult
to tell which `x` will be used as it is possible to
dynamically change the search order (in fact there have
been Forth implementations that do away with the immediate
and compiling word distinction and instead put words
in different vocabularies depending on whether they are
immediate or not, `:` would thus load the compiling 
vocabulary and `;` would unloaded it, this is not common
nor standard behavior).

We can also redefine `x` multiple times within the same
vocabulary, we should be warned about defining multiple
words of the same name within the same vocabulary (although
your Forth may not do this), but not when defining a
word with the same name as a word in a different 
vocabulary.

Now onto some examples, let us imagine we want to define
a set of words with the same names as the built in ones
but that print out debug messages, we can use this word
set when loading code to debug it:

	wordlist constant debug
	debug +order
	definitions
	: + 2dup . . ."  + -> " + dup . cr ;
	: - 2dup . . ."  - -> " - dup . cr ;
	: * 2dup . . ."  * -> " * dup . cr ;
	: / 2dup . . ."  / -> " / dup . cr ;
	only forth definitions

(If `wordlist` is not defined in your version of SUBLEQ
eFORTH you can define it like so `: wordlist here 0 , ;`,
this is not a portable definition).

Because of the way defining new words in Forth works, when
using `+` within the new definition of `+` the old 
definition of `+` is used. To perform recursion instead
you need to use `recurse`, see the chapter on recursion
for more information about this.

You can then use these new definitions to debug your code:

	debug +order
	marker unload
	: x 2 + ;
	3 x
	4 x
	unload

Once it is working, you can recompile your code by 
using `debug -order`. `marker` is a word that can be used
to create a word which when called will erase all word
definitions defined after and including itself. You should
not add words to other vocabularies between defining the
marker and using it, all definitions should belong to the
same vocabulary.

**TODO:**

* Forth storage of dictionary, storing headers with word
definitions or separately.

	: also get-order over swap 1+ set-order ; ( -- )
	: previous get-order nip 1- set-order ; ( -- )
	: anonymous ( -- : make anon voc and enable it )
	  get-order 1+ here dup 1 cells allot 0 swap ! 
	  swap set-order ;
	: wordlist ( -- wid : alloc wid )
	  here cell allot 0 over ! ;

## Meta Compilation: Forth Cross Compilation in Forth

## SUBLEQ eFORTH defined words

With extra words defined:

	cold glossary facos fasin fatan2 fatan fasinh
	facosh fatanh f** flog flog2 flnp1 fln agm sins
	fhypot filog2 fsqrt f~ ftan fcos fsin fsincos
	frad fdeg fln10 fln2 fe f2pi fhpi fpi fs. e. e
	e.r ftanh fsincosh fcosh fsinh fexpm1 falog fexp
	exp finv f1- f1+ fone fmod fround floor f>s fix
	fliteral fconstant f f. f.r f# s>f d>f fwithin
	fmax fmin f0>= f0<= f0< f0> f= f<= f>= f> f< f-
	fzero f+ f/ um/ f0= fsq f* f2/ f2* fsign fnegate
	fnip f2drop fdrop -frot frot ftuck f2dup fover
	fswap fdup faligned falign f, f! f@ precision
	set-precision float+ floats fcopysign fdepth
	fabs cos sin cordic */mod sm/rem m* 2literal
	2variable 2constant 2, 2over dabs 2swap d0<> d0=
	d= d- d2/ d2* arshift convert spaces /string
	1+! 2- 2+ resize free allocate macro +loop
	loop ?do do leave k j i unloop endcase endof
	of case many rpick editor quit load evaluate
	set-input get-input list blank block buffer
	empty-buffers flush save-buffers update b/buf
	at-xy page bell ms [if] [else] [then] defined
	dump see compile-only immediate postpone \ .( (
	abort" $" ." exit rdrop r> >r marker does> >body
	user constant variable create next aft for else
	repeat while then again until if begin recurse '
	:noname : ; [char] char word definitions +order
	-order (order) get-order interpret compile,
	literal compile find search-wordlist cfa nfa
	compare .s number? >number . u. u.r sign <# #s #
	#> hold parse -trailing query tib expect accept
	echo / mod /mod m/mod um/mod * um* d+ dnegate
	um+ abort throw catch space erase fill cmove
	type +string count c, , allot align aligned
	source 2r> 2>r 2@ 2! source-id min max c! c@
	lshift +! pick set-current get-current cr emit
	key key? ?exit execute cell- cells cell+ cell
	abs s>d negate within u<= u>= u> u< <= >= 0>=
	0< > < 0<= 0<> 0> <> = 2dup 2drop -rot rot r@
	?dup tuck nip [ ] decimal hex rp! rp@ sp! sp@
	here bl span >in state hld dpl base scr blk
	context #vocs pad this root-voc current <ok>
	! @ 2/ and or xor invert over ) 1- 1+ 2* 0=
	rshift swap drop dup bye - + eforth words only
	forth system forth-wordlist set-order

## SUBLEQ eFORTH Quick Glossary

This small database of words consists of one word per line,
the first space delimited word being the word being
described, then a stack effect comment in between "(" and
")", then a very short description of the word. This
database consists of words found in SUBLEQ eFORTH, 
including in the `system` vocabulary.

	! ( u a -- ) Store u at at
	# ( d -- d ) Processed single digit, Pictured Numeric Output
	#-1 ( -- -1 ) push -1 on to the stack
	#0 ( -- 0 ) push 0 on to the stack
	#1 ( -- 1 ) push 1 on to the stack
	#2 ( -- 2 ) push 2 on to the stack
	#> ( d -- a u ) end Pictured Numeric Output
	#s ( d -- 0 0 ) process number into Pictured Numeric Output
	#vocs ( -- u ) maximum vocabularies that can be loaded
	$" ( "string" --, Runtime -- b ) compile string into word
	' ( "name" -- xt ) get execution token of "name"
	( ( -- ) discard everything from input stream line until )
	($) ( -- a ) used with to implement $"
	(.) ( n -- ) used to implement ".", much faster than u.
	(abort) ( n -- ) compiled by abort", abort if n non-zero
	(block) ( ca ca cu -- ) transfer to/from block storage
	(boot) ( -- ) complex boot routine stored in <boot>
	(comp) ( -- ) used to implement does>
	(const) ( -- u ) used to implement constant
	(does) ( -- ) used to implement does>
	(emit) ( c -- ) emit a single character to output always
	(error) ( u -- ) default error handler in quit loop
	(find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 )
	(literal) ( u -- ) default behavior of literal
	(marker) ( -- ) used to implement marker
	(nfa) ( u -- ) toggle name field address in last defined word
	(order) ( w voc*n n -- voc*n w n ) used in +order and -order
	(push) ( -- u ) push next cell 
	(s) ( "string" -- ) compile string into dictionary
	(search) ( a voc -- pwd pwd 1 | pwd pwd -1 | 0 a 0 ) search
	(up) ( -- u ) access user variable stored in next cell
	(user) ( -- a ) used to implement user
	(var) ( -- a ) used to implement variable
	) ( -- ) immediate, do nothing, terminate comment
	* ( n n -- n ) multiple two numbers
	+ ( n n -- n ) add two numbers
	+! ( n a -- ) add n to memory location
	+order ( voc -- ) add voc to current search order
	+string ( a u n -- ) increment string by n
	, ( n -- ) write n into the next dictionary cell
	- ( n1 n2 -- n ) subtract n2 from n1
	-cell ( -- -2 ) push the negated cell size
	-order ( voc -- ) remove voc from current search order
	-rot ( n1 n2 n3 -- n3 n1 n2 ) reverse of rot
	-trailing ( a u -- a u ) remove trailing whitespace
	. ( n -- ) display signed number in current output radix
	." ( "string" -- ) compile string into word that prints itself
	.$ ( -- ) used to implement ."
	.( ( "display" -- ) parse and emit until matching )
	.emit ( c -- ) print char, replacing non-graphic ones
	.id ( pwd -- ) print word name field
	.s ( ??? -- ??? ) display variable stack
	/ ( n1 n2 -- n ) divide n1 by n2 
	/mod ( n1 n2 -- n1%n2 n1/n2 ) divide n1 by n2 
	0< ( n -- f ) is less than zero?
	0<= ( n -- f ) is less than or equal to zero?
	0<> ( n -- f ) is not equal to zero?
	0= ( n -- f ) is equal to zero?
	0> ( n -- f ) is greater than zero?
	0>= ( n -- f ) is greater or equal to zero?
	1+ ( n -- n ) increment n
	1- ( n -- n ) decrement n
	2! ( n n a -- ) store two values at address and next cell
	2* ( u -- u ) multiply by two, bitshift left by 1
	2/ ( u -- u ) divide by two, bitshift right by 1
	2>r ( n n --, R: -- n n ) move two values to return stack
	2@ ( a -- n n ) retrieve two values from address and next cell
	2drop ( n n -- ) discard two values
	2dup ( n1 n2 -- n1 n2 n1 n2 ) duplicate two stack items
	2r> ( -- n n, R: n n -- ) move two values from return stack
	: ( "name" -- ) parse name and start word definition
	:noname ( -- xt ) start anonymous word definition
	; ( -- ) immediate, end word definition
	< ( n1 n2 -- f ) is n1 less than n2
	<# ( -- ) start Pictured Numeric Output
	<= ( n1 n2 -- f ) is n1 less than or equal to n2
	<> ( n n -- f ) are two values not equal to each other?
	<block> ( -- a ) execution vector for block
	<boot> ( -- a ) execution vector for cold
	<echo> ( -- a ) execution vector for echo
	<emit> ( -- a ) execution vector for emit
	<error> ( -- a ) execution vector for error handling
	<expect> ( -- a ) execution vector for expect
	<key> ( -- a ) execution vector for key
	<literal> ( -- a ) execution vector for literal
	<ok> ( -- a ) execution vector for okay prompt
	<quit> ( -- a ) execution vector for final boot word
	<tap> ( -- a ) execution vector for tap
	= ( n n -- f ) are two numbers equal?
	> ( n1 n2 -- f ) is n1 greater than n2?
	>= ( n1 n2 -- f ) is n1 greater or equal to n2?
	>blk ( k -- ca ) turn block into cell address
	>body ( xt -- body ) move to a created words body
	>in ( -- a ) input buffer position user variable
	>number ( ud b u -- ud b u ) convert string to number
	>r ( n --, R: -- n ) move value from variable to return stk.
	?depth ( n -- ) depth check, throw if too few stack items
	?dup ( n -- n n | 0 ) conditionally duplicate if non zero
	?exit ( n -- ) compile-only, conditionally exit word
	?found ( b f -- b ) throw if flag false with error message
	?len ( b -- b ) throw if counted string too long
	?nul ( b -- b ) throw if counted string is zero length
	?unique ( b -- b ) warn if word definition already exists
	@ ( a -- n ) retrieve contents of memory address
	@+ ( a -- a n ) get value at address, keep address
	@execute ( ??? a -- ??? ) retrieve execution token and execute
	[ ( -- ) immediate, turn command mode on
	[!] ( u ca -- ) store value at cell address
	[@] ( ca -- u ) retrieve value from cell address
	[char] ( "char" --, Runtime: -- b ) compile character literal
	[else] ( -- ) skip input until "[then]"
	[if] ( n -- ) conditional input until "[else]/[then]"
	[then] ( -- ) do nothing
	\ ( "line" -- ) discard everything from \ to end of line
	] ( -- ) turn compile mode on, command mode off
	abort ( -- ) call throw -1 unconditionally
	abort" ( "string" --, Runtime: n -- ) print abort if non-zero
	abs ( n -- u ) absolute value, beware $8000
	accept ( b u -- b u ) accept a line of input
	activate ( xt task-address -- ) activate a task
	aft ( -- ) part of for...aft...then...next loop
	again ( -- ) part of begin...again infinite loop
	align ( -- ) align dictionary pointer up
	aligned ( a -- a ) align address up
	allot ( n -- ) allocate bytes in dictionary
	and ( n n -- n ) bitwise and
	at-xy ( x y -- ) set cursor position, 1 index based
	b/buf ( -- 1024 ) number of bytes in a block
	banner ( +n c -- ) output c n times
	base ( -- a ) address of numeric input output radix 2-36
	begin ( -- ) part of a begin...until, begin...again loop
	bell ( -- ) emit ASCII bell character
	bget ( k -- ) transfer block from mass storage to buffer
	bl ( -- 32 ) push ASCII space character
	blank ( a u -- ) set array of bytes to space
	blk ( -- a ) currently loaded block
	blk0 ( -- a ) block buffer zero block loaded value
	block ( blk -- a ) load data off disk, store modified buffer
	bput ( k -- ) transfer block buffer to mass storage
	buf0 ( -- a ) address of block buffer zero
	buffer ( blk -- a ) like block but it performs no load of data
	bye ( -- ) halt system
	c! ( c a -- ) write a single byte to memory location a
	c, ( c -- ) write byte into dictionary
	c/buf ( -- 512 ) cells in a block
	c@ ( a -- c ) retrieve a single byte
	c@+ ( a -- a c ) retrieve single byte, keep address
	calibration ( -- a ) value used by ms for 1 ms wait
	catch ( xt -- n ) execute xt, catching result of any throws
	cell ( -- 2 ) size of a single cell in bytes
	cell+ ( a -- a ) increment address by cell size
	cell- ( a -- a ) decrement address by cell size
	cells ( n -- n ) turn a cell count into a byte count
	cfa ( pwd -- cfa ) move word pwd field to its code field
	char ( "char" -- c ) turn a character of input into a byte
	cksum ( a u -- u ) calculate additive checksum over range
	clean ( -- ) opposite of update, mark last loaded block clean
	cmove ( b1 b2 u -- ) copy u characters from b1 to b2
	cold ( -- ) perform a cold boot
	compare ( a1 u1 a2 u2 -- n ) compare two strings
	compile ( -- ) compile next address in word into dictionary
	compile, ( xt - ) compile execution token into word def.
	compile-only ( -- ) make previously defined word compile-only
	console ( -- ) setup input/output for terminal/console
	constant ( n "name" -- ) create a constant with value n
	context ( -- a ) array containing loaded vocs
	count ( a -- a c ) retrieve byte and increment a by 1
	cr ( -- ) emit a newline
	create ( "name" -- ) create word which pushes field address
	csi ( -- ) emit ANSI terminal escape sequence
	current ( -- a ) current vocabulary definitions are added to
	cycles ( -- a ) address of number of task switches performed
	d+ ( d d -- d ) add two double cell values
	decimal ( -- ) set input and output radix to decimal
	defined ( "name" -- f ) is "name" a defined word?
	definitions ( -- ) add future definitions to top vocabulary
	depth ( ??? -- n ) get variable stack depth
	digit ( u -- c ) extract a character from number
	dirty0 ( -- a ) dirty flag for block buffer 0
	dnegate ( d -- d ) negate double cell value
	do$ ( -- a ) push location of compiled string, jump over it
	does> ( -- ) part of `create...does>` routine
	dpl ( -- a ) address of double cell number decimal position
	drop ( n -- ) drop top of stack
	dump ( a u -- ) dump array to output
	dup ( n -- n n ) duplicate top of stack
	echo ( c -- ) emit a single character, terminal output echo
	editor ( -- ) load block editor word set, setup editor
	eforth ( -- ver ) push eforth version number
	else ( -- ) part of if...else...then statement
	emit ( c -- ) display a single character 
	empty-buffers ( -- ) call clean and invalidate
	erase ( a u -- ) write zero to array
	eval ( "line" -- ) evaluate line got with query
	evaluate ( ??? a u -- ??? ) evaluate string
	execute ( ??? xt -- ??? ) execute an execution token
	exit ( -- ) compile-only, exit current word definition
	expect ( a u -- ) calls accept, stores result in span
	extract ( ud ud -- ud u ) extract digit from number
	file ( -- ) ready I/O for file transfer instead of console
	fill ( a u c -- ) fill array with byte n
	find ( b -- pwd 1 | pwd -1 | a 0 ) find word in dictionary
	flush ( -- ) discard and un-assign dirty block buffers
	for ( --, Runtime: n --, R: -- n ) for...aft...then..next loop
	forth ( -- ) set search order to root-voc and forth-wordlist
	forth-wordlist ( -- voc ) push the default Forth vocabulary
	get-current ( -- voc ) equivalent to "current @"
	get-input ( -- n1...n5 ) get the input stream state
	get-order ( -- voc0...vocn n ) get search order
	h? ( -- a ) push the location of the dictionary pointer
	hand ( -- ) set default xt for I/O for terminal
	here ( -- u ) current dictionary position
	hex ( -- ) set number input/output radix to hexadecimal
	hide ( "name" -- ) toggle hidden bit in word definition
	hld ( -- a ) user variable index into hold space
	hold ( c -- ) add c to hold space in Pictured Numeric Output
	if ( --, Runtime: n -- ) immediate, compile-only, if-statement
	immediate ( -- ) make last defined word immediate
	info ( -- ) print system information
	ini ( -- ) initialize current task
	interpret ( b -- ) interpret a counted word
	invalidate ( -- ) invalidate blk0 storing -1 to it
	invert ( u -- u ) bitwise invert
	io! ( -- ) setup input/output routines
	key ( -- c ) get character from input
	key? ( -- c 0 | -1 ) get character from input or -1 on failure
	ktap ( bot eot cur c -- bot eor cur ) handle terminal input
	last ( -- a ) get last defined word
	leq0 ( n -- 0 | 1 ) 1 if n is less than or equal to 0, else 0
	line ( k l -- a u ) index into block by 64 byte lines
	list ( blk -- ) list a block, set scr
	literal ( n -- Runtime: -- n ) immediate, compile number
	load ( ??? blk -- ??? ) execute code stored in a block
	loaded? ( k -- k f ) check to see if block is loaded already
	loadline ( ??? k l -- ??? ) evaluate a line )
	look ( b u c xt -- b u ) skip until xt succeeds
	lshift ( u n -- u ) left shift u by n
	m/mod ( d n -- r q ) floored division with remainder/quotient
	mark ( -- a ) mark a hole in dictionary
	marker ( "name" -- ) make word that deletes words later after
	match ( c1 c2 -- f ) used with look in parse
	max ( n n -- n ) signed maximum of two numbers
	min ( n n -- n ) signed minimum of two numbers
	mod ( n1 n2 -- n1%n2 ) compute modulus of n1 divided by n2
	ms ( n -- ) wait for approximately n milliseconds
	multi ( -- ) enable multithreading mode, single turns it off
	mux ( n1 n2 sel -- n ) bitwise multiplex operation
	negate ( n -- n ) twos compliment negation
	next ( -- ) part of for...next/for..aft...then...next loop
	nfa ( pwd -- nfa ) move pwd to name field address
	nip ( n1 n2 -- n2 ) discard second stack item
	number? ( a u -- d -1 | a u 0 ) easier to use than >number
	ok ( -- ) state aware okay prompt
	only ( -- ) set vocabulary to only the root-voc
	or ( n n -- n ) bitwise or
	over ( n1 n2 -- n1 n2 n1 ) duplicate second item on stack
	pace ( -- ) emit pacing character
	pad ( -- a ) get thread local pad or scratch space
	page ( -- ) clear screen (using ANSI terminal codes)
	parse ( "string" c -- b u ) parse a c delimited string
	pause ( -- ) invoke multithreading scheduler, yield
	pick ( nu...n0 n -- nu...n0 nn ) pick item on stack
	postpone ( "name" -- ) immediate, compile word into dict.
	query ( -- ) get a line of text, filling the terminal buffer
	quit ( -- ) interpreter loop
	r> ( -- n, R: n -- ) move value from return stack to var stk.
	r@ ( -- n, R: n -- n ) copy value from return stack
	radix ( -- u ) retrieve input/output radix in base
	rdrop ( --, R: n -- ) drop value from return stack
	receive ( -- msg task-addr ) pause until message received
	recurse ( -- ) immediate, compile-only, recurse current word
	repeat ( -- ) part of begin...while...repeat loop
	root-voc ( -- voc ) push root vocabulary
	rot ( n1 n2 n3 -- n2 n3 n1 ) rotate three stack items
	rp! ( n -- , R: ??? -- ??? ) set return stack pointer
	rp@ ( -- n, R: ??? -- ??? ) get return stack pointer
	rshift ( u n -- u ) perform rshift of u by n
	s>d ( n -- d ) convert single cell number to double cell
	save-buffers ( -- ) save all block buffers to disk
	scr ( -- a ) last listed block as used with `list`
	search-wordlist ( a voc -- pwd 1| pwd -1| a 0 ) search voc
	see ( "name" -- ) decompile word
	send ( msg task-addr -- ) blocking send message to task
	set-current ( voc -- ) set current variable
	set-input ( n1...n5 -- ) set input execution tokens
	set-order ( n1...nx x -- ) set search order, -1 is special
	shed ( n1 n2 n3 -- n2 n3 ) remove third-most stack item
	sign ( -- ) add sign to hold space in Pictured Numeric Output
	signal ( addr -- ) signal to thread calling wait
	single ( -- ) force single threaded mode
	source ( -- a u ) get terminal input source
	source-id ( -- u ) get input type (0 = terminal, -1 = block)
	sp ( -- a ) variable containing the stack address
	sp! ( sp -- ) set stack pointer
	sp@ ( -- sp ) get stack pointer
	space ( -- ) emit a space character
	span ( -- a ) user variable set when calling expect
	state ( -- a ) push address of stack control location
	swap ( n1 n2 -- n2 n1 ) swap two stack items
	system ( -- voc ) push system vocabulary
	tap ( bot eot cur c -- bot eor cur ) add char to line
	task-init ( task-address -- ) initialize a task
	task: ( "name" -- ) create a named task
	then ( -- ) part of if...then or if...else...then
	this ( -- a ) address of task thread memory
	throw ( n -- ) throw n to be caught by catch, 0 is no throw
	tib ( -- b ) get the Terminal Input Buffer address
	toggle ( u a -- ) toggle bits at address [xor them with u]
	token ( "name" -- ) equivalent to "bl word"
	transfer ( a a u -- ) transfer bytes to/from mass storage
	tuck ( n1 n2 -- n1 n2 n1 ) tuck a variable behind two
	tup ( -- a ) get address of the Terminal Input Buffer
	type ( a u -- ) emit string displaying it
	u. ( u -- ) display unsigned number
	u.r ( u n -- ) display unsigned number space filled up to n
	u< ( u1 u2 -- f ) u1 unsigned less than u2
	u<=  ( u1 u2 -- f ) u1 unsigned less than or equal to u2
	u> ( u1 u2 -- f ) u1 unsigned greater than u2
	u>= ( u1 u2 -- f ) u1 unsigned greater than or equal to u2
	um* ( u u -- ud ) mixed multiply
	um+ ( u u -- u carry ) mixed add with carry
	um/mod ( ud u -- ur uq ) unsigned double cell div/mod
	unmatch ( c1 c2 -- f ) used with look in parse
	until ( --, Runtime: u -- ) part of begin...until loop
	update ( -- ) mark last loaded block as dirty or modified
	user ( "name" -- ) create a new thread local user variable
	user? ( -- a ) address of the user variable pointer
	valid? ( k -- k f ) is block valid?
	variable ( "name" -- ) create a variable
	wait ( addr -- ) pause until contents of address is non zero
	while ( --, Runtime: u -- ) part of begin/while/repeat loop
	within ( u lo hi -- f ) is u within lo and hi, lo <= u < hi
	word ( "string" c -- ) parse string until c
	words ( -- ) display loaded words
	xio ( xt xt xt -- ) exchange input/output 
	xor ( u u -- u ) bitwise exclusive or
