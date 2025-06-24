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

* Add questions, answers and exercises.
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
* Formatting: 64 column width
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


**TODO: exiting early, do loop**

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

### Defining `min` and `max`

This chapter contains multiple definitions of the same
words, done in different styles and for different reasons,
mainly for the purposes of micro optimizations which may
not be relevant for what you are doing or for the platform
you are on.

**TODO** check these.

	: min 2dup > if swap then drop ;
	: max 2dup < if swap then drop ;

	: min 2dup > if drop exit then nip ;
	: max 2dup < if drop exit then nip ;

	: min 2dup > if drop else nip then ;
	: max 2dup < if drop else nip then ;

	: min 2dup > mux ;
	: max 2dup < mux ;

	\ equivalent 32-bit C, needs arithmetic right shift
	\ return b + ((a-b) & (a-b)>>31); /* min */
	\ return a - ((a-b) & (a-b)>>31); /* max */
	: high? $8000 and 0<> ;
	: min 2dup nip - dup high? and + ;
	: max 2dup drop swap - dup high? and - ;
	\ 16-bit `high?` without using `and`:
	: high? $8000 + 0>= ;

## Command Mode and Compile Mode

### Defining new words


## Allocating memory

TODO: 

* , here @ ! allocate free allot 

## Pictured Numeric Output

TODO:

* Numeric input vs output

## Recursion

## Double Word Set

## CREATE and DOES>

### Forth data structures

## USER words and cooperative multithreading

## Block Word Set

### Forth Block Editor

## Vocabulary Words

TODO:

* extended words, +order, -order, (order)
* Forth storage of dictionary, storing headers with word
definitions or separately.

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





