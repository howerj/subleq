# Albert van der Horst

From <https://home.hccnet.nl/a.w.m.van.der.horst/forthlecture6.html> as
part of a lecture series on Forth.

	\ Do a coroutine call. To be explained later.
	\ In ciforth this is in the kernel.
	: CO R> R> SWAP >R >R ;

	\ Allocate AMOUNT locals on the return stack, to be accessed
	\ by $# words.
	: LOCALS
	R> SWAP           \ Move return stack top to stack
	    RSP@ SWAP CELLS OVER SWAP -  RSP! >R  \ Allocate X cells
	>R                     \ Move back
	CO                        \ Execute caller
	R>  RSP!            \ Restore stack pointer
	;


	\ Compile the address of local variable 3.
	: $3 3 2 + CELLS POSTPONE LITERAL   POSTPONE RSP@ POSTPONE + ;
	IMMEDIATE

	\ local variables by $0 .. $9
	\ This is a denotation equivalent to the above definition.
	: $  0. (WORD) >NUMBER 2DROP DROP 2 +
	POSTPONE LITERAL
	POSTPONE CELLS POSTPONE RSP@ POSTPONE +  ; IMMEDIATE
	LATEST >FFA 8 TOGGLE

	\ Test code

	: V+ 4 LOCALS  ( a b c d -- a+c b+d )
	    $3 ! $2 ! $1 ! $0 !
	    $0 @ $2 @ +  $1 @ $3 @ +
	;

	." EXPECT : 202 101 :" 100 200 1 2 V+ . . CR

	: TOKYO
	       ." HERE TOKYO OVER" CR CO
	       ." WHAT GIVES? OVER" CR CO
	       ." YES, MORE? OVER" CR CO
	       ." OVER AND OUT" CR
	 ;

	: AMSTERDAM
	   TOKYO
	   ." HERE ASTERDAM OVER" CR CO
	   ." HAS IT ARRIVED OVER" CR CO
	   ." NO. OVER AND OUT" CR

	  ;
	." EXPECT SENSIBLE CONVERSATION:" CR AMSTERDAM
	EXIT

	: AMSTERDAM                          |   : TOKYO
	   TOKYO                             |    ." HERE TOKYO OVER" CR CO
	   ." HERE ASTERDAM OVER" CR CO      |    ." WHAT GIVES? OVER" CR CO
	   ." HAS IT ARRIVED? OVER" CR CO    |    ." YES, MORE? OVER" CR CO
	   ." NO. OVER AND OUT" CR           |    ." OVER AND OUT" CR
	;                                    |   ;

Using CO for iteration.

With the iterator FOR-WORDS the xt on the stack is executed once for
each word in the wid that is on the stack :

	CRACK WORDS

	: WORDS
	C/L   OUT   !   'ID.   CURRENT   @   FOR-WORDS
	;

The implementation of FOR-WORDS is rather ugly:

CRACK FOR-WORDS

	: FOR-WORDS
	SWAP   >R   >R   ( keep xt runner on the return stack)
	BEGIN
	    R>   R@
	    OVER   >LFA   @   >R   ( Prepare next runner)
	    EXECUTE
	R@   0= UNTIL           ( Next runner is zero)
	RDROP   RDROP     (Clean return stack)
	;

With coroutines this all becomes

	\  Break off my co-routine.
	'RDROP ALIAS CO-EXIT

	: FOR-WORDS   BEGIN DUP WHILE DUP CO >LFA @ REPEAT CO-EXIT ;

	: WORDS' C/L OUT ! CURRENT @ FOR-WORDS BEGIN ID. CO AGAIN ;
                                       
This however leaves a zero on the stack of the end of the link chain.
What if you don't want to drop that zero in FOR-WORDS ?
(Doing so introduces a kind of stack imbalance. If we force FOR-WORDS
out of business by a CO-EXIT in WORDS' , FOR-WORDS leaves a word
on the stack compared to the regular exit.)

Once more coroutines come to the rescue.

	: AFTER-DROP CO DROP ;

And we get.

	: WORDS'  C/L OUT ! CURRENT @ AFTER-DROP FOR-WORDS BEGIN ID. CO AGAIN ;

The good thing here is that we are no longer restricted to a single
word at the position of ID.

Further ideas:
Look up the STRING in wordlist WID. Leave STRING and DEA.
The dea may be a ``NULL''.

	: (FIND) FOR-WORDS BEGIN MATCH 0= WHILE CO AGAIN CO-EXIT ;

Match now only have to take care of comparing the STRING with the
name of the dictionary entry, not with any bookkeeping.

This does away with a MATCH word that zero's out the third item
on the return stack and remembers a result in a global variable (!).


