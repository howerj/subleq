: debug source type ."  ok" cr ; ' debug <ok> !

\ This is a Forth test bench for: <https://github.com/howerj/embed>, it also
\ contains extensions to the base interpreter, such as floating point support.
\
\ The test bench consists of a few support words, and three words that should
\ be used together, they are 'T{', '->' and '}T'.
\
\ 'T{' sets up the test, the test itself should appear on a single line, with
\ the '}T' terminating it. The arguments to a function to test and function to
\ test should appear to the left of the '->' word, and the values it returns
\ should to the right of it. The test bench must also account for any items
\ already on the stack prior to calling 'T{' which must be ignored.
\
\ A few other words are also defined, but they are not strictly needed, they
\ are 'throws?' and 'statistics'. 'throws?' parses the next word in the
\ input stream, and executes it, catching any exceptions. It empties the
\ variable stack and only returns the exception number thrown. This can be
\ used to test that words throw the correct exception in given circumstances.
\ 'statistics' is used for information about the tests; how many tests failed,
\ and how many tests were executed.
\
\ The test benches are not only used to test the internals of the Forth system,
\ and their edge cases, but also to document how the words should be used, so
\ words which this test bench relies on and trivial words are also tested. The
\ best test bench is actually the cross compilation method used to create new
\ images with the metacompiler, it tests nearly every single aspect of the
\ Forth system.
\
\ It might be worth setting up another interpreter loop until the corresponding
\ '}T' is reached so any exceptions can be caught and dealt with.
\
\ The organization of this file needs to be improved, it also contains
\ some useful extensions to the language not present in the 'embed.fth' file.

\ A few generic helper words will be built, to check if a word is defined, or
\ not, and to conditionally execute a line.

only forth definitions system +order

\ Create anonymous namespace:
: anonymous get-order 1+ here 1 cells allot swap set-order ;

: undefined? bl word find nip 0= ; ( "name", -- f: Is word not in search order? )
: defined? undefined? 0= ;       ( "name", -- f: Is word in search order? )
: ?\ 0= if postpone \ then ;    ( f --, <string>| : conditional compilation )

\ As a space saving measure some standard words may not be defined in the
\ core Forth image. If they are not defined, we define them here.
undefined? 0<   ?\ : 0< 0 < ;
undefined? 1-   ?\ : 1- 1 - ;
undefined? 2*   ?\ : 2* 1 lshift ;
\ undefined? rdup ?\ : rdup r> r> dup >r >r >r ;
undefined? 1+!  ?\ : 1+! 1 swap +! ;
\ undefined? -throw ?\ : -throw negate throw ;

: within over - >r - r> u< ; ( u lo hi -- f )
\ : dnegate invert >r invert 1 um+ r> + ; ( d -- d )
: arshift ( n u -- n : arithmetic right shift )
  2dup rshift >r swap $8000 and
  if $10 swap - -1 swap lshift else drop 0 then r> or ;
\ : 2/  1 rshift ; ( u -- u : non compliant version of '2/' )
: d2* over $8000 and >r 2* swap 2* swap r> if 1 or then ;
: d2/ dup      1 and >r 2/ swap 2/ r> if $8000 or then swap ;
\ : d+  >r swap >r um+ r> + r> + ; 
\ : d+ rot + -rot um+ rot + ;
: d- dnegate d+ ;
: d= rot = -rot = and ;
: d0= or 0= ;
: d0<> d0= 0= ;
: 2swap >r -rot r> -rot ;
\ : s>d  dup 0< ;                ( n -- d )
: dabs s>d if dnegate then ;   ( d -- ud )
: 2over ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
  >r >r 2dup r> swap >r swap r> r> -rot ;
: 2, , , ;
: 2constant create 2, does> 2@ ;
: 2variable create 0 , 0 , ; \ does> ;
: 2literal swap postpone literal postpone literal ; immediate
: +- 0< if negate then ; ( n n -- n : copy sign )
: >< dup 8 rshift swap 8 lshift or ; ( u -- u : byte swap )
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
: nand and invert ;  ( u u -- u )
: nor  or invert ;  ( u u -- u )
: @bits swap @ and ; ( a u -- u )
: ascii state @ if postpone [char] else char then ; immediate

: /string over min rot over + -rot - ;  ( b u1 u2 -- b u : advance string u2 )

: ccitt ( crc c -- crc : crc polynomial $1021 AKA "x16 + x12 + x5 + 1" )
  over $8 rshift xor   ( crc x )
  dup  $4 rshift xor   ( crc x )
  dup  $5 lshift xor   ( crc x )
  dup  $C lshift xor   ( crc x )
  swap $8 lshift xor ; ( crc )

: crc ( b u -- u : calculate ccitt-ffff CRC )
  $FFFF ( -1 = 0xffff ) >r
  begin
    ?dup
  while
   over c@ r> swap ccitt >r +string
  repeat r> nip ;

: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r          ( dl dh nn -- dl dh,      R: -- dh nn )
  dabs r@ abs um/mod  ( dl dh    -- rem quo,    R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;

: */mod ( a b c -- rem a*b/c : use double precision intermediate value )
    >r m* r> sm/rem ;

\ We can define some more functions to test to make sure the arithmetic
\ functions, control structures and recursion works correctly, it is
\ also handy to have these functions documented somewhere in case they come
\ in use
: factorial ?dup 0= if 1 exit then >r 1 r> 1- for r@ 1+ * next ; ( u -- u )
: permutations over swap - factorial swap factorial swap / ; ( u1 u2 -- u )
: combinations dup dup permutations >r permutations r> / ;   ( u1 u2 -- u )
: gcd dup if tuck mod recurse exit then drop ;               ( u1 u2 -- u )
: lcm 2dup gcd / * ; \ Least Common Multiple                 ( u1 u2 -- u )
: square dup * ;                                             ( u -- u )
\ : limit rot min max ;                                      ( u hi lo -- u )
\ : sum 1- 0 $7FFF limit for aft + then next ;               ( a0...an n -- n )

\ *merge* takes two word lists and appends 'wid1' to 'wid2. Most of the
\ complexity is down to the fact that words in this eForth implementation
\ store word related information in the PWD field (which points to the
\ previous word). This needs masking off when traversing the list and
\ preserving when modifying it.
\

\ : >@ $3FFF and ;     ( a -- a : address with attribute bits masked off )
\ : >attr $C000 and ;  ( a -- u : get attribute bits from an address )
\ : link! dup @ >attr rot >@ or swap ! ; ( u a -- ) 
\ : link >@ @ >@ ;
\ : merge swap @ swap begin link dup link 0= until link! ; ( wid1 wid2 -- )

\ This virtual machine has no concept of time, however with some manual input 
\ from the user and the awesome power of a busy waiting loop it is possible
\ to define a system that can be calibrated so that the word *ms* waits for
\ approximately one millisecond. To do this, run the word *calibrate* and
\ start a stop watch at the same time, stop it when *calibrate* prints '0'.
\ You can use this number to work out a correct number to store in the double
\ cell value *ms0*. On my machine '$780.' (running at 2.4GHz) gives a time of 
\ '1 minute, 0.1 seconds', another gave '1 minute, 0 seconds', to increase 
\ the precision of this calibration run it for longer and do multiple 
\ calibrations, and average the results. The correct timing of this depends 
\ on the accuracy of your calibration, but also the machines speed this is 
\ running under and the load it is under.
\
\ 2variable ms0 $780. ms0 2!
\ : 1ms ms0 2@ begin 2dup d0<> while 1 s>d d- repeat 2drop ;
\ : ms for 1ms next ;
\ : 1s 999 ms ;       ( delay for approximately 1 second )
\ : calibrate ." START TIMER" cr 59 for 1s r@ . cr next ." DONE" cr ;
\ hide 1ms hide 1s

\ From: https://en.wikipedia.org/wiki/Integer_square_root
\ This function computes the integer square root of a number.
: sqrt ( n -- u : integer square root )
  s>d  if -$B throw then ( does not work for signed values )
  dup 2 < if exit then      ( return 0 or 1 )
  dup                       ( u u )
  2 rshift recurse 2*       ( u sc : 'sc' == unsigned small candidate )
  dup                       ( u sc sc )
  1+ dup square             ( u sc lc lc^2 : 'lc' == unsigned large candidate )
  >r rot r> <               ( sc lc bool )
  if drop else nip then ;   ( return small or large candidate respectively )

: log ( u base -- u : compute the integer logarithm of u in 'base' )
  >r
  dup 0= if -$B throw then ( logarithm of zero is an error )
  0 swap
  begin
    swap 1+ swap r@ / dup 0= ( keep dividing until 'u' is 0 )
  until
  drop 1- rdrop ;

: log2 2 log ; ( u -- u : compute the integer logarithm of u in base )

\ http://forth.sourceforge.net/algorithm/bit-counting/index.html
: count-bits ( number -- bits )
  dup $5555 and swap 1 rshift $5555 and +
  dup $3333 and swap 2 rshift $3333 and +
  dup $0F0F and swap 4 rshift $0F0F and +
  $FF mod ;

\ http://forth.sourceforge.net/algorithm/firstbit/index.html
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

: binary $2 base ! ;

\ : + begin dup while 2dup and 1 lshift >r xor r> repeat drop ;

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

\ ========================= CORDIC CODE =======================================

anonymous definitions
create lookup ( 16 values )
$3243 , $1DAC , $0FAD , $07F5 , $03FE , $01FF , $00FF , $007F ,
$003F , $001F , $000F , $0007 , $0003 , $0001 , $0000 , $0000 ,

$26DD constant cordic_1K $6487 constant pi/2

variable tx 0 tx ! variable ty 0 ty ! variable tz 0 tz !
variable x  0  x ! variable y  0  y ! variable z  0  z !
variable d  0  d ! variable k  0  k !

forth-wordlist current ! 

( CORDIC: valid in range -pi/2 to pi/2, arguments are in fixed )
( point format with 1 = 16384, angle is given in radians.  )
: cordic ( angle -- sine cosine )
  z ! cordic_1K x ! 0 y ! 0 k !
  $10 begin ?dup while
    z @ 0< d !
    x @ y @ k @ arshift d @ xor d @ - - tx !
    y @ x @ k @ arshift d @ xor d @ - + ty !
    z @ k @ cells lookup + @ d @ xor d @ - - tz !
    tx @ x ! ty @ y ! tz @ z !
    k 1+!
    1-
  repeat y @ x @ ;
: sin cordic drop ; ( rad/16384 -- sin : fixed-point sine )
: cos cordic nip ;  ( rad/16384 -- cos : fixed-point cosine )

only forth definitions

\ ========================= CORDIC CODE =======================================

\ ========================= FLOATING POINT CODE ===============================

\ 
\ 	Vierte Dimension Vol.2, No.4 1986
\ 
\ 
\ 		A FAST HIGH-LEVEL FLOATING POINT
\ 
\ 			Robert F. Illyes
\ 
\ 			     ISYS
\ 		       PO Box 2516, Sta. A
\ 		       Champaign, IL 61820
\ 		       Phone: 217/359-6039
\ 
\ 	If binary normalization and rounding are used, a fast
\ 	single-precision FORTH floating point can be built with
\ 	accuracy adequate for many applications. The creation
\ 	of such high-level floating points has become of more
\ 	than academic interest with the release of the Novix
\ 	FORTH chip. The FORTH-83 floating point presented here
\ 	is accurate to 4.8 digits. Values may range from about
\ 	9E-4933 to about 5E4931. This floating point may be
\ 	used without fee provided the copyright notice and
\ 	associated information in the source are included.
\ 
\ 	FIXED VS. FLOATING POINT
\ 
\ 	FORTH programmers have traditionally favored fixed over
\ 	floating point. A fixed point application is harder to
\ 	write. The range of each value must be known and
\ 	considered carefully, if adequate accuracy is to be
\ 	maintained and overflow avoided. But fixed point
\ 	applications are much more compact and often much
\ 	faster than floating point (in the absence of floating
\ 	point hardware).
\ 
\ 	The debate of fixed vs. floating point is at least as
\ 	old as the ENIAC, the first electronic digital
\ 	computer. John von Neumann used fixed point on the
\ 	ENIAC. He felt that the detailed understanding of
\ 	expressions required by fixed point was desirable, and
\ 	that fixed point was well worth the extra time (1).
\ 
\ 	But computers are no longer the scarce resource that
\ 	they once were, and the extra programming time is often
\ 	more costly than any advantages offered by fixed point.
\ 	For getting the most out of the least hardware,
\ 	however, fixed point will always be the technique of
\ 	choice.
\ 
\ 	Fixed point arithmetic represents a real number as a
\ 	ratio of integers, with an implicit divisor. This
\ 	implicit divisor may be thought of as the
\ 	representation of unity. If unity were represented by
\ 	300, for example, 2.5 would be represented by 750.
\ 
\ 	To multiply 2.5 by 3.5, with all values representing
\ 	unity as ten, one would evaluate
\ 
\ 			     25 * 35
\ 			     -------
\ 			       10
\ 
\ 	The ten is called a scale factor, and the division by
\ 	ten is called a scaling operation. This expression is
\ 	obviously inefficient, requiring both a division and a
\ 	multiplication to accomplish a multiplication.
\ 
\ 	Most scaling operations can, however, be eliminated by
\ 	a little algebraic manipulation. In the case of the sum
\ 	of two squares, for example, where A and B are fixed
\ 	point integers and unity is represented by the integer
\ 	U,
\ 
\ 	      A * A   B * B           (A * A)+(B * B)
\ 	      ----- + -----    -->    ---------------
\ 		U       U                    U
\ 
\ 	In addition to the elimination of a division by U, the
\ 	right expression is more accurate. Each division can
\ 	introduce some error, and halving the number of
\ 	divisions halves the worst-case error.
\ 
\ 	DECIMAL VS. BINARY NORMALIZATION
\ 
\ 	A floating point number consists of two values, an
\ 	exponent and a mantissa. The mantissa may represent
\ 	either an integer or a fraction. The exponent and the
\ 	mantissa are related to each other in the same way as
\ 	the value and power of ten in scientific notation are
\ 	related.
\ 
\ 	A mantissa is always kept as large as possible. This
\ 	process is called normalization. The following
\ 	illustrates the action of decimal normalization with an
\ 	unsigned integer mantissa:
\ 
\ 		 Value       Stack representation
\ 		       4
\ 
\ 		 5 * 10         50000  0  --
\ 		       3
\ 		   * 10          7000  0  --
\ 		       3
\ 		   * 10         50000 -1  --
\ 
\ 	The smallest the mantissa can become is 6554. If a
\ 	mantissa of 6553 is encountered, normalization requires
\ 	that it be made 65530, and that the exponent be
\ 	decremented. It follows that the worst-case error in
\ 	representing a real number is half of 1 part in 6554,
\ 	or 1 part in 13108. The error is halved because of the
\ 	rounding of the real number to the nearest floating
\ 	point value.
\ 
\ 	Had we been using binary normalization, the smallest
\ 	mantissa would have been 32768, and the worst case
\ 	error in representation would have been 1 part 65536,
\ 	or 1/5 that of decimal normalization. LOG10(65536) is
\ 	4.8, the accuracy in decimal digits.
\ 
\ 	As with fixed point, scaling operations are required by
\ 	floating point. With decimal normalization, this takes
\ 	the form of division and multiplication by powers of
\ 	ten. Unlike fixed point, no simple algebraic
\ 	manipulation will partly eliminate the scale factors.
\ 	Consequently there are twice as many multiplications
\ 	and divisions as the floating point operators would
\ 	seem to imply. Due to the presence of scaling in 73% of
\ 	decimally normalized additions (2), the amount is
\ 	actually somewhat over twice.
\ 
\ 	With binary normalization, by contrast, this extra
\ 	multiplication effectively disappears. The scaling by
\ 	a power of two can usually be handled with a single
\ 	shift and some stack manipulation, all fast operations.
\ 
\ 	Though a decimally normalized floating point can be
\ 	incredibly small (3), a binary normalized floating
\ 	point has 1/5 the error and is about twice as fast.
\ 
\ 	It should be mentioned that the mantissa should be
\ 	multiples of 2 bytes if the full speed advantage of
\ 	binary normalization is to be available. Extra shifting
\ 	and masking operations are necessary with odd byte
\ 	counts when using the 2-byte arithmetic of FORTH.
\ 
\ 	NUMBER FORMAT AND ARITHMETIC
\ 
\ 	This floating point package uses an unsigned single
\ 	precision fraction with binary normalization,
\ 	representing values from 1/2 to just under 1. The high
\ 	bit of the fraction is always set.
\ 
\ 	The sign of the floating point number is carried in the
\ 	high bit of the single precision exponent, The
\ 	remaining 15 bits of the exponent represent a power of
\ 	2 in excess 4000 hex. The use of excess 4000 permits
\ 	the calculation of the sign as an automatic outcome of
\ 	exponent arithmetic in multiplication and division.
\ 
\ 	A zero floating point value is represented by both a
\ 	zero fraction and a zero exponent. Any operation that
\ 	produces a zero fraction also zeros the exponent.
\ 
\ 	The exponent is carried on top of the fraction , so the
\ 	sign may be tested by the phrase DUP 0< and zero by the
\ 	phrase DUP 0= .
\ 
\ 	The FORTH-83 Double Number Extension Word Set is
\ 	required. Floating point values are used with the "2"
\ 	words: 2CONSTANT, 2@, 2DUP, etc.
\ 
\ 	There is no checking for exponent overflow or underflow
\ 	after arithmetic operation, nor is division by zero
\ 	checked for. The rationale for this is the same as with
\ 	FORTH integer arithmetic. By requiring that the user
\ 	add any such tests, 1) all arithmetic isn't slowed by
\ 	tests that are only sometimes needed and 2) the way in
\ 	which errors are resolved may be determined by the
\ 	user. The extremely large exponent range makes exponent
\ 	overflow and underflow quite unlikely, of course.
\ 
\ 	All of the arithmetic is rounding. The failure to round
\ 	is the equivalent of throwing a bit of accuracy away.
\ 	The representational accuracy of 4.8 digits will be
\ 	quickly lost without rounding arithmetic.
\ 
\ 	The following words behave like their integer
\ 	namesakes:
\ 
\ 	     F+  F-  F*  F/  F2*  F2/  FABS  FNEGATE  F<
\ 
\ 	Single precision integers may be floated by FLOAT, and
\ 	produced from floating point by FIX and INT, which are
\ 	rounding and truncating, respectively. DFLOAT floats a
\ 	double precision integer.
\ 
\ 	NUMBER INPUT AND OUTPUT
\ 
\ 	Both E and F formats are supported. A few illustrations
\ 	should suffice to show their usage. An underscore
\ 	indicates the point at which the return key is pressed.
\ 	PLACE determines the number of places to the right of
\ 	the decimal point for output only.
\ 
\ 		   12.34  F      F. _ 12.340
\ 		   12.34  F      E. _ 1.234E1
\ 		   .033 E -1002  E. _ 3.300E-1004
\ 
\ 		   4 PLACES
\ 
\ 		   2. F -3. F F/ F. _ -0.6667
\ 		   2. F -3. F F/ E. _ -6.6667E-1
\ 
\ 	F and E will correctly float any input string
\ 	representing a signed double precision number. There
\ 	may be as many as 9 digits to the right of the decimal
\ 	point. Numbers input by E are accurate to over 4
\ 	digits. F is accurate to the full 4.8 digits if there
\ 	are no digits to the right of the decimal point.
\ 	Conversion is slightly less accurate with zeros to the
\ 	right of the decimal point because a division by a
\ 	power of ten must be added to the input conversion
\ 	process.
\ 
\ 	F and E round the input value to the nearest floating
\ 	point value. So a sixth digit will often allow a more
\ 	accurately rounded conversion, even thought the result
\ 	is only accurate to 4.8 digits. There is no advantage
\ 	to including trailing zeros, however. In many floating
\ 	points, this extra accuracy can only be achieved by the
\ 	inconvenient procedure of entering the values as
\ 	hexadecimal integers.
\ 
\ 	Only the leftmost 5 digits of the F. output are
\ 	significant. F. displays values from 32767 to -32768,
\ 	with up to 4 additional places to the right of the
\ 	decimal point. The algorithm for F. avoids double
\ 	rounding by using integer rather than floating point
\ 	multiplication to scale by a power of ten. This gives
\ 	as much accuracy as possible at the expense of a
\ 	somewhat limited range. Since a higher limit on size
\ 	would display digits that are not significant, this
\ 	range limit does not seem at all undesirable.
\ 
\ 	Like E input, E. is accurate to somewhat over 4 digits.
\ 	The principal source of inaccuracy is the function EXP,
\ 	which calculates powers of 2.
\ 
\ 	The following extended fraction is used by EXP. It
\ 	gives the square root of 2 to the x power. The result
\ 	must be squared to get 2 to the x.
\ 
\ 			      2x
\ 		 1 + ---------------------------
\ 				      57828
\ 		     34.668 - x - --------------
\ 						2
\ 				  2001.18 + (2x)
\ 
\ 	In order to do E format I/O conversion, one must be
\ 	able to evaluate the expressions
\ 
\ 		 a   a/log10(2)        b    b*log10(2)
\ 	       10 = 2           and   2 = 10
\ 
\ 	These log expressions may be easily evaluated with
\ 	great precision by applying a few fixed point
\ 	techniques. First, a good rational approximation to
\ 	log10(2) is needed.
\ 
\ 		    log10(2)     = .3010299957
\ 		    4004 / 13301 = .3010299978
\ 
\ 	The following code will convert an integer power of
\ 	ten, assumed to be on the stack, into a power of 2:
\ 
\ 		       13301 4004 */MOD >R
\ 		       FLOAT 4004 FLOAT F/ EXP
\ 		       R> +
\ 
\ 	The first line divides the power of ten by log10(2) and
\ 	pushes the quotient on the return stack. The quotient
\ 	is the integer part of the power of two.
\ 
\ 	The second line finds the fractional part of the power
\ 	of two by dividing the remainder by the divisor. This
\ 	floating point fractional part is evaluated using EXP.
\ 
\ 	The third line adds the integer power of two into the
\ 	exponent of the floating point value of the fractional
\ 	part, completing the conversion.
\ 
\ 	The inverse process is used to convert a power of 2 to
\ 	a power of ten.
\ 
\ 	FORTH-83 LIMITATIONS
\ 
\ 	Perhaps the most serious deficiency in the FORTH-83
\ 	with extensibility as its pre-eminent feature, it is
\ 	surprisingly difficult to write standard code that will
\ 	alter the processing of numeric input strings by the
\ 	interpreter and compiler.
\ 
\ 	It is usually a simple matter to replace the system
\ 	conversion word (usually called NUMBER) with a routine
\ 	of ones choice. But there if no simple standard way of
\ 	doing this. The interpreter, compiler and abort
\ 	language are all interwoven, and may all have to be
\ 	replaced if a standard solution is sought.
\ 
\ 	This floating point package assumes that double
\ 	precision integers are generated if the numeric input
\ 	string contains a period, and that a word PLACES can be
\ 	written that will leave the number of digits to the
\ 	right of the period. This does not seem to be
\ 	guaranteed by FORTH-83, although it may be safely
\ 	assumed on most systems that include double precision.
\ 
\ 	If you know how this part of your system works, you
\ 	will probably want to eliminate the words E and F, and
\ 	instead force floating point conversion of any input
\ 	string containing a period. Double precision integers
\ 	could still be generated by using a comma or other
\ 	punctuation.
\ 
\ 	It is suggested that future FORTH standards include the
\ 	word NUMBER, which is a vector to the current input
\ 	numeric word.
\ 
\ 	It is also suggested that the Double Number Extension
\ 	Wordset specification include a requirement that the
\ 	interpreter and compiler be able to accept input
\ 	strings specifying double precision values.
\ 
\ 	COMMENTS ON THE FOLLOWING CODE
\ 
\ 	The words ". and "- leave the ASCII values for period
\ 	and minus, respectively. Replace these with whatever
\ 	language you prefer for insertion of ASCII values.
\ 
\ 	The size of F+ can be considerably reduced at the
\ 	expense of quite a lot of execution speed. Think twice
\ 	before you simplify it.
\ 
\ 	The normalizing word NORM expects the stack value under
\ 	the exponent to be a double precision signed integer.
\ 	It leaves a normalized floating point number, rounding
\ 	the double precision integer into the fraction.
\ 
\ 	ALIGN and RALIGN expect an integer shift count with an
\ 	unsigned double precision number beneath. They leave
\ 	double precision unsigned integer results. At least one
\ 	shift is always performed. RALIGN rounds after
\ 	alignment.
\ 
\ 	UM/ divides an unsigned double precision number by an
\ 	unsigned single precision number, and rounds the single
\ 	precision quotient.
\ 
\ 	ZERO forces a floating point value with a zero fraction
\ 	to also have a zero exponent.
\ 
\ 	FSIGN applies the sign of the stack value under the
\ 	exponent to the exponent. The double precision integer
\ 	under an exponent is left unsigned.
\ 
\ 	FEXP evaluates a power of e. It is included because it
\ 	is a trivial but useful application of EXP.
\ 
\ 	GET converts the next word in the input stream into a
\ 	single precision signed integer.
\ 
\ 	REFERENCES
\ 
\ 	1. Von Neumann, J., John von Neumann Collected Works,
\ 	vol. 5, p.113.
\ 
\ 	2. Knuth, D. K., The Art of Computer Programming,
\ 	second edition, vol. 2, pp. 238,9.
\ 
\ 	3. Tracy, M., Zen Floating Point, 1984 FORML Conference
\ 	Proceedings, pp. 33-35.
\ 
\ 
\ 	( FORTH-83 FLOATING POINT.
\ 	  ----------------------------------
\ 	  COPYRIGHT 1985 BY ROBERT F. ILLYES
\ 
\ 		PO BOX 2516, STA. A
\ 		CHAMPAIGN, IL 61820
\ 		PHONE: 217/826-2734  )     HEX
\ 
\ 	: ZERO  OVER 0= IF DROP 0 THEN ;
\ 	: FNEGATE 8000 XOR ZERO ;
\ 	: FABS  7FFF AND ;
\ 	: NORM  >R 2DUP OR
\ 		IF BEGIN DUP 0< NOT
\ 		   WHILE D2* R> 1- >R
\ 		   REPEAT SWAP 0< - ?DUP
\ 		   IF R> ELSE 8000 R> 1+ THEN
\ 		ELSE R> DROP THEN ;
\ 
\ 	: F2*   1+ ZERO ;
\ 	: F*    ROT + 4000 - >R UM* R> NORM ;
\ 	: FSQ   2DUP F* ;
\ 
\ 	: F2/   1- ZERO ;
\ 	: UM/   DUP >R UM/MOD SWAP R>
\ 		OVER 2* 1+ U< SWAP 0< OR - ;
\ 	: F/    ROT SWAP - 4000 + >R
\ 		0 ROT ROT 2DUP U<
\ 		IF   UM/ R> ZERO
\ 		ELSE >R D2/ FABS R> UM/ R> 1+
\ 		THEN ;
\ 
\ 	: ALIGN 20 MIN 0 DO D2/ LOOP ;
\ 	: RALIGN 1- ?DUP IF ALIGN THEN
\ 		1 0 D+ D2/ ;
\ 	: FSIGN FABS OVER 0< IF >R DNEGATE R>
\ 		8000 OR THEN ;
\ 
\ 	: F+    ROT 2DUP >R >R FABS SWAP FABS -
\ 		DUP IF DUP 0<
\ 			IF   ROT SWAP  NEGATE
\ 			     R> R> SWAP >R >R
\ 			THEN 0 SWAP RALIGN
\ 		THEN SWAP 0 R> R@ XOR 0<
\ 		IF   R@ 0< IF 2SWAP THEN D-
\ 		     R> FSIGN ROT SWAP NORM
\ 		ELSE D+ IF 1+ 2/ 8000 OR R> 1+
\ 			ELSE R> THEN THEN ;
\ 
\ 	: F-    FNEGATE F+ ;
\ 	: F<    F- 0< SWAP DROP ;
\ 
\ 	( FLOATING POINT INPUT/OUTPUT ) DECIMAL
\ 
\ 	CREATE PL 3 , HERE  ,001 , ,   ,010 , ,
\ 		  ,100 , ,            1,000 , ,
\ 		10,000 , ,          100,000 , ,
\ 	     1,000,000 , ,       10,000,000 , ,
\ 	   100,000,000 , ,    1,000,000,000 , ,
\ 
\ 	: TENS  2* 2* LITERAL + 2@ ;     HEX
\ 	: PLACES PL ! ;
\ 	: SHIFTS FABS 4010 - DUP 0< NOT
\ 		ABORT" TOO BIG" NEGATE ;
\ 	: F#    >R PL @ TENS DROP UM* R> SHIFTS
\ 		RALIGN PL @ ?DUP IF 0 DO # LOOP
\ 		". HOLD THEN #S ROT SIGN ;
\ 	: TUCK  SWAP OVER ;
\ 	: F.    TUCK <# F# #> TYPE SPACE ;
\ 	: DFLOAT 4020 FSIGN NORM ;
\ 	: F     DFLOAT POINT TENS DFLOAT F/ ;
\ 	: FCONSTANT F 2CONSTANT ;
\ 
\ 	: FLOAT DUP 0< DFLOAT ;
\ 	: -+    DROP SWAP 0< IF NEGATE THEN ;
\ 	: FIX   TUCK 0 SWAP SHIFTS RALIGN -+ ;
\ 	: INT   TUCK 0 SWAP SHIFTS  ALIGN -+ ;
\ 
\ 	1.      FCONSTANT ONE DECIMAL
\ 	34.6680 FCONSTANT X1
\ 	-57828. FCONSTANT X2
\ 	2001.18 FCONSTANT X3
\ 	1.4427  FCONSTANT X4
\ 
\ 	: EXP   2DUP INT DUP >R FLOAT F-
\ 		F2* X2 2OVER FSQ X3 F+ F/
\ 		2OVER F2/ F-     X1 F+ F/
\ 		ONE F+ FSQ R> + ;
\ 	: FEXP  X4 F* EXP ;
\ 	: GET   BL WORD DUP 1+ C@ "- = TUCK -
\ 		0 0 ROT CONVERT DROP -+ ;
\ 	: E     F GET >R R@ ABS 13301 4004 */MOD
\ 		>R FLOAT 4004 FLOAT F/ EXP R> +
\ 		R> 0< IF F/ ELSE F* THEN ;
\ 
\ 	: E.    TUCK FABS 16384 TUCK -
\ 		4004 13301 */MOD >R
\ 		FLOAT 4004 FLOAT F/ EXP F*
\ 		2DUP ONE F<
\ 		IF 10 FLOAT F* R> 1- >R THEN
\ 		<# R@ ABS 0 #S R> SIGN 2DROP
\ 		"E HOLD F# #>     TYPE SPACE ;
\ 
\ 

\ This floating point library has been adapted from one found in
\ Vierte Dimensions Vol.2, No.4 1986, it should be free to use so long as the
\ following copyright is left in the code:
\ 
\	     FORTH-83 FLOATING POINT.
\	----------------------------------
\	COPYRIGHT 1985 BY ROBERT F. ILLYES
\
\	      PO BOX 2516, STA. A
\	      CHAMPAIGN, IL 61820
\	      PHONE: 217/826-2734 
\
\ NB. There is not under or overflow checking, nor division by zero checks
only forth definitions system +order
variable float-voc

 

: zero  over 0= if drop 0 then ; ( f -- f : zero exponent if mantissa is )
: norm  >r 2dup or               ( f -- f : normalize input float )
        if begin s>d invert
           while d2* r> 1- >r
           repeat swap 0< - ?dup
           if r> else $8000 r> 1+ then
        else r> drop then ;
: lalign $20 min for aft d2/ then next ;
: ralign 1- ?dup if lalign then 1 0 d+ d2/ ;

: f@ 2@ ;              ( a -- f )
: f! 2! ;              ( f a -- )
: falign align ;       ( -- )
: faligned aligned ;   ( a -- a )
: fdepth depth ;       ( -- u )
: fdup 2dup ;          ( f -- f f )
: fswap 2swap ;        ( f1 f2 -- f2 f1 )
: fover 2over ;        ( f1 f2 -- f1 f2 f1 )
: f2dup fover fover ;  ( f1 f2 -- f1 f2 f1 f2 )
: fdrop 2drop ;        ( f -- )
: fnip fswap fdrop ;   ( f1 f2 -- f2 )
: fnegate $8000 xor zero ;                  ( f -- f )
: fabs  $7FFF and ;                         ( f -- f )
: fsign fabs over 0< if >r dnegate r> $8000 or then ;


: f2*   1+ zero ;                          ( f -- f )
: f*    rot + $4000 - >r um* r> norm ;     ( f f -- f )
: fsq   fdup f* ;                          ( f -- f )
: f2/   1- zero ;                          ( f -- f )
: um/   dup >r um/mod swap r> over 2* 1+ u< swap 0< or - ;
: f0=   zero d0= ;                       ( f -- f )

: f/    
\	fdup f0= if -44 throw then
        rot swap - $4000 + >r
        0 -rot 2dup u<
        if   um/ r> zero
        else >r d2/ fabs r> um/ r> 1+
        then ;


: f+  rot 2dup >r >r fabs swap fabs - ( f f -- f : floating point addition )
        dup if s>d
                if   rot swap  negate
                     r> r> swap >r >r 
                then 0 swap ralign
        then swap 0 r> r@ xor 0< 
        if   r@ 0< if 2swap then d-
             r> fsign rot swap norm 
        else d+ if  1+ 2/ $8000 or r> 1+
                else r> then then  ;

: f- fnegate f+ ;      ( f1 f2 -- t : floating point subtract )
: f< f- 0< nip ;       ( f1 f2 -- t : floating point less than )
: f> fswap f< ;        ( f1 f2 -- t : floating point greater than )
: fmin f2dup f< if fdrop exit then fnip ; ( f1 f2 -- f : min of two floats )
: fmax f2dup f> if fdrop exit then fnip ; ( f1 f2 -- f : max of two floats )

( floating point input/output ) 
decimal

create precision 3 , 
          .001 , ,        .010 , ,
          .100 , ,       1.000 , ,
        10.000 , ,     100.000 , ,
      1000.000 , ,   10000.000 , ,
    100000.000 , , 1000000.000 , ,

: floats 2* cells ;   ( u -- u )
: float+ 1 floats + ; ( a -- a )
: tens 2* cells  [ precision cell+ ] literal + 2@ ;     

: set-precision dup 0 $5 within if precision ! exit then -$2B throw ; ( +n -- )
: shifts fabs $4010 - s>d invert if -$2B throw then negate ;
: f#    base @ $A <> if -$28 throw then
	>r precision @ tens drop um* r> shifts
        ralign precision @ ?dup if for aft # then next
        [char] . hold then #s rot sign ;

: f.    tuck <# f# #> type space ;
: d>f $4020 fsign norm ;           ( d -- f : double to float )
: f     d>f dpl @ tens d>f f/ ;    ( d -- f : formatted double to float )
: fconstant f 2constant ;          ( "name" , f --, Run Time: -- f )
: fliteral  f postpone 2literal ; immediate ( f --, Run Time: -- f )
: s>f   s>d d>f ;                  ( n -- f )
: -+    drop swap 0< if negate then ;
: fix   tuck 0 swap shifts ralign -+ ;
: f>s   tuck 0 swap shifts lalign -+ ; ( f -- n )

1. fconstant one 

: f0<  [ 0. ] fliteral f< ;       ( f     -- t )

: exp   2dup f>s dup >r s>f f-     ( f -- f : raise 2.0 to the power of 'f' )
        f2* [ -57828. ] fliteral 2over fsq [ 2001.18 ] fliteral f+ f/
        2over f2/ f- [ 34.6680 ] fliteral f+ f/
        one f+ fsq r> + ;
: fexp  [ 1.4427 ] fliteral f* exp ; ( f -- f : raise e to the power of 'f' )
: get   bl word dup 1+ c@ [char] - = tuck -
        0 0 rot ( convert drop ) count >number nip 0<> throw -+ ;
: e     f get >r r@ abs 13301 4004 */mod
        >r s>f 4004 s>f f/ exp r> +
        r> 0< if f/ else f* then ;

: e.    tuck fabs 16384 tuck -
        4004 13301 */mod >r
        s>f 4004 s>f f/ exp f*
        2dup one f<
        if 10 s>f f* r> 1- >r then
        <# r@ abs 0 #s r> sign 2drop
        [char] e hold f# #>     type space ;

: fexpm1 fexp one f- ;                      ( f -- f : e raised to 'f' less 1 )
: fsinh fexpm1 fdup fdup one f+ f/ f+ f2/ ; ( f -- fsinh : hyperbolic sine )
: fcosh fexp   fdup one fswap f/ f+ f2/ ;   ( f -- fcosh : hyperbolic cosine )
: fsincosh fdup fsinh fswap fcosh ;         ( f -- sinh cosh )
: ftanh fsincosh f/ ;                       ( f -- ftanh : hyperbolic tangent )

\ : fln one f- flnp1 ;

3.14159265 fconstant pi
1.57079632 fconstant pi/2
6.28318530 fconstant 2pi
\ 2.71828    fconstant euler

\ : >deg [ pi f2* ] 2literal f/ [   360. ] fliteral f* ; ( rad -- deg )
\ : >rad [   360. ] fliteral f/ [ pi f2* ] 2literal f* ; ( deg -- rad )

: floor  f>s s>f ; ( f -- f )
: fround fix s>f ; ( f -- f )
: ftuck fover fswap ; ( f1 f2 -- f2 f1 f2 )

anonymous definitions

: fmod f2dup f/ floor f* f- ;
: >cordic     [ 16384. ] fliteral f* f>s ;   ( f -- n )
: cordic> s>f [ 16384. ] fliteral f/ ;       ( n -- f )
: quadrant fdup                   f0< 4 and >r
           fabs 2pi fmod fdup pi   f< 1 and >r 
                 pi fmod      pi/2 f> 2 and r> r> or or ;
: >sin dup 3 and >r 4 and if fnegate r> -1 >r >r else r> 0 >r >r  then
          r@ 3 = r@ 2 = or if fnegate one f+ then
          r@ 0 = r@ 2 = or if fnegate then 
          rdrop r> if fnegate then ;

: >cos 3 and >r
         r@ 3 = r@ 2 = or if fnegate one f+ then
         r@ 0 = r@ 3 = or if fnegate then 
         rdrop ;

: (fsincos) pi/2 fmod >cordic cordic >r cordic> r> cordic> ; 

forth-wordlist current ! 

\ @warning fsincos still needs a lot of work, and simplifying
: fsincos 2pi fmod fdup quadrant >r (fsincos) r@ >cos fswap r> >sin fswap ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )

only forth definitions
   
\ : fpow ( f u -- f : raise 'f' to an integer power )
\	?dup 0= if fdrop one exit then
\	>r fdup r> 1- for aft fover f* then next fnip ;

\ https://stackoverflow.com/questions/9799041/
\ https://en.wikipedia.org/wiki/Taylor_series


\ : .q e. ." <-> " source type cr ;
\ 0. f         fsin .q
\ pi 0.25 f f* fsin .q
\ pi/2         fsin .q
\ pi           fsin .q
\ 
\ 0. f         fcos .q
\ pi 0.25 f f* fcos .q
\ pi/2         fcos .q
\ 
\ : sins
\   2pi fnegate
\   begin
\     fdup 2pi f<
\   while
\     fdup fdup f. [char] , emit space fsincos fswap e. [char] , emit e. 10 emit
\     [ 2pi 50. f f/ ] 2literal f+
\   repeat fdrop ;
\ 

\     
\ 
\ : quads 
\   [ 0. ] fliteral 
\   begin
\     fdup 2pi f<
\   while
\     fdup fdup f. [char] : emit quadrant . cr
\     [ 2pi 50. f f/ ] 2literal f+
\   repeat fdrop ;
\ 
\ : fcos
\    one  ( rads -- f )
\    fover    fsq [   2. ] fliteral f/ f-
\    fover 4 fpow [  24. ] fliteral f/ f+
\    fswap 6 fpow [ 720. ] fliteral f/ f- ;
\ 
\ : fsin
\   fabs fdup 2pi fmod quadrant >r 2pi fmod
\   fdup ( rads -- f )
\   fdup  3 fpow [    6. ] fliteral f/ f- 
\   fover 5 fpow [  120. ] fliteral f/ f+ 
\   fswap 7 fpow [ 5040. ] fliteral f/ f- 
\ 
\    r> dup >r 1 and if [char] X emit then
\           r> 2 and if [char] Y emit then ; 
\ 
\ 
\ : .q f. ." <-> " source type cr ;
\ 
\ 0. f         fsin .q
\ pi 0.25 f f* fsin .q
\ pi/2         fsin .q
\ pi 0.75 f f* fsin .q
\ pi           fsin .q
\ 2pi          fsin .q
\ 
\ 
\ : fsincos fdup fsin fswap fcos ; ( rads -- sin cos )
\ : ftan fsincos f/ ;              ( rads -- tan )
\ 

system +order
\ hide norm hide zero hide tens hide ralign hide lalign
\ hide   -+ hide  one hide fix  hide shifts 
system -order

\ ========================= FLOATING POINT CODE ===============================

\ ========================= DYNAMIC MEMORY ALLOCATION =========================
\ ## Dynamic Memory Allocation
\ alloc.fth
\  Dynamic Memory Allocation package
\  this code is an adaptation of the routines by
\  Dreas Nielson, 1990; Dynamic Memory Allocation;
\  Forth Dimensions, V. XII, No. 3, pp. 17-27

\ pointer to beginning of free space
variable freelist  0 , 0 freelist !

\ : cell_size ( addr -- n ) >body cell+ @ ;       \ gets array cell size

: initialize ( start_addr length -- : initialize memory pool )
  over dup freelist !  0 swap !  swap cell+ ! ;

: allocate ( u -- addr ior ) \ allocate n bytes, return pointer to block
                             \ and result flag ( 0 for success )
                             \ check to see if pool has been initialized 
  freelist @ 0= if drop 0 -59 exit then
  dup 0= if drop 0 -59 exit then
  cell+ freelist dup
  begin
  while dup @ cell+ @ 2 pick u<
    if 
      @ @ dup   \ get new link
    else   
      dup @ cell+ @ 2 pick - 2 cells max dup 2 cells =
      if 
        drop dup @ dup @ rot !
      else  
        2dup swap @ cell+ !   swap @ +
      then
      2dup ! cell+ 0  \ store size, bump pointer
    then                   \ and set exit flag
  repeat
  nip dup 0= ;

: free ( ptr -- ior ) \ free space at ptr, return status ( 0 for success )
  1 cells - dup @ swap 2dup cell+ ! freelist dup
  begin
    dup 3 pick u< and
  while
    @ dup @
  repeat

  dup @ dup 3 pick ! ?dup
  if 
    dup 3 pick 5 pick + =
    if 
      dup cell+ @ 4 pick + 3 pick cell+ ! @ 2 pick !
    else  
      drop 
    then
  then

  dup cell+ @ over + 2 pick =
  if  
    over cell+ @ over cell+ dup @ rot + swap ! swap @ swap !
  else 
    !
  then
  drop 0 ; \ this code always returns a success flag

\ create pool  1000 allot
\ pool 1000 initialize
\ 5000 1000 initialize
\ 5000 100 dump
\ 40 allocate throw
\ 80 allocate throw .s swap free throw .s 20 allocate throw .s cr

\ ========================= UNIT TEST FRAMEWORK ===============================
.( BEGIN TEST SUITE DEFINITIONS ) here . cr
.( SET MARKER 'XXX' ) cr
marker xxx

variable test
system +order 
test +order definitions 

variable total    ( total number of tests )
variable passed   ( number of tests that passed )
variable vsp      ( stack depth at execution of '->' )
variable vsp0     ( stack depth at execution of 'T{' )
variable n        ( temporary store for 'equal' )
variable verbose  ( verbosity level of the tests )

1 verbose !

: quine source type cr ;                 ( -- : print out current input line )
: ndrop for aft drop then next ;         ( a0...an n -- )
: ndisplay for aft . then next ;         ( a0...an n -- )
: empty-stacks depth ndrop ;             ( a0...an -- )
: .pass   verbose @ 1 > if ."   ok: " space quine then ; ( -- )
: .failed verbose @ 0 > if ." fail: " space quine then ; ( -- )
: pass passed 1+! ;                      ( -- )
: fail empty-stacks -$B throw ;           ( -- )

\ 'equal' is the most complex word in this test bench, it tests whether two
\ groups of numbers of the same length are equal, the length of the numbers
\ is specified by the first argument to 'equal'.
: equal ( a0...an b0...bn n -- a0...an b0...bn n f )
  dup n !
  for aft
    r@ pick r@ n @ 1+ + pick xor if rdrop n @ 0 exit then
  then next n @ -1 ;

\ '?stacks' is given two numbers representing stack depths, if they are
\ not equal it prints out an error message, and calls 'abort'.
: ?stacks ( u u -- )
  2dup xor
  if
    .failed ." Too Few/Many Arguments Provided" cr
    ." Expected:  " u. cr
    ." Got: "       u. cr
    ." Full Stack:" .s cr
    fail exit
  else 2drop then ;

\ 'equal?' takes two lists of numbers of the same length and checks if they
\ are equal, if they are not then an error message is printed and 'abort'
\ is called.
: ?equal ( a0...an b0...bn n -- )
  dup >r
  equal nip 0= if
    .failed ." Argument Value Mismatch" cr
    ." Expected:  " r@ ndisplay cr
    ." Got: "       r@ ndisplay cr
    fail exit
  then r> 2* ndrop ;

only forth definitions system +order test +order

: }T depth vsp0 @ - vsp @ 2* ?stacks vsp @ ?equal pass .pass ;
: -> depth vsp0 @ - vsp ! ;
: T{ depth vsp0 ! total 1+! ;
: statistics total @ passed @ ;
: throws? postpone ' catch >r empty-stacks r> ; ( "name" -- n  )

: logger( verbose @ 1 > if postpone .( cr exit then postpone (  ;
: logger\ verbose @ 1 > if exit then postpone \ ;

\ ========================= UNIT TEST FRAMEWORK ===============================

system +order
test +order
.( BEGIN FORTH TEST SUITE ) cr
logger( DECIMAL BASE )
decimal


T{  1. ->  1 0 }T
\ T{ -2. -> .s -2 -1 }T
\ T{ : RDL1 6. ; RDL1 -> 6 0 }T
\ T{ : RDL2 -4. ; RDL2 -> -4 -1 }T

T{               ->  }T
T{  1            ->  1 }T
T{  1 2 3        ->  1 2 3 }T
T{  1 1+         ->  2 }T
T{  2 2 +        ->  4 }T
T{  3 2 4 within -> -1 }T
T{  2 2 4 within -> -1 }T
T{  4 2 4 within ->  0 }T
T{ 98  4 min     ->  4 }T
T{  1  5 min     ->  1 }T
T{ -1  5 min     -> -1 }T
T{ -6  0 min     -> -6 }T
T{  55 3 max     -> 55 }T
T{ -55 3 max     ->  3 }T
T{  3 10 max     -> 10 }T
T{ -2 negate     ->  2 }T
T{  0 negate     ->  0 }T
T{  2 negate     -> -2 }T
T{ $8000 negate  -> $8000 }T
T{  0 aligned    ->  0 }T
T{  1 aligned    ->  2 }T
T{  2 aligned    ->  2 }T
T{  3 aligned    ->  4 }T
T{  3  4 >       ->  0 }T
T{  3 -4 >       -> -1 }T
T{  5  5 >       ->  0 }T
T{  6  6 u>      ->  0 }T
T{  9 -8 u>      ->  0 }T
T{  5  2 u>      -> -1 }T
T{ -4 abs        ->  4 }T
T{  0 abs        ->  0 }T
T{  7 abs        ->  7 }T
T{ $100 $10 $8  /string -> $108 $8 }T
T{ $100 $10 $18 /string -> $110 $0 }T
T{ 9 log2 -> 3 }T
T{ 8 log2 -> 3 }T
T{ 4 log2 -> 2 }T
T{ 2 log2 -> 1 }T
T{ 1 log2 -> 0 }T
T{ $FFFF count-bits -> $10 }T
T{ $FF0F count-bits -> $C }T
T{ $F0FF count-bits -> $C }T
T{ $0001 count-bits -> $1 }T
T{ $0000 count-bits -> $0 }T
T{ $0002 count-bits -> $1 }T
T{ $0032 count-bits -> $3 }T
T{ $0000 first-bit  -> $0 }T
T{ $0001 first-bit  -> $1 }T
T{ $0040 first-bit  -> $40 }T
T{ $8040 first-bit  -> $8000 }T
T{ $0005 first-bit  -> $0004 }T

logger( BINARY BASE )
binary

T{ 0    gray-encode ->    0 }T
T{ 1    gray-encode ->    1 }T
T{ 10   gray-encode ->   11 }T
T{ 11   gray-encode ->   10 }T
T{ 100  gray-encode ->  110 }T
T{ 101  gray-encode ->  111 }T
T{ 110  gray-encode ->  101 }T
T{ 111  gray-encode ->  100 }T
T{ 1000 gray-encode -> 1100 }T
T{ 1001 gray-encode -> 1101 }T
T{ 1010 gray-encode -> 1111 }T
T{ 1011 gray-encode -> 1110 }T
T{ 1100 gray-encode -> 1010 }T
T{ 1101 gray-encode -> 1011 }T
T{ 1110 gray-encode -> 1001 }T
T{ 1111 gray-encode -> 1000 }T

T{ 0    gray-decode ->    0 }T
T{ 1    gray-decode ->    1 }T
T{ 11   gray-decode ->   10 }T
T{ 10   gray-decode ->   11 }T
T{ 110  gray-decode ->  100 }T
T{ 111  gray-decode ->  101 }T
T{ 101  gray-decode ->  110 }T
T{ 100  gray-decode ->  111 }T
T{ 1100 gray-decode -> 1000 }T
T{ 1101 gray-decode -> 1001 }T
T{ 1111 gray-decode -> 1010 }T
T{ 1110 gray-decode -> 1011 }T
T{ 1010 gray-decode -> 1100 }T
T{ 1011 gray-decode -> 1101 }T
T{ 1001 gray-decode -> 1110 }T
T{ 1000 gray-decode -> 1111 }T

logger( DECIMAL BASE )
decimal
T{ 50 25 gcd -> 25 }T
T{ 13 23 gcd -> 1 }T

T{ 1 2 3 4 5 1 pick -> 1 2 3 4 5 4 }T
T{ 1 2 3 4 5 0 pick -> 1 2 3 4 5 5 }T
T{ 1 2 3 4 5 3 pick -> 1 2 3 4 5 2 }T

T{ 4  square -> 16 }T
T{ -1 square -> 1 }T
T{ -9 square -> 81 }T

T{ 6 factorial -> 720  }T
T{ 0 factorial -> 1  }T
T{ 1 factorial -> 1  }T

T{ 0 sqrt -> 0 }T
T{ 1 sqrt -> 1 }T
T{ 2 sqrt -> 1 }T
T{ 3 sqrt -> 1 }T
T{ 9 sqrt -> 3 }T
T{ 10 sqrt -> 3 }T
T{ 16 sqrt -> 4 }T
T{ 36 sqrt -> 6 }T
T{ -1 throws? sqrt -> -11 }T
T{  4 throws? sqrt ->  0  }T
T{ -9 throws? sqrt -> -11 }T

T{ 10 11 lcm -> 110 }T
T{ 3   2 lcm ->   6 }T
T{ 17 12 lcm -> 204 }T

T{ 3 4 / -> 0 }T
T{ 4 4 / -> 1 }T
T{ 1   0 throws? / -> -10 }T
T{ -10 0 throws? / -> -10 }T
T{ 2 2   throws? / -> 0 }T

marker string-tests

: s1 $" xxx"   count ;
: s2 $" hello" count ;
: s3 $" 123"   count ;
: s4 $" aBc"   count ;
: s5 $" abc"   count ;
: <#> 0 <# #s #> ; ( n -- b u )

logger( Test Strings: )
logger\ .( s1:  ) space s1 type cr
logger\ .( s2:  ) space s2 type cr
logger\ .( s3:  ) space s3 type cr

system +order
T{ s1 crc -> $C35A }T
T{ s2 crc -> $D26E }T
system -order

T{ s1 s2 compare 0= ->  0 }T
T{ s2 s1 compare 0= ->  0 }T
T{ s1 s1 compare 0= -> -1 }T
T{ s2 s2 compare 0= -> -1 }T

.( COMPARE ) cr
\ s4 s5 compare . space source type cr
\ s5 s4 compare . space source type cr

T{ s3  123 <#> compare 0= -> -1 }T
T{ s3 -123 <#> compare 0= ->  0 }T
T{ s3   99 <#> compare 0= ->  0 }T
 
string-tests

T{ 0 ?dup -> 0 }T
T{ 3 ?dup -> 3 3 }T

T{ 1 2 3  rot -> 2 3 1 }T
T{ 1 2 3 -rot -> 3 1 2 }T

T{ 2 3 ' + execute -> 5 }T
T{ : test-1 [ $5 $3 * ] literal ; test-1 -> $F }T

marker variable-test

logger( Defined variable 'x' ) 
variable x
T{ 9 x  ! x @ ->  9 }T
T{ 1 x +! x @ -> $A }T

variable-test

T{     0 invert -> -1 }T
T{    -1 invert -> 0 }T
T{ $5555 invert -> $AAAA }T

T{     0     0 and ->     0 }T
T{     0    -1 and ->     0 }T
T{    -1     0 and ->     0 }T
T{    -1    -1 and ->    -1 }T
T{ $FA50 $05AF and -> $0000 }T
T{ $FA50 $FA00 and -> $FA00 }T

T{     0     0  or ->     0 }T
T{     0    -1  or ->    -1 }T
T{    -1     0  or ->    -1 }T
T{    -1    -1  or ->    -1 }T
T{ $FA50 $05AF  or -> $FFFF }T
T{ $FA50 $FA00  or -> $FA50 }T

T{     0     0 xor ->     0 }T
T{     0    -1 xor ->    -1 }T
T{    -1     0 xor ->    -1 }T
T{    -1    -1 xor ->     0 }T
T{ $FA50 $05AF xor -> $FFFF }T
T{ $FA50 $FA00 xor -> $0050 }T

system +order
T{ $FFFF     1 um+ -> 0 1  }T
T{ $40   $FFFF um+ -> $3F 1  }T
T{ 4         5 um+ -> 9 0  }T

T{ $FFFF     1 um* -> $FFFF     0 }T
T{ $FFFF     2 um* -> $FFFE     1 }T
T{ $1004  $100 um* ->  $400   $10 }T
T{     3     4 um* ->    $C     0 }T
system -order

T{     1     1   < ->  0 }T
T{     1     2   < -> -1 }T
T{    -1     2   < -> -1 }T
T{    -2     0   < -> -1 }T
T{ $8000     5   < -> -1 }T
T{     5    -1   < -> 0 }T

T{     1     1  u< ->  0 }T
T{     1     2  u< -> -1 }T
T{    -1     2  u< ->  0 }T
T{    -2     0  u< ->  0 }T
T{ $8000     5  u< ->  0 }T
T{     5    -1  u< -> -1 }T

T{     1     1   = ->  -1 }T
T{    -1     1   = ->   0 }T
T{     1     0   = ->   0 }T

T{   2 dup -> 2 2 }T
T{ 1 2 nip -> 2 }T
T{ 1 2 over -> 1 2 1 }T
T{ 1 2 tuck -> 2 1 2 }T
T{ 1 negate -> -1 }T
T{ 3 4 swap -> 4 3 }T
T{ 0 0= -> -1 }T
T{ 3 0= ->  0 }T
T{ -5 0< -> -1 }T
T{ 1 2 3 2drop -> 1 }T

T{ 1 2 lshift -> 4 }T
T{ 1 $10 lshift -> 0 }T
T{ $4001 4 lshift -> $0010 }T

T{ 8     2 rshift -> 2 }T
T{ $4001 4 rshift -> $0400 }T
T{ $8000 1 rshift -> $4000 }T

T{ 99 throws? throw -> 99 }T

T{ 50 10 /mod ->  0  5 }T
T{ -4 3  /mod -> -1 -1 }T
T{ -8 3  /mod -> -2 -2 }T

T{     0 ><   -> 0     }T
T{    -1 ><   -> -1    }T
T{ $0001 ><   -> $0100 }T
T{ $CAFE ><   -> $FECA }T
T{ $1234 ><   -> $3412 }T

marker definition-test

logger( Created word 'y' 0 , 0 , )
create y 0 , 0 ,
T{ 4 5 y 2! -> }T
T{ y 2@ -> 4 5 }T

: e1 $" 2 5 + " count ;
: e2 $" 4 0 / " count ;
: e3 $" : z [ 4 dup * ] literal ; " count ;
logger\ .( e1: ) space e1 type cr
logger\ .( e2: ) space e2 type cr
logger\ .( e3: ) space e3 type cr
T{ e1 evaluate -> 7 }T
T{ e2 throws? evaluate -> $A negate }T
T{ e3 evaluate z -> $10 }T

definition-test


T{ here 4 , @ -> 4 }T
T{ here 0 , here swap cell+ = -> -1 }T

\ T{ depth depth depth -> 0 1 2 }T

T{ char 0     -> $30 }T
T{ char 1     -> $31 }T
T{ char g     -> $67 }T
T{ char ghijk -> $67 }T

\ T{ #vocs 8 min -> 8 }T     \ minimum number of vocabularies is 8
T{ b/buf       -> $400 }T  \ b/buf should always be 1024
\ defined? sp@ ?\ T{ sp@ 2 3 4 sp@ nip nip nip - abs chars -> 4 }T
T{ here 4 allot -4 allot here = -> -1 }T

defined? d< ?\ T{  0  0  0  0 d< ->  0 }T
defined? d< ?\ T{  0  0  0  1 d< -> -1 }T
defined? d< ?\ T{  0  0  1  0 d< -> -1 }T
defined? d< ?\ T{  0 -1  0  0 d< -> -1 }T
defined? d< ?\ T{  0 -1  0 -1 d< ->  0 }T
defined? d< ?\ T{  0 -1  0  1 d< -> -1 }T
defined? d< ?\ T{ $FFFF -1  0  1 d< -> -1 }T
defined? d< ?\ T{ $FFFF -1  0  -1 d< -> 0 }T

$FFFF constant min-int 
$7FFF constant max-int
$FFFF constant 1s

T{       0 s>d              1 sm/rem ->  0       0 }T
T{       1 s>d              1 sm/rem ->  0       1 }T
T{       2 s>d              1 sm/rem ->  0       2 }T
T{      -1 s>d              1 sm/rem ->  0      -1 }T
T{      -2 s>d              1 sm/rem ->  0      -2 }T
T{       0 s>d             -1 sm/rem ->  0       0 }T
T{       1 s>d             -1 sm/rem ->  0      -1 }T
T{       2 s>d             -1 sm/rem ->  0      -2 }T
T{      -1 s>d             -1 sm/rem ->  0       1 }T
T{      -2 s>d             -1 sm/rem ->  0       2 }T
T{       2 s>d              2 sm/rem ->  0       1 }T
T{      -1 s>d             -1 sm/rem ->  0       1 }T
T{      -2 s>d             -2 sm/rem ->  0       1 }T
T{       7 s>d              3 sm/rem ->  1       2 }T
T{       7 s>d             -3 sm/rem ->  1      -2 }T
T{      -7 s>d              3 sm/rem -> -1      -2 }T
T{      -7 s>d             -3 sm/rem -> -1       2 }T
T{ max-int s>d              1 sm/rem ->  0 max-int }T
T{ min-int s>d              1 sm/rem ->  0 min-int }T
T{ max-int s>d        max-int sm/rem ->  0       1 }T
T{ min-int s>d        min-int sm/rem ->  0       1 }T
T{      1s 1                4 sm/rem ->  3 max-int }T
T{       2 min-int m*       2 sm/rem ->  0 min-int }T
T{       2 min-int m* min-int sm/rem ->  0       2 }T
T{       2 max-int m*       2 sm/rem ->  0 max-int }T
T{       2 max-int m* max-int sm/rem ->  0       2 }T
T{ min-int min-int m* min-int sm/rem ->  0 min-int }T
T{ min-int max-int m* min-int sm/rem ->  0 max-int }T
T{ min-int max-int m* max-int sm/rem ->  0 min-int }T
T{ max-int max-int m* max-int sm/rem ->  0 max-int }T

T{ :noname 2 6 + ; execute -> 8 }T

decimal

\ 3 set-precision
\ 20 s>f f. cr
\ 20 s>f 3 s>f f- f. cr
\ 25 s>f f2/ f2/ f. cr
\ 12 s>f fsq f. cr
\ 
\ 
\ 2 s>f 3 s>f f+ f. cr
\ 2 s>f 4 s>f f* f. cr
\ 400.0 f 2 s>f f/ f. cr
\ 10.3 f f. cr
\ 6 s>f f. cr
\ -12.34 f e. cr
\ -1 s>f 2 s>f f< . cr
\ 2 s>f 1 s>f f< . cr
\ 9 s>f exp f. cr

T{ -1  s>f f>s       -> -1 }T
T{ 123 s>f f>s       -> 123 }T
T{ 12  s>f 13 s>f f< -> -1 }T
T{ -4  s>f -9 s>f f< ->  0 }T
T{ 12  s>f fsq f>s   -> 144 }T
T{ 400 s>f 2 s>f f/ f>s -> 200 }T
T{ 3.0 f 9.00 f f+ f>s -> 12 }T
\ T{ 3.0 f 9. f f+ f>s -> 12 }T

\  T{ random random <> -> -1 }T

.( TESTS COMPLETE ) cr
decimal
.( passed: ) statistics u. space .( / ) 0 u.r cr
.( here:   ) here . cr
statistics <> ?\ .( [FAILED]     ) cr  \ abort
statistics  = ?\ .( [ALL PASSED] ) cr   

.( CALLING MARKER 'XXX' ) cr
xxx

.( SAVING NEW IMAGE [SIZE:) here u. .( ] ) cr

\ 0 here dump

.( FINISHED TESTS ) cr
\ bye
\ ========================= END OF TESTS ======================================



