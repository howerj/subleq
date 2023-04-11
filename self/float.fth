\ # Floating Point Package (and more)
\
\ TODO: Meta-compilation and integrate into base system?
\ TODO: Make "precision" a user variable
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

only forth definitions decimal system +order
: debug source type ."  ok" cr ; 
 ' debug <ok> !

: undefined? bl word find nip 0= ; ( "name", -- f )
: defined? undefined? 0= ; ( "name", -- f: Is word defined ? )
undefined? postpone [if] 
  : postpone [compile] [compile] ; immediate 
[then] 
: ?\ 0= if postpone \ then ; ( f --, <string>| : cond. comp. )

undefined? 0<   ?\ : 0< 0 < ;
undefined? 1-   ?\ : 1- 1 - ;
undefined? 2+   ?\ : 2+ 2 + ;
undefined? 2*   ?\ : 2* 1 lshift ;
undefined? 2/   ?\ : 2/ 1 rshift ;
undefined? >=   ?\ : >= < 0= ;
\ undefined? rdup ?\ : rdup r> r> dup >r >r >r ;
undefined? 1+!  ?\ : 1+! 1 swap +! ;
undefined? dnegate ?\ : dnegate invert >r invert 1 um+ r> + ; 
undefined? d+ ?\ : d+ >r swap >r um+ r> + r> + ; 
undefined? s>d ?\ : s>d dup 0< ;
undefined? 2>r ?\ : 2>r r> swap >r swap >r >r ; 
undefined? 2r> ?\ : 2r> r> r> swap r> swap >r ; 
undefined? /string ?\ : /string over min rot over + -rot - ;
\  /string ( b u1 u2 -- b u : advance string u2 )
undefined? ?depth ?\ : ?depth depth >= -4 and throw ;
undefined? user ?\ : user variable ; ( single thread systems )

1 cells 8 * constant #bits
1 #bits 1- lshift constant #msb

: banner ( +n c -- : print a character 'n' times )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ; 
: spaces bl banner ; ( +n  -- : print space 'n' times )
\ : anonymous ( -- : make anonymous vocabulary and enable it )
\  get-order 1+ here dup 1 cells allot 0 swap ! swap set-order ;
: convert count >number drop ; ( +d1 addr1 -- +d2 addr2 )
: arshift ( n u -- n : arithmetic right shift )
  2dup rshift >r swap $8000 and
  if $10 swap - -1 swap lshift else drop 0 then r> or ;
: d2* over $8000 and >r 2* swap 2* swap r> if 1 or then ;
: d2/ dup      1 and >r 2/ swap 2/ r> if $8000 or then swap ;
: d- dnegate d+ ; ( d d -- d : double cell subtraction )
: d= rot = -rot = and ; ( d d -- f : double cell equal )
: d0= or 0= ;     ( d -- f : double cell number equal to zero )
: d0<> d0= 0= ;   ( d -- f : double not equal to zero )
: 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
: dabs s>d if dnegate then ; ( d -- ud )
: 2over ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
  >r >r 2dup r> swap >r swap r> r> -rot ;
: 2, , , ; ( n n -- : write to values into dictionary )
: 2constant create 2, does> 2@ ; ( d --, Run: -- d )
: 2variable create 0 , 0 , ; \ does> ; ( d --, Run: -- a )
: 2literal swap postpone literal postpone literal ; immediate
: +- 0< if negate then ; ( n n -- n : copy sign )
: m* ( n n -- d : single to double cell multiply [16x16->32] ) 
  2dup xor 0< >r abs swap abs um* r> if dnegate then ; 
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r         ( dl dh nn -- dl dh,   R: -- dh nn )
  dabs r@ abs um/mod ( dl dh    -- rem quo, R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : double prec. intermediate val )
  >r m* r> sm/rem ;
: d>s drop ; ( d -- n : convert dubs to single )

\ From: https://en.wikipedia.org/wiki/Integer_square_root
\ This function computes the integer square root of a number.
\
\ 'sc': unsigned small candidate
\ 'lc': unsigned large candidate
\
: square dup * ; ( n -- n : square a number )
: sqrt ( n -- u : integer square root )
  1 ?depth
  s>d  if -$B throw then ( does not work for negative values )
  dup 2 < if exit then   ( return 0 or 1 )
  dup                    ( u u )
  2 rshift recurse 2*    ( u sc )
  dup                    ( u sc sc )
  1+ dup square          ( u sc lc lc^2 )
  >r rot r> <            ( sc lc bool )
  if drop else nip then ; ( return small or large candidate )

: log ( u base -- u : the integer logarithm of u in 'base' )
  >r
  dup 0= -$B and throw ( logarithm of zero is an error )
  0 swap
  begin
    swap 1+ swap r@ / dup 0= ( keep dividing until 'u' is 0 )
  until
  drop 1- rdrop ;

: clz ( u -- : count leading zeros )
  ?dup 0= if #bits exit then
  #msb 0 >r begin
   2dup and 0=
  while
   r> 1+ >r 2/
  repeat
  2drop r> ;

( : log2 2 log ; ( u -- u : binary integer logarithm )
: log2 ?dup 0= -11 and throw clz #bits swap - 1- ; ( u -- u )

\ <forth.sourceforge.net/algorithm/bit-counting/index.html>
: count-bits ( number -- bits )
  dup $5555 and swap 1 rshift $5555 and +
  dup $3333 and swap 2 rshift $3333 and +
  dup $0F0F and swap 4 rshift $0F0F and +
  $FF mod ;

\ <forth.sourceforge.net/algorithm/firstbit/index.html>
: first-bit ( number -- first-bit )
  dup   1 rshift or
  dup   2 rshift or
  dup   4 rshift or
  dup   8 rshift or
  dup $10 rshift or
  dup   1 rshift xor ;

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
\ and would make for a poor Forth word.
\
\ Using "variable" would create problems on a preemptive
\ multitasking Forth system (this system uses cooperative
\ threading and CORDIC does not call "pause" anywhere). A
\ simple fix would be to use "user" instead of "variable" to
\ make all of these variables thread-local, however there are
\ a lot of those variables and thread local storage is in
\ short supply.
\
\

system +order definitions
create lookup ( 16 values )
$3243 , $1DAC , $0FAD , $07F5 , $03FE , $01FF , $00FF , $007F ,
$003F , $001F , $000F , $0007 , $0003 , $0001 , $0000 , $0000 ,

$26DD constant cordic_1K $6487 constant hpi

variable tx 0 tx ! variable ty 0 ty ! variable tz 0 tz !
variable x  0  x ! variable y  0  y ! variable z  0  z !
variable d  0  d ! variable k  0  k !

only forth definitions system +order

( CORDIC: valid in range -pi/2 to pi/2, arguments in fixed )
( point format with 1 = 16384, angle is given in radians.  )

: cordic ( angle -- sine cosine | x y -- atan sqrt )
  z ! cordic_1K x ! 0 y ! 0 k !
  $10 begin ?dup while
    z @ 0< d !
    x @ y @ k @ arshift d @ xor d @ - - tx !
    y @ x @ k @ arshift d @ xor d @ - + ty !
    z @ k @ cells lookup + @ d @ xor d @ - - tz !
    tx @ x ! ty @ y ! tz @ z !
    k 1+!
    1-
  repeat 
  y @ x @ ;

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
only forth definitions system +order

: fabs $7FFF and ; ( f -- f )

system definitions

user (precision) 4 (precision) !

create ftable
          .001 , ,        .010 , ,
          .100 , ,       1.000 , ,
        10.000 , ,     100.000 , ,
      1000.000 , ,   10000.000 , ,
    100000.000 , , 1000000.000 , ,

: zero ( f -- f : zero exponent if mantissa is )
  over 0= if drop 0 then ; 
: norm >r 2dup or  ( normalize input float )
  if begin s>d invert
    while d2* r> 1- >r
    repeat swap 0< - ?dup
    if r> else $8000 r> 1+ then
  else r> drop then ;
: lalign $20 min for aft d2/ then next ;
: ralign 1- ?dup if lalign then 1 0 d+ d2/ ;
: tens 2* cells ftable + 2@ ; ( a -- d )
: shifts fabs $4010 - s>d invert if -43 throw then negate ;
: base? base @ $A <> -40 and throw ; ( -- : check base )
: unaligned? dup 1 and = -9 and throw ; ( -- : check ptr )
: -+ drop swap 0< if negate then ;

only forth definitions system +order

\ "fdepth" is standards compliant, but pretty useless because
\ there is no separate floating point stack.

: fcopysign $8000 and nip >r fabs r> or ; ( r1 r2 -- r1 )
: floats 2* cells ;    ( u -- u )
: float+ [ 1 floats ] literal + ; ( a -- a : inc addr by float )
: set-precision ( +n -- : set FP decimals printed out )
  dup 0 5 within if (precision) ! exit then -43 throw ; 
: precision (precision) @ ; ( -- u : precision of FP values )
: f@ unaligned? 2@ ;   ( a -- r : fetch FP value )
: f! unaligned? 2! ;   ( r a -- : store FP value )
: f, 2, ; ( r -- )
: falign align ;       ( -- : align the dict. to store a FP )
: faligned aligned ;   ( a -- a : align point for FP )
: fdepth depth 2/ ;    ( -- n : number of floats, approximate )
: fdup 2 ?depth 2dup ; ( r -- r r : FP duplicate )
: fswap 4 ?depth 2swap ; ( r1 r2 -- r2 r1 : FP swap )
: fover 4 ?depth 2over ; ( r1 r2 -- r1 r2 r1 )
: f2dup fover fover ;  ( r1 r2 -- r1 r2 r1 r2 )
: ftuck fdup 2>r fswap 2r> ; ( r1 r2 -- r2 r1 r2 )
: frot 2>r fswap 2r> fswap ; ( r1 r2 r3 -- r2 r3 r1 )
: -frot frot frot ;    ( r1 r2 r3 -- r3 r1 r2 )
: fdrop 2 ?depth 2drop ; ( r -- : floating point drop )
: f2drop fdrop fdrop ; ( r1 r2 -- : FP 2drop )
: fnip fswap fdrop ;   ( r1 r2 -- r2 : FP nip )
: fnegate $8000 xor zero ;  ( r -- r : FP negate )
: fsign fabs over 0< if >r dnegate r> $8000 or then ;
: f2* 2 ?depth 1+ zero ; ( r -- r : FP times by two )
: f2/ 2 ?depth 1- zero ; ( r -- r : FP divide by two )
: f*  4 ?depth rot + $4000 - >r um* r> norm ; ( r r -- r )
: fsq fdup f* ;        ( r -- r : FP square )
: f0= fabs zero d0= ; ( r -- r : FP equal to zero [incl -0.0] )
: um/ ( ud u -- u : ud/u and round )
  dup >r um/mod swap r> over 2* 1+ u< swap 0< or - ; 

: f/ ( r1 r2 -- r1/r2 : floating point division )
  4 ?depth
  fdup f0= -42 and throw
  rot swap - $4000 + >r
  0 -rot 2dup u<
  if  um/ r> zero 
  else >r d2/ fabs r> um/ r> 1+
  then ;

: f+ ( r r -- r : floating point addition )
  4 ?depth
  rot 2dup >r >r fabs swap fabs - 
  dup if s>d
    if rot swap negate
      r> r> swap >r >r 
    then 0 swap ralign
  then swap 0 r> r@ xor 0< 
  if r@ 0< if 2swap then d-
    r> fsign rot swap norm 
  else d+ if  1+ 2/ $8000 or r> 1+
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
: d>f $4020 fsign norm ;  ( d -- r : double to float )
: s>f s>d d>f ;           ( n -- r : single to float )

: f# 
  base?
  >r precision tens drop um* r> shifts
  ralign precision ?dup if for aft # then next
  [char] . hold then #s rot sign ;
: f.r >r tuck <# f# #> r> over - spaces type ; ( f +n -- )
: f. space 0 f.r ; ( f -- : output floating point )

( N.B. 'f' and 'e' require "dpl" to be set correctly! )

: f ( n|d -- f : formatted double to float )
   base?
   dpl @ 0< if ( input was single number )
     1 ?depth s>d 0 dpl ! 
   else ( else a double )
     2 ?depth
   then 
   d>f dpl @ tens d>f f/ ;    

: fconstant f 2constant ; ( "name", r --, Run Time: -- r )
: fliteral  f postpone 2literal ; immediate ( r --, Run: -- r )
: fix tuck 0 swap shifts ralign -+ ; ( r -- n : f>s rounding )
: f>s tuck 0 swap shifts lalign -+ ; ( r -- n : f>s truncate )
: floor  f>s s>f ; ( r -- r )
: fround fix s>f ; ( r -- r )
: fmod f2dup f/ floor f* f- ; ( r1 r2 -- r )

1.0 fconstant fone decimal

: f1+ fone f+ ; ( r -- r : increment FP number )
: f1- fone f- ; ( r -- r : decrement FP number )
: finv fone fswap f/ ; ( r -- r : FP 1/x )

: exp ( r -- r : raise 2.0 to the power of 'r' )
  2dup f>s dup >r s>f f-     
  f2* [ -57828.0 ] fliteral 
  2over fsq [ 2001.18 ] fliteral f+ f/
  2over f2/ f- [ 34.6680 ] fliteral f+ f/
  f1+ fsq r> + ;
: fexp  ( r -- r : raise e to the power of 'r' )
  [ 1.4427 ( = log2(e) ] fliteral f* exp ; 
: falog [ 3.3219 ( = log2(10) ] fliteral f* exp ; ( r -- r )
: get ( "123" -- : get a single signed number )
  bl word dup 1+ c@ [char] - = tuck -
  0 0 rot convert drop ( should throw if not number... ) -+ ;
: e ( f "123" -- usage "1.23 e 10", input scientific notation )
  f get >r r@ abs 13301 4004 */mod
  >r s>f 4004 s>f f/ exp r> +
  r> 0< if f/ else f* then ;
: e.r ( r +n -- : output scientific notation )
  >r
  tuck fabs 16384 tuck -
  4004 13301 */mod >r
  s>f 4004 s>f f/ exp f*
  2dup fone f<
  if 10 s>f f* r> 1- >r then
  <# r@ abs 0 #s r> sign 2drop
  [char] e hold f# #> r> over - spaces type ;
: e. space 0 e.r ;

: fexpm1 fexp fone f- ; ( r1 -- r2 : e raised to 'r1' less 1 )
: fsinh fexpm1 fdup fdup f1+ f/ f+ f2/ ; ( r -- fsinh : h-sin )
: fcosh fexp fdup fone fswap f/ f+ f2/ ; ( r -- fcosh : h-cos )
: fsincosh fdup fsinh fswap fcosh ; ( f -- sinh cosh )
: ftanh fsincosh f/ ; ( f -- ftanh : hyperbolic tangent )

( Define some useful constants )
 3.14159265 fconstant fpi    \ PI
 1.57079632 fconstant fhpi   \ Half PI
 6.28318530 fconstant f2pi   \ 2*PI
 2.71828182 fconstant fe     \ e
 0.69314718 fconstant fln2   \ ln(2.0)  (natural log of 2)
 2.30258509 fconstant fln10  \ ln(10.0) (natural log of 10)

: fdeg f2pi f/ [ 360.0 ] fliteral f* ; ( rad -- deg )
: frad [ 360.0 ] fliteral f/ f2pi f* ; ( deg -- rad )

system +order definitions

: >cordic     [ 16384.0 ] fliteral f* f>s ; ( f -- n )
: cordic> s>f [ 16384.0 ] fliteral f/ ;     ( n -- f )

: quadrant 
  fdup fhpi f< if fdrop 0 exit then 
  fdup  fpi f< if fdrop 1 exit then 
      [ fpi fhpi f+ ] 2literal f< if 2 exit then 
  3 ;
: >sin 2 4 within if fnegate then ;
: >cos 1 3 within if fnegate then ;
: scfix >r 
  r@ 1 = if fnegate fpi f+ rdrop exit then
  r> 3 = if fnegate f2pi f+ then ;

: (fsincos) fhpi fmod >cordic cordic >r cordic> r> cordic> ; 

only forth definitions system +order

: fsincos ( rads -- sin cos )
   fdup f0< >r
   fabs
   f2pi fmod fdup quadrant dup >r scfix (fsincos) 
   r@ >cos fswap r> >sin fswap
   r> if fswap fnegate fswap then ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )
: ftan fsincos f/ ; ( rads -- tan )

only forth definitions system +order decimal

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
  fdup f0< if fdrop -46 throw then
  fdup f0= if fdrop fzero exit then
  fone 
  16 for aft 
    f2dup fsq fswap f- fover f2* f/ f-
  then next
  fnip ;

: filog2 ( r -- u : Floating point integer logarithm )
  zero
  fdup fzero f<= -46 and throw
  ( norm ) nip $4001 - ;

: fhypot f2dup f> if fswap then ( a b -- c : hypotenuse )
  fabs 2>r fdup 2r> fswap f/ fsq f1+ fsqrt f* ;
 
only forth definitions system +order

1 [if]
: sins
  f2pi fnegate
  begin
    fdup f2pi f<
  while
    fdup fdup f. [char] , emit space fsincos 
    fswap f. [char] , emit space f. cr
    [ f2pi 50.0 f f/ ] 2literal f+
  repeat fdrop ;
[then] 

\ <en.wikipedia.org/wiki/Arithmetic%E2%80%93geometric_mean>
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
: fln
  [ 2 12 - s>f exp ] 2literal fswap f/ 
  fone fswap 
  12 for aft agm then next f+ fpi 
  fswap f/ [ 12 s>f fln2 f* ] 2literal f- ;
: flnp1 fone f+ fln ;

\ An Alternate "flog2":
\
\        : flog2
\          [ 2 12 - s>f exp ] 2literal fswap f/ 
\          fone fswap 
\          12 for aft agm then next f+ fln2 f* fpi 
\          fswap f/ [ 12 s>f ] 2literal f- ;

: flog2 fln fln2 f/ ; ( r -- r : base  2 logarithm )
: flog fln fln10 f/ ; ( r -- r : base 10 logarithm )
: f** fswap flog2 f* exp ; ( r1 r2 -- r : pow[r1, r2] )

: fatanh ( r1 -- r2 : atanh, -1 < r1 < 1 )
  fdup f1+ fswap fone fswap f- f/ fln f2/ ;
: facosh ( r1 -- r2 : acosh, 1 <= r1 < INF )
  fdup fsq f1- fsqrt f+ fln ; 
: fasinh fdup fsq f1+ fsqrt f+ fln ; ( r -- r )

system +order definitions

\ N.B This is a better version of atan where x > 1, but it
\ is larger and uses global variables
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

: fatan-lo fdup fsq fdup 
   [  0.07765095 ( = A ) ] fliteral f* 
   [ -0.28743447 ( = B ) ] fliteral f+ f* 
   [  0.99518168 ( pi/4 - A - B ) ] fliteral f+ f* ;

\ atan(x) = pi/2 - atan(1/x)
: fatan-hi finv fatan-lo fhpi fswap f- ;

only forth definitions system +order

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
  fdup f0< if fdrop [ fhpi fnegate ] 2literal exit then
  -46 throw ;

: fasin fdup fsq fone fswap f- fsqrt f/ fatan ; ( r -- r )
: facos fasin fhpi fswap f- ; ( r -- r )

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

: scopy ( b u -- b u : copy a string into the dictionary )
  align here >r aligned dup allot
  r@ swap dup >r cmove r> r> swap ;

: macro ( c" xxx" --, : create a late-binding macro )
  create immediate align here 2 cells + ,
  0 parse dup , scopy 2drop
  does> 2@ swap evaluate ;

system -order
.( DONE ) cr

: 2pick dup >r pick r> 2+ pick swap ;


