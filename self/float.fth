\ FORTH Floating point package
\
\ TODO: "100." fails (gives 1000.0), "100.0" works
\ TODO: Using "num." instead of "num.num" causes problems:
\        3276. f f. ( prints 3276.000 )
\        10. f f.   ( prints 17.000 )
\        10.0 f f.   ( prints 10.000 )
\ TODO: Meta-compilation
\ TODO: Documentation
\ TODO: Optional FP stack?
\ TODO: Integrate into base system?
\ TODO: Replace "if NUM throw then" with "NUM and throw"
\ NB. Input is finicky, requires "dpl" to be set, so
\ "fone e 1" will not work, but "1.0 e 1" will.
\
\ TODO: Add back in unit test framework?
\ missing: f**, facos, fln, facosh, falog, fasin, fasinh, 
\ fatan, fatan2, fatanh, fsqrt, finv,
\
\ Add things from <https://github.com/howerj/q>?
\
\ SQRT: https://stackoverflow.com/questions/3581528/
\ MATH: https://stackoverflow.com/questions/4541130/
\ FORTH: https://forth-standard.org/standard/float/
\


only forth definitions decimal system +order
 <ok> @ constant okok
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
undefined? 2*   ?\ : 2* 1 lshift ;
undefined? 2/   ?\ : 2/ 1 rshift ;
\ undefined? rdup ?\ : rdup r> r> dup >r >r >r ;
undefined? 1+!  ?\ : 1+! 1 swap +! ;
undefined? dnegate ?\ : dnegate invert >r invert 1 um+ r> + ; 
undefined? d+ ?\ : d+ >r swap >r um+ r> + r> + ; 
undefined? s>d ?\ : s>d dup 0< ;
undefined? 2>r ?\ : 2>r r> swap >r swap >r >r ; 
undefined? 2r> ?\ : 2r> r> r> swap r> swap >r ; 
undefined? /string ?\ : /string over min rot over + -rot - ;
\  /string ( b u1 u2 -- b u : advance string u2 )


1 cells 8 * constant #bits
1 #bits 1- lshift constant #msb

: banner ( +n c -- : print a character 'n' times )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ; 
: spaces bl banner ; ( +n  -- : print space 'n' times )
: anonymous get-order 1+ here 1 cells allot swap set-order ;
: convert count >number drop ; ( +d1 addr1 -- +d2 addr2 )
: arshift ( n u -- n : arithmetic right shift )
  2dup rshift >r swap $8000 and
  if $10 swap - -1 swap lshift else drop 0 then r> or ;
: d2* over $8000 and >r 2* swap 2* swap r> if 1 or then ;
: d2/ dup      1 and >r 2/ swap 2/ r> if $8000 or then swap ;
: d- dnegate d+ ;
: d= rot = -rot = and ;
: d0= or 0= ;
: d0<> d0= 0= ;
: 2swap >r -rot r> -rot ;
: dabs s>d if dnegate then ;   ( d -- ud )
: 2over ( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
  >r >r 2dup r> swap >r swap r> r> -rot ;
: 2, , , ;
: 2constant create 2, does> 2@ ;
: 2variable create 0 , 0 , ; \ does> ;
: 2literal swap postpone literal postpone literal ; immediate
: +- 0< if negate then ; ( n n -- n : copy sign )
: m* ( n n -- d ) 
  2dup xor 0< >r abs swap abs um* r> if dnegate then ; 
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r         ( dl dh nn -- dl dh,   R: -- dh nn )
  dabs r@ abs um/mod ( dl dh    -- rem quo, R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : double prec. intermediate val )
    >r m* r> sm/rem ;

\ From: https://en.wikipedia.org/wiki/Integer_square_root
\ This function computes the integer square root of a number.
\
\ 'sc': unsigned small candidate
\ 'lc': unsigned large candidate
\
: sqrt ( n -- u : integer square root )
  s>d  if -$B throw then ( does not work for signed values )
  dup 2 < if exit then   ( return 0 or 1 )
  dup                    ( u u )
  2 rshift recurse 2*    ( u sc )
  dup                    ( u sc sc )
  1+ dup square          ( u sc lc lc^2 )
  >r rot r> <            ( sc lc bool )
  if drop else nip then ; ( return small or large candidate )

: log ( u base -- u : the integer logarithm of u in 'base' )
  >r
  dup 0= if -$B throw then ( logarithm of zero is an error )
  0 swap
  begin
    swap 1+ swap r@ / dup 0= ( keep dividing until 'u' is 0 )
  until
  drop 1- rdrop ;

: clz ( u -- : count leading zeros )
  ?dup 0= if #bits exit then
  #msb  0 >r begin
   2dup and 0=
  while
   r> 1+ >r
   2/
  repeat
  2drop
  r> ;

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

\ ==================== CORDIC CODE ============================

anonymous definitions
create lookup ( 16 values )
$3243 , $1DAC , $0FAD , $07F5 , $03FE , $01FF , $00FF , $007F ,
$003F , $001F , $000F , $0007 , $0003 , $0001 , $0000 , $0000 ,

$26DD constant cordic_1K $6487 constant fhpi

variable tx 0 tx ! variable ty 0 ty ! variable tz 0 tz !
variable x  0  x ! variable y  0  y ! variable z  0  z !
variable d  0  d ! variable k  0  k !

forth-wordlist current ! 

( CORDIC: valid in range -pi/2 to pi/2, arguments in fixed )
( point format with 1 = 16384, angle is given in radians.  )
\ TODO: Extend to arc-{sin,cos} with new mode.
: cordic ( angle -- sine cosine )
  z ! cordic_1K x ! 0 y ! 0 k !
  $10 begin ?dup while
    rotate @ if z @ else y @ negate then 0< d !
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

\ ==================== CORDIC CODE ============================

\ ================ FLOATING POINT CODE ========================

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

: fabs  $7FFF and ;   ( f -- f )

\ anonymous definitions
create ftable 3 , 
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
: tens 2* cells  [ ftable cell+ ] literal + 2@ ;     
: shifts fabs $4010 - s>d invert if -$2B throw then negate ;
: base? base @ $A <> if -$28 throw then ; ( -- : check base )
: unaligned? dup 1 and = if -$9 throw then ; ( -- : check ptr )
: -+ drop swap 0< if negate then ;

forth-wordlist current !

: set-precision ( +n -- : set FP decimals printed out )
  dup 0 5 within if ftable ! exit then -$2B throw ; 
: precision ftable @ ; ( -- u )
: f@ unaligned? 2@ ;  ( a -- r )
: f! unaligned? 2! ;  ( r a -- )
: falign align ;      ( -- )
: faligned aligned ;  ( a -- a )
: fdepth depth ;      ( -- u )
: fdup 2dup ;         ( r -- r r )
: fswap 2swap ;       ( r1 r2 -- r2 r1 )
: fover 2over ;       ( r1 r2 -- r1 r2 r1 )
: f2dup fover fover ; ( r1 r2 -- r1 r2 r1 r2 )
: ftuck fover fswap ; ( r1 r2 -- r2 r1 r2 )
: frot 2>r fswap 2r> fswap ; ( r1 r2 r3 -- r2 r3 r1 )
: fdrop 2drop ;       ( r -- )
: f2drop fdrop fdrop ; ( r1 r2 -- )
: fnip fswap fdrop ;  ( r1 r2 -- r2 )
: fnegate $8000 xor zero ;  ( r -- r )
: fsign fabs over 0< if >r dnegate r> $8000 or then ;
: f2* 1+ zero ;       ( r -- r : FP times by two )
: f2/ 1- zero ;       ( r -- r : FP divide by two )
: f*  rot + $4000 - >r um* r> norm ; ( r r -- r )
: fsq fdup f* ;       ( r -- r : FP square )
: f0= zero d0= ;      ( r -- r : FP equal to zero )
: floats 2* cells ;   ( u -- u )
: float+ 1 floats + ; ( a -- a )
: um/ ( ud u -- u )
  dup >r um/mod swap r> over 2* 1+ u< swap 0< or - ; 

: f/ ( f1 f2 == f1/f2 : floating point division )
  fdup f0= if -44 throw then
  rot swap - $4000 + >r
  0 -rot 2dup u<
  if  um/ r> zero 
  else >r d2/ fabs r> um/ r> 1+
  then ;

: f+ ( f f -- f : floating point addition )
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

: f- fnegate f+ ; ( r1 r2 -- t : floating point subtract )
: f< f- 0< nip ; ( r1 r2 -- t : floating point less than )
: f> fswap f< ;  ( r1 r2 -- t : floating point greater than )
: f>= f< 0= ;    ( r1 r2 -- t : FP greater or equal )
: f<= f> 0= ;    ( r1 r2 -- t : FP less than or equal )
: f= d= ;        ( r1 r2 -- t : FP exact equality )
: f0> 0 0 f> ;   ( r1 r2 -- t : FP greater than zero )
: f0< 0 0 f< ;   ( r1 r2 -- t : FP less than zero )
: f0<= f0> 0= ;  ( r1 r2 -- t : FP less than or equal to zero )
: f0>= f0< 0= ;  ( r1 r2 -- t : FP more than or equal to zero )
: fmin f2dup f< if fdrop exit then fnip ; ( r1 r2 -- f : min )
: fmax f2dup f> if fdrop exit then fnip ; ( r1 r2 -- f : max )
: d>f $4020 fsign norm ;  ( d -- r : double to float )
: s>f s>d d>f ;           ( n -- r : single to float )

: f# 
  base?
  >r precision tens drop um* r> shifts
  ralign precision ?dup if for aft # then next
  [char] . hold then #s rot sign ;
: f.r >r tuck <# f# #> r> over - spaces type ; ( f +n -- )
: f. space 0 f.r ; ( f -- : output floating point )

\ NB. 'f' and 'e' require "dpl" to be set correctly!
system +order
: f ( n|d -- f : formatted double to float )
   base?
\  1 ?depth ( NB. 2 ?depth if following line missing )
   dpl @ 0< if s>d 0 dpl ! then ( input was single number )
   d>f dpl @ tens d>f f/ ;    
system -order
: fconstant f 2constant ; ( "name" , r --, Run Time: -- r )
: fliteral  f postpone 2literal ; immediate ( r --, Run Time: -- r )
: fix tuck 0 swap shifts ralign -+ ; ( r -- n : f>s rounding )
: f>s tuck 0 swap shifts lalign -+ ; ( r -- n : f>s truncate )
: floor  f>s s>f ; ( r -- r )
: fround fix s>f ; ( r -- r )
: fmod f2dup f/ floor f* f- ; ( r1 r2 -- f )

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
\ : f** flog2 f* exp ;
: get ( "123" -- : get a single signed number )
  bl word dup 1+ c@ [char] - = tuck -
  0 0 rot convert drop ( -$18 and throw ) -+ ;
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

\ "fexpm1" needs implementing better
: fexpm1 fexp fone f- ; ( f -- f : e raised to 'f' less 1 )
: fsinh fexpm1 fdup fdup f1+ f/ f+ f2/ ; ( f -- fsinh : hyperbolic sine )
: fcosh fexp fdup fone fswap f/ f+ f2/ ; ( f -- fcosh : hyperbolic cosine )
: fsincosh fdup fsinh fswap fcosh ; ( f -- sinh cosh )
: ftanh fsincosh f/ ; ( f -- ftanh : hyperbolic tangent )
\ : fln fone f- flnp1 ;

3.14159265 fconstant fpi
1.57079632 fconstant fhpi
6.28318530 fconstant f2pi
2.71828182 fconstant fe

: fdeg f2pi f/ [ 360.0 ] fliteral f* ; ( rad -- deg )
: frad [ 360.0 ] fliteral f/ f2pi f* ; ( deg -- rad )

anonymous definitions

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

forth-wordlist current ! 

: fsincos ( rads -- sin cos )
   fdup f0< >r
   fabs
   f2pi fmod fdup quadrant dup >r scfix (fsincos) 
   r@ >cos fswap r> >sin fswap
   r> if fswap fnegate fswap then ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )
: ftan fsincos f/ ; ( rads -- tan )

: sins
  f2pi fnegate
  begin
    fdup f2pi f<
  while
    fdup fdup f. [char] , emit space fsincos 
    fswap f. [char] , emit space f. cr
    [ f2pi 50.0 f f/ ] 2literal f+
  repeat fdrop ;

system +order
\ okok <ok> !

only forth definitions decimal
.( DONE ) cr

\ TODO: Test
: f~ ( f1 f2 f3 -- flag )
  fdup f0> if 2>r f- fabs 2r> f< exit then
  fdup f0= if fdrop f= exit then
  fabs 2>r f2dup f+ 2r> f* 2>r f- 2r> f< ;

: fsqrt
  fdup f0< if fdrop -43 throw then
  fdup f0= if fdrop 0 0 exit then
  ( fone ) fdup f2/
  16 for aft 
  f2dup fsq fswap f- fover f2* f/ f-
  then next
  fnip ;
 
.s cr
256.0 f fsqrt f. cr
100.0 f fsqrt f. cr
 81.0 f fsqrt f. cr
 10.0 f fsqrt f. cr
  9.0 f fsqrt f. cr
  4.0 f fsqrt f. cr
  3.0 f fsqrt f. cr
  2.5 f fsqrt f. cr
  2.0 f fsqrt f. cr
  1.5 f fsqrt f. cr
  1.0 f fsqrt f. cr
  0.5 f fsqrt f. cr
  0.1 f fsqrt f. cr
  0.0 f fsqrt f. cr
.s

\ 200 f fsq f. -> Error
: disp e. ;
: tsqrt
  for aft 
  r@ dup * s>f fdup disp ."  <->" fsqrt disp cr
  then next ;

10 tsqrt

only forth definitions


: filog2 ( r -- u : Floating point integer logarithm )
  zero
  fdup fone f< -42 and throw
  ( norm ) nip $4001 - ;

: ff . space  ;
1 e 2 fdup  0 f f~ ff
1 e 2 fdup  1 f f~ ff
1 e 2 fdup -1 f f~ ff
1 e 2 fdup fnegate  0 f f~ ff
1 e 2 fdup fnegate  1 f f~ ff
1 e 2 fdup fnegate -1 f f~ ff
1 e 2 fdup fnegate fswap  0 f f~ ff
1 e 2 fdup fnegate fswap  1 f f~ ff
1 e 2 fdup fnegate fswap -1 f f~ ff


