\ FORTH Floating point package
\
\ TODO: "100." fails (gives 1000.0), "100.0" works
\ TODO: Using "num." instead of "num.num" causes problems:
\        3276. f f. ( prints 3276.000 )
\        10. f f.   ( prints 17.000 )
\        10.0 f f.   ( prints 10.000 )
\ TODO: Hide words that should not be exported / put in 
\       different vocab
\ TODO: Meta-compilation
\ TODO: Documentation
\ TODO: Optional FP stack?
\ TODO: Integrate into base system?
\ NB. Input is finicky, requires "dpl" to be set, so
\ "fone e 1" will not work, but "1.0 e 1" will.
\
\ TODO: Documentation
\ TODO: Add back in unit test framework?
\ missing: f**, facos, fln, facosh, falog, fasin, fasinh, 
\ fatan, fatan2, fatanh, fsqrt, 
\
\
\ https://stackoverflow.com/questions/3581528/how-is-the-square-root-function-implemented
\ https://stackoverflow.com/questions/4541130/definitions-of-sqrt-sin-cos-pow-etc-in-cmath?noredirect=1&lq=1
\ https://forth-standard.org/standard/float/
\


only forth definitions decimal
: debug source type ."  ok" cr ; <ok> @ ' debug <ok> !
constant okok


: anonymous get-order 1+ here 1 cells allot swap set-order ;
: 1+! 1 swap +! ;
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
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
: /string over min rot over + -rot - ;  ( b u1 u2 -- b u : advance string u2 )
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r          ( dl dh nn -- dl dh,      R: -- dh nn )
  dabs r@ abs um/mod  ( dl dh    -- rem quo,    R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : use double precision intermediate value )
    >r m* r> sm/rem ;

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

: zero  over 0= if drop 0 then ; ( f -- f : zero exponent if mantissa is )
: norm  >r 2dup or               ( f -- f : normalize input float )
        if begin s>d invert
           while d2* r> 1- >r
           repeat swap 0< - ?dup
           if r> else $8000 r> 1+ then
        else r> drop then ;
: lalign $20 min for aft d2/ then next ;
: ralign 1- ?dup if lalign then 1 0 d+ d2/ ;
: tens 2* cells  [ ftable cell+ ] literal + 2@ ;     
: set-precision dup 0 5 within if ftable ! exit then -$2B throw ; ( +n -- )
: shifts fabs $4010 - s>d invert if -$2B throw then negate ;
: base? base @ $A <> if -$28 throw then ; ( -- : check base )
: unaligned? dup 1 and = if -$9 throw then ; ( -- : check ptr )

\ forth-wordlist current !

: precision ftable @ ; ( -- u )
: f@ unaligned? 2@ ;  ( a -- f )
: f! unaligned? 2! ;  ( f a -- )
: falign align ;      ( -- )
: faligned aligned ;  ( a -- a )
: fdepth depth ;      ( -- u )
: fdup 2dup ;         ( f -- f f )
: fswap 2swap ;       ( f1 f2 -- f2 f1 )
: fover 2over ;       ( f1 f2 -- f1 f2 f1 )
: f2dup fover fover ; ( f1 f2 -- f1 f2 f1 f2 )
: frot 2>r fswap 2r> fswap ; ( f1 f2 f3 -- f2 f3 f1 )
: fdrop 2drop ;       ( f -- )
: fnip fswap fdrop ;  ( f1 f2 -- f2 )
: fnegate $8000 xor zero ;  ( f -- f )
: fsign fabs over 0< if >r dnegate r> $8000 or then ;
: f2* 1+ zero ;       ( f -- f )
: f*  rot + $4000 - >r um* r> norm ;     ( f f -- f )
: fsq fdup f* ;       ( f -- f )
: f2/ 1- zero ;       ( f -- f )
: f0= zero d0= ;      ( f -- f )
: floats 2* cells ;   ( u -- u )
: float+ 1 floats + ; ( a -- a )
: ftuck fover fswap ; ( f1 f2 -- f2 f1 f2 )
: um/ dup >r um/mod swap r> over 2* 1+ u< swap 0< or - ; ( ud u -- u )
: convert count >number drop ; ( +d1 addr1 -- +d2 addr2 )

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

: f- fnegate f+ ; ( f1 f2 -- t : floating point subtract )
: f< f- 0< nip ; ( f1 f2 -- t : floating point less than )
: f> fswap f< ;  ( f1 f2 -- t : floating point greater than )
: f>= f> 0= ;    ( f1 f2 -- t : FP greater or equal )
: f<= f< 0= ;    ( f1 f2 -- t : FP less than or equal )
: f0> 0 0 f> ;   ( f1 f2 -- t : FP greater than zero )
: f0< 0 0 f< ;   ( f1 f2 -- t : FP less than zero )
: f0<= f0> 0= ;  ( f1 f2 -- t : FP less than or equal to zero )
: f0>= f0< 0= ;  ( f1 f2 -- t : FP more than or equal to zero )
: fmin f2dup f< if fdrop exit then fnip ; ( f1 f2 -- f : min )
: fmax f2dup f> if fdrop exit then fnip ; ( f1 f2 -- f : max )
: d>f $4020 fsign norm ;  ( d -- f : double to float )
: s>f s>d d>f ;           ( n -- f )

: f#    base?
        >r precision tens drop um* r> shifts
        ralign precision ?dup if for aft # then next
        [char] . hold then #s rot sign ;

: f. tuck <# f# #> type space ;
\ NB. 'f' and 'e' require "dpl" to be set correctly!
system +order
: f 
   base?
   1 ?depth ( NB. 2 ?depth if following line missing )
   dpl @ 0< if s>d 0 dpl ! then ( input was single number )
   d>f dpl @ tens d>f f/ ;    ( n|d -- f : formatted double to float )
system -order
: fconstant f 2constant ;          ( "name" , f --, Run Time: -- f )
: fliteral  f postpone 2literal ; immediate ( f --, Run Time: -- f )
: -+    drop swap 0< if negate then ;
: fix   tuck 0 swap shifts ralign -+ ; ( f -- n : float to int, rounding )
: f>s   tuck 0 swap shifts lalign -+ ; ( f -- n : float to int, truncating )
: floor  f>s s>f ; ( f -- f )
: fround fix s>f ; ( f -- f )
: fmod f2dup f/ floor f* f- ;

1.0 fconstant fone decimal

: exp ( f -- f : raise 2.0 to the power of 'f' )
 2dup f>s dup >r s>f f-     
 f2* [ -57828.0 ] fliteral 2over fsq [ 2001.18 ] fliteral f+ f/
 2over f2/ f- [ 34.6680 ] fliteral f+ f/
 fone f+ fsq r> + ;
: fexp  ( f -- f : raise e to the power of 'f' )
  [ 1.4427 ] fliteral f* exp ; 
: get ( "123" -- : get a single signed number )
  bl word dup 1+ c@ [char] - = tuck -
  0 0 rot convert drop ( -$18 and throw ) -+ ;
: e ( f "123" -- usage "1.23 e 10", input scientific notation )
  f get >r r@ abs 13301 4004 */mod
  >r s>f 4004 s>f f/ exp r> +
  r> 0< if f/ else f* then ;
: e. ( f -- : output scientific notation )
  tuck fabs 16384 tuck -
  4004 13301 */mod >r
  s>f 4004 s>f f/ exp f*
  2dup fone f<
  if 10 s>f f* r> 1- >r then
  <# r@ abs 0 #s r> sign 2drop
  [char] e hold f# #> type space ;

\ "fexpm1" needs implementing better
: fexpm1 fexp fone f- ;                      ( f -- f : e raised to 'f' less 1 )
: fsinh fexpm1 fdup fdup fone f+ f/ f+ f2/ ; ( f -- fsinh : hyperbolic sine )
: fcosh fexp   fdup fone fswap f/ f+ f2/ ;   ( f -- fcosh : hyperbolic cosine )
: fsincosh fdup fsinh fswap fcosh ;          ( f -- sinh cosh )
: ftanh fsincosh f/ ;                        ( f -- ftanh : hyperbolic tangent )
\ : fln fone f- flnp1 ;

3.14159265 fconstant pi
1.57079632 fconstant pi/2
6.28318530 fconstant 2pi
2.71828182 fconstant euler

: >deg [ pi f2* ] 2literal f/ [  360.0 ] fliteral f* ; ( rad -- deg )
: >rad [  360.0 ] fliteral f/ [ pi f2* ] 2literal f* ; ( deg -- rad )

\ anonymous definitions

\ TODO: The quadrants are all fucked up, otherwise sin/cos
\ "work", in that the curves look alright, albeit flipped

: >cordic     [ 16384. ] fliteral f* f>s ; ( f -- n )
: cordic> s>f [ 16384. ] fliteral f/ ;     ( n -- f )
: quadrant fdup                   f0< 4 and >r
           fabs 2pi fmod fdup pi   f< 1 and >r 
                 pi fmod      pi/2 f> 2 and r> r> or or ;
: >sin dup 3 and >r 4 and if fnegate r> -1 >r >r else r> 0 >r >r  then
          r@ 3 = r@ 2 = or if fnegate fone f+ then
          r@ 0 = r@ 2 = or if fnegate then 
          rdrop r> if fnegate then ;

: >cos 3 and >r
         r@ 3 = r@ 2 = or if fnegate fone f+ then
         r@ 0 = r@ 3 = or if fnegate then 
         rdrop ;

: (fsincos) pi/2 fmod >cordic cordic >r cordic> r> cordic> ; 

\ forth-wordlist current ! 

\ TODO: fsincos still needs a lot of work, and simplifying
: fsincos 2pi fmod fdup quadrant >r (fsincos) r@ >cos fswap r> >sin fswap ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )
: ftan fsincos f/ ; ( rads -- tan )

only forth definitions
   
\ : fpow ( f u -- f : raise 'f' to an integer power )
\       ?dup 0= if fdrop fone exit then
\       >r fdup r> 1- for aft fover f* then next fnip ;

\ https://stackoverflow.com/questions/9799041/
\ https://en.wikipedia.org/wiki/Taylor_series


: .q f. ." <-> " source type cr ;
0. f         fsin .q
pi 0.25 f f* fsin .q
pi/2         fsin .q
pi           fsin .q

0. f         fcos .q
pi 0.25 f f* fcos .q
pi/2         fcos .q

: sins
  2pi fnegate
  begin
    fdup 2pi f<
  while
    fdup fdup f. [char] , emit space fsincos fswap f. [char] , emit space f. cr
    [ 2pi 50.0 f f/ ] 2literal f+
  repeat fdrop ;

    

: quads 
  [ 0.0 ] fliteral 
  begin
    fdup 2pi f<
  while
    fdup fdup f. [char] : emit quadrant . cr
    [ 2pi 50.0 f f/ ] 2literal f+
  repeat fdrop ;

\ : fcos
\    fone  ( rads -- f )
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

okok <ok> !
.( DONE ) cr




