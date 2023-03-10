: debug source type ."  ok" cr ; <ok> @ ' debug <ok> !
constant okok

only forth definitions system +order

\ Create anonymous namespace:
: anonymous get-order 1+ here 1 cells allot swap set-order ;
: undefined? bl word find nip 0= ; ( "name", -- f: Is word not in search order? )
: defined? undefined? 0= ;       ( "name", -- f: Is word in search order? )
: ?\ 0= if postpone \ then ;    ( f --, <string>| : conditional compilation )
\ undefined? rdup ?\ : rdup r> r> dup >r >r >r ;
undefined? 1+!  ?\ : 1+! 1 swap +! ;
: arshift ( n u -- n : arithmetic right shift )
  2dup rshift >r swap $8000 and
  if $10 swap - -1 swap lshift else drop 0 then r> or ;
: d2* over $8000 and >r 2* swap 2* swap r> if 1 or then ;
: d2/ dup      1 and >r 2/ swap 2/ r> if $8000 or then swap ;
: d- dnegate d+ ;
\ : d= rot = -rot = and ;
: d0= or 0= ;
\ : d0<> d0= 0= ;
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
\ : /string over min rot over + -rot - ;  ( b u1 u2 -- b u : advance string u2 )
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r          ( dl dh nn -- dl dh,      R: -- dh nn )
  dabs r@ abs um/mod  ( dl dh    -- rem quo,    R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : use double precision intermediate value )
    >r m* r> sm/rem ;

\    : square dup * ;
\    \ From: https://en.wikipedia.org/wiki/Integer_square_root
\    \ This function computes the integer square root of a number.
\    : sqrt ( n -- u : integer square root )
\      s>d  if -$B throw then ( does not work for signed values )
\      dup 2 < if exit then      ( return 0 or 1 )
\      dup                       ( u u )
\      2 rshift recurse 2*       ( u sc : 'sc' == unsigned small candidate )
\      dup                       ( u sc sc )
\      1+ dup square             ( u sc lc lc^2 : 'lc' == unsigned large candidate )
\      >r rot r> <               ( sc lc bool )
\      if drop else nip then ;   ( return small or large candidate respectively )
\    
\    : log ( u base -- u : compute the integer logarithm of u in 'base' )
\      >r
\      dup 0= if -$B throw then ( logarithm of zero is an error )
\      0 swap
\      begin
\        swap 1+ swap r@ / dup 0= ( keep dividing until 'u' is 0 )
\      until
\      drop 1- rdrop ;
\    
\    : log2 2 log ; ( u -- u : compute the integer logarithm of u in base )
\    
\    \ http://forth.sourceforge.net/algorithm/bit-counting/index.html
\    : count-bits ( number -- bits )
\      dup $5555 and swap 1 rshift $5555 and +
\      dup $3333 and swap 2 rshift $3333 and +
\      dup $0F0F and swap 4 rshift $0F0F and +
\      $FF mod ;
\    
\    \ http://forth.sourceforge.net/algorithm/firstbit/index.html
\    : first-bit ( number -- first-bit )
\      dup   1 rshift or
\      dup   2 rshift or
\      dup   4 rshift or
\      dup   8 rshift or
\      dup $10 rshift or
\      dup   1 rshift xor ;
\    
\    : gray-encode dup 1 rshift xor ; ( gray -- u )
\    : gray-decode ( u -- gray )
\    \ dup $10 rshift xor ( <- 32 bit )
\      dup   8 rshift xor 
\      dup   4 rshift xor
\      dup   2 rshift xor 
\      dup   1 rshift xor ;
\    
\    : binary $2 base ! ;
\    
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

\ This floating point library has been adapted from one found in
\ Vierte Dimensions Vol.2, No.4 1986, it should be free to use so long as the
\ following copyright is left in the code:
\ 
\            FORTH-83 FLOATING POINT.
\       ----------------------------------
\       COPYRIGHT 1985 BY ROBERT F. ILLYES
\
\             PO BOX 2516, STA. A
\             CHAMPAIGN, IL 61820
\             PHONE: 217/826-2734 
\
\ NB. There is not under or overflow checking, nor division by zero checks
only forth definitions system +order
\ variable float-voc

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
: f2* 1+ zero ;                          ( f -- f )
: f*  rot + $4000 - >r um* r> norm ;     ( f f -- f )
: fsq fdup f* ;                          ( f -- f )
: f2/ 1- zero ;                          ( f -- f )
: um/ dup >r um/mod swap r> over 2* 1+ u< swap 0< or - ; ( ud u -- u )
: f0= zero d0= ;                       ( f -- f )

\ TODO: More invalid checks? +-INF, NAN?

: f/    
\       fdup f0= if -44 throw then
        rot swap - $4000 + >r
        0 -rot 2dup u<
        if  um/ r> zero 
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

: f- fnegate f+ ; ( f1 f2 -- t : floating point subtract )
: f< f- 0< nip ;  ( f1 f2 -- t : floating point less than )
: f> fswap f< ;   ( f1 f2 -- t : floating point greater than )
: f>= f> 0= ;
: f<= f< 0= ;
: f0> 0 0 f> ;
: f0< 0 0 f< ;
: f0<= f0> 0= ;
: f0>= f0< 0= ;


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

\ TODO: Detect single point number entry and correct for it
\ : point dpl @ dup 0< if drop 0 then ;
: f.    tuck <# f# #> type space ;
: d>f $4020 fsign norm ;           ( d -- f : double to float )
: f     d>f dpl @ tens d>f f/ ;    ( d -- f : formatted double to float )
: fconstant f 2constant ;          ( "name" , f --, Run Time: -- f )
: fliteral  f postpone 2literal ; immediate ( f --, Run Time: -- f )
: s>f   s>d d>f ;                  ( n -- f )
: -+    drop swap 0< if negate then ;
: fix   tuck 0 swap shifts ralign -+ ;
: f>s   tuck 0 swap shifts lalign -+ ; ( f -- n )

1. fconstant fone decimal

: exp   2dup f>s dup >r s>f f-     ( f -- f : raise 2.0 to the power of 'f' )
        f2* [ -57828. ] fliteral 2over fsq [ 2001.18 ] fliteral f+ f/
        2over f2/ f- [ 34.6680 ] fliteral f+ f/
        fone f+ fsq r> + ;
: fexp  [ 1.4427 ] fliteral f* exp ; ( f -- f : raise e to the power of 'f' )

: convert count >number drop ; ( +d1 addr1 -- +d2 addr2 )

\ TODO: Fix "get"
: get   bl word dup 1+ c@ [char] - = tuck -
        0 0 rot convert drop ( -$18 and throw ) -+ ;

\ NB. Input is finicky, requires "dpl" to be set, so
\ "fone e 1" will not work, but "1.0 e 1" will.
\
\ TODO: Get with negatives fails
\ TODO: Documentation

: e     f get >r r@ abs 13301 4004 */mod
        >r s>f 4004 s>f f/ exp r> +
        r> 0< if f/ else f* then ;

: e.    tuck fabs 16384 tuck -
        4004 13301 */mod >r
        s>f 4004 s>f f/ exp f*
        2dup fone f<
        if 10 s>f f* r> 1- >r then
        <# r@ abs 0 #s r> sign 2drop
        [char] e hold f# #>     type space ;

\ "fexpm1" needs implementing better
: fexpm1 fexp fone f- ;                      ( f -- f : e raised to 'f' less 1 )
: fsinh fexpm1 fdup fdup fone f+ f/ f+ f2/ ; ( f -- fsinh : hyperbolic sine )
: fcosh fexp   fdup fone fswap f/ f+ f2/ ;   ( f -- fcosh : hyperbolic cosine )
: fsincosh fdup fsinh fswap fcosh ;         ( f -- sinh cosh )
: ftanh fsincosh f/ ;                       ( f -- ftanh : hyperbolic tangent )

\ : fln fone f- flnp1 ;

3.14159265 fconstant pi
1.57079632 fconstant pi/2
6.28318530 fconstant 2pi
2.71828182 fconstant euler

: >deg [ pi f2* ] 2literal f/ [   360. ] fliteral f* ; ( rad -- deg )
: >rad [   360. ] fliteral f/ [ pi f2* ] 2literal f* ; ( deg -- rad )

: floor  f>s s>f ; ( f -- f )
: fround fix s>f ; ( f -- f )
: ftuck fover fswap ; ( f1 f2 -- f2 f1 f2 )

anonymous definitions

\ TODO: The quadrants are all fucked up, otherwise sin/cos
\ "work", in that the curves look alright, albeit flipped

: fmod f2dup f/ floor f* f- ;
: >cordic     [ 16384. ] fliteral f* f>s ;   ( f -- n )
: cordic> s>f [ 16384. ] fliteral f/ ;       ( n -- f )
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

forth-wordlist current ! 

\ @warning fsincos still needs a lot of work, and simplifying
: fsincos 2pi fmod fdup quadrant >r (fsincos) r@ >cos fswap r> >sin fswap ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )

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
    [ 2pi 50. f f/ ] 2literal f+
  repeat fdrop ;

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

system +order
\ hide norm hide zero hide tens hide ralign hide lalign
\ hide   -+ hide  fone hide fix  hide shifts 
system -order

okok <ok> !
.( DONE ) cr




