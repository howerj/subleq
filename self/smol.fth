: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
only forth definitions system +order
: anonymous get-order 1+ here 1 cells allot swap set-order ;
: undefined? bl word find nip 0= ; ( "name", -- f: Is word not in search order? )
: defined? undefined? 0= ;       ( "name", -- f: Is word in search order? )
: ?\ 0= if postpone \ then ;    ( f --, <string>| : conditional compilation )
undefined? 0<   ?\ : 0< 0 < ;
undefined? 1-   ?\ : 1- 1 - ;
undefined? 2*   ?\ : 2* 1 lshift ;
undefined? 1+!  ?\ : 1+! 1 swap +! ;
: within over - >r - r> u< ; ( u lo hi -- f )
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
: >< dup 8 rshift swap 8 lshift or ; ( u -- u : byte swap )
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
: nand and invert ;  ( u u -- u )
: nor  and invert ;  ( u u -- u )
: @bits swap @ and ; ( a u -- u )
: ascii state @ if postpone [char] else char then ; immediate
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r          ( dl dh nn -- dl dh,      R: -- dh nn )
  dabs r@ abs um/mod  ( dl dh    -- rem quo,    R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : use double precision intermediate value )
    >r m* r> sm/rem ;
: factorial ?dup 0= if 1 exit then >r 1 r> 1- for r@ 1+ * next ; ( u -- u )
: permutations over swap - factorial swap factorial swap / ; ( u1 u2 -- u )
: combinations dup dup permutations >r permutations r> / ;   ( u1 u2 -- u )
: gcd dup if tuck mod recurse exit then drop ;               ( u1 u2 -- u )
: lcm 2dup gcd / * ; \ Least Common Multiple                 ( u1 u2 -- u )
: square dup * ;                                             ( u -- u )
\ : >@ $3FFF and ;     ( a -- a : address with attribute bits masked off )
\ : >attr $C000 and ;  ( a -- u : get attribute bits from an address )
\ : link! dup @ >attr rot >@ or swap ! ; ( u a -- ) 
\ : link >@ @ >@ ;
\ : merge swap @ swap begin link dup link 0= until link! ; ( wid1 wid2 -- )
\ hide >@ hide link hide >attr hide link!
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
: count-bits ( number -- bits )
  dup $5555 and swap 1 rshift $5555 and +
  dup $3333 and swap 2 rshift $3333 and +
  dup $0F0F and swap 4 rshift $0F0F and +
  $FF mod ;
: first-bit ( number -- first-bit )
  dup   1 rshift or
  dup   2 rshift or
  dup   4 rshift or
  dup   8 rshift or
  dup $10 rshift or
  dup   1 rshift xor ;
: gray-encode dup 1 rshift xor ; ( gray -- u )
: gray-decode ( u -- gray )
  dup   8 rshift xor 
  dup   4 rshift xor
  dup   2 rshift xor 
  dup   1 rshift xor ;
: binary $2 base ! ;
 
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
: f/    
	( fdup f0= if -44 throw then )
        rot swap - $4000 + >r
        0 -rot 2dup u<
        if   um/ r> zero
        else >r d2/ fabs r> um/ r> 1+
        then ;
: f+    rot 2dup >r >r fabs swap fabs - ( f f -- f : floating point addition )
        dup if s>d
                if   rot swap  negate
                     r> r> swap >r >r
                then 0 swap ralign
        then swap 0 r> r@ xor 0<
        if   r@ 0< if 2swap then d-
             r> fsign rot swap norm
        else d+ if 1+ 2/ $8000 or r> 1+
                else r> then then ;
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
3.14159265 fconstant pi
1.57079632 fconstant pi/2
6.28318530 fconstant 2pi
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
: fsincos 2pi fmod fdup quadrant >r (fsincos) r@ >cos fswap r> >sin fswap ;
: fsin fsincos fdrop ; ( rads -- sin )
: fcos fsincos fnip  ; ( rads -- cos )
only forth definitions
   
system +order
hide norm hide zero hide tens hide ralign hide lalign
hide   -+ hide  one hide fix  hide shifts 
system -order
variable freelist  0 , 
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
