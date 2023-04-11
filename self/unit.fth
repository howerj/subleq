: debug source type ."  ok" cr ; ' debug <ok> !

only forth definitions system +order

: anonymous get-order 1+ here 1 cells allot swap set-order ;
: undefined? bl word find nip 0= ; ( "name", -- f: Is word not in search order? )
: defined? undefined? 0= ;   ( "name", -- f: Is word in search order? )
: ?\ 0= if postpone \ then ; ( f --, <string>| : conditional compilation )
: 1+! 1 swap +! ;
: dabs s>d if dnegate then ;   ( d -- ud )
: +- 0< if negate then ; ( n n -- n : copy sign )
: >< dup 8 rshift swap 8 lshift or ; ( u -- u : byte swap )
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ; ( n n -- d )
: /string ( b u1 u2 -- b u : advance string u2 )
  over min rot over + -rot - ; 
: sm/rem ( dl dh nn -- rem quo: symmetric division )
  over >r >r         ( dl dh nn -- dl dh,   R: -- dh nn )
  dabs r@ abs um/mod ( dl dh    -- rem quo, R: dh nn -- dh nn )
  r> r@ xor +- swap r> +- swap ;
: */mod ( a b c -- rem a*b/c : double precision intermediate val )
    >r m* r> sm/rem ;

\ From: https://en.wikipedia.org/wiki/Integer_square_root
\ This function computes the integer square root of a number.
\
\ - 'lc' = large candidate
\ - 'sc' = small candidate
\
: sqrt ( n -- u : integer square root )
  s>d  if -$B throw then ( does not work for signed values )
  dup 2 < if exit then   ( return 0 or 1 )
  dup                    ( u u )
  2 rshift recurse 2*    ( u sc )
  dup                    ( u sc sc )
  1+ dup square          ( u sc lc lc^2 )
  >r rot r> <            ( sc lc bool )
  if drop else nip then ; ( return sc or lc respectively )

: log ( u base -- u : integer logarithm of u in 'base' )
  >r
  dup 0= if -$B throw then ( logarithm of zero is an error )
  0 swap
  begin
    swap 1+ swap r@ / dup 0= ( keep dividing until 'u' is 0 )
  until
  drop 1- rdrop ;

: log2 2 log ; ( u -- u : integer logarithm of u in base )

\ =================== UNIT TEST FRAMEWORK =====================
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

: quine source type cr ; ( -- : print out current input line )
: ndrop for aft drop then next ;  ( a0...an n -- )
: ndisplay for aft . then next ;  ( a0...an n -- )
: empty-stacks depth ndrop ;      ( a0...an -- )
: .pass   verbose @ 1 > if ."   ok: " space quine then ; ( -- )
: .failed verbose @ 0 > if ." fail: " space quine then ; ( -- )
: pass passed 1+! ;               ( -- )
: fail empty-stacks -$B throw ;   ( -- )

\ 'equal' is the most complex word in this test bench, it tests 
\ whether two groups of numbers of the same length are equal, 
\ the length of the numbers is specified by the first argument 
\ to 'equal'.
: equal ( a0...an b0...bn n -- a0...an b0...bn n f )
  dup n !
  for aft
    r@ pick r@ n @ 1+ + pick xor if rdrop n @ 0 exit then
  then next n @ -1 ;

\ '?stacks' is given two numbers representing stack depths, if 
\ they are not equal it prints out an error message, and calls 
\ 'abort'.
: ?stacks ( u u -- )
  2dup xor
  if
    .failed ." Too Few/Many Arguments Provided" cr
    ." Expected:  " u. cr
    ." Got: "       u. cr
    ." Full Stack:" .s cr
    fail exit
  else 2drop then ;

\ 'equal?' takes two lists of numbers of the same length and 
\ checks if they are equal, if they are not then an error 
\ message is printed and 'abort' is called.
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
: throws? ( "name" -- n  )
  postpone ' catch >r empty-stacks r> ; 

: logger( verbose @ 1 > if postpone .( cr exit then postpone ( ;
: logger\ verbose @ 1 > if exit then postpone \ ;

\ =================== UNIT TEST FRAMEWORK =====================

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

.( TESTS COMPLETE ) cr
decimal
.( passed: ) statistics u. space .( / ) 0 u.r cr
.( here:   ) here . cr
statistics <> ?\ .( [FAILED]     ) cr  \ abort
statistics  = ?\ .( [ALL PASSED] ) cr   

.( CALLING MARKER 'XXX' ) cr
xxx


