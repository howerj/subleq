<ok> @ ' nop <ok> !
: debug source type ."  ok" cr ; ' debug <ok> !

 0 constant false
-1 constant true
variable seed ( NB. Could be mixed with keyboard input )
: random ( -- u : 16-bit xorshift )
  seed @ dup 0= if 0= then ( seed must not be zero )
  dup 13 lshift xor
  dup  9 rshift xor
  dup  7 lshift xor
  dup seed ! ;
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
: cell- 2 - ;
: rpick rp@ swap - 1- 2* @ ;
: c, here c! 1 allot ; ( c -- )
: cell- cell - ; ( a -- a )
: umin 2dup swap u< if swap then drop ; ( u u -- u )
: umax 2dup      u< if swap then drop ; ( u u -- u )
: off false swap ! ; ( a -- )
: on true swap ! ; ( a -- )
: ?exit if rdrop then ;
: tab 9 emit ; ( -- )
: spaces ( n -- : equiv. bl banner  )
    ?dup 0> if for aft space then next then ;
: 2+ 2 + ; ( u -- u )
: 2- 2 - ; ( u -- u )
: not -1 xor ; ( u -- u )
: binary $2 base ! ; ( -- )
: octal $8 base ! ; ( -- )
: .base base @ dup decimal . base ! ; ( -- )
: only -1 set-order ; ( -- )
: also get-order over swap 1+ set-order ; ( -- )
: previous get-order swap drop 1- set-order ; ( -- )
: buffer block ; ( k -- a )
: enum dup constant 1+ ; ( n --, <string> )
: logical 0= 0= ; ( n -- f )
: square dup * ; ( n -- n )
: limit rot min max ; ( n lo hi -- n )
: odd 1 and logical ; ( n -- f )
: even odd invert ; ( n -- f )
: nor or invert ; ( u u -- u )
: nand and invert ; ( u u -- u )
: under >r dup r> ; ( n1 n2 -- n1 n1 n2 )
: 2nip >r >r 2drop r> r> ; ( n1 n2 n3 n4 -- n3 n4 )
( n1 n2 n3 n4 -- n1 n2 n3 n4 n1 n2 )
: 2over >r >r 2dup r> swap >r swap r> r> -rot ;
: 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
: 2tuck 2swap 2over ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 n3 n4 )
: 4drop 2drop 2drop ; ( n1 n2 n3 n4 -- )
: trip dup dup ; ( n -- n n n )
: log  >r 0 swap ( u base -- u )
  begin swap 1+ swap r@ / dup 0= until
  drop 1- rdrop ;
: log2 0 swap ( u -- u )
  begin swap 1+ swap   2/ dup 0= until
  drop 1- ;
: average um+ 2 um/mod nip ; ( u u -- u )
: <=> 2dup > if 2drop -1 exit then < ;
: bounds over + swap ;
: 2, , , ; ( n n -- )
: d< rot 2dup >                    ( d -- f )
   if = nip nip if 0 exit then -1 exit then
   2drop u< ;
: d>= d< invert ;                  ( d -- f )
: d>  2swap d< ;                   ( d -- f )
: d<= d> invert ;                  ( d -- f )
\ : du> 2swap du< ;                  ( d -- f )
: d=  rot = -rot = and ;           ( d d -- f )
: d- dnegate d+ ;                  ( d d -- d )
: dabs  s>d if dnegate exit then ; ( d -- ud )
: d<> d= 0= ;                      ( d d -- f )
: 2rdrop r> rdrop rdrop >r ; ( R: n n -- )
: 2. swap . . ;
: m* 2dup xor 0< >r abs swap abs um* r> if dnegate then ;
: */mod  >r m* r> m/mod ;  ( n n n -- r q )
: */  */mod nip ;          ( n n n -- q )
: holds begin dup while 1- 2dup + c@ hold repeat 2drop ;
: roll  dup 0> if swap >r 1- recurse r> swap else drop then ;
: signum dup 0< swap 0> 1 and xor ; ( n -- -1 | 0 1 : signum )
: >< dup 8 rshift swap 8 lshift or ; ( u -- u : swap bytes )
: #digits >r dup 0= if 1+ exit then r> log 1+ ; ( u b -- u )
: ** ( n u -- n )
  ?dup if
    over >r
    begin
      dup 1 >
    while
      swap r@ * swap 1-
    repeat rdrop drop
  else logical 1 and then ;
: b. base @ swap 2 base ! u. base ! ; ( u -- )
: h. base @ swap hex u. base ! ;      ( u -- )
: o. base @ swap 8 base ! u. base ! ; ( u -- )
: d. base @ swap decimal . base ! ;   ( n -- )
: @bits swap @ and ;                  ( a u -- u )
: ?\ if postpone \ then ; immediate
: ?( if postpone ( then ; immediate ( )
: ?if compile dup postpone if ; immediate
: screens ( k1 k2 -- : list blocks k1 to k2 )
  over -
  for
    dup . dup list 1+ key $D = if rdrop drop exit then
  next drop ;
: /string over min rot over + -rot - ; 
: ndrop for aft drop then next ;
: unused $FFFF here - ; ( 65536 bytes available in this VM )
: char+ 1+ ;
: mux dup >r and swap r> invert and or ; ( x1 x2 mask -- x )
\ : or dup mux ;
\ : and 0 -rot mux ;

\ Usage:
\
\        : x case
\          1 of ." one" endof
\          2 of ." two" endof
\          ." default"
\          endcase ;
\
system +order definitions
: (case) r> swap >r >r	; compile-only
: (of) r> r@ swap >r = ; compile-only
: (endcase) r> r> drop >r ;
forth-wordlist +order definitions
: case compile (case) 30 ; compile-only immediate
: of compile (of) postpone if ; compile-only immediate
: endof postpone else 31 ; compile-only immediate
: endcase
   begin
    dup 31 =
   while
    drop			
    postpone then
   repeat
   30 <> abort" Bad case construct!"
   compile (endcase) ; compile-only immediate
only forth definitions

system +order definitions
: r+ 1+ ; ( NB. Should be cell+ on most platforms )
: (unloop) r> rdrop rdrop rdrop >r ; compile-only
: (leave) rdrop rdrop rdrop ; compile-only
: (j) 4 rpick ; compile-only
: (k) 7 rpick ; compile-only
: (do) r> dup >r swap rot >r >r r+ >r ; compile-only
: (?do) 
   2dup <> if
     r> dup >r swap rot >r >r r+ >r exit
   then 2drop ; compile-only
: (loop) 
  r> r> 1+ r> 2dup <> if
    >r >r 2* @ >r exit \ NB. 2* and 2/ cause porting problems
  then >r 1- >r r+ >r ; compile-only
: (+loop) 
   r> swap r> r> 2dup - >r
   2 pick r@ + r@ xor 0>=
   3 pick r> xor 0>= or if
     >r + >r 2* @ >r exit
   then >r >r drop r+ >r ; compile-only
forth-wordlist +order definitions
: unloop compile (unloop) ; immediate compile-only 
: i compile r@ ; immediate compile-only
: j compile (j) ; immediate compile-only
: k compile (k) ; immediate compile-only
: leave compile (leave) ; immediate compile-only
: do compile (do) 0 , here ; immediate compile-only
: ?do compile (?do) 0 , here ; immediate compile-only
: loop 
  compile (loop) dup 2/ , 
  compile (unloop) 
  cell- here cell- 2/ swap ! ; immediate compile-only
: +loop 
  compile (+loop) dup 2/ , 
  compile (unloop) 
  cell- here cell- 2/ swap ! ; immediate compile-only
only forth definitions


\ \ https://news.ycombinator.com/item?id=27485454
\ 
\ \ TODO: make a standards compliant version,
\ \ with "+field" and "field:" that can handle 32-bit data
\ \ and perhaps even bit-fields. 
\
  
: struct 0 ;
: field:  ( offset size -- offset' )
  create over , +
  does> @ + ;
: byte:   1 field: ;
: cell:   2 field: ;
: long:   4 field: ;
: union:  0 field: ;
: unused  +  ;

: size:  constant  ;
: ;struct drop ;

\ Example:
\ 
\ struct
\   byte: >b1
\   byte: >b2
\   cell: >w1
\   cell: >w2
\ size: /foo
\ 
\ struct
\   byte: >c1
\   7 unused
\   /foo field: >foo
\ ;struct
\ 
: mark 
  $" defined (mark) [if] (mark) [then] marker (mark) " 
  count evaluate ;
mark
' nop <ok> !
.( LOADED EFORTH. ) cr
.( DICTIONARY: ) here . cr
.( EFORTH:     ) ' 1- u. cr
<ok> !


