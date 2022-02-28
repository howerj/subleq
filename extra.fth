\ TODO: Merge this into subleq.fth in its own section
\ : debug source type ."  ok" cr ;
<ok> @ ' nop <ok> !
: cell- cell - ; ( a -- a )
: umin 2dup swap u< if swap then drop ; ( u u -- u )
: umax 2dup      u< if swap then drop ; ( u u -- u )
 0 constant false
-1 constant true
: off false swap ! ; ( a -- )
: on true swap ! ; ( a -- )
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
\ : c, $FF and
\   here @ here 1 and if 
\     $FF and swap 8 lshift or 
\   else $FF00 and or then
\   here ! 1 allot ;
\ : thru ;

: compile-only ;

\ TODO: Get this working!
\
: (do) r> dup >r swap rot >r >r cell+ >r ; compile-only
: (?do) 
   2dup <> if
     r> dup >r swap rot >r >r cell+ >r exit
   then 2drop exit ; compile-only
: (loop) 
   r> r> 1+ r> 2dup <> if
    >r >r @ >r exit
  then >r 1- >r cell+ >r ; compile-only
: (+loop) 
   r> swap r> r> 2dup - >r
   2 pick r@ + r@ xor 0< 0=
   3 pick r> xor 0< 0= or if
    >r + >r @ >r exit
   then >r >r drop cell+ >r 
; compile-only
: unloop r> rdrop rdrop rdrop >r ; compile-only
: ?do compile (?do) 0 , here ; immediate compile-only
: do compile (do) 0 , here ; immediate compile-only
: loop compile (loop) dup , compile unloop cell- here 2/ swap ! ; immediate compile-only
: +loop compile (+loop) dup , compile unloop cell- here 2/ swap ! ; immediate compile-only
: leave rdrop rdrop rdrop ; immediate compile-only
: i r> r> tuck >r >r ; compile-only
\ : j ; compile-only

\ Usage:
\
\        : x case
\          1 of ." one" endof
\          2 of ." two" endof
\          ." default"
\          endcase ;
\
: (case) r> swap >r >r	; compile-only
: case compile (case) 30 ; compile-only immediate
: (of) r> r@ swap >r = ; compile-only
: of compile (of) postpone if ; compile-only immediate
: endof postpone else 31 ; compile-only immediate
: (endcase) r> r> drop >r ;
: endcase
   begin
    dup 31 =
   while
    drop			
    postpone then
   repeat
   30 <> abort" Bad case construct!"
   compile (endcase) ; compile-only immediate
<ok> !
.( LOADED. DIC: ) here . .(  UNUSED: ) unused u. cr

