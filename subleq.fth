\ Project: Cross Compiler and eForth interpreter for a SUBLEQ CPU
\ License: The Unlicense
\ Author:  Richard James Howe
\ Email:   howe.r.j.89@gmail.com
\ Repo:    <https://github.com/howerj/subleq>
\
\ References:
\
\ - <https://en.wikipedia.org/wiki/Threaded_code>
\ - <https://github.com/howerj/embed>
\ - <https://github.com/howerj/forth-cpu>
\ - <https://github.com/samawati/j1eforth>
\ - <https://www.bradrodriguez.com/papers/>
\ - 8086 eForth 1.0 by Bill Muench and C. H. Ting, 1990
\
\ Tested with GForth version 0.7.3
\

only forth also definitions hex

wordlist constant meta.1
wordlist constant target.1
wordlist constant assembler.1
wordlist constant target.only.1

: (order) ( u wid*n n -- wid*n u n )
   dup if
    1- swap >r recurse over r@ xor if
     1+ r> -rot exit then r> drop then ;
: -order ( wid -- ) get-order (order) nip set-order ;
: +order ( wid -- ) dup >r -order get-order r> swap 1+ set-order ;

meta.1 +order also definitions

   2 constant =cell
4000 constant size ( 16384 bytes, 8192 cells )
4000 constant =end
  40 constant =stksz
  60 constant =buf
0008 constant =bksp
000A constant =lf
000D constant =cr
007F constant =del

create tflash size cells here over erase allot

variable tdp
variable tep
variable tlast
size =cell - tep !
0 tlast !

: :m meta.1 +order also definitions : ;
: ;m postpone ; ; immediate
:m there tdp @ ;m
:m tc! tflash + c! ;m
:m tc@ tflash + c@ ;m
:m t! over ff and over tc! swap 8 rshift swap 1+ tc! ;m
:m t@ dup tc@ swap 1+ tc@ 8 lshift or ;m
:m talign there 1 and tdp +! ;m
:m tc, there tc! 1 tdp +! ;m
:m t, there t! 2 tdp +! ;m
:m $literal [char] " word count dup tc, 0 ?do count tc, loop drop talign ;m
:m tallot tdp +! ;m
:m thead
  talign
  there tlast @ t, tlast !
  parse-word dup tc, 0 ?do count tc, loop drop talign ;m
:m dec# base @ >r decimal dup >r abs 0 <# bl hold #s r> sign #> r> base ! ;m
:m >neg dup 7FFF u> if 10000 - then ;
:m save-target ( <name> -- )
  parse-word w/o create-file throw
  there 0 do i t@  over >r >neg dec# r> write-file throw =cell +loop
   close-file throw ;m
:m .h base @ >r hex     u. r> base ! ;m
:m .d base @ >r decimal u. r> base ! ;m
:m tlen dup tflash + =cell + count 1f and nip ;m
:m twords
   cr tlast @
   begin
      dup dup tlen + 2/ .d tflash + =cell + count 1f and type space t@
   ?dup 0= until ;m
:m .stat
  0 if
    ." target: "      target.1      +order words cr cr
    ." target-only: " target.only.1 +order words cr cr
    ." assembler: "   assembler.1   +order words cr cr
    ." meta: "        meta.1        +order words cr cr
  then
  0 if twords then
  ." used> " there dup ." 0x" .h ." / " .d cr ;m
:m .end only forth also definitions decimal ;m
:m atlast tlast @ ;m
:m tvar   get-current >r meta.1 set-current create r> set-current there , t, does> @ ;m
:m label: get-current >r meta.1 set-current create r> set-current there ,    does> @ ;m
:m tdown =cell negate and ;m
:m tnfa =cell + ;m ( pwd -- nfa : move to name field address)
:m tcfa tnfa dup c@ $1F and + =cell + tdown ;m ( pwd -- cfa )
:m compile-only tlast @ tnfa t@ $20 or tlast @ tnfa t! ;m ( -- )
:m immediate    tlast @ tnfa t@ $40 or tlast @ tnfa t! ;m ( -- )
:m half 2/ ;m

\ ---------------------------------- Forth VM --------------------------------

:m Z 0 t, ;m \ Address 0 must contain 0
:m NADDR there 2/ 1+ t, ;m
:m HALT 0 t, 0 t, -1 t, ;m
:m JMP 2/ Z Z t, ;m ( c -- )
:m ADD swap 2/ t, Z NADDR Z 2/ t, NADDR Z Z NADDR ;m ( a b -- )
:m SUB swap 2/ t, 2/ t, NADDR ;m ( a b -- )
:m NOP Z Z NADDR ;m ( -- )
:m ZERO dup 2/ t, 2/ t, NADDR ;m
:m PUT 2/ t, -1 t, NADDR ;m ( a -- : load from address and output character )
:m GET 2/ -1 t, t, NADDR ;m ( a -- : get character from input and store at addr. )
assembler.1 +order also definitions
: begin talign there ; ( -- addr )
: again JMP ; ( addr -- )
: mark there 0 t, ;
: if 2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t, mark ;
: until
	2/ dup t, Z there 2/ 4 + dup t,
	Z Z 6 + t,
	Z Z NADDR
	Z t, 2/ t, ; ( var -- addr )
: +if   Z 2/ t, mark ;
: -if
	2/ t, Z there 2/ 4 + t,
	Z Z there 2/ 4 + t,
	Z Z mark ;  ( var -- addr )
: -until 2/ t, Z there 2/ 4 + t, Z Z there 2/ 4 + t, Z Z 2/ t, ;  ( addr var -- addr )
: then begin 2/ swap t! ;
: else mark swap then ;
: while if swap ;
: repeat JMP then ;
assembler.1 -order
meta.1 +order also definitions

:m MOV
	2/ >r r@ dup t, t, NADDR
        2/ t, Z  NADDR
	r> Z  t, NADDR
	Z Z NADDR ;m ( a b -- )

:m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m ( indr w -- : indirect load )
:m iJMP there 2/ E + 2* MOV NOP ;m ( indr -- )
:m iSTORE
	swap >r
	there 2/ 3 4 * 3 * 0 + + 2dup 2* MOV
	                     2dup 1 + 2* MOV
	                          7 + 2* MOV
	r> 0 MOV ;m ( addr w -- )

	0 t, 0 t,
label: entry
	-1 t,
	-1 tvar neg1     \ must contain -1
	1 tvar one       \ must contain  1
	0 tvar R0        \ temporary register
	0 tvar R1        \ temporary register

	0 tvar <cold>    \ entry point of virtual machine program, set later on
	0 tvar pwd       \ previous word pointer

\ TODO: Add hooks in for <key>, <emit>, <literal>, ...
	0 tvar ip        \ instruction pointer
	0 tvar w         \ working pointer
	0 tvar t         \ temporary register for Virtual Machine
	0 tvar tos       \ top of stack
	0 tvar h         \ dictionary pointer
	0 tvar {state}   \ compiler state
	0 tvar {hld}     \ hold space pointer
	0 tvar {base}    \ input/output radix, default = 16
	0 tvar {dpl}     \ number of places after fraction
	0 tvar {in}      \ position in query string
	0 tvar {handler} \ throw/catch handler
	0 tvar {last}    \ last defined word
	0 tvar #tib      \ terminal input buffer
	0 tvar primitive \ any address lower than this one must be a primitive

	=end                       dup tvar {sp0} tvar {sp} \ grows downwards
	=end =stksz 4 * -          dup tvar {rp0} tvar {rp} \ grows upwards
	=end =stksz 4 * - =buf - constant TERMBUF \ pad buffer space

	TERMBUF =buf + constant =tbufend

:m INC 2/ neg1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ one  2/ t, t, NADDR ;m ( b -- )
:m INV R0 ZERO dup R0 SUB dup R0 swap MOV DEC ;m ( b -- : invert NB. b - a = b + ~a + 1 )
:m ++sp {sp} DEC ;m
:m --sp {sp} INC ;m
:m --rp {rp} DEC ;m
:m ++rp {rp} INC ;m

\ TODO: Optimize VM
assembler.1 +order
label: start
	start 2/ entry t!
	{sp0} {sp} MOV
	{rp0} {rp} MOV
	<cold> ip MOV
	( fall-through )
label: vm
	ip w MOV
	ip INC
	t w iLOAD
	t w MOV
	primitive t SUB
	t -if w iJMP then
	++rp
	ip {rp} iSTORE
	w ip MOV
	vm JMP
assembler.1 -order

:m :ht ( "name" -- : forth only routine )
  get-current >r target.1 set-current create
  r> set-current CAFEBABE talign there ,
  does> @ 2/ t, ( really a call ) ;m

:m :t ( "name" -- : forth only routine )
  >in @ thead >in !
  get-current >r target.1 set-current create
  r> set-current CAFEBABE talign there ,
  does> @ 2/ t, ( really a call ) ;m

:m :to ( "name" -- : forth only, target only routine )
  >in @ thead >in !
  get-current >r target.only.1 set-current create r> set-current
  there ,
  CAFEBABE
  does> @ 2/ t, ;m

:m :a ( "name" -- : assembly only routine, no header )
  $CAFED00D
  target.1 +order also definitions
  create talign there ,
  assembler.1 +order
  does> @ 2/ t, ;m
:m (a);
   $CAFED00D <> if abort" unstructured" then assembler.1 -order ;m
:m ;a (a); vm JMP ;m

:a bye HALT ;a
:a 1- tos DEC ;a
:a 1+ tos INC ;a
:a invert tos INV ;a
:a [@] w tos iLOAD w tos MOV ;a
:a [!] tos w MOV tos {sp} iLOAD --sp tos w iSTORE tos {sp} iLOAD --sp ;a
:a emit tos PUT tos {sp} iLOAD --sp ;a
:a opKey?  ++sp tos {sp} iSTORE tos GET ;a
:a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a
:a opSwap tos w MOV tos {sp} iLOAD w {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a
:a opOver w {sp} iLOAD ++sp tos {sp} iSTORE w tos MOV ;a
:a opDrop tos {sp} iLOAD --sp ;a
:a - w {sp} iLOAD tos w SUB w tos MOV --sp ;a
:a + w {sp} iLOAD w tos ADD --sp ;a
:a opJump w ip iLOAD w ip MOV ;a
:a opJumpZ tos w MOV tos {sp} iLOAD --sp w if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a opJumpN tos w MOV tos {sp} iLOAD --sp w -if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a op0>  tos w MOV  0   tos MOV w +if neg1 tos MOV then ;a
:a op0<  tos w MOV  0   tos MOV w -if neg1 tos MOV then ;a
:a op0=  tos w MOV neg1 tos MOV w  if 0   tos MOV then ;a
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opMul w {sp} iLOAD t ZERO begin w while tos t ADD w DEC repeat t tos MOV --sp ;a
:a opExit ip {rp} iLOAD --rp ;a
:a rdrop --rp ;a
:a opNext
	w {rp} iLOAD
	w DEC
	w {rp} iSTORE
	w if t ip iLOAD t ip MOV vm JMP then
	ip INC
	--rp
	;a
:a op2* tos tos ADD ;a
:a sp@ ++sp tos {sp} iSTORE {sp} tos MOV tos DEC ;a
:a sp! tos {sp} MOV ;a
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a
:a rp!  tos {rp} MOV --sp tos {sp} iLOAD ;a
:a opExecute tos ip MOV --sp tos {sp} iLOAD ;a
\ :a opDivMod
\ 	w {sp} iLOAD
\ 	R0 ZERO
\ 	t ZERO
\ 	begin
\ 		#1 R1 MOV
\ 		tos w SUB
\ 		w -if then
\ 	while
\ 		R0 INC
\ 		tos w SUB
\ 	repeat
\ 	R0 {sp} iSTORE
\ 	w tos MOV ;a
:a lsb
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD
	tos if one tos MOV then ;a
\ :a op2/
\ 	w ZERO
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then w w ADD tos tos ADD
\ 	tos -if w INC then
\ 	w tos MOV ;a
\
there 2/ primitive t!

:m ;t CAFEBABE <> if abort" unstructured" then talign opExit target.only.1 -order ;m

:m lit         opPush t, ;m
:m [char] char opPush t, ;m
:m char   char opPush t, ;m
:m begin talign there ;m
:m until talign opJumpZ 2/ t, ;m
:m -until talign opJumpN 2/ t, ;m
:m again talign opJump  2/ t, ;m
:m if opJumpZ there 0 t, ;m
:m -if opJumpN there 0 t, ;m
:m mark opJump there 0 t, ;m
:m then there 2/ swap t! ;m
:m else mark swap then ;m
:m while if ;m
:m repeat swap again then ;m
:m aft drop mark begin swap ;m
:m next opNext talign 2/ t, ;m
:m for opToR begin ;m


\ TODO: Optimize by inline-ing assembly and renaming (ie. 1+ -> 1+).
:ht #0 0 lit ;t
:ht #1 1 lit ;t
:ht #-1 -1 lit ;t

:m dup opDup ;m
:m drop opDrop ;m
:m over opOver ;m
:m swap opSwap ;m
:m >r opToR ;m
:m r> opFromR ;m
:m 0> op0> ;m
:m 0= op0= ;m
:m 0< op0< ;m
:m exit opExit ;m

:to 1+ 1+ ;t
:to 1- 1- ;t
:to + + ;t
:to - - ;t
:to invert invert ;t
:to bye bye ;t
:to emit emit ;t
:to dup dup ;t
:to drop opDrop ;t
:to over opOver ;t
:to swap opSwap ;t
:to [@] [@] ;t
:to [!] [!] ;t
:to sp@ sp@ ;t
:to sp! sp! ;t
\ TODO: Change to immediate and compile-only, compiling in assembly op
:to >r opFromR opSwap opToR opToR ;t compile-only
:to r> opFromR opFromR opSwap opToR ;t compile-only
:to r@ opFromR r@ opSwap opToR ;t compile-only
:to rdrop opFromR rdrop opToR ;t compile-only
:to 0> op0> ;t
:to 0= op0= ;t
:to 0< op0< ;t
:to lsb lsb ;t

:t execute rdrop opExecute ;t
:t here h 2/ lit [@] ;t
:t base {base} lit ;t
:t dpl {dpl} lit ;t
:t hld {hld} lit ;t
:t bl 20 lit ;t
:t >in {in} lit ;t
:t hex  $10 lit {base} 2/ lit [!] ;t
:t source TERMBUF 2/ lit #tib 2/ lit [@] ;t
:t last {last} 2/ lit [@] ;t
:t state {state} lit op2* ;t
:t ] #-1 {state} 2/ lit [!] ;t
:t [ #0  {state} 2/ lit [!] ;t immediate
:t nip swap drop ;t
:t tuck swap over ;t
:t ?dup dup if dup then ;t
:t rot >r swap r> swap ;t
:t -rot rot rot ;t
:t 2drop drop drop ;t
:t 2dup  over over ;t
:t 0<= 0> invert ;t
:t 0<> 0= 0= ;t
:t = - 0= ;t
:t <> = 0= ;t
:t >= swap - 0<= ;t
:t < >= invert ;t
:t > swap < ;t
:t <= > invert ;t
:t msb dup 8000 lit = if drop #-1 exit then 0< ;t
:t 0>= 0< 0= ;t
:t 2* dup 8000 lit = if 7FFF lit - then op2* ;t
:t s>d dup 0< ;t
:t negate 1- invert ;t
:t abs s>d if negate then ;t
:t cell 2 lit ;t
:t cell+ cell + ;t
:t pick sp@ + [@] ;t
:t cr =cr lit emit =lf lit emit ;t
:t key? opKey? dup 0< if drop #0 exit then #-1 ;t
:t key begin key? until ;t
:t u< 2dup 0>= swap 0>= = opToR < r> = ;t
:t u> swap u< ;t
:t u>= u< invert ;t
:t u<= u> invert ;t
:t * 2dup u< if swap then opMul ;t
:t u/mod
  #0 opToR
  begin over over u>=
  while
    r> 1+ opToR
    tuck - swap
  repeat
  drop r> ;t
:t umod u/mod drop ;t
:t u/ u/mod nip ;t
\ TODO: Fast divide by two using repeated double and testing of top bit
\ to build up a new value
:t 2/ 2 lit u/ ;t
:t @ 2/ [@] ;t \ TODO: Avoid using this where possible, use [@]
:t ! 2/ [!] ;t \ TODO: Avoid using this where possible, use [!]
:t +! 2/ tuck [@] + swap [!] ;t
:t lshift begin ?dup while 1- swap 2* swap repeat ;t
:t rshift begin ?dup while 1- swap 2/ swap repeat ;t
:t logical if #1 else #0 then ;t
:t ? dup 30 lit + emit ;t \ TODO: delete when no longer needed
:t or
   $10 lit #0 >r >r
   begin
     r> dup 1- >r
   while
     r> r> 2* >r >r
     2dup msb swap msb + if
       r> r> 1+ >r >r
     then
     2* swap 2*
   repeat 2drop rdrop r> ;t
:t xor
   $10 lit #0 >r >r
   begin
     r> dup 1- >r
   while
     r> r> 2* >r >r
     2dup msb swap msb + #-1 = if
       r> r> 1+ >r >r
     then
     2* swap 2*
   repeat 2drop rdrop r> ;t
:t and
   $10 lit #0 >r >r
   begin
     r> dup 1- >r
   while
     r> r> 2* >r >r
     2dup msb swap msb + -2 lit = if
       r> r> 1+ >r >r
     then
     2* swap 2*
   repeat 2drop rdrop r> ;t
:t c@ dup @ swap lsb if 8 lit rshift  else $FF lit and then ;t
:t c!  swap $FF lit and dup 8 lit lshift or swap
   tuck dup @ swap lsb #0 = $FF lit xor
   >r over xor r> and xor swap ! ;t
:t max 2dup < if nip else drop then ;t
:t min 2dup > if nip else drop then ;t
:t count dup 1+ swap c@ ;t
:t aligned dup lsb + ;t
:t align here aligned h half lit [!] ;t
:t allot aligned h lit +! ;t
:t , align h half lit [!] cell allot ;t
:t +string #1 over min rot over + rot rot - ;t
:t type begin dup while swap count emit swap 1- repeat 2drop ;t
:t cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;t ( b1 b2 u -- )
:t do$ r> r> 2* dup count + aligned 2/ >r swap >r ;t ( -- a : )
:t ($) do$ ;t            ( -- a : do string NB. )
:t .$ do$ count type ;t  ( -- : print string, next cells contain string )
:m ." .$ $literal ;m
:m $" ($) $literal ;m
:t space bl emit ;t
\ :t cr .$ 2 tc, =cr tc, =lf tc, ;t
:t catch        ( xt -- exception# | 0 \ return addr on stack )
   sp@ >r                        ( xt )   \ save data stack pointer
   {handler} half lit [@] >r     ( xt )   \ and previous handler
   rp@ {handler} half lit [!]    ( xt )   \ set current handler
   opExecute                     ( )      \ execute returns if no throw
   r> {handler} half lit [!]     ( )      \ restore previous handler
   rdrop                         ( )      \ discard saved stack ptr
   #0 ;t                         ( 0 )    \ normal completion
:t throw ( ??? exception# -- ??? exception# )
    ?dup if                      ( exc# )     \ 0 throw is no-op
      {handler} half lit [@] rp! ( exc# )     \ restore prev return stack
      r> {handler} half lit [!]  ( exc# )     \ restore prev handler
      r> swap >r                 ( saved-sp ) \ exc# on return stack
      sp! drop r>                ( exc# )     \ restore stack
    then ;t
:t um+ 2dup u< 0= if swap then over + swap over swap u< logical ;t ( u u -- u carry )
:t um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) $F lit
  for dup um+ >r >r dup um+ r> + r>
    if >r over um+ r> + then
  next rot drop ;t
:t um/mod ( ud u -- ur uq : unsigned double cell width divide/modulo )
  ?dup 0= if -A lit throw then
  2dup u< \ TODO: Fix/Broken
  if negate $F lit
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> or
      if >r drop 1+ r> else drop then r>
    next
    drop swap exit
  then 2drop drop #-1 dup ;t
:t nfa cell+ ;t ( pwd -- nfa : move word pointer to name field )
:t cfa nfa dup c@ 1F lit and + cell+ cell negate and ;t ( pwd -- cfa )
\ TODO: Fix words, words with length 1 do not print correctly
:t words last begin dup nfa count 1F lit and space type @ ?dup 0= until cr ;t

:t cold
there half <cold> t!

	\ words
	3 lit    3 lit um* ? drop space ? drop cr

	9 lit 8 lit  9 lit um/mod ? drop space ? drop cr
	3 lit 3 lit um+ 30 lit + emit space 30 lit + emit cr
	5 lit FFFD lit um+ 30 lit + emit space 30 lit + emit cr
	FFFF lit FFFF lit um+ 30 lit + emit space 30 lit + emit cr
	4 lit -5 lit u< if char Y emit else char N emit then cr
	4 lit -5 lit  < if char Y emit else char N emit then cr
	char @ 1 lit xor emit cr
	char A 8 lit lshift FFFF lit xor 8 lit rshift FFFF lit xor emit cr
	char A 8 lit lshift FF00 lit and 8 lit rshift 00FF lit and emit cr
	char A 4 lit lshift FFFF lit xor 4 lit rshift FFFF lit xor emit cr
	char A FFFF lit xor FFFF lit xor emit cr
	char A 80FF lit xor FF lit and FF lit xor emit cr
	char A 7FFF lit xor 7FFF lit xor emit cr

	\ begin key? 0< while emit repeat drop
	#1 0> if
	  3 lit for char H emit char i emit char ! emit cr next
	  then
	bye ;t

\ ---------------------------------- Image Generation ------------------------

there h t!
atlast {last} t!
save-target subleq.dec
.stat
.end
bye
