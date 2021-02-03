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

\ Tested
:a opBye HALT ;a
:a opDec tos DEC ;a
:a opInc tos INC ;a
:a opInvert tos INV ;a
:a opLoad w tos iLOAD w tos MOV ;a
:a opStore tos w MOV tos {sp} iLOAD --sp tos w iSTORE tos {sp} iLOAD --sp ;a
:a opEmit tos PUT tos {sp} iLOAD --sp ;a
:a opKey?  ++sp tos {sp} iSTORE tos GET ;a
:a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a
:a opSwap tos w MOV tos {sp} iLOAD w {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a
:a opOver w {sp} iLOAD ++sp tos {sp} iSTORE w tos MOV ;a
:a opDrop tos {sp} iLOAD --sp ;a
:a opSub w {sp} iLOAD tos w SUB w tos MOV --sp ;a
:a opAdd w {sp} iLOAD w tos ADD --sp ;a
:a opJump w ip iLOAD w ip MOV ;a
:a opJumpZ tos w MOV tos {sp} iLOAD --sp w if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a opJumpN tos w MOV tos {sp} iLOAD --sp w -if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a opR@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a op0>  tos w MOV  0   tos MOV w +if neg1 tos MOV then ;a
:a op0<  tos w MOV  0   tos MOV w -if neg1 tos MOV then ;a
:a op0=  tos w MOV neg1 tos MOV w  if 0   tos MOV then ;a
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opMul w {sp} iLOAD t ZERO begin w while tos t ADD w DEC repeat t tos MOV --sp ;a
\ untested
:a opExit ip {rp} iLOAD --rp ;a
:a opRDrop --rp ;a
:a opSp@ ++sp tos {sp} iSTORE {sp} tos MOV tos DEC ;a
:a opSp! tos {sp} MOV ;a
:a opRp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a
:a opRp!  tos {rp} MOV --sp tos {sp} iLOAD ;a
:a opExecute tos ip MOV --sp tos {sp} iLOAD ;a
:a opNext
	w {rp} iLOAD
	w t MOV
	w if t DEC t {rp} iSTORE t ip iLOAD t ip MOV vm JMP then
	ip INC 
	--rp
	;a
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
:a opLsb
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD 
	tos if one tos MOV then ;a
:a op2* tos tos ADD ;a
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
\ TODO: Optimize by inline-ing assembly and renaming (ie. opInc -> 1+).
:ht #0 0 lit ;t
:ht #1 1 lit ;t
:ht #-1 -1 lit ;t

:t 1+ opInc ;t
:t 1- opDec ;t
:t + opAdd ;t
:t - opSub ;t
:t invert opInvert ;t
:t bye opBye ;t
:t emit opEmit ;t
:t dup opDup ;t
:t drop opDrop ;t
:t over opOver ;t
:t swap opSwap ;t
:t [@] opLoad ;t
:t [!] opStore ;t
:t execute opRDrop opExecute ;t
:t sp@ opSp@ ;t
:t sp! opSp! ;t
:to >r opFromR opSwap opToR opToR ;t compile-only
:to r> opFromR opFromR opSwap opToR ;t compile-only
:to r@ opFromR opR@ opSwap opToR ;t compile-only

:t here h 2/ lit opLoad ;t
:t base {base} lit ;t  ( -- a : base variable controls input/output radix )
:t dpl {dpl} lit ;t    ( -- a : push address of 'dpl' onto the variable stack )
:t hld {hld} lit ;t    ( -- a : push address of 'hld' onto the variable stack )
:t bl 20 lit ;t        ( -- space : push a space onto the stack )
:t >in {in} lit ;t     ( -- b : push pointer to terminal input position )
:t hex  $10 lit {base} 2/ lit opStore ;t ( -- : switch to hexadecimal input/output radix )
:t source TERMBUF 2/ lit #tib 2/ lit opLoad ;t ( -- b u )
:t last {last} 2/ lit opLoad ;t ( -- : last defined word )
:t state {state} lit op2* ;t      ( -- a : compilation state variable )
:t ] #-1 {state} 2/ lit opStore ;t           ( -- : turn compile mode on )
:t [ #0  {state} 2/ lit opStore ;t immediate ( -- : turn compile mode off )
:t nip opSwap opDrop ;t          ( u1 u2 -- u2 : remove next stack value )
:t tuck opSwap opOver ;t         ( u1 u2 -- u2 u1 u2 : save top stack value )
:t ?dup opDup if opDup then ;t   ( u -- u u | 0 : duplicate if not zero )
:t rot opTor opSwap opFromR opSwap ;t  ( u1 u2 u3 -- u2 u3 u1 : rotate three numbers )
:t -rot rot rot ;t
:t 2drop opDrop opDrop ;t    ( u u -- : drop two numbers )
:t 2dup  opOver opOver ;t    ( u1 u2 -- u1 u2 u1 u2 : duplicate set of values )
\ TODO: Implement more test primitives for efficiencies sake
:t 0> op0> ;t                ( n -- f )
:t 0<= op0> opInvert ;t      ( n -- f )
:t 0= op0= ;t                ( u -- f )
:t 0<> op0= op0= ;t
:t = opSub op0= ;t               ( u u -- f )
:t <> = op0= ;t              ( u u -- f )
:t >= opSwap opSub 0<= ;t \ TODO: cleanup
:t < >= opInvert ;t
:t > opSwap < ;t
:t <= > opInvert ;t
\ :t 0< op0< ;t
:t 0< opDup 8000 lit = if opDrop #-1 opExit then op0< ;t                ( n -- f )
:t 0>= 0< 0= ;t
:t 2* dup 8000 lit = if 7FFF lit - then dup + ;t
:t s>d opDup 0< ;t           ( n -- d )
:t negate 1- opInvert ;t     ( u -- u )
:t abs s>d if negate then ;t ( n -- u )
:t cell 2 lit ;t             ( -- u : size of memory cell )
:t cell+ cell + ;t           ( a -- a : increment address to next cell )
:t pick opSp@ + opLoad ;t    ( ??? u -- ??? u u : )
:t cr =cr lit emit =lf lit emit ;t ( -- )
:t key? opKey? dup 0< if drop #0 opExit then #-1 ;t
:t key begin key? until ;t
:t u< 2dup 0>= opSwap 0>= = opToR < opFromR = ;t ( u u -- f : )
:t u> opSwap u< ;t
:t u>= u< opInvert ;t
:t u<= u> opInvert ;t
\ TODO: Optimize / and *, rewrite in assembly along with other words
:t * 2dup u< if opSwap then opMul ;t
:t u/mod 
  #0 opToR
  begin opOver opOver u>=
  while 
    opFromR opInc opToR 
    tuck opSub opSwap
  repeat 
  opDrop opFromR ;t
:t umod u/mod drop ;t
:t u/ u/mod nip ;t
\ TODO: Fast divide by two using repeated double and testing of top bit
\ to build up a new value
:t 2/ 2 lit u/ ;t
:t @ 2/ opLoad ;t \ TODO: Avoid using this where possible, use [@]
:t ! 2/ opStore ;t \ TODO: Avoid using this where possible, use [!]
:t lsb opLsb ;t
:t +! tuck @ + swap ! ;t     ( n a -- : increment value at address by 'n' )
:t lshift begin ?dup while 1- opSwap 2* opSwap repeat ;t
:t rshift begin ?dup while 1- opSwap 2/ opSwap repeat ;t
:t logical if #1 else #0 then ;t
:t ? dup 30 lit + emit ;t \ TODO: delete when no longer needed
:t or
   $10 lit #0 opToR opToR
   begin
     opFromR opDup opDec opToR
   while
     opFromR opFromR 2* opToR opToR
     2dup 0< opSwap 0< + if
       opFromR opFromR opInc opToR opToR
     then
     2* swap 2*
   repeat 2drop opRDrop opFromR ;t
:t xor
   $10 lit #0 opToR opToR
   begin
     opFromR opDup opDec opToR
   while
     opFromR opFromR 2* opToR opToR
     2dup 0< opSwap 0< opAdd #-1 = if
     \ 2dup 0< opSwap 0< opAdd #-1 <> if
       opFromR opFromR opInc opToR opToR
     then
     2* swap 2*
   repeat 2drop opRDrop opFromR ;t
:t and
   $10 lit #0 opToR opToR
   begin
     opFromR opDup opDec opToR
   while
     opFromR opFromR 2* opToR opToR
     2dup 0< opSwap 0< + -2 lit = if
       opFromR opFromR opInc opToR opToR
     then
     2* opSwap 2*
   repeat 2drop opRDrop opFromR ;t

\ TODO: Rewrite with efficiency in mind for this platform, use
\ [@] and [!], multiply directly by 256, and divide by 256
:t c@ dup @ swap 1 lit and if 8 lit rshift ( 100 lit u/ ) else $FF lit and then ;t
:t c!  swap $FF lit and dup 8 lit lshift ( 100 lit * ) or swap
   tuck dup @ swap 1 lit and #0 = $FF lit xor
   opToR over xor opFromR and xor swap ! ;t
:t max 2dup < if nip else drop then ;t  ( n n -- n : maximum of two numbers )
:t min 2dup > if nip else drop then ;t  ( n n -- n : minimum of two numbers )
:t count dup 1+ swap c@ ;t ( b -- b c : advance string, get next char )
:t aligned dup #1 and + ;t       ( b -- u : align a pointer )
:t align here aligned h lit ! ;t ( -- : align dictionary pointer )
:t allot aligned h lit +! ;t      ( u -- : allocate space in dictionary )
:t , align here ! cell allot ;t   ( u -- : write a value into the dictionary )
:t +string #1 over min rot over + rot rot - ;t ( b u -- b u : increment str )
:t type begin dup while swap count emit swap 1- repeat 2drop ;t ( b u -- )
:t cmove for aft opToR dup c@ opR@ c! 1+ opFromR 1+ then next 2drop ;t ( b1 b2 u -- )
:t do$ opFromR opFromR 2* dup count + aligned 2/ opToR swap opToR ;t ( -- a : )
:t ($) do$ ;t            ( -- a : do string NB. )
:t .$ do$ count type ;t  ( -- : print string, next cells contain string )
:m ." .$ $literal ;m
:m $" ($) $literal ;m
:t space bl emit ;t               ( -- : print space )
\ :t cr .$ 2 tc, =cr tc, =lf tc, ;t ( -- : print new line )
:t catch ( xt -- exception# | 0 \ return addr on stack )
   opSp@ opToR              ( xt )   \ save data stack pointer
   {handler} lit @ opToR    ( xt )   \ and previous handler
   opRp@ {handler} lit !    ( xt )   \ set current handler
   opExecute                ( )      \ execute returns if no throw
   opFromR {handler} lit !  ( )      \ restore previous handler
   opRdrop                  ( )      \ discard saved stack ptr
   #0 ;t                    ( 0 )    \ normal completion
:t throw ( ??? exception# -- ??? exception# )
    ?dup if                   ( exc# )     \ 0 throw is no-op
      {handler} lit @ opRp!   ( exc# )     \ restore prev return stack
      opFromR {handler} lit ! ( exc# )     \ restore prev handler
      opFromR opSwap opToR    ( saved-sp ) \ exc# on return stack
      opSp! opDrop opFromR    ( exc# )     \ restore stack
    then ;t
:t um+ 2dup u< op0= if opSwap then opOver opAdd opSwap opOver opSwap u< logical ;t ( u u -- u carry )
:t um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) F lit
  for opDup um+ opToR opToR opDup um+ opFromR opAdd opFromR
    if opToR opOver um+ opFromR + then
  next rot drop ;t
:t um/mod ( ud u -- ur uq : unsigned double cell width divide/modulo )
  ?dup 0= if -A lit throw then
  2dup u<
  if negate $F lit
    for opToR dup um+ opToR opToR dup um+ opFromR + dup
      opFromR opR@ opSwap opToR um+ opFromR or
      if opToR drop 1+ opFromR else drop then opFromR
    next
    drop opSwap opExit
  then 2drop drop #-1 dup ;t
:t nfa cell+ ;t ( pwd -- nfa : move word pointer to name field )
:t cfa nfa dup c@ $1F lit and + cell+ cell negate and ;t ( pwd -- cfa )
:t words last begin dup nfa count 1f lit and ( space ) type cr @ ?dup 0= until ;t

:t cold 
there half <cold> t!

	\ words

	\ 4 lit    4 lit um* ? drop space ? drop cr
	3 lit    3 lit um* emit emit cr


	4 lit 0 lit  2 lit um/mod ? drop space ? drop cr
	\ char A 8 lit lshift FFFF lit xor 8 lit rshift FFFF lit xor emit cr
	\ char A 8 lit lshift FF00 lit and 8 lit rshift 00FF lit and emit cr

	char @ 1 lit or emit cr
	3 lit 3 lit um+ 30 lit + opEmit space 30 lit + emit cr
	5 lit FFFD lit um+ 30 lit + opEmit space 30 lit + emit cr
	FFFF lit FFFF lit um+ 30 lit + opEmit space 30 lit + emit cr
	4 lit -5 lit u< if char Y emit cr then
	char A 4 lit lshift FFFF lit xor 4 lit rshift FFFF lit xor emit cr
	char A FFFF lit xor FFFF lit xor emit cr
	char A 80FF lit xor FF lit and FF lit xor emit cr
	char A 7FFF lit xor 7FFF lit xor emit cr

	\ begin key? 0< while emit repeat drop
	#1 0> if
	  3 lit for aft char H emit char i emit char ! emit cr then next
	  then
	bye ;t

\ ---------------------------------- Image Generation ------------------------

there h t!
atlast {last} t!
save-target subleq.dec
.stat
.end
bye
