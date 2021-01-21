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
2000 constant =end ( 8192  bytes, leaving 4096 for Dual Port Block RAM )
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
: if
	2/ dup t, Z there 2/ 4 + dup t,
	Z Z 6 + t,
	Z Z NADDR
	Z t, mark ; ( var -- addr )
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
\ :m iJMP  there 2/ 3 4 * 5 + + 2* MOV NOP ;m ( indr -- )
:m iJMP \ TODO: Fix this, there are problems with it, also why does it work?
	there 2/ E + 2* MOV NOP ;m ( indr -- )
:m iSTORE 
	swap >r
	there 2/ 3 4 * 3 * 0 + + 2dup 2* MOV
	                     2dup 1 + 2* MOV
	                          7 + 2* MOV
	r> 0 MOV ;m ( addr w -- )

	0 t, 0 t,
label: entry
	-1 t,
	-1 tvar #N1      \ must contain -1
	1 tvar #1        \ must contain  1
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
	=end =stksz 2* -           dup tvar {rp0} tvar {rp} \ grows upwards
	=end =stksz 2* - =buf - constant TERMBUF \ pad buffer space

	TERMBUF =buf + constant =tbufend

:m INC 2/ #N1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ #1  2/ t, t, NADDR ;m ( b -- )
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

\ TODO: lshift, rshift, and, or, xor, um+, c@, c!, 

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
:a opSub w {sp} iLOAD w tos SUB --sp ;a
:a opAdd w {sp} iLOAD w tos ADD --sp ;a
:a opJump w ip iLOAD w ip MOV ;a
:a opJumpZ tos w MOV --sp w if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a opJumpN tos w MOV --sp w -if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a opR@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a opNext
	w {rp} iLOAD
	w t MOV
	w if
		t DEC
		t {rp} iSTORE
		t ip iLOAD
		t ip MOV
		vm JMP
	then
	ip INC 
	--rp
	;a
:a op0<= tos w MOV #N1 tos MOV w +if 0 tos MOV then ;a
:a op0=  tos w MOV #N1 tos MOV w  if 0 tos MOV then ;a

\ untested
:a opFromR
	++sp
	tos {sp} iSTORE
	tos {rp} iLOAD
	--rp ;a
:a opRDrop --rp ;a
:a opSp@
	++sp
	tos {sp} iSTORE
	{sp} tos MOV
	tos DEC ;a
:a opSp! tos {sp} MOV ;a
:a opRp@ ++sp
	tos {sp} iSTORE
	{rp} tos MOV ;a
:a opRp!
	tos {rp} MOV
	--sp
	tos {sp} iLOAD ;a
:a opExecute
	tos ip MOV
	--sp
	tos {sp} iLOAD ;a
:a opExit
	ip {rp} iLOAD
	--rp ;a
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

:t 1+ opInc ;t
:t 1- opDec ;t
:t + opAdd ;t
:t - opSub ;t
:t invert opInvert ;t
:t bye opBye ;t
:t emit opEmit ;t
:t key? opKey? ;t
:t dup opDup ;t
:t drop opDrop ;t
:t over opOver ;t
:t swap opSwap ;t
:t [@] opLoad ;t
:t [!] opStore ;t
:t execute opRDrop opExecute ;t
:t sp@ opSp@ ;t
:t sp! opSp! ;t
:t 0<= op0<= ;t
:t 0= op0= ;t
:to >r opFromR opSwap opToR opToR ;t compile-only
:to r> opFromR opFromR opSwap opToR ;t compile-only
:to r@ opFromR opR@ opSwap opToR ;t compile-only

:t cr =cr lit emit =lf lit emit ;t
:t 0> 0<= invert ;t
:t = - 0= ;t
:t <> = invert ;t
:t 2dup over over ;t
:t 2drop drop drop ;t
:t 2* dup + ;t

:t cold 
there 2/ <cold> t!
	1 lit 0> if
		3 lit for aft char H emit char i emit char ! emit cr then next
	then
	bye 
;t

\ ---------------------------------- Image Generation ------------------------

save-target subleq.dec
.stat
.end
bye
