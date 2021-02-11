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
8000 constant size ( 16384 bytes, 8192 cells )
8000 constant =end
  40 constant =stksz
 100 constant =buf
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
:m $literal talign [char] " word count dup tc, 0 ?do count tc, loop drop talign ;m
:m tallot tdp +! ;m
:m thead
  talign
  there tlast @ t, tlast !
  parse-word talign dup tc, 0 ?do count tc, loop drop talign ;m
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
:m t' ' >body @ ;m

\ ---------------------------------- Forth VM --------------------------------

:m Z 0 t, ;m \ Address 0 must contain 0
:m NADDR there 2/ 1+ t, ;m
:m HALT 0 t, 0 t, -1 t, ;m
:m JMP 2/ Z Z t, ;m
:m ADD swap 2/ t, Z NADDR Z 2/ t, NADDR Z Z NADDR ;m
:m SUB swap 2/ t, 2/ t, NADDR ;m
:m NOP Z Z NADDR ;m
:m ZERO dup 2/ t, 2/ t, NADDR ;m
:m PUT 2/ t, -1 t, NADDR ;m
:m GET 2/ -1 t, t, NADDR ;m
:m MOV 2/ >r r@ dup t, t, NADDR 2/ t, Z  NADDR r> Z  t, NADDR Z Z NADDR ;m

assembler.1 +order also definitions
: begin talign there ;
: again JMP ;
: mark there 0 t, ;
: if 2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t, mark ;
: until
	2/ dup t, Z there 2/ 4 + dup t,
	Z Z 6 + t,
	Z Z NADDR
	Z t, 2/ t, ;
: +if   Z 2/ t, mark ;
: -if
	2/ t, Z there 2/ 4 + t,
	Z Z there 2/ 4 + t,
	Z Z mark ; 
: then begin 2/ swap t! ;
: else mark swap then ;
: while if swap ;
: repeat JMP then ;
assembler.1 -order
meta.1 +order also definitions

:m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m
:m iJMP there 2/ E + 2* MOV NOP ;m
:m iSTORE swap >r there 2/ 24 + 2dup 2* MOV 2dup 1+ 2* MOV 7 + 2* MOV r> 0 MOV ;m ( addr w -- )

	0 t, 0 t,
label: entry
	-1 t,
	-1 tvar neg1     \ must contain -1
	1 tvar one       \ must contain  1
	0 tvar R0        \ temporary register
	0 tvar R1        \ TODO: DELETE (temporary register)
	0 tvar {cold}    \ entry point of virtual machine program, set later on
	\ 0 tvar {key}     \ execution vector for key?
	\ 0 tvar {emit}    \ execution vector for emit
	\ 0 tvar {literal} \ execution vector for literal
	\ 0 tvar {interp}  \ execution vector for interpret
	0 tvar {ok}      \ execution vector for .ok
	0 tvar ip        \ instruction pointer
	0 tvar w         \ working pointer
	0 tvar x         \ working pointer
\	0 tvar y         \ working pointer
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

assembler.1 +order
label: start
	start 2/ entry t!
	{sp0} {sp} MOV
	{rp0} {rp} MOV
	{cold} ip MOV
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

:m :ht ( "name" -- : forth routine, no header )
  get-current >r target.1 set-current create
  r> set-current CAFEBABE talign there ,
  does> @ 2/ t, ( really a call ) ;m

:m :t ( "name" -- : forth routine )
  >in @ thead >in !
  get-current >r target.1 set-current create
  r> set-current CAFEBABE talign there ,
  does> @ 2/ t, ( really a call ) ;m

:m :to ( "name" -- : forth, target only routine )
  >in @ thead >in !
  get-current >r target.only.1 set-current create r> set-current
  there ,
  CAFEBABE
  does> @ 2/ t, ;m

:m :a ( "name" -- : assembly routine, no header )
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
:a opJump w ip iLOAD w ip MOV ;a
:a opJumpZ tos w MOV tos {sp} iLOAD --sp w if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a op0>  tos w MOV  0   tos MOV w +if neg1 tos MOV then ;a
:a op0=  tos w MOV neg1 tos MOV w  if 0   tos MOV then ;a
:a op<   w {sp} iLOAD --sp tos w SUB 0 tos MOV w -if neg1 tos MOV then ;a
:a op>   w {sp} iLOAD --sp tos w SUB 0 tos MOV w +if neg1 tos MOV then ;a
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opMul w {sp} iLOAD t ZERO begin w while tos t ADD w DEC repeat t tos MOV --sp ;a

\ :a op0<  
\   tos w   MOV  
\   0   tos MOV 
\   w -if neg1 tos MOV vm JMP then 
\   neg1 tos MOV
\   highb w SUB w if 0 tos MOV then
\ ;a
\ 

\ \ :t u< 2dup 0>= swap 0>= = >r < r> = ;t
\ :a opu<
\ 	w {sp} iLOAD
\ 	y ZERO
\ 	--sp
\ 	tos +if y INC then tos if y INC then
\ 	w   +if y INC then w   if y INC then
\ 	w tos SUB tos -if then
\ ;a
\ 

:a opExit ip {rp} iLOAD --rp ;a
:a op2* tos tos ADD ;a
:a opExecute tos ip MOV tos {sp} iLOAD --sp ;a
:a - w {sp} iLOAD tos w SUB w tos MOV --sp ;a
:a + w {sp} iLOAD w tos ADD --sp ;a
:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a
:a rdrop --rp ;a
:a sp@ ++sp tos {sp} iSTORE {sp} tos MOV tos INC ;a
:a sp! tos {sp} MOV ;a
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a
:a rp!  tos {rp} MOV --sp tos {sp} iLOAD ;a
:a r1+ w {rp} iLOAD w INC w {rp} iSTORE ;a
:a opNext
	w {rp} iLOAD
	w if 
		w DEC w {rp} iSTORE t ip iLOAD t ip MOV vm JMP 
	then
	ip INC
	--rp
	;a
:a opDivMod \ NB. a "op2/" instruction would improve speed even more
	w {sp} iLOAD
	t ZERO
	begin
		one x MOV
		w -if 0 x MOV then \ TODO: Replace with u>=
		x
	while
		t INC
		tos w SUB	
	repeat
	tos w ADD
	t DEC
	t tos MOV
	w {sp} iSTORE ;a
:a op0<  tos w MOV  0   tos MOV w -if neg1 tos MOV then ;a
:a lsb
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD tos tos ADD tos tos ADD
	tos tos ADD tos tos ADD
	tos if one tos MOV then ;a
:a op0<  FFFC t, tos 2/ t, NADDR ;a
:a opTmp2/  FFFE t, tos 2/ t, NADDR ;a
:a opTmpMsb FFFC t, tos 2/ t, NADDR ;a
:a op2*     FFFD t, tos 2/ t, NADDR ;a
:a lsb      FFFB t, tos 2/ t, NADDR ;a

there 2/ primitive t!

:m ;t CAFEBABE <> if abort" unstructured" then talign opExit target.only.1 -order ;m

:m lit         opPush t, ;m
:m [char] char opPush t, ;m
:m char   char opPush t, ;m
:m begin talign there ;m
:m until talign opJumpZ 2/ t, ;m
:m again talign opJump  2/ t, ;m
:m if opJumpZ there 0 t, ;m
:m mark opJump there 0 t, ;m
:m then there 2/ swap t! ;m
:m else mark swap then ;m
:m while if ;m
:m repeat swap again then ;m
:m aft drop mark begin swap ;m
:m next opNext talign 2/ t, ;m
:m for opToR begin ;m
:m =push   [ t' opPush  ] literal ;m
:m =jump   [ t' opJump  ] literal ;m
:m =jumpz  [ t' opJumpZ ] literal ;m
:m =unnest [ t' opExit  ] literal ;m
:m =>r     [ t' opToR   ] literal ;m
:m =next   [ t' opNext  ] literal ;m

:m dup opDup ;m
:m drop opDrop ;m
:m over opOver ;m
:m swap opSwap ;m
:m >r opToR ;m
:m r> opFromR ;m
:m 0> op0> ;m
:m 0= op0= ;m
:m 0< op0< ;m
:m < op< ;m
:m > op> ;m
:m exit opExit ;m

:ht #0 0 lit ;t
:ht #1 1 lit ;t
:ht #-1 FFFF lit ;t
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
:to 0> op0> ;t
:to 0= op0= ;t
:to 0< op0< ;t
:to lsb lsb ;t
:to < op< ;t
:to > op> ;t

:t cr =cr lit emit =lf lit emit ;t
:t here h half lit [@] ;t
:t base {base} lit ;t
:t dpl {dpl} lit ;t
:t hld {hld} lit ;t
:t bl 20 lit ;t
:t >in {in} lit ;t
:t hex  $10 lit {base} half lit [!] ;t
:t source TERMBUF lit #tib half lit [@] ;t
:t last {last} half lit [@] ;t
:t state {state} lit ;t
:t ] #-1 {state} half lit [!] ;t
:t [ #0  {state} half lit [!] ;t immediate
:t nip swap drop ;t
:t tuck swap over ;t
:t ?dup dup if dup then ;t
:t rot >r swap r> swap ;t
:t -rot rot rot ;t
:t 2drop drop drop ;t
:t 2dup  over over ;t
:t 0<= 0> 0= ;t
:t 0<> 0= 0= ;t
:t = - 0= ;t
:t <> = 0= ;t
:t >= swap - 0<= ;t
:t <= > invert ;t
\ :t msb dup 8000 lit - 0= if drop #-1 exit then 0< ;t
:t msb opTmpMsb ;t
:t 0>= 0< 0= ;t
:t negate invert 1- ;t
:t s>d dup 0< ;t
:t abs s>d if negate then ;t
:t 2* op2* ;t
:t cell 2 lit ;t
:t cell+ cell + ;t
:t key? opKey? dup 0< if drop #0 exit then #-1 ;t
:t key begin key? until ;t
:t u< 2dup 0>= swap 0>= <> >r < r> <> ;t
:t u> swap u< ;t
:t u>= u< 0= ;t
:t u<= u> 0= ;t
:t * 2dup u< if swap then opMul ;t
:t u/mod \ opDivMod ;t
  #0 >r begin over over u>= while r1+ tuck - swap repeat drop r> ;t
:t umod u/mod drop ;t
:t u/ u/mod nip ;t
:t 2/ opTmp2/ ;t
\ :t 2/ 2 lit u/ ;t
\ :t execute opExecute ;t
:t execute 2/ >r ;t
:t @ 2/ [@] ;t
:t ! 2/ [!] ;t
:t pick sp@ + [@] ;t 
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
       r> r1+ >r
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
       r> r1+ >r
     then
     2* swap 2*
   repeat 2drop rdrop r> ;t
:t and
   $10  lit #0 >r >r
   begin
     r> dup 1- >r
   while
     r> r> 2* >r >r
     2dup msb swap msb + -2 lit = if
       r> r1+ >r
     then
     2* swap 2*
   repeat 2drop rdrop r> ;t
:t c@ dup @ swap lsb if 8 lit rshift else $FF lit and then ;t
:t c!  swap $FF lit and dup 8 lit lshift or swap
   tuck dup @ swap lsb 0= $FF lit xor
   >r over xor r> and xor swap ! ;t
:t max 2dup < if nip else drop then ;t
:t min 2dup > if nip else drop then ;t
:t count dup 1+ swap c@ ;t
:t aligned dup lsb + ;t
:t align here aligned h half lit [!] ;t
:t allot aligned h lit +! ;t
:t +string #1 over min rot over + rot rot - ;t
:t type begin dup while swap count emit swap 1- repeat 2drop ;t
:t cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;t ( b1 b2 u -- )
:t do$ r> r> 2* dup count + aligned 2/ >r swap >r ;t ( -- a : )
:t ($) do$ ;t            ( -- a : do string NB. )
:t .$ do$ count type ;t  ( -- : print string, next cells contain string )
:m ." .$ $literal talign ;m
:m $" ($) $literal talign ;m
:t space bl emit ;t
\ :t cr .$ 2 tc, =cr tc, =lf tc, 0 tc, ;t

\ ==========================================================================
\ ===                                                                    ===
\ ===                       UNTESTED / NOT WORKING                       ===
\ ===                                                                    ===
\ ==========================================================================
\ TODO: Fix words, words with length 1 do not print correctly
\ TODO: um*, um/mod not working correctly
\ TODO: >r, r>, r@, rdrop will need testing in a run time compiled word
\ TODO: Fix "8000 lshift", other operators, c@, ...
\ TODO: Test numeric words, interpreter loop
\ TODO: Implement system hooks for common I/O, interpreter loop, literal
\ TODO: Remove usage of logical operators where possible

:t catch        ( xt -- exception# | 0 \ return addr on stack )
   sp@ >r                        ( xt )   \ save data stack pointer
   {handler} half lit [@] >r     ( xt )   \ and previous handler
   rp@ {handler} half lit [!]    ( xt )   \ set current handler
   execute                       ( )      \ execute returns if no throw
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
  2dup u<
  if negate $F lit
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> ( 0<> swap 0<> + ) or
      if >r drop 1+ r> else drop then r>
    next
    drop swap exit
  then 2drop drop #-1 dup ;t
:t depth {sp0} half lit [@] sp@ - 1- ;t ( -- u : variable stack depth )
:t ktap ( bot eot cur c -- bot eot cur )
  dup dup =cr lit <> >r  =lf lit <> r> and if \ Not End of Line?
    dup =bksp lit <> >r =del lit <> r> and if \ Not Delete Char?
      bl ( tap -> ) dup emit over c! 1+ ( bot eot cur c -- bot eot cur )
      exit
    then
    >r over r@ < dup if
      =bksp lit dup emit space emit
    then
    r> +
    exit
  then drop nip dup ;t
:t accept ( b u -- b u : read in a line of user input )
  over + over
  begin
    2dup <>
  while
    key dup bl - $5F lit u< if ( tap -> ) dup emit over c! 1+ else ktap then
  repeat drop over - ;t
:t query TERMBUF lit =buf lit accept #tib lit ! drop #0 >in ! ;t ( -- : get line)
:t ?depth drop ( depth > if -4 lit throw then ) ;t ( u -- : check stack depth ) \ TODO Fix
:t -trailing ( b u -- b u : remove trailing spaces )
  for
    aft bl over r@ + c@ <
      if r> 1+ exit then
    then
  next #0 ;t
\ TODO: Fix parse, pick, execute, depth
:t look ( b u c xt -- b u : skip until *xt* test succeeds )
  swap >r rot rot
  begin
    dup
  while
    over c@ r@ - r@ bl = 4 lit pick execute 
    if rdrop rot drop exit then
    +string
  repeat rdrop rot drop ;t
:t no-match if 0> exit then 0= 0= ;t ( c1 c2 -- t )
:t match no-match invert ;t          ( c1 c2 -- t )
:t parse ( c -- b u ; <string> )
    >r source drop >in @ + #tib lit @ >in @ - r@
    >r over r> swap >r >r
    r@ t' no-match lit look 2dup
    r> t' match    lit look swap r> - >r - r> 1+  ( b u c -- b u delta )
    >in +!
    r> bl = if -trailing then #0 max ;t
:t spaces begin dup 0> while space 1- repeat drop ;t ( +n -- )
:t hold #-1 hld +! hld @ c! ;t ( c -- : save a character in hold space )
:t #> 2drop hld @ =tbufend lit over - ;t  ( u -- b u )
:t #  ( d -- d : add next character in number to hold space )
   2 lit ?depth
   #0 base @
   ( extract ->) dup >r um/mod r> swap >r um/mod r> rot ( ud ud -- ud u )
   ( digit -> ) 9 lit over < 7 lit and + [char] 0 + ( u -- c )
   hold ;t
:t #s begin # 2dup ( d0= -> ) or 0= until ;t       ( d -- 0 )
:t <# =tbufend lit hld ! ;t                        ( -- )
:t sign 0< if [char] - hold then ;t                ( n -- )
:t u.r >r #0 <# #s #>  r> over - spaces type ;t    ( u +n -- : print u right justified by +n )
:t u.  #0 <# #s #> space type ;t                   ( u -- : print unsigned number )
:t . dup >r abs #0 <# #s r> sign #> space type ;t  ( n -- print number )
:t >number ( ud b u -- ud b u : convert string to number )
  begin
    2dup >r >r drop c@ base @        ( get next character )
    ( digit? -> ) >r [char] 0 - 9 lit over <
    if 7 lit - dup $A lit < or then dup r> u< ( c base -- u f )
    0= if                            ( d char )
      drop                           ( d char -- d )
      r> r>                          ( restore string )
      exit                           ( ..exit )
    then                             ( d char )
    swap base @ um* drop rot base @ um* ( d+ -> ) >r swap >r um+ r> + r> + ( accumulate digit )
    r> r>                            ( restore string )
    +string dup 0=                   ( advance string and test for end )
  until ;t
:t number? ( a u -- d -1 | a u 0 : string to a number [easier to use] )
  #-1 dpl !
  base @ >r
  over c@ [char] - = dup >r if     +string then
  over c@ [char] $ =        if hex +string then
  >r >r #0 dup r> r>
  begin
    >number dup
  while over c@ [char] . xor
    if rot drop rot r> 2drop #0 r> base ! exit then
    1- dpl ! 1+ dpl @
  repeat 
  2drop r> if 
    ( dnegate -> ) invert >r invert #1 um+ r> + 
  then r> base ! #-1 ;t
:t compare ( a1 u1 a2 u2 -- n : string equality )
  rot
  over - ?dup if >r 2drop r> nip exit then
  for ( a1 a2 )
    aft
      count rot count rot - ?dup
      if rdrop nip nip exit then
    then
  next 2drop #0 ;t
:to .s depth  for aft r@ pick . then next ;t ( -- : print variable stack )
:t nfa cell+ ;t ( pwd -- nfa : move word pointer to name field )
:t cfa nfa dup c@ 1F lit and + cell+ cell negate and ;t ( pwd -- cfa )
:t , align h half lit [!] cell allot ;t
:t (find) ( a wid -- PWD PWD 1|PWD PWD -1|0 a 0: find word in WID )
  swap >r dup
  begin
    dup
  while
    dup nfa count $9F lit ( $1F:word-length + $80:hidden ) and r@ count compare 0=
    if ( found! )
      rdrop
      dup ( immediate? -> ) nfa $40 lit swap @ and 0= 0=
      #1 or negate exit
    then
    nip dup @
  repeat
  2drop #0 r> #0 ;t
:t find last (find) rot drop ;t  ( "name" -- b )
:t literal state @ if =push lit , , then ;t immediate ( u -- )
:t compile, 2/ align , ;t  ( xt -- )
:t ?found if exit then space count type [char] ? emit cr -D lit throw ;t ( u f -- )
:t interpret                                          ( b -- )
  find ?dup if
    state @
    if
      0> if cfa execute exit then \ <- immediate word are executed
      cfa compile, exit           \ <- compiling word are...compiled.
    then
    drop
    dup nfa c@ 20 lit and if -E lit throw then ( <- ?compile )
    cfa execute exit  \ <- if its not, execute it, then exit *interpreter*
  then
  \ not a word
  dup >r count number? if rdrop \ it is a number!
    dpl @ 0< if \ <- dpl will be -1 if it is a single cell number
       drop     \ drop high cell from 'number?' for single cell output
    else        \ <- dpl is not -1, it is a double cell number
       state @ if swap then
       postpone literal \ literal is executed twice if it's a double
    then
    postpone literal exit
  then
  r> #0 ?found \ Could vector ?found here, to handle arbitrary words
  ;t
:t word parse here dup >r 2dup ! 1+ swap cmove r> ;t ( c -- b )
:t words last begin dup nfa count 1F lit and space type @ ?dup 0= until cr ;t
:to see bl word find ?found
    cr begin dup @ =unnest lit <> while dup @ u. cell+ repeat @ u. ;t
:to : align here last , {last} half lit [!] ( "name" -- : define a new word )
    bl word
    dup c@ 0= if -A lit throw then
    count + h half lit [!] align
    ] BABE lit ;t
:to ; postpone [ BABE lit <> if -16 lit throw then  =unnest lit , ;t immediate compile-only
:to begin align here ;t immediate compile-only
:to until =jumpz lit , 2/ , ;t immediate compile-only
:to again =jump  lit , 2/ , ;t immediate compile-only
:to if =jumpz lit , here #0 , ;t immediate compile-only
:to then here 2/ swap ! ;t immediate compile-only
:to for =>r lit , here ;t immediate compile-only
:to next =next lit , 2/ , ;t immediate compile-only
:to ' bl word find ?found cfa literal ;t immediate
:t compile r> dup 2* @ , 1+ >r ;t compile-only 
:to >r compile opToR ;t immediate compile-only
:to r> compile opFromR ;t immediate compile-only
:to r@ compile r@ ;t immediate compile-only \ TODO: check it compiles right addr
:to rdrop compile rdrop ;t immediate compile-only \ TODO: check it compiles right addr
:to exit compile opExit ;t immediate compile-only
:to ." compile .$  [char] " word count + h half lit [!] align ;t immediate compile-only
:to $" compile ($) [char] " word count + h half lit [!] align ;t immediate compile-only  \ "
:to ( [char] ) parse 2drop ;t immediate 
:to \ source drop @ {in} half lit [!] ;t immediate  
:to immediate last nfa @ $40 lit or last nfa ! ;t 
:to see bl word find ?found
    cr begin dup @ =unnest lit <> while dup @ u. cell+ repeat @ u. ;t
:to dump begin over c@ u. +string ?dup 0= until drop ;t
:t <ok> ."  ok" cr ;t
:t eval begin bl word dup c@ while interpret #1 ?depth repeat drop {ok} lit @ execute ;t ( "word" -- )
:t ini 
    hex postpone [ 
    #0 {in} half lit [!] 
    #-1 {dpl} half lit [!]
    t' <ok> lit {ok} half lit [!] ;t ( -- )
cr
:t quit ( -- : interpreter loop [and more, does more than most QUITs] )
   there half {cold} t! \ program entry point set here
   ." eForth v0.1" cr \ TODO Print out bytes used/left
   ini 
   begin
     query eval
     \ query t' eval lit catch
     ( ?error -> ) ?dup if
\       space . [char] ? emit cr ini
     then again ;t

0005 tvar xx \ TODO: Delete after testing
0706 tvar yy \ TODO: Delete after testing
\ TODO: Create a test bench for all bitwise operators
:t cold
\   there half {cold} t! \ program entry point set here
	." TESTING/NOT WORKING" cr
	3 lit 2 lit xor  ? cr
	3 lit 1 lit xor  ? cr
	3 lit 1 lit and  ? cr
	3 lit 2 lit and  ? cr
	2 lit 1 lit or   ? cr
	0 lit 1 lit or   ? cr
	3 lit 1 lit xor  ? cr
	cr


	\ TODO: fix so it should all 0, 1, 2
	8000 lit 8 lit lshift 8 lit rshift ? cr
	   1 lit 8 lit lshift 8 lit rshift ? cr
	   2 lit 8 lit lshift 8 lit rshift ? cr
	cr

	xx lit @     ? cr
	xx lit c@    ? cr
	yy lit @     ? cr
	yy lit c@    ? cr
	yy lit 1+ c@ ? cr
	cr

	\ ini words
	\ char A FFFF lit xor FFFF lit xor emit cr

	2 lit    1 lit um* ? drop space ? drop cr
	0 lit 1 lit  1 lit um/mod ? drop space ? drop cr
	3 lit 3 lit um+ 30 lit + emit space 30 lit + emit cr
	5 lit FFFD lit um+ 30 lit + emit space 30 lit + emit cr
	5 lit FFFF lit um+ 30 lit + emit space 30 lit + emit cr
	FFFF lit FFFF lit um+ 30 lit + emit space 30 lit + emit cr
	\ 4 lit -5 lit u< if char Y emit else char N emit then cr
	\ 4 lit -5 lit  < if char Y emit else char N emit then cr
	\ 4 lit 8000 lit u< if char Y emit else char N emit then cr
	\ 8000 lit 8000 lit u< if char Y emit else char N emit then cr
	\ 8000 lit 4    lit u< if char Y emit else char N emit then cr
	cr

	char @ 1 lit xor emit cr
	char A 7 lit lshift FFFF lit xor 7 lit rshift FFFF lit xor emit cr
	char A 4 lit lshift FFFF lit xor 4 lit rshift FFFF lit xor emit cr
	char A FFFF lit xor FFFF lit xor emit cr
	char A FFFF lit and emit cr
	char A 80FF lit xor   FF lit and FF lit xor emit cr
	char A FFFF lit xor   FF lit and FF lit xor emit cr

	cr
	\ TODO: Should print 'A', fix them
	char A FFFF lit xor FF00 lit xor FF lit xor emit cr
	char A 8 lit lshift FFFF lit xor 8 lit rshift FFFF lit xor emit cr
	char A 8 lit lshift FF00 lit and 8 lit rshift 00FF lit and emit cr
	char A 8 lit lshift FF00 lit and 100 lit u/   00FF lit and emit cr
	char A 7FFF lit xor 7FFF lit xor emit cr
	char A 100 lit * FFFF lit xor 100 lit u/ FFFF lit xor emit cr

	\ words

	\ begin key? 0< while emit repeat drop
	#1 0> if
	  0 lit for char H emit char i emit char ! emit cr next
	  then
	bye ;t

\ ---------------------------------- Image Generation ------------------------

there h t!
atlast {last} t!
save-target subleq.dec
.stat
.end
bye
