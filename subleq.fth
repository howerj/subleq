defined eforth [if] ' nop <ok> ! [then]
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
\ - <https://www.bradrodriguez.com/papers/mtasking.html> For multitasking
\ - <https://forth-standard.org/standard/block> For the block word-set
\
\ Notes:
\
\ - If "see", the decompiler, was advanced enough we could dispense with the
\ source code, which is an interesting concept.
\ - The image could determine the virtual machine size, and adjust itself
\ accordingly, it would not even require a power-of-two integer width.
\ - The image could be compressed with LZSS to save on space.
\ - A website with an interactive simulator is available at:
\   <https://github.com/howerj/subleq-js>
\ - It would be nice to make a 7400 Integrated Circuit board that could run
\ and execute this code, or a project in VHDL for an FPGA that could do it.
\ - The virtual machine could be sped up with optimization magic, it should
\ be noted that nearly half the memory used by this system is used by the
\ virtual machine needed to run a higher level language. Also of note, it
\ would be possible to make the virtual machine support the TASK and USER
\ word-sets, allowing for cooperative multitasking.
\
\ TODO: Fast div by 10 for numbers, fix comparison,
\ support more of the numeric words (perhaps one that accepts a buffer).
\ Perhaps the TASK and USER word set could also be supported. Fix "COLD",
\ it does not work quite right.
only forth definitions hex

: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if
      1+ r> -rot exit
    then rdrop
  then ;
: -order get-order (order) nip set-order ;
: +order dup >r -order get-order r> swap 1+ set-order ;

defined eforth [if] : wordlist here cell allot 0 over ! ; [then]

wordlist constant meta.1
wordlist constant target.1
wordlist constant assembler.1
wordlist constant target.only.1

defined eforth [if] system +order [then]
meta.1 +order definitions

   2 constant =cell
4000 constant size
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

: :m meta.1 +order definitions : ;
: ;m postpone ; ; immediate
:m tcell 2 ;m
:m there tdp @ ;m
:m tc! tflash + c! ;m
:m tc@ tflash + c@ ;m
:m t! over $FF and over tc! swap 8 rshift swap 1+ tc! ;m
:m t@ dup tc@ swap 1+ tc@ 8 lshift or ;m
:m taligned dup 1 and + ;m
:m talign there 1 and tdp +! ;m
:m tc, there tc! 1 tdp +! ;m
:m t, there t! 2 tdp +! ;m
:m tallot tdp +! ;m
defined eforth [if]
  :m tpack dup tc, for aft count tc, then next drop ;m
  :m parse-word bl word ?nul count ;m
  :m limit ;m
[else]
  :m tpack dup tc, 0 ?do count tc, loop drop ;m
  :m limit $FFFF and ;m
[then]
:m $literal talign [char] " word count tpack talign ;m
:m thead talign there tlast @ t, tlast ! parse-word talign tpack talign ;m
:m #dec dup >r abs 0 <# $A hold #s r> sign #> type ;m  ( n -- print number )
:m mdump aligned
    begin ?dup while swap dup @ limit #dec tcell + swap tcell - repeat drop ;m
:m save-target parse-word drop decimal tflash there mdump ;m
:m .end only forth definitions decimal ;m
:m setlast tlast ! ;m
:m atlast tlast @ ;m
:m tvar
  get-current >r meta.1 set-current create r> set-current there , t, does> @ ;m
:m label:
  get-current >r meta.1 set-current create r> set-current there ,    does> @ ;m
:m tdown =cell negate and ;m
:m tnfa =cell + ;m ( pwd -- nfa : move to name field address )
:m tcfa tnfa dup c@ $1F and + =cell + tdown ;m ( pwd -- cfa )
:m compile-only tlast @ tnfa t@ $20 or tlast @ tnfa t! ;m ( -- )
:m immediate    tlast @ tnfa t@ $40 or tlast @ tnfa t! ;m ( -- )
:m half dup 1 and abort" unaligned" 2/ ;m
:m double 2* ;m
defined eforth [if]
  :m (') bl word find ?found cfa ;m
  :m t' (') >body @ ;m
  :m to' target.only.1 +order (') >body @ target.only.1 -order ;m
 [else]
  :m t' ' >body @ ;m
  :m to' target.only.1 +order ' >body @ target.only.1 -order ;m
[then]
:m tcksum taligned dup $C0DE - $FFFF and >r
   begin ?dup while swap dup t@ r> + $FFFF and >r =cell + swap =cell - repeat
   drop r> ;m
:m mkck dup there swap - tcksum ;m
defined eforth [if] system -order [then]

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
:m MOV 2/ >r r@ dup t, t, NADDR 2/ t, Z  NADDR r> Z  t, NADDR Z Z NADDR ;m
:m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m
:m iJMP there 2/ E + 2* MOV NOP ;m
:m iSTORE ( addr w -- )
   swap >r there 2/ 24 + 2dup 2* MOV 2dup 1+ 2* MOV 7 + 2* MOV r> 0 MOV ;m

assembler.1 +order definitions
: begin talign there ;
: again JMP ;
: mark there 0 t, ;
: if ( NB. "if" does not work for 8000 )
   2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t, mark ;
: until 2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t, 2/ t, ;
: +if   Z 2/ t, mark ;
: -if 2/ t, Z there 2/ 4 + t, Z Z there 2/ 4 + t, Z Z mark ;
: then begin 2/ swap t! ;
: while if swap ;
: repeat JMP then ;
assembler.1 -order
meta.1 +order definitions

  0 t, 0 t,        \ both locations must be zero
label: entry       \ used to set entry point in next cell
  -1 t,            \ system entry point
  B tvar {options}  \ bit #1=echo off, #2 = checksum on, #4=info, #8=die on EOF
  0 tvar h         \ dictionary pointer
  0 tvar primitive \ any address lower than this one must be a primitive
  0 tvar check     \ used for system checksum
  8000 tvar hbit   \ must contain 8000
  -2   tvar ntwo   \ must contain -2
  -1 tvar neg1     \ must contain -1
  1 tvar one       \ must contain  1
  2 tvar two       \ must contain  1
 $10 tvar bwidth   \ must contain 16
  0 tvar INVREG    \ temporary register used for inversion only
  0 tvar {cold}    \ entry point of virtual machine program, set later on
  0 tvar {key}     \ execution vector for key?
  0 tvar {emit}    \ execution vector for emit
  0 tvar {literal} \ execution vector for literal
  0 tvar {ok}      \ execution vector for .ok
  0 tvar {echo}    \ execution vector for echo
  0 tvar ip        \ instruction pointer
  0 tvar w         \ working pointer
  0 tvar x         \ working pointer
  0 tvar t         \ temporary register for Virtual Machine
  0 tvar bl1       \ bitwise extra register
  0 tvar bl2       \ bitwise extra register
  0 tvar bt        \ bitwise extra register
  0 tvar tos       \ top of stack
  0 tvar {state}   \ compiler state
  0 tvar {hld}     \ hold space pointer
  $A tvar {base}   \ input/output radix
  -1 tvar {dpl}    \ number of places after fraction
  0 tvar {in}      \ position in query string
  0 tvar {handler} \ throw/catch handler
  0 tvar {last}    \ last defined word
  $2F tvar {blk}   \ current loaded block
  $2F tvar {scr}   \ last viewed screen
  0 tvar {dirty}   \ is block dirty?
  0 tvar {id}      \ executing from block or terminal?
  $C00 tvar {ms}   \ delay loop calibration variable
  0 tvar {tib}     \ terminal input buffer
  =cell tallot
  0 tvar {context} $E tallot \ vocabulary context
  0 tvar {current} \ vocabulary which new definitions are added to
  0 tvar {forth-wordlist} \ forth word list (main vocabulary)
  0 tvar {editor}   \ editor vocabulary
  0 tvar {root-voc} \ absolute minimum vocabulary
  0 tvar {system}  \ system functions vocabulary
  =end                       dup tvar {sp0} tvar {sp} \ grows downwards
  =end =stksz 4 * -          dup tvar {rp0} tvar {rp} \ grows upwards
  =end =stksz 4 * - =buf - constant TERMBUF \ pad buffer space
  TERMBUF =buf + constant =tbufend

:m INC 2/ neg1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ one  2/ t, t, NADDR ;m ( b -- )
:m INV ( b -- : invert NB. b - a = b + ~a + 1 )
  INVREG ZERO dup INVREG SUB dup INVREG swap MOV DEC ;m
:m ++sp {sp} DEC ;m
:m --sp {sp} INC ;m
:m --rp {rp} DEC ;m
:m ++rp {rp} INC ;m
:m a-optim >r there =cell - r> 2/ t! ;m

assembler.1 +order
label: start
  start 2/ entry t!
  {sp0} {sp} MOV
  {rp0} {rp} MOV
  {cold} ip MOV
  ( fall-through )
label: vm ( Forth Inner Interpreter )
  ip w MOV
  ip INC
  t w iLOAD
  t w MOV
  primitive t SUB
  t -if w iJMP then ( jump straight to VM functions )
  ++rp
  ip {rp} iSTORE
  w ip MOV vm a-optim
  vm JMP
assembler.1 -order

:m header >in @ thead >in ! ;m
:m :ht ( "name" -- : forth routine, no header )
  get-current >r target.1 set-current create
  r> set-current $BABE talign there ,
  does> @ 2/ t, ;m
:m :t header :ht ;m ( "name" -- : forth routine )
:m :to ( "name" -- : forth, target only routine )
  header
  get-current >r target.only.1 set-current create r> set-current
  $BABE talign there ,
  does> @ 2/ t, ;m
:m :a ( "name" -- : assembly routine, no header )
  D00D
  target.1 +order definitions
  create talign there , assembler.1 +order does> @ 2/ t, ;m
:m (a); D00D <> if abort" unstructured" then assembler.1 -order ;m
:m ;a (a); vm a-optim vm JMP ;m
:m postpone target.only.1 +order t' target.only.1 -order 2/ t, ;m
\ NB. There are some bugs with the comparison operators "op<" and "op>" when
\ they deal with extreme values like "$8000 1 <", "$8002 1 <" words fine.
:a bye HALT ;a
:a 1- tos DEC ;a
:a 1+ tos INC ;a
:a invert tos INV ;a
:a [@] tos tos iLOAD ;a
:a [!] w {sp} iLOAD w tos iSTORE --sp tos {sp} iLOAD --sp ;a
:a opEmit tos PUT tos {sp} iLOAD --sp ;a
:a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a
:a opSwap tos w MOV tos {sp} iLOAD w {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a
:a opOver w {sp} iLOAD ++sp tos {sp} iSTORE w tos MOV ;a
:a opDrop tos {sp} iLOAD --sp ;a
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opMul w {sp} iLOAD t ZERO
   begin w while tos t ADD w DEC repeat t tos MOV --sp ;a
:a opExit ip {rp} iLOAD --rp ;a
:a - w {sp} iLOAD tos w SUB w tos MOV --sp ;a
:a + w {sp} iLOAD w tos ADD --sp ;a
:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a
:a rdrop --rp ;a
:a sp@ ++sp tos {sp} iSTORE {sp} tos MOV tos INC ;a
:a sp! tos {sp} MOV ;a
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a
:a rp! tos {rp} MOV tos {sp} iLOAD --sp ;a
:a opNext w {rp} iLOAD
   w if w DEC w {rp} iSTORE t ip iLOAD t ip MOV vm JMP then ip INC --rp ;a
:a lsb
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD
    tos w MOV 0 tos MOV w if neg1 tos MOV then ;a
:a opJump ip ip iLOAD ;a
:a opJumpZ
  tos w MOV 0 t MOV w if neg1 t MOV then w DEC w +if neg1 t MOV then
  tos {sp} iLOAD --sp t if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a op0> tos w MOV 0 tos MOV w +if neg1 tos MOV then ;a
:a op0=
   tos w MOV neg1 tos MOV w if 0 tos MOV then w DEC w +if 0 tos MOV then ;a
:a op0<
   tos w MOV 0 tos MOV w -if neg1 tos MOV then w INC w -if neg1 tos MOV then ;a
:a op< w {sp} iLOAD --sp tos w SUB 0 tos MOV w -if neg1 tos MOV then ;a
:a op> w {sp} iLOAD --sp tos w SUB 0 tos MOV w +if neg1 tos MOV then ;a
:a op2* tos tos ADD ;a
:a op2/
  bwidth w MOV
  x ZERO
  begin w DEC w while
    x x ADD
  tos bt MOV 0 bl1 MOV bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
    bl1 if x INC then
    tos tos ADD
  repeat
  x tos MOV ;a
:a rshift
  bwidth w MOV
  tos w SUB
  tos {sp} iLOAD --sp
  x ZERO
  begin w while
    x x ADD
  tos bt MOV 0 bl1 MOV bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
    bl1 if x INC then
    tos tos ADD
    w DEC
  repeat
  x tos MOV ;a
:a opOr
  bwidth w MOV
  x ZERO
  t {sp} iLOAD
  --sp
  begin w while
   x x ADD
 tos bt MOV 0 bl1 MOV bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
 t   bt MOV 0 bl2 MOV bt -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD bl2 if x INC then
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a
:a opXor
  bwidth w MOV
  x ZERO
  t {sp} iLOAD
  --sp
  begin w while
   x x ADD
 tos bt MOV 0 bl1 MOV bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
 t   bt MOV 0 bl2 MOV bt -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD bl2 INC one bl1 MOV bl2 if 0 bl1 MOV then bl1 x ADD
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a
:a opAnd
  bwidth w MOV
  x ZERO
  t {sp} iLOAD
  --sp
  begin w while
   x x ADD
 tos bt MOV 0 bl1 MOV bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
 t   bt MOV 0 bl2 MOV bt -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD two bl2 ADD one bl1 MOV bl2 if 0 bl1 MOV then bl1 x ADD
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a

there 2/ primitive t!

:m ;t $BABE <>
     if abort" unstructured" then talign opExit target.only.1 -order ;m
:m :s tlast @ {system} t@ tlast ! $F00D :t drop 0 ;m
:m :so  tlast @ {system} t@ tlast ! $F00D :to drop 0 ;m
:m ;s drop $BABE ;t $F00D <> if abort" unstructured" then
   tlast @ {system} t! tlast ! ;m
:m :r tlast @ {root-voc} t@ tlast ! $BEEF :t drop 0 ;m
:m ;r drop $BABE ;t $BEEF <> if abort" unstructured" then
   tlast @ {root-voc} t! tlast ! ;m
:m :e tlast @ {editor} t@ tlast ! $DEAD :t drop 0 ;m
:m ;e drop $BABE ;t $DEAD <> if abort" unstructured" then
   tlast @ {editor} t! tlast ! ;m

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
:m next talign opNext 2/ t, ;m
:m for opToR begin ;m
:m =push   [ t' opPush  half ] literal ;m
:m =jump   [ t' opJump  half ] literal ;m
:m =jumpz  [ t' opJumpZ half ] literal ;m
:m =unnest [ t' opExit  half ] literal ;m
:m =>r     [ t' opToR   half ] literal ;m
:m =next   [ t' opNext  half ] literal ;m
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
:m or opOr ;m
:m xor opXor ;m
:m and opAnd ;m
:m exit opExit ;m
:m 2/ op2/ ;m

:s #0 0 lit ;s
:s #1 1 lit ;s
:s #-1 FFFF lit ;s
:to 1+ 1+ ;t
:to 1- 1- ;t
:to + + ;t
:to - - ;t
:to invert invert ;t
:to bye bye ;t
:to dup dup ;t
:to drop opDrop ;t
:to over opOver ;t
:to swap opSwap ;t
:to rshift rshift  ;t
:so [@] [@] ;s
:so [!] [!] ;s
:so lsb lsb ;s
:to sp@ sp@ ;t
:to sp! sp! ;t
:to 0> op0> ;t
:to 0= op0= ;t
:to 0< op0< ;t
:to < op< ;t
:to > op> ;t
:to 2/ op2/ ;t
:to or opOr ;t
:to xor opXor ;t
:to and opAnd ;t
:to * opMul ;t
:t nop ;t
:t <ok> {ok} lit ;t
:s <emit> {emit} lit ;s
:s <key>  {key} lit ;s
:s <literal> {literal} lit ;s
:s <cold> {cold} lit ;s
:s <echo> {echo} lit ;s
:t current {current} lit ;t
:t root-voc {root-voc} lit ;t
:t #vocs 8 lit ;t
:t context {context} lit ;t
:t here h half lit [@] ;t
:t base {base} lit ;t
:t dpl {dpl} lit ;t
:t hld {hld} lit ;t
:t bl 20 lit ;t
:t >in {in} lit ;t
:t hex  $10 lit {base} half lit [!] ;t
:t decimal $A lit {base} half lit [!] ;t
:s many #0 {in} half lit [!] ;s
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
:t >= < 0= ;t
:t <= > 0= ;t
:t 0>= 0< 0= ;t
:t negate 1- invert ;t
:t s>d dup 0< ;t
:t abs s>d if negate then ;t
:t 2* op2* ;t
:t cell 2 lit ;t
:t cell+ cell + ;t
:t cells op2* ;t
:t u< 2dup 0< 0= swap 0< 0= <> >r < r> <>  ;t
:t u> swap u< ;t
:t u>= u< 0= ;t
:t u<= u> 0= ;t
:t execute 2/ >r ;t
:t key? #-1 [@] negate s>d if
      {options} half lit [@] 8 lit and if bye then drop #0 exit then #-1 ;t
:t key begin {key} half lit [@] execute until ;t
:t emit {emit} half lit [@] execute ;t
:t cr =cr lit emit =lf lit emit ;t
:t @ 2/ [@] ;t
:t ! 2/ [!] ;t
:t get-current current @ ;t
:t set-current current ! ;t
:s last get-current @ ;s
:t pick sp@ + [@] ;t
:t +! 2/ tuck [@] + swap [!] ;t
:t lshift begin ?dup while 1- swap 2* swap repeat ;t
:t c@ dup @ swap lsb if 8 lit rshift else $FF lit and then ;t
:t c!  swap $FF lit and dup 8 lit lshift or swap
   tuck dup @ swap lsb 0= $FF lit xor
   >r over xor r> and xor swap ! ;t
:t max 2dup < if nip else drop then ;t
:t min 2dup > if nip else drop then ;t
:t source-id {id} lit @ ;t
:t 2! tuck ! cell+ ! ;t    
:t 2@ dup cell+ @ swap @ ;t
:t source {tib} lit 2@ ;t
:t 2>r r> swap >r swap >r >r ;t compile-only
:t 2r> r> r> swap r> swap >r nop ;t compile-only
:t count dup 1+ swap c@ ;t
:s logical 0<> if #1 else #0 then ;s
:t aligned dup lsb logical + ;t
:t align here aligned h half lit [!] ;t
:t +string #1 over min rot over + rot rot - ;t
:t type begin dup while swap count emit swap 1- repeat 2drop ;t
:t cmove for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;t ( b1 b2 u -- )
:t fill  swap for swap aft 2dup c! 1+ then next 2drop ;t     ( b u c -- )
:t erase #0 fill ;t ( NB. blank is bl fill )
:s do$ r> r> 2* dup count + aligned 2/ >r swap >r ;s ( -- a : )
:s ($) do$ ;s            ( -- a : do string NB. )
:s .$ do$ count type ;s  ( -- : print string, next cells contain string )
:m ." .$ $literal ;m
:m $" ($) $literal ;m
:t space bl emit ;t
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
:t abort #-1 throw ;t
:s (abort) do$ swap if count type abort then drop ;s
:t um+
  2dup + >r r@ #0 >= >r 2dup and 0< r> or >r or 0< r> and invert 1+ r> swap ;t
:t dnegate invert >r invert #1 um+ r> + ;t ( d -- d )
:t d+ >r swap >r um+ r> + r> + ;t         ( d d -- d )
:t um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) $F lit
  for
    dup um+ >r >r dup um+ r> + r>
    if >r over um+ r> + then
  next rot drop ;t
:t um/mod ( ud u -- ur uq : unsigned double cell width divide/modulo )
  ?dup 0= if -A lit throw then
  2dup u<
  if negate $F lit
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> ( or -> ) 0<> swap 0<> +
      if >r drop 1+ r> else drop then r>
    next
    drop swap exit
  then 2drop drop #-1 dup ;t
:t m/mod ( d n -- r q : floored division )
  s>d dup >r
  if
    negate >r dnegate r>
  then
  >r s>d if r@ + then r> um/mod r>
  if swap negate swap exit then ;t
:t /mod  over 0< swap m/mod ;t
:t mod  /mod drop ;t
:t /    /mod nip ;t
:s depth {sp0} half lit [@] sp@ - 1- ;s
:s (emit) opEmit ;s
:t echo {echo} half lit [@] execute ;t
:s tap dup echo over c! 1+ ;s ( bot eot cur c -- bot eot cur )
:s ktap ( bot eot cur c -- bot eot cur )
  dup dup =cr lit <> >r  =lf lit <> r> and if ( Not End of Line? )
    dup =bksp lit <> >r =del lit <> r> and if ( Not Delete Char? )
      bl tap
      exit
    then
    >r over r@ < dup if
      =bksp lit dup echo bl echo echo
    then
    r> +
    exit
  then drop nip dup ;s
:t accept ( b u -- b u : read in a line of user input )
  over + over begin
    2dup <>
  while
    key dup bl - $5F lit u< if tap else ktap then
  repeat drop over - ;t
:t tib source drop ;t
:t query tib =buf lit accept {tib} lit ! drop #0 >in ! ;t
:s ?depth depth > if -4 lit throw then ;s ( u -- : check stack depth )
:t -trailing for aft bl over r@ + c@ < if r> 1+ exit then then next #0 ;t
:s look ( b u c xt -- b u : skip until *xt* test succeeds )
  swap >r rot rot
  begin
    dup
  while
    over c@ r@ - r@ bl = 4 lit pick execute
    if rdrop rot drop exit then
    +string
  repeat rdrop rot drop ;s
:s unmatch if 0> exit then 0<> ;s ( c1 c2 -- t )
:s match unmatch invert ;s        ( c1 c2 -- t )
:t parse ( c -- b u ; <string> )
    >r tib >in @ + {tib} lit @ >in @ - r@
    >r over r> swap >r >r
    r@ t' unmatch lit look 2dup
    r> t' match   lit look swap r> - >r - r> 1+  ( b u c -- b u delta )
    >in +!
    r> bl = if -trailing then #0 max ;t
:t spaces begin dup 0> while space 1- repeat drop ;t ( +n -- )
:t hold #-1 hld +! hld @ c! ;t ( c -- : save a character in hold space )
:t #> 2drop hld @ =tbufend lit over - ;t  ( u -- b u )
:s extract dup >r um/mod r> swap >r um/mod r> rot ;s ( ud ud -- ud u )
:s digit 9 lit over < 7 lit and + [char] 0 + ;s ( u -- c )
:t #  2 lit ?depth #0 base @ extract digit hold ;t ( d -- d )
:t #s begin # 2dup ( d0= -> ) or 0= until ;t       ( d -- 0 )
:t <# =tbufend lit hld ! ;t                        ( -- )
:t sign 0< if [char] - hold then ;t                ( n -- )
:t u.r >r #0 <# #s #>  r> over - spaces type ;t
:t u.     #0 <# #s #> space type ;t
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
    swap base @ um* drop rot base @ um* d+ ( accumulate digit )
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
  while over c@ [char] . <>
    if rot drop rot r> 2drop #0 r> base ! exit then
    1- dpl ! 1+ dpl @
  repeat
  2drop r> if dnegate then r> base ! #-1 ;t
:t compare ( a1 u1 a2 u2 -- n : string equality )
  rot
  over - ?dup if >r 2drop r> nip exit then
  for ( a1 a2 )
    aft
      count rot count rot - ?dup
      if rdrop nip nip exit then
    then
  next 2drop #0 ;t
:t .s depth for aft r@ pick . then next ;t
:t nfa cell+ ;t ( pwd -- nfa : move word pointer to name field )
:t cfa nfa dup c@ $1F lit and + cell+ cell negate and ;t ( pwd -- cfa )
:t allot aligned h lit +! ;t
:t , align here ! cell allot ;t
:s (search-wordlist) ( a wid -- PWD PWD 1|PWD PWD -1|0 a 0: find word in WID )
  swap >r dup
  begin
    dup
  while
    dup nfa count $9F lit ( $1F:word-length + $80:hidden )
    and r@ count compare 0=
    if ( found! )
      rdrop
      dup ( immediate? -> ) nfa $40 lit swap @ and 0<>
      #1 or negate exit
    then
    nip dup @
  repeat
  rdrop 2drop #0 ;s
:s (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word dictionary )
  >r
  context
  begin
    dup @
  while
    dup @ @ r@ swap (search-wordlist) ?dup
    if
      >r rot drop r> rdrop exit
    then
    cell+
  repeat drop #0 r> #0 ;s
:t search-wordlist (search-wordlist) rot drop ;t ( a wid -- PWD 1|PWD -1|a 0 )
:t find ( a -- pwd 1 | pwd -1 | a 0 : find a word in the dictionary )
  (find) rot drop ;t
:s (literal) state @ if =push lit , , then ;s
:t literal <literal> @ execute ;t immediate ( u -- )
:t compile, 2/ align , ;t  ( xt -- )
:s ?found
   if exit then space count type [char] ? emit cr -D lit throw ;s ( u f -- )
:t interpret ( b -- )
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
  r> #0 ?found ;t \ Could vector ?found here, to handle arbitrary words
:s .id nfa count $1F lit and type space ;s ( pwd -- : print out a word )
:t get-order ( -- widn ... wid1 n : get the current search order )
  context
  ( find empty cell -> ) #0 >r begin dup @ r@ xor while cell+ repeat rdrop
  dup cell - swap
  context - 2/ dup >r 1- s>d if -50 lit throw then
  for aft dup @ swap cell - then next @ r> ;t
:r set-order ( widn ... wid1 n -- : set the current search order )
  dup #-1 = if drop root-voc #1 set-order exit then ( NB. Recursion! )
  dup #vocs > if -49 lit throw then
  context swap for aft tuck ! cell+ then next #0 swap ! ;r
:r forth-wordlist {forth-wordlist} lit ;r ( -- wid )
:r system {system} lit ;r ( -- wid )
:r forth root-voc forth-wordlist 2 lit set-order ;r ( -- )
:r only #-1 set-order ;r                            ( -- )
:r words
  get-order begin ?dup while swap dup cr u. ." : " @
    begin ?dup while dup nfa c@ $80 lit and 0= if dup .id then @ repeat cr
  1- repeat ;r
:t definitions context @ set-current ;t
:t word parse here dup >r 2dup ! 1+ swap cmove r> ;t ( c -- b )
:s ?unique ( a -- a : print a message if a word definition is not unique )
 dup get-current (search-wordlist) 0= if exit then space
 2drop {last} lit @ .id ." redefined" cr ;s
:s ?nul dup c@ if exit then -10 lit throw ;s
:to char bl word ?nul count drop c@ ;t
:to [char] postpone char =push lit , , ;t immediate
:to ; $BABE lit <> if -16 lit throw then =unnest lit , postpone [
 ?dup if get-current ! exit then ;t immediate compile-only ( -- wid )
:to : align here dup {last} lit ! ( "name", -- colon-sys )
  last , bl word ?nul ?unique count + h lit ! align $BABE lit postpone ] ;t
:to :noname here $BABE lit ] ;t
:to begin align here ;t immediate compile-only
:to until =jumpz lit , 2/ , ;t immediate compile-only
:to again =jump  lit , 2/ , ;t immediate compile-only
:to if =jumpz lit , here #0 , ;t immediate compile-only
:to then here 2/ swap ! ;t immediate compile-only
:to while postpone if ;t immediate compile-only
:to repeat swap postpone again postpone then ;t immediate compile-only
:to else =jump lit , here #0 , swap postpone then ;t immediate compile-only
:to for =>r lit , here ;t immediate compile-only
:to aft drop =jump lit , here #0 , align here swap ;t immediate compile-only
:to next =next lit , 2/ , ;t immediate compile-only
:to ' bl word find ?found cfa literal ;t immediate
:t compile r> dup [@] , 1+ >r ;t compile-only
:t recurse {last} lit @ cfa compile, ;t immediate compile-only
:s toggle tuck @ xor swap ! ;s
:s hide bl word find ?found nfa $80 lit swap toggle ;s
:s (var) r> 2* ;s compile-only
:s (const) r> [@] ;s compile-only
:s (marker) r> 2* dup @ h lit ! cell+ @ get-current ! ;s compile-only
:t create postpone : drop postpone [ compile (var) get-current ! ;t
:to variable create #0 , ;t
:to constant create cell negate allot compile (const) , ;t
:to marker last here create cell negate allot compile (marker) , , ;t
:t >body cell+ ;t ( a -- a )
:s (does) r> r> 2* swap >r ;s compile-only
:s (comp) r> {last} lit @ cfa ! ;s compile-only
:t does> compile (comp) compile (does) ;t immediate compile-only
:to rp! compile rp! ;t immediate compile-only
:to rp@ compile rp@ ;t immediate compile-only
:to >r compile opToR ;t immediate compile-only
:to r> compile opFromR ;t immediate compile-only
:to r@ compile r@ ;t immediate compile-only
:to rdrop compile rdrop ;t immediate compile-only
:to exit compile opExit ;t immediate compile-only
:to ." compile .$
  [char] " word count + h half lit [!] align ;t immediate compile-only
:to $" compile ($)
  [char] " word count + h half lit [!] align ;t immediate compile-only
:to abort" compile (abort)
  [char] " word count + h half lit [!] align ;t immediate compile-only
:to ( [char] ) parse 2drop ;t immediate
:to .( [char] ) parse type ;t immediate
:to postpone bl word find ?found cfa compile, ;t immediate
:to ) ;t immediate
:to \ tib @ {in} half lit [!] ;t immediate
:to immediate last nfa @ $40 lit or last nfa ! ;t
:to see bl word find ?found
    cr begin dup @ =unnest lit <> while dup @ u. cell+ repeat @ u. ;t
:to dump aligned begin ?dup while swap dup @ . cell+ swap cell - repeat drop ;t
:s cksum aligned dup $C0DE lit - >r
     begin ?dup while swap dup @ r> + >r cell+ swap cell - repeat drop r> ;s
:t defined bl word find nip 0<> ;t
:to [then] ;t immediate
:to [else]
   begin
     begin bl word dup c@ while
       find drop cfa dup t' [else] lit = swap t' [then] lit = or
       if exit then repeat query again ;t immediate
:to [if] if exit then postpone [else] ;t immediate
:s ok state @ 0= if ."  ok" cr then ;s
:s eval
   begin bl word dup c@ while
     interpret #1 ?depth
   repeat drop {ok} half lit [@] execute ;s ( "word" -- )
:r eforth $0106 lit ;r ( -- version )
:s info cr
  ." Project: eForth v1.6 " ( here . ) cr
  ." Author:  Richard James Howe" cr
  ." Email:   howe.r.j.89@gmail.com" cr
  ." Repo:    https://github.com/howerj/subleq" cr
  ." License: The Unlicense / Public Domain" cr ;s
:s ini only forth definitions decimal postpone [
  #0 {in} half lit [!] #-1 {dpl} half lit [!]
  TERMBUF lit #0 {tib} lit 2! ;s ( -- )
:s opts
  {options} half lit [@] lsb if to' drop lit {echo} half lit [!] then
  {options} half lit [@] 4 lit and if info then
  {options} half lit [@] 2 lit and if
    primitive half lit [@] 2* dup here swap - cksum
    check half lit [@] <> if ." cksum fail" bye then
    {options} half lit [@] 2 lit xor {options} half lit [!]
  then ;s
:t quit ( -- : interpreter loop, and more, does more than most QUITs )
  ini
  opts
  begin
   query t' eval lit catch
   ?dup if dup space . [char] ? emit cr #-1 = if bye then ini then
  again ;t
:t cold {cold} half lit [@] execute ;t

\ TODO: Create an in memory file system optimized for constrained system
\ that only requires the block word set ("block", "blk", "update", "flush").
\ This could then be used to make a DOS like program OS for computer games...
\ TODO: Editor should catch errors, use different 'ini'
\ TODO: Make a simpler list? With the following format:
\       number line cr
:s calibration {ms} lit ;s
:t bell 7 lit emit ;t
:t ms for $800 lit for next next ;t
:s csi $1B lit emit $5B lit emit ;s
:t page csi ." 2J" csi ." 1;1H" ( csi ." 0m" ) ;t
:t at-xy base @ decimal >r csi #0 u.r ." ;" #0 u.r ." H" r> base ! ;t
:t blk {blk} lit ;t
:t scr {scr} lit ;t
:t editor root-voc {editor} lit 2 lit set-order ;t
:t b/buf $400 lit ;t
:t block dup blk ! $A lit lshift ;t ( NB. No mass storage! )
:t update #-1 {dirty} lit ! ;t
:t flush ;t ( NOP, no mass storage )
:t blank bl fill ;t
:t within over - >r - r> u< ;t ( u lo hi -- t )
:s banner 1- for [char] - emit next ;s
:s right [char] | emit ;s
:s left right ;s
:s head space space space space $40 lit banner cr ;s
:s display dup $20 lit $7F lit within 0= if drop [char] . then emit ;s
:t list page cr head dup scr ! block $F lit
    for 
      $F lit r@ - 3 lit u.r left $3F lit for count display next right cr 
    next drop head ;t
:t get-input source >in @ source-id <ok> @ ;t ( -- n1...n5 )
:t set-input <ok> ! {id} lit ! >in ! {tib} lit 2! ;t     ( n1...n5 -- )
:t evaluate ( a u -- ) \ TODO: Fix/Buggy
  get-input 2>r 2>r >r
  #0 #-1 t' nop lit set-input
  t' eval lit catch
  r> 2r> 2r> set-input
  throw ;t
:s line 6 lit lshift swap block + $40 lit ;s
:s loadline line evaluate ;s
:t load #0 $F lit for 2dup 2>r loadline 2r> 1+ next 2drop ;t ( k -- )
\ 0 [if]
\ :e h
\   ."                    FORTH BLOCK EDITOR" cr
\   ."   w - list commands.               l - list current block." cr
\   ."   x - execute current block.       e - erase current block." cr
\   ."   h - display help.                s - save and flush." cr
\   ."   q - exit back to forth.          ? - display current block." cr
\   ."   n - move to next block.          p - move to previous block." cr
\   ." # d - delete line #.             # r - set current block." cr
\   ." # i ... - replace line.      #1 #2 ia ... - replace line at." cr ;e
\ [then]
:e q only forth ;e
:e ? scr @ . ;e
:e l scr @ list scr @ . cr ;e
:e e q scr @ load editor ;e
:e ia 6 lit lshift + scr @ block + tib >in @ +
   swap source nip >in @ - cmove
   tib @ {in} lit ! ( <- *OR* postpone \ ) update l ;e
:e i #0 swap ia ;e
:e w words ;e
:e s update flush ;e
:e n  #1 scr +! l ;e
:e p #-1 scr +! l ;e
:e r scr ! l ;e
:e x scr @ block b/buf blank l ;e
:e d >r scr @ block r> 6 lit lshift + $40 lit blank l ;e

\ ---------------------------------- Image Generation ------------------------

t' quit half {cold} t!
t' key? {key} t!
t' (emit) {echo} t!
t' (emit) {emit} t!
t' ok {ok} t!
t' (literal) {literal} t!
atlast {forth-wordlist} t!
{forth-wordlist} {current} t!
there h t!
primitive t@ double mkck check t!
atlast {last} t!
save-target subleq.dec
there .end
bye
