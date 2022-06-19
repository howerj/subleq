defined eforth [if] ' nop <ok> ! [then] ( Turn off ok prompt )
only forth definitions hex
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
defined eforth [if]
  : wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )
[then]
wordlist constant meta.1        ( meta-compiler word set )
wordlist constant target.1      ( target eForth word set )
wordlist constant assembler.1   ( assembler word set )
wordlist constant target.only.1 ( target only word set )
defined eforth [if] system +order [then]
meta.1 +order definitions
   2 constant =cell  \ Target cell size
4000 constant size   \ Size of image working area
 100 constant =buf   \ Size of text input buffers in target
 100 constant =stksz \ Size of return and variable stacks
0008 constant =bksp  \ Backspace character value
000A constant =lf    \ Line feed character value
000D constant =cr    \ Carriage Return character value
007F constant =del   \ Delete character
FC00 constant =stack-start \ Initial start of stack
create tflash tflash size cells allot size erase
variable tdp 0 tdp ! ( target dictionary pointer )
variable tlast 0 tlast ! ( last defined target word pointer )
variable tlocal 0 tlocal ! ( local variable allocator )
: :m meta.1 +order definitions : ; ( --, "name" )
: ;m postpone ; ; immediate ( -- )
:m tcell 2 ;m ( -- 2 : bytes in a target cell )
:m there tdp @ ;m ( -- a : target dictionary pointer value )
:m tc! tflash + c! ;m ( c a -- : target write char )
:m tc@ tflash + c@ ;m ( a -- c : target get char )
:m t! over FF and over tc! swap 8 rshift swap 1+ tc! ;m
:m t@ dup tc@ swap 1+ tc@ 8 lshift or ;m ( a -- u : target @ )
:m taligned dup 1 and + ;m ( u -- u : align target pointer )
:m talign there 1 and tdp +! ;m ( -- : align target dic. ptr. )
:m tc, there tc! 1 tdp +! ;m ( c -- : write char to targ. dic.)
:m t, there t! 2 tdp +! ;m ( u -- : write cell to target dic. )
:m tallot tdp +! ;m ( u -- : allocate bytes in target dic. )
defined eforth [if]
  :m tpack dup tc, for aft count tc, then next drop ;m
  :m parse-word bl word ?nul count ;m ( -- a u )
  :m limit ;m ( u -- u16 : not needed on 16-bit systems )
[else]
  :m tpack talign dup tc, 0 ?do count tc, loop drop ;m
  :m limit FFFF and ;m ( u -- u16 : limit variable to 16 bits )
[then]
:m $literal talign [char] " word count tpack talign ;m
defined eforth [if]
  :m #dec dup 0< if [char] - emit then (.) ;m ( n16 -- )
[else]
  :m #dec dup 8000 u>= if negate limit -1 >r else 0 >r then
     0 <# #s r> sign #> type ;m ( n16 -- )
[then]
0 constant cgen
cgen [if] :m msep 2C emit ;m [else] :m msep A emit ;m [then]
:m mdump taligned ( a u -- )
  begin ?dup
  while swap dup @ limit #dec msep tcell + swap tcell -
  repeat drop ;m
:m save-target decimal tflash there mdump ;m ( -- )
:m .end only forth definitions decimal ;m ( -- )
:m atlast tlast @ ;m ( -- a )
:m lallot >r tlocal @ r> + tlocal ! ;m ( u -- allot in target )
:m tuser ( --, "name", Created-Word: -- u )
  get-current >r meta.1 set-current create r>
  set-current tlocal @ =cell lallot , does> @ ;m
:m tvar get-current >r ( --, "name", Created-Word: -- a )
     meta.1 set-current create
   r> set-current there , t, does> @ ;m
:m label: get-current >r ( --, "name", Created-Word: -- a )
     meta.1 set-current create
   r> set-current there ,    does> @ ;m
:m tdown =cell negate and ;m ( a -- a : align down )
:m tnfa =cell + ;m ( pwd -- nfa : move to name field address )
:m tcfa tnfa dup c@ 1F and + =cell + tdown ;m ( pwd -- cfa )
:m compile-only tlast @ tnfa t@ 20 or tlast @ tnfa t! ;m 
:m immediate    tlast @ tnfa t@ 40 or tlast @ tnfa t! ;m 
:m half dup 1 and abort" unaligned" 2/ ;m ( a -- a )
:m double 2* ;m ( a -- a )
defined eforth [if]
:m (') bl word find ?found cfa ;m
:m t' (') >body @ ;m ( --, "name" )
:m to' target.only.1 +order (') >body @ target.only.1 -order ;m
[else]
:m t' ' >body @ ;m ( --, "name" )
:m to' target.only.1 +order ' >body @ target.only.1 -order ;m
[then]
:m tcksum taligned dup C0DE - FFFF and >r
   begin ?dup
   while swap dup t@ r> + FFFF and >r =cell + swap =cell -
   repeat drop r> ;m ( a u -- u : compute a checksum )
:m mkck dup there swap - tcksum ;m ( -- u : checksum of image )
:m postpone ( --, "name" )
   target.only.1 +order t' target.only.1 -order 2/ t, ;m
:m thead talign there tlast @ t, tlast !
   parse-word talign tpack talign ;m ( --, "name" )
:m header >in @ thead >in ! ;m ( --, "name" )
:m :ht ( "name" -- : forth routine, no header )
  get-current >r target.1 set-current create
  r> set-current BABE talign there ,
  does> @ 2/ t, ;m
:m :t header :ht ;m ( "name" -- : forth routine )
:m :to ( "name" -- : forth, target only routine )
  header
  get-current >r
    target.only.1 set-current create
  r> set-current
  BABE talign there ,
  does> @ 2/ t, ;m
:m :a ( "name" -- : assembly routine, no header )
  D00D target.1 +order definitions
  create talign there , assembler.1 +order does> @ 2/ t, ;m
:m (a); D00D <>
   if abort" unstructured" then assembler.1 -order ;m
defined eforth [if] system -order [then]
:m Z 0 t, ;m ( -- : Address 0 must contain 0 )
:m NADDR there 2/ 1+ t, ;m ( --, jump to next cell )
:m HALT 0 t, 0 t, -1 t, ;m ( --, Halt but do not catch fire )
:m JMP 2/ Z Z t, ;m ( a --, Jump to location )
:m ADD swap 2/ t, Z NADDR Z 2/ t, NADDR Z Z NADDR ;m
:m SUB swap 2/ t, 2/ t, NADDR ;m ( a a -- : subtract )
:m NOOP Z Z NADDR ;m ( -- : No operation )
:m ZERO dup 2/ t, 2/ t, NADDR ;m ( a -- : zero a location )
:m PUT 2/ t, -1 t, NADDR ;m ( a -- : put a byte )
:m GET 2/ -1 t, t, NADDR ;m ( a -- : get a byte )
:m MOV 2/ >r r@ dup t, t, NADDR 2/ t, Z  NADDR r> Z  t, NADDR
   Z Z NADDR ;m
:m iLOAD there 2/ 3 4 * 3 + + 2* MOV 0 swap MOV ;m ( a a -- )
:m iJMP there 2/ E + 2* MOV Z Z NADDR ;m ( a -- )
:m iSTORE ( a a -- )
   swap >r there 2/ 24 + 2dup 2* MOV 2dup 1+ 2* MOV 7 + 2* MOV
   r> 0 MOV ;m
assembler.1 +order definitions
: begin talign there ; ( -- a )
: again JMP ; ( a -- )
: mark there 0 t, ; ( -- a : create hole in dictionary )
: if ( a -- a : NB. "if" does not work for 8000 )
   2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t, Z Z NADDR Z t,
   mark ;
: until 2/ dup t, Z there 2/ 4 + dup t, Z Z 6 + t,
   Z Z NADDR Z t, 2/ t, ; ( a -- a )
: +if   Z 2/ t, mark ; ( a -- a )
: -if 2/ t, Z there 2/ 4 + t, Z Z there 2/ 4 + t, Z Z mark ;
: then begin 2/ swap t! ; ( a -- )
: while if swap ; ( a a -- a a )
: repeat JMP then ; ( a a -- )
assembler.1 -order
meta.1 +order definitions
  0 t, 0 t,        \ both locations must be zero
label: entry       \ used to set entry point in next cell
  -1 t,            \ system entry point
  B tvar {options} \ bit #1=echo off, #2 = checksum on,
                   \ #4=info, #8=die on EOF
  0 tvar primitive \ any address lower must be a VM primitive
  8000 tvar hbit   \ must contain 8000
  7FFF tvar imax   \ must contain 7FFF
  =stksz half tvar stacksz \ must contain 80
  -2   tvar ntwo   \ must contain -2
  -1 tvar neg1     \ must contain -1
  1 tvar one       \ must contain  1
  2 tvar two       \ must contain  1
 10 tvar bwidth    \ must contain 16
  0 tvar INVREG    \ temporary register used for inversion only
  0 tvar w         \ working pointer
  0 tvar x         \ working pointer
  0 tvar t         \ temporary register for Virtual Machine
  0 tvar bl1       \ bitwise extra register
  0 tvar bl2       \ bitwise extra register
  0 tvar bt        \ bitwise extra register
  0 tvar h         \ dictionary pointer
  =stack-start half tvar {up} \ Current task addr. (Half size)
  F00 tvar {ms}    \ delay loop calibration variable
  0 tvar check     \ used for system checksum
  0 tvar {context} E tallot \ vocabulary context
  0 tvar {current}  \ vocabulary to add new definitions to
  0 tvar {forth-wordlist} \ forth word list (main vocabulary)
  0 tvar {editor}   \ editor vocabulary
  0 tvar {root-voc} \ absolute minimum vocabulary
  0 tvar {system}   \ system functions vocabulary
  0 tvar {cold}     \ entry point of VM program, set later on
  0 tvar {dirty}    \ is block dirty?
  0 tvar {last}     \ last defined word
  0 tvar {cycles}   \ number of times we have switched tasks
  0 tvar {single}   \ is multi processing off?
  2F tvar {blk}     \ current loaded block
  2F tvar {scr}     \ last viewed screen
  0 tvar padding    \ BUG: Getting set during image generation
  \ Thread variables, not all of which are user variables
  0 tvar ip        \ instruction pointer
  0 tvar tos       \ top of stack
  =stack-start =stksz        + half dup tvar {rp0} tvar {rp}
  =stack-start =stksz double + half dup tvar {sp0} tvar {sp}
  200 constant =tib \ Start of terminal input buffer
  380 constant =num \ Start of numeric input buffer
  tuser {next-task} \ next task in task list
  tuser {ip-save}   \ saved instruction pointer
  tuser {tos-save}  \ saved top of variable stack
  tuser {rp-save}   \ saved return stack pointer
  tuser {sp-save}   \ saved variable stack pointer
  tuser {base}      \ input/output radix
  tuser {dpl}       \ number of places after fraction
  tuser {hld}       \ hold space pointer
  tuser {in}        \ position in query string
  tuser {key}       \ execution vector for key?
  tuser {emit}      \ execution vector for emit
  tuser {literal}   \ execution vector for literal
  tuser {ok}        \ execution vector for .ok
  tuser {echo}      \ execution vector for echo
  tuser {tap}       \ execution vector for ktap
  tuser {expect}    \ execution vector for expect
  tuser {error}     \ execution vector for error handling
  tuser {state}     \ compiler state
  tuser {handler}   \ throw/catch handler
  tuser {sender}    \ multitasking; msg. send, 0 = no message
  tuser {message}   \ multitasking; the message itself
  tuser {id}        \ executing from block or terminal?
  tuser {span}      \ used by "expect" to store char count
  tuser {tib}       \ terminal input buffer: cell 1,
  =cell lallot      \ terminal input buffer: cell 2
:m INC 2/ neg1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ one  2/ t, t, NADDR ;m ( b -- )
:m INV ( b -- : invert NB. b - a = b + ~a + 1 )
  INVREG ZERO dup INVREG SUB dup INVREG swap MOV DEC ;m
:m ++sp {sp} DEC ;m ( -- : grow variable stack )
:m --sp {sp} INC ;m ( -- : shrink variable stack )
:m --rp {rp} DEC ;m ( -- : shrink return stack )
:m ++rp {rp} INC ;m ( -- : grow return stack )
:m a-optim ;m \ >r there =cell - r> 2/ t! ;m ( a -- )
( Error message string "Error: Not a 16-bit SUBLEQ VM" )
 7 tvar err-str
 45 t, 72 t, 72 t, 6F t, 72 t, 0D t, 0A t, 00 t,
err-str 2/ tvar err-str-addr
assembler.1 +order
label: die
   err-str-addr x MOV
   w ZERO
   err-str w MOV
   begin
     w
   while
     w DEC
     x INC
     bl2 x iLOAD
     bl2 PUT
   repeat
   HALT
label: start         \ System Entry Point
  start 2/ entry t!  \ Set the system entry point
  \ check we are running on the right VM width
  imax x MOV x x ADD x +if die JMP then
  \ hbit +if die JMP then
  {sp0} {sp} MOV     \ Setup initial variable stack
  {rp0} {rp} MOV     \ Setup initial return stack
  {cold} ip MOV      \ Get the first instruction to execute
  ( fall-through )
label: vm ( Forth Inner Interpreter )
  ip w MOV           \ Move Instruction Pointer To Working Ptr.
  ip INC             \ IP now points to next instruction!
  t w iLOAD          \ Get actual instruction to execute
  t w MOV            \ Copy it, as SUB is destructive
  primitive t SUB    \ Check if it is a primitive
  t -if w iJMP then  \ Jump straight to VM functions if it is
  ++rp               \ If it wasn't a VM instruction, inc {rp}
  ip {rp} iSTORE     \ and store ip to return stack
  w ip MOV vm a-optim \ "w" becomes our next instruction
  vm JMP             \ Ad infinitum...
assembler.1 -order
:m ;a (a); vm a-optim vm JMP ;m
:a bye HALT ;a       ( -- : HALT system )
:a 1- tos DEC ;a     ( u -- u : increment tos by one )
:a 1+ tos INC ;a     ( u -- u : decrement tos by one )
:a invert tos INV ;a ( u -- u : bitwise invert tos )
:a [@] tos tos iLOAD ;a
:a [!] w {sp} iLOAD w tos iSTORE --sp tos {sp} iLOAD --sp ;a
:a opEmit tos PUT tos {sp} iLOAD --sp ;a ( c -- )
:a opKey ++sp tos {sp} iSTORE tos GET ;a ( -- n )
:a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a
:a opUp ++sp tos {sp} iSTORE tos ip iLOAD ip INC
   {up} tos ADD {up} tos ADD ;a
:a opSwap tos w MOV tos {sp} iLOAD w {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a ( n -- n n )
:a opOver w {sp} iLOAD ++sp tos {sp} iSTORE w tos MOV ;a
:a opDrop tos {sp} iLOAD --sp ;a ( n -- )
:a opToR ++rp tos {rp} iSTORE tos {sp} iLOAD --sp ;a
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opExit ip {rp} iLOAD --rp ;a ( R: a -- )
:a - w {sp} iLOAD tos w SUB w tos MOV --sp ;a ( n n -- n )
:a + w {sp} iLOAD w tos ADD --sp ;a ( n n -- n )
:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a ( R: u --, -- u )
:a rdrop --rp ;a ( R: u -- )
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a ( R: ???, -- u )
:a rp! tos {rp} MOV tos {sp} iLOAD --sp ;a ( u -- R: ??? )
:a sp@ ++sp tos {sp} iSTORE {sp} tos MOV tos INC ;a ( -- u )
:a sp! tos {sp} MOV ;a ( u u -- )
:a opNext w {rp} iLOAD ( R: n -- | n-1 )
   w if w DEC w {rp} iSTORE t ip iLOAD t ip MOV vm JMP then
   ip INC --rp ;a
:a lsb ( u -- f )
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD tos tos ADD tos tos ADD
    tos tos ADD tos tos ADD
    tos w MOV 0 tos MOV w if neg1 tos MOV then ;a
:a opJump ip ip iLOAD ;a ( -- )
:a opJumpZ ( u -- )
  tos w MOV 0 t MOV
  w if neg1 t MOV then w DEC w +if neg1 t MOV then
  tos {sp} iLOAD --sp
  t if ip INC vm JMP then w ip iLOAD w ip MOV ;a
:a op0> tos w MOV 0 tos MOV w +if neg1 tos MOV then ;a
:a op0=
   tos w MOV neg1 tos MOV
   w if 0 tos MOV then w DEC w +if 0 tos MOV then ;a
:a op0<
   tos w MOV 0 tos MOV
   w -if neg1 tos MOV then w INC w -if neg1 tos MOV then ;a
:a op< w {sp} iLOAD --sp tos w SUB 0 tos MOV
   w -if neg1 tos MOV then ;a
:a op> w {sp} iLOAD --sp tos w SUB 0 tos MOV
   w +if neg1 tos MOV then ;a
:a op2* tos tos ADD ;a
:a op2/
  bwidth w MOV
  x ZERO
  begin w DEC w while
    x x ADD
    tos bt MOV 0 bl1 MOV
    bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
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
    tos bt MOV 0 bl1 MOV
    bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
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
   tos bt MOV 0 bl1 MOV
   bt -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
   t   bt MOV 0 bl2 MOV
   bt -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
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
   tos bt MOV 0 bl1 MOV bt
   -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
   t   bt MOV 0 bl2 MOV bt
   -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD bl2 INC one bl1 MOV
   bl2 if 0 bl1 MOV then bl1 x ADD
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
 tos bt MOV 0 bl1 MOV bt
   -if neg1 bl1 MOV then bt INC bt -if neg1 bl1 MOV then
   t   bt MOV 0 bl2 MOV bt
   -if neg1 bl2 MOV then bt INC bt -if neg1 bl2 MOV then
   bl1 bl2 ADD two bl2 ADD one bl1 MOV
   bl2 if 0 bl1 MOV then bl1 x ADD
   t t ADD
   tos tos ADD
   w DEC
  repeat
  x tos MOV ;a
:a opDivMod
  w {sp} iLOAD
  t ZERO
  begin
    one x MOV
    w -if 0 x MOV then
    x
  while
    t INC
    tos w SUB
  repeat
  tos w ADD
  t DEC
  t tos MOV
  w {sp} iSTORE ;a
:a pause
  {single} if vm JMP then
  w {up} iLOAD \ load next task pointer from user storage
  w if
    {cycles} INC        \ increment "pause" count
    {up} t MOV  t INC   \ load TASK pointer, skip next task loc
      ip t iSTORE t INC \ save registers to current task
     tos t iSTORE t INC
    {rp} t iSTORE t INC
    {sp} t iSTORE t INC
        w {rp0} MOV stacksz {rp0} ADD \ change {rp0} to new loc
    {rp0} {sp0} MOV stacksz {sp0} ADD \ same but for {sp0}
     w {up} MOV w INC  \ set next task
      ip w iLOAD w INC \ reverse of save registers
     tos w iLOAD w INC
    {rp} w iLOAD w INC
    {sp} w iLOAD w INC \ we're all golden
  then ;a
there 2/ primitive t!
:m ;t BABE <> if abort" unstructured" then
  talign opExit target.only.1 -order ;m
:m :s tlast @ {system} t@ tlast ! F00D :t drop 0 ;m
:m :so  tlast @ {system} t@ tlast ! F00D :to drop 0 ;m
:m ;s drop BABE ;t F00D <> if abort" unstructured" then
  tlast @ {system} t! tlast ! ;m
:m :r tlast @ {root-voc} t@ tlast ! BEEF :t drop 0 ;m
:m ;r drop BABE ;t BEEF <> if abort" unstructured" then
  tlast @ {root-voc} t! tlast ! ;m
:m :e tlast @ {editor} t@ tlast ! DEAD :t drop 0 ;m
:m ;e drop BABE ;t DEAD <> if abort" unstructured" then
  tlast @ {editor} t! tlast ! ;m
:m : :t ;m ( -- ???, "name" : start cross-compilation )
:m ; ;t ;m ( ??? -- : end cross-compilation of a target word )
:m lit         opPush t, ;m ( n -- : compile literal )
:m up          opUp   t, ;m ( n -- : compile user variable )
:m [char] char opPush t, ;m ( --, "name" : compile char )
:m char   char opPush t, ;m ( --, "name" : compile char )
:m begin talign there ;m ( -- a )
:m until talign opJumpZ 2/ t, ;m  ( a -- )
:m again talign opJump  2/ t, ;m ( a -- )
:m if opJumpZ there 0 t, ;m ( -- a )
:m mark opJump there 0 t, ;m ( -- a )
:m then there 2/ swap t! ;m ( a -- )
:m else mark swap then ;m ( a -- a )
:m while if ;m ( -- a )
:m repeat swap again then ;m
:m aft drop mark begin swap ;m ( a -- a a )
:m next talign opNext 2/ t, ;m ( a -- )
:m for opToR begin ;m ( -- a )
:m =push   [ t' opPush  half ] literal ;m ( -- a )
:m =jump   [ t' opJump  half ] literal ;m ( -- a )
:m =jumpz  [ t' opJumpZ half ] literal ;m ( -- a )
:m =unnest [ t' opExit  half ] literal ;m ( -- a )
:m =>r     [ t' opToR   half ] literal ;m ( -- a )
:m =next   [ t' opNext  half ] literal ;m ( -- a )
:m =up     [ t' opUp    half ] literal ;m ( -- a )
:m dup opDup ;m ( -- : compile opDup into the dictionary )
:m drop opDrop ;m ( -- : compile opDrop into the dictionary )
:m over opOver ;m ( -- : compile opOver into the dictionary )
:m swap opSwap ;m ( -- : compile opSwap into the dictionary )
:m >r opToR ;m ( -- : compile opTorR into the dictionary )
:m r> opFromR ;m ( -- : compile opFromR into the dictionary )
:m 0> op0> ;m ( -- : compile op0> into the dictionary )
:m 0= op0= ;m ( -- : compile op0= into the dictionary )
:m 0< op0< ;m ( -- : compile op0< into the dictionary )
:m < op< ;m ( -- : compile op< into the dictionary )
:m > op> ;m ( -- : compile op> into the dictionary )
:m or opOr ;m ( -- : compile opOr into the dictionary )
:m xor opXor ;m ( -- : compile opXor into the dictionary )
:m and opAnd ;m ( -- : compile opAnd into the dictionary )
:m exit opExit ;m ( -- : compile opExit into the dictionary )
:m 2/ op2/ ;m ( -- : compile op2/ into the dictionary )
:s #0 0 lit ;s ( -- 0 : push the number zero onto the stack )
:s #1 1 lit ;s ( -- 1 : push one onto the stack )
:s #-1 -1 lit ;s ( -- -1 : push negative one onto the stack )
:to 1+ 1+ ; ( n -- n : increment a number by one )
:to 1- 1- ; ( n -- n : decrement a number by one )
:to + + ; ( n n -- n : addition )
:to - - ; ( n1 n2 -- n : subtract n2 from n1 )
:to invert invert ; ( u -- u : bitwise logical negate )
:to bye bye ; ( -- : halt the system )
:to dup dup ; ( n -- n n : duplicate top of variable stack )
:to drop opDrop ; ( n -- : drop top of variable stack )
:to over opOver ; ( x y -- x y x : get over it! )
:to swap opSwap ; ( x y -- y x : swap two variables on stack )
:to rshift rshift ; ( u n -- u : logical right shift by "n" )
:so [@] [@] ;s ( vma -- : fetch -VM Address- )
:so [!] [!] ;s ( u vma -- : store to -VM Address- )
:so lsb lsb ;s ( u -- f : get least significant bit )
:to sp@ sp@ ; ( -- a : get the variable stack location )
:to sp! sp! ; ( a -- ??? : set the variable stack location )
:to 0> op0> ; ( n -- f : signed greater than zero )
:to 0= op0= ; ( n -- f : equal to zero )
:to 0< op0< ; ( n -- f : signed less than zero )
:to < op< ; ( n1 n2 -- f : signed less than )
:to > op> ; ( n1 n2 -- f : signed greater than )
:to 2/ op2/ ; ( u -- u : div by two, equivalent to 1 rshift )
:to or opOr ; ( u u -- u : bitwise or )
:to xor opXor ; ( u u -- u : bitwise xor )
:to and opAnd ; ( u u -- u : bitwise and )
:so pause pause ;s ( -- : pause current task, task switch )
: nop ; ( -- : do nothing! )
: @ 2/ [@] ; ( a -- u : fetch a cell to a memory location )
: ! 2/ [!] ; ( u a -- : write a cell to a memory location )
: <ok> {ok} up ; ( -- a )
:s <emit> {emit} up ;s ( -- a )
:s <key>  {key} up ;s ( -- a )
:s <echo> {echo} up ;s ( -- a )
:s <literal> {literal} up ;s ( -- a )
:s <tap> {tap} up ;s ( -- a )
:s <expect> {expect} up ;s ( -- a )
:s <error> {error} up ;s ( -- a )
:s <cold> {cold} lit ;s ( -- a )
: current {current} lit ; ( -- a : get current vocabulary )
: root-voc {root-voc} lit ; ( -- a : get root vocabulary )
: this 0 up ; ( -- a : address of task thread memory )
: pad this 3C0 lit + ; ( -- a : index into pad area )
: #vocs 8 lit ; ( -- u : number of vocabularies )
: context {context} lit ; ( -- a )
: here h lit @ ; ( -- u : push the dictionary pointer )
: base {base} up ; ( -- a : push the radix for numeric I/O )
: dpl {dpl} up ; ( -- a : decimal point variable )
: hld {hld} up ; ( -- a : index to hold space for num. I/O)
: state {state} up ; ( -- f : interpreter state )
:s calibration {ms} lit ;s ( -- a : "ms" calibration var )
: blk {blk} lit ; ( -- a : latest loaded block )
: scr {scr} lit ; ( -- a : last view block )
: >in {in} up ; ( -- a : input buffer position var )
: span {span} up ; ( -- u : number of chars saved by expect )
: bl 20 lit ; ( -- 32 : push space character )
: cycles {cycles} lit ; ( -- a : number of "cycles" ran for )
: hex  10 lit base ! ; ( -- : change to hexadecimal base )
: decimal A lit base ! ; ( -- : change to decimal base )
: ]
  #-1 state ! ; ( -- : return to compile mode )
: [ #0
  state ! ; immediate ( -- : initiate command mode )
:s many #0 >in ! ;s ( -- : repeat current line )
: nip swap drop ; ( x y -- y : remove second item on stack )
: tuck swap over ; ( x y -- y x y : save item for rainy day )
: ?dup dup if dup then ; ( x -- x x | 0 : conditional dup )
: rot >r swap r> swap ; ( x y z -- y z x : "rotate" stack )
: -rot rot rot ; ( x y z -- z x y : "rotate" stack backwards )
: 2drop drop drop ; ( x x -- : drop it like it is hot )
: 2dup  over over ; ( x y -- x y x y )
: 0<= 0> 0= ; ( n -- f )
: 0<> 0= 0= ; ( n -- f : not equal to zero )
: = - 0= ; ( u1 u2 -- f : equality )
: <> = 0= ; ( u1 u2 -- f : inequality )
: >= < 0= ; ( u1 u2 -- f : greater than or equal to )
: <= > 0= ; ( u1 u2 -- f : less than or equal to )
: 0>= 0< 0= ; ( u1 u2 -- f )
: u< 2dup 0< 0= swap 0< 0= <> >r < r> <> ; ( u1 u2 -- f )
: u> swap u< ; ( u1 u2 -- f )
: u>= u< 0= ; ( u1 u2 -- f )
: u<= u> 0= ; ( u1 u2 -- f )
: negate 1- invert ; ( n -- n )
: s>d dup 0< ; ( n -- d )
: abs s>d if negate then ; ( n -- u )
: 2* op2* ; ( u -- u )
: cell 2 lit ; ( -- u )
: cell+ cell + ; ( a -- a )
: cells op2* ; ( u -- u )
: execute 2/ >r ; ( xt -- )
: key? opKey s>d ( -- c 0 | -1 : get single byte of input )
   if
     {options} lit @ 8 lit and if bye then drop #0 exit
   then #-1 ;
: key begin pause <key> @ execute until ; ( -- c )
: emit pause <emit> @ execute ; ( c -- : output byte )
: cr =cr lit emit =lf lit emit ; ( -- : emit new line )
: get-current current @ ; ( -- wid )
: set-current current ! ; ( -- wid )
:s last get-current @ ;s ( -- wid )
: pick sp@ + [@] ; ( nu...n0 u -- nu )
: +! 2/ tuck [@] + swap [!] ; ( u a -- )
: lshift begin ?dup while 1- swap 2* swap repeat ; ( n u -- n )
: c@ dup @ swap lsb if 8 lit rshift else FF lit and then ;
: c!  swap FF lit and dup 8 lit lshift or swap ( c a -- )
   tuck dup @ swap lsb 0= FF lit xor
   >r over xor r> and xor swap ! ;
: max 2dup < if nip else drop then ; ( n1 n2 -- n )
: min 2dup > if nip else drop then ; ( n1 n2 -- n )
: source-id {id} up @ ; ( -- u : input type )
: 2! tuck ! cell+ ! ; ( u1 u2 a -- )
: 2@ dup cell+ @ swap @ ; ( a -- u1 u2 )
: 2>r r> swap >r swap >r >r ; compile-only ( n n --,R: -- n n )
: 2r> r> r> swap r> swap >r ; compile-only ( -- n n,R: n n -- )
: tup {tib} up ; ( -- a )
: source tup 2@ ; ( -- a u : get terminal input source )
: aligned dup lsb 0<> #1 and + ; ( u -- u : align up ptr. )
: align here aligned h lit ! ; ( -- : align up dict. ptr. )
: allot h lit +! ; ( n -- : take dictionary space )
: , align here ! cell allot ;
: count dup 1+ swap c@ ; ( b -- b c )
: +string #1 over min rot over + rot rot - ; ( b u -- b u )
: type ( a u -- : print out a string )
  begin dup while swap count emit swap 1- repeat 2drop ;
: cmove ( b1 b2 u -- )
   for aft >r dup c@ r@ c! 1+ r> 1+ then next 2drop ;
: fill ( b u c -- )
   swap for swap aft 2dup c! 1+ then next 2drop ;
: erase #0 fill ; ( NB. blank is bl fill )
:s do$ r> r> 2* dup count + aligned 2/ >r swap >r ;s ( -- a  )
:s ($) do$ ;s           ( -- a : do string NB. )
:s .$ do$ count type ;s ( -- : print string in next cells )
:m ." .$ $literal ;m ( --, ccc" : compile string )
:m $" ($) $literal ;m ( --, ccc" : compile string )
: space bl emit ; ( -- : emit a space )
: catch        ( xt -- exception# | 0 \ return addr on stack )
   sp@ >r                ( xt )   \ save data stack pointer
   {handler} up @ >r     ( xt )   \ and previous handler
   rp@ {handler} up !    ( xt )   \ set current handler
   execute               ( )      \ execute returns if no throw
   r> {handler} up !     ( )      \ restore previous handler
   rdrop                 ( )      \ discard saved stack ptr
   #0 ;                 ( 0 )     \ normal completion
: throw ( ??? exception# -- ??? exception# )
    ?dup if              ( exc# )     \ 0 throw is no-op
      {handler} up @ rp! ( exc# )     \ restore prev ret. stack
      r> {handler} up !  ( exc# )     \ restore prev handler
      r> swap >r         ( saved-sp ) \ exc# on return stack
      sp! drop r>        ( exc# )     \ restore stack
    then ;
: abort #-1 throw ; ( -- : Time to die. )
:s (abort) do$ swap if count type abort then drop ;s ( n -- )
:s depth {sp0} lit @ sp@ - 1- ;s ( -- n )
:s ?depth depth >= if -4 lit throw then ;s ( ??? n -- )
: um+ 2dup + >r r@ #0 >= >r ( u u -- u carry )
   2dup and 0< r> or >r or 0< r> and invert 1+ r> swap ;
: dnegate invert >r invert #1 um+ r> + ; ( d -- d )
: d+ >r swap >r um+ r> + r> + ;         ( d d -- d )
: um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) F lit
  for
    dup um+ >r >r dup um+ r> + r>
    if >r over um+ r> + then
  next rot drop ;
: * um* drop ;
: um/mod ( ud u -- ur uq : unsigned double cell div/mod )
  ?dup 0= if -A lit throw then
  2dup u<
  if negate F lit
    for >r dup um+ >r >r dup um+ r> + dup
      r> r@ swap >r um+ r> ( or -> ) 0<> swap 0<> +
      if >r drop 1+ r> else drop then r>
    next
    drop swap exit
  then 2drop drop #-1 dup ;
: m/mod ( d n -- r q : floored division )
  s>d dup >r
  if
    negate >r dnegate r>
  then
  >r s>d if r@ + then r> um/mod r>
  if swap negate swap exit then ;
: /mod over 0< swap m/mod ; ( u1 u2 -- u1%u2 u1/u2 )
: mod  /mod drop ; ( u1 u2 -- u1%u2 )
: /    /mod nip ; ( u1 u2 -- u1/u2 )
:s (emit) opEmit ;s ( c -- : output byte to terminal )
: echo <echo> @ execute ; ( c -- : emit a single character )
:s tap dup echo over c! 1+ ;s ( bot eot cur c -- bot eot cur )
:s ktap ( bot eot cur c -- bot eot cur )
  dup dup =cr lit <> >r  =lf lit <> r> and if ( Not EOL? )
    dup =bksp lit <> >r =del lit <> r> and if ( Not Del Char? )
      bl tap ( replace any other character with bl )
      exit
    then
    >r over r@ < dup if             ( if not at start of line )
      =bksp lit dup echo bl echo echo ( erase char )
    then
    r> + ( add 0/-1 to cur )
    exit
  then drop nip dup ;s ( set cur = eot )
: accept ( b u -- b u : read in a line of user input )
  over + over begin
    2dup <>
  while
    key dup bl - 5F lit u< if tap else <tap> @ execute then
  repeat drop over - ;
: expect <expect> @ execute span ! drop ; ( a u -- )
: tib source drop ; ( -- b )
: query tib =buf lit <expect> @ execute tup ! drop #0 >in ! ;
: -trailing for aft ( b u -- b u : remove trailing spaces )
     bl over r@ + c@ < if r> 1+ exit then
   then next #0 ;
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
: parse ( c -- b u ; <string> )
  >r tib >in @ + tup @ >in @ - r@ ( get memory to parse )
  >r over r> swap >r >r
  r@ t' unmatch lit look 2dup ( look for start of match )
  r> t' match   lit look swap ( look for end of match )
    r> - >r - r> 1+ ( b u c -- b u delta : compute match len )
  >in +!
  r> bl = if -trailing then
  #0 max ;
:s banner ( +n c -- )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ;s
: hold ( c -- : save character in hold space )
  #-1 hld +! hld @ c! ;
: #> 2drop hld @ this =num lit + over - ; ( u -- b u )
:s extract ( ud ud -- ud u : extract digit from number )
  dup >r um/mod r> swap >r um/mod r> rot ;s
:s digit 9 lit over < 7 lit and + [char] 0 + ;s ( u -- c )
: #  2 lit ?depth #0 base @ extract digit hold ; ( d -- d )
: #s begin # 2dup ( d0= -> ) or 0= until ;       ( d -- 0 )
: <# this =num lit + hld ! ;                     ( -- )
: sign 0< if [char] - hold then ;                ( n -- )
: u.r >r #0 <# #s #>  r> over - bl banner type ; ( u r -- )
: u.     #0 <# #s #> space type ; ( u -- )
:s (.) abs base @ opDivMod ?dup if (.) then digit emit ;s
: . space dup 0< if [char] - emit then (.) ;
: >number ( ud b u -- ud b u : convert string to number )
  begin
    2dup >r >r drop c@ base @        ( get next character )
    ( digit? -> ) >r [char] 0 - 9 lit over <
    if 7 lit - dup A lit < or then dup r> u< ( c base -- u f )
    0= if                            ( d char )
      drop                           ( d char -- d )
      r> r>                          ( restore string )
      exit                           ( finished...exit )
    then                             ( d char )
    swap base @ um* drop rot base @ um* d+ ( accumulate digit )
    r> r>                            ( restore string )
    +string dup 0=                   ( advance, test for end )
  until ;
: number? ( a u -- d -1 | a u 0 : easier to use than >number )
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
  2drop r> if dnegate then r> base ! #-1 ;
: .s depth for aft r@ pick . then next ; ( -- : show stack )
: compare ( a1 u1 a2 u2 -- n : string equality )
  rot
  over - ?dup if >r 2drop r> nip exit then
  for ( a1 a2 )
    aft
      count rot count rot - ?dup
      if rdrop nip nip exit then
    then
  next 2drop #0 ;
: nfa cell+ ; ( pwd -- nfa : move word ptr to name field )
: cfa ( pwd -- cfa : move to Code Field Address )
  nfa dup c@ 1F lit and + cell+ cell negate and ;
:s (search) ( a wid -- PWD PWD 1 | PWD PWD -1 | 0 a 0 )
  \ Search for word "a" in "wid"
  swap >r dup
  begin
    dup
  while
    dup nfa count 9F lit ( $1F:word-length + $80:hidden )
    and r@ count compare 0=
    if ( found! )
      rdrop
      dup ( immediate? -> ) nfa 40 lit swap @ and 0<>
      #1 or negate exit
    then
    nip dup @
  repeat
  rdrop 2drop #0 ;s
:s (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word )
  >r
  context
  begin
    dup @
  while
    dup @ @ r@ swap (search) ?dup
    if
      >r rot drop r> rdrop exit
    then
    cell+
  repeat drop #0 r> #0 ;s
: search-wordlist ( a wid -- PWD 1|PWD -1|a 0 )
   (search) rot drop ;
: find ( a -- pwd 1 | pwd -1 | a 0 : find word in dictionary )
  (find) rot drop ;
:s (literal) state @ if =push lit , , then ;s
: literal <literal> @ execute ; immediate ( u -- )
: compile, 2/ align , ;  ( xt -- )
:s ?found if exit then ( b f -- b | ??? )
   space count type [char] ? emit cr -D lit throw ;s
: interpret ( b -- )
  find ?dup if
    state @
    if
      0> if cfa execute exit then \ <- execute immediate words
      cfa compile, exit \ <- compiling word are...compiled.
    then
    drop
    dup nfa c@ 20 lit and if -E lit throw then ( <- ?compile )
    \ if it's not compiling, execute it then exit *interpreter*
    cfa execute exit
  then
  \ not a word
  dup >r count number? if rdrop \ it is numeric!
    dpl @ 0< if \ <- dpl is -1 if it's a single cell number
       drop     \ drop high cell from 'number?' for single cell
    else        \ <- dpl is not -1, it is a double cell number
       state @ if swap then
       postpone literal \ literal executed twice if # is double
    then \ NB. "literal" is state aware
    postpone literal exit
  then
  \ NB. Could vector ?found here, to handle arbitrary words
  r> #0 ?found ;
: get-order ( -- widn...wid1 n : get current search order )
  context
   \ next line finds first empty cell
   #0 >r begin dup @ r@ xor while cell+ repeat rdrop
  dup cell - swap
  context - 2/ dup >r 1- s>d if -50 lit throw then
  for aft dup @ swap cell - then next @ r> ;
:r set-order ( widn ... wid1 n -- : set current search order )
  \ NB. Uses recursion, however the meta-compiler does not use
  \ the Forth compilation mechanism, so the current definition
  \ of "set-order" is available immediately.
  dup #-1 = if drop root-voc #1 set-order exit then
  dup #vocs > if -49 lit throw then
  context swap for aft tuck ! cell+ then next #0 swap ! ;r
:r forth-wordlist {forth-wordlist} lit ;r ( -- wid )
:r system {system} lit ;r ( -- wid )
:r forth root-voc forth-wordlist 2 lit set-order ;r ( -- )
:r only #-1 set-order ;r ( -- : set minimal search order )
:s .id ( pwd -- : print word )
  nfa count 1F lit and type space ;s
:r words ( -- )
  cr get-order begin ?dup while swap ( dup u. ." : " ) @
  begin ?dup
  while dup nfa c@ 80 lit and 0= if dup .id then @
  repeat ( cr )
  1- repeat ;r
: definitions context @ set-current ; ( -- )
: word ( -- b )
  #1 ?depth parse here aligned dup >r 2dup ! 1+ swap cmove r> ;
:s ?unique ( a -- a : warn if word definition is not unique )
 dup get-current (search) 0= if exit then space
 2drop {last} lit @ .id ." redefined" cr ;s ( b -- b )
:s ?nul dup c@ if exit then -10 lit throw ;s ( b -- b )
:s ?len dup c@ 1F lit > if -13 lit throw then ;s ( b -- b )
:to char bl word ?nul count drop c@ ; ( "name", -- c )
:to [char] postpone char =push lit , , ; immediate
:to ;
  BABE lit <> if -16 lit throw then ( check compile safety )
  =unnest lit ,                     ( compile exit )
  postpone [                        ( back to command mode )
  ?dup if                           ( link word in if non 0 )
    get-current ! exit              ( this inks the word in )
  then ; immediate compile-only
:to :   ( "name", -- colon-sys )
  align                 ( must be aligned before hand )
  here dup              ( push location for ";" )
  {last} lit !          ( set last defined word )
  last ,                ( point to previous word in header )
  bl word ?nul ?len ?unique ( parse word and do basic checks )
  count + h lit ! align ( skip over packed word and align )
  BABE lit              ( push constant for compiler safety )
  postpone ] ;          ( turn compile mode on )
:to :noname align here #0 BABE lit ] ; ( "name", -- xt )
:to ' bl word find ?found cfa literal ; immediate
: compile r> dup [@] , 1+ >r ; compile-only ( -- )
:to recurse {last} lit @ cfa compile, ; immediate compile-only
:s toggle tuck @ xor swap ! ;s ( u a -- : toggle bits at addr )
:s hide bl word find ?found nfa 80 lit swap toggle ;s
:to begin align here ; immediate compile-only
:to until =jumpz lit , 2/ , ; immediate compile-only
:to again =jump  lit , 2/ , ; immediate compile-only
:to if =jumpz lit , here #0 , ; immediate compile-only
:to then align here 2/ swap ! ; immediate compile-only
:to while postpone if ; immediate compile-only
:to repeat swap postpone again postpone then ;
    immediate compile-only
:to else =jump lit , here #0 , swap postpone then ;
    immediate compile-only
:to for =>r lit , here ; immediate compile-only
:to aft drop =jump lit , here #0 , here swap ;
    immediate compile-only
:to next =next lit , 2/ , ; immediate compile-only
:s (var) r> 2* ;s compile-only ( R: a --, -- a )
:s (const) r> [@] ;s compile-only ( R: a --, -- u )
:s (marker) r> 2* dup @ h lit ! cell+ @ get-current ! ;s
   compile-only
: create postpone : drop postpone [ compile (var)
   get-current ! ;
:to variable create #0 , ;
:to constant create cell negate allot compile (const) , ;
: >body cell+ ; ( a -- a : move to a create words body )
:s (does) r> r> 2* swap >r ;s compile-only
:s (comp)
  r> {last} lit @ cfa
  ( check we are running does> on a created word )
  dup @ to' (var) half lit <> if -1F lit throw then
  ! ;s compile-only
: does> compile (comp) compile (does) ;
   immediate compile-only
:to marker last align here create cell negate allot compile
    (marker) , , ; ( --, "name" )
:to rp! compile rp! ; immediate compile-only
:to rp@ compile rp@ ; immediate compile-only
:to >r compile opToR ; immediate compile-only
:to r> compile opFromR ; immediate compile-only
:to r@ compile r@ ; immediate compile-only
:to rdrop compile rdrop ; immediate compile-only
:to exit compile opExit ; immediate compile-only
:to ." compile .$
  [char] " word count + h lit ! align ; immediate compile-only
:to $" compile ($)
  [char] " word count + h lit ! align ; immediate compile-only
:to abort" compile (abort)
  [char] " word count + h lit ! align ; immediate compile-only
:to ( [char] ) parse 2drop ; immediate ( c"xxx" -- )
:to .( [char] ) parse type ; immediate ( c"xxx" -- )
:to ) ; immediate ( -- )
:to \ tib @ >in ! ; immediate ( c"xxx" -- )
:to postpone bl word find ?found cfa compile, ; immediate
:to immediate last nfa @ 40 lit or last nfa ! ; ( -- )
:to compile-only last nfa @ 20 lit or last nfa ! ; ( -- )
:to see bl word find ?found cr ( --, "name" : decompile  word )
  begin dup @ =unnest lit <>
  while dup @ . cell+ here over < if drop exit then
  repeat @ u. ;
:to dump aligned ( a u -- : display section of memory )
  begin ?dup
  while swap dup @ . cell+ swap cell -
  repeat drop ;
:s cksum aligned dup C0DE lit - >r ( a u -- u )
  begin ?dup
  while swap dup @ r> + >r cell+ swap cell -
  repeat drop r> ;s
: defined bl word find nip 0<> ; ( -- f )
:to [then] ; immediate ( -- )
:to [else]
 begin
  begin bl word dup c@ while
   find drop cfa dup to' [else] lit = swap to' [then] lit = or
    if exit then repeat query drop again ; immediate
:to [if] if exit then postpone [else] ; immediate
: ms for pause calibration @ for next next ; ( ms -- )
: bell 7 lit emit ; ( -- : emit ASCII BEL character )
:s csi 1B lit emit 5B lit emit ;s ( -- : ANSI Term. Esc. Seq. )
: page csi ." 2J" csi ." 1;1H" ( csi ." 0m" ) ;
: at-xy base @ decimal ( x y -- : set cursor position )
   >r csi #0 u.r ." ;" #0 u.r ." H" r>
   base ! ;
: b/buf 400 lit ; ( -- u )
: block #1 ?depth dup blk ! A lit lshift pause ; ( k -- u )
: flush ( save-buffers empty-buffers ) ; ( -- )
: update #-1 {dirty} lit ! ; ( -- )
: blank bl fill ; ( a u -- : blank an area of memory )
: list ( k -- : list a block )
   page cr         ( clean the screen )
   dup scr ! block ( update "scr" and load block )
   F lit for       ( for each line in the block )
     F lit r@ - 3 lit u.r space    ( print the line number )
     3F lit for count emit next cr ( print line )
   next drop ;
: get-input source >in @ source-id <ok> @ ; ( -- n1...n5 )
: set-input <ok> ! {id} up ! >in ! tup 2! ; ( n1...n5 -- )
:s ok state @ 0= if ."  ok" cr then ;s ( -- )
:s eval
   begin bl word dup c@ while
     interpret #0 ?depth
   repeat drop <ok> @ execute ;s ( "word" -- )
: evaluate ( a u -- : evaluate a string )
  get-input 2>r 2>r >r        ( save the current input state )
  #0 #-1 t' nop lit set-input ( set new input )
  t' eval lit catch           ( evaluate the string )
  r> 2r> 2r> set-input        ( restore input state )
  throw ;                     ( throw on error )
:s line 6 lit lshift swap block + 40 lit ;s ( k l -- a u )
:s loadline line evaluate ;s ( k l -- ??? : execute a line! )
: load #0 F lit for
   2dup 2>r loadline 2r> 1+ next 2drop ; ( k -- : exec blk )
:r eforth 0108 lit ;r ( --, version )
 \ :s info cr ( --, print system info )
 \  ." Project: eForth v1.8"  here . check lit @ . cr
 \  ." Author:  Richard James Howe" cr
 \  ." Email:   howe.r.j.89@gmail.com" cr
 \  ." Repo:    https://github.com/howerj/subleq" cr
 \  ." License: The Unlicense / Public Domain" cr ;s
:s xio t' accept lit <expect> ! <tap> ! <echo> ! <ok> ! ;s
:s hand t' ok lit
    t' (emit) lit ( Default: echo on )
    {options} lit @ lsb if drop to' drop lit then
    t' ktap lit postpone [ xio ;s ( -- )
:s pace B lit emit ;s ( -- : emit pacing character )
:s file t' pace lit to' drop lit t' ktap lit xio ;s ( -- )
:s console t' key? lit <key> ! t' (emit) lit <emit> ! hand ;s
:s io! console ;s ( -- : setup system I/O )
:s task-init ( task-addr -- : initialize USER task )
  {up} lit @ swap {up} lit !
  this 2/ {next-task} up !
  to' bye lit 2/ {ip-save} up ! ( Default execution token )
  this =stksz        lit + 2/ {rp-save} up !
  this =stksz double lit + 2/ {sp-save} up !
  #0 {tos-save} up !
  decimal
  io!
  t' (literal) lit <literal> !
  to' bye lit <error> !
  #0 >in ! #-1 dpl !
  this =tib lit + #0 tup 2! \ Set terminal input buffer loc.
  {up} lit ! ;s
:s ini {up} lit @ task-init ;s ( -- : initialize current task )
:s (error) ( u -- : quit loop error handler )
   dup space . [char] ? emit cr #-1 = if bye then
   ini t' (error) lit <error> ! ;s
: quit ( -- : interpreter loop )
  t' (error) lit <error> !         ( set error handler )
  begin                            ( infinite loop start... )
   query t' eval lit catch         ( evaluate a line )
   ?dup if <error> @ execute then  ( error? )
  again ;                          ( do it all again... )
:s (cold) ( -- : Forth boot sequence )
  only forth definitions ( un-mess-up dictionary / set it )
  ini ( initialize the current thread correctly )
  {options} lit @ 4 lit and if ( info ) then ( display info? )
  {options} lit @ 2 lit and if ( checksum on? )
    primitive lit @ 2* dup here swap - cksum  ( calc. cksum )
    check lit @ <> if ." cksum fail" bye then ( oops... )
    {options} lit @ 2 lit xor {options} lit ! ( disable cksum )
  then quit ;s ( call the interpreter loop AKA "quit" )
:s task: ( create a named task )
  create here 400 lit allot 2/ task-init ;s
:s activate ( xt task-address -- : start task executing xt )
  dup task-init
  dup >r swap 2/ swap {ip-save} lit + ! ( set execution word )
  r> this @ >r dup 2/ this ! r> swap ! ;s ( link in task )
:s wait ( addr -- : wait for signal )
  begin pause dup @ until #0 swap ! ;s
:s signal this swap ! ;s ( addr -- : signal to wait )
:s single #1 {single} lit ! ;s ( -- : disable other tasks )
:s multi  #0 {single} lit ! ;s ( -- : enable multitasking )
:s send ( msg task-addr -- : send message to task )
  this over {sender} lit +
  begin pause dup @ 0= until
  ! {message} lit + ! ;s
:s receive ( -- msg task-addr : block until message )
  begin pause {sender} up @ until
  {message} up @ {sender} up @
  #0 {sender} up ! ;s
: editor {editor} lit #1 set-order ; ( Micro BLOCK editor )
:e q only forth ;e ( -- : exit back to Forth interpreter )
:e ? scr @ . ;e ( -- : print block number of current block )
:e l scr @ list ;e ( -- : list current block )
:e e q scr @ load editor ;e ( -- : evaluate current block )
:e ia 2 lit ?depth 6 lit lshift + scr @ block + tib >in @ +
   swap source nip >in @ - cmove tib @ >in ! update l ;e
:e i #0 swap ia ;e ( line --, "line" : insert line at )
:e w words ;e ( -- : display block editor commands )
:e s update flush ;e ( -- : save edited block )
:e n  #1 scr +! l ;e ( -- : display next block )
:e p #-1 scr +! l ;e ( -- : display previous block )
:e r scr ! l ;e ( k -- : retrieve given block )
:e x scr @ block b/buf blank l ;e ( -- : erase current block )
:e d #1 ?depth >r scr @ block r> 6 lit lshift + 40 lit
   blank l ;e ( line -- : delete line )
: cold {cold} lit 2* @ execute ; ( -- )
t' (cold) half {cold} t!      \ Set starting Forth word
atlast {forth-wordlist} t!    \ Make wordlist work
{forth-wordlist} {current} t! \ Set "current" dictionary
there h t!                    \ Assign dictionary pointer
primitive t@ double mkck check t! \ Set checksum over VM
atlast {last} t!              \ Set last defined word
save-target                   \ Output target
.end                          \ Get back to normal Forth
bye                           \ Auf Wiedersehen
