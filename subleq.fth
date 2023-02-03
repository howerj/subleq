defined eforth [if] ' ) <ok> ! [then] ( Turn off ok prompt )
\ Project: Cross Compiler / eForth interpreter for a SUBLEQ CPU
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
\ - <https://www.bradrodriguez.com/papers/mtasking.html>,
\   For multitasking support
\ - <https://forth-standard.org/standard/block>,
\   For the block word-set, which is partially implemented.
\
\ The way this cross compiler works is the following:
\
\ 1. An assembler for the SUBLEQ machine is made.
\ 2. A virtual machine is made with that assembler that can 
\    support higher level programming constructs, specifically, 
\    the easy execution of Forth.
\ 3. Forth word definitions are built up, which are then used 
\    to construct a full Forth interpreter.
\ 4. The resulting image is output to standard out.
\
\ Check the references for more detailed examples for other 
\ systems. Some keywords that will help are; Forth, 
\ Meta-compilation, Cross compiler, eForth, 8086 eForth, 
\ JonesForth, j1eforth.
\
\ Notes:
\
\ - If "see", the decompiler, was advanced enough we could 
\ dispense with the source code, an interesting concept.
\ - The eForth image could determine the SUBLEQ machine size, 
\ and adjust itself accordingly, it would not even require a 
\ power-of-two integer width. Another interesting concept would
\ be to adapt this eForth to a SUBLEQ machine that used bignums
\ for each cell, this would require re-engineering functions 
\ like bitwise AND/OR/XOR as they require a fixed cell width to 
\ work efficiently.
\ - The eForth image could be compressed with LZSS to save on 
\ space. If the de-compressor was written in pure SUBLEQ it 
\ would compression of most of the image instead of just the 
\ eForth section of it.
\ - A website with an interactive simulator is available at:
\   <https://github.com/howerj/subleq-js>
\ - It would be nice to make a 7400 Integrated Circuit board 
\ that could run and execute this code, or a project in VHDL 
\ for an FPGA that could do it.
\ - The virtual machine could be sped up with optimization 
\ magic
\ - Half of the memory used is just for the virtual machine 
\ that allows Forth to be written.
\ - The BLOCK word-set does not use mass storage, but maps 
\ blocks to memory, if a mass storage peripheral were to be 
\ added these functions would have to be modified. It might be 
\ nice to make a Forth File System based on blocks as well, 
\ then this system could act like a primitive DOS.
\ - Reformatting the text for a 64 byte line width would allow 
\ storage in Forth blocks. This file could then perhaps we 
\ stored within the image.
\ - Much like my Embed VM project and Forth interpreter, 
\ available at \ <https://github.com/howerj/embed>, this file 
\ could be documented extensively and explain how to build up 
\ a Forth interpreter from scratch.
\
only forth definitions hex
defined (order) 0= [if]
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
[then]
defined eforth [if]
  : wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )
[then]
wordlist constant meta.1        ( meta-compiler word set )
wordlist constant target.1      ( target eForth word set )
wordlist constant assembler.1   ( assembler word set )
wordlist constant target.only.1 ( target only word set )
defined eforth [if] system +order [then]
meta.1 +order definitions
   2 constant =cell   \ Target cell size
4000 constant size    \ Size of image working area
 100 constant =buf    \ Size of text input buffers in target
 100 constant =stksz  \ Size of return and variable stacks
FC00 constant =thread \ Initial start of thread area
0008 constant =bksp   \ Backspace character value
000A constant =lf     \ Line feed character value
000D constant =cr     \ Carriage Return character value
007F constant =del    \ Delete character
create tflash tflash size cells allot size erase
variable tdp 0 tdp ! ( target dictionary pointer )
variable tlast 0 tlast ! ( last defined target word pointer )
variable tlocal 0 tlocal ! ( local variable allocator )
variable voc-last 0 voc-last ! ( last defined in any vocab )
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
:m mdrop drop ;m ( u -- : always call drop )
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
:m #dec s>d if [char] - emit then (.) ;m ( n16 -- )
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
:m local? tlocal @ ;m
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
:m compile-only voc-last @ tnfa t@ 20 or voc-last @ tnfa t! ;m 
:m immediate   voc-last @ tnfa t@ 40 or voc-last @ tnfa t! ;m 
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
:m tcompile to' half t, ;m
:m >tbody =cell + ;m
:m tcksum taligned dup C0DE - FFFF and >r
   begin ?dup
   while swap dup t@ r> + FFFF and >r =cell + swap =cell -
   repeat drop r> ;m ( a u -- u : compute a checksum )
:m mkck dup there swap - tcksum ;m ( -- u : checksum of image )
:m postpone ( --, "name" )
   target.only.1 +order t' target.only.1 -order 2/ t, ;m
:m thead talign there tlast @ t, dup tlast ! voc-last !
   parse-word talign tpack talign ;m ( --, "name" )
:m header >in @ thead >in ! ;m ( --, "name" )
:m :ht ( "name" -- : forth routine, no header )
  get-current >r target.1 set-current create
  r> set-current CAFE talign there ,
  does> @ 2/ t, ;m
:m :t header :ht ;m ( "name" -- : forth routine )
:m :to ( "name" -- : forth, target only routine )
  header
  get-current >r
    target.only.1 set-current create
  r> set-current
  CAFE talign there ,
  does> @ 2/ t, ;m
:m :a ( "name" -- : assembly routine, no header )
  1234 target.1 +order definitions
  create talign there , assembler.1 +order does> @ 2/ t, ;m
:m (fall-through); 1234 <>
   if abort" unstructured" then assembler.1 -order ;m
:m (a); (fall-through); ;m
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
  =stksz half tvar stacksz \ must contain $80
 -1 tvar neg1      \ must contain -1
  1 tvar one       \ must contain  1
  2 tvar two       \ must contain  2
 10 tvar bwidth    \ must contain 16
  0 tvar r0        \ working pointer 1 (register r0)
  0 tvar r1        \ register 1
  0 tvar r2        \ register 2
  0 tvar r3        \ register 3
  0 tvar r4        \ register 4
  0 tvar r5        \ register 5
  0 tvar r6        \ register 6
  0 tvar r7        \ register 7
  0 tvar h         \ dictionary pointer
  =thread half tvar {up} \ Current task addr. (Half size)
  0 tvar check     \ used for system checksum
  0 tvar {context} E tallot \ vocabulary context
  0 tvar {current}  \ vocabulary to add new definitions to
  0 tvar {forth-wordlist} \ forth word list (main vocabulary)
  0 tvar {editor}   \ editor vocabulary
  0 tvar {root-voc} \ absolute minimum vocabulary
  0 tvar {system}   \ system functions vocabulary
  0 tvar {cold}     \ entry point of VM program, set later on
  0 tvar {last}     \ last defined word
  0 tvar {cycles}   \ number of times we have switched tasks
  0 tvar {single}   \ is multi processing off?
  0 tvar {user}     \ Number of locals assigned
  \ Thread variables, not all of which are user variables
  0 tvar ip         \ instruction pointer
  0 tvar tos        \ top of stack
  =thread =stksz        + half dup tvar {rp0} tvar {rp}
  =thread =stksz double + half dup tvar {sp0} tvar {sp}
  200 constant =tib \ Start of terminal input buffer
  380 constant =num \ Start of numeric input buffer
  tuser {next-task} \ next task in task list
  tuser {ip-save}   \ saved instruction pointer
  tuser {tos-save}  \ saved top of variable stack
  tuser {rp-save}   \ saved return stack pointer
  tuser {sp-save}   \ saved variable stack pointer
  tuser {handler}   \ throw/catch handler
  tuser {sender}    \ multitasking; msg. send, 0 = no message
  tuser {message}   \ multitasking; the message itself
  tuser {id}        \ executing from block or terminal?
:m INC 2/ neg1 2/ t, t, NADDR ;m ( b -- )
:m DEC 2/ one  2/ t, t, NADDR ;m ( b -- )
:m ONE! dup ZERO INC ; ( a -- : set address to '1' )
:m NG1! dup ZERO DEC ; ( a -- : set address to '-1' )
:m ++sp {sp} DEC ;m ( -- : grow variable stack )
:m --sp {sp} INC ;m ( -- : shrink variable stack )
:m --rp {rp} DEC ;m ( -- : shrink return stack )
:m ++rp {rp} INC ;m ( -- : grow return stack )
:m a-optim drop ;m \ >r there =cell - r> 2/ t! ;m ( a -- )
( Error message string "Error: Not a 16-bit SUBLEQ VM" )
1F tvar err-str
  45 t, 72 t, 72 t, 6F t, 72 t, 3A t, 20 t, 4E t,
  6F t, 74 t, 20 t, 61 t, 20 t, 31 t, 36 t, 2D t,
  62 t, 69 t, 74 t, 20 t, 53 t, 55 t, 42 t, 4C t,
  45 t, 51 t, 20 t, 56 t, 4D t, 0D t, 0A t,
err-str 2/ tvar err-str-addr
assembler.1 +order
label: die
   err-str-addr r1 MOV
   r0 ZERO
   err-str r0 MOV
   begin
     r0
   while
     r0 DEC
     r1 INC
     r4 r1 iLOAD
     r4 PUT
   repeat
   HALT
label: start         \ System Entry Point
  start 2/ entry t!  \ Set the system entry point
  r1 ONE!
  r0 ONE!
label: chk16
  r0 r0 ADD                        \ r0 = r0 * 2
  r1 INC                           \ r1++
  r0 +if chk16 JMP then            \ check if still positive
  bwidth r1 SUB r1 if die JMP then \ r1 - bwidth should be zero
  {sp0} {sp} MOV     \ Setup initial variable stack
  {rp0} {rp} MOV     \ Setup initial return stack
  {cold} ip MOV      \ Get the first instruction to execute
  ( fall-through )
label: vm ( Forth Inner Interpreter )
  ip r0 MOV          \ Move Instruction Pointer To Working Ptr.
  ip INC             \ IP now points to next instruction!
  r1 r0 iLOAD        \ Get actual instruction to execute
  r1 r0 MOV          \ Copy it, as SUB is destructive
  primitive r1 SUB   \ Check if it is a primitive
  r1 -if r0 iJMP then \ Jump straight to VM functions if it is
  ++rp               \ If it wasn't a VM instruction, inc {rp}
  ip {rp} iSTORE     \ and store ip to return stack
  r0 ip MOV vm a-optim \ "w" becomes our next instruction
  vm JMP             \ Ad infinitum...
assembler.1 -order
:m ;a (fall-through); vm a-optim vm JMP ;m
:a bye HALT (a);    ( -- : HALT system )
( :a opPush ++sp tos {sp} iSTORE tos ip iLOAD ip INC ;a )
:a opSwap tos r0 MOV tos {sp} iLOAD r0 {sp} iSTORE ;a
:a opDup ++sp tos {sp} iSTORE ;a ( n -- n n )
:a opFromR ++sp tos {sp} iSTORE tos {rp} iLOAD --rp ;a
:a opToR ++rp tos {rp} iSTORE (fall-through);
:a opDrop tos {sp} iLOAD --sp ;a ( n -- )
:a [@] tos tos iLOAD ;a
:a [!] r0 {sp} iLOAD r0 tos iSTORE --sp t' opDrop JMP (a);
:a opEmit tos PUT t' opDrop JMP (a);
( :a opKey ++sp tos {sp} iSTORE tos GET ;a ) ( -- n )
:a - r0 {sp} iLOAD tos r0 SUB r0 tos MOV --sp ;a ( n n -- n )
:a + r0 {sp} iLOAD r0 tos ADD --sp ;a ( n n -- n )
:a opExit ip {rp} iLOAD (fall-through); ( R: a -- )
:a rdrop --rp ;a ( R: u -- )
:a r@ ++sp tos {sp} iSTORE tos {rp} iLOAD ;a ( R: u --, -- u )
:a rp@ ++sp tos {sp} iSTORE {rp} tos MOV ;a ( R: ???, -- u )
:a rp! tos {rp} MOV t' opDrop JMP (a); ( u -- R: ??? )
:a sp! tos {sp} MOV ;a ( u -- ??? )
:a opJumpZ ( u -- : Conditional jump on zero )
  r2 ZERO
  tos if r2 NG1! then tos DEC tos +if r2 NG1! then
  tos {sp} iLOAD --sp
  r2 if ip INC vm JMP then (fall-through); 
:a opJump ip ip iLOAD ;a ( -- : Unconditional jump )
:a opNext r0 {rp} iLOAD ( R: n -- | n-1 )
   r0 if r0 DEC r0 {rp} iSTORE t' opJump JMP then
   ip INC --rp ;a
:a op0=
   tos r0 MOV tos NG1!
   r0 if tos ZERO then r0 DEC r0 +if tos ZERO then ;a
:a op0> tos +if tos NG1! vm JMP then tos ZERO ;a
:a op0<
   tos r0 MOV tos ZERO
   r0 -if tos NG1! then r0 INC r0 -if tos NG1! then ;a
:a rshift
  bwidth r0 MOV
  tos r0 SUB
  tos {sp} iLOAD --sp
  r1 ZERO
  begin r0 while
    r1 r1 ADD
    tos r2 MOV r3 ZERO
    r2 -if r3 NG1! then r2 INC r2 -if r3 NG1! then
    r3 if r1 INC then
    tos tos ADD
    r0 DEC
  repeat
  r1 tos MOV ;a
:a opMux
  bwidth r0 MOV
  r1 ZERO
  r3 {sp} iLOAD --sp
  r4 {sp} iLOAD --sp
  begin r0 while
    r1 r1 ADD
    tos r5 MOV r6 ZERO
    r5 -if r6 NG1! then r5 INC r5 -if r6 NG1! then
    r6 -if 
      r4 r7 MOV r5 ZERO
      r7 -if r5 ONE! then r7 INC r7 -if r5 ONE! then
      r5 r1 ADD
    then
    r6 INC
    r6 if  
      r3 r7 MOV r5 ZERO
      r7 -if r5 ONE! then r7 INC r7 -if r5 ONE! then
      r5 r1 ADD
    then
    tos tos ADD
    r3 r3 ADD
    r4 r4 ADD
    r0 DEC
  repeat
  r1 tos MOV ;a
:a opDivMod
  r0 {sp} iLOAD
  r2 ZERO
  begin
    r1 ONE!
    r0 -if r1 ZERO then
    r1 
  while
    r2 INC
    tos r0 SUB
  repeat
  tos r0 ADD
  r2 DEC
  r2 tos MOV
  r0 {sp} iSTORE ;a
:a pause
  {single} if vm JMP then \ Do nothing if single-threaded mode
  r0 {up} iLOAD \ load next task pointer from user storage
  r0 if
    {cycles} INC        \ increment "pause" count
    {up} r2 MOV  r2 INC \ load TASK pointer, skip next task loc
      ip r2 iSTORE r2 INC \ save registers to current task
     tos r2 iSTORE r2 INC
    {rp} r2 iSTORE r2 INC
    {sp} r2 iSTORE r2 INC
       r0 {rp0} MOV stacksz {rp0} ADD \ change {rp0} to new loc
   {rp0} {sp0} MOV stacksz {sp0} ADD \ same but for {sp0}
       r0 {up} MOV r0 INC  \ set next task
      ip r0 iLOAD r0 INC \ reverse of save registers
     tos r0 iLOAD r0 INC
    {rp} r0 iLOAD r0 INC
    {sp} r0 iLOAD r0 INC \ we're all golden
  then ;a
there 2/ primitive t!
:m munorder target.only.1 -order talign ;m
:m (;t) 
   CAFE <> if abort" Unstructured" then 
   munorder ;m
:m ;t (;t) opExit ;m
:m :s tlast @ {system} t@ tlast ! F00D :t drop 0 ;m
:m :so  tlast @ {system} t@ tlast ! F00D :to drop 0 ;m
:m ;s drop CAFE ;t F00D <> if abort" unstructured" then
  tlast @ {system} t! tlast ! ;m
:m :r tlast @ {root-voc} t@ tlast ! BEEF :t drop 0 ;m
:m ;r drop CAFE ;t BEEF <> if abort" unstructured" then
  tlast @ {root-voc} t! tlast ! ;m
:m :e tlast @ {editor} t@ tlast ! DEAD :t drop 0 ;m
:m ;e drop CAFE ;t DEAD <> if abort" unstructured" then
  tlast @ {editor} t! tlast ! ;m
:m system[ tlast @ {system} t@ tlast ! BABE ;m
:m ]system BABE <> if abort" unstructured" then
   tlast @ {system} t! tlast ! ;m
:m root[ tlast @ {root-voc} t@ tlast ! D00D ;m
:m ]root D00D <> if abort" unstructured" then
   tlast @ {root-voc} t! tlast ! ;m
:m : :t ;m ( -- ???, "name" : start cross-compilation )
:m ; ;t ;m ( ??? -- : end cross-compilation of a target word )
:m begin talign there ;m ( -- a )
:m until talign opJumpZ 2/ t, ;m  ( a -- )
:m again talign opJump  2/ t, ;m ( a -- )
:m if opJumpZ there 0 t, ;m ( -- a )
:m tmark opJump there 0 t, ;m ( -- a )
:m then there 2/ swap t! ;m ( a -- )
:m else tmark swap then ;m ( a -- a )
:m while if ;m ( -- a )
:m repeat swap again then ;m ( a a -- )
:m aft drop tmark begin swap ;m ( a -- a a )
:m next talign opNext 2/ t, ;m ( a -- )
:m for opToR begin ;m ( -- a )
:m =jump   [ t' opJump  half ] literal ;m ( -- a )
:m =jumpz  [ t' opJumpZ half ] literal ;m ( -- a )
:m =unnest [ t' opExit  half ] literal ;m ( -- a )
:m =>r     [ t' opToR   half ] literal ;m ( -- a )
:m =next   [ t' opNext  half ] literal ;m ( -- a )
:m dup opDup ;m ( -- : compile opDup into the dictionary )
:m drop opDrop ;m ( -- : compile opDrop into the dictionary )
:m swap opSwap ;m ( -- : compile opSwap into the dictionary )
:m >r opToR ;m ( -- : compile opTorR into the dictionary )
:m r> opFromR ;m ( -- : compile opFromR into the dictionary )
:m 0= op0= ;m ( -- : compile op0= into the dictionary )
:m 0< op0< ;m ( -- : compile op0< into the dictionary )
:m 0> op0> ;m ( -- : compile op0> into the dictionary )
:m mux opMux ;m ( -- : compile opMux into the dictionary )
:m exit opExit ;m ( -- : compile opExit into the dictionary )
:to + + ; ( n n -- n : addition )
:to - - ; ( n1 n2 -- n : subtract n2 from n1 )
:to bye bye ; ( -- : halt the system )
:to dup dup ; ( n -- n n : duplicate top of variable stack )
:to drop opDrop ; ( n -- : drop top of variable stack )
:to swap opSwap ; ( x y -- y x : swap two variables on stack )
:to rshift rshift ; ( u n -- u : logical right shift by "n" )
:so [@] [@] ;s ( vma -- : fetch -VM Address- )
:so [!] [!] ;s ( u vma -- : store to -VM Address- )
:to sp! sp! ; ( a -- ??? : set the variable stack location )
:to 0= op0= ; ( n -- f : equal to zero )
:to 0< op0< ; ( n -- f : signed less than zero )
:to 0> op0> ; ( n -- f : signed greater than zero )
:so mux opMux ;s ( u1 u2 sel -- u : bitwise multiplex op. )
:so pause pause ;s ( -- : pause current task, task switch )
: 2* dup + ; ( u -- u : multiply by two )
:s (const) r> [@] ;s compile-only ( R: a --, -- u )
:m constant :t mdrop (const) t, munorder  ;m
system[
 0 constant #0  ( --  0 : push the number zero onto the stack )
 1 constant #1  ( --  1 : push one onto the stack )
-1 constant #-1 ( -- -1 : push negative one onto the stack )
 2 constant #2  ( --  2 : push two onto the stack )
]system
: 1+ #1 + ; ( n -- n : increment value in cell )
: 1- #1 - ; ( n -- n : decrement value in cell )
:s (push) r> dup [@] swap 1+ >r ;s
:m lit (push) t, ;m ( n -- : compile a literal )
:s (up) 
   r> dup [@] {up} half lit [@] + 2* swap 1+ >r ;s
  compile-only
:s (var) r> 2* ;s compile-only ( R: a --, -- a )
:s (user) r> [@] {up} half lit [@] + 2* ;s compile-only
  ( R: a --, -- u )
:m up (up) t, ;m ( n -- : compile user variable )
:m [char] char (push) t, ;m ( --, "name" : compile char )
:m char   char (push) t, ;m ( --, "name" : compile char )
:m variable :t mdrop (var) 0 t, munorder ;m
:m user :t mdrop (user) local? =cell lallot t, munorder ;m
:to ) ; immediate
: over swap dup >r swap r> ; ( n1 n2 -- n1 n2 n1 )
: invert #0 swap - 1- ;           ( u -- u : bitwise invert )
: xor >r dup invert swap r> mux ; ( u u -- u : bitwise xor )
: or over mux ;                   ( u u -- u : bitwise or )
: and #0 swap mux ;               ( u u -- u : bitwise and )
: 2/ #1 rshift ; ( u -- u : divide by two )
: @ 2/ [@] ; ( a -- u : fetch a cell to a memory location )
: ! 2/ [!] ; ( u a -- : write a cell to a memory location )
:s @+ dup @ ;s ( a -- a u : non-destructive load )
user <ok> ( -- a : okay prompt xt loc. )
system[
  user <emit>    ( -- a : emit xt loc. )
  user <key>     ( -- a : key xt loc. )
  user <echo>    ( -- a : echo xt loc. )
  user <literal> ( -- a : literal xt loc. )
  user <tap>     ( -- a : tap xt loc. )
  user <expect>  ( -- a : expect xt loc. )
  user <error>   ( -- a : <error> xt container. )
]system
  
:s <cold> {cold} lit ;s ( -- a : cold xt loc. )
: current {current} lit ; ( -- a : get current vocabulary )
: root-voc {root-voc} lit ; ( -- a : get root vocabulary )
: this 0 up ; ( -- a : address of task thread memory )
: pad this 3C0 lit + ; ( -- a : index into pad area )
: #vocs 8 lit ; ( -- u : number of vocabularies )
: context {context} lit ; ( -- a )
variable blk ( -- a : latest loaded block )
variable scr ( -- a : latest listed block )
2F t' blk >tbody t!
2F t' scr >tbody t!
user base  ( -- a : push the radix for numeric I/O )
user dpl   ( -- a : decimal point variable )
user hld   ( -- a : index to hold space for num. I/O)
user state ( -- f : interpreter state )
user >in   ( -- a : input buffer position var )
user span  ( -- a : number of chars saved by expect )
$20 constant bl ( -- 32 : push space character )
system[
       h constant h?  ( -- a : push the location of dict. ptr )
{cycles} constant cycles ( -- a : number of "cycles" ran for )
    {sp} constant sp ( -- a : address of v.stk ptr. )
  {user} constant user? ( -- a : )
         variable calibration F00 t' calibration >tbody t!
]system
:s radix base @ ;s ( -- u : retrieve base )
: here h? @ ; ( -- u : push the dictionary pointer )
: sp@ sp @ 1+ ; ( -- a : Fetch variable stack pointer )
: hex  $10 lit base ! ; ( -- : change to hexadecimal base )
: decimal $A lit base ! ; ( -- : change to decimal base )
: ] #-1 state ! ; ( -- : return to compile mode )
: [  #0 state ! ; immediate ( -- : initiate command mode )
: nip swap drop ;   ( x y -- y : remove second item on stack )
: tuck swap over ;  ( x y -- y x y : save item for rainy day )
: ?dup dup if dup then ; ( x -- x x | 0 : conditional dup )
: rot >r swap r> swap ; ( x y z -- y z x : "rotate" stack )
: -rot rot rot ; ( x y z -- z x y : "rotate" stack backwards )
: 2drop drop drop ; ( x x -- : drop it like it is hot )
: 2dup  over over ; ( x y -- x y x y )
:s shed rot drop ;s ( x y z -- y z : drop third stack item )
: > - 0> ;    ( n1 n2 -- f : signed greater than )
: < swap > ;  ( n1 n2 -- f : signed less than )
: = - 0= ;    ( u1 u2 -- f : equality )
: <> = 0= ;   ( u1 u2 -- f : inequality )
: 0<> 0= 0= ; ( n -- f : not equal to zero )
: 0<= 0> 0= ; ( n -- f : less than or equal to zero )
: 0>= 0< 0= ; ( n1 n2 -- f : greater or equal to zero )
: >= < 0= ;   ( n1 n2 -- f : greater than or equal to )
: <= > 0= ;   ( n1 n2 -- f : less than or equal to )
: u< 2dup 0>= swap 0>= <> >r < r> <> ; ( u1 u2 -- f )
: u> swap u< ; ( u1 u2 -- f : unsigned greater than )
: u>= u< 0= ; ( u1 u2 -- f )
: u<= u> 0= ; ( u1 u2 -- f )
: within over - >r - r> u< ; ( u lo hi -- f )
: negate 1- invert ; ( n -- n : twos compliment negation )
: s>d dup 0< ; ( n -- d : signed to double width cell )
: abs s>d if negate then ; ( n -- u : absolute value )
: cell #2 ;   ( -- u : push bytes in cells to stack )
: cell+ cell + ; ( a -- a : increment address by cell width )
: cells 2* ;     ( u -- u : multiply # of cells to get bytes )
: execute 2/ >r ; ( xt -- : execute an execution token )
:s @execute ( ?dup 0= ?exit ) @ execute ;s ( xt -- )
: ?exit if rdrop then ; compile-only ( u --, R: -- |??? )
: key? #-1 [@] negate ( -- c 0 | -1 : get byte of input )
   s>d if
     {options} lit @ 8 lit and if bye then drop #0 exit
   then #-1 ;
: key begin pause <key> @execute until ; ( -- c )
: emit pause <emit> @execute ; ( c -- : output byte )
: cr =cr lit emit =lf lit emit ; ( -- : emit new line )
: get-current current @ ; ( -- wid : get definitions vocab. )
: set-current current ! ; ( -- wid : set definitions vocab. )
:s last get-current @ ;s ( -- wid : get last defined word )
: pick sp@ + [@] ; ( nu...n0 u -- nu : pick item on stack )
: +! 2/ tuck [@] + swap [!] ; ( u a -- : add val to cell )
: lshift begin ?dup while 1- swap 2* swap repeat ; ( n u -- n )
: c@ @+ swap #1 and if 8 lit rshift exit then FF lit and ;
: c! swap FF lit and dup 8 lit lshift or swap
   tuck @+ swap #1 and 0= FF lit xor
   >r over xor r> and xor swap ! ; ( c a -- character store )
:s c@+ dup c@ ;s ( b -- b u : non-destructive 'c@' )
: max 2dup > mux ; ( n1 n2 -- n : highest of two numbers )
: min 2dup < mux ; ( n1 n2 -- n : lowest of two numbers )
: source-id {id} up @ ; ( -- u : input type )
: 2! tuck ! cell+ ! ; ( u1 u2 a -- : store two cells )
: 2@ dup cell+ @ swap @ ; ( a -- u1 u2 : fetch two cells )
: 2>r r> swap >r swap >r >r ; compile-only ( n n --,R: -- n n )
: 2r> r> r> swap r> swap >r ; compile-only ( -- n n,R: n n -- )
system[ user tup =cell tallot ]system
: source tup 2@ ; ( -- a u : get terminal input source )
: aligned dup #1 and 0<> #1 and + ; ( u -- u : align up ptr. )
: align here aligned h? ! ; ( -- : align up dict. ptr. )
: allot h? +! ; ( n -- : allocate space in dictionary )
: , align here ! cell allot ; ( u -- : write value into dict. )
: count dup 1+ swap c@ ; ( b -- b c : advance string )
: +string #1 over min rot over + -rot - ; ( b u -- b u )
: type ( a u -- : print out a string )
  begin dup while swap count emit swap 1- repeat 2drop ;
: cmove ( b1 b2 u -- : move character blocks around )
   for aft >r c@+ r@ c! 1+ r> 1+ then next 2drop ;
: fill ( b u c -- : write byte 'c' to array 'b' of 'u' length )
   swap for swap aft 2dup c! 1+ then next 2drop ;
: erase #0 fill ; ( b u -- : write zeros to array )
:s do$ 2r> 2* dup count + aligned 2/ >r swap >r ;s ( -- a  )
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
   #0 ;                  ( 0 )    \ normal completion
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
:s ?depth depth >= -4 lit and throw ;s ( ??? n -- )
: um+ 2dup + >r r@ 0>= >r ( u u -- u carry )
  2dup and 0< r> or >r or 0< r> and negate r> swap ;
: dnegate invert >r invert #1 um+ r> + ; ( d -- d )
: d+ >r swap >r um+ r> + r> + ;         ( d d -- d )
: um* ( u u -- ud : double cell width multiply )
  #0 swap ( u1 0 u2 ) F lit
  for
    dup um+ >r >r dup um+ r> + r>
    if >r over um+ r> + then
  next shed ;
: * um* drop ; ( n n -- n : multiply two numbers )
: um/mod ( ud u -- ur uq : unsigned double cell div/mod )
  ?dup 0= -A lit and throw
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
  if negate >r dnegate r> then
  >r s>d if r@ + then r> um/mod r>
  if swap negate swap then ;
: /mod over 0< swap m/mod ; ( u1 u2 -- u1%u2 u1/u2 )
: mod  /mod drop ; ( u1 u2 -- u1%u2 )
: /    /mod nip ; ( u1 u2 -- u1/u2 )
:s (emit) opEmit ;s ( c -- : output byte to terminal )
: echo <echo> @execute ; ( c -- : emit a single character )
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
    key dup bl - 5F lit u< if tap else <tap> @execute then
  repeat drop over - ;
: expect <expect> @execute span ! drop ; ( a u -- )
: tib source drop ; ( -- b )
: query tib =buf lit <expect> @execute tup ! drop #0 >in ! ;
: -trailing for aft ( b u -- b u : remove trailing spaces )
     bl over r@ + c@ < if r> 1+ exit then
   then next #0 ;
:s look ( b u c xt -- b u : skip until *xt* test succeeds )
  swap >r -rot
  begin
    dup
  while
    over c@ r@ - r@ bl = 4 lit pick execute
    if rdrop shed exit then
    +string
  repeat rdrop shed ;s
:s unmatch if 0> exit then 0<> ;s ( c1 c2 -- t )
:s match unmatch invert ;s        ( c1 c2 -- t )
: parse ( c -- b u ; <string> )
  >r tib >in @ + tup @ >in @ - r@ ( get memory to parse )
  >r over r> swap 2>r
  r@ t' unmatch lit look 2dup ( look for start of match )
  r> t' match   lit look swap ( look for end of match )
    r> - >r - r> 1+ ( b u c -- b u delta : compute match len )
  >in +!
  r> bl = if -trailing then
  #0 max ;
:s banner ( +n c -- )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ;s
: hold #-1 hld +! hld @ c! ; ( c -- : save char in hold space )
: #> 2drop hld @ this =num lit + over - ; ( u -- b u )
:s extract ( ud ud -- ud u : extract digit from number )
  dup >r um/mod r> swap >r um/mod r> rot ;s
:s digit 9 lit over < 7 lit and + [char] 0 + ;s ( u -- c )
: #  #2 ?depth #0 radix extract digit hold ; ( d -- d )
: #s begin # 2dup ( d0= -> ) or 0= until ;   ( d -- 0 )
: <# this =num lit + hld ! ;                 ( -- )
: sign 0>= ?exit [char] - hold ; ( n -- )
: u.r >r #0 <# #s #>  r> over - bl banner type ; ( u r -- )
: u. space #0 u.r ; ( u -- : unsigned numeric output )
:s (.) abs radix opDivMod ?dup if (.) then digit emit ;s
: . space s>d if [char] - emit then (.) ; ( n -- )
: >number ( ud b u -- ud b u : convert string to number )
  begin
    2dup 2>r drop c@ radix           ( get next character )
    ( digit? -> ) >r [char] 0 - 9 lit over <
    if 7 lit - dup A lit < or then dup r> u< ( c base -- u f )
    0= if                            ( d char )
      drop                           ( d char -- d )
      2r>                            ( restore string )
      exit                           ( finished...exit )
    then                             ( d char )
    swap radix um* drop rot radix um* d+ ( accumulate digit )
    2r>                              ( restore string )
    +string dup 0=                   ( advance, test for end )
  until ;
: number? ( a u -- d -1 | a u 0 : easier to use than >number )
  #-1 dpl !
  radix >r
  over c@ [char] - = dup >r if         +string then
  over c@ [char] $ =        if hex     +string then
( over c@ [char] # =        if decimal +string then )
  2>r #0 dup 2r>
  begin
    >number dup
  while over c@ [char] . <>
    if shed rot r> 2drop #0 r> base ! exit then
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
  nfa c@+ 1F lit and + cell+ cell negate and ;
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
    nip @+
  repeat
  rdrop 2drop #0 ;s
:s (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 : find a word )
  >r
  context
  begin
    @+
  while
    @+ @ r@ swap (search) ?dup
    if
      >r shed r> rdrop exit
    then
    cell+
  repeat drop #0 r> #0 ;s
: search-wordlist ( a wid -- PWD 1|PWD -1|a 0 )
   (search) shed ;
: find ( a -- pwd 1 | pwd -1 | a 0 : find word in dictionary )
  (find) shed ;
: compile r> dup [@] , 1+ >r ; compile-only ( -- )
:s (literal) state @ if compile (push) , then ;s
: literal <literal> @execute ; immediate ( u -- )
: compile, align 2/ , ;  ( xt -- )
:s ?found ?exit ( b f -- b | ??? )
   space count type [char] ? emit cr -D lit throw ;s
: interpret ( b -- )
  find ?dup if
    state @
    if
      0> if cfa execute exit then \ <- execute immediate words
      cfa compile, exit \ <- compiling word are...compiled.
    then
    drop
    dup nfa c@ 20 lit and 0<> -E lit and throw ( <- ?compile )
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
   #0 >r begin @+ r@ xor while cell+ repeat rdrop
  dup cell - swap
  context - 2/ dup >r 1- s>d -$32 lit and throw
  for aft @+ swap cell - then next @ r> ;
:r set-order ( widn ... wid1 n -- : set current search order )
  \ NB. Uses recursion, however the meta-compiler does not use
  \ the Forth compilation mechanism, so the current definition
  \ of "set-order" is available immediately.
  dup #-1 = if drop root-voc #1 set-order exit then
  dup #vocs > -$31 lit and throw
  context swap for aft tuck ! cell+ then next #0 swap ! ;r
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r ( recurse -> ) (order) over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
root[
  {forth-wordlist} constant forth-wordlist ( -- wid )
          {system} constant system         ( -- wid )
]root
:r forth root-voc forth-wordlist #2 set-order ;r ( -- )
:r only #-1 set-order ;r ( -- : set minimal search order )
:s .id ( pwd -- : print word )
  nfa count 1F lit and type space ;s
:r words ( -- )
  cr get-order 
  begin ?dup while swap ( dup u. ." : " ) @
    begin ?dup
    while dup nfa c@ 80 lit and 0= if dup .id then @
    repeat ( cr )
  1- repeat ;r
: definitions context @ set-current ; ( -- )
: word ( -- b )
  #1 ?depth parse here aligned dup >r 2dup ! 1+ swap cmove r> ;
:s token bl word ;s ( -- b )
:s ?unique ( a -- a : warn if word definition is not unique )
 dup get-current (search) 0= ?exit space
 2drop {last} lit @ .id ." redefined" cr ;s ( b -- b )
:s ?nul c@+ ?exit -$10 lit throw ;s ( b -- b )
:s ?len c@+ 1F lit > -$13 lit and throw ;s ( b -- b )
:to char token ?nul count drop c@ ; ( "name", -- c )
:to [char] postpone char compile (push) , ; immediate
:to ;
  CAFE lit <> -$16 lit and throw ( check compile safety )
  =unnest lit ,                     ( compile exit )
  postpone [                        ( back to command mode )
  ?dup if                           ( link word in if non 0 )
    get-current !                   ( this inks the word in )
  then ; immediate compile-only
:to :   ( "name", -- colon-sys )
  align                 ( must be aligned before hand )
  here dup              ( push location for ";" )
  {last} lit !          ( set last defined word )
  last ,                ( point to previous word in header )
  token ?nul ?len ?unique ( parse word and do basic checks )
  count + h? ! align    ( skip over packed word and align )
  CAFE lit              ( push constant for compiler safety )
  postpone ] ;          ( turn compile mode on )
:to :noname align here #0 CAFE lit ] ; ( "name", -- xt )
:to ' token find ?found cfa literal ; immediate
:to recurse {last} lit @ cfa compile, ; immediate compile-only
:s toggle tuck @ xor swap ! ;s ( u a -- : toggle bits at addr )
:s hide token find ?found nfa 80 lit swap toggle ;s
:s mark here #0 , ;s compile-only
:to begin here ; immediate compile-only
:to if =jumpz lit , mark ; immediate compile-only
:to until 2/ postpone if ! ; immediate compile-only
:to again =jump lit , compile, ; immediate compile-only
:to then here 2/ swap ! ; immediate compile-only
:to while postpone if ; immediate compile-only
:to repeat swap postpone again postpone then ;
    immediate compile-only
:to else =jump lit , mark swap postpone then ;
    immediate compile-only
:to for =>r lit , here ; immediate compile-only
:to aft drop =jump lit , mark here swap ;
    immediate compile-only
:to next =next lit , compile, ; immediate compile-only
:s (marker) r> 2* @+ h? ! cell+ @ get-current ! ;s compile-only
: create postpone : drop postpone [ compile (var)
   get-current ! ;
:to variable create #0 , ;
:to constant create cell negate allot compile (const) , ;
:to user create cell negate allot compile (user) 
    user? @ , #1 user? +! ;
: >body cell+ ; ( a -- a : move to a create words body )
:s (does) r> r> 2* swap >r ;s compile-only
:s (comp)
  r> {last} lit @ cfa
  ( check we are running does> on a created word )
  @+ to' (var) half lit <> -$1F lit and throw
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
:s (s) align [char] " word count nip 1+ allot align ;s
:to ." compile .$ (s)  ; immediate compile-only
:to $" compile ($) (s)  ; immediate compile-only
:to abort" compile (abort) (s) ; immediate compile-only
:to ( [char] ) parse 2drop ; immediate ( c"xxx" -- )
:to .( [char] ) parse type ; immediate ( c"xxx" -- )
:to \ tib @ >in ! ; immediate ( c"xxx" -- )
:to postpone token find ?found cfa compile, ; immediate
:s (nfa) last nfa toggle ;s ( u -- )
:to immediate 40 lit (nfa) ; ( -- : mark prev word as immed. )
:to compile-only 20 lit (nfa) ; ( -- )
:to see token find ?found cr ( --, "name" : decompile  word )
  begin @+ =unnest lit <>
  while @+ . cell+ here over < if drop exit then
  repeat @ u. ;
:to dump aligned ( a u -- : display section of memory )
  begin ?dup
  while swap @+ . cell+ swap cell -
  repeat drop ;
:s cksum aligned dup C0DE lit - >r ( a u -- u )
  begin ?dup
  while swap @+ r> + >r cell+ swap cell -
  repeat drop r> ;s
: defined token find nip 0<> ; ( -- f )
:to [then] ; immediate ( -- )
:to [else]
 begin
  begin token c@+ while
   find drop cfa dup to' [else] lit = swap to' [then] lit = or
    ?exit repeat query drop again ; immediate
:to [if] ?exit postpone [else] ; immediate
: ms for pause calibration @ for next next ; ( ms -- )
: bell 7 lit emit ; ( -- : emit ASCII BEL character )
:s csi 1B lit emit 5B lit emit ;s ( -- : ANSI Term. Esc. Seq. )
: page csi ." 2J" csi ." 1;1H" ( csi ." 0m" ) ;
: at-xy radix decimal ( x y -- : set cursor position )
   >r csi #0 u.r ." ;" #0 u.r ." H" r> base ! ;
( system[ variable dirty ]system )
: b/buf 400 lit ; ( -- u : size of the block buffer )
: block #1 ?depth dup blk ! A lit lshift pause ; ( k -- u )
: flush ( dirty @ if save-buffers empty-buffers then ) ;
: update ( #-1 dirty ! ) ; ( -- : mark cur. buf. as dirty )
: blank bl fill ; ( a u -- : blank an area of memory )
: list ( k -- : display a block )
   page cr         ( clean the screen )
   dup scr ! block ( update "scr" and load block )
   F lit for       ( for each line in the block )
     F lit r@ - 3 lit u.r space    ( print the line number )
     40 lit 2dup type cr +         ( print line )
   ( 3F lit for count emit next cr \ print line )
   next drop ;
: get-input source >in @ source-id <ok> @ ; ( -- n1...n5 )
: set-input <ok> ! {id} up ! >in ! tup 2! ; ( n1...n5 -- )
:s ok state @ ?exit ."  ok" cr ;s ( -- : okay prompt )
:s eval ( "word" -- )
   begin token c@+ while
     interpret #0 ?depth
   repeat drop <ok> @execute ;s 
: evaluate ( a u -- : evaluate a string )
  get-input 2>r 2>r >r        ( save the current input state )
  #0 #-1 to' ) lit set-input  ( set new input )
  t' eval lit catch           ( evaluate the string )
  r> 2r> 2r> set-input        ( restore input state )
  throw ;                     ( throw on error )
:s line 6 lit lshift swap block + 40 lit ;s ( k l -- a u )
:s loadline line evaluate ;s ( k l -- ??? : execute a line! )
: load #0 F lit for
   2dup 2>r loadline 2r> 1+ next 2drop ; ( k -- : exec blk )
:r eforth 0109 lit ;r ( --, version )
:s info cr ( --, print system info )
  ." eForth v1.9, Public Domain,"  here . cr
  ." Richard James Howe, howe.r.j.89@gmail.com" cr
  ." https://github.com/howerj/subleq" cr ;s
:s xio t' accept lit <expect> ! <tap> ! <echo> ! <ok> ! ;s
:s hand t' ok lit
    t' (emit) lit ( Default: echo on )
    {options} lit @ #1 and if drop to' drop lit then
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
   ?dup if <error> @execute then   ( error? )
  again ;                          ( do it all again... )
:s (cold) ( -- : Forth boot sequence )
  forth definitions ( un-mess-up dictionary / set it )
  ini ( initialize the current thread correctly )
  {options} lit @ 4 lit and if info then ( display info? )
  {options} lit @ #2 and if ( checksum on? )
    primitive lit @ 2* dup here swap - cksum  ( calc. cksum )
    check lit @ <> if ." bad cksum" bye then ( oops... )
    {options} lit @ #2 xor {options} lit ! ( disable cksum )
  then quit ;s ( call the interpreter loop AKA "quit" )
:s task: ( "name" -- : create a named task )
  create here b/buf allot 2/ task-init ;s
:s activate ( xt task-address -- : start task executing xt )
  dup task-init
  dup >r swap 2/ swap {ip-save} lit + ! ( set execution word )
  r> this @ >r dup 2/ this ! r> swap ! ;s ( link in task )
:s wait ( addr -- : wait for signal )
  begin pause @+ until #0 swap ! ;s
:s signal this swap ! ;s ( addr -- : signal to wait )
:s single #1 {single} lit ! ;s ( -- : disable other tasks )
:s multi  #0 {single} lit ! ;s ( -- : enable multitasking )
:s send ( msg task-addr -- : send message to task )
  this over {sender} lit +
  begin pause @+ 0= until
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
:e ia #2 ?depth 6 lit lshift + scr @ block + tib >in @ +
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
: cold {cold} lit 2* @execute ; ( -- )
t' (cold) half {cold} t!      \ Set starting Forth word
atlast {forth-wordlist} t!    \ Make wordlist work
{forth-wordlist} {current} t! \ Set "current" dictionary
there h t!                    \ Assign dictionary pointer
local? {user}  t!             \ Assign number of locals
primitive t@ double mkck check t! \ Set checksum over Forth
atlast {last} t!              \ Set last defined word
save-target                   \ Output target
.end                          \ Get back to normal Forth
bye                           \ Auf Wiedersehen
As we have called "bye", we can write what we want here without
it being run.

