\
\ Cross Compiler and eForth interpreter for the SUBLEQ CPU available at:
\
\     <https://github.com/howerj/subleq>
\
\ This is a work in progress.
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
:m hex# ( u -- addr len )  0 <# base @ >r hex =lf hold # # # # r> base ! #> ;m
:m save-hex ( <name> -- )
  parse-word w/o create-file throw
  there 0 do i t@  over >r hex# r> write-file throw =cell +loop
   close-file throw ;m
:m save-target ( <name> -- )
  parse-word w/o create-file throw >r
   tflash there r@ write-file throw r> close-file ;m
:m .h base @ >r hex     u. r> base ! ;m
:m .d base @ >r decimal u. r> base ! ;m
:m twords
   cr tlast @
   begin
      dup tflash + =cell + count 1f and type space t@
   ?dup 0= until ;m
:m .stat
  0 if
    ." target: "      target.1      +order words cr cr
    ." target-only: " target.only.1 +order words cr cr
    ." assembler: "   assembler.1   +order words cr cr
    ." meta: "        meta.1        +order words cr cr
  then
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

\ TODO: start/vm, nest, unnest, push, jump, jumpz, next, bye, exit, lshift,
\ rshift, and, or, xor, +, um+, @, !, c@, c!, dup, drop, swap, over, 1-, >r,
\ r>, r@, rdrop, execute, sp!, rp!, sp@, rp@, key, emit
\ TODO: Virtual machine setup

\ NOTES:
\
\ Z = location of a zero, may be temporarily modified
\ #<num> = location of a memory cell containing <num>
\ Omitted 'c' implies suppression (jump to next instruction)
\
\ BEQ b, c
\     subleq b, Z, L1
\     subleq Z, Z, OUT
\ L1: subleq Z, Z
\     subleq Z, b, c
\ OUT: ...
\

:m Z 0 t, ;m  \ TODO: store 0, 1, -1 in DAT section
:m #1 1 t, ;m
:m #N1 -1 t, ;m
:m NADDR tdp @ 1+ t, ;m \ TODO: Round up

:m JMP Z Z t, ;m ( c -- )
:m ADD swap t, Z NADDR Z t, NADDR Z Z NADDR ;m ( a b -- )
:m NOP Z Z NADDR ;m ( -- )
:m INC #N1 t, NADDR ;m ( b -- )
:m DEC #1 t, NADDR ;m ( b -- )
:m MOV >r r@ dup t, t, NADDR t, Z NADDR Z r> t, NADDR Z Z NADDR ;m ( a b -- )

:m & rot t, swap t, t, ;m

\ Hello World!
decimal
15   17   -1   &
17   -1   -1   &
16   1    -1   &
16   3    -1   &
15   15   0    &
0    -1   72   &
101  108  108  &
111  44   32   &
119  111  114  &
108  100  33   &
10   t,   0    t,
hex

\ ---------------------------------- Image Generation ------------------------

save-hex    subleq.hex
save-target subleq.bin
bye
