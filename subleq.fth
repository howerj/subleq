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

0 t,
1 t,
-1 t,

save-hex    subleq.hex
save-target subleq.bin
bye
