defined eforth [if] ' nop <ok> ! [then] ( Turn off ok prompt )
\ Program: Specialized script to turn custom markup in Forth
\          code into a markdown file.
\ Usage:
\    cat convert.fth subleq.fth | ./subleq 1.dec > subleq.md
hex
variable buf 80 allot
variable text? 0 text? !
variable cnt 0 cnt !
: tab 9 emit ;
: banner ( +n c -- )
  >r begin dup 0> while r@ emit 1- repeat drop rdrop ;
: display 0 <# #s #> dup 4 swap - 0 max [char] 0 banner type ;
: process
  begin
   buf 80 accept ?dup if
     2dup drop c@ [char] \ = if
       text? @ 0= if cr then
       +string ?dup if +string type else drop then cr
       1 text? !
     else
       text? @ if cr then
       0 text? !
       1 cnt +!
       tab cnt @ display tab type cr
     then
    else drop then
  again ;
decimal process
