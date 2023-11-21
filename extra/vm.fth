\ Author: Richard James Howe
\ Email: howe.r.j.89@gmail.com
\ Repo: https://github.com/howerj/subleq
\ License: The Unlicense 
\ 
\ A SUBLEQ interpreter written in Forth.
\
\ TODO: Key Vs Key?, refactor, vocabulary,
\ memory bounds, better exit conditions, Single word version
\ (inline all support words), remove need for pick, add to
\ "subleq.fth" file and write about it, debugging options.
\
 
1 constant vm

vm 0 = [if]
: A swap 0 + cells + @ ; ( pc base -- A )
: B swap 1+  cells + @ ; ( pc base -- B )
: C swap 2 + cells + @ ; ( pc base -- C )
: m+A tuck A cells swap + ; ( pc base -- m+A )
: m+B tuck B cells swap + ; ( pc base -- m+B )
: m[A] m+A @ ; ( pc base -- m[A] )
: m[B] m+B @ ; ( pc base -- m[B] )
: subleq ( a u -- n )
  >r >r 0
  begin
    dup r@ A -1 = if
      dup r@ m+B key swap ! 3 +
    else
      dup r@ B -1 = if
        dup r@ m[A] emit 3 +
      else
        dup r@ m[A]          \ pc m[A]
        over r@ m[B] swap -  \ pc m[B]-m[A]
        dup 0<=              \ pc m[B]-m[A] f
        2 pick r@ C          \ pc m[B]-m[A] f C
        rot                  \ pc f C m[B]-m[A] 
        3 pick r@ m+B ! swap \ pc C f
        if nip else drop 3 + then
      then
    then
  \ Negative or out of bounds
  dup 0< 
  r> r> tuck >r >r
  2 pick < or
  until rdrop rdrop 0 ; 
[then]

vm 1 = [if]
: subleq ( a u -- n )
  >r >r 0
  begin
    dup r@ swap cells + @ -1 = if
      dup r@ tuck swap 1+ cells + @ cells swap + key swap ! 
      3 +
    else
      dup r@ swap 1+ cells + @ -1 = if
        dup r@ tuck swap cells + @ cells swap + @ emit 
        3 +
      else
        dup r@ tuck swap cells + @ cells swap + @       
        over r@ tuck swap 1+ cells + @ cells swap + @ swap - 
        dup 0<=             
        2 pick r@ swap 2 + cells + @         
        rot                 
        3 pick r@ tuck swap 1+ cells + @ cells swap + ! swap
        if nip else drop 3 + then
      then
    then
  dup 0<
  r> r> tuck >r >r
  2 pick < or
  until rdrop rdrop 0 ; 
[then]
variable (program)
: {{ 0 (program) ! ;
: }} ;
: #program (program) @ ;
: | 1 (program) +! , ;

4 constant select

select 0 = [if] \ Print "Hi" (no newline)
create program {{
	9   |  -1   |  3   |
	10  |  -1   |  6   |
	0   |  0    |  -1  |
	72  |  105  |
}}
[then]

select 1 = [if] \ Echo single char
create program {{
	-1 | 9  | 3  |
	9  | -1 | 6  |
	0  | 0  | -1 |
	0  |
}}
[then]

select 2 = [if] \ Echo
create program {{
	-1  |  9   |  3  |
	9   |  -1  |  6  |
	9   |  9   |  0  |
	0   |
}}
[then]

select 3 = [if] \ Print "Hello, world!"
create program {{
 15   |  17   |  -1   |  17   |  -1   |  -1   |  16  |  1   |
 -1   |  16   |  3    |  -1   |  15   |  15   |  0   |  0   |
 -1   |  72   |  101  |  108  |  108  |  111  |  44  |  32  |
 119  |  111  |  114  |  108  |  100  |  33   |  10  |  0   |
}}
[then]

select 4 = [if]
create program {{
\ Self interpreter
15   |  15   |  3    |  145  |  144   |  6    |
144  |  15   |  9    |  144  |  144   |  12   |
114  |  114  |  15   |  0    |  144   |  18   |
144  |  114  |  21   |  144  |  144   |
24   |  15   |  15   |  27   |  114   |  144  |
30   |  144  |  15   |  33   |  144   |  144  |
36   |  147  |  15   |  42   |  148   |  114  |
42   |  147  |  145  |  45   |  60    |  60   |
48   |  145  |  144  |  51   |  144   |
60   |  54   |  144  |  144  |  57    |
115  |  115  |  60   |  0    |  144   |
63   |  144  |  115  |  66   |  144   |
144  |  69   |  60   |  60   |  72    |  115  |
144  |  75   |  144  |  60   |  78    |
144  |  144  |  81   |  147  |  60    |
87   |  148  |  115  |  87   |  147   |
145  |  90   |  105  |  105  |  93    |
145  |  144  |  96   |  144  |  105   |
99   |  144  |  144  |  102  |  146   |
146  |  105  |  0    |  144  |  108   |
144  |  146  |  111  |  144  |  144   |  114  |
0    |  0    |  123  |  147  |  145   |  120  |
144  |  144  |  0    |  145  |  145   |  126  |
146  |  144  |  129  |  144  |  145   |  132  |
144  |  144  |  135  |  147  |  146   |  -1   |
148  |  145  |  141  |  144  |  144   |  0    |
0    |  149  |  0    |  -1   |  -149  |
\ Print "Hello, world!"
 15   |  17   |  -1   |  17   |  -1   |  -1   |  16  |  1   |
 -1   |  16   |  3    |  -1   |  15   |  15   |  0   |  0   |
 -1   |  72   |  101  |  108  |  108  |  111  |  44  |  32  |
 119  |  111  |  114  |  108  |  100  |  33   |  10  |  0   |

}}
[then]

program #program subleq throw bye
