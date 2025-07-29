\ TODO:
\ * Scanning equivalent / scanf version
\ * Document Format specifiers
\ * AT-XY
\ * Leading chars, selectable ('0' or space or whatever)
\ * Namespace stuff, factorize
\ * Could return printed char count or error code
\ * TUI, Banners, Boxes
\ * Print in arbitrary base
\ * Scan equivalent:
\   - Regex
\   - Parse out ASCII
\   - Scanf behavior
\


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
  system +order
  : wordlist here 0 , ; ( -- wid : alloc wid )
[then]

defined s" 0= [if]
: s" 
  state @ if postpone $" [ ' count ] literal compile, 
  else 
    [char] " parse tuck here dup >r swap cmove r> swap dup 
    allot align
  then ; immediate
[then]

\ variable seed here seed !
\ : random ( -- u : 16-bit xorshift )
\  seed @ dup 0= if 0= then ( seed must not be zero )
\  dup 13 lshift xor
\  dup  9 rshift xor
\  dup  7 lshift xor
\  dup seed ! ;


wordlist constant printing ( TODO: make this anonymous? )
printing +order definitions

user <format>
user printed
user colorize

\ user @scan
\ user #scan

' emit <format> !
0 printed !
-1 colorize !

: .format ( c -- )
(  colorize @ 0= if drop exit then )
   <format> @ execute 1 printed +! ;
: .formats 1- for count .format next drop ;
: ?depth depth >= -4 and throw ;
: invalid -21 throw ;
\ : format> <format> @ printed @ colorize @ ;
\ : >format colorize ! printed ! <format> ! ;

: banner ( +n c -- : output 'c' 'n' times )
  >r begin dup 0> while r@ .format 1- repeat drop rdrop ;
: u.f 0 <# #s #> .formats ; ( u -- )
: .f dup >r abs 0 <# #s r> sign #> .formats ; ( n -- )
: d.rf ( d +n -- )
  >r tuck dabs <# #s rot sign #> r> over - bl banner .formats ;
: ud.rf >r <# #s #> r> over - bl banner .formats ; ( ud +n -- )
: d.f 0 d.r ;   ( d -- )
: ud.f 0 ud.r ; ( ud -- )

: .decimal base @ >r decimal u.f r> base ! ; ( n -- )
: .hex base @ >r hex u.f r> base ! ; ( u -- )
: .octal base @ >r 8 base ! u.f r> base ! ; ( u -- )
: .binary base @ >r 2 base ! u.f r> base ! ; ( u -- )

: csi $1B .format $5B .format ; ( -- )
: .at-xy ( n n -- )
  csi .decimal [char] ; .format .decimal [char] H .format ;
: .page csi [char] 2 .format [char] J .format 1 1 .at-xy ;

: .color csi .decimal [char] m .format ;

: percent ( ??? c -- ??? : handle `%` escape chars )
  case
    [char] d of 1 ?depth .f endof
    [char] D of 2 ?depth d.f endof \ Cell
    [char] u of 1 ?depth u.f endof
    [char] U of 2 ?depth ud.f endof \ Double Cell
    [char] x of 1 ?depth .hex endof
    [char] o of 1 ?depth .octal endof
    [char] b of 1 ?depth .binary endof
    [char] c of 1 ?depth .format endof
    [char] s of 2 ?depth .formats endof
    [char] S of 1 ?depth count .formats endof \ Counted string
    [char] % of [char] % .format endof
    [char] @ of 1 ?depth execute endof
    [char] n of 1 ?depth printed @ swap ! endof
    [char] # of 2 ?depth banner endof
    invalid
  endcase ;
: tilde ( ??? c -- ??? : handle `~` escape chars )
  case
    [char] k of 30 .color endof
    [char] r of 31 .color endof
    [char] g of 32 .color endof
    [char] y of 33 .color endof
    [char] b of 34 .color endof
    [char] m of 35 .color endof
    [char] c of 36 .color endof
    [char] w of 37 .color endof
    [char] x of 1 ?depth 8 mod abs 30 + .color endof

    [char] K of 40 .color endof
    [char] R of 41 .color endof
    [char] G of 42 .color endof
    [char] Y of 43 .color endof
    [char] B of 44 .color endof
    [char] M of 45 .color endof
    [char] C of 46 .color endof
    [char] W of 47 .color endof
    [char] X of 1 ?depth 8 mod abs 40 + .color endof

    [char] d of 1 .color endof ( bold )
    [char] f of 2 .color endof ( faint )
    [char] p of .page endof
    [char] @ of 2 ?depth .at-xy endof
    [char] n of 0  .color endof ( reset )
    [char] ~ of [char] ~ .format endof
    invalid
  endcase ;

: advance ( a u -- a u c )
  dup 0= -18 and throw
  over c@ >r
  +string
  r> ;

: escape ( ??? a u c -- ??? : handle `\` escape chars )
    case
       [char] a of $7 .format endof
       [char] b of $8 .format endof
       [char] e of $1B .format endof
       [char] f of $C .format endof
       [char] n of $A .format endof
       [char] r of $D .format endof
       [char] t of $9 .format endof
       [char] v of $B .format endof
       [char] \ of [char] \ .format endof
       [char] q of [char] " .format endof
       [char] N of $D .format $A .format endof
\       [char] x of  endof \ TODO: hex specifiers
      invalid
    endcase ;

only forth definitions
printing +order

: format ( ??? a u -- ior | u )
  2 ?depth
  0 printed !
  begin
    dup
  while
    advance 
    dup case
      [char] % of drop advance -rot 2>r percent 2r> endof
      [char] ~ of drop advance -rot 2>r tilde   2r> endof
      [char] \ of drop advance escape endof
      .format
    endcase
  repeat
  2drop printed @ ;

: scan (  ) ;

cr
101 123 s" abc%udef%c%% " format . cr
s" Dbc " format . cr
s" abc~rdef~nghi " format . cr
s" A~mB~dC~fD~n " format . cr
20 char = s" %# " format . cr 

: test ( -- )
  cr
  7 for
    r@ 7 for 
      dup r@ 2dup s" ~d~x~X%d:%d~n " format drop
    next drop
    cr
  next
  cr ;

test
only forth definitions decimal


