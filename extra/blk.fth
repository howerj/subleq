\ TODO: Multiple buffers, testing, integrate as optional
\ component in main application, document
\
\ N.B. Block to block transfer without an intermediary buffer
\ will not work!

decimal system +order definitions

\ TODO: Remove test code
\ : .ch dup bl 127 within 0= if drop [char] . then emit ;
\ : display
\   for aft count .ch r@ 64 mod 0= if cr then then next drop ;

variable <block>

$F800 constant buf0
variable dirty0
variable blk0

: >addr dup 1 and 0<> -9 and throw 2/ ; ( a -- a/2 )
: (block) ( a a u -- : transfer to/from "mass storage" ) 
  >r >addr swap >addr swap r> >addr
  for 
    aft 2dup [@] swap [!] 1+ swap 1+ swap
    then
  next 2drop ;

' (block) <block> !

: valid? dup 0 $7F within ; ( k -- k f )
: transfer <block> @ execute ; ( a a u -- )
: >blk b/buf * ; ( k -- a : convert blk addr to addr )
: clean 0 dirty0 ! ; ( -- : opposite of update, clean blk buf )
: invalidate -1 blk0 ! ; ( -- )
: put valid? if >blk buf0 b/buf transfer exit then drop ;
: get valid? if >blk buf0 swap b/buf transfer exit then drop ;

forth-wordlist +order definitions

: update -1 dirty0 ! ; ( -- )
: save-buffers dirty0 @ if blk0 @ put clean then ; ( -- )
: flush save-buffers invalidate ; ( -- )
: empty-buffers clean invalidate ; ( -- )
: buffer ( k -- a )
  1 ?depth dup 1 $80 within 0= -$23 and throw
  1- 
  pause dup blk0 @ = if drop buf0 exit then
  dirty0 @ if buf0 blk0 @ put then
  blk0 ! buf0 ;
: block dup buffer swap 1- get ; ( k -- a )

only forth definitions
