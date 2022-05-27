<ok> @ ' ) <ok> !

: wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )

: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;

( NB. Bitwise ops must be masked off on non 16-bit machines )
: crc ( b u -- u : calculate ccitt-ffff CRC )
  $FFFF >r begin ?dup while
   over c@ r> swap 
   ( CCITT polynomial $1021, or "x16 + x12 + x5 + 1" )
   over $8 rshift xor ( crc x )
   dup  $4 rshift xor ( crc x )
   dup  $5 lshift xor ( crc x )
   dup  $C lshift xor ( crc x )
   swap $8 lshift xor ( crc )
   >r +string
  repeat r> nip ;

( A primitive user login system [that is super insecure]. )
wordlist +order definitions
wordlist constant users
dup constant (prompt)
: conceal $1B emit ." [8m" ; ( NB. Could also override <emit> )
: reveal $1B emit ." [28m" ;
: secure users 1 set-order ;
: restore only forth definitions decimal (prompt) <ok> ! ;
: message ." user: " ;
: fail ." Invalid username or password" cr message ;
: success ." logged in." ;
: pass bl word count crc ; ( super-super-secure <_< )
: ask ." pass: " conceal query reveal ;

forth-wordlist +order definitions

: user 
  create pass ,
  does> ask @ pass = if restore success exit then fail ;
: login secure message ' ) <ok> ! ;
: .users get-order secure words set-order ;

users +order definitions
user guest guest
user admin password1
user archer dangerzone
user cyril figgis
user lana stirling
users -order
.( EFORTH ONLINE ) cr
<ok> ! login

