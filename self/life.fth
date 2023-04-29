\ ## Conways Game of Life
\ 
\ A Conways Games Of Life in a few screens, adapted to 
\ this Forth:
\ 
\ From: http://wiki.c2.com/?ForthBlocks

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

only forth definitions hex
variable life-vocabulary 0 life-vocabulary !
life-vocabulary +order definitions

   $40 constant c/b
   $10 constant l/b
c/b 1- constant c/b>
l/b 1- constant l/b>
    bl constant off
char * constant on

variable state-blk
variable life-blk
variable statep

: wrapy dup 0< if drop l/b> then dup l/b> > if drop 0 then ;
: wrapx dup 0< if drop c/b> then dup c/b> > if drop 0 then ;
: wrap  wrapy swap wrapx swap ;
: deceased? wrap c/b * + life-blk @ block + c@ off = ;
: living?  deceased? 0= ;
: (-1,-1) 2dup 1- swap 1- swap living? 1 and ;
: (0,-1)  >r 2dup 1- living? 1 and r> + ;
: (1,-1)  >r 2dup 1- swap 1+ swap living? 1 and r> + ;
: (-1,0)  >r 2dup swap 1- swap living? 1 and r> + ;
: (1,0)   >r 2dup swap 1+ swap living? 1 and r> + ;
: (-1,1)  >r 2dup 1+ swap 1- swap living? 1 and r> + ;
: (0,1)   >r 2dup 1+ living? 1 and r> + ;
: (1,1)   >r 1+ swap 1+ swap living? 1 and r> + ;
: mates (-1,-1) (0,-1) (1,-1) (-1,0) (1,0) (-1,1) (0,1) (1,1) ;
: born?  mates 3 = ;
: survives?  2dup living? -rot mates 2 = and ;
: lives?  2dup born? -rot survives? or ;        ( u u -- )
: newstate  state-blk @ block update statep ! ; ( -- )
: state!  statep @ c! 1 statep +! ;             ( c -- )
: alive  on state! ;                            ( -- )
: dead  off state! ;                            ( -- )
: cell?  2dup swap lives? if alive else dead then ; ( u u -- )
: rows   0 begin dup c/b < while cell? 1+ repeat drop ;
: iterate-block 0 begin dup l/b < while rows 1+ repeat drop ;
: generation  life-blk @ state-blk @ life-blk ! state-blk ! ;
: iterate  newstate iterate-block generation ;
: done?  key [char] q = ;                      ( -- f )
: prompt  cr ." q to quit" cr ;                ( -- )
: view (  page ) life-blk @ list prompt ;      ( -- )
: game  begin view iterate done? until ;       ( -- )

variable seed here seed !

system +order
: random seed 1 cells crc ?dup 0= if here then dup seed ! ;
system -order

: randomize ( k -- )
  block b/buf
  for aft
    random 1 and if on else off then over c! 1+
  then next
  drop ;

life-vocabulary -order definitions
life-vocabulary +order


decimal

: life life-blk ! state-blk ! game ;       ( k1 k2 -- )
: random-life 30 randomize 31 30 life ; ( -- )


editor l z
3 a      ***  
4 a      *    
5 a       *   
q


.( Usage: 31 30 life ) cr
.( Or:    random-life ) cr
\ random-life

