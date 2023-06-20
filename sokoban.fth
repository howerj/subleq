' ( <ok> !
.( LOADING... ) cr

only forth definitions hex

variable sokoban-wordlist
sokoban-wordlist +order definitions

$20    constant maze
char X constant wall
char * constant boulder
char . constant off
char & constant on
char @ constant player
char ~ constant player+ ( player + off pad )
$10    constant l/b     ( lines   per block )
$40    constant c/b     ( columns per block )
     7 constant bell    ( bell character )

variable position  ( current player position )
variable moves     ( moves made by player )
variable lblk      ( last block loaded )

( used to store rule being processed )
create rule 3 c, 0 c, 0 c, 0 c,

: n1+ swap 1+ swap ; ( n n -- n n )
: match              ( a a -- f )
  n1+ ( replace with umin of both counts? )
  count
  for aft
    count rot count rot <> if 2drop rdrop 0 exit then
  then next 2drop -1 ;

: beep bell emit ; ( -- )
: ?apply           ( a a a -- a, R: ? -- ?| )
  >r over swap match if drop r> rdrop exit then rdrop ;

: apply ( a -- a )
 $" @ "  $"  @"  ?apply
 $" @."  $"  ~"  ?apply
 $" @* " $"  @*" ?apply
 $" @*." $"  @&" ?apply
 $" @&." $"  ~&" ?apply
 $" @& " $"  ~*" ?apply
 $" ~ "  $" .@"  ?apply
 $" ~."  $" .~"  ?apply
 $" ~* " $" .@*" ?apply
 $" ~*." $" .@&" ?apply
 $" ~&." $" .~&" ?apply
 $" ~& " $" .~*" ?apply beep ;

: pack ( c0...cn b n -- )
  2dup swap c! for aft 1+ tuck c! then next drop ;

: locate ( b u c -- u f )
  >r
  begin
    ?dup
  while
    1- 2dup + c@ r@ = if nip rdrop -1 exit then
  repeat
  rdrop
  drop
  0 0 ;

: relative swap c/b * + + ( $3ff and ) ; ( +x +y pos -- pos )
: +position position @ relative ; ( +x +y -- pos )
: double 2* swap 2* swap ;  ( u u -- u u )
: arena lblk @ block b/buf ; ( -- b u )
: >arena arena drop + ;     ( pos -- a )
: fetch                     ( +x +y -- a a a )
  2dup   +position >arena >r
  double +position >arena r> swap
  position @ >arena -rot ;
: rule@ fetch c@ rot c@ rot c@ rot ; ( +x +y -- c c c )
: 3reverse -rot swap ;               ( 1 2 3 -- 3 2 1 )
: rule! rule@ 3reverse rule 3 pack ; ( +x +y -- )
: think 2dup rule! rule apply >r fetch r> ; ( +x +y --a a a a )
: count! count rot c! ;              ( a a -- )

\ 'act' could be made to be more elegant, but it works, it
\ handles rules of length 2 and length 3

: act ( a a a a -- )
  count swap >r 2 =
  if
     drop swap r> count! count!
  else
     3reverse r> count! count! count!
  then drop ;

: #boulders ( -- n )
   0 arena
   for aft
     dup c@ boulder = if n1+ then
     1+
   then next drop ;
: instructions ;                      ( -- )
: .boulders  ." BOLDERS: " #boulders u. cr ; ( -- )
: .moves     ." MOVES:   " moves    @ u. cr ; ( -- )
: .help      ." WASD:     MOVEMENT" cr ( -- ) 
             ." H:        HELP" cr ; 
: .maze lblk @ list ;                  ( -- )
: show ( page cr ) .maze .boulders .moves .help ; ( -- )
: solved? #boulders 0= ;               ( -- )
: finished? solved? if 1 throw then ; ( -- )
: where >r arena r> locate ;          ( c -- u f )
: player? player where 0= if drop player+ where else -1 then ;
: player! player? 0= -2 and throw position ! ; ( -- )
: start player! 0 moves ! ;           ( -- )
: .winner show cr ." SOLVED!" cr ;    ( -- )
: .quit cr ." Quitter!" cr ;          ( -- )
: finish 1 = if .winner exit then .quit ; ( n -- )
: rules think act player! ;           ( +x +y -- )
: +move 1 moves +! ;                  ( -- )
: ?ignore over <> if rdrop then ;     ( c1 c2 --, R: x -- | x )
: left  [char] a ?ignore -1  0 rules +move ; ( c -- c )
: right [char] d ?ignore  1  0 rules +move ; ( c -- c )
: up    [char] w ?ignore  0 -1 rules +move ; ( c -- c )
: down  [char] s ?ignore  0  1 rules +move ; ( c -- c )
: help  [char] h ?ignore instructions ; ( c -- c )
: end  [char] q ?ignore drop 2 throw ; ( c -- | c, R ? -- | ? )
: default drop ;  ( c -- )
: command up down left right help end default finished? ;
: maze! dup lblk ! block drop ; ( k -- )
: input key ;        ( -- c )

sokoban-wordlist -order definitions
sokoban-wordlist +order

: sokoban ( k -- : play a game of sokoban given a Forth block )
  maze! start
  begin
    show input ' command catch ?dup
  until finish ;

only forth definitions decimal
editor 30 r z
 1 a            XXXXX
 2 a            X   X
 3 a            X*  X
 4 a          XXX  *XXX
 5 a          X  *  * X
 6 a        XXX X XXX X     XXXXXX
 7 a        X   X XXX XXXXXXX  ..X
 8 a        X *  *             ..X
 9 a        XXXXX XXXX X@XXXX  ..X
10 a            X      XXX  XXXXXX
11 a            XXXXXXXX
n z
 1 a       XXXXXXXXXXXX
 2 a       X..  X     XXX
 3 a       X..  X *  *  X
 4 a       X..  X*XXXX  X
 5 a       X..    @ XX  X
 6 a       X..  X X  * XX
 7 a       XXXXXX XX* * X
 8 a         X *  * * * X
 9 a         X    X     X
10 a         XXXXXXXXXXXX
? n z
 1 a               XXXXXXXX
 2 a               X     @X
 3 a               X *X* XX
 4 a               X *  *X
 5 a               XX* * X
 6 a       XXXXXXXXX * X XXX
 7 a       X....  XX *  *  X
 8 a       XX...    *  *   X
 9 a       X....  XXXXXXXXXX
10 a       XXXXXXXX
n z
 1 a                     XXXXXXXX
 2 a                     X  ....X
 3 a          XXXXXXXXXXXX  ....X
 4 a          X    X  * *   ....X
 5 a          X ***X*  * X  ....X
 6 a          X  *     * X  ....X
 7 a          X ** X* * *XXXXXXXX
 8 a       XXXX  * X     X
 9 a       X   X XXXXXXXXX
10 a       X    *  XX
11 a       X **X** @X
12 a       X   X   XX
13 a       XXXXXXXXX
q

system +order ' ok <ok> ! only forth definitions decimal
.( LOADED ) cr
.( Type '# sokoban' to play, where '#' is a block number ) cr
.( For example "30 sokoban" ) cr
.( Follow the on screen instructions to play a game. ) cr


