\ https://groups.google.com/forum/#!topic/comp.lang.forth/mHs30gEBuyE
\ bitbuster decoder in Forth - reduced to max 65535 bytes 
\ output. No encoder...

variable data     ( input data pointer - moves )
variable bitcount ( bit counter - read it MOD 8 )
variable bits     ( shift buffer for bit data )

: @byte ( -- c )
  data @ c@ 1 data +! ;

: @bit ( -- 1|0 )
  bitcount @ 7 and if bits @ else @byte
  then dup 2* bits !
  128 and 7 rshift 1 bitcount +! ;

: @bit+ ( n -- n' )
  2* @bit or ;

: @gamma ( -- n )
  1 0 begin @bit while 1+ repeat
  0 ?do @bit+ loop 1+ ;

: decstep ( -- )
  @bit if                              ( compressed )
    here 1- @byte dup 128 and if       ( 11 bits offset )
      127 and
      @bit @bit+ @bit+ @bit+
      7 lshift or
    then - @gamma bounds do i c@ c, loop
  else @byte c, then ;

: bbdecode ( p -- flag  ( flag is true on success )
  data ! bitcount off bits off
  @byte @byte 8 lshift or here +
  @byte @byte or 0= if
    begin here over u< while decstep repeat
  then here = ;
