\ TODO: Implement a generic block word set interface with
\ a vectored execution word for actually transferring data.

system +order

2 constant #buffers
1024 constant b/buf

1 [if]
$F800 constant buf0
$F400 constant buf1
[else]
create buf0 b/buf allot
create buf1 b/buf allot
[then]

( space for two buffers, consists of addr and blk/flags )
\ TODO: Store in frequency sorted list
create buffers buf0 , 0 , buf1 , 0 ,

variable <block>
variable scr
variable blk
variable last

: >buffer 2 cells * buffers + ;
: >blk >buffer cell+ ;
: &blk >blk 255 and ;
: blk? dup 1 128 within 0= -35 and throw ;

: (block) blk? ( addr addr flag -- )
  \ TODO: Check alignment on addresses
  b/buf for aft then next

; 

: buffer blk? ;

: update last @ blk? ;
: empty-buffers ;
: save-buffers ;
: flush save-buffers empty-buffers ;
: thru ; ( k1 k2 -- )
: show ; ( k1 k2 -- )

\ find buffer
\ transfer

' (block) <block> !


