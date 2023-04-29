\ TODO: Implement a generic block word set interface with
\ a vectored execution word for actually transferring data.
\ TODO: MS-DOS subsystem, file system and file access words
\ built upon the block system.

system +order

1 [if]
$F800 constant buf0
$F400 constant buf1
[else]
create buf0 b/buf allot
create buf1 b/buf allot
[then]

( space for two buffers, consists of addr and blk/flags )
\ TODO: Store in frequency sorted list

\ Data structure:
\
\ * "buffers" contains an array of pointers to block buffer
\ locations
\ * "block" contains an array of block numbers (highest bit
\ is used for a dirty flag), zero is an invalid block number
\ and used for free blocks.
\ * Each location in "buffers" is paired with a location in
\ "block". The list is frequency sorted with the last accessed
\ block first.
\

2 constant #buffers
1024 constant b/buf
create buffers buf0 , 0 , buf1 , 0 ,
create blocks 0 , 0 ,

user <block>
user scr
user blk

: blk? dup 1 128 within 0= -35 and throw ;

\ This word should transfer either to or from a storage
\ medium.
: transfer blk? ( addr addr flag -- )
  \ TODO: Check alignment on addresses
  b/buf for aft then next
; 

: aswap ( a a -- : swap to values at two addresses ) ; 

: buffer blk? ;
: block ;

: update ;
: empty-buffers ;
: save-buffers ;
: flush save-buffers empty-buffers ;
: thru ; ( k1 k2 -- )
: show ; ( k1 k2 -- )
: list ;
: screens ;
: load ;

\ find buffer
\ transfer

' transfer <block> !


