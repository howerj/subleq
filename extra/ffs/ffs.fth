\ TODO:
\ - Implement a file system based upon Forth blocks
\   - [ ] Design directory and file system structure
\   - [ ] Implement FAT
\   - [ ] Add error handling around block word set (if
\         a block cannot be read/written to then mark the
\         block as bad)
\   - [ ] Add more meta-data to header, allow greater
\         customization of the file system
\   - [ ] Add words to their own vocabularies
\ - [ ] Implement the File-Access word-set upon the file system
\   - [ ] Implement all words
\   - [ ] Read/Write to stdin/stdout if opening special file
\ - [ ] Come up with an execution model for Forth scripts
\       (can run multiple times) and Forth modules (should be
\       loaded only once).
\ - [ ] Implement a basic DOS upon the file system
\   - [ ] COMMAND prompt
\   - [ ] ls/rm/grep/cat/cp/mv/stat/cd/pwd/mkdir/rmdir/...
\ - [ ] Test under Gforth/SUBLEQ eFORTH
\ - [ ] Implement equivalent program in C that can turn a
\   set of files on disk into a FFS image


use ffs.fb

defined b/buf 0= [if] $0400 constant b/buf [then]

$0001 constant fs.version  ( FFS FAT Version )
$0010 constant fs.fat-size ( FAT entry size: 16, 24, 32, ... )
b/buf fs.fat-size 8 / / constant fepb ( FAT Entries Per Block )

\ FAT Entry Values (anything else is part of a FAT Chain)
$FFFF constant fat.invalid  ( Not allocated to the table )
$FFFE constant fat.error    ( Read/Write error on block )
$FFFE constant fat.special  ( Special use block )
$0000 constant fat.free     ( Free block )
$0001 constant fat.end      ( Marks the end of FAT chain )

\ Directory entry values 
$0000 constant dir.unused
$0001 constant dir.dir
$0002 constant dir.file
$0003 constant dir.special

( : dir? dir.special > -1 and throw ; )

\ TODO: Load from disk, along with other settings
$0001 constant fs.blk.start
$0080 constant fs.blk.end
fs.blk.end fs.blk.start - constant fs.blk.count

create pwd 128 allot

defined 0= [if] : 2+ 2 + ; [then]
: 16@ c@ over 1+ c@ 8 lshift or ; ( a -- 16 )
: 16! 2dup c! swap 8 rshift swap 1+ c! ; ( 16 a -- )
: 16, tuck 16! update 2+ ; ( u a -- a )

: blk.erase block b/buf erase update flush ;
: blk.erase.range 1- for dup blk.erase 1+ next drop ;

: b, over c! 1+ update ; ( a u -- a )
: header ( header-block -- )
  block 
  ( "eFORTH FFS HOWE " )
  $65 b, $46 b, $4F b, $52 b, $54 b, $48 b, $20 b,
  $46 b, $46 b, $53 b, $20 b, $48 b, $4F b, $57 b, 
  $45 b, $20 b,
  
  fs.version swap 16,
  fs.fat-size swap 16,
  fs.blk.start swap 16, 0 swap 16,
  fs.blk.end swap 16, 0 swap 16,
  \ Add time to header?
  \ Add dirty flag?
  \ Add CRC?
  flush
  drop ;

: k+ ; ( k u -- k u : increment to next block if u > b/buf )

: fat.format
  \ FAT Blocks == Special, Unreachable = Invalid

  1- for
  next
;

\ TODO: Adjust for fs.blk.start
: findex fepb /mod ; ( k -- u k )
: funidx fepb * + ; ( u k -- k )
: fat.find-free ;
: fat! findex swap block + 16! ; ( u k -- )
: fat.unallocate fat.free swap fat! ;
: fat? fs.blk.start fs.blk.end 1+ within 0= -1 and throw ;
: fat.next ; ( k -- k )
: fat.last ; ( k -- k )

: fat.foreach.cell ;

: fat.foreach ( k1 k2 xt -- n )
  dup fat? swap fat? swap
;

: format ( k u -- ior )
  2dup
  blk.erase.range
  drop header
  \ TODO: Create top level directory
  \ TODO: Set variables if not set to defaults
;

: mount ( k u -- ior  ) 
;

: cd ;
: ls ;
: mkdir ;
: rmdir ;
: rm ;
: sh ;
: stat ;

\ TODO: Attempt to mount, if fails, then attempt to format
fs.blk.start fs.blk.count format

\ Although standard words, these should be in their own
\ vocabulary so as to not conflict with Gforths implementation
\
\ : unimplemented abort" Not yet implement" ;
\ : bin unimplemented ;
\ : close-file unimplemented ;
\ : create-file unimplemented ;
\ : delete-file unimplemented ;
\ : file-position unimplemented ;
\ : file-size unimplemented ;
\ : file-status unimplemented ;
\ : flush-file unimplemented ;
\ : include-file unimplemented ;
\ : include unimplemented ;
\ : included unimplemented ;
\ : open-file unimplemented ;
\ : r/o unimplemented ;
\ : r/w unimplemented ;
\ : read-file unimplemented ;
\ : read-line unimplemented ;
\ : refill unimplemented ;
\ : rename-file unimplemented ;
\ : reposition-file unimplemented ;
\ : require unimplemented ;
\ : required unimplemented ;
\ : resize-file unimplemented ;
\ : source-id unimplemented ;
\ : w/o unimplemented ;
\ : write-file unimplemented ;
\ : write-line unimplemented ;

