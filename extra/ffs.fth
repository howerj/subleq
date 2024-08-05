\ # Simple Forth File System
\
\ * Author: Richard James Howe
\ * License: Public Domain / The Unlicense
\ * Repo: <https://github.com/howerj/ffs>
\ * Email: <mailto:howe.r.j.89@gmail.com>
\
\ This project contains a file system written for Forth systems
\ that uses Forth Blocks as the mechanism to store and retrieve
\ data meaning that this file should be fairly portable.
\
\ ## Format
\
\ The disk format consists of a series of 1024 byte, aligned,
\ Forth Blocks. The format is optimized to work with Forth
\ blocks and it is expected that a Forth interpreter is
\ available to execute files if needed.
\
\ Formerly the file system consisted of a maximum of 512 
\ blocks, which is the maximum number of 16-byte entries that 
\ can fit in a single block. This limited the maximum file 
\ system size to 512KiB. Now multiple blocks can be used to
\ store the FAT table meaning this file system can store
\ up to 64MiB of data (less some overhead and some blocks that
\ cannot be used).
\
\ The FAT is stored as a binary file. Directories are fixed
\ format text files. Files consist of potentially non
\ contiguous Forth Blocks, and thus files must be multiples
\ of 1024 bytes when allocating space to them (that is a file
\ that occupies 3 bytes of space must take up 1024 bytes,
\ excluding the directory entry itself).
\
\ The file system is designed to be forwards compatible to
\ a degree with various potential changes, or at least offer
\ a possible avenue for an upgrade.
\
\ The file system and utilities are designed for a single
\ user system. This is due to the fact that global variables
\ are used (this could be alleviated by using USER variables)
\ and there is no locking. There is currently no way to 
\ allocate another set of variables to mount a second file
\ system.
\
\ ### FAT - File Allocation Table
\
\ The FAT, or File Allocation Table, is a data-structure at
\ the heart of this file system. It was contained in a single
\ Forth block in this version of the file system and consisted
\ of 512 16-bit entries (which limits this file system 512KiB).
\ It now spans multiple blocks, upping the file system limit
\ to 64MiB. The FAT blocks have to be stored in a contiguous
\ fashion, the first of which is stored after the boot block.
\
\ After the last FAT block the first directory is stored.
\
\ The number of FAT blocks required for a file system is
\ the number of blocks allocated to that file system divided
\ by 512 and rounded up to the next block (e.g we need one
\ FAT block to store 1, 512, or 6 FAT entries, and we need two
\ to store 513 or 1024 FAT entries). There must be at least one
\ FAT block.
\
\ Each 16-bit entry is either a special value or a node in a
\ linked list of entries terminating in a special value.
\
\ Special values include; This block is free, this block is
\ bad (an error occurred reading or writing that block), this
\ block is unmapped (if fewer than 512 blocks are available in
\ any FAT block), or this block is special for some other 
\ reason (for example that block holds the FAT itself, the 
\ initial directory or the boot block). The use of special
\ values has largely been removed, even for FAT blocks,
\ instead a linked list is used and other means are used to
\ prevent those blocks from being freed or reallocated. This
\ allows the command line tools to be used to inspect those
\ data-structures if a special file is constructed that points
\ to them using `mknod`.
\
\ The block entry stored in the FAT table does not necessarily
\ directly correspond to the actual block number as stored on
\ disk. An offset is applied which is useful when the Forth
\ implementation is mapped on to some of the blocks or certain
\ blocks have special meaning (usually the 0 block) and cannot
\ be used.
\
\ The FAT is simply a linked list, it can be traversed in one
\ direction only (at least easily) which can slow down seeking.
\ It would be possible to speed this operation up by copying
\ the FAT into a different in memory data structure and syncing
\ that to disk when needed, this is not done as it adds
\ complexity and increases RAM usage. We could have also used
\ a XOR linked list (see the following Wikipedia article:
\ <https://en.wikipedia.org/wiki/XOR_linked_list>) in order
\ to speed up traversing the list in both directions, this
\ would have added unneeded complexity.
\
\ In order to traverse a linked list in reverse order a pointer
\ to the head of the list is needed. We can get to the previous
\ pointer by linking each node until we get to the current
\ node in the link, the link prior to this then becomes our
\ new position. This is an expensive operation, but not too
\ expensive and seeking like this should not be needed.
\
\ FAT-12, FAT-16 and FAT-32 are much more complex than this
\ file system, one more complexity of them is that they store
\ multiple copies of the entire FAT data-structure, often
\ just two, allowing recovery on failure of the main FAT
\ table. As there are only two copies there is no definitive
\ way of telling which one is correct however.
\
\ ### Text Directory Format
\
\ The directory format is incredibly simple, it consists of
\ fixed width text fields that are human and machine readable.
\
\        \ DIRNAME         BLK
\        F FILE.FTH        BLK
\        F FILE.TXT        BLK
\        D DIRECTORY       BLK
\        S FILE.SPC        BLK
\
\ The fields are:
\
\ * Directory entry type, which is two bytes in size.
\ * A file name which is 16 bytes in size.
\ * A block field which is 5 bytes in size.
\
\ The directories must be stored in a compact fashion with no
\ gaps, this matters when entries are removed from the 
\ directory.
\
\ The first entry in a directory contains a special 32-byte
\ field. It has the directory entry type of "\". It contains
\ a copy of the directories name which is used when printing
\ out the present working directory with `pwd`.
\
\ All other directory entries are also 32-bytes in size, 
\ 23 bytes are currently used. Non used bytes must be set to 
\ the space ASCII character.
\
\ File and directory names are 16-bytes in length always, even 
\ when the directory entry is a file name like "ABC.TXT", when 
\ stored as a directory entry the file is padded with trailing 
\ spaces up to the 16 byte limit. The file name is not 
\ separated into file-name and file-extension with an implied
\ "." like in FAT-12, FAT-16 or FAT-32.
\
\ The "BLK" fields are formatted as 16-bit unsigned hexadecimal 
\ numbers with a "$" prefix. They are the initial entries in a
\ FAT table, which may point to a FAT linked list, a special
\ block, or a sentinel value in the case that the file is a
\ single block in size.
\
\ All files must have associated blocks assigned to them, even
\ empty files. This may change in the future. That is a file
\ with a size of zero bytes in size has one backing block, 
\ much like a file with a 1024, 6, or 1000 bytes assigned to
\ it.
\
\ Directories consist of a single block, which limits their
\ size. This may change in the future.
\
\ Thirty one directory entries can fit in a single directory, 
\ which is not shown above as each entry being 32 bytes two 
\ entries fill and entire line which would push the line length 
\ over 64 in this document.
\
\ ### File Format
\
\ A file consists of a entry in a directory and a linked list
\ of blocks in the FAT. All files in this version of the file
\ system must be multiples of 1024 bytes in size. Files do not
\ not have to consist of a contiguous set of blocks, which 
\ marks the major advantage of this file system over 
\ traditional ways of managing Forth blocks.
\
\ Special files are marked with an "S" instead of an "F", many
\ but not all utilities will work with these files, and editing
\ these special files may cause instability.
\
\ ### Special Blocks
\
\ There are at least three special blocks, a boot block, a
\ FAT table, and the root directory. They are the first three
\ blocks in the file system.
\
\ ### Commands
\
\ The commands are implemented as a series of Forth words that
\ use parsing words to take their arguments from the input
\ stream if they have any, that is they do not expect their
\ arguments to be present on the stack.
\
\ There is a help and list of commands that is compiled into
\ the example image that is created at the end of this file,
\ along with an example executable program.
\
\ Commands take their names from Unix and MS-DOS, for example
\ both "ls" and "dir" are present (although they behave
\ slightly differently).
\
\ ### Block Editor
\
\ A block editor is included that works on files stored in a
\ non-contiguous fashion. It can also grow files. The editor
\ commands consist of single or double letter commands, it
\ is described later on, in a help file that is created on
\ disk.
\
\ ## Future Direction
\
\ As with any project there are many things that could be done,
\ only some of which will, writing down these potentialities
\ can be a sort of catharsis in lieu of doing the actual work
\ to resolve them. As to what can be done we could; implement
\ more commands, refactor many of the words, improve error 
\ handling and detection, calculate fragmentation and offer a 
\ way to defragment the file system, perform error checking on 
\ the file systems data-structures and orphaned nodes, offer a 
\ way to securely erase files, perhaps add checksums on the
\ file systems data structures and files, make a utility in C 
\ for manipulating the file system and importing and exporting 
\ files to it, allow zero length files, add more file system 
\ meta-data, rewrite `list` so we have more control over how 
\ things look like, make a better more DOS like shell, 
\ differing blocks sizes, a primitive journal just temporary 
\ FAT directories and more. 
\
\ The following provides a way to calculate fragmentation:
\ <https://stackoverflow.com/questions/4586972>, which will be
\ very slow on the SUBLEQ system.
\
\ Checksums, as mentioned, could be added to the file system
\ to give some guarantee that the data has not been corrupted.
\ If it were to be added adding it to the FAT, then to each
\ directory, and then to each file. Adding checksums to the
\ FAT would be easy, and little effort, adding to directories
\ still fairly easy, and to files it would involve a higher
\ effort, but more importantly the file systems performance
\ would suffer when writing data.
\
\ Some file system features there is no intention to ever
\ implement (such as hard or symbolic links), partly due to
\ file system limitations and partly due to a lack of need.
\
\ TODO: Document each section, capture GIF of usage, make
\ notes about file path parsing and C program.
\

\ ## Basic Word Definitions
\
\ If you can seen one Forth, you can seen one Forth. Words
\ which are deemed indispensable to one Forth programmer are
\ cast by the wayside by another. This section, common to
\ every non-trivial Forth program, defines basic words which
\ may or may not be present depending on which Forth this
\ program is running under.
\


\ `(order)`, `+order` and `-order` make using word-lists
\ easy, they should be standard, to back this with objective 
\ evidence I present the irrefutable - my opinion on the 
\ matter.
defined (order) 0= [if]
: (order) ( w wid*n n -- wid*n w n )
  dup if
    1- swap >r recurse over r@ xor
    if 1+ r> -rot exit then rdrop
  then ;
: -order get-order (order) nip set-order ; ( wid -- )
: +order dup >r -order get-order r> swap 1+ set-order ;
[then]

defined wordlist 0= [if]
: wordlist here cell allot 0 over ! ; ( -- wid : alloc wid )
[then]

\ The word `?\` offers another mechanism for conditional
\ compilation, an analogue using the parenthesis comment
\ method can also be defined like so:
\ 
\        : ?( ?exit postpone ( ;
\
\ Perhaps a word called `?:` could be defined, which would
\ be more complex, to mean "If following word is not defined
\ then define it". 
\
: ?\ ?exit postpone \ ; ( f "line"? -- )

defined eforth [if]
system +order
\ `quine` is useful for debugging under SUBLEQ eFORTH, it
\ echos the line just evaluated, gforth prints out more useful
\ error information on an error which SUBLEQ eFORTH lacks.
: quine source type cr ; ' quine <ok> !
[else]
\ Assume `gforth`, and use the file `ffs.db` to back the
\ file system.
use ffs.fb
[then]

defined ?depth 0= [if]
: ?depth depth 1- > -4 and throw ;
[then]

\ A lot of double cell arithmetic is used as on 16-bit 
\ platforms a single cell is not large enough to store a 
\ reasonable file size. SUBLEQ eFORTH is a 16-bit platform,
\ whereas GForth is 32 or 64 bit depending on the compilation
\ target.
\
defined du. 0= [if] : du. <# #s #> type ; [then]
defined d- 0= [if] : d- dnegate d+ ; [then]
defined d= 0= [if] : d= rot = -rot = and ; [then]
defined 2swap 0= [if] : 2swap >r -rot r> -rot ; [then]
defined d< 0= [if] 
: d< rot 2dup >
  if = nip nip if 0 exit then -1 exit then
  2drop u< ;
[then]
defined d> 0= [if] : d>  2swap d< ; [then]
defined dabs 0= [if] : dabs s>d if dnegate then ; [then]

cell 2 = ?\ $8000 constant #msb
cell 4 = ?\ $80000000 constant #msb
cell 8 = ?\ $8000000000000000 constant #msb
defined #msb 0= [if] abort" #msb not set" [then]

\ We only need to define "d2/" and "drshift" here (because it
\ is much more efficient than "um/mod" under SUBLEQ eFORTH),
\ but for the sake of completeness here are the other double
\ cell bitwise words which are often lacking.
\
\        : d2* over #msb and >r 2* swap 2* swap r> 
\            if 1 or then ;
\        : dlshift begin ?dup while >r d2* r> 1- repeat ;
\        : dand rot and >r and r> ; ( d d -- d )
\        : dor rot or >r or r> ; ( d d -- d )
\        : dxor rot xor >r xor r> ; ( d d -- d )
\
defined d2/ 0= [if]
: d2/ dup 1 and >r 2/ swap 2/ r> if #msb or then swap ;
[then]
: drshift begin ?dup while >r d2/ r> 1- repeat ; ( d u -- d )

defined 2over 0= [if]
: 2over >r >r 2dup r> swap >r swap r> r> -rot ;
[then]

defined 2swap 0= [if]
: 2swap >r -rot r> -rot ; ( n1 n2 n3 n4 -- n3 n4 n1 n2 )
[then]

defined dmax 0= [if]
: dmax 2over 2over d< if 2swap then 2drop ; ( d1 d2 -- d )
[then]

defined dmin 0= [if]
: dmin 2over 2over d> if 2swap then 2drop ; ( d1 d2 -- d )
[then]

defined b/buf 0= [if] 1024 constant b/buf [then]
defined d>s 0= [if] : d>s drop ; [then]

: dsignum ( d -- n : double cell signum function )
  2dup 0 0 d= if 2drop  0 exit then
       0 0 d< if       -1 exit then
                        1 ;

defined holds 0= [if]
: holds begin dup while 1- 2dup + c@ hold repeat 2drop ;
[then]

defined spaces 0= [if]
: spaces begin ?dup 0> while bl emit 1- repeat ;
[then]

\ SUBLEQ eForth lacks `s"`, instead using counted strings
\ where possible with `$"` and internally in `."`.
defined eforth [if] system +order [then]
defined s" 0= [if]
: s" 
  state @ if postpone $" [ ' count ] literal compile, 
  else 
    [char] " parse tuck here dup >r swap cmove r> swap dup 
    allot align
  then ; immediate
[then]

defined /string 0= [if] 
: /string ( b u1 u2 -- b u : advance string u2 )
  over min rot over + -rot - ;
[then]

\ If needed, `toggle` is:
\
\      : toggle tuck @ xor swap ! ; ( u a -- )
\
: set tuck @ or swap ! ; ( u a -- )
: clear tuck @ swap invert and swap ! ; ( u a -- )

\ `lower?` and `>upper` do not need defining, but are here
\ for the sake of completeness.
\
\        : lower? 97 123 within ; ( ch -- f )
\        : >upper dup lower? 32 and xor ; ( ch -- ch )
\
: upper? 65 91 within ; ( ch -- f )
: >lower dup upper? 32 and xor ; ( ch -- ch )

\ Case insensitivity is off by default and is less tested, so 
\ may cause problems. `icompare` implements a case insensitive
\ comparison, it is ASCII aware only and will not work for
\ UTF-8 (Well Unicode, not UTF-8).
\
: icompare ( a1 u1 a2 u2 -- n : string comparison )
  rot
  over >lower swap >lower swap - ?dup 
  if >r 2drop r> nip exit then
  >r
  begin
    r@
  while
    2dup c@ >lower swap c@ >lower swap - ?dup if
      rdrop nip nip exit
    then
    1+ swap 1+ swap
    r> 1- >r
  repeat rdrop 2drop 0 ;

\ `prefix` and `iprefix` attempts to find a match on a prefix
\ of a string (using the smallest string length), they will be 
\ used within `search` and `isearch`.
: prefix rot min tuck compare ; ( c1 u1 c2 u2 -- f )
: iprefix rot min tuck icompare ; ( c1 u1 c2 u2 -- f )

defined search 0= [if] \ Not defined in SUBLEQ eForth
: search ( c1 u1 c2 u2 -- c3 u3 f : find c2/u2 in c1/u1 )
  swap >r >r 2dup
  begin
    dup r@ >= over 0> and
  while
    2dup r> r> 2dup >r >r swap prefix
    0= if rot drop rot drop rdrop rdrop -1 exit then
    +string
  repeat
  2drop rdrop rdrop 0 ;
[then]

\ A case insensitive version of `search`. It is optionally
\ used along with `isearch`.
: isearch ( c1 u1 c2 u2 -- c3 u3 f : find c2/u2 in c1/u1 )
  swap >r >r 2dup
  begin
    dup r@ >= over 0> and
  while
    2dup r> r> 2dup >r >r swap iprefix
    0= if rot drop rot drop rdrop rdrop -1 exit then
    +string
  repeat
  2drop rdrop rdrop 0 ;

\ `untype` does the opposite of `type`, instead of displaying
\ string it gets bytes from the input stream and puts them
\ in a string.
: untype ( c-addr u -- remaining ior )
  dup >r
  begin
    dup
  while
    over key swap c!
    +string
  repeat 2drop r> 0 ;

\ Many basic string manipulation routines are missing within
\ Forth, and many proposals are lacking (e.g they allocate
\ memory dynamically, they are too large, weirdly named, too
\ particular to the idiosyncrasies of a single programmer, ...)
\
\ We need a few extra string manipulation routines, such
\ as `replace`, and we not be defining many more string
\ functions.
: replace ( c1 c2 c-addr u -- : replace c2 with c1 in string )
  begin
    ?dup
  while
    over c@ 3 pick = if over 4 pick swap c! then
    +string
  repeat drop 2drop ;

\ This section implements a Pseudo Random Number generator, it
\ uses the xor-shift algorithm to do so.
\
\ See:
\ * <https://en.wikipedia.org/wiki/Xorshift>
\ * <http://excamera.com/sphinx/article-xorshift.html>
\ * <http://xoroshiro.di.unimi.it/>
\ 
\ The constants used have been collected from various places
\ on the web and are specific to the size of a cell.
\ 
defined random 0= [if]
cell 2 = ?\ 13 constant #a 9  constant #b 7  constant #c
cell 4 = ?\ 13 constant #a 17 constant #b 5  constant #c
cell 8 = ?\ 12 constant #a 25 constant #b 27 constant #c
defined #a 0= [if] abort" Invalid Cell Size" [then]

variable seed 7 seed ! ( must not be zero )

: seed! ( x -- : set the value of the PRNG seed )
  dup 0= if drop 7 ( zero not allowed ) then seed ! ;

: random ( -- x : random number )
  seed @
  dup #a lshift xor
  dup #b rshift xor
  dup #c lshift xor
  dup seed! ;

wordlist constant {ffs}
{ffs} +order definitions
wordlist constant {dos}

\ We could save space by defining these words in a different
\ way, especially the list of strings containing error codes.
\ Space is more of premium under the 16-bit SUBLEQ eForth 
\ system where it might be worth eliding the error strings
\ completely and directly using the constants instead of
\ defining these constant words.
127
dup 1+ swap constant EUNKN ( unknown error )
dup 1+ swap constant EIBLK ( bad block )
dup 1+ swap constant EFILE ( file not found )
dup 1+ swap constant EFULL ( disk full )
dup 1+ swap constant EFSCK ( corrupt datastructure / disk )
dup 1+ swap constant EEXIS ( already exists )
dup 1+ swap constant EDDPT ( directory depth exceeded )
dup 1+ swap constant EFLEN ( file length )
dup 1+ swap constant EDFUL ( directory full )
dup 1+ swap constant EDNEM ( directory not empty )
dup 1+ swap constant ENFIL ( not a file )
dup 1+ swap constant ENDIR ( not a directory )
dup 1+ swap constant EARGU ( invalid argument )
dup 1+ swap constant EINTN ( internal error )
dup 1+ swap constant EINAM ( invalid name )
dup 1+ swap constant EPERM ( permission denied )
dup 1+ swap constant ELOCK ( could not obtain lock )
dup 1+ swap constant ERONY ( attempt to modify read-only FS )
dup 1+ swap constant EHAND ( file I/O error )
dup 1+ swap constant ESEEK ( not seekable )
drop

: e>s ( code -- )
  dup EUNKN = if drop s" unknown error" exit then
  dup EIBLK = if drop s" bad block" exit then
  dup EFILE = if drop s" file not found" exit then
  dup EFULL = if drop s" disk full" exit then
  dup EFSCK = if drop s" corrupt disk" exit then
  dup EEXIS = if drop s" already exists" exit then
  dup EDDPT = if drop s" directory depth exceeded" exit then
  dup EFLEN = if drop s" file length" exit then
  dup EDFUL = if drop s" directory full" exit then
  dup EDNEM = if drop s" directory not empty" exit then
  dup ENFIL = if drop s" not a file" exit then
  dup ENDIR = if drop s" not a directory" exit then
  dup EARGU = if drop s" invalid argument" exit then
  dup EINTN = if drop s" internal error" exit then
  dup EINAM = if drop s" invalid name" exit then
  dup EPERM = if drop s" permission denied" exit then
  dup ELOCK = if drop s" already locked" exit then
  dup ERONY = if drop s" read only" exit then
  dup EHAND = if drop s" file i/o error" exit then
  dup ESEEK = if drop s" not seekable" exit then
  drop s" unknown error code" ;

variable error-level 0 error-level !
: elucidate dup error-level ! ?dup if e>s type ." ?" then ;
: error swap if dup elucidate throw then drop ; ( f code -- )

\ In the FAT table any block number above, and including, 
\ the hexadecimal value `$FFF0` is treated as a special value.
\
\ This limits the number of blocks available to the system
\ and hence the ultimate file system size.

$FFF0 constant blk.lastv    \ Start of special block numbers
$FFFB constant blk.end      \ End of FAT chain
$FFFC constant blk.unmapped \ Unmapped / Not memory
$FFFD constant blk.bad-blk  \ Block is bad
$FFFE constant blk.special  \ Special blocks
$FFFF constant blk.free     \ Block is free to use

16 constant maxname         \ Maximum directory entry length
 8 constant maxdir          \ Maximum directory depth
b/buf constant #rem         \ Default Remaining/Used bytes
create dirstk maxdir cells allot dirstk maxdir cells erase
variable key-buf \ For `key-file` and `emit-file`

: namebuf: create here maxname dup allot blank does> maxname ;
namebuf: namebuf \ Used to store names temporarily
namebuf: findbuf \ Used to store file names for dir-find
namebuf: compbuf \ Used to store file names for validation
namebuf: movebuf \ Used to store file names for move/rename

32 constant dirsz                \ Length of directory entry
create dirent-store dirsz allot  \ Directory Stack
variable dirp 0 dirp !           \ Directory Stack Pointer
variable read-only 0 read-only ! \ Make file system read only 
$0100 constant version           \ File System version

: bbuf/ ( d -- rem quo : div/mod by 1024 )
  over 1023 and >r 10 drshift drop r> swap ;
: hbuf/ ( d -- rem quo : div/mod by 512 )
  over 511 and >r 9 drshift drop r> swap ; 
: fatcnt ( -- : FAT blocks need to store file system )
  0 hbuf/ ( b/buf 2/ um/mod ) swap 0<> negate + 1 max ;

\ This section contains system constants that defines what
\ sections of the file system go where.
\
\ It would be better if these constants were set a run time,
\ but it is not necessary. It does mean the user of this
\ program would have to tailor these constants, such as
\ `start` and `end` to their own purposes.
\
defined eforth [if]
1   constant start             \ Starting block
126 constant end               \ End block
65  constant init              \ Initial program block
$F000 constant copy-store      \ Used to copy blocks
[else]
1    constant start            \ Starting block
1024 constant end              \ End block
0    constant init             \ Initial program block
create copy-store b/buf allot  \ Used to copy blocks
[then]
init 1+ constant fat           \ FAT Block
end fatcnt constant fats       \ FAT Block Count
fat fats + constant dirstart   \ Top level directory
1 constant dsl                 \ Directory Start Line
16 constant l/blk              \ Lines per block
b/buf l/blk / constant c/blk   \ Columns per block
b/buf dirsz / constant d/blk   \ Directories per block
variable loaded 0 loaded !     \ Loaded initial program?
variable eline 0 eline !       \ Empty link in directory
variable insensitive 0 insensitive ! \ Case insensitivity
variable fatal 0 fatal ! \ Has a fatal error occurred?
variable handle -1 handle ! \ Used with `file:`
create linebuf c/blk allot  \ Used as temporary line buffer

\ The following variables are used for the file access routines
\ which build upon the file system.
8 constant fopen-max
7 cells constant fhandle-size
create fhandles fhandle-size fopen-max * dup cells allot 
       fhandles swap erase
create reqbuf maxname 1+ allot \ File name as a counted string
create newline 2 c, $D c, $A c, align

\ These are set later, they store the file handles that back
\ `stdin`, `stdout` and `stderr`, you can redirect them by
\ opening up a new file and storing the handle in one of these.
variable <stdin>  ( File handle for STDIN )
variable <stdout> ( File handle for STDOUT ) 
variable <stderr> ( File handle for STDERR )

\ These flags are used for the file handle flag field.
  1 constant flg.used   \ Is the file handle in use
  2 constant flg.ren    \ Read Enable
  4 constant flg.wen    \ Write Enable
  8 constant flg.stdin  \ Read from Stdin
 16 constant flg.stdout \ Write to Stdout
 32 constant flg.error  \ Error flag
 64 constant flg.eof    \ End Of File
128 constant flg.mem    \ Reserved for memory mapped files

\ Offsets into the file handle structure.
\
\ If we wanted or needed to we could add extra fields for
\ callbacks that would replace the default implementations
\ of reading, writing, setting and get the file position,
\ opening closing and resizing a file. This would allow us to
\ extend the File Access Methods to read from a chunk of
\ memory (one that could be reallocated) or write to a network
\ if needed. 

\ This would be useful in conjunction with pseudo
\ files (Special Files with a pointer to a variable containing
\ the callbacks that are used to implement that file) so we
\ could make a file that spat out random data when read, or
\ acted like it was always full, or contained infinite zeros.
\
\ It would also allow us to remove the special cases of
\ handling `flg.stdin` and `flg.stdout`.
\
: f.flags 0 cells + ; ( File Flags and Options )
: f.head 1 cells + ;  ( Head Block of file )
: f.end  2 cells + ;  ( Bytes in last block )
: f.blk  3 cells + ;  ( Current Block Position )
: f.pos  4 cells + ;  ( Position in bytes within block )
: f.dline 5 cells + ; ( Directory Line of File )
: f.dblk 6 cells + ;  ( Directory Block of File )

defined eforth [if] : numberify number? ; [else]
: numberify ( a u -- d -1 | a u 0 : easier than >number )
  -1 dpl !
  base @ >r
  over c@ [char] - = dup >r if +string then
  over c@ [char] $ = if hex +string 
    ( dup 0= if dup rdrop r> base ! exit then ) 
  then
  2>r 0 dup 2r>
  begin
    >number dup
  while over c@ [char] . <>
    if rot drop rot r> 2drop 0 r> base ! exit then
    1- dpl ! 1+ dpl @
  repeat
  2drop r> if dnegate then r> base ! -1 ;
[then]

: fvalid? dup 0 fopen-max 1+ within 0= throw ; 
: findex fvalid? fhandles swap fhandle-size * + ;
: fundex fhandles - fhandle-size / ;
\ N.B. We could lock files by making their type upper or lower
\ case, we would also need a command to force unlocking, 
\ perhaps one could be made to force file closing. Turning the
\ machine off and on also works however (or reopening the
\ program).
: locked? ( dir -- f )
  >r 0
  begin
    dup fopen-max <
  while
    dup findex f.dblk @ r@ = if rdrop drop -1 exit then
    1+
  repeat
  drop rdrop 0 ;
: ferase findex fhandle-size erase ; ( fhandle -- )
: force-unlock ( -- : force unlocking file system )
  1 ( ignore handle 0, as it contain stdin/stdout/stderr... )
  begin
    dup fopen-max <
  while
    dup ferase
    1+
  repeat drop ;
: locked!? locked? ELOCK error ; ( dir -- )
: equate insensitive @ if icompare exit then compare ;
: examine insensitive @ if isearch exit then search ;
: ro? read-only @ 0<> ERONY error ; ( -- f )
: nul? count nip 0= ; ( a -- f : is counted word empty? )
: token bl word dup nul? EARGU error ; ( -- b )
: grab ( <word> -- a : get word from input stream  )
  begin bl word dup nul? 0= ?exit drop query again ;
: integer grab count numberify nip dpl @ 0< and ; 
: integer? integer 0= -$18 and throw ; ( "int" -- )
: modify read-only @ ?exit update ;
: save read-only @ ?exit update save-buffers ;
: fatal? fatal @ 0<> throw ;
: block? ( blk -- blk )
  fatal?
  start + dup start end 1+ within 0= EIBLK error ;
: addr? block? [ ' block ] literal catch 0<> if
   \ We could mark the block as being bad so long as it is
   \ not a FAT block, otherwise that is a fatal error.
   -1 fatal ! -1 EIBLK error
 then ; ( blk -- addr )
: eline? eline @ ;
: little-endian base c@ 0<> ; ( -- f )
cell 2 = little-endian and [if]
: 16! fatal? ro? ! modify ;
: 16@ fatal? @ ;
[else]
: 16! fatal? ro? 2dup c! swap 8 rshift swap 1+ c! modify ;
: 16@ fatal? dup c@ swap 1+ c@ 8 lshift or ;
[then]

: fat? dup [ b/buf 2/ fats * ] literal u<= ; ( blk -- blk f )
: decompose [ b/buf 2/ ] literal /mod swap ; ( blk -- blk n )
: f@t ( blk -- u : read from FAT record )
  fat? 0= throw
  decompose 2* swap fat + addr? + 16@ ;
: f!t ( u blk -- : write to FAT record )
  fat? 0= throw
  decompose 2* swap fat + addr? + 16! modify ;
: linkable ( blk -- blk f )
  dup 1 end 1+ within 
  \ N.B. If `block.end` then we are linkable in a way, we
  \ could handle that.
  over f@t blk.lastv u< swap and ; ( blk -- blk f )
: link ( blk -- blk : load next block from FAT )
  linkable 0= if drop blk.end exit then f@t ; 
: previous ( head-blk prior-to-blk -- blk )
  swap
  begin
    2dup link = if
      nip
      exit
    then 
    link
    dup blk.end =
  until drop ;
: setrange ( val blk u : set block range in FAT to value )
  rot >r
  begin
    ?dup
  while
    over r@ swap f!t
    +string
  repeat drop rdrop ;
: btotal end start - ; ( -- n : total blocks allocatable )
: bcheck btotal 4 < -1 and throw ; ( -- )
: bblk addr? b/buf blank save ; ( blk -- : blank a block )
: fblk addr? b/buf erase save ; ( blk -- : erase a block )
: free? ( -- blk f : is a block free? )
  dirstart 1+
  begin
    dup end <
  while
    dup f@t blk.free = if -1 exit then
    1+
  repeat 2drop 0 0 ;
: balloc? ( -- blk -1 | -1 0 : allocate block w/ status code )
  free? 0= if -1 0 exit then ( failed to allocate block )
  dup blk.end swap f!t save -1 ;
: balloc ( -- blk : allocate single block )
  balloc? 0= EFULL error dup fblk ;
: btally ( blk-type -- n : count of blocks of type in FAT )
  >r 0 end begin
    dup start >=
  while 
    dup f@t r@ = if swap 1+ swap then
    1-
  repeat drop rdrop ;
: ballocs ( n -- blk : allocate `n` non-contiguous blocks )
  ?dup 0= EINTN error
  dup 1 = if drop balloc exit then
  dup blk.free btally > EFULL error
  blk.end swap
  1- for
    balloc tuck f!t
  next ;
: bvalid? ( blk -- blk : can we free this block? )
  dup dirstart <= EIBLK error
  dup link blk.special = EIBLK error
  dup end >= EIBLK error ;
: bfree ( blk -- : free a linked list ) 
  dup dirstart <= if drop exit then
  dup end >= if drop exit then
  \ Calling linkable here does not work, as blk.end is
  \ treated as not being linkable. This should probably be
  \ changed, but the current situation works.
  \ 
  \   linkable 0= if drop exit then )
  begin
  dup link swap blk.free swap bvalid? f!t
  dup blk.end = until drop save ; 
: bcount ( blk -- n )
  0 swap begin swap 1+ swap link dup blk.end = until drop ;
: btruncate ( n blk -- )
  >r dup 1 r@ bcount within 0= if rdrop drop exit then
  r@ swap r> swap
  1- for nip dup link
  next bfree blk.end swap f!t save ;
: reserve-range ( blk n -- : force allocate contiguous blocks )
  begin
    ?dup
  while
    dup 1 <= 
    if over blk.end swap 
    else over dup 1+ swap then f!t 
    1- swap 1+ swap
  repeat drop save ;
: fmt.init ( -- )
  init addr? b/buf blank
  \ A program that would be useful is one that would load
  \ the file system word-set form a series of contiguous blocks
  \ if the word-set was not present.
  s" .( HOWERJ SIMPLE FORTH FILE SYSTEM / DOS ) cr 1 loaded !" 
  init addr? swap cmove save ;
: fmt.fat ( -- )
  fat fats 1- for dup fblk 1+ next drop
  0 fats [ b/buf 2/ ] literal * 1- for
    blk.unmapped over f!t 1+
  next drop
  init if 0 init 1- reserve-range then
  fat fats reserve-range
  init 1 reserve-range
  dirstart 1 reserve-range
  dirstart 1+
  begin
    end start - over >
  while
    blk.free over f!t 1+
  repeat
  drop
  save ;
: fmt.blks ( -- )
  dirstart end dirstart - start - 1- for
    dup fblk 1+
  next drop ;
: apply ( file xt -- : apply execution token to file )
  >r begin dup r@ swap >r execute r> link dup blk.end = until 
  rdrop drop ;
: +list block? list ; ( blk -- )
: +load block? load ; ( blk -- )
: (grep) ( N.B - mcopy must hold search term )
  addr?
  l/blk 1- for
    dup c/blk movebuf -trailing examine nip nip if
      dup c/blk type cr
    then
    c/blk +
  next drop ;

cell 2 = [if] \ limit arithmetic to a 16-bit value
: limit immediate ; [else] : limit $FFFF and ; [then]

\ http://stackoverflow.com/questions/10564491
\ https://www.lammertbies.nl/comm/info/crc-calculation.html
: ccitt ( crc ch -- crc : Poly. 0x1021 AKA "x16+x12+x5+1" )
  limit over 8 rshift xor    ( crc x )
  dup  4  rshift xor         ( crc x )
  dup  5  lshift limit xor   ( crc x )
  dup  12 lshift limit xor   ( crc x )
  swap 8  lshift limit xor ; ( crc )

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

: link-load [ ' +load ] literal apply ; ( file -- )
: link-blank [ ' bblk ] literal apply ; ( file -- )
: more? key [ 32 invert ] literal and [char] Q = ;
: more> cr ." --- (q)uit? --- " ;
: moar +list more> more? ;
: link-more ( file -- ) 
  begin 
    dup moar if cr ." QUIT" drop exit then link dup blk.end = 
  until drop cr ." EOF" ; 
: fat-end ( blk -- blk : last block in FAT chain )
  begin dup link blk.end = if exit then link again ;
\ N.B. `fat-append` does not set the appended block to
\ `blk.end`, `balloc` does however. This is so another linked
\ list can be appended. It could set it intelligently 
\ however...
: fat-append fat-end f!t save ; ( blk file -- )
: contiguous? ( blk n -- f : is contiguous range free? )
  begin
    ?dup
  while
    over f@t blk.free <> if 2drop 0 exit then
    1- swap 1+ swap
  repeat drop -1 ;
: contiguous ( n -- blk f : find contiguous slab )
  ?dup 0= if 0 0 exit then
  >r
  dirstart 1+
  begin
    dup end <
  while
    dup r@ contiguous? if rdrop -1 exit then
    1+ ( This could be sped up by incrementing past failure )
  repeat rdrop drop 0 0 ;
\ Although `largest` has been sped up, it is still quite bad
\ and exhibits pathological behavior with a large number of
\ blocks that are fragmented. This could be sped up with a
\ binary search instead of a linear one, and improvements to
\ `contiguous`.
: largest ( -- n : largest block that we can allocate )
  blk.free btally 
  begin dup while dup contiguous nip ?exit 1- repeat ;
: cballoc ( n -- blk f : allocate contiguous slab )
  dup contiguous if tuck swap reserve-range -1 exit then 0 ;
: dirp? ( -- )
  dirp @ 0 maxdir within 0= if 0 dirp ! 1 EDDPT error then ;
: (dir) dirp? dirstk dirp @ cells + ;
: pushd (dir) ! 1 dirp +! ; ( dir -- )
: popd dirp @ if -1 dirp +! then (dir) @  ; ( -- dir )
: peekd popd dup pushd ; ( -- dir )
: nlen? dup maxname > -1 and throw ; ( n -- n )
: nclear namebuf blank ; ( -- )
: ncopy over namebuf drop = if 2drop exit then
  nclear nlen? namebuf drop swap cmove ; ( c-addr u )
: fclear findbuf blank ; ( -- )
: fcopy over findbuf drop = if 2drop exit then
  fclear nlen? findbuf drop swap cmove ; ( c-addr u )
: cclear compbuf blank ; ( -- )
: ccopy over compbuf drop = if 2drop exit then
  cclear nlen? compbuf drop swap cmove ; ( c-addr u )
: mclear movebuf blank ; ( -- )
: mcopy over movebuf drop = if 2drop exit then
  mclear nlen? movebuf drop swap cmove ; ( c-addr u )
: .hex base @ >r hex 0 <# # # # # #> type r> base ! ;
: hexp base @ >r hex 0 <# # # # # [char] $ hold #> r> base ! ;
: cvalid ( ch -- f : is character valid for a dir name? )
  dup 47 = if drop 0 exit then
  32 127 within ;
: nvalid? ( c-addr u -- f : is directory entry name valid? )
  ?dup 0= if drop 0 exit then
\ over c@ 32 <= if 2drop 0 exit then \ Should check all leading
  2dup s" ." ccopy compbuf equate 0= if 2drop 0 exit then
  2dup s" .." ccopy compbuf equate 0= if 2drop 0 exit then
  begin
   ?dup
  while
   over c@ cvalid 0= if 2drop 0 exit then
   swap 1+ swap 1-
  repeat drop -1 ;
: >la dup 0 d/blk 1+ within 0= -1 and throw dirsz * ;
: index >la swap addr? + ; ( dir line -- addr )
: dirent-type! index dup >r c! bl r> 1+ c! save ;
: dirent-type@ index c@ ; ( dir line -- type-char )
: dirent-name! >r >r 2dup nvalid? 0= EINAM error r> r> 
  index 2 + swap cmove save ;
: dirent-name@ index 2 + maxname ; 
: dirent-blk@ ( blk line -- n )
  index maxname + 2 + 5 numberify 0= throw d>s ;
: dirent-blk!  ( n blk line -- )
  index maxname + 2 + >r hexp r> swap cmove save ;
: dirent-rem@ ( blk line -- n )
  index maxname + 7 + 5 numberify 0= throw d>s ;
: dirent-rem! ( n blk line -- )
  index maxname + 2 + 5 + >r hexp r> swap cmove save ;
: dirent-erase ( blk line )
  >la swap addr? + dirsz blank save ; 
: >copy addr? copy-store b/buf cmove ;
: copy> addr? copy-store swap b/buf cmove modify ;
: >dir >la swap addr? + dirent-store dirsz cmove ; 
: dir> >la swap addr? + dirent-store swap dirsz cmove modify ;
: dirent ( c-addr u dir -- )
  >r fcopy
  [char] \ r@ 0 dirent-type!
  findbuf r@ 0 dirent-name!
  #rem r@ 0 dirent-rem!
  r@ r> 0 dirent-blk! 
  save ;
: fmtdir dup bblk dirent ; ( c-addr u dir -- )
: dir-find ( c-addr u blk -- line | -1 )
  >r fcopy
  \ We could extend this to not find much more if needed,
  \ the next line prevents files beginning with a space (or
  \ control characters).
  findbuf drop c@ bl <= if rdrop -1 exit then
  dsl ( skip first line at zero, this contains directory info )
  begin
    dup d/blk <
  while
    dup r@ swap dirent-name@ findbuf equate 
    0= if rdrop exit then
    1+
  repeat
  rdrop drop -1 ; 
: namelen ( c-addr u -- n : count until space )
  0 >r
  begin
    dup
  while
    over c@ bl <= if 2drop r> exit then
    r> 1+ >r
    1- swap 1+ swap
  repeat 2drop r> ;
: empty? ( blk -- line | -1 : get empty line )
  addr? dirsz dsl * + dsl ( skip first line )
  begin
   dup d/blk <
  while
   over c@ bl <= if nip exit then
   swap dirsz + swap 1+
  repeat
  2drop -1 ;
: is-unempty? empty? dsl <> ; ( blk -- f )
: fmt.root ( -- : format root directory )
  nclear namebuf dirstart
  fmtdir blk.end dirstart f!t save ;
: /root dirp @ for popd drop next ; ( -- navigate to root dir )
: dir? dirent-type@ [char] D = ; ( dir line -- f )
: special? dirent-type@ [char] S = ; ( dir line -- f )
: file? dirent-type@ [char] F = ; ( dir line -- f )
: (remove) ( dir line f -- )
  ro?
  2 pick locked!?
  >r 2dup dir? if
    r@ 0= ENFIL error
    2dup dirent-blk@ is-unempty? EDNEM error
  then
  rdrop
  2dup special? 0= if 
    2dup dirent-blk@
    \ For secure erase do `dup link-erase` before `bfree`.
    bfree 
  then
  2dup dirent-erase
  >r addr? r@
  dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
  - cmove
  rdrop save ;
 : (rm) ( f --, call narg before )
  >r namebuf peekd dir-find dup 0< EFILE error
  peekd swap r> (remove) ;
: (copy) ( src-blks dst-blks )
  swap
  begin
    dup >copy over copy>
    link swap link swap
    dup
    blk.end =
  until <> throw ;

\ ## Command Helper Words

: .type ( blk line -- : print directory entry type )
    2dup dir?     if ." DIR   " then
    2dup file?    if ." FILE  " then 
         special? if ." SPEC  " then ;

: .dir ( blk -- )
  cr
  ( ." /" dup 0 dirent-name@ type cr )
  dsl
  begin
    dup d/blk <
  while
    2dup dirent-type@ bl <= if 2drop exit then
    2dup .type
    2dup dirent-name@ type space
    2 spaces
    2dup special? if
      2dup dirent-blk@ ." *" u. 
    else
      ( previously just `2dup dirent-blk@ bcount u.` )
      2dup dirent-blk@ bcount 1- >r
      2dup dirent-rem@ 0 r> b/buf um* d+ du.
    then
    cr
    1+
  repeat 2drop ;
: narg token count ncopy ; ( "token" -- )
: (entry) ( dir, "file" -- blk line )
  >r narg
  namebuf r@ dir-find dup 0< EFILE error r> swap ;
: (file) ( "file" -- blk )
  peekd (entry)
  2dup dir? ENFIL error
  dirent-blk@ ;
: found? peekd eline? ; ( -- cwd line )
: dfull? empty? dup eline ! -1 = EDFUL error ; ( blk -- )
: full? peekd dfull? ; ( -- : is cwd full? )
: (create) ( -- blk: call narg prior, create|open existing )
  namebuf peekd dir-find dup 
  0>= if peekd swap dirent-blk@ exit then
  drop
  full?
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  balloc dup link-blank found? dirent-blk!
  [char] F found? dirent-type!
  found? dirent-blk@ ;
: (round) ( -- : call narg prior, round up file to blk size )
  namebuf peekd dir-find dup 0< ENFIL error
  #rem swap peekd swap dirent-rem! ;
: (mkfile) ( n -- : `narg` should have name in it )
  >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  r@ 0<> #rem and found? dirent-rem!
  r> dup 0= 1 and + ballocs dup link-blank found? dirent-blk!
  [char] F found? dirent-type! ;
: (deltree) ( dir -- : recursive delete of directory )
  >r
  begin
    r@ is-unempty? 0= if rdrop exit then
    ." DEL: " r@ dsl dirent-name@ type cr
    r@ dsl dir? if
      r@ dsl dirent-blk@ recurse
      r@ dsl 1 (remove)
    else
      r@ dsl 0 (remove)
    then
  again ;

: yes? if s" yes" exit then s" no" ; ( f -- c-addr u )

\ ## File Access Methods
\
\ The File Access Words / Methods (FAM) are standard, but
\ optional, Forth words that provide a way of interacting with
\ the file system. They are analogous to the C standard library
\ words present in the `stdio.h`.
\
\ See <https://forth-standard.org/standard/file> for the
\ Standard ANS Forth File Access words and
\ <https://cplusplus.com/reference/cstdio> for the C file
\ API.
\
\ Useful to bear in mind is the File Handle Structure:
\
\        FLAGS:    16/cell
\        HEAD-BLK: 16/cell
\        BLK-END:  16/cell
\        BLK:      16/cell
\        BLK-POS:  16/cell
\        DIR-LINE: 16/cell
\        DIR-BLK:  16/cell
\
\ Some useful debug words for printing out file system
\ information:
\
\        : flag ( flg ch -- )
\          swap if emit exit then drop ." -" ; 
\        : .flag
\          dup flg.used   and [char] U flag
\          dup flg.ren    and [char] R flag
\          dup flg.wen    and [char] W flag
\          dup flg.stdin  and [char] I flag
\          dup flg.stdout and [char] O flag
\          dup flg.error  and [char] ! flag
\          dup flg.eof    and [char] E flag
\          drop ;
\        : .fhandle ( findex -- )
\          cr
\          dup f.flags @ ." FLG: " dup u. ." -> " .flag cr
\          dup f.head  @ ." HED: " u. cr
\          dup f.end   @ ." END: " u. cr
\          dup f.blk   @ ." BLK: " u. cr
\          dup f.pos   @ ." POS: " u. cr
\          dup f.dline @ ." DLN: " u. cr
\          dup f.dblk  @ ." DBK: " u. cr
\          drop ;
\
\ ### Include/Require
\
\ `entry` and `required?` are used by `included`, `include`,
\ `require` and `required` in order to prevent loading the
\ same file twice when using `require` or `required`. They do
\ this by adding a word to a wordlist that has the same
\ name as the file being executed, this wordlist can be checked
\ by future calls to `required?`.
\       
wordlist constant {required}
\ This prints out the files that have been `included` or
\ `required` in the past.
\
\        : .require 
\          get-order {required} 1 set-order words set-order ;
\
\ This really should turn a file name into a canonical file
\ name and then remember that.
\
: entry ( cs -- f : create a new word if one does not exist )
  dup >r find nip ?dup if rdrop exit then
  <# r> count holds s" create  " holds 0 0 #> evaluate 0 ;
: required? ( cs -- f : add to required list in {required} )
  >r get-order {required} +order definitions r> entry >r
  set-order definitions r> ;

\ ## File Access Methods
\
\ This section contains helper words for the File Access 
\ Methods and then the File Access Methods themselves, which
\ are standard Forth words, albeit extension words and thus
\ are sometimes not present.
\
\ Missing is a way of opening memory up as a file, this would
\ require us to add a level of indirection on many of the File
\ Access Methods and store Execution Tokens within the file
\ handle structure.
\
: unused? ( -- ptr f : find a free handle if one exists )
  0
  begin
   dup fopen-max <
  while
   dup findex f.flags @ 0= if -1 exit then
   1+
  repeat
  drop -1 0 ;
: handles? ( -- u : count of free handles )
  0 >r 0
  begin
   dup fopen-max <
  while
    dup findex f.flags @ 0= if r> 1+ >r then
    1+
  repeat drop r> ;
: take ( -- ptr f : take a free handle if one exists )
  unused? 0= if 0 exit then
  dup ferase
  dup findex f.flags flg.used swap set -1 ;
: pack ( c-addr u )
  reqbuf maxname 1+ blank
  nlen?
  dup reqbuf c!
  reqbuf 1+ swap cmove ;
: (stdio) flg.stdout flg.stdin or ; ( -- u )
: stdio flg.wen flg.ren or (stdio) or ; ( -- u )
: fam? dup stdio invert and 0<> throw ; ( fam -- fam )
: ferror findex f.flags @ flg.error and 0<> ; ( handle -- f )
: fail f.flags flg.error swap set ; ( findex -- )
: nlast? f.blk @ link blk.end = ; ( blk -- f )
: limit? dup nlast? if f.end @ exit then drop b/buf ;
: fremaining dup >r f.pos @ r> limit? swap - 0 max ;
: nblock ( u findex -- f )
  >r r@ f.pos +!
  r@ f.pos @ r@ limit? >= if
    r@ nlast? if
      flg.eof r@ f.flags set
      rdrop 0 exit
    then
    r@ limit? r@ f.pos @ swap - r@ f.pos !
    r@ f.blk @ link r@ f.blk !
  then
  rdrop -1 ;
: stretch f.pos @ b/buf swap - ;

defined eforth [if] forth-wordlist +order definitions [then]

flg.ren constant r/o    ( -- fam : read only )
flg.wen constant w/o    ( -- fam : write only )
r/o w/o or constant r/w ( -- fam : read/write )

: bin fam? ; ( fam -- fam )
: open-file ( c-addr u fam -- fileid ior ) 
  fam?
  dup w/o and if 
    read-only @ if 2drop drop -1 ERONY exit then 
  then
  >r ncopy namebuf s" ." fcopy findbuf equate 0= if 
     take 0= if rdrop -1 EHAND exit then
     dup findex f.flags
     r> flg.stdin or flg.stdout or swap set
     0
     exit
  then
  namebuf peekd dir-find dup 0< if 
    rdrop drop -1 EFILE exit 
  then
  \ We might want to be able to open up directories, at least
  \ in a read-only manner, this can be done by removing this
  \ line.
  peekd over dir? if rdrop drop 0 ENFIL exit then
  take 0= if rdrop 2drop -1 EHAND exit then
  dup r> swap >r >r
  findex r> over f.flags set
  >r
  dup r@ f.dline !
  peekd r@ f.dblk !
  peekd over dirent-rem@ r@ f.end !
  peekd over dirent-blk@ dup r@ f.blk ! r@ f.head !
  drop rdrop r> 0 ; 
: file-exists? ( c-addr u -- f )
  ncopy namebuf peekd dir-find dsl >= ;
: file-eof? findex f.flags @ flg.eof and 0<> ; ( fileid -- f )
: file-error? ( fileid -- f )
  findex f.flags @ flg.error and 0<> ;
: create-file ( c-addr u fam -- fileid ior )
  ro? fam? >r 2dup ncopy full? 0 [ ' (mkfile) ] literal catch
  ?dup if nip nip nip -1 swap rdrop exit then save
  r> open-file ; 
: flush-file ( fileid -- ior )
  save
  dup ferror if drop EHAND exit then
  findex >r
  r@ f.flags @ flg.used and 0= if rdrop EHAND exit then
  r@ f.end @ r@ f.dblk @ r@ f.dline @ dirent-rem!
  rdrop 0 ;
: close-file ( fileid -- ior )
  dup findex f.flags @ flg.used and 0= if drop EHAND exit then
  dup flush-file swap ferase ;
: file-size ( fileid -- ud ior ) 
  dup ferror if drop 0 0 EHAND exit then
  findex dup f.head @ bcount swap 
  f.end @ >r b/buf um* r> b/buf swap - 0 d- 0 ; 
: refill query -1 ; ( -- flag )
: include-file ( fileid -- )
  dup ferror EHAND error
  findex 
  dup f.flags @ flg.ren and 0= EPERM error
  dup f.flags @ flg.stdin and 0<> EPERM error
  f.head @ link-load ; 
: included ( c-addr u -- )
  -trailing pack reqbuf required? drop reqbuf count
  ncopy namebuf peekd dir-find dup 0< EFILE error
  peekd swap dirent-blk@ link-load ; 
: include token count included ; ( "file" -- )
: required ( c-addr u -- : execute file, only once )
  pack reqbuf dup count mcopy required? ?exit reqbuf count
  included ;
: require ( "name" -- : execute file, only once )
  token dup count mcopy
  required? ?exit movebuf included ;
: rename-file ( c-addr1 u1 c-addr2 u2 -- ior ) 
  ro?
  peekd locked!?
  mcopy ncopy
  namebuf peekd dir-find dup >r 0< if rdrop EFILE exit then
  movebuf peekd dir-find 0>= if rdrop EEXIS exit then
  peekd r@ dir? if
    findbuf peekd r@ dirent-blk@ 0 dirent-name!
  then
  findbuf peekd r> dirent-name! 0 ;
: delete-file ( c-addr u -- ior )
  ncopy 0 [ ' (rm) ] literal catch dup if nip then ; 
: file-position ( fileid -- ud ior ) 
  dup ferror if drop 0 0 EHAND exit then
  findex >r
  r@ f.flags @ (stdio) and 0<> if rdrop 0 0 ESEEK exit then
  0 r@ f.head @ begin
    dup r@ f.blk @ <>
  while
    link dup blk.end = if 2drop rdrop 0 0 EHAND exit then
    swap 1+ swap
  repeat drop 
  b/buf um*
  r@ f.pos @ 0 d+ rdrop 0 ;
: file-length ( fileid -- ud ior )
  dup ferror if drop 0 0 EHAND exit then
  findex >r
  r@ f.flags @ (stdio) and if rdrop 0 0 EPERM exit then
  r@ f.head @ bcount 1- b/buf um*
  r> f.end @ 0 d+
  0 ;
: file-status ( c-addr u -- x ior )
  r/o open-file ?dup if exit then
  close-file 0 swap ;
: read-file ( c-addr u fileid -- u ior )
  dup ferror if 2drop drop 0 0 EHAND exit then
  over >r >r 2dup erase r>
  findex dup f.flags @ flg.ren and
  0= if rdrop 2drop drop 0 EPERM exit then
  dup f.flags @ flg.stdin and if rdrop drop untype exit then
  >r
  begin
    ?dup
  while ( c-addr u )
    r@ fremaining over min ( c-addr u min )
    r@ f.blk @ addr? r@ f.pos @ + ( c-addr u min baddr )
    swap ( c-addr u baddr min ) 3 pick swap
    dup >r cmove r>
    dup r@ nblock
    0= if 
      rdrop nip nip r> min 0 exit
    then
    /string
  repeat
  rdrop drop r> 0 ;
: read-line ( c-addr u fileid -- u flag ior ) 
  dup ferror if 2drop drop 0 0 EHAND exit then
  over >r >r
  begin
    ?dup
  while
    over 1 r@ read-file ?dup if
      nip rdrop r> swap - swap rot drop 0 swap exit
    then
    0= if nip rdrop r> swap - 0 0 exit then
    \ For a newline we get `$A` in SUBLEQ eForth (Linux) and 
    \ Windows Gforth 0.7.0, `$D` in Linux Gforth 0.7.3, so we
    \ need to handle both cases. We should really be ignoring
    \ `$D` or adding both `$A` and `$D` to the buffer.
    \ 
    \ The code really should be:
    \ 
    \   over c@ $A = if  
    \     nip rdrop r> swap - -1 0 exit then
    \   over c@ $D <> if +string then
    \ 
    over c@ ( dup $D = swap ) $A = ( or ) if 
      nip rdrop r> swap - -1 0 exit then
    +string
  repeat
  drop
  rdrop r> 0 0 ; 
: write-file ( c-addr u fileid -- ior ) 
  dup ferror if 2drop drop EHAND exit then
  findex dup f.flags @ flg.wen and 
  0= if 2drop drop EPERM exit then
  dup f.flags @ flg.stdout and if drop type 0 exit then
  >r
  begin
    ?dup
  while ( c-addr u )
     r@ stretch 0= if 
       r@ nlast? if
         balloc? 0= if 2drop r> fail EFULL exit then
         r@ f.blk @ fat-append
       then
       r@ f.blk @ link r@ f.blk !
       0 r@ f.pos !
       0 r@ f.end !
     else
        dup r@ stretch min >r over r> ( c-addr u c-addr v )
        r@ f.blk @ addr? r@ f.pos @ + swap dup >r cmove r> 
        ( c-addr u v )
        modify
        dup r@ f.pos +!
        /string
        r@ nlast? if r@ f.pos @ r@ f.end @ max r@ f.end ! then
     then
  repeat
  r@ fundex flush-file throw
  rdrop drop 0 ;
: key-file ( file-id -- key : retrieve a single byte )
  >r key-buf 1 r> read-file 0<> swap 1 <> or if -1 exit then
  key-buf c@ ;
: emit-file ( ch file-id -- ior : write a single byte )
  swap key-buf c! >r key-buf 1 r> write-file ;
: write-line ( c-addr u fileid -- ior )
  dup >r write-file ?dup if rdrop exit then
  newline count r> write-file ;  
\ A version that acted more like `fseek`, from C, in that it
\ accepted `SEEK_SET`, `SEEK_CUR`, and `SEEK_END` could be
\ build upon these words.
: reposition-file ( ud fileid -- ior ) 
  dup ferror if 2drop drop EHAND exit then
  findex dup >r f.flags flg.eof swap clear
  r@ f.flags @ (stdio) and 0<> if rdrop 2drop ESEEK exit then
  r@ fundex file-size ?dup if rdrop 2drop 2drop exit then
  dmin
  bbuf/ swap ( cnt rem ) ( b/buf um/mod swap )
  r@ f.pos ! ( cnt )
  r@ f.head @ swap
  ?dup if 1- for link next then
  r> f.blk ! 0 ; 
: resize-file ( ud fileid -- ior )
  dup ferror if 2drop drop EHAND exit then
  findex >r
  r@ f.flags @ w/o and 0= if rdrop 2drop EPERM exit then
  r@ f.flags @ (stdio) and if rdrop 2drop ESEEK exit then
  r@ fundex file-length ?dup if 
    rdrop >r 2drop 2drop r> exit 
  then
  2>r 2dup 2r> d- dsignum dup 0= if
    drop 2drop rdrop 0 exit
  then
  0< if \ Shrink
    bbuf/ 1+ ( b/buf um/mod 1+ )
    ?dup if r@ f.blk @ btruncate then 
    r@ f.end !
    \ After `resize-file` `file-position` is unspecified, we
    \ could improve this but the current behavior is allowed
    \ by the standard.
    r@ f.head @ r@ f.blk !
    0 r@ f.pos !
    r> fundex flush-file
    exit
  then \ Grow
  r@ fundex file-length ?dup if \ Get length...again
    rdrop >r 2drop 2drop r> exit
  then
  ( d- b/buf um/mod ?dup if )
  d- bbuf/ ?dup if
    ballocs r@ f.head @ fat-append
  then
  r@ f.end +! r@ f.end @ b/buf > if
    balloc? 0= if r> fail EFULL exit then
    r@ f.head @ fat-append
    b/buf negate r@ f.end +!
  then
  r@ fundex flush-file
  rdrop ;
: recreate-file ( c-addr u fam -- handle ior )
  >r 2dup file-exists?
  if 2dup delete-file ?dup if rdrop >r 2drop r> exit then then
  r> create-file ;
: rewind-file >r 0 0 r> reposition-file ; ( file -- ior )
: end-file ( file -- ior : move to the end of a file )
  dup >r file-length ?dup if 2drop rdrop exit then
  r> reposition-file ;
: append-file ( c-addr u fam -- file ior :  )
  open-file ?dup ?exit >r
  r@ end-file ?dup if r> close-file drop exit then
  r> 0 ;
: line-end? ( u flag ior -- u f : read-line at EOF? )
  ?dup if nip exit then
  0= swap tuck 0= and ;

\ ## Utility Helper Functions
\
\ We can now build utilities using these File Access Methods,
\ along with more primitive ways of directly accessing the
\ file system. We are free to mix and match here. There is
\ no real structure to this section, there is a menagerie of
\ helping words, often one per command that will be implemented
\ later, for example `(cmp)` is used to implement the command
\ `cmp`. Prior sections are also composed of helper words
\ for the next section.
\

{ffs} +order definitions

: remaining source nip >in @ - ; ( -- n )
: leftovers source nip remaining - >r source r> /string ;
: -handle -1 handle ! ; ( -- )
: discard source nip >in ! ; ( -- )

: (cksum) ( file-id -- cksum )
  >r
  $FFFF
  begin r@ key-file dup 0>= while ccitt repeat
  drop rdrop ;

: (cmp) ( file-id file-id -- file-id file-id )
  0 >r
  begin
    2dup key-file swap key-file
    tuck <> swap 0< swap if
      drop
      cr ." FILES DIFFER BYTE: " r> u. cr
      exit
    then
    r> 1+ >r
  until
  rdrop ;

: (b2f) ( handle handle -- error handle handle )
  begin
    linebuf c/blk 3 pick read-file 0<> swap 0= or if
      0 -rot exit
    then
    linebuf c/blk -trailing 2 pick write-line ?dup
  until -rot ;

\ N.B. This will fail if the line length approaches 64 chars.
: (f2b) ( handle handle -- error handle handle )
  begin
    linebuf c/blk blank
    linebuf c/blk 3 pick read-line drop nip 0= if
      0 -rot exit
    then
    bl $A linebuf c/blk replace
    bl $D linebuf c/blk replace
    linebuf c/blk 2 pick write-file ?dup 
  until -rot ;

: open-or-create-file ( c-addr u fam -- handle ior )
  >r 2dup file-exists? r@ w/o and 0= or if 
    r> open-file exit then 
  r> create-file ;

: with-files ( "file" "file" xt -- )
  >r
  token count r/w open-file throw 
  token count r/w open-or-create-file ?dup 
    if swap close-file drop exit then
  r> execute close-file swap close-file throw throw throw ;

: (hexhump) ( c-addr u f -- : hex dump a file, optional addr )
  >r r/o open-file throw r> 
  base @ >r hex
  >r >r
  0 begin
    r@ key-file dup 0>=
  while
    swap dup $F and 0= if
      cr r> r> tuck >r >r if dup 4 u.r ." : " then
    then swap
    \ N.B. Under SUBLEQ eForth `.` is much faster.
    0 <# bl hold # #s #> type
    1+
  repeat 2drop
  r> close-file rdrop r> base ! throw ;

: blksum ( blk cksum -- cksum : calculate block checksum )
  swap addr? b/buf 1- for
    count swap >r ccitt r> 
  next drop ;
: blksums ( blk u -- cksum : calculate checksum over blocks )
  $FFFF >r
  begin
    ?dup
  while
    over r> blksum >r
    +string
  repeat drop r> ;
: dirsum ( $FFFF dirstart -- u : checksum over directories )
  >r r@ swap blksum
  dsl
  begin
    dup d/blk <
  while
    r@ over dirent-type@ bl <= if drop rdrop exit then
    r@ over dir? if
      r@ over dirent-blk@ >r swap r> recurse swap
    then
    1+
  repeat drop rdrop ; 

\ ## Block Editor
\
\ This implements a block editor that operates on discontinuous 
\ blocks. SUBLEQ eForth contains a block editor that operates
\ on continuous blocks if one is needed, as it allows the
\ blocks to be directly addressed which is hidden from the
\ user with this editor.
\
\ There is a help section later on in this file for this 
\ editor. New blocks will be automatically allocated as needed.
\
\ A line oriented editor could be made as we have the file
\ access words to do so now.
\
\ All the editor commands are contained within their own
\ `{edlin}` vocabulary.
\
\ A command to delete the current working block would be 
\ useful, instead of the `r` command which is a poor facsimile.
\
\ In order to lock the file we are editing we open up a file
\ handle using `open-file` later when we call `edlin` and store
\ it in `flock`. Otherwise the File Access Methods are not used
\ for editing.
\
wordlist constant {edlin}
{edlin} +order definitions
variable vista 1 vista ! \ Used to be `scr`
variable head 1 head !
variable line 0 line !
variable flock -1 flock !
: s save ; ( -- : save edited block )
: q s [ {edlin} ] literal -order ( -- : quit editor )
  flock @ close-file -1 flock ! throw ( dos ) ; 
: ? head @ . vista @ . line @ . ; ( -- : print blk and line )
: l vista @ block? list ; ( -- : list current block )
: x q head @ link-load
    [ {edlin} ] literal +order ; ( -- : exe file )
: ia 2 ?depth 
  [ $6 ] literal lshift + vista @ addr? + tib
  >in @ + swap source nip >in @ - cmove tib @ >in ! ;
: a 0 swap ia ; : i a ; ( line --, "line" : insert line at )
: w get-order [ {edlin} ] literal 1 ( -- : list cmds )
     set-order words set-order ; 
: n 0 line ! s vista @ link blk.end = if 
    balloc dup bblk head @ fat-append 
  then
  vista @ link vista ! ( l ) ;
: p ( -- : prev block )
  0 line ! s head @ vista @ previous vista ! ( l ) ; 
: y vista @ >copy ;
: u vista @ copy> save ;
: z vista @ addr? b/buf blank ; ( -- : erase current block )
: d 1 ?depth >r vista @ addr? r> [ $6 ] literal lshift +
   [ $40 ] literal blank ; ( line -- : delete line )
: - line @ -1 line +! line @ 0< if l/blk 1- line ! p then ;
: + line @ a 1 line +! line @ l/blk >= if 0 line ! n then ;
: r head @ bcount 1- head @ btruncate head @ vista ! ;
{ffs} +order definitions
: edlin ( BLOCK editor )
  flock !
  vista ! head ! 0 line ! ( only ) [ {edlin} ] literal +order ; 
{edlin} -order

\ ## LZP Compression CODEC
\
\ This code uses LZP to make a compression CODEC, for more
\ information see <https://github.com/howerj/lzp>, and
\ <https://github.com/howerj/shrink>. The CODEC is very
\ simple and does not have that great of a compression ratio.
\
\ LZSS has also been considered but even that (simple) CODEC
\ is much larger.
\

wordlist constant {lzp} {lzp} +order definitions

8 constant model-bits \ log_2(model-size)
1 model-bits lshift constant model-size
create model model-size allot align \ Sie ist ein Model...
create buf 9 allot align \ Control Byte + Up to 8 literals
variable bufp       \ Pointer into `buf`
variable fin        \ End of Input?
variable infile     \ File handle for input
variable outfile    \ File handle for output
variable icnt       \ Bytes in input text
variable ocnt       \ Bytes in output text
variable prediction \ Current prediction state / hash value
variable run        \ Do we have a run of data?

: +hash  ( h c -- h : our predictor model, a simple hash )
  swap 4 lshift xor [ model-size 1- ] literal and ; 

\ Needed for gforth `key-file` that comes with gforth and uses
\ the systems file system, not the one provided by `ffs` which
\ behaves differently.
\
\        : get ( file -- c|-1 )
\          infile @ key-file dup 0< if fin ! exit then 
\          infile @ file-eof? if drop -1 fin ! -1 exit then
\          1 icnt +! ;
\
: get ( file -- c|-1 )
  infile @ key-file dup 0< if -1 fin ! exit then 1 icnt +! ;
: put outfile @ emit-file throw 1 ocnt +! ; ( c -- )
: predict prediction @ swap +hash prediction ! ; ( c -- )
: reset ( -- : reset LZP model and variables )
  0 prediction !
  0 fin ! 
  0 icnt ! 0 ocnt !
  buf 9 erase
  model model-size erase ;
: model? prediction @ model + c@ ; ( -- c )
: model! prediction @ model + c! ; ( c -- )
: breset 1 bufp ! 0 buf c! 0 run ! ;
: wbuf 
  run @ if
    buf bufp @ outfile @ write-file throw
    bufp @ ocnt +!
  then breset ;
: lzp-encode ( -- ior : LZP compress, set file handles before )
  reset
  begin
    breset
    0 begin
      dup 8 < fin @ 0= and
    while
      get >r r@ 0< if wbuf rdrop drop 0 exit then
      -1 run !
      r@ model? = if \ Match! Set bit in control byte
        dup 1 swap lshift buf c@ or buf c!
      else \ No match, update model and output literal byte
        r@ model! 
        r@ buf bufp @ + c! 
        1 bufp +!
      then 
      r> predict \ Update hash either way
      1+
    repeat drop
    wbuf
  fin @ until 0 ;
: lzp-decode ( -- ior : LZP decompress )
  reset
  begin
    get dup 0< if drop 0 exit then
    0 begin
      dup 8 <
    while
      2dup 1 swap lshift and if
        model?
      else
        get dup 0< if drop 2drop 0 exit then
        dup model!
      then
      dup put predict
      1+
    repeat 2drop
  fin @ until 0 ;

: lzp-statistics ( -- )
  cr
  ." in : " icnt @ . cr
  ." out: " ocnt @ . cr ;

: lzp-file outfile ! infile ! lzp-encode ;
: unlzp-file outfile ! infile ! lzp-decode ;
  
: lzp-file-name ( "from" "to" -- ior )
  w/o recreate-file ?dup if nip exit then outfile !
  r/o open-file ?dup if nip outfile @ close-file drop exit then
  infile !
  [ ' lzp-encode ] literal catch
  infile @ close-file 0 infile !
  outfile @ close-file 0 outfile !
  ?dup if nip nip exit then
  ?dup if nip exit then ;

: unlzp-file-name ( "from" "to" -- ior )
  w/o recreate-file ?dup if nip exit then outfile !
  r/o open-file ?dup if nip infile @ close-file drop exit then
  infile !
  [ ' lzp-decode ] literal catch
  infile @ close-file 0 infile !
  outfile @ close-file 0 outfile !
  ?dup if nip nip exit then
  ?dup if nip exit then ;

\ ## DOS Commands
\
\ This section has a series commands that form the interface
\ between the file system and the user. These are the words
\ that form the core of this module and what everything has
\ been building up to.
\
\ Although a shell word could be made that parses user input
\ and passes the arguments to functions to keep things simple
\ the parsing and execution of commands is done by the built
\ in mechanisms available to Forth.
\

{dos} +order definitions

: df cr ( -- : list disk usage and file system info )
  loaded @ 0= if ." NO DISK" cr exit then
  ." MOUNTED" cr
  ." VERSION:     " version .hex cr
  ." BLK SZ:      " b/buf u. cr
  ." START BLK:   " start u. cr
  ." END BLK:     " end u. cr
  ." MAX DIRS:    " maxdir u. cr
  ." MAX:         " end start - dup . ."  /" b/buf * u. cr
  ." FREE:        " blk.free btally dup . ."  /" b/buf * u. cr 
  ." BAD BLKS:    " blk.bad-blk btally u. cr 
  ." FILES OPEN:  " fopen-max handles? - u. cr
  ." MAX HANDLES: " fopen-max u. cr
  ." LARGEST CONTIGUOUS BLOCK: " largest u. cr 
  ." READ ONLY:    " read-only @ yes? type cr
  ." INSENSITIVE:  " insensitive @ yes? type cr 
  ." FAT CKSUM:   " fat fats blksums u. cr
  ." DIR CKSUM:   " $FFFF dirstart dirsum u. cr
  ." FS  CKSUM:   " fat end fat - blksums u. cr ;
: cksum ( "file" -- calculate checksum over file )
  token count r/o open-file throw
  dup (cksum) cr u. cr
  close-file throw ;
: fsync save-buffers ; ( -- : save file system data )
: ls peekd .dir ; ( -- : list contents of Present Working Dir )
: dir peekd block? list ; ( -- : crude version of `ls` )
: mount loaded @ ?exit  ( -- : initialize file system )
   init block? load 0 dirp ! dirstart pushd ;
: freeze 1 read-only ! ; ( -- : make file system read only )
: melt 0 read-only ! ; ( -- : allowing writing to file system )
: fdisk melt bcheck fmt.init fmt.fat fmt.blks fmt.root mount ;
: rename ( "file" "file" -- : rename a file )
  ( `locked!?` does not need to be called )
  ro?
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  narg ( dir-find uses `findbuf` )
  namebuf peekd dir-find 0>= EEXIS error
  peekd r@ dir? if
    findbuf peekd r@ dirent-blk@ 0 dirent-name!
  then
  findbuf peekd r> dirent-name! ;
: mv ( "file" "file" -- : move a file to different dirs )
  ro?
  narg namebuf mcopy 
  movebuf peekd dir-find dup >r dup 0<= EFILE error
  peekd swap >dir
  token count 2dup ncopy
  movebuf namebuf equate 0= if rdrop 2drop exit then
  2dup s" ." equate 0= if rdrop 2drop exit then
  s" .." equate 0= if 
    popd peekd dup locked!? >r pushd
  else
    peekd locked!?
    namebuf peekd dir-find dup 0>= if
      dup peekd swap dir? if
        peekd swap dirent-blk@ >r
      else 1 ENDIR error then
    else \ rename
      rdrop
      drop movebuf peekd dir-find 
      >r 
      peekd r@ dir? if
        namebuf peekd r@ dirent-blk@ 0 dirent-name!
      then
      namebuf peekd r> dirent-name! exit
    then
  then
  r@ dfull?
  movebuf r@ dir-find 0>= EEXIS error
  r> eline? dir>
  peekd r> 2dup dirent-erase 
  ( compact )
  >r addr? r@
  dirsz * + dup dirsz + swap b/buf r@ 1+ dirsz * 
  - cmove rdrop save ;
\ N.B. We could set a per directory checksum, a checksum over
\ the directory would be relatively cheap to calculate,
\ assuming file creation and deletion is not too common
\ relative to other file operations.
: mkdir ( "dir" -- : make a directory )
  ro?
  dirp @ maxdir >= EDDPT error
  full?
  narg
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  balloc dup >r found? dirent-blk!
  [char] D found? dirent-type!
  namebuf r> fmtdir ; 
: copy ( "src" "dst" -- : copy a file )
  ro?
  full?
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  \ For stricter file copying:
  \ 
  \   peekd r@ file? 0= ENFIL error
  \ 
  peekd r@ dir? ENFIL error
  narg
  namebuf peekd dir-find 0>= EEXIS error
  peekd r@ dirent-rem@ found? dirent-rem!
  peekd r> dirent-blk@ dup bcount ballocs dup >r (copy)
  r> found? dirent-blk!
  [char] F found?  dirent-type!
  namebuf found? dirent-name! ;
: cmp ( "file" "file" -- : compare two files )
  token count r/o open-file throw
  token count r/o open-file ?dup 
    if swap close-file drop exit then
  (cmp) close-file swap close-file throw throw ;
: mkfile ro? full? narg 0 (mkfile) ; ( "file" -- )
: fallocate ro? full? narg integer? (mkfile) ; ( "file" u -- )
: fgrow ( "file" n -- : grow a file to n blocks )
  ro?
  narg integer? 
  namebuf peekd dir-find dup >r 0< ENFIL error
  ballocs dup link-blank peekd r> dirent-blk@ fat-append save ;
: ftruncate ( "file" n -- : truncate a file to n blocks )
  peekd locked!?
  ro?
  narg integer?
  namebuf peekd dir-find dup >r 0< ENFIL error
  peekd r> dirent-blk@ btruncate ;
: mknod ( "file" node -- : make a special file )
  ro?
  full?
  narg
  integer? >r
  namebuf peekd dir-find 0>= EEXIS error
  namebuf found? dirent-name!
  #rem found? dirent-rem!
  r> found? dirent-blk! 
  [char] S found? dirent-type! ;
: bgrep ( search file -- : search a blocks for a string )
  cr
  token count mcopy
  narg namebuf peekd dir-find dup 0< EFILE error
  peekd swap dirent-blk@ [ ' (grep) ] literal apply ; 
: grep ( "search" "file" -- : search file for a string )
  token count mcopy
  narg namebuf r/o open-file throw >r
  begin
    copy-store b/buf r@ read-line line-end? 0=
  while
    >r
    copy-store r@ movebuf -trailing examine nip nip if 
      copy-store r@ type cr
    then
    rdrop
  repeat
  drop
  r> close-file throw ;
\ N.B. `script` is very slow in SUBLEQ eForth because of poor 
\ performance of `read-line`. This should be improved. It is 
\ especially a problem when executing files formatted as Forth 
\ blocks, as `read-line` searches the entire buffer for an End 
\ Of Line marker.
\
\ TODO: BUG: Executing a script containing file commands
\ fails, such as "mkdir a", "mkdir b", "ls". It introduces
\ garbage. Only in SUBLEQ eFORTH...
\
: script ( "file" -- : execute a line based Forth script )
  narg namebuf r/o open-file throw >r
  begin
    copy-store b/buf r@ read-line line-end? 0=
  while
    copy-store swap [ ' evaluate ] literal catch ?dup
    if r> close-file throw throw then
  repeat
  drop r> close-file throw ;
: rm ro? narg 0 (rm) ; ( "file" -- )
: rmdir ro? narg 1 (rm) ; ( "dir" -- )
: cd ( "dir" -- : change the Present Working Directory )
  token count 2dup s" ." equate 0= if 2drop exit then
  2dup s" .." equate 0= if 2drop popd drop exit then
  2dup s" /" equate 0= if 2drop /root exit then
  peekd dir-find dup >r 0< EFILE error
  peekd r@ dir? 0= ENDIR error
  peekd r> dirent-blk@ pushd ;
: pwd ( -- : printing present working directory )
  0
  begin
    dup dirp @ <
  while
    dup cells dirstk + @ 0 dirent-name@ 
    over >r namelen r> swap type ." /"
    1+
  repeat drop ;
: tree ( -- : tree view of file system )
  cr pwd ls
  dsl
  begin
    dup d/blk <
  while
    peekd over dirent-type@ bl <= if drop exit then
    peekd over dir? if
      peekd over dirent-blk@ pushd recurse popd drop
    then
    1+
  repeat drop ; 
: deltree ( "dir" -- : recursively delete a directory )
  ro?
  narg
  namebuf peekd dir-find dup >r 0< EFILE error
  ." DEL: " peekd r@ dirent-name@ type cr
  peekd r@ dir? 0= if rdrop 0 (rm) exit then
  peekd r@ dirent-blk@ (deltree)
  peekd r> 1 (remove) ;
: help ( -- : print a spartan help message )
  cr
  cr ." Use `more help.txt` in the root directory for help." 
  cr ." Type `cd /` to get to the root directory and `ls`
  cr ." to view files."
  cr cr ." Otherwise visit <https://github.com/howerj/ffs>"
  cr cr ." Command List: " cr words cr cr ;
: cat ( "file" -- : barf a file out )
  token count r/o open-file throw cr
  >r
  begin r@ key-file dup 0>= while emit repeat drop
  r> close-file throw ;
: more ( "file" -- : display a file with breaks )
  token count r/o open-file throw
  >r 0
  begin r@ key-file dup 0>= 
  while 
    dup $A = if swap 1+ swap then emit 
    dup $F > if more> more? if
      ." QUIT" cr
      r> close-file throw drop exit then 
      drop 0
    then 
  repeat 2drop
  r> close-file throw ." EOF" cr ;
: bcat (file) [ ' +list ] literal apply ; ( "file" -- )
: bmore (file) link-more ; ( "file" -- : display blocks )
: exe (file) link-load  ; ( "file" -- )
: hexdump ( "file" -- : nicely formatted hexdump of file )
  token count -1 (hexhump) ;
: stat ( "file" -- : detailed file stats )
  peekd (entry) 
  cr 2dup .type 
  cr 2dup dirent-rem@ ." REMB: " u.
  cr      dirent-blk@ dup bcount ." BCNT: " u. 
  cr  dup ." HEAD: " u.
  cr ." BLKS: " [ ' u. ] literal apply ;
: b2f ( "file" "file" -- : convert block file to byte file )
  [ ' (b2f) ] literal with-files ;
: f2b ( "file" "file" -- : convert byte file to block file )
  [ ' (f2b) ] literal with-files ;
{edlin} +order
: edit ro? narg (create) dup (round) 
  namebuf r/o open-file throw edlin ; ( "file" -- )
{edlin} -order
: export ( -- : export file system )
  dsl
  begin
    dup d/blk <
  while
    peekd over dirent-type@ bl <= if 
      ." cd .." cr drop exit 
    then
    peekd over special? if
      peekd over ." mknod " dirent-blk@ u. cr
    then
    peekd over file? if
      \ We could determine if the file is a binary file,
      \ and if it is not, do a text dump instead.
      peekd over ." hex: " dirent-name@ type
      peekd over dirent-name@ 0 (hexhump) cr
      ." ." cr
    then
    peekd over dir? if
      peekd over ." mkdir " dirent-name@ type cr
      peekd over ." cd " dirent-name@ type cr
      peekd over dirent-blk@ pushd recurse popd drop
    then
    1+
  repeat drop ; 
\ We could also detect if the file is binary, and then
\ extract the logic into another word so we can determine if
\ files are block oriented (non binary files with no newlines),
\ or not.
: wc ( "file" -- display file byte/word/line count )
  token count r/o open-file throw >r
  0 0 bl
  begin
    r@ key-file dup 0>=
  while ( lines words prev key )
    dup $A  = if >r >r >r 1+ r> r> r> then
    dup bl <= if over bl > if >r >r 1+ r> r> then then
    nip
  repeat 2drop
  cr
  ." BYTES: " r@ file-position drop du. cr
  ." WORDS: " u. cr
  ." LINES: " u. cr
  r> close-file throw ;
: yes ( "file" "string" count -- : print string repeatedly )
  ro? narg token count mcopy integer?
  namebuf w/o create-file throw >r
  begin
    ?dup
  while
    movebuf -trailing r@ write-line drop
    1-
  repeat
  r> close-file throw ;
: mkrandom ( "file" bytes -- : file a file with random bytes )
  ro? narg integer? >r namebuf w/o create-file throw r> swap >r
  begin
    ?dup
  while
    random r@ emit-file drop
    1-
  repeat r> close-file throw ;

\ ### Easy File Creation
\
\ This subsection of DOS commands introduces a syntax that
\ allows the creation of new files, both textual and binary,
\ so long as no editing is required. Instead of a simple
\ command that performs an action several words are created 
\ that work in conjunction.
\
\ `file:` and `append:` are defined first for textual files:
\
\ Usage:
\
\      file: example.txt
\      | March on, join bravely, let us to it pell mell, if not
\      | to heaven then hand in hand to hell!
\      ;file
\
\ And to append to a file:
\
\      append: example.txt
\      |
\      | More Shakespeare!
\      |
\      ;append
\
: file: -handle token count w/o recreate-file throw handle ! ;
: append: -handle token count w/o append-file throw handle ! ;
: ;file handle @ close-file -handle throw ;
: ;append ;file ;
: | 
  handle @ 0< if discard exit then
  leftovers handle @ write-line ?dup if ;file then discard ;
\
\ `hex:` works slightly differently, it allows the creation of
\ binary files byte by byte. Once `hex:` and a file name has
\ been entered a series of hexadecimal numbers can be entered.
\ The sequence can be terminated with a `.` (or any non
\ numeric sequence).
\
\ Usage:
\
\        hex: file.bin
\        0 1 2 3 AA FF
\        01 20
\        .
\
: hex: ( "file" -- )
  token count w/o recreate-file throw
  base @ >r hex
  cr
  >r
  begin
    integer dpl @ 0< and
  while
    r@ emit-file drop
  repeat drop
  r> close-file r> base ! throw ;

\ ### File Compression
\
\ `lzp` and `unlzp` are commands that perform file compression.
\ The compression routine has already been described, it is
\ fairly poor in terms of compression performance but it is
\ simple and can save about 20% of the files size, one some
\ types of file it can save a lot more (especially files
\ formatted as Forth blocks).
\
{lzp} +order
: lzp ( "from" "to" -- )
  peekd locked!? ro? 
  narg namebuf mcopy narg movebuf namebuf lzp-file-name 
  lzp-statistics ;
: unlzp ( "from" "to" -- ) 
  peekd locked!? ro? 
  narg namebuf mcopy narg movebuf namebuf unlzp-file-name
  lzp-statistics ;
{lzp} -order

\ ### Missing Commands
\
\ `chkdsk` and `defrag` are missing. These would be complex
\ commands for interactively repairing broken disks and
\ for defragmenting the disk respectively.
\
\ Defragmentation can be performed in a crude way be using
\ `export` to export the entire file system and then importing
\ that to a fresh file system. It is not ideal, but it is
\ much simpler than implementing a defragmentation routine.
\
\ To defragment the file system free space is required,
\ files would have to be moved to the free blocks and placed
\ back in a more ordered sequence in the holes left over from
\ moving files around, multiple passes might be required to
\ fully defragment the system. It is a slow process, the
\ Wikipedia page on the subject has more information, see
\ <https://en.wikipedia.org/wiki/Defragmentation>.
\
\ For a `chkdsk` or Check Disk routine some recovery routines
\ could be done semi-automatically. User intervention is 
\ needed to decided on what to do with broken files. This
\ file system is non journalling, and although checksums can
\ be calculated over the various data structures in the file
\ system they are not automatically applied but instead used
\ for a manual check of system integrity.
\
\ There are some basic checks that can be done:
\
\ 1. For the FAT partition a check that all pointers are within
\ bounds. Checks can be performed that boot and root directory
\ are also present, and the root directory looks okay (for
\ example no non-ASCII characters should be present in a
\ directory entry, the directory header should be present, 
\ etcetera).
\ 2. If there are orphaned blocks, these can be collected
\ together. There is unfortunately no way to determine which
\ file an orphaned block belongs to, nor if the block was
\ actually a directory entry. A file could be created for each
\ orphaned chain.
\ 3. For each directory entry in all the directories a check 
\ that there is a corresponding FAT entry.
\ 4. For all files and directories check that no files overlap 
\ (use the same blocks).
\ 5. Bad blocks could be tested for.
\
\
\ There are many more checks that could be thought of and
\ implemented, the complexity of this command has no upper
\ bound. At some point it would become easier and more reliable
\ to add journaling to the file system instead.
\
\ ### Aliases
\
\ Aliases for commands have been added, some have been
\ avoided because they clash with Forth words. Common
\ synonyms from Unix and DOS have been added to make 
\ programmers familiar with either system more comfortable, and
\ to lessen the impact of tripping over muscle memory on a new
\ platform.
\
\ Adding an alias is as simple as adding a new word definition.
\
: chdir cd ; ( "dir" -- change Present Working Directory )
: cls page ; ( -- : clear screen )
: cp copy ; ( "file" "file" -- copy file )
: del rm ; ( "file" -- delete file )
: ed edit ; ( "file" -- edit file )
: touch mkfile ; ( "file" -- : create a file )
: diff cmp ; ( "file" "file" -- : compare two files )
( : sh script ; ( "file" -- execute file ) \ Clashes in Gforth
( : bye halt ; ) \ Clashes with FORTHs `bye`word.
( : exit halt ; ) \ Clashes with FORTHs `exit` word.
( : quit halt ; ) \ Clashes with FORTHs `quit` word.
( : move mv ; ) \ Clashes with FORTHs `move` word.
( : type cat ; ) \ Clashes with FORTHs `type` word.

\ ## Helper words and start up routines

forth-wordlist +order definitions
: +ffs {ffs} +order ;
: +dos {dos} +order ;
: +system system +order ;
: dos ( only ) +dos +ffs mount {ffs} -order ;

defined eforth 0= [if]
\ If we are running in eForth we want to add the definitions
\ to the main Forth vocabulary, otherwise we want to add them
\ to the `{ffs}` vocabulary so as not to redefine the gforths
\ File Access Words.
\
+ffs definitions
[then]

\ `stdin`, `stdout`, and `stderr` could be words that loaded
\ a file-id from a variable, this would allow these streams to
\ be redirected.
\
s" ." r/w open-file throw ( open special file '.' )
dup <stdin> !
dup <stdout> !
dup <stderr> !
drop

: stdin <stdin> @ ; ( -- file )
: stdout <stdout> @ ; ( -- file )
: stderr <stderr> @ ; ( -- file )

\ ### Primitive Login System

defined eforth [if]
\ A primitive user login system [that is super insecure].
\ If no users are present then we login automatically. The
\ action after logging in is the same (it is not customizable 
\ with an execution token) which is to drop in to the Forth
\ shell with the FFS commands in the `{dos}` vocabulary loaded.
\
system +order
{ffs} +order
wordlist +order definitions
wordlist constant users

variable proceed 0 proceed !
: conceal $1B emit ." [8m" ; ( Could also override <emit> )
: reveal $1B emit ." [28m" ;
: secure users 1 set-order ; ( load password database )
: restore only forth definitions +dos [ ' ok ] literal <ok> ! ;
: message ." user: " ; ( -- : prompt asking for user-name )
: fail ." Invalid username or password" cr ; ( -- error msg )
: success 1 proceed ! ." Logged in." cr ; ( signal success )
: pass token count crc ; ( "xxx" -- u : super-secure <_< )
: ask ." pass: " conceal query reveal ;
: empty depth for aft drop then next ; ( ??? -- )
: prompt secure message [ ' ) ] literal <ok> ! ;
: uget query eval ; ( "xxx" -- : get user name )
: nousers users @ 0= ; ( -- : no users defined? )
: retry nousers ?exit begin prompt [ ' uget ] literal catch 
  drop empty proceed @ until ;

+dos definitions

: mkuser ( "user" "password" -- : create new user entry )
users +order definitions create pass , only forth definitions
  does> ask @ pass = if restore success exit then fail ;
: login 0 proceed ! retry dos ; ( -- : enter login system )
: lsuser get-order secure words set-order ; ( -- )

\ To use the system you have to create some users like so:
\
\        mkuser guest guest
\        mkuser admin password1
\        mkuser archer dangerzone
\        mkuser cyril figgis
\        mkuser lana stirling
\
\ Next time `login` is called the user will be greeted with
\ a login prompt.
only forth definitions +dos +ffs +system

: reboot login quit ;
' reboot <quit> !
[then]

\ ## Initializing Disk
\
\ This section formats a disk if the disk has not been 
\ formatted and populates the disk with a default set of files 
\ and directories, including a help file and some system
\ files.
\
\ Special files that are created with `mknod` include:
\
\ * \[BOOT\]
\ * \[FAT\]
\ * \[KERNEL\]
\
\ "\[BOOT\]" contains a single Forth Block that is executed
\ on boot. It should contain Forth source and not machine code.
\ It is specially formatted.
\
\ "\[FAT\]" contains a linked list of blocks that hold the
\ FAT partition. This allows direct access to the FAT partition
\ to query or modify it. Modifying the FAT partition is a
\ dangerous operation fraught with peril! As it is presented as
\ a file it is possible to read the partition with the built
\ in utilities such as `hexdump`. 
\
\ "\[KERNEL\]" is present only in SUBLEQ eFORTH, it contains
\ a linked list of the first 64 blocks, which map on to the
\ first 64KiB of memory within the SUBLEQ eFORTH interpreter.
\ Modifying this file can cause system instability much like
\ modifying "\[FAT\]" can.
\
\ These are all special files as their blocks should not be
\ allocated to files or directories as they hold special data
\ structures or their contents is outside that of normal file
\ system partition.
\
\ The entries can be removed with no problems.
\
\ Other files created are:
\
\ * `demo.blk`
\ * `help.txt`
\ * `errors.db`
\ * `words.db` (only on SUBLEQ eFORTH)
\
\

mount loaded @ 0= [if]
cr .( FFS NOT PRESENT, FORMATTING... ) cr
fdisk
\ Nested compile time "\[if\]"s do not work in SUBLEQ eFORTH...
\ TODO: BUG: The big image of SUBLEQ eFORTH places these files
\ in the wrong place...
mkdir system
cd system
defined eforth 0= ?\ mknod [BOOT] 0
defined eforth 0= ?\ mknod [FAT] 1
defined eforth    ?\ mknod [BOOT] 65
defined eforth    ?\ mknod [FAT] 66
defined eforth    ?\ mknod [KERNEL] 1
cd ..
\ This is a simple demo program that does not define new words,
\ a better, more complex or interesting program could be made,
\ or perhaps a utility, but it is just used to demonstrate that
\ the script execution functionality works.
edit demo.blk
+ .( HELLO, WORLD ) cr
+ 2 2 + . cr
+
+ .( GOODBYE, CRUEL WORLD! ) cr
q
\ A file containing some general help, always useful for the
\ confused. The file also contains a list of commands (which
\ could perhaps go in its own file) and instructions for the
\ block based text editor.
file: help.txt
| FORTH FILE SYSTEM HELP AND COMMANDS
|
| Author: Richard James Howe
| Repo:   https://github.com/howerj/ffs
| Email:  howe.r.j.89@gmail.com
|
| This is a simple DOS like file system for FORTH, it makes
| it easier to edit and manipulate data than using raw blocks.
|
| For more information see:
|
| <https://github.com/howerj/ffs>
| <https://github.com/howerj/subleq>
|
| To simplify the implementation only the `move` command can
| move files and directories into other directories, `move`
| also handles targets "." and "..", like `cd` does.
|
| To execute a file type `sh <FILE>` or `exe <FILE>`.
|
| Commands:
|
| b2f <FILE1> <FILE2>: convert block files to line/byte files
| bcat <FILE>: display a series of blocks
| bgrep <STRING> <FILE>: Search for <STRING> in <FILE> blocks
| bmore <FILE>: display a file, pause for each block
| cat <FILE>: display a file
| cd / chdir <DIR>: change working directory to <DIR>
| cksum <FILE>: Compute and print CRC (16-bit CCITT) of file
| cls: clear screen
| cmp / diff <FILE> <FILE>: compare two files
| cp / copy <FILE1> <FILE2>: copy <FILE1> to new <FILE2>
| deltree <FILE/DIR>: delete a file, or directory recurisvely
| df: display file system information
| ed / edit <FILE>: edit a <FILE> with the block editor
| exe <FILE>: execute source <FILE> of blocks
| export: export (or dump), from pwd, the file system
| f2b <FILE1> <FILE2>: convert line/byte files to block files
| fallocate <FILE> <NUM>: make <FILE> with <NUM> blocks
| fdisk: **WARNING** formats disk deleting all data!
| fgrow <FILE> <NUM>: grow <FILE> by <NUM> blocks
| freeze: Freeze file system - Turn on read only mode
| fsync: save any block changes
| ftruncate <FILE> <NUM>: truncate <FILE> to <NUM> blocks
| grep <STRING> <FILE>: Search for <STRING> in lines of <FILE>
| help: display a short help
| hexdump <FILE>: hexdump a file consisting of blocks
| login: password login (NOP if no users) (SUBLEQ eForth only)
| ls / dir : list directory
| lsuser: List all users in login system (SUBLEQ eFORTH only)
| lzp <FILE1> <FILE2>: LZP Compress <FILE1> into <FILE2>
| melt: Unfreeze file system - Turn off read only mode
| mkdir <DIR>: make a directory
| mknod <FILE> <NUM>: make a special <FILE> with <NUM>
| mkrandom <FILE> <BYTES>: file <FILE> with random <BYTES>
| mkuser <USER> <PASSWORD>: create new user entry (SUBLEQ only)
| more <FILE>: display a file, pause for every X lines
| mount: attempt file system mounting
| pwd: print current working directory
| rename <DIR/FILE1> <DIR/FILE2>: rename a file or directory
| rm / del <FILE>: remove a <FILE> and not a <DIR>
| rmdir <DIR>: remove an empty directory
| sh / script <FILE>: execute line based <FILE>
| sh / shell: invoke file system shell
| stat <FILE>: display detailed information on <FILE>
| touch / mkfile <FILE>: make a new file
| tree: recursively print directories from the current one
| unlzp <FILE1> <FILE2>: LZP Decompress <FILE1> into <FILE2>
| wc <FILE>: display byte, word and line count of a file
| yes <FILE> <STR> <NUM>: fill <FILE> with <NUM> lines of <STR>
| 
| `grep` is limited to 16 byte search terms and 1024 byte 
| lines, it also uses the same copy buffer that the editor
| uses for the copy command.
| 
| Executing files requires that they are block formatted and
| not line formatted. 
| 
| Example commands:
|
| mkdir test
| cd test
| edit HELLO.FTH
| 0 i .( HELLO, WORLD ) cr
| s q
| exe HELLO.FTH
| rm HELLO.FTH
| ls
| 
| To make new files the words `file:` and `hex:` can be used
| to create text files and binary files respectively.
|
| Example usage of `file:`:
|
|        file: example.txt
|        | This is an example line, line #1
|        | This is an example line, line #2
|        ;file
|
| This will create a file called `example.txt` with the two
| line, without the `|` prefix.
|
| To create a binary file using `hex:`:
|
|        hex: example.bin
|        0 1 2 3 AA FF
|        3 4 5 6
|        .
| 
| EDITOR COMMANDS
|
| This block editor is primitive but usable. It operates on
| 1024 byte Forth blocks, with 16 lines per block. Some 
| commands accept a line number or position.
| q : quit editor
| s : save work
| n : move to next block in file, allocating one if needed
| p : move to previous block in file
| + <LINE>: insert <LINE>, advance line count, 'n' if needed
| - : decrement line count, call 'p' if needed, no <LINE>
| x : execute current file from the start
| #line d : delete #line
| l : list current block we are editing
| w : list commands available to the editor
| #line a / i <LINE>: insert <LINE> onto #line of current block
| #line #row ia <LINE>: insert <LINE> into #line at #row
| ? : display current block
| z : blank current block
| y : yank block to storage buffer
| u : replace screen with storage buffer
| r : remove last block in chain, set working block to first
|
| Note that it is possible to delete a file whilst editing it,
| which is not advised but should not break anything.
|
| When editing a file with `edit` if the file does not exist
| it is created.
|
| A typical session might look like:
|
| edit hello.fth
| 0 i \ HELLO WORLD PROGRAM, VERSION #666, RJH, 15/04/2024
| 1 i .( AHOY THERE WORLD, SALUTATIONS AND WARM GREETINGS ) cr
| s
| q
| exe hello.fth
|
| This is a block editor, and not a freeform text editor, so
| it does follow a strict format of 16 lines of text per block.
|
| "p" and "n" can be used to move forward and backward in the
| file, a new block is assigned if "n" is at the end of the
| file.
;file
\ `errors.db` contains a list of standard Forth errors, one
\ per line. It is possible to construct a set of words that
\ look up the textual description of an error given the number
\ quite easily.
file: errors.db
| -1  ABORT
| -2  ABORT"
| -3  stack overflow
| -4  stack underflow
| -5  return stack overflow
| -6  return stack underflow
| -7  do-loops nested too deeply
| -8  dictionary overflow
| -9  invalid memory address
| -10 division by zero
| -11 result out of range
| -12 argument type mismatch
| -13 undefined word
| -14 interpreting a compile-only word
| -15 invalid FORGET
| -16 attempt to use 0-len str. as a name
| -17 pictured numeric out. str. overflow
| -18 parsed string overflow
| -19 definition name too long
| -20 write to a read-only location
| -21 unsupported operation
| -22 control structure mismatch
| -23 address alignment exception
| -24 invalid numeric argument
| -25 return stack imbalance
| -26 loop parameters unavailable
| -27 invalid recursion
| -28 user interrupt
| -29 compiler nesting
| -30 obsolescent feature
| -31 >BODY used on non-CREATEd def.
| -32 invalid name arg. (e.g., TO xxx)
| -33 block read exception
| -34 block write exception
| -35 invalid block number
| -36 invalid file position
| -37 file I/O exception
| -38 non-existent file
| -39 unexpected end of file
| -40 wrong BASE in float point convert
| -41 loss of precision
| -42 floating-point divide by zero
| -43 floating-point result out of range
| -44 floating-point stack overflow
| -45 floating-point stack underflow
| -46 floating-point invalid argument
| -47 compilation word list deleted
| -48 invalid POSTPONE
| -49 search-order overflow
| -50 search-order underflow
| -51 compilation word list changed
| -52 control-flow stack overflow
| -53 exception stack overflow
| -54 floating-point underflow
| -55 floating-point unidentified fault
| -56 QUIT
| -57 exception in tx or rx a character
| -58 [ IF ], [ ELSE ], or [ THEN ] exception
| -59 ALLOCATE
| -60 FREE
| -61 RESIZE
| -62 CLOSE-FILE
| -63 CREATE-FILE
| -64 DELETE-FILE
| -65 FILE-POSITION
| -66 FILE-SIZE
| -67 FILE-STATUS
| -68 FLUSH-FILE
| -69 OPEN-FILE
| -70 READ-FILE
| -71 READ-LINE
| -72 RENAME-FILE
| -73 REPOSITION-FILE
| -74 RESIZE-FILE
| -75 WRITE-FILE
| -76 WRITE-LINE
;file
[then]

\ Despite having limited space in the SUBLEQ eFORTH system
\ we can add this glossary to provide online help with all
\ of the words defined in the base image. If the user wants to
\ reclaim the space they can do.

defined eforth [if]
file: words.db
| ! ( u a -- ) Store u at at
| # ( d -- d ) Processed single digit, Pictured Numeric Output
| #-1 ( -- -1 ) push -1 on to the stack
| #0 ( -- 0 ) push 0 on to the stack
| #1 ( -- 1 ) push 1 on to the stack
| #2 ( -- 2 ) push 2 on to the stack
| #> ( d -- a u ) end Pictured Numeric Output
| #s ( d -- 0 0 ) process number into Pictured Numeric Output
| #vocs ( -- u ) maximum vocabularies that can be loaded
| $" ( "string" --, Runtime -- b ) compile string into word
| ' ( "name" -- xt ) get execution token of "name"
| ( ( -- ) discard everything from input stream line until )
| ($) ( -- a ) used with to implement $"
| (.) ( n -- ) used to implement ".", much faster than u.
| (abort) ( n -- ) compiled by abort", abort if n non-zero
| (block) ( ca ca cu -- ) transfer to/from block storage
| (boot) ( -- ) complex boot routine stored in <boot>
| (comp) ( -- ) used to implement does>
| (const) ( -- u ) used to implement constant
| (does) ( -- ) used to implement does>
| (emit) ( c -- ) emit a single character to output always
| (error) ( u -- ) default error handler in quit loop
| (find) ( a -- pwd pwd 1 | pwd pwd -1 | 0 a 0 )
| (literal) ( u -- ) default behavior of literal
| (marker) ( -- ) used to implement marker
| (nfa) ( u -- ) toggle name field address in last defined word
| (order) ( w voc*n n -- voc*n w n ) used in +order and -order
| (push) ( -- u ) push next cell 
| (s) ( "string" -- ) compile string into dictionary
| (search) ( a voc -- pwd pwd 1 | pwd pwd -1 | 0 a 0 ) search
| (up) ( -- u ) access user variable stored in next cell
| (user) ( -- a ) used to implement user
| (var) ( -- a ) used to implement variable
| ) ( -- ) immediate, do nothing, terminate comment
| * ( n n -- n ) multiple two numbers
| + ( n n -- n ) add two numbers
| +! ( n a -- ) add n to memory location
| +order ( voc -- ) add voc to current search order
| +string ( a u n -- ) increment string by n
| , ( n -- ) write n into the next dictionary cell
| - ( n1 n2 -- n ) subtract n2 from n1
| -cell ( -- -2 ) push the negated cell size
| -order ( voc -- ) remove voc from current search order
| -rot ( n1 n2 n3 -- n3 n1 n2 ) reverse of rot
| -trailing ( a u -- a u ) remove trailing whitespace
| . ( n -- ) display signed number in current output radix
| ." ( "string" -- ) compile string into word that prints itself
| .$ ( -- ) used to implement ."
| .( ( "display" -- ) parse and emit until matching )
| .emit ( c -- ) print char, replacing non-graphic ones
| .id ( pwd -- ) print word name field
| .s ( ??? -- ??? ) display variable stack
| / ( n1 n2 -- n ) divide n1 by n2 
| /mod ( n1 n2 -- n1%n2 n1/n2 ) divide n1 by n2 
| 0< ( n -- f ) is less than zero?
| 0<= ( n -- f ) is less than or equal to zero?
| 0<> ( n -- f ) is not equal to zero?
| 0= ( n -- f ) is equal to zero?
| 0> ( n -- f ) is greater than zero?
| 0>= ( n -- f ) is greater or equal to zero?
| 1+ ( n -- n ) increment n
| 1- ( n -- n ) decrement n
| 2! ( n n a -- ) store two values at address and next cell
| 2* ( u -- u ) multiply by two, bitshift left by 1
| 2/ ( u -- u ) divide by two, bitshift right by 1
| 2>r ( n n --, R: -- n n ) move two values to return stack
| 2@ ( a -- n n ) retrieve two values from address and next cell
| 2drop ( n n -- ) discard two values
| 2dup ( n1 n2 -- n1 n2 n1 n2 ) duplicate two stack items
| 2r> ( -- n n, R: n n -- ) move two values from return stack
| : ( "name" -- ) parse name and start word definition
| :noname ( -- xt ) start anonymous word definition
| ; ( -- ) immediate, end word definition
| < ( n1 n2 -- f ) is n1 less than n2
| <# ( -- ) start Pictured Numeric Output
| <= ( n1 n2 -- f ) is n1 less than or equal to n2
| <> ( n n -- f ) are two values not equal to each other?
| <block> ( -- a ) execution vector for block
| <boot> ( -- a ) execution vector for cold
| <echo> ( -- a ) execution vector for echo
| <emit> ( -- a ) execution vector for emit
| <error> ( -- a ) execution vector for error handling
| <expect> ( -- a ) execution vector for expect
| <key> ( -- a ) execution vector for key
| <literal> ( -- a ) execution vector for literal
| <ok> ( -- a ) execution vector for okay prompt
| <quit> ( -- a ) execution vector for final boot word
| <tap> ( -- a ) execution vector for tap
| = ( n n -- f ) are two numbers equal?
| > ( n1 n2 -- f ) is n1 greater than n2?
| >= ( n1 n2 -- f ) is n1 greater or equal to n2?
| >blk ( k -- ca ) turn block into cell address
| >body ( xt -- body ) move to a created words body
| >in ( -- a ) input buffer position user variable
| >number ( ud b u -- ud b u ) convert string to number
| >r ( n --, R: -- n ) move value from variable to return stk.
| ?depth ( n -- ) depth check, throw if too few stack items
| ?dup ( n -- n n | 0 ) conditionally duplicate if non zero
| ?exit ( n -- ) compile-only, conditionally exit word
| ?found ( b f -- b ) throw if flag false with error message
| ?len ( b -- b ) throw if counted string too long
| ?nul ( b -- b ) throw if counted string is zero length
| ?unique ( b -- b ) warn if word definition already exists
| @ ( a -- n ) retrieve contents of memory address
| @+ ( a -- a n ) get value at address, keep address
| @execute ( ??? a -- ??? ) retrieve execution token and execute
| [ ( -- ) immediate, turn command mode on
| [!] ( u ca -- ) store value at cell address
| [@] ( ca -- u ) retrieve value from cell address
| [char] ( "char" --, Runtime: -- b ) compile character literal
| "[else]" ( -- ) skip input until "[then]"
| "[if]" ( n -- ) conditional input until "[else]/[then]"
| "[then]" ( -- ) do nothing
| \ ( "line" -- ) discard everything from \ to end of line
| ] ( -- ) turn compile mode on, command mode off
| abort ( -- ) call throw -1 unconditionally
| abort" ( "string" --, Runtime: n -- ) print abort if non-zero
| abs ( n -- u ) absolute value, beware $8000
| accept ( b u -- b u ) accept a line of input
| activate ( xt task-address -- ) activate a task
| aft ( -- ) part of for...aft...then...next loop
| again ( -- ) part of begin...again infinite loop
| align ( -- ) align dictionary pointer up
| aligned ( a -- a ) align address up
| allot ( n -- ) allocate bytes in dictionary
| and ( n n -- n ) bitwise and
| at-xy ( x y -- ) set cursor position, 1 index based
| b/buf ( -- 1024 ) number of bytes in a block
| banner ( +n c -- ) output c n times
| base ( -- a ) address of numeric input output radix 2-36
| begin ( -- ) part of a begin...until, begin...again loop
| bell ( -- ) emit ASCII bell character
| bget ( k -- ) transfer block from mass storage to buffer
| bl ( -- 32 ) push ASCII space character
| blank ( a u -- ) set array of bytes to space
| blk ( -- a ) currently loaded block
| blk0 ( -- a ) block buffer zero block loaded value
| block ( blk -- a ) load data off disk, store modified buffer
| bput ( k -- ) transfer block buffer to mass storage
| buf0 ( -- a ) address of block buffer zero
| buffer ( blk -- a ) like block but it performs no load of data
| bye ( -- ) halt system
| c! ( c a -- ) write a single byte to memory location a
| c, ( c -- ) write byte into dictionary
| c/buf ( -- 512 ) cells in a block
| c@ ( a -- c ) retrieve a single byte
| c@+ ( a -- a c ) retrieve single byte, keep address
| calibration ( -- a ) value used by ms for 1 ms wait
| catch ( xt -- n ) execute xt, catching result of any throws
| cell ( -- 2 ) size of a single cell in bytes
| cell+ ( a -- a ) increment address by cell size
| cell- ( a -- a ) decrement address by cell size
| cells ( n -- n ) turn a cell count into a byte count
| cfa ( pwd -- cfa ) move word pwd field to its code field
| char ( "char" -- c ) turn a character of input into a byte
| cksum ( a u -- u ) calculate additive checksum over range
| clean ( -- ) opposite of update, mark last loaded block clean
| cmove ( b1 b2 u -- ) copy u characters from b1 to b2
| cold ( -- ) perform a cold boot
| compare ( a1 u1 a2 u2 -- n ) compare two strings
| compile ( -- ) compile next address in word into dictionary
| compile, ( xt - ) compile execution token into word def.
| compile-only ( -- ) make previously defined word compile-only
| console ( -- ) setup input/output for terminal/console
| constant ( n "name" -- ) create a constant with value n
| context ( -- a ) array containing loaded vocs
| count ( a -- a c ) retrieve byte and increment a by 1
| cr ( -- ) emit a newline
| create ( "name" -- ) create word which pushes field address
| csi ( -- ) emit ANSI terminal escape sequence
| current ( -- a ) current vocabulary definitions are added to
| cycles ( -- a ) address of number of task switches performed
| d+ ( d d -- d ) add two double cell values
| decimal ( -- ) set input and output radix to decimal
| defined ( "name" -- f ) is "name" a defined word?
| definitions ( -- ) add future definitions to top vocabulary
| depth ( ??? -- n ) get variable stack depth
| digit ( u -- c ) extract a character from number
| dirty0 ( -- a ) dirty flag for block buffer 0
| dnegate ( d -- d ) negate double cell value
| do$ ( -- a ) push location of compiled string, jump over it
| does> ( -- ) part of `create...does>` routine
| dpl ( -- a ) address of double cell number decimal position
| drop ( n -- ) drop top of stack
| dump ( a u -- ) dump array to output
| dup ( n -- n n ) duplicate top of stack
| echo ( c -- ) emit a single character, terminal output echo
| editor ( -- ) load block editor word set, setup editor
| eforth ( -- ver ) push eforth version number
| else ( -- ) part of if...else...then statement
| emit ( c -- ) display a single character 
| empty-buffers ( -- ) call clean and invalidate
| erase ( a u -- ) write zero to array
| eval ( "line" -- ) evaluate line got with query
| evaluate ( ??? a u -- ??? ) evaluate string
| execute ( ??? xt -- ??? ) execute an execution token
| exit ( -- ) compile-only, exit current word definition
| expect ( a u -- ) calls accept, stores result in span
| extract ( ud ud -- ud u ) extract digit from number
| file ( -- ) ready I/O for file transfer instead of console
| fill ( a u c -- ) fill array with byte n
| find ( b -- pwd 1 | pwd -1 | a 0 ) find word in dictionary
| flush ( -- ) discard and un-assign dirty block buffers
| for ( --, Runtime: n --, R: -- n ) for...aft...then..next loop
| forth ( -- ) set search order to root-voc and forth-wordlist
| forth-wordlist ( -- voc ) push the default Forth vocabulary
| get-current ( -- voc ) equivalent to "current @"
| get-input ( -- n1...n5 ) get the input stream state
| get-order ( -- voc0...vocn n ) get search order
| h? ( -- a ) push the location of the dictionary pointer
| hand ( -- ) set default xt for I/O for terminal
| here ( -- u ) current dictionary position
| hex ( -- ) set number input/output radix to hexadecimal
| hide ( "name" -- ) toggle hidden bit in word definition
| hld ( -- a ) user variable index into hold space
| hold ( c -- ) add c to hold space in Pictured Numeric Output
| if ( --, Runtime: n -- ) immediate, compile-only, if-statement
| immediate ( -- ) make last defined word immediate
| info ( -- ) print system information
| ini ( -- ) initialize current task
| interpret ( b -- ) interpret a counted word
| invalidate ( -- ) invalidate blk0 storing -1 to it
| invert ( u -- u ) bitwise invert
| io! ( -- ) setup input/output routines
| key ( -- c ) get character from input
| key? ( -- c 0 | -1 ) get character from input or -1 on failure
| ktap ( bot eot cur c -- bot eor cur ) handle terminal input
| last ( -- a ) get last defined word
| leq0 ( n -- 0 | 1 ) 1 if n is less than or equal to 0, else 0
| line ( k l -- a u ) index into block by 64 byte lines
| list ( blk -- ) list a block, set scr
| literal ( n -- Runtime: -- n ) immediate, compile number
| load ( ??? blk -- ??? ) execute code stored in a block
| loaded? ( k -- k f ) check to see if block is loaded already
| loadline ( ??? k l -- ??? ) evaluate a line )
| look ( b u c xt -- b u ) skip until xt succeeds
| lshift ( u n -- u ) left shift u by n
| m/mod ( d n -- r q ) floored division with remainder/quotient
| mark ( -- a ) mark a hole in dictionary
| marker ( "name" -- ) make word that deletes words later after
| match ( c1 c2 -- f ) used with look in parse
| max ( n n -- n ) signed maximum of two numbers
| min ( n n -- n ) signed minimum of two numbers
| mod ( n1 n2 -- n1%n2 ) compute modulus of n1 divided by n2
| ms ( n -- ) wait for approximately n milliseconds
| multi ( -- ) enable multithreading mode, single turns it off
| mux ( n1 n2 sel -- n ) bitwise multiplex operation
| negate ( n -- n ) twos compliment negation
| next ( -- ) part of for...next/for..aft...then...next loop
| nfa ( pwd -- nfa ) move pwd to name field address
| nip ( n1 n2 -- n2 ) discard second stack item
| number? ( a u -- d -1 | a u 0 ) easier to use than >number
| ok ( -- ) state aware okay prompt
| only ( -- ) set vocabulary to only the root-voc
| or ( n n -- n ) bitwise or
| over ( n1 n2 -- n1 n2 n1 ) duplicate second item on stack
| pace ( -- ) emit pacing character
| pad ( -- a ) get thread local pad or scratch space
| page ( -- ) clear screen (using ANSI terminal codes)
| parse ( "string" c -- b u ) parse a c delimited string
| pause ( -- ) invoke multithreading scheduler, yield
| pick ( nu...n0 n -- nu...n0 nn ) pick item on stack
| postpone ( "name" -- ) immediate, compile word into dict.
| query ( -- ) get a line of text, filling the terminal buffer
| quit ( -- ) interpreter loop
| r> ( -- n, R: n -- ) move value from return stack to var stk.
| r@ ( -- n, R: n -- n ) copy value from return stack
| radix ( -- u ) retrieve input/output radix in base
| rdrop ( --, R: n -- ) drop value from return stack
| receive ( -- msg task-addr ) pause until message received
| recurse ( -- ) immediate, compile-only, recurse current word
| repeat ( -- ) part of begin...while...repeat loop
| root-voc ( -- voc ) push root vocabulary
| rot ( n1 n2 n3 -- n2 n3 n1 ) rotate three stack items
| rp! ( n -- , R: ??? -- ??? ) set return stack pointer
| rp@ ( -- n, R: ??? -- ??? ) get return stack pointer
| rshift ( u n -- u ) perform rshift of u by n
| s>d ( n -- d ) convert single cell number to double cell
| save-buffers ( -- ) save all block buffers to disk
| scr ( -- a ) last listed block as used with `list`
| search-wordlist ( a voc -- pwd 1| pwd -1| a 0 ) search voc
| see ( "name" -- ) decompile word
| send ( msg task-addr -- ) blocking send message to task
| set-current ( voc -- ) set current variable
| set-input ( n1...n5 -- ) set input execution tokens
| set-order ( n1...nx x -- ) set search order, -1 is special
| shed ( n1 n2 n3 -- n2 n3 ) remove third-most stack item
| sign ( -- ) add sign to hold space in Pictured Numeric Output
| signal ( addr -- ) signal to thread calling wait
| single ( -- ) force single threaded mode
| source ( -- a u ) get terminal input source
| source-id ( -- u ) get input type (0 = terminal, -1 = block)
| sp ( -- a ) variable containing the stack address
| sp! ( sp -- ) set stack pointer
| sp@ ( -- sp ) get stack pointer
| space ( -- ) emit a space character
| span ( -- a ) user variable set when calling expect
| state ( -- a ) push address of stack control location
| swap ( n1 n2 -- n2 n1 ) swap two stack items
| system ( -- voc ) push system vocabulary
| tap ( bot eot cur c -- bot eor cur ) add char to line
| task-init ( task-address -- ) initialize a task
| task: ( "name" -- ) create a named task
| then ( -- ) part of if...then or if...else...then
| this ( -- a ) address of task thread memory
| throw ( n -- ) throw n to be caught by catch, 0 is no throw
| tib ( -- b ) get the Terminal Input Buffer address
| toggle ( u a -- ) toggle bits at address [xor them with u]
| token ( "name" -- ) equivalent to "bl word"
| transfer ( a a u -- ) transfer bytes to/from mass storage
| tuck ( n1 n2 -- n1 n2 n1 ) tuck a variable behind two
| tup ( -- a ) get address of the Terminal Input Buffer
| type ( a u -- ) emit string displaying it
| u. ( u -- ) display unsigned number
| u.r ( u n -- ) display unsigned number space filled up to n
| u< ( u1 u2 -- f ) u1 unsigned less than u2
| u<=  ( u1 u2 -- f ) u1 unsigned less than or equal to u2
| u> ( u1 u2 -- f ) u1 unsigned greater than u2
| u>= ( u1 u2 -- f ) u1 unsigned greater than or equal to u2
| um* ( u u -- ud ) mixed multiply
| um+ ( u u -- u carry ) mixed add with carry
| um/mod ( ud u -- ur uq ) unsigned double cell div/mod
| unmatch ( c1 c2 -- f ) used with look in parse
| until ( --, Runtime: u -- ) part of begin...until loop
| update ( -- ) mark last loaded block as dirty or modified
| user ( "name" -- ) create a new thread local user variable
| user? ( -- a ) address of the user variable pointer
| valid? ( k -- k f ) is block valid?
| variable ( "name" -- ) create a variable
| wait ( addr -- ) pause until contents of address is non zero
| while ( --, Runtime: u -- ) part of begin/while/repeat loop
| within ( u lo hi -- f ) is u within lo and hi, lo <= u < hi
| word ( "string" c -- ) parse string until c
| words ( -- ) display loaded words
| xio ( xt xt xt -- ) exchange input/output 
| xor ( u u -- u ) bitwise exclusive or
;file
[then]

+ffs
+dos
.( DONE ) cr \ And we are finished and ready to go!
