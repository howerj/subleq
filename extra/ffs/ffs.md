# Forth File System (FFS)

* Author: Richard James Howe
* License: The Unlicense / Public Domain
* Email: howe.r.j.89@gmail.com
* Repo: TBD

The usual way file systems are accessible under Forth is the following:

	File System <-> BIOS/Kernel <-> File Access Methods

The BLOCK word-set is either implemented upon the File Access Methods, or
in the case of an embedded system it is implemented via direct transfers
to a mass storage device.

This File System is instead structured like this:

	BIOS/Kernel/Drivers <-> Block Word Set <-> File System <-> File Access Methods

This allows for a portable, albeit limited, File System to be rapidly
deployed to new embedded systems.


## File System Structure

The overall structure is as follows:

* BLOCK 1:        Header
* BLOCK 2 to N:   File Allocation Table
* BLOCK N+1 to M: File/Data Blocks (Block N+1 contains the first directory).

A block consists of a 1024 byte block of memory, aligned on a 1024
byte boundary.

There are three main data structures, the header, the File Allocation
Table and Directories.

### Header

The header is the first block and contains meta-data about the file
system and magic strings used to identify the partition. 

You will need to read this header to determine the size of the
partition and how many blocks are used by the File Allocation Table.

**TODO: Ellaborate and describe structure**

### File Allocation Table

The File Allocation Table (FAT) is used to determine which blocks
are allocated to files, directories and other special uses (for example
the first entry is taken by the header, and then the following entries
are used by the FAT itself).

The entries in the FAT either consist of special values or are
pointers to the next block in one of many chains of blocks terminated
by a special value.

The special values are:

* 0: Free block
* 1: Terminating sentinel value, used to terminate a chain of blocks
* All Bits Set: Invalid/Block not allocated to system
* All Bits Set - 1: Read/Write error block / bad block
* All Bits Set - 2: Special

### Directory Structure

**In Flux/TBD**

Some notes on the directory structure:

* A directory can be an arbitrary number of blocks in length (determined
by the FAT), the list is terminated by either the terminating value in
the FAT chain or by a NULL entry in the current directory block (the
file system is in an invalid state if their are more blocks allocated to
the directory after there is a NULL entry).
* The first block after the FAT contains the first block of the top
level directory.
* Each directory entry is 32 bytes in length, as each block is 1024 bytes
in size this means that 32 directory entries can be stored in a block.

A directory entry consists of:

* 0:     TYPE (0 = UNUSED, 1 = DIR, 2 = FILE, 3 = SPECIAL)
* 1:     RESERVED (must be zero)
* 2-3:   BYTES USED IN LAST BLOCK (How many bytes are used)
* 4-5:   FILE START BLOCK
* 6-7:   RESERVED FOR FILE START BLOCK EXTENSION
* 8-11:  RESERVED FOR LAST UPDATE TIME
* 12-15: RESERVED (must be zero)
* 16-31: NAME (File, directory, or special data)

The length of a file can be determined by:

        BLOCKS IN FILE * 1024 - (1024 - BYTES USED IN LAST BLOCK)

Given a pointer to a file in the FAT the next block in the chain
can be determined by dereferencing that pointer, which will contain
another block.

## File System Limitations

* File and directory names are limited to 16 bytes, including any
dots and file extensions.
* Blocks are always 1024 bytes in length.
* The minimum size of the File System is 3 blocks (3 * 1024 bytes).
* For a FFS with a 16-bit cell size in the FAT (meaning 512 entries
can be stored per block) the maximum partition size is 65536 * 1024
bytes, or 64MiB.
* The maximum file size on a FFS with a 16-bit FAT cell size is
(65536 - 3) * 1024.

## To Do

* Holes have been reserved for forwards compatibility, make sure
you don't have to do awkward bit twiddling to read them (i.e. make
sure the endianesses add up).
* Implement what is written here.
* Add optional checksum, or RWX permissions, to file structure?
