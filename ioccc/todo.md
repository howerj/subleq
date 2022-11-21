- Branch and create new IOCCC eForth image.
  - Reduce size as much as possible at the expense of speed.
  This can be done with more "higher level" Forth, mux, lsb
  and shift removal.
- <https://github.com/ioccc-src/mkiocccentry>
- Separate Self-interpreter program that can be prepended.
- LZ\_XOR / LZ\_SUB to compress image?
- Make a simple bootloader and embed it here to replace atoi()?
- More obfuscation.
  - Swap memory locations with XOR, then swap it back later.
- Test on 16/32/64 bit systems and windows.
- Tidy up build system once a new image has been made.
- Callbacks to prior IOCCC entries, like the previous
  Forth interpreter?
- Hide my name/email in image
- Move checksum code to SUBLEQ assembly and not in VM
- SUBLEQ debugger in C
- A full SUBLEQ book loader and system:
  - Options variable for controlling EOF behavior, echoing, etcetera.
  - A eForth image, along with source.
  - Written in SUBLEQ assembler:
    - An additive checksum over the rest of the image.
    - SUBLEQ cell width detection
    - A bootloader capable of loading programs over standard input
    by reading in decimal numbers, terminated with a period.
    - A self interpreter for SUBLEQ capable of emulating an N-bit
    machine, so long as N is less than the bit width of the machine
    that is being currently run.
    - An LZSS decompressor, based off of LZ\_XOR or an equivalent
    using subtraction.
  - Add to book / original eForth source.
  - Make a simple DOS like operating system and file system?
