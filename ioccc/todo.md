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
