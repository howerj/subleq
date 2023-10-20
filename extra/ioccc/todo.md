- [ ] Branch and create new IOCCC eForth image.
  - [ ] Reduce size as much as possible at the expense of speed.
  This can be done with more "higher level" Forth, mux, lsb
  and shift removal.
- [ ] <https://github.com/ioccc-src/mkiocccentry>
- [ ] Separate Self-interpreter program that can be prepended.
  - [ ] Deal with arbitrary bit length 
- [ ] LZ\_XOR / LZ\_SUB to compress image?
- [ ] Test on 16/32/64 bit systems and windows.
- [ ] Tidy up build system once a new image has been made.
- [ ] Hide my name/email in image
  - [ ] Add to book / original eForth source.
- [ ] Make sure bit-width check works with new IOCCC program
  (seems to fail sometimes, indicating different between this
  program and nbit.c)
- [ ] Make a permutation of the function pointers to obfuscate
  things further.
- [ ] Shrink one-line candidate to under 140 characters if possible,
  ideally 120 characters, but that is probably impossible.
