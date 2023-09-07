# SUBLEQ IMAGE CODEC

This folder contains experimental compression code, the idea being to create a
SUBLEQ program that is capable of decompressing an eForth image and then
booting into it. The size of the eForth image and the decompressor combined 
should be less than (or equal to) the size of the uncompressed file. The
compressor could be written in C, or most likely Forth so it can be run by the
meta-compiler in "subleq.fth".

The compression CODEC should be optimized to run on a SUBLEQ machine, so most
bitwise operations are too expensive to use (except bitwise negation and
testing the topmost bit in a cell). Speed is less of a concern.

The experimental code will be written in C, and if the compression ratio is
good enough whilst still being simple enough to implement in SUBLEQ assembly it
will (hopefully) be translated into SUBLEQ assembly where it could be prepended
to a compressed eForth image.

The compressor and decompressor operate on 16-bit space delimited ASCII 
signed decimals, so a smaller binary image could result in larger ASCII file.

# Ideas

1) Zero compression

First successful idea; zero compression. "subleq.dec" contains many repeated
zeros, negative values could represent runs of zeros, the absolute amount plus
one indicating the number of zeroes to output, a negative value of -1
could represent the next value is a literal, and anything else is to be treated
as a literal. This achieves a tiny saving, but no doubt an decoder written in
SUBLEQ would eat this saving up.

2) Range based LZSS

Potential idea; using <https://www.reuneker.nl/files/ngram/>, many n-grams of
length 2-5 (4 to 10 bytes) are present when run on "subleq.dec". This means
dictionary compression should be possible (which is confirmed by running
"subleq.dec" against an LZSS CODEC, albeit one that operates on bytes). This
requires encoding a length-offset pair. This could be done by looking at ranges
instead of bytes, a range of values (easier to check in SUBLEQ assembly) could
represent a length of a match, subtracting the lower value of that range would
give the offset. A similar encoding strategy to first idea could be used.


