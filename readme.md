# SUBLEQ

* Author: Richard James Howe
* Email: <mailto:howe.r.j.89@gmail.com>
* Repo: <https://github.com/howerj/subleq>
* License: The Unlicense

A 16-bit SUBLEQ computer, with a few special cases to halt the machine and
for Input/Output purposes. The goal of this project is to get a eForth
interpreter up an running in a virtual machine, perhaps get it running
on an FPGA, and compare it to the [bit-serial](https://github.com/howerj/bit-serial)
CPU and project to see if this esoteric system is faster or produces a
smaller system on an FPGA.

An 8-bit SUBLEQ machine might be nice to target also, however that would
be a little more difficult, but still doable. A 16-bit virtual machine
would have to be emulated anyway. A few modifications to the SUBLEQ machine,
which are not implemented, would increase speed, reduce memory usage or both,
one is two have an always zero register, which reduces the size of some
common assembly routines which have to zero the register after use. Another
obvious one, which defeats the purpose of the machine, is to add more
instructions, just adding one or two instructions can greatly speed up the
system, such as left/right shifts and a bitwise instruction such as NAND. If
just one instruction were to be added, NAND would be it.

## Building and Running

To build and run the SUBLEQ VM program you will need Make and a C compiler,
GNU make and GCC should work. To build the image that is run on the VM, you
will need [gforth](https://gforth.org/). Type:

	make

To build the SUBLEQ Virtual Machine, and the target image. Type:

	make run

To run the target image in the VM.

## SUBLEQ Specification

SUBLEQ is a One Instruction Set Computer, or OISC, there are multiple types
of OISC of which SUBLEQ is one. SUBLEQ stands for "Subtract and Branch if
Less Than Or Equal (to Zero)", each SUBLEQ instruction consists of three
operands called "a", "b", and "c", the contents of the memory location pointed
to by "a" is subtracted from the contents of the memory location pointed to
by "b", the result is stored in "b", if the result is less than or equal to
zero then execution continues on from the location pointed to by "c", otherwise
execution continues on at the next instruction. This specification does not
mention anything about input or output yet, most implementations get the CPU
to treat memory locations specially, not quite a direct memory mapping, as
the CPU operation is directly effected by the I/O, other implementations do
use direct memory mapping.

The SUBLEQ Virtual Machine is quite small at around 900 bytes of C and
is available [here](subleq.c).

## To Do

* [x] Implement virtual machine and skeleton of project
  * [ ] Optional: Add proper terminal handling for Unix and Windows
  * [ ] Optional: Implement 32-bit version of VM and eForth
  * [ ] Add more realistic I/O (eg. return failure if we cannot output a byte)
* [x] Implement SUBLEQ routines
* [ ] Implement eForth
  - [x] Implement Virtual Machine capable of executing Forth
  - [ ] Implement Forth on the virtual machine
  - [ ] Make the system self-hosting, and remove gforth as a dependency.
  - [ ] Make a LZSS compressed image that is decompressed at run time,
        a CRC check would be nice also.
  - [ ] Micro-optimizations (eg. merging jumps with previous instructions,
    which may still need the following jump).
* [ ] Documentation
* [ ] Optional: Port to FPGA
* [ ] Optional: Port and improve self-interpreter with I/O
  - See <http://www.mazonka.com/subleq/index.html>
  - See <https://eigenratios.blogspot.com/2006/09/mark-ii-oisc-self-interpreter.html>

## References

* <https://en.wikipedia.org/wiki/Forth_(programming_language)>
* <https://en.wikipedia.org/wiki/One-instruction_set_computer>
* <https://github.com/howerj/bit-serial>
* <http://gerigeri.uw.hu/DawnOS/index.html>
* <https://hackaday.io/project/158329-dawn-the-subleq-operating-system-by-geri>
* <https://rosettacode.org/wiki/Subleq>
* <https://github.com/8l/hsq>
* <http://mazonka.com/subleq/>
* <https://stackoverflow.com/questions/2982729>
* <https://stackoverflow.com/questions/34120161>
* <https://web.ece.ucsb.edu/~parhami/pubs_folder/parh88-ijeee-ultimate-risc.pdf>

