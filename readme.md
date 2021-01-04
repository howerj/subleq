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

## To Do

* [x] Implement virtual machine and skeleton of project
  * [ ] Optional: Add proper terminal handling for Unix and Windows
  * [ ] Optional: Add debugging functionality
* [ ] Implement SUBLEQ routines
* [ ] Implement eForth
  - [ ] Implement Virtual Machine capable of executing Forth
  - [ ] Implement Forth on the virtual machine
* [ ] Documentation
* [ ] Optional: Port to FPGA
* [ ] Optional: Port and improve self-interpreter with I/O
  - See <http://www.mazonka.com/subleq/index.html>
  - See <https://eigenratios.blogspot.com/2006/09/mark-ii-oisc-self-interpreter.html>

## References

* <https://en.wikipedia.org/wiki/Forth_(programming_language)>
* <https://github.com/howerj/bit-serial>
* <http://gerigeri.uw.hu/DawnOS/index.html>
* <https://hackaday.io/project/158329-dawn-the-subleq-operating-system-by-geri>
* <https://rosettacode.org/wiki/Subleq>
* <https://en.wikipedia.org/wiki/One-instruction_set_computer#Subtract_and_branch_if_less_than_or_equal_to_zero>
* <https://github.com/8l/hsq>
* <http://mazonka.com/subleq/>

