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

