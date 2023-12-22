all: build run

.PHONY: build run

eforth-dec.asm: ../subleq.dec
	echo "dw \\" > eforth-dec.asm
	sed 's/$$/, \\/' ../subleq.dec >> eforth-dec.asm

build: subleq-boot.asm eforth-dec.asm
	nasm -f bin subleq-boot.asm -o boot.com
	dd status=noxfer conv=notrunc if=boot.com of=subleq-boot.flp

run: build
	qemu-system-i386 -drive file=subleq-boot.flp,format=raw,index=0,if=floppy,readonly=on

dbg: build
	qemu-system-i386 -drive file=subleq-boot.flp,format=raw,index=0,if=floppy,readonly=on -s -S

clean:
	rm -f boot.com subleq-boot.flp eforth-dec.asm
