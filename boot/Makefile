all: build run

.PHONY: build run

build:
	nasm -f bin subleq-boot.asm -o boot.com
	dd status=noxfer conv=notrunc if=boot.com of=subleq-boot.flp

run:
	qemu-system-i386 -boot order=a -fda subleq-boot.flp

clean:
	rm -f boot.com subleq-boot.flp
