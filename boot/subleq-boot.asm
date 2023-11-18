;;; Source: https://stackoverflow.com/questions/21846342/how-to-use-more-than-512-bytes-of-my-own-bootable-floppy/69622813#69622813

	[org 0x7c00]
								; stack and segment setup
	xor ax, ax
	mov es, ax
	mov ds, ax
	mov bp, 0x1200              ; thank you user ecm for notifying me to not use
											; 0x8000 as the stack address
	mov ss, ax                  ; thank you user ecm for notifying me to add this specified line of code
	mov sp, bp
											; load more than 512 bytes into memory
	mov ah, 0x02                ; read sectors
	mov al, 0x01                ; sectors to read
	mov ch, 0x00                ; cylinder idx
	mov dh, 0x00                ; head idx
	mov cl, 0x02                ; sector idx
	mov dl, 0x00                ; disk idx
	mov bx, program             ; address of more than 512 bytes
	int 0x13

								; because i'm tired you can do the error checking by checking if al is the
								; same as what you set it to and checking for the carry flag

								; jump to program (no error checking!)
	jmp program

	times 510-($-$$) db 0
	dw 0xAA55

								; more than 512 bytes program
program:
	mov ah, 0x0E
	mov bh, 0x00
	mov al, 'Z'
	int 0x10
	jmp $

								; amount of zeros = 512 + (number of sectors read * 512)
	times 1024-($-$$) db 0
