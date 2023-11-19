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
    mov bx, data                ; address of more than 512 bytes
    int 0x13

                                ; because i'm tired you can do the error checking by checking if al is the
                                ; same as what you set it to and checking for the carry flag

    call cls

    mov dx, 0x0000
    call moveto

    xor si, si                  ; sp = SI

    ;; 0. if (pc < 0) halt
    ;; pc += 3
    ;; 1. A < 0  [B] = getchar
    ;; 2. B < 0  putchar [A]
    ;; 3. [B] = [B] - [A]; if ([B] <= 0) pc = C


subleq:
    cmp si, 0
    jl  exit

    mov ax, [data + si]         ; A = AX
    mov bx, [data + si + 2]     ; B = BX
    mov cx, [data + si + 4]     ; C = CX

    add si, 6

    cmp ax, 0                   ; 1. A < 0  [B] = getchar; pc += 3
    jl  .A_NEG

    cmp bx, 0                   ; 2. B < 0  putchar [A]
    jl  .B_NEG

    jmp .ELSE                   ; 3. [B] = [B] - [A]; if ([B] <= 0) pc = C

 .A_NEG:
    call getchar
    xor ah, ah
    shl bx, 1
    mov [data + bx], ax
    jmp subleq

  .B_NEG:
    mov di, ax
    shl di, 1
    mov ax, [data + di]
    call putchar
    jmp subleq

 .ELSE
    mov di, ax                   ; 3. [B] = [B] - [A]
    shl di, 1
    mov ax, [data + di]

    mov di, bx
    shl di, 1
    mov bx, [data + di]

    sub bx, ax

    mov [data + di], bx

    cmp bx, 0                   ; if ([B] <= 0) pc = C
    ja subleq

    shl cx, 1
    mov si, cx
    jmp subleq


;;;************************************
;;;          Subroutines              *
;;;************************************
exit:
    cli
    hlt

cls:
    mov ah, 0x07                ; tells BIOS to scroll down window
    mov al, 0x00                ; clear entire window
    mov bh, 0x07                ; white on black
    mov cx, 0x00                ; specifies top left of screen as (0,0)
    mov dh, 0x18                ; 18h = 24 rows of chars
    mov dl, 0x4f                ; 4fh = 79 cols of chars
    int 10h                     ; calls video interrupt
    ret

moveto:                         ; move cursor to DH = Row, DL = Column
    mov ah, 0x02
    mov bh, 0x00
    int 10h
    ret

putchar:                        ; put char in al
    mov ah, 0x0E
    mov bh, 0x00
    int 0x10
    cmp al, 13
    je  .LF
    ret
 .LF:
    mov al, 10
    jmp putchar

getchar:                        ; get char in al
    mov ah, 0x00
    int 0x16
    ret

    times 510-($-$$) db 0
    dw 0xAA55

                                 ; more than 512 bytes program
data: dw \
    15, 17, -1,      17, -1, -1,    16, 1, -1,       16, 3, -1,     15, 15, 0,     0, -1, 72, \
    101, 108, 108,   111, 44, 32,   119, 111, 114,   108, 100, 33,  0, 0

    ;;  3, -10, 0, 98, \  ; print char
    ;;  -1, 3, 0, \ ; putchar

                                ; amount of zeros = 512 + (number of sectors read * 512)
    times 1024-($-$$) db 0
