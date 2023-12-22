;;; Source: https://stackoverflow.com/questions/21846342/how-to-use-more-than-512-bytes-of-my-own-bootable-floppy/69622813#69622813
    bits 16
    cpu 8086

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
    mov al, 65                  ; number of sectors to read
    mov ch, 0x00                ; cylinder idx
    mov dh, 0x00                ; head idx
    mov cl, 0x02                ; sector idx
    mov dl, 0x00                ; disk idx
    mov bx, data                ; address of more than 512 bytes
    int 0x13

    jc  exit                    ; check int 13h errors
    cmp al, 65
    jne exit

    call cls

    mov dx, 0x0000
    call moveto

    mov ax, 'G'
    call putchar
    mov ax, 'o'
    call putchar
    mov ax, 13
    call putchar

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; SUBLEQ                                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; 0. if (pc < 0) halt
;;; pc += 3
;;; 1. A == -1  [B] = getchar
;;; 2. B == -1  putchar [A]
;;; 3. [B] = [B] - [A]; if ([B] <= 0) pc = C

    xor si, si                          ; pc = SI

subleq:
    cmp si, 0
    jl  exit

    mov ax, [data + si]                 ; A = AX
    mov bx, [data + si + 2]             ; B = BX
    mov cx, [data + si + 4]             ; C = CX

    add si, 6

    cmp ax, -1                          ; 1. A == -1 ?  [B] = getchar
    je  .A_NEG_GETCHAR

    cmp bx, -1                          ; 2. B == -1 ?  putchar [A]
    je  .B_NEG_PUTCHAR

    jmp .ELSE                           ; 3. [B] = [B] - [A]; if ([B] <= 0) pc = C

 .A_NEG_GETCHAR:
    mov di, bx
    shl di, 1
    call getchar
    mov [data + di], ax
    jmp subleq

 .B_NEG_PUTCHAR:
    mov di, ax
    shl di, 1
    mov ax, [data + di]
    call putchar
    jmp subleq

 .ELSE:
    mov di, ax                          ; 3. [B] = [B] - [A]
    shl di, 1
    mov ax, [data + di]

    mov di, bx
    shl di, 1
    mov bx, word [data + di]

    sub bx, ax

    mov word [data + di], bx

    cmp bx, 0
    jg subleq                           ; [B] > 0

    shl cx, 1                           ; [B] <= 0 ! pc = c
    mov si, cx
    jmp subleq


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;          Subroutines                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
exit:
    mov ax, 'X'
    call putchar
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
    xor ah, ah

    call translate_char

    push ax                     ; echo char
    push bx
    call putchar
    pop bx
    pop ax
    ret

    times 510-($-$$) db 0
    dw 0xAA55

    ;; more than 512 bytes program
data:
    ;;%include "./simple-dec.asm"
    ;;%include "./test-3rd-dec.asm"
    ;;%include "./hello-world-dec.asm"
    %include "./eforth-dec.asm"

    ;; amount of zeros = 512 + (number of sectors read * 512)
    times 32768-($-$$) db 0

translate_char:
    ;; Uncomment for an FR keyboard (azerty)
    ;; %include "./keyb-FR.asm"
    ret
