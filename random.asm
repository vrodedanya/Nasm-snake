org 0x100

mov si, 1
mov cx,50
i:
	call random
	pop dx
	xor ax, ax
	mov al, dl
	mov bl, 6
	div bl
	inc ah
	mov byte[next + si], ah
	inc si
loop i
ret


random:
pop bp
	mov al, 11
	mov bl, byte[next]
	mul bl
	mov bl, 128
	div bl
	mov byte[next],ah
	xor dx, dx
	mov dl, ah
	push dx
push bp
ret



next db 3
