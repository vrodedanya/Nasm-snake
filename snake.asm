org 0x100

mov ah, 0x0
mov al, 0x10
int 0x10 ; enable graphic mode

mov ah, 0x0
int 0x1a
mov byte[next],dl

call input_snake
call input_apple
for:
	call move_snake
	push 0 ; counts
	push 1 ; color
	push 18 ; length 
	push 0 ; x
	push 0 ; y
	call draw_matrix
	add sp, 10
	call event_handler
jmp for

end:
mov ah, 0x0
mov al, 0x3
int 0x10 ; enable text mode

push 0
ret

event_handler:
	pop dx 
	mov ah, 0x1
	int 0x16 ; check event
	jz skip_event
	
	mov ah, 0x0
	int 0x16	
	
	skip_event:
	cmp al,'q'
	jz end
	cmp al,'d'
	jnz event_1
		mov word[way], 1
		mov word[way + 2], 0
		jmp event_end
	event_1:
	cmp al,'a'
	jnz event_2	
		mov word[way], -1
		mov word[way + 2], 0
		jmp event_end
	event_2:
	cmp al,'s'
	jnz event_3
		mov word[way + 2], 1
		mov word[way], 0
		jmp event_end
	event_3:
	cmp al,'w'
	jnz event_end
		mov word[way + 2], -1
		mov word[way], 0
		jmp event_end
	event_end:
	push dx
ret

draw_rect: ; color, length, x, y
	mov bh,0
	mov bp, sp
	mov ax, word[bp + 8] ; color
	mov cx, word[bp + 4]  ; x
	mov dx, word[bp + 2]  ; y
	mov si, cx  ; x + length
	add si, word[bp + 6]
	mov di, dx  ; y + length
	add di, word[bp + 6]
	mov ah, 0xC ; Function
	i:
		j:
			int 0x10
			inc cx
			cmp cx, si
			jnz j ; If cx more than length value + x0
		mov cx, word[bp + 4]
		inc dx
		cmp dx, di
		jnz i
ret

draw_matrix: ; color, length, x, y
	push matrix
	matrix_i:
		mov bp, sp
		mov bx, word[bp]
		cmp byte[bx], 1
		jnz clr_check_1
			mov word[bp + 10], 1 ; if 1 - blue
			jmp skip
		clr_check_1:
		cmp byte[bx], 0 
		jnz clr_check_2
			inc word[bp] ; if 0 - next line
			mov ax, word[bp + 8]
			inc ax
			add word[bp + 4], ax
			mov word[bp + 6], 0
			jmp matrix_i
		clr_check_2: ; if 2 - green
		cmp byte[bx], 2
		jnz clr_check_3
			mov word[bp + 10], 2
			jmp skip
		clr_check_3: ; if 3 - black
		cmp byte[bx], 3
		jnz clr_check_4
			mov word[bp + 10], 0
			jmp skip
		clr_check_4:
		cmp byte[bx], 4
		jnz clr_check_5
			mov word[bp + 10], 4
			jmp skip
		clr_check_5:
			mov word[bp + 10], 5
		skip:
		push word[bp + 10]
		push word[bp + 8]
		push word[bp + 6]
		push word[bp + 4]
		call draw_rect
		add sp, 8
		mov bp,sp
		
		inc word[bp + 12] ; increment counter
		mov ax, word[bp + 8] 
		inc ax
		add word[bp + 6], ax ; move OX
		inc word[bp]
		inc word[bp + 10]
	cmp word[bp + 12], 81
	jnz matrix_i
	add sp, 2
ret

change_place: ; new_value, x, y
	pop bp
	mov di, 10 ;length
	pop cx
	pop si
	pop ax
	mult_i:
		add si, di
	loop mult_i
	mov bx, matrix
	mov byte[bx + si], al
	push bp
ret

check_place: ; x,y
pop bp
	mov di, 10
	pop cx
	pop si

	check_i:
		add si, di
	loop check_i
	mov bx, matrix
	mov dl, byte[bx + si]
	xor dh,dh
	push dx
push bp
ret

input_snake:
	push 0
	i_snake:
		mov bp, sp
		mov si, word[bp]
		cmp word[snake_str + si], -1
		jz exit_snake
		push 2
		push word[snake_str + si]
		push word[snake_str + si + 2]
		call change_place
		
		mov bp, sp
		add word[bp], 4
		jmp i_snake
	exit_snake:
	add sp, 2
ret

input_apple:
	input_apple_repeat:
	call random
	pop ax
	mov bl, 7
	div bl
	inc ah
	mov al,ah
	xor ah,ah
	mov si,ax
	call random
	pop ax
	mov bl, 7
	div bl
	inc ah
	mov al,ah
	xor ah,ah
	mov di,ax

	push si
	push di

	push si
	push di
	call check_place
	pop ax
	pop di
	pop si
	cmp ax, 2
	jz input_apple_repeat
	push 4
	push si
	push di
	call change_place
ret

add_new:
	mov bx, snake_str
	mov cx, 49
	check_not_added:
	cmp word[bx],-1
	jz exit
	add bx, 4
	loop check_not_added
	exit:
	mov si, word[bx - 4]
	mov di, word[bx - 2]
	mov word[bx], si
	mov word[bx + 2], di
	mov byte[apple], 1
ret

check_barrier: ; x, y
	mov bp, sp
	push word[bp + 4]
	push word[bp + 2]
	call check_place
	pop cx
	cmp cx, 4
	jnz not_apple
	call add_new
	not_apple:
	cmp cx, 2
	jz end
ret

move_snake:
	mov si, word[snake_str]
	add si, word[way]
	cmp si, 0
	jnz move_check_1
		mov ax, word[snake_str]
		mov bx, word[snake_str + 2]
		pusha
		push 3
		push word[snake_str]
		push word[snake_str + 2]
		call change_place
		popa
		push 7
		push word[snake_str + 2]
		call check_barrier
		add sp,4
		mov word[snake_str],7
		jmp move_next
	move_check_1:
	mov si, word[snake_str]
	add si, word[way]
	cmp si, 8
	jnz move_check_2
		mov ax, word[snake_str]
		mov bx, word[snake_str + 2]
		pusha
		push 3
		push word[snake_str]
		push word[snake_str + 2]
		call change_place
		popa

		push 1
		push word[snake_str + 2]
		call check_barrier
		add sp,4

		mov word[snake_str],1
		jmp move_next
	move_check_2:
	mov si, word[snake_str + 2]
	add si, word[way + 2]
	cmp si, 0
	jnz move_check_3
		mov ax, word[snake_str]
		mov bx, word[snake_str + 2]
		pusha
		push 3
		push word[snake_str]
		push word[snake_str + 2]
		call change_place
		popa

		push word[snake_str]
		push 7
		call check_barrier
		add sp,4
		mov word[snake_str + 2], 7

		jmp move_next
	move_check_3:
	mov si, word[snake_str + 2]
	add si, word[way + 2]
	cmp si, 8
	jnz move_skip
		mov ax, word[snake_str]
		mov bx, word[snake_str + 2]
		pusha
		push 3
		push word[snake_str]
		push word[snake_str + 2]
		call change_place
		popa
		
		push word[snake_str]
		push 1
		call check_barrier
		add sp,4
		mov word[snake_str + 2], 1

		jmp move_next
	move_skip:

	
	
	pusha
	push 3
	push word[snake_str]
	push word[snake_str + 2]
	call change_place
	popa
	
	mov di,word[way + 2]
	mov si,word[way]
	cmp di,0
	jnz not_zero
	cmp si,0
	jz move_exit

	not_zero:

	mov dx, word[way]
	mov si, word[snake_str]
	add si, dx
	push si
	mov si, word[snake_str + 2]
	mov dx,word[way + 2]
	add si, dx
	push si
	call check_barrier
	add sp,4

	mov ax, word[snake_str]
	mov bx, word[snake_str + 2]
	
	mov dx, word[way] ; move first body
	add word[snake_str], dx
	mov dx, word[way + 2]
	add word[snake_str + 2], dx
	
	move_next:
	mov si, 4
	move_i:
		cmp word[snake_str + si], -1
		jz move_exit
		mov dx, word[snake_str + si]
		mov di, word[snake_str + si + 2]
		
		pusha
		push 3
		push word[snake_str + si]
		push word[snake_str + si + 2]
		call change_place
		popa

		mov word[snake_str + si], ax ; move second body
		mov word[snake_str + si + 2], bx
	
		cmp word[snake_str + si + 4], -1
		jz move_exit
		mov ax, word[snake_str + si + 4]
		mov bx, word[snake_str + si + 6]
		
		pusha
		push 3
		push word[snake_str + si + 4]
		push word[snake_str + si + 6]
		call change_place
		popa

		mov word[snake_str + si + 4], dx
		mov word[snake_str + si + 6], di
		add si, 8
	jmp move_i
	move_exit:
	call input_snake
	cmp byte[apple], 1
	jnz nap
		call input_apple
		mov byte[apple], 0
	nap:
ret

random:
pop bp
	mov al, byte[next]
	mov bl, 9
	mul bl
	add ax, 3
	mov bl, 128
	div bl
	mov byte[next],ah
	xor dx, dx
	mov dl, ah
	push dx
push bp
ret

next db 3

apple db 0

snake_str dw 3,1, 2,1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1 , -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1 ,-1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1, -1,-1
way dw 0,0
matrix db 1,1,1,1,1,1,1,1,1,0, 1,3,3,3,3,3,3,3,1,0, 1,3,3,3,3,3,3,3,1,0, 1,3,3,3,3,3,3,3,1,0, 1,3,3,3,3,3,3,3,1,0, 1,3,3,3,3,3,3,3,1,0, 1,3,3,3,3,3,3,3,1,0, 1,3,3,3,3,3,3,3,1,0, 1,1,1,1,1,1,1,1,1,0,
