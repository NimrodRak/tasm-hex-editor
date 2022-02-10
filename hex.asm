IDEAL
MODEL small
STACK 100h
; copy from bmp now
DATASEG

compl		  db 1 dup (0)
buffer		  db 132 dup (?)
bytes_read	  db 1 dup (0)
hex_digits	  db '0123456789ABCDEF$'
textfilename	  db 40 dup (1)
complement_toggle db 'inverted', 0
buffer_toggle db 'offset:', 0
textfilehandle	  dw 1 dup (0)
file_offset	  db 1 dup (0)
BYTES_PER_ROW equ 09h
BYTES_PER_COL equ 0Ch
INDEX_COLOR   equ 0Bh
VALUE_COLOR   equ 0Fh
CHAR_COLOR	  equ 0Fh
OFF_COLOR	  equ 0Ch
ON_COLOR	  equ 0Ah
HIDE_COLOR	  equ 00h
TRUE		  equ 01h
FALSE		  equ 00h

CODESEG


; takes in (coordinates, char, color)
proc printChar
	push bp
	mov bp, sp

	mov dx, [bp + 4] ; set the col and row
	mov  bh, 0    ;Display page
	mov  ah, 02h  ;SetCursorPosition
	int  10h

	mov  ax, [bp + 6] ; char
	mov  bx, [bp + 8] ; color
	mov  bh, 0    ;Display page
	mov  ah, 0Eh  ;Teletype
	int  10h
	pop bp
	ret
endp printChar

; takes in (coordinates, byte, color)
proc printByte
	push bp
	mov bp, sp

	; dl has the digit
	mov dx, [bp + 6]
	and dl, 0Fh
	mov bx, offset hex_digits
	add bl, dl
	mov dl, [bx]

	push [bp + 8] ; color parm for printChar
	push dx
	inc [bp + 4] ; move aside one col
	push [bp + 4] ; right col of byte
	call printChar
	add sp, 6

	mov dx, [bp + 6]
	shr dl, 4
	and dl, 0Fh
	mov bx, offset hex_digits
	add bl, dl
	mov dl, [bx]

	push [bp + 8] ; color parm for printChar
	push dx
	dec [bp + 4] ; move aside one col
	push [bp + 4] ; left col of byte
	call printChar
	add sp, 6

	pop bp
	ret
endp printByte

proc printLayout
	xor cx, cx
	print_col_index:
		; print byte cx at (cx + 1) * 3 - 1
		; print space at (cx + 1) * 3 + 2
		push INDEX_COLOR ; color
		push cx ; byte

		; calculate col
		mov dx, cx
		inc dl
		mov al, 3
		mul dl

		mov dl, al ; col
		dec dl
		mov dh, 0 ; row
		push dx

		call printByte
		add sp, 6

	inc cl
	cmp cl, BYTES_PER_ROW
	jl print_col_index

	xor cx, cx
	print_row_index:
		push INDEX_COLOR ; color
		push cx ; byte

		mov dl, 0 ; col
		mov dh, cl ; row
		inc dh
		mov al, 2
		mul dh
		mov dh, al
		dec dh
		push dx

		call printByte
		add sp, 6

	inc cl
	cmp cl, BYTES_PER_COL
	jl print_row_index
	ret
endp printLayout

proc openTextFile
	; Open file
	mov ah, 3Dh
	xor al, al

	lea dx, [textfilename]
	add dx, 2h

	int 21h
	jc openerror
	mov [textfilehandle], ax
	openerror:
	ret
endp openTextFile

proc complementBuffer
	xor cl, cl
	mov bx, offset buffer
	complement_loop:
		neg [byte ptr bx]
		inc bx
	inc cl
	cmp cl, [bytes_read]
	jb complement_loop
	ret
endp complementBuffer

proc seekFile
	mov ax, 4200h
	mov bx, textfilehandle
	xor cx, cx
	mov dl, [file_offset]
	xor dh, dh
	int 21h
	ret
endp seekFile

proc readFile
	mov ah,3Fh
	mov bx, [textfilehandle]
	mov cx, 132
	mov dx, offset buffer
	int 21h
	mov [bytes_read], al

	; complement if need be
	mov dl, 0
	not dl
	cmp [compl], dl
	jne no_complement

	call complementBuffer

	no_complement:
	ret
endp readFile

proc closeFile
	mov ah,3Eh
	mov bx, [textfilehandle]
	int 21h
	ret
endp closeFile

proc printBufferByte
	push bp
	mov bp, sp

	cmp [bp + 8], TRUE
	jne hide_byte
	push VALUE_COLOR
	jmp end_color
	hide_byte:
	push HIDE_COLOR
	end_color:

	mov cx, [bp + 4]
	mov bx, offset buffer
	mov al, BYTES_PER_ROW
	mov dl, ch
	mul dl
	add al, cl
	add bl, al

	; get byte from buffer at ch * BYTES_PER_ROW + BYTER_PER_COL
	push [bp + 6]

	; col is (ch + 1) * 3 - 1
	mov dl, cl
	inc dl
	mov al, 3
	mul dl
	mov dl, al
	dec dl

	; row is (2 * cl) + 1
	mov dh, ch
	mov al, 2
	mul dh
	mov dh, al
	inc dh

	push dx

	call printByte
	add sp, 6

	pop bp
	ret
endp printBufferByte

proc printBufferChar
	push bp
	mov bp, sp
	; TODO : add cursor matching for char-byte - don't
	; TODO : add file offset + text prompt for it
	; TODO : (mb don't) add edit option

	cmp [bp + 8], TRUE
	jne hide_char
	push CHAR_COLOR
	jmp end_color_char
	hide_char:
	push HIDE_COLOR
	end_color_char:

	mov ax, [bp + 6]
	; dx <= 7a && dx >= 61 || dx <= 41 && dx >= 5a

	cmp al, 41h               ; compare al with "A"
	jl non_numeric               ; jump to next character if less
	cmp al, 5Ah              ; compare al with "Z"
	jle is_numeric           ; if al is >= "A" && <= "Z" -> found a letter
	cmp al, 61h               ; compare al with "a"
	jl non_numeric               ; jump to next character if less (since it's between "Z" & "a")
	cmp al, 7Ah               ; compare al with "z"
	jg non_numeric               ; above "Z" -> not a character
	jmp is_numeric
	non_numeric:
	mov al, '.'
	is_numeric:
	push ax

	mov dx, 011Dh

	mov cx, [bp + 4]
	add dl, cl
	add dh, ch
	mov al, 2
	mul dh
	mov dh, al
	dec dh
	push dx

	call printChar
	add sp, 6

	pop bp
	ret
endp printBufferChar

proc printBufferItem
	push bp
	mov bp, sp

	push [bp + 8]
	push [bp + 6]
	push [bp + 4]

	call printBufferByte
	add sp, 6

	push [bp + 8]
	push [bp + 6]
	push [bp + 4]

	call printBufferChar
	add sp, 6

	pop bp
	ret
endp printBufferItem

proc printBuffer
	; ch holds current row
	xor ax, ax
	push ax
	xor ch, ch
	row_data_loop:
		; cl holds current col
		xor cl, cl
		col_data_loop:
			; check if these are hidden bytes
			pop ax
			cmp al, [bytes_read]
			push ax
			jb shown
			; if is hidden
			push FALSE
			push 0h
			jmp print_end

			shown:
			push TRUE
			; get current char
			mov bx, offset buffer
			add bl, al
			push [bx]

			print_end:
			push cx
			call printBufferItem
			add sp, 6

		; inc buffer index and col index
		pop ax
		inc ax
		push ax
		inc cl
		cmp cl, BYTES_PER_ROW
		jl col_data_loop

	inc ch
	cmp ch, BYTES_PER_COL
	jl row_data_loop

	pop ax

	ret
endp printBuffer

proc printColoredString
	push bp
	mov bp, sp
	xor cx, cx
	string_loop:
		mov bx, [bp + 6] ;offset complement_toggle
		add bx, cx

		cmp [byte ptr bx], 0
		je string_end

		push cx ; save actual index to state
		add cx, [bp + 4]
		push [bp + 8] ;0A on;C-off;E-highleted
		push [bx]
		push cx

		call printChar
		add sp, 6

		pop cx
	inc cx
	jmp string_loop
	string_end:
	pop bp
	ret
endp printColoredString

proc printToggles
	cmp [compl], 0
	je compl_on

	push OFF_COLOR
	jmp compl_end
	compl_on:
	push ON_COLOR
	compl_end:

	lea dx, [textfilename]
	add dx, 2h
	; push dx
	push offset complement_toggle
	push 1802h
	call printColoredString
	add sp, 6

	push INDEX_COLOR
	push offset buffer_toggle
	push 001Dh
	call printColoredString
	add sp, 6

	push INDEX_COLOR
	mov dl, [file_offset]
	push dx
	push 0024h
	call printByte
	add sp, 6

	ret
endp printToggles

proc gettextfilename
	mov dx, offset textfilename
	mov bx, dx
	mov [byte ptr bx], 50
	mov ah, 0Ah
	int 21h

	mov bx, offset textfilename
	inc bx
	add bl, [byte ptr bx]
	inc bx
	mov [byte ptr bx], 0h
	ret
endp gettextfilename

start:
	; for string printing
	mov ax, @data
	mov ds, ax


	; jmp game_over

	call gettextfilename
	; set graphic mode
	mov ax, 13h
	int 10h

	call printLayout
	game_loop:
		call printToggles
		call openTextFile

		cmp [textfilehandle], 00h
		je game_over

		call seekFile
		call readFile
		call closeFile
		call printBuffer

		; get key
		mov ax, 0h
		int 16h

		; scroll down only if it wouldn't leave empty spaces above
		cmp al, 'k'
		jne check_k
		cmp [file_offset], 0h
		je game_loop
		sub [file_offset], 9h
		jmp game_loop

		; scroll up only if you aren't going in too deep
		check_k:
		cmp al, 'j'
		jne check_c
		cmp [bytes_read], 0
		je game_loop
		add [file_offset], 9h
		jmp game_loop

		; complement all bytes
		check_c:
		cmp al, 'c'
		jne game_over
		not [compl]
		jmp game_loop

	game_over:
	; return to text mode
	mov al, 2h
	mov ah, 0
	int 10h
exit:
	mov ax, 4c00h
	int 21h
END start