.286
IDEAL
MODEL small
STACK 100h
DATASEG

; boolean value of byte invertion
invert		   db 1 dup (0)
; buffer of file
buffer		   db 132 dup (?)
; nymber of bytes read into buffer
bytes_read	   db 1 dup (0)
hex_digits	   db '0123456789ABCDEF$'
; maximun file length: 38 bytes
textfilename   db 40 dup (1)
invert_toggle  db 'inverted', 0
offset_label  db 'offset:', 0
; file handle for the text file
textfilehandle dw 1 dup (0)
file_offset	   db 1 dup (0)
; constants for byte and char printing
BYTES_PER_ROW  equ 09h
BYTES_PER_COL  equ 0Ch
LAYOUT_COLOR    equ 0Bh
VALUE_COLOR    equ 0Fh
CHAR_COLOR	   equ 0Fh
OFF_COLOR	   equ 1Ah
ON_COLOR	   equ 0Ah
HIDE_COLOR	   equ 00h

TRUE		   equ 01h
FALSE		   equ 00h

; BMP vars
filename	  db 'splash2.bmp',0
filehandle 	  dw ?
Header 		  db 54 dup (0)
Palette 	  db 256*4 dup (0)
ScrLine 	  db 320 dup (0)
ErrorMsg 	  db 'Error', 13, 10,'$'

; --------------------------
x_coordinate  dw ? ; place in line
y_coordinate  dw ? ; place in column
color  		  dw 2, 3, 4, 14, 15

x_begin  	  dw 50 ; Starting point on line
y_begin  	  dw 60 ; Starting point on column
x_count  	  db 50 ; loop count line draw
y_count  	  db 80 ; loop count column draw
; --------------------------
CODESEG

; -------------------------------------------------------------------------------------------------------------
; Read a BMP file 320x200 and print it to screen
; Author: Barak Gonen, 2014
; Credit: Diego Escala, www.ece.msstate.edu/~reese/EE3724/labs/lab9/bitmap.asm
; -------------------------------------------------------------------------------------------------------------
proc OpenFile
	; Open file
	mov ah, 3Dh
	xor al, al
	mov dx, offset filename
	int 21h
	jc openerror
	mov [filehandle], ax
	ret
	openerror:
	mov dx, offset ErrorMsg
	mov ah, 9h
	int 21h
	ret
endp OpenFile

proc ReadHeader
	; Read BMP file header, 54 bytes
	mov ah,3fh
	mov bx, [filehandle]
	mov cx,54
	mov dx,offset Header
	int 21h
	ret
endp ReadHeader

proc ReadPalette
	; Read BMP file color palette, 256 colors * 4 bytes (400h)
	mov ah,3fh
	mov cx,400h
	mov dx,offset Palette
	int 21h
	ret
endp ReadPalette

proc CopyPal
	; Copy the colors palette to the video memory
	; The number of the first color should be sent to port 3C8h
	; The palette is sent to port 3C9h
	mov si,offset Palette
	mov cx,256
	mov dx,3C8h
	mov al,0
	; Copy starting color to port 3C8h
	out dx,al
	; Copy palette itself to port 3C9h
	inc dx
	PalLoop:
	; Note: Colors in a BMP file are saved as BGR values rather than RGB.
	mov al,[si+2] ; Get red value.
	shr al,2 ; Max. is 255, but video palette maximal
	; value is 63. Therefore dividing by 4.
	out dx,al ; Send it.
	mov al,[si+1] ; Get green value.
	shr al,2
	out dx,al ; Send it.
	mov al,[si] ; Get blue value.
	shr al,2
	out dx,al ; Send it.
	add si,4 ; Point to next color.
	; (There is a null chr. after every color.)
	loop PalLoop
	ret
endp CopyPal

proc CopyBitmap
	; BMP graphics are saved upside-down.
	; Read the graphic line by line (200 lines in VGA format),
	; displaying the lines from bottom to top.
	mov ax, 0A000h
	mov es, ax
	mov cx,200
	PrintBMPLoop:
	push cx
	; di = cx*320, point to the correct screen line
	mov di,cx
	shl cx,6
	shl di,8
	add di,cx
	; Read one line
	mov ah,3fh
	mov cx,320
	mov dx,offset ScrLine
	int 21h
	; Copy one line into video memory
	cld ; Clear direction flag, for movsb
	mov cx,320
	mov si,offset ScrLine

	rep movsb ; Copy line to the screen
	;rep movsb is same as the following code:
	;mov es:di, ds:si
	;inc si
	;inc di
	;dec cx
	;loop until cx=0
	pop cx
	loop PrintBMPLoop
	ret
endp CopyBitmap

; Draw Pixel
proc drawPixel
	pusha
	xor bh, bh
	mov cx, [x_coordinate]
	mov dx, [y_coordinate]
	mov ax, [color+si]
	mov ah, 0ch ; write pixel at coordinates
	int 10h
	popa
	ret
endp drawPixel

; Draw Rectangle at x_begin, y_begin position, size 25*35
Proc DrawRectangle
	pusha
	mov [x_count],50   ; Rectangle width
	mov [y_count],80   ; Rectangle height
	mov ax,[x_begin]   ; Save begin point on X
	mov [x_coordinate], ax
	;mov ax, [y_begin+si] ; Save begin point on Y <------- corrected
	mov ax, [y_begin] ; Save begin point on Y
	mov [y_coordinate], ax
	; line of dots on screen
	line_loop:
	call drawPixel
	inc [x_coordinate] ; Position the next pixel one step to the left
	dec [x_count]
	cmp [x_count], 0   ; Check if the end of the line loop
	jnz line_loop

	mov ax, [x_begin] ; Reset line counters
	mov [x_coordinate], ax
	mov [x_count], 50 ; Reset column counters
	inc [y_coordinate] ; Position the next pixel one step down
	dec [y_count]
	cmp [y_count], 0  ; Check if the end of the column loop
	jnz line_loop

	popa
	ret
endp DrawRectangle
; END DRAW


; all functions answer to pseudo-stdcall conventions: paramters are sent in first-to-last,
; state keeping left to callee and stack reset left to caller

; takes in (coordinates in the same word, char, color) and prints the character accordingly
proc printChar
	push bp
	mov bp, sp

	mov dx, [bp + 4] ; set the col and row
	mov  bh, 0       ; Display page
	mov  ah, 02h     ; SetCursorPosition
	int  10h

	mov  ax, [bp + 6] ; char
	mov  bx, [bp + 8] ; color
	mov  bh, 0        ; Display page
	mov  ah, 0Eh      ; Teletype (prints char)
	int  10h

	pop bp
	ret
endp printChar

; takes in (coordinates, byte, color) and prints text hex-encoding of byte
proc printByte
	push bp
	mov bp, sp

	; print lower nibble
	mov dx, [bp + 6] ; byte
	and dl, 0Fh
	mov bx, offset hex_digits
	add bl, dl
	mov dl, [bx]  ; dl holds ascii code of hex value of lower nibble

	push [bp + 8] ; color parm for printChar
	push dx
	inc [bp + 4]  ; move aside one col (right nible printed on the right)
	push [bp + 4] ; right col of byte
	call printChar
	add sp, 6

	; print higher nibble
	mov dx, [bp + 6]
	shr dl, 4
	and dl, 0Fh
	mov bx, offset hex_digits
	add bl, dl
	mov dl, [bx] ; higher nible

	push [bp + 8] ; color parm for printChar
	push dx
	dec [bp + 4]  ; move aside one col
	push [bp + 4] ; left col of byte for MSB nibble
	call printChar
	add sp, 6

	pop bp
	ret
endp printByte

; print the table indices
proc printLayout
	xor cx, cx
	print_col_index:
		; print byte cx at (cx + 1) * 3 - 1
		; print space at (cx + 1) * 3 + 2
		push LAYOUT_COLOR
		push cx ; index byte to print

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
	; printing for loop, jump if we printed the desired number of rows
	inc cl
	cmp cl, BYTES_PER_ROW
	jl print_col_index

	; same thing just for row indices
	xor cx, cx
	print_row_index:
		push LAYOUT_COLOR
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

; get text filehandle from filename
proc openTextFile
	mov ah, 3Dh
	xor al, al
	lea dx, [textfilename]
	add dx, 2h

	int 21h
	jc opentexterror
	mov [textfilehandle], ax
	opentexterror:
	ret
endp openTextFile

; invert bytes in buffer if inverting is on
proc invertBuffer
	xor cl, cl
	mov bx, offset buffer

	; loop over buffer and negate each byte
	invert_loop:
		neg [byte ptr bx]
		inc bx
	inc cl
	cmp cl, [bytes_read]
	jb invert_loop

	ret
endp invertBuffer

; seek to desired offset in text file
proc seekFile
	mov ax, 4200h
	mov bx, textfilehandle
	xor cx, cx
	mov dl, [file_offset]
	xor dh, dh
	int 21h

	ret
endp seekFile

; read filehandle into buffer, with inversion
proc readFile
	mov ah, 3Fh
	mov bx, [textfilehandle]
	mov cx, 132
	mov dx, offset buffer
	int 21h ; copy bytes into buffer
	mov [bytes_read], al ; get number of bytes read

	; invert if need be (if invert variable is not 0)
	mov dl, 0
	not dl
	cmp [invert], dl
	jne no_inversion

	call invertBuffer

	no_inversion:

	ret
endp readFile

; closes file denoted by the filehandle
proc closeFile
	mov ah, 3Eh
	mov bx, [textfilehandle]
	int 21h
	ret
endp closeFile

; prints byte in byte form on the left side of the screen based on parameters (show_byte - Y/N, byte, index)
proc printBufferByte
	push bp
	mov bp, sp

	; if show != TRUE, hide it with black color, otherwise se a visible color
	; we need to print every spot again every time so that scrolling doesn't dupe infinitely
	cmp [bp + 8], TRUE
	jne hide_byte

	push VALUE_COLOR
	jmp end_color

	hide_byte:
	push HIDE_COLOR

	end_color:

	; cx holds index
	mov cx, [bp + 4]

	; byte from buffer at ch * BYTES_PER_ROW + BYTER_PER_COL passed by caller
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

	; tuple of coors on screen
	push dx

	call printByte
	add sp, 6

	pop bp
	ret
endp printBufferByte

; prints byte in character form on the right side of the screen based on parameters (show_byte - Y/N, byte, index)
proc printBufferChar
	push bp
	mov bp, sp
	; same functionality/logic as the previous function
	cmp [bp + 8], TRUE
	jne hide_char

	push CHAR_COLOR
	jmp end_color_char

	hide_char:
	push HIDE_COLOR

	end_color_char:

	mov ax, [bp + 6]
	; check if this is an ascii character, if yes, print it, otherwise, print a dot
	; dx <= 7a && dx >= 61 || dx <= 41 && dx >= 5a

	cmp al, 41h       ; compare al with "A"
	jl non_ascii    ; jump to next character if less
	cmp al, 5Ah       ; compare al with "Z"
	jle ascii    ; if al is >= "A" && <= "Z" -> found a letter
	cmp al, 61h       ; compare al with "a"
	jl non_ascii    ; jump to next character if less (since it's between "Z" & "a")
	cmp al, 7Ah       ; compare al with "z"
	jg non_ascii    ; above "Z" -> not a character
	jmp ascii

	non_ascii:
	mov al, '.'

	ascii:
	push ax

	; initial position for char coordinates
	mov dx, 011Dh

	; calculate position on screen
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

; this is just a wrapper functino that recieves (show_byte, byte, index) and calls the actuall printing functions
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

; prints the entire buffer out to the screen, both char and hex form
proc printBuffer
	; al is the number of bytes read currently, we keep it's state on the stack (popping and repushing when done using)
	xor ax, ax
	push ax

	; ch holds current row
	xor ch, ch
	row_data_loop:
		; cl holds current col
		xor cl, cl
		col_data_loop:
			; check if these are hidden bytes (if we should paint them black) by checking if the bytes read index is larger than
			; the number of available bytes
			pop ax
			cmp al, [bytes_read]
			push ax
			jb shown

			; if is hidden
			push FALSE ; show_byte = hide
			push 0h ; generic byte to print, doesn't matter since it's hidden
			jmp print_end

			shown:
			push TRUE ; show

			; get current byte from buffer
			mov bx, offset buffer
			add bl, al
			push [bx]

			print_end:

			push cx ; current index in form (row, col)
			call printBufferItem
			add sp, 6

		; inc buffer index and col index
		pop ax
		inc ax
		push ax

		inc cl
		; stop inner loop if we've finished this row
		cmp cl, BYTES_PER_ROW
		jl col_data_loop

	inc ch
	; stop if we've reached the last row
	cmp ch, BYTES_PER_COL
	jl row_data_loop

	pop ax ; leave the stack at the same state it was we got it in

	ret
endp printBuffer
; prints colored string based on (color, string, screen coordinates)
proc printColoredString
	push bp
	mov bp, sp

	; cx holds the current string index
	xor cx, cx
	string_loop:
	 	; bx holds by the end the memory location of the current byte being printed
		mov bx, [bp + 6]
		add bx, cx

		; if we reached the null byte, end the print loop
		cmp [byte ptr bx], 0
		je string_end

		push cx ; save actual string index to state
		add cx, [bp + 4]

		push [bp + 8] ; color
		push [bx]
		; cx is now the exact position of the current char to be printed (initial coors + current char offset)
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

; prints inversion toggle and offset label
proc printToggles
	; jump to inversion if requested
	cmp [invert], 0
	jne inversion_on

	; color argument for colored string printing later on, grey if no inversion was requested
	push OFF_COLOR
	jmp inversion_end

	inversion_on:
	; green because is inverted
	push ON_COLOR

	inversion_end:

	; together with the color from before, string pointer and coordinates on screen
	; are pushed as arguments to the colored string printing function
	push offset invert_toggle
	push 1802h
	call printColoredString
	add sp, 6

	; prints "offset:" in the correct location for the offset label (later joined by the offset count)
	push LAYOUT_COLOR
	push offset offset_label
	push 001Dh

	call printColoredString
	add sp, 6

	; print the offset next to its label
	push LAYOUT_COLOR
	mov dl, [file_offset]
	push dx
	push 0024h

	call printByte
	add sp, 6

	ret
endp printToggles

; calls input interruts and places name of file from user in textfilename
; (actually textfilename + 2 because of the return format of the interrupt
proc gettextfilename
	; get text from user
	mov dx, offset textfilename
	mov bx, dx
	mov [byte ptr bx], 50
	mov ah, 0Ah
	int 21h

	; nullify final byte to create a C-style string
	mov bx, offset textfilename
	inc bx
	add bl, [byte ptr bx]
	inc bx
	mov [byte ptr bx], 0h

	ret
endp gettextfilename

; function that controls the entire control flow of the application after file has been recieved
proc gameLoop
	mov ax, 13h
	int 10h

	; print the initial layout (which stays constant)
	call printLayout

	life_cycle:
	 	; toggles include offset and inversion indicators
		call printToggles

		call openTextFile

		; check we got a valid filehandle (non-zero), if it is, stop thr run
		cmp [textfilehandle], 00h
		je end_life_cycle

		; skip to the desired offset, read into buffer, close the file, and print to the user
		call seekFile
		call readFile
		call closeFile
		call printBuffer

		; get key
		mov ax, 0h
		int 16h

		; k for scroll up (vim style)
		cmp al, 'k'
		jne check_scroll_up

		; scroll up only if it doesn't reveal empty spaces above (aka offset would be negative)
		cmp [file_offset], 0h
		je life_cycle

		; scroll up = de-offset by 9 bytes (size of row in the GUI)
		sub [file_offset], 9h
		jmp life_cycle

		check_scroll_up:
		cmp al, 'j'
		jne check_inversion

		; scroll down only if you aren't going in too deep
		; (aka have hidden empty rows above you, reach only a blank screen and don't allow blank scrolling)
		cmp [bytes_read], 0
		je life_cycle

		; offset is to offset by another row
		add [file_offset], 9h
		jmp life_cycle

		check_inversion:
		cmp al, 'i'
		jne end_life_cycle

		; set inversion flag for next print cycle
		not [invert]

		; look back around the "infinite" life cycle loop
		jmp life_cycle

	end_life_cycle:

	; return to text mode
	mov al, 2h
	mov ah, 0
	int 10h

	ret
endp gameLoop

; function that controls initial application jobs: show image and recieve user input for file name
proc splashScreen
	; set graphic mode
	mov ax, 13h
	int 10h

	; Process BMP file
	call OpenFile
	call ReadHeader
	call ReadPalette
	call CopyPal
	call CopyBitmap

	; set cursor position for input box in splash screen
	mov dx, 0FA6h ; set the col and row
	mov bh, 0     ; Display page
	mov ah, 02h   ; SetCursorPosition
	int 10h

	call gettextfilename

	mov ax, 0
	int 10h

	ret
endp splashScreen
start:
	mov ax, @data
	mov ds, ax

	call splashScreen

	; we do text mode and then graphics mode again to clear the screen

	call gameLoop

exit:
	mov ax, 4c00h
	int 21h
END start