IDEAL
MODEL SMALL
STACK 100h
p186
jumps

DATASEG
; --------------------------
; Your variables here
; --------------------------

filename db 'splash2.bmp',0
filehandle dw ?
Header db 54 dup (0)
Palette db 256*4 dup (0)
ScrLine db 320 dup (0)
ErrorMsg db 'Error', 13, 10,'$'

; --------------------------
x_coordinate  dw ? ; place in line
y_coordinate dw ? ; place in column
color  dw 2, 3, 4, 14, 15

x_begin  dw 50 ; Starting point on line
y_begin  dw 60 ; Starting point on column
x_count  db 50 ; loop count line draw
y_count  db 80 ; loop count column draw
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

start:
mov ax, @data
mov ds, ax
; --------------------------
; Your code here
; --------------------------

; Graphic mode
mov ax, 13h
int 10h
; Process BMP file
call OpenFile
call ReadHeader
call ReadPalette
call CopyPal
call CopyBitmap

;wait for key
mov ah, 0h
int 16h

mov ax, 13h
int 10h

mov si,0 ; setup rectangle color to green
call DrawRectangle

mov si,4 ; setup rectangle color to red
mov [x_begin], 220 ; starting x of second card
mov [y_begin], 60 ; starting y of second card
call DrawRectangle

;wait for key
mov ah, 0h
int 16h

exit:
mov ax, 03h
int 10h
mov ax, 4c00h
int 21h
END start