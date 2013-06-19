;Engine for TEX file

=> detect mouse
#available%(seg,off)
;================================================================
push bp
mov bp,sp
xor ax,ax
int 33
mov es,[bp+8]
mov bx,[bp+6]
es:mov [bx],ax
pop bp
retf 4
;================================================================

=>show mouse
#No parameters
;================================================================
mov ax,1		;call for microsoft show mouse function
int 33		;make call to microsoft mouse routine
retf		;return to absolute which in turn returns to basic
;================================================================

=>hide mouse
#no return
;================================================================
mov ax,2		;call for microsoft hide mouse function
int 33		;make call to microsoft mouse routine
retf		;return to absolute which in turn returns to basic
;================================================================

=>set mouse range
#x1,y1,x2,y2
;================================================================
push bp
mov bp,sp
mov cx,[bp+c]	;x1
mov dx,[bp+8]	;x2
mov ax,7		;call for x boundaries setting function
int 33		;call to microsoft mouse function
mov cx,[bp+a]	;y1
mov dx,[bp+6]	;y2
mov ax,8		;call for y boundaries setting function
int 33		;call to microsoft mouse function
pop bp
retf 8		;return to absolute which in turn returns to basic
;================================================================

=>put mouse
#x,y
;================================================================
push bp
mov bp,sp
mov cx,[bp+8]	;read x pos at[0]
mov dx,[bp+6]	;read y pos at[2]
mov ax,4		;call for mouse put function
int 33		;call to microsoft mouse function
pop bp
retf 4		;return to absolute which in turn returns to basic
;================================================================

=>mouse status
#Array%(6)[xpos,ypos,leftbutton,rightbutton,leftclick,rightclick]
;================================================================
push bp
mov bp,sp
push si
mov es,[bp+8]
mov si,[bp+6]
mov ax,3
int 33
es:mov [si],cx
es:mov [si+2],dx
mov cx,bx
and cx,1
mov dx,cx
es:xor cx,[si+4]
es:and cx,[si+4]
es:mov [si+8],cx
es:mov [si+4],dx
mov cx,bx
shr cx,1
mov dx,cx
es:xor cx,[si+6]
es:and cx,[si+6]
es:mov [si+a],cx
es:mov [si+6],dx
pop si
pop bp
retf 4
;================================================================
=>Write a string with attribute
#x,y,String(seg,off)
;================================================================
push bp
mov bp,sp
push ds
push si
push di
pushf
mov ds,[bp+8]
mov si,[bp+6]
mov ax,b800
mov es,ax
mov ax,a0
mov bx,[bp+a]
mul bx
add ax,[bp+c]
add ax,[bp+c]
mov di,ax

mov ah,[si]
inc si
cld
::write2
lodsb
cmp al,0
je ::write1
stosw
jmp ::write2
::write1
popf
pop di
pop si
pop ds
pop bp
retf 8
;================================================================
=>Get an area of text screen
#(x1,y1,x2,y2,Free(seg,off))
;================================================================
push bp
mov bp,sp
push ds
push si
push di
pushf
mov es,[bp+8]		;es:di=Free location
mov di,[bp+6]		;ds:si(dx)=Screen location
mov ax,b800
mov ds,ax
mov ax,a0
mov bx,[bp+e]
mul bx
add ax,[bp+10]
add ax,[bp+10]
mov si,ax
mov dx,ax

mov ax,[bp+c]		;ax=xcount
sub ax,[bp+10]
inc ax

mov bx,[bp+a]		;bx=ycount
sub bx,[bp+e]
inc bx

es:mov [di],ax		;store size
es:mov [di+2],bx
add di,4

cld
::getarea1
mov cx,ax
rep movsw
add dx,a0
mov si,dx
dec bx
jnz ::getarea1

popf
pop di
pop si
pop ds
pop bp
retf c
;================================================================
=>Put an area to text screen
#(x,y,Free(seg,off))
;================================================================
push bp
mov bp,sp
push ds
push si
push di
pushf

mov ds,[bp+8]		;ds:si=Free location
mov si,[bp+6]		;es:di(dx)=Screen location
mov ax,b800
mov es,ax
mov ax,a0
mov bx,[bp+a]
mul bx
add ax,[bp+c]
add ax,[bp+c]
mov di,ax
mov dx,ax

mov ax,[si]		;get xcount,ycount
mov bx,[si+2]
add si,4

cld
::putarea1
mov cx,ax
rep movsw
add dx,a0
mov di,dx
dec bx
jnz ::putarea1

popf
pop di
pop si
pop ds
pop bp
retf 8
;================================================================
=>String count
#Ans(seg,off),Data$(seg,off),find,len
;================================================================
push bp
mov bp,sp
push di
pushf
mov es,[bp+c]
mov di,[bp+a]
mov al,[bp+8]
mov cx,[bp+6]
xor dx,dx

cld
::count2
scasb
jne ::count1
inc dx
::count1
loop ::count2
mov es,[bp+10]
mov si,[bp+e]
es:mov [si],dx

popf
pop di
pop bp
retf c
;================================================================
=>Draw box
#x1,y1,x2,y2,char,attr
;================================================================
push bp
mov bp,sp
push di
push si
pushf
mov ax,b800
mov es,ax
mov ax,a0
mov bx,[bp+e]
mul bx
add ax,[bp+10]
add ax,[bp+10]
mov si,ax
mov bx,[bp+c]
sub bx,[bp+10]
inc bx
mov dx,[bp+a]
sub dx,[bp+e]
inc dx
mov al,[bp+8]
mov ah,[bp+6]

cld
::box1
mov di,si
mov cx,bx
rep stosw
add si,a0
dec dx
jnz ::box1

popf
pop si
pop di
pop bp
retf c
;================================================================

=>Show cursor
;================================================================
mov cx,607
mov ah,1
int 10
retf
;================================================================

=>Hide cursor
;================================================================
mov ch,32
mov ah,1
int 10
retf
;================================================================

=>Show box cursor
;================================================================
mov cx,7
mov ah,1
int 10
retf
;================================================================

=>Set cursor position
#column,row,page
;================================================================
push bp
mov bp,sp
mov dl,[bp+a]
mov dh,[bp+8]
mov bh,[bp+6]
int 10
pop bp
retf 4
;================================================================


;happy ending ^_^.
