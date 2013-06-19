'Draw a Screen 0 Image = "aaaa.dat" bsave 4000 bytes
DECLARE SUB mouse.loadprog ()
DECLARE FUNCTION mouse.init% ()
DECLARE SUB mouse.show ()
DECLARE SUB mouse.start2 ()
DECLARE SUB mouse.show2 ()
DECLARE SUB mouse.end2 ()
DECLARE SUB mouse.start3 ()
DECLARE SUB mouse.show3 ()
DECLARE SUB mouse.writeat2 (char%, attr%)
DECLARE SUB mouse.writeat3 (char%, attr%)
DECLARE SUB mouse.hide ()
DECLARE SUB mouse.setrange (x1%, y1%, x2%, y2%)
DECLARE SUB mouse.put (x&, y&)
DECLARE SUB mouse.status ()
DECLARE SUB mouse.relativestatus ()

DECLARE FUNCTION dat.datum% (fl1$, pos1&)
DECLARE SUB dat.loaddata (fl1$, pos1&, pos2&, segment&)

ON ERROR GOTO errors
TYPE mouse
        left AS INTEGER
        right AS INTEGER
        oldleft AS INTEGER
        oldright AS INTEGER
        xpos AS LONG
        ypos AS LONG
        oldxpos AS LONG
        oldypos AS LONG
        mousetype AS INTEGER
        mouseattrib AS INTEGER
        oldmousetype AS INTEGER
        oldmouseattrib AS INTEGER
        virtualattrib AS INTEGER
END TYPE
DIM Jerry AS mouse
mouse$ = ""
Jerry.mousetype = 1
Jerry.mouseattrib = 1
Jerry.virtualattrib = 0

TYPE filestring
        byte AS STRING * 1
END TYPE
DIM file AS filestring


'Start mouse
SCREEN 0
mouse.loadprog
a1% = mouse.init%
IF (a1% <> 1) THEN
PRINT "Mouse not installed."
SYSTEM
END IF
mouse.hide
mouse.put 0, 0

'BLOAD "aaaa.dat", &HB800
DEF SEG = &HB800
OPEN "B", #1, "aaaa.dat"
SEEK #1, 1
FOR i& = 0 TO LOF(1) - 1
k$ = INPUT$(1, #1)
POKE i&, ASC(k$)
NEXT
CLOSE #1
DEF SEG

mouse.start2
attr1% = 1
attr2% = 0
attr3% = 0
char% = 1
DO
        k$ = ""
        WHILE k$ = ""
        mouse.show2
        k$ = INKEY$
        IF (k$ = CHR$(27)) THEN EXIT DO
        attr% = attr2% * 128 + attr3% * 16 + attr1%
        IF (Jerry.left = 1) THEN mouse.writeat2 Jerry.mousetype, attr%
        IF (Jerry.right = 1) THEN mouse.writeat2 0, 0
        IF (Jerry.left = 1 AND Jerry.right = 1) THEN CLS
        WEND
       
SELECT CASE k$
CASE CHR$(26)
attr1% = attr1% + 1
IF attr1% > 15 THEN attr1% = 0
attr% = attr2% * 128 + attr3% * 16 + attr1%
Jerry.mouseattrib = attr%
CASE CHR$(24)
attr1% = attr1% - 1
IF (attr1% < 0) THEN attr1% = 15
attr% = attr2% * 128 + attr3% * 16 + attr1%
Jerry.mouseattrib = attr%
CASE CHR$(3)
attr2% = attr2% + 1
IF (attr2% > 1) THEN attr2% = 0
attr% = attr2% * 128 + attr3% * 16 + attr1%
Jerry.mouseattrib = attr%
CASE CHR$(22)
attr3% = attr3% + 1
IF (attr3% > 7) THEN attr3% = 0
attr% = attr3% * 128 + attr3% * 16 + attr1%
Jerry.mouseattrib = attr%
CASE CHR$(2)
attr3% = attr3% - 1
IF (sttr3% < 0) THEN attr3% = 7
attr% = attr2% * 128 + attr3% * 16 + attr1%
Jerry.mouseattrib = attr%
CASE CHR$(17)
char% = char% + 1
IF char% > 255 THEN char% = 0
Jerry.mousetype = char%
CASE CHR$(23)
char% = char% - 1
IF char% < 0 THEN char% = 255
Jerry.mousetype = char%
CASE CHR$(5)
DEF SEG = &HB800
Jerry.mousetype = PEEK(5000)
DEF SEG
CASE CHR$(18)
DEF SEG = &HB800
FOR lv1& = 0 TO 159
POKE 4000 + lv1&, PEEK(lv1&)
NEXT
DEF SEG
LOCATE 1, 1
PRINT "Location:"; Jerry.xpos, Jerry.ypos
k$ = INPUT$(1)
DEF SEG = &HB800
FOR lv1& = 0 TO 159
POKE lv1&, PEEK(4000 + lv1&)
NEXT
DEF SEG
CASE ELSE
attr% = attr2% * 128 + attr3% * 16 + attr1%
mouse.writeat2 ASC(k$), attr%
Jerry.xpos = Jerry.xpos + 1
IF (Jerry.xpos > 79) THEN Jerry.xpos = 79
mouse.put Jerry.xpos, Jerry.ypos
END SELECT
LOOP

mouse.end2

'Save
OPEN "O", #1, "aaaa.dat"
CLOSE #1
DEF SEG = &HB800
OPEN "B", #1, "aaaa.dat"
FOR i& = 0 TO 3999
k$ = CHR$(PEEK(i&))
PUT #1, i& + 1, k$
NEXT
CLOSE #1
DEF SEG

errors: RESUME NEXT

FUNCTION dat.datum% (fl1$, pos1&)
SHARED file AS filestring
fr% = FREEFILE
OPEN "B", #fr%, fl1$
SEEK #fr%, pos1&
file.byte = INPUT$(1, #fr%)
CLOSE #fr%
dat.datum% = ASC(file.byte)
END FUNCTION

SUB dat.loaddata (fl1$, pos1&, pos2&, segment&)
SHARED file AS filestring
DEF SEG = segment&
fr% = FREEFILE
OPEN "B", #fr%, fl1$
IF (pos2& > LOF(fr%)) THEN pos2& = LOF(fr%)
posp& = pos1&
sz& = pos2& - pos1& + 3
POKE 0, (sz& AND &HFF00) \ &H100
POKE 1, sz& MOD 256
mem1& = 2
DO UNTIL posp& > pos2&
SEEK #fr%, posp&
posp& = posp& + 1
file.byte = INPUT$(1, #fr%)
POKE mem1&, ASC(file.byte)
mem1& = mem1& + 1
LOOP
CLOSE #fr%
DEF SEG
END SUB

SUB mouse.end2
SHARED Jerry AS mouse
mem1& = (Jerry.xpos + Jerry.ypos * 80) * 2
DEF SEG = &HB800
POKE mem1&, PEEK(5000)
POKE mem1& + 1, PEEK(5001)
DEF SEG
END SUB

SUB mouse.hide
SHARED mouse$
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 22
CALL absolute(mem1&)
DEF SEG
END SUB

FUNCTION mouse.init%
SHARED mouse$
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$)
CALL absolute(mem1&)
DEF SEG = &H100
IF (PEEK(0) = 255 AND PEEK(1) = 255) THEN a1% = 1
DEF SEG
mouse.init% = a1%
END FUNCTION

SUB mouse.loadprog
SHARED mouse$
CLS                          'Load ASM Program to mouse$
OPEN "B", #1, "mouse.dll"
FOR i = 1 TO LOF(1)
SEEK #1, i
k$ = INPUT$(1, #1)
mouse$ = mouse$ + k$
NEXT
CLOSE #1
END SUB

SUB mouse.put (x&, y&)
SHARED mouse$
x& = x& * 8
y& = y& * 8
DEF SEG = &H101
POKE 0, x& MOD 256
POKE 1, x& \ 256
POKE 2, y& MOD 256
POKE 3, y& \ 256
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 66
CALL absolute(mem1&)
DEF SEG
END SUB

SUB mouse.relativestatus
SHARED Jerry AS mouse
SHARED mouse$
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 117
CALL absolute(mem1&)
DEF SEG = &H100
a1% = PEEK(0)
Jerry.left = a1% AND 1
Jerry.right = (a1% AND 2) \ 2
a1& = PEEK(2)
a2& = PEEK(3)
Jerry.xpos = a2& * 256 + a1&
IF (Jerry.xpos AND &H8000 = &H8000) THEN Jerry.xpos = -1 * (NOT (Jerry.xpos) + 1)
Jerry.xpos = Jerry.xpos \ 2
a1& = PEEK(4)
a2& = PEEK(5)
Jerry.ypos = a2& * 256 + a1&
IF (Jerry.ypos AND &H8000 = &H8000) THEN Jerry.ypos = -1 * (NOT (Jerry.ypos) + 1)
DEF SEG
END SUB

SUB mouse.setrange (x1%, y1%, x2%, y2%)
SHARED mouse$
DEF SEG = &H101
POKE 1, 0'x1% MOD 256
POKE 0, 0' x1% \ 256
POKE 3, 200'x2% MOD 256
POKE 2, 0'x2% \ 256
POKE 5, 0'y1% MOD 256
POKE 4, 0'y1% \ 256
POKE 7, 100'y2% MOD 256
POKE 6, 0'y2% \ 256
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 28
CALL absolute(mem1&)
DEF SEG
END SUB

SUB mouse.show
SHARED mouse$
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 16
CALL absolute(mem1&)
DEF SEG
END SUB

SUB mouse.show2
SHARED Jerry AS mouse, k$
mouse.status
IF (Jerry.virtualattrib > 0) THEN
IF (Jerry.oldleft <> Jerry.left OR Jerry.oldright <> Jerry.right) THEN
at% = Jerry.virtualattrib
wr% = at% AND &HF
IF (Jerry.left = 1) THEN wr% = (wr% * 2) AND &HF
wr1% = at% AND &HF0
IF (Jerry.right = 1) THEN wr1% = (wr1% * 2) AND &HF0
wr% = wr1% + wr%
IF (Jerry.oldxpos = Jerry.xpos AND Jerry.oldypos = Jerry.ypos) THEN
DEF SEG = &HB800
mem1& = (Jerry.ypos * 80 + Jerry.xpos) * 2 + 1
POKE mem1&, wr%
DEF SEG
END IF
Jerry.oldleft = Jerry.left
Jerry.oldright = Jerry.right
Jerry.mouseattrib = wr%
END IF
END IF
IF (Jerry.oldxpos <> Jerry.xpos OR Jerry.oldypos <> Jerry.ypos OR Jerry.oldmouseattrib <> Jerry.mouseattrib OR Jerry.oldmousetype <> Jerry.mousetype) THEN
DEF SEG = &HB800
mem1& = (Jerry.oldypos * 80 + Jerry.oldxpos) * 2
POKE mem1&, PEEK(5000)
POKE mem1& + 1, PEEK(5001)
mem1& = (Jerry.ypos * 80 + Jerry.xpos) * 2
POKE 5000, PEEK(mem1&)
POKE 5001, PEEK(mem1& + 1)
POKE mem1&, Jerry.mousetype
POKE mem1& + 1, Jerry.mouseattrib
Jerry.oldypos = Jerry.ypos
Jerry.oldxpos = Jerry.xpos
Jerry.oldmouseattrib = Jerry.mouseattrib
Jerry.oldmousetype = Jerry.mousetype
DEF SEG
END IF
END SUB

SUB mouse.show3
SHARED Jerry AS mouse
mouse.status
IF (Jerry.oldxpos <> Jerry.xpos OR Jerry.oldypos <> Jerry.ypos) THEN
DEF SEG = &HB800
mem1& = (Jerry.oldypos * 80 + Jerry.oldxpos) * 2
POKE mem1&, Jerry.mousetype
POKE mem1& + 1, NOT (Jerry.mouseattrib)
mem1& = (Jerry.ypos * 80 + Jerry.xpos) * 2
Jerry.mousetype = PEEK(mem1&)
Jerry.mouseattrib = NOT (PEEK(mem1& + 1))
POKE mem1&, Jerry.mousetype
POKE mem1& + 1, Jerry.mouseattrib
Jerry.oldypos = Jerry.ypos
Jerry.oldxpos = Jerry.xpos
DEF SEG
END IF
END SUB

SUB mouse.start2
SHARED Jerry AS mouse
mem1& = (Jerry.xpos + Jerry.ypos * 80) * 2
DEF SEG = &HB800
POKE 5000, PEEK(mem1&)
POKE 5001, PEEK(mem1& + 1)
DEF SEG
END SUB

SUB mouse.start3
SHARED Jerry AS mouse
DEF SEG = &HB800
mem1& = (Jerry.oldypos * 80 + Jerry.oldxpos) * 2
Jerry.mousetype = PEEK(mem1&)
Jerry.mouseattrib = NOT (PEEK(mem1& + 1))
DEF SEG
END SUB

SUB mouse.status
SHARED Jerry AS mouse
SHARED mouse$
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 89
CALL absolute(mem1&)
DEF SEG = &H100
a1% = PEEK(0)
Jerry.left = a1% AND 1
Jerry.right = (a1% AND 2) \ 2
a1& = PEEK(2)
a2& = PEEK(3)
Jerry.xpos = (a2& * 256 + a1&) \ 8
a1& = PEEK(4)
a2& = PEEK(5)
Jerry.ypos = (a2& * 256 + a1&) \ 8
DEF SEG
END SUB

SUB mouse.writeat2 (char%, attr%)
DEF SEG = &HB800
POKE 5000, char%
POKE 5001, attr%
DEF SEG
END SUB

SUB mouse.writeat3 (char%, attr%)
SHARED Jerry AS mouse
Jerry.mousetype = char%
Jerry.mouseattrib = NOT (attr%)
END SUB

