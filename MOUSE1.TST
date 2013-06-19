'successful test
DECLARE SUB mouse.loadprog ()
DECLARE FUNCTION mouse.init% ()
DECLARE SUB mouse.show ()
DECLARE SUB mouse.hide ()
DECLARE SUB mouse.setrange (x1%, y1%, x2%, y2%)
DECLARE SUB mouse.put (x%, y%)
DECLARE SUB mouse.status ()


TYPE mouse
        left AS INTEGER
        right AS INTEGER
        xpos AS LONG
        ypos AS LONG
END TYPE
DIM Jerry AS mouse
mouse$ = ""

'main
'set screen mode
SCREEN 12
'load ASSEMBLY PROGRAM
mouse.loadprog
'find if mouse is present
a1% = mouse.init%
IF (a1% <> 1) THEN
PRINT "Mouse not installed."
SYSTEM
END IF
'show mouse
mouse.show
'set mouse range(defect)
'mouse.setrange 0, 0, 319, 199
'put mouse
mouse.put 0, 0


'loop
clr% = 0
c% = 0
'DO
'mouse.status
'PRINT Jerry.left; Jerry.right; Jerry.xpos; Jerry.ypos
'LOOP





DO
mouse.status
IF (Jerry.left = 1) THEN
LINE (0, 0)-(Jerry.xpos, Jerry.ypos), clr%
clr% = (clr% + 1) MOD 15
END IF
IF (Jerry.right = 1) THEN
LINE (640, 0)-(Jerry.xpos, Jerry.ypos), clr%
clr% = (clr% + 1) MOD 15
END IF
PALETTE 15, c% * 65536 + c% * 256 + c%
c% = (c% + 1) MOD 64
LOOP

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

SUB mouse.put (x%, y%)
SHARED mouse$
DEF SEG = &H101
POKE 0, x% MOD 256
POKE 1, x% \ 256
POKE 2, y% MOD 256
POKE 3, y% \ 256
DEF SEG = VARSEG(mouse$)
mem1& = SADD(mouse$) + 66
CALL absolute(mem1&)
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
Jerry.xpos = a2& * 256 + a1&
a1& = PEEK(4)
a2& = PEEK(5)
Jerry.ypos = a2& * 256 + a1&
DEF SEG
END SUB

