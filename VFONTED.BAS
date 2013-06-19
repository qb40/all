'
'                        V F o n t E d i t   3
'
'Hello people. This is a part of VFont you won't have seen before (seeing as
'it's new): the editor! This is the biggest change in VFont: custom fonts. You
'can now make them with this editor. The interface is pretty self-explanitory,
'the only thing I think needs explaining is that you control the width of the
'character in the edit box with the left and right arrow keys.
'
'Note: If you hate the flicker on this and you think I should have used paging,
'try putting paging in yourself. OK, you can't be bothered, so I'll tell you
'what happens: strange black-and-white dashed line appear on the screen. I've
'worked out the cause is the mouse. Solution? Stop the paging.
'
'Credits:
'
'Interrupt rountine - Hans Lunsing
'The Rest - Sam Thursfield (sam.thursfield@btinternet.com)

DECLARE SUB Black ()
DECLARE SUB DrawButton (X1%, Y1%, X2%, Y2%, Title$, Pressed%)
DECLARE SUB DrawFrame (X1%, Y1%, X2%, Y2%)
DECLARE SUB DrawWindow (X1%, Y1%, X2%, Y2%, TitleBar%, TitleBarCaption$)
DECLARE SUB FadeIn ()
DECLARE SUB FadeOut ()
DECLARE SUB Init ()
DECLARE SUB IntQB (IntNo AS INTEGER, Inreg AS ANY, OutReg AS ANY)
DECLARE SUB LoadASM ()
DECLARE SUB Main ()
DECLARE SUB MouseHide ()
DECLARE SUB MouseInfo (Xpos%, YPos%, But%)
DECLARE SUB MouseShow ()
DECLARE SUB NewFile ()
DECLARE SUB OpenFile ()
DECLARE SUB Quit ()
DECLARE SUB Redraw (Drawing%)
DECLARE SUB SaveFile ()
DECLARE SUB Test ()
DECLARE SUB VFont (Xpos%, YPos%, Text$, Col%, BCol%, Style%, FontArray%())
DECLARE SUB VFontBig (Xpos%, YPos%, Text$, Col%, BCol%, Style%, FontArray%())
DECLARE SUB VLoadFont (File$, Array%())
DECLARE SUB VSaveFont (File$, Array%())
DECLARE FUNCTION MouseDetected% ()
DECLARE FUNCTION MouseOver% (X1%, Y1%, X2%, Y2%)

DEFINT A-Z

CONST True = -1, False = NOT True
CONST Buttons = 1, Windows = 2, CharList = 4, CharEditBox = 8

TYPE RegType
    AX    AS INTEGER
    BX    AS INTEGER
    CX    AS INTEGER
    DX    AS INTEGER
    BP    AS INTEGER
    SI    AS INTEGER
    DI    AS INTEGER
    Flags AS INTEGER
    DS    AS INTEGER
    ES    AS INTEGER
END TYPE

DIM SHARED Regs AS RegType

DIM SHARED ErrorStatus

DIM SHARED FontBios8(255, 16)
DIM SHARED FontBios16(255, 16)
DIM SHARED EditingFont(255, 16)

DIM SHARED IntQBASM(45)

DIM SHARED CurButton
DIM SHARED CurChar

DIM SHARED Pal(0 TO 255, 1 TO 3)

Init
Main

'-----------------------------------------------------------------------
' The machine code of the interrupt procedure for IntQB
'-----------------------------------------------------------------------
IntQBASM:
' Number of bytes
DATA  190
' Hexadecimal representation of machine code
DATA 55,8B,EC,8B,5E,0C,8B,17,0A,F6
DATA 74,07,C7,07,FF,FF,E9,A7,00,8B
DATA 5E,06,8B,1F,2E,88,97,77,00,32
DATA C0,80,FA,25,74,05,80,FA,26,75
DATA 02,0C,02,50,1E,06,56,57,9C,8B
DATA 76,0A,80,FA,20,7C,05,80,FA,30
DATA 7C,0A,81,7C,08,FF,FF,74,03,8B
DATA 6C,08,8B,44,0E,25,D5,0F,50,8B
DATA 04,8B,5C,02,8B,4C,04,8B,54,06
DATA 8B,7C,0C,FF,74,0A,81,7C,12,FF
DATA FF,74,03,8E,44,12,81,7C,10,FF
DATA FF,74,03,8E,5C,10,5E,9D,CD,00
DATA 55,8B,EC,9C,83,C5,0E,F6,46,FE
DATA 02,74,02,45,45,1E,56,8E,5E,FC
DATA 8B,76,08,89,04,89,5C,02,89,4C
DATA 04,89,54,06,8F,44,0A,89,7C,0C
DATA 8F,44,10,8C,44,12,8F,44,0E,8F
DATA 44,08,F6,46,FE,02,74,02,44,44
DATA 9D,5F,5E,07,1F,58,5D,CA,08,00

FileError:
DrawWindow 150, 150, 490, 182, True, "Error"
VFont 154, 172, "File problem", 0, 0, 0, FontBios8()
SLEEP
RESUME NEXT

SUB Black
FOR I = 0 TO 255
    OUT &H3C7, I
    OUT &H3C9, 0
    OUT &H3C9, 0
    OUT &H3C9, 0
NEXT I
END SUB

SUB DrawButton (X1, Y1, X2, Y2, Title$, Pressed)
IF Pressed THEN
    LINE (X1, Y1)-(X2, Y2), 7, BF
    LINE (X1, Y1)-(X2, Y2), 8, B
    LINE (X1, Y2)-(X2, Y2), 15
    LINE (X2, Y1 + 1)-(X2, Y2), 15
    VFont X1 + 3, Y1 + 3, Title$, 15, 0, 0, FontBios8()
ELSE
    LINE (X1, Y1)-(X2, Y2), 7, BF
    LINE (X1, Y1)-(X2, Y2), 15, B
    LINE (X1, Y2)-(X2, Y2), 8
    LINE (X2, Y1 + 1)-(X2, Y2), 8
    VFont X1 + 3, Y1 + 3, Title$, 15, 0, 0, FontBios8()
END IF
END SUB

SUB DrawFrame (X1, Y1, X2, Y2)

LINE (X1, Y1)-(X2, Y2), 15, B
LINE (X2, Y1)-(X1, Y1), 8
LINE (X1, Y1)-(X1, Y2 - 1), 8

END SUB

SUB DrawWindow (X1, Y1, X2, Y2, TitleBar, TitleBarCaption$)
LINE (X1, Y1)-(X2, Y2), 7, BF
LINE (X1, Y1)-(X2, Y2), 15, B
LINE (X1 + 1, Y1 + 1)-(X2 - 1, Y2 - 1), 15, B
LINE (X1, Y2)-(X2, Y2), 8
LINE (X1 + 1, Y2 - 1)-(X2 - 1, Y2 - 1), 8
LINE (X2, Y1 + 1)-(X2, Y2), 8
LINE (X2 - 1, Y1 + 2)-(X2 - 1, Y2 - 1), 8

IF TitleBar THEN
    LINE (X1 + 4, Y1 + 4)-(X2 - 4, Y1 + 4 + 16), 4, BF
    LINE (X1 + 4, Y1 + 4)-(X2 - 4, Y1 + 4 + 16), 12, B
    LINE (X2 - 4, Y1 + 5)-(X2 - 4, Y1 + 4 + 16), 0
    LINE (X1 + 4, Y1 + 4 + 16)-(X2 - 4, Y1 + 4 + 16), 0
    VFont X1 + 7, Y1 + 5, TitleBarCaption$, 15, 0, 0, FontBios16()
END IF
END SUB

SUB FadeIn

FOR Intensity = 1 TO 64
    WAIT &H3DA, 8
    FOR Col = 0 TO 255
        OUT &H3C7, Col
        r = INP(&H3C9)
        G = INP(&H3C9)
        b = INP(&H3C9)
        IF r < Pal(Col, 1) THEN r = r + 1
        IF G < Pal(Col, 2) THEN G = G + 1
        IF b < Pal(Col, 3) THEN b = b + 1
        OUT &H3C8, Col
        OUT &H3C9, r
        OUT &H3C9, G
        OUT &H3C9, b
    NEXT
NEXT


END SUB

SUB FadeOut

FOR Intensity = 1 TO 64
    WAIT &H3DA, 8
    FOR Col = 0 TO 255
        OUT &H3C7, Col
        r = INP(&H3C9)
        G = INP(&H3C9)
        b = INP(&H3C9)
        IF r > 0 THEN r = r - 1
        IF G > 0 THEN G = G - 1
        IF b > 0 THEN b = b - 1
        OUT &H3C8, Col
        OUT &H3C9, r
        OUT &H3C9, G
        OUT &H3C9, b
    NEXT
NEXT

END SUB

SUB Init

LOCATE 1, 1
COLOR 4, 0
PRINT "АААБББВВВллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллллВВВБББААА"
COLOR 15, 4
LOCATE 1, 36
PRINT "VFontEdit 3"
PRINT

COLOR 14, 0
PRINT "ў Loading:"
COLOR 7
PRINT "    њ Loading ASM code...";
LoadASM
PRINT "done"
PRINT "    њ Loading fonts...";
VLoadFont "BIOS8.FNT", FontBios8()
VLoadFont "BIOS16.FNT", FontBios16()
VLoadFont "BIOS16.FNT", EditingFont()
PRINT "done"
PRINT "    њ Loading palette...";
FOR I = 0 TO 255
    OUT &H3C7, I
    Pal(I, 1) = INP(&H3C9)
    Pal(I, 2) = INP(&H3C9)
    Pal(I, 3) = INP(&H3C9)
NEXT I
PRINT "done"
PRINT


COLOR 14, 0
PRINT "ў Initialising:"
COLOR 7
PRINT "    њ Initialising mouse...";
IF NOT MouseDetected THEN PRINT "no mouse found!": END
PRINT "done"

FadeOut
SCREEN 9
Black
MouseShow
END SUB

SUB IntQB (IntNo AS INTEGER, Inreg AS RegType, OutReg AS RegType) STATIC
'-----------------------------------------------------------------------
' Interrupt procedure. Works in the same way as its QB 4.5 counterpart.
'-----------------------------------------------------------------------

IF NOT MachineCode% THEN
  ' First time dimension string array to hold machine code
  RESTORE IntQBASM
  READ nASMBYTES%
  REDIM ASMBuffer(0 TO nASMBYTES% - 1) AS STRING * 1
END IF

' Get address of machine code
DEF SEG = VARSEG(ASMBuffer(0))
Offset% = VARPTR(ASMBuffer(0))

IF NOT MachineCode% THEN
  ' First time load string array with machine code
  FOR I% = 0 TO nASMBYTES% - 1
    READ Code$
    POKE Offset% + I%, VAL("&H" + Code$)
  NEXT I%
  ' Indicate availability of machine code
  MachineCode% = True
END IF

' Call interrupt.
' The first Offset% parameter is used by the machine code modifying it self,
' so don't leave it out.
CALL ABSOLUTE(IntNo%, Inreg, OutReg, Offset%, Offset%)

DEF SEG
END SUB

SUB LoadASM

RESTORE IntQBASM
DEF SEG = VARSEG(IntQBASM(0))
    FOR I = 0 TO 99
        READ HexCode$
        IF HexCode$ = "#" THEN EXIT FOR
        POKE VARPTR(IntQBASM(0)) + I, VAL("&H" + HexCode$)
    NEXT I
DEF SEG

END SUB

SUB Main

SHARED X, Y, b
Redraw Buttons + Windows + CharList + CharEditBox
FadeIn

DO
    I$ = INKEY$

    MouseInfo X, Y, b

    IF I$ = CHR$(0) + "K" THEN
        IF EditingFont(CurChar, 0) > 1 THEN EditingFont(CurChar, 0) = EditingFont(CurChar, 0) - 1
        Redraw CharEditBox
    END IF

    IF I$ = CHR$(0) + "M" THEN
        IF EditingFont(CurChar, 0) < 8 THEN EditingFont(CurChar, 0) = EditingFont(CurChar, 0) + 1
        Redraw CharEditBox
    END IF

    IF b = 0 AND CurButton <> 0 THEN
        IF CurButton = 1 AND MouseOver(4, 23, 40, 35) THEN Quit
        IF CurButton = 2 AND MouseOver(60, 23, 96, 35) THEN OpenFile
        IF CurButton = 3 AND MouseOver(116, 23, 152, 35) THEN SaveFile
        IF CurButton = 4 AND MouseOver(172, 23, 200, 35) THEN NewFile
        IF CurButton = 5 AND MouseOver(220, 23, 256, 35) THEN Test
        IF CurButton = 6 AND MouseOver(5, 41, 261, 297) THEN
            CurCharI = 0
            DrawFrame 4, 40, 262, 298
            FOR J = 1 TO 256 STEP 16
                FOR I = 1 TO 256 STEP 16
                    IF MouseOver(I + 4, J + 40, I + 4 + 16, J + 40 + 16) THEN CurChar = CurCharI
                    CurCharI = CurCharI + 1
                NEXT
            NEXT
            Redraw CharList + CharEditBox
            CurButton = 0
            GOTO SkipDraw
        END IF
        IF CurButton = 7 AND MouseOver(311, 41, 439, 297) THEN
            CurPos = 0
            FOR I = 311 TO 439 STEP 16
                FOR J = 41 TO 297 STEP 16
                    IF MouseOver(I, J, I + 15, J + 15) THEN
                        IF POINT(I + 1, J + 1) = 0 THEN
                            LINE (I, J)-(I + 15, J + 15), 15, BF
                        ELSE
                            LINE (I, J)-(I + 15, J + 15), 0, BF
                        END IF
                        FOR L = 1 TO 16
                            PosY = ((L - .5) * 16) + 41
                            BitPattern = 0
                            IF POINT(312, PosY) = 15 THEN BitPattern = BitPattern + 128
                            IF POINT(312 + 16, PosY) = 15 THEN BitPattern = BitPattern + 64
                            IF POINT(312 + 32, PosY) = 15 THEN BitPattern = BitPattern + 32
                            IF POINT(312 + 48, PosY) = 15 THEN BitPattern = BitPattern + 16
                            IF POINT(312 + 64, PosY) = 15 THEN BitPattern = BitPattern + 8
                            IF POINT(312 + 80, PosY) = 15 THEN BitPattern = BitPattern + 4
                            IF POINT(312 + 96, PosY) = 15 THEN BitPattern = BitPattern + 2
                            IF POINT(312 + 112, PosY) = 15 THEN BitPattern = BitPattern + 1
                            EditingFont(CurChar, L) = BitPattern
                        NEXT L
                    END IF
                NEXT J
            NEXT I
            Redraw CharEditBox
            CurButton = 0
            GOTO SkipDraw
        END IF
        CurButton = 0
        Redraw Buttons
    END IF

SkipDraw:
    IF b = 1 AND CurButton <> 0 THEN
        IF CurButton = 1 AND NOT MouseOver(4, 23, 40, 35) THEN CurButton = 0: Redraw Buttons
        IF CurButton = 2 AND NOT MouseOver(60, 23, 96, 35) THEN CurButton = 0: Redraw Buttons
        IF CurButton = 3 AND NOT MouseOver(116, 23, 152, 35) THEN CurButton = 0: Redraw Buttons
        IF CurButton = 4 AND NOT MouseOver(172, 23, 200, 35) THEN CurButton = 0: Redraw Buttons
        IF CurButton = 5 AND NOT MouseOver(220, 23, 256, 35) THEN CurButton = 0: Redraw Buttons
        IF CurButton = 6 AND NOT MouseOver(5, 41, 261, 297) THEN CurButton = 0
        IF CurButton = 7 AND NOT MouseOver(311, 41, 439, 297) THEN CurButton = 0
    END IF

    IF b = 1 AND CurButton = 0 THEN
        IF MouseOver(4, 23, 40, 35) THEN
            CurButton = 1
            Redraw Buttons
        ELSEIF MouseOver(60, 23, 96, 35) THEN
            CurButton = 2
            Redraw Buttons
        ELSEIF MouseOver(116, 23, 152, 35) THEN
            CurButton = 3
            Redraw Buttons
        ELSEIF MouseOver(172, 23, 200, 35) THEN
            CurButton = 4
            Redraw Buttons
        ELSEIF MouseOver(220, 23, 256, 35) THEN
            CurButton = 5
            Redraw Buttons
        ELSEIF MouseOver(5, 41, 261, 297) THEN
            CurButton = 6
        ELSEIF MouseOver(311, 41, 439, 297) THEN
            CurButton = 7
        ELSEIF CurButton <> 0 THEN
            CurButton = 0
            Redraw Buttons
        END IF
    END IF
LOOP

END SUB

FUNCTION MouseDetected

Regs.AX = 0

IntQB &H33, Regs, Regs

MouseDetected = Regs.AX

END FUNCTION

SUB MouseHide

Regs.AX = 2
IntQB &H33, Regs, Regs

END SUB

SUB MouseInfo (Xpos, YPos, But)

Regs.AX = 3
IntQB &H33, Regs, Regs

But = Regs.BX

Xpos = Regs.CX
YPos = Regs.DX

END SUB

FUNCTION MouseOver (X1, Y1, X2, Y2)
SHARED X, Y, b
IF X > X1 AND X < X2 AND Y > Y1 AND Y < Y2 THEN MouseOver = True
END FUNCTION

SUB MouseShow

Regs.AX = 1
IntQB &H33, Regs, Regs

END SUB

SUB NewFile
VLoadFont "BIOS16.FNT", EditingFont()
Redraw CharList + CharEditBox
END SUB

SUB OpenFile
DrawWindow 150, 150, 490, 200, True, "Select File"
VFont 154, 172, "Enter name and path to VFont file:", 0, 0, 0, FontBios8()

DrawFrame 154, 181, 486, 196
LINE (155, 182)-(485, 195), 0, BF
LOCATE 14, 21
INPUT "", FileName$

ON ERROR GOTO FileError
VLoadFont FileName$, EditingFont()
ON ERROR GOTO 0

Redraw Buttons + Windows + CharList + CharEditBox

END SUB

SUB Quit
FadeOut
SCREEN 0
PRINT "That was VFontEdit 3, by Sam Thursfield"
Black
FadeIn
END
END SUB

SUB Redraw (Drawing)

WAIT &H3DA, 8
MouseHide

IF Drawing AND Windows THEN DrawWindow 0, 0, 639, 349, True, "VFontEdit 3"

IF Drawing AND Buttons THEN
    DrawButton 4, 23, 40, 35, "Exit", CurButton = 1
    DrawButton 60, 23, 96, 35, "Open", CurButton = 2
    DrawButton 116, 23, 152, 35, "Save", CurButton = 3
    DrawButton 172, 23, 200, 35, "New", CurButton = 4
    DrawButton 220, 23, 256, 35, "Test", CurButton = 5
END IF

IF Drawing AND CharList THEN
    CurCharI = 0
    CurCharJ = 0
    DrawFrame 4, 40, 262, 298
    FOR J = 1 TO 256 STEP 16
        FOR I = 8 TO 256 STEP 16
            LINE (I + 4, J + 40)-(I + 4 + 8, J + 40 + 16), 2, BF
            VFont I + 4, J + 40, CHR$(CurCharJ), 14, 0, 0, FontBios16()
            CurCharJ = CurCharJ + 1
        NEXT
        FOR I = 1 TO 256 STEP 16
            LINE (I + 4, J + 40)-(I + 4 + 8, J + 40 + 16), 1 + ABS(3 * (CurChar = CurCharI)), BF
            VFont I + 4, J + 40, CHR$(CurCharI), 15, 0, 0, EditingFont()
            CurCharI = CurCharI + 1
        NEXT
    NEXT
END IF

IF Drawing AND CharEditBox THEN
    DrawFrame 310, 40, 441, 298
    LINE (311, 41)-(440, 297), 0, BF
    VFontBig 311, 41, CHR$(CurChar), 15, 0, 0, EditingFont()
    FOR I = 1 TO 9
        IF EditingFont(CurChar, 0) = I - 1 THEN
            LINE (311 + ((I - 1) * 16), 41)-(311 + ((I - 1) * 16), 297), 14
        ELSE
            LINE (311 + ((I - 1) * 16), 41)-(311 + ((I - 1) * 16), 297), 4
        END IF
    NEXT I
    FOR I = 1 TO 17
        LINE (311, 41 + ((I - 1) * 16))-(439, 41 + ((I - 1) * 16)), 4
    NEXT I
END IF

MouseShow

END SUB

SUB SaveFile

DrawWindow 150, 150, 490, 200, True, "Select File"
VFont 154, 172, "Enter name and path for VFont file:", 0, 0, 0, FontBios8()

DrawFrame 154, 181, 486, 196
LINE (155, 182)-(485, 195), 0, BF
LOCATE 14, 21
INPUT "", FileName$

ON ERROR GOTO FileError
VSaveFont FileName$, EditingFont()
ON ERROR GOTO 0

Redraw Buttons + Windows + CharList + CharEditBox

END SUB

SUB Test

SHARED X, Y, b

CurButton = 0
MouseHide
DrawWindow 50, 50, 590, 300, True, "Test Font"


VFont 54, 70, "Quick-Blowing Zephyrs Vex Daft Jim! 0123456789? (It's working!)", 15, 0, 0, EditingFont()
VFont 54, 86, "Quick-Blowing Zephyrs Vex Daft Jim! 0123456789? (It's working!)", 15, 0, 2, EditingFont()
VFont 54, 102, "Quick-Blowing Zephyrs Vex Daft Jim! 0123456789? (It's working!)", 15, 0, 4, EditingFont()
VFont 54, 118, "Quick-Blowing Zephyrs Vex Daft Jim! 0123456789? (It's working!)", 15, 0, 6, EditingFont()
DrawButton 54, 284, 586, 296, "OK", False
MouseShow

DO
    MouseInfo X, Y, b

    IF b = 0 AND CurButton <> 0 THEN
        IF CurButton = 1 AND MouseOver(54, 284, 586, 296) THEN
            CurButton = 0
            Redraw Buttons + CharList + CharEditBox + Windows
            EXIT SUB
        END IF
    END IF

    IF b = 1 AND CurButton <> 0 THEN
        IF CurButton = 1 AND NOT MouseOver(54, 284, 586, 296) THEN CurButton = 0: MouseHide: DrawButton 54, 284, 586, 296, "OK", False: MouseShow
    END IF

    IF b = 1 AND CurButton = 0 THEN
        IF MouseOver(54, 284, 586, 296) THEN
            CurButton = 1
            MouseHide
            DrawButton 54, 284, 586, 296, "OK", True
            MouseShow
        ELSEIF CurButton <> 0 THEN
            CurButton = 0
            MouseHide
            DrawButton 54, 284, 586, 296, "OK", False
            MouseShow
        END IF
    END IF

LOOP
END SUB

SUB VFont (Xpos, YPos, Text$, Col, BCol, Style, FontArray())

IF Xpos = -32767 THEN Xpos = ScreenWidth \ 2 - INT(LEN(Text$) * 4)

Xx = Xpos + 7 - (5 * (Style AND 2))
Yy = YPos

IF Style AND 4 THEN
    VFont Xpos - 1, YPos - 1, Text$, BCol, 0, Style - 4, FontArray()
END IF

FOR Chr = 1 TO LEN(Text$)
    'MemPtr = 8 * ASC(MID$(Text$, Chr, 1)) + &HE
    FOR L = 0 TO 15
        BitPattern = FontArray(ASC(MID$(Text$, Chr, 1)), L + 1)
        IF Style AND 2 THEN
            IF (BitPattern AND 1) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 7 THEN PSET (Xx + (15 - (L \ 2)), Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 2) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 6 THEN PSET (Xx + (15 - (L \ 2)) - 1, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 4) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 5 THEN PSET (Xx + (15 - (L \ 2)) - 2, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 8) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 4 THEN PSET (Xx + (15 - (L \ 2)) - 3, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 16) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 3 THEN PSET (Xx + (15 - (L \ 2)) - 4, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 32) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 2 THEN PSET (Xx + (15 - (L \ 2)) - 5, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 64) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 1 THEN PSET (Xx + (15 - (L \ 2)) - 6, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 128) AND FontArray(ASC(MID$(Text$, Chr, 1)), 0) > 0 THEN PSET (Xx + (15 - (L \ 2)) - 7, Yy + L), Col - (15 - L) * (Style AND 1)
            'IF Style AND 8 AND L = 7 THEN PSET (Xx + (15- (L \ 2)) - 16, Yy + L), Col - (15 - L) * (Style AND 1)
        ELSE
            IF (BitPattern AND 1) THEN PSET (Xx, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 2) THEN PSET (Xx - 1, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 4) THEN PSET (Xx - 2, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 8) THEN PSET (Xx - 3, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 16) THEN PSET (Xx - 4, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 32) THEN PSET (Xx - 5, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 64) THEN PSET (Xx - 6, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 128) THEN PSET (Xx - 7, Yy + L), Col - (15 - L) * (Style AND 1)
            'IF Style AND 8 AND L = 15 THEN PSET (Xx - 16, Yy + L), Col - (15 - L) * (Style AND 1)
        END IF
    NEXT
    Xx = Xx + FontArray(ASC(MID$(Text$, Chr, 1)), 0)
NEXT
END SUB

SUB VFontBig (Xpos, YPos, Text$, Col, BCol, Style, FontArray())

Xx = Xpos - 16
Yy = YPos

FOR Chr = 1 TO LEN(Text$)       'Do each letter
    Xx = Xx + 128               'Step one character
    'MemPtr = 8 * ASC(MID$(Text$, Chr, 1)) + &HE
    FOR L = 0 TO 15              'Do each line
        'DEF SEG = &HFFA6
        '    BitPattern = PEEK(MemPtr + L)
        'DEF SEG
        BitPattern = FontArray(ASC(MID$(Text$, Chr, 1)), L + 1)
        IF BitPattern AND 1 THEN LINE (Xx, Yy + L * 16)-(Xx + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 2 THEN LINE (Xx - 16, Yy + L * 16)-(Xx - 16 + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 4 THEN LINE (Xx - 32, Yy + L * 16)-(Xx - 32 + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 8 THEN LINE (Xx - 48, Yy + L * 16)-(Xx - 48 + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 16 THEN LINE (Xx - 64, Yy + L * 16)-(Xx - 64 + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 32 THEN LINE (Xx - 80, Yy + L * 16)-(Xx - 80 + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 64 THEN LINE (Xx - 96, Yy + L * 16)-(Xx - 96 + 16, Yy + 16 + L * 16), Col, BF
        IF BitPattern AND 128 THEN LINE (Xx - 112, Yy + L * 16)-(Xx - 112 + 16, Yy + 16 + L * 16), Col, BF
    NEXT
NEXT

END SUB

SUB VLoadFont (File$, Array())
DEF SEG = VARSEG(Array(0, 0))
    BLOAD File$, VARPTR(Array(0, 0))
DEF SEG
END SUB

SUB VSaveFont (File$, Array%())
DEF SEG = VARSEG(Array(0, 0))
    BSAVE File$, VARPTR(Array(0, 0)), (256 * 17) * 2
DEF SEG
END SUB

