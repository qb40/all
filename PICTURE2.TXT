COMMON SHARED cursorx, cursory, tadd%, spread$, userobj$, imagex, imagey, option$(), optionx%(), cps$(), clrs(), return$, clr%, tools$, pens%, objs1$, area1(), cscreen, screens, scxx1, scyy1, scxx2, scyy2

'------------------------------Shared variables------------------------------

'  cursorx              - x position of cursor
'  cursory              - y position of cursor
'  area1(),area2()      - stores screen area
'  pens%                - stores wteher pen up or pen down (0/1)
'  spread$              - spreadline selected
'  userobj$             - user object created in OBJ folder
'  imagex               - size x of an object stored (no main)
'  imagey               - size y of an object stored (no main)
'  option$()            - array of options stored for use in mainmenu
'  optionx%()           - array for options x positions for use in mainmenu     
'  cps$()               - array for storing comments on options for mainmenu                                                                                                         
'  clrs()               - double array for storing palette values
'  return$              - stores any return value of a subroutine                                                                                                                                                                        
'  clr%                 - current colour selected                                                                                                             
'  tools$               - stores the tool name selected
'  objs1%               - stores the object name selected
'  tadd%                - stores time constant

'============================================================================







'===============================Declarations=================================

DECLARE SUB scrlinex (hx1!, hy1!, hx2!, hy2!, t AS INTEGER, c AS INTEGER)
DECLARE SUB scrliney (hx1!, hy1!, hx2!, hy2!, t AS INTEGER, c AS INTEGER)
DECLARE SUB scrlinez (hx1!, hy1!, hx2!, hy2!, t AS INTEGER, c AS INTEGER)
DECLARE SUB scrpnt (x1!, y1!, x2!, y2!, c%)
DECLARE SUB setscreens (n AS INTEGER)
DECLARE SUB scrarea (x1, y1, x2, y2)
DECLARE SUB setscr (s%)
DECLARE SUB scrdot (x, y, c%)
DECLARE SUB scrLINE (x1, y1, x2, y2, c%)
DECLARE SUB scrCIRCLE (x1, y1, rad, c%, rat)
DECLARE SUB scrgene (x1, y1, x2, y2)
DECLARE SUB scrgenerate ()
DECLARE SUB scrend ()
DECLARE FUNCTION position (x AS SINGLE, y AS SINGLE)

DECLARE SUB nullify ()
DECLARE SUB obj1 ()
DECLARE SUB obj1p (s AS STRING, x AS SINGLE, y AS SINGLE)

DECLARE SUB obj2 ()
DECLARE SUB obj2d (f$, x AS SINGLE, y AS SINGLE, rx AS SINGLE, ry AS SINGLE)
DECLARE SUB objimgsize (f$)
DECLARE SUB obj2p (f$, x AS SINGLE, y AS SINGLE)


DECLARE SUB savedot (x, y, c%)
DECLARE FUNCTION drawmenu$ ()
DECLARE SUB text ()
DECLARE SUB sprdlns ()
DECLARE FUNCTION escmenu$ (t$, b$, hx1%, hy1%, hx2%, hy2%, c%)


DECLARE FUNCTION nospc$ (s AS STRING)
DECLARE SUB tool ()
DECLARE SUB tlbrush ()
DECLARE SUB tlline ()
DECLARE SUB tlrectangle ()
DECLARE SUB tlcircle (o%)
DECLARE SUB tlspray ()
DECLARE SUB tlsprdln ()
DECLARE SUB tlpencil (fp%)

DECLARE SUB getit (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE)
DECLARE SUB disp (x1 AS SINGLE, y1 AS SINGLE, x2 AS SINGLE, y2 AS SINGLE)
DECLARE SUB cursor (x AS SINGLE, y AS SINGLE, c%, a%, p%)
DECLARE SUB intensity (r%, g%, b%)
DECLARE SUB colour ()
DECLARE SUB status (s AS STRING, c%)
DECLARE SUB ld ()
DECLARE SUB win (h$, c%)
DECLARE FUNCTION randnum (s1 AS SINGLE, s2 AS SINGLE)
DECLARE SUB pnt (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, c%)
DECLARE FUNCTION realx (x AS SINGLE)
DECLARE FUNCTION realy (y AS SINGLE)

DECLARE FUNCTION mainmenu% (h$, hc%, u%, os%, oc%, bx1%, by1%, bx2%, by2%, bc%, bt%)
DECLARE FUNCTION menu$ (s$, x1%, y1%, x2%, y2%, t$, b$, f%, tt%, c%, BF%, ib%)
DECLARE SUB rub (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER)
DECLARE SUB linez (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, t AS INTEGER, c AS INTEGER)
DECLARE SUB char (s AS STRING, x AS INTEGER, y AS INTEGER, t AS INTEGER, p AS INTEGER, c AS INTEGER)
DECLARE SUB word (s AS STRING, x AS INTEGER, y AS INTEGER, t1 AS INTEGER, p1 AS INTEGER, c AS INTEGER, p AS INTEGER, f AS INTEGER, t AS DOUBLE)
DECLARE SUB expand (s AS STRING, x AS INTEGER, y AS INTEGER, t1 AS INTEGER, p1 AS INTEGER, c AS INTEGER, p AS INTEGER, f AS INTEGER, t AS DOUBLE)
DECLARE SUB linex (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, t AS INTEGER, c AS INTEGER)
DECLARE SUB liney (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER, t AS INTEGER, c AS INTEGER)
DECLARE SUB click1 ()
DECLARE SUB click2 ()
DECLARE SUB click3 ()

DECLARE FUNCTION currdir$ ()
DECLARE FUNCTION volname$ ()
DECLARE FUNCTION volsnum$ ()
DECLARE FUNCTION numfiles (s AS STRING)
DECLARE FUNCTION numdirs ()
DECLARE FUNCTION dirs$ (s AS STRING)
DECLARE FUNCTION fzl$ (a AS STRING, s AS STRING)
DECLARE FUNCTION files$ (a AS STRING, s AS STRING)

'============================================================================


DEFINT A-B '//////// Defining variable types
DEFSNG C
DEFINT D-H
DEFLNG I
DEFINT J-Z
DEFDBL T

ON ERROR GOTO errs
'============================Declaring Keys==================================
CONST backspc = 8, enter = 13, htab = 9
CONST left = 75, right = 77, up = 72, down = 80
CONST uplt = 71, uprt = 73, dnlt = 79, dnrt = 81
CONST insert = 82, home = 73, pageup = 71, del = 83, endk = 81, pagedn = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134
'-----------------------------Keys declared---------------------------------
DIM SHARED area(25000)
return$ = ""
clr% = 0
REM $DYNAMIC
DIM clrs(16, 3)
CLS
SCREEN 12
getit 0, 0, 10, 10
inst = 0
'===============================Getting timer================================
aaaa: sec1 = VAL(RIGHT$(TIME$, 2))
IF sec1 > 58 THEN GOTO aaaa
DO UNTIL (VAL(RIGHT$(TIME$, 2)) = sec1 + 1)
inst = inst + 1
LOOP
tadd% = FIX((inst / 29102) + .5)
IF tadd% <= 1 THEN tadd% = 1
IF tadd% = 3 THEN tadd% = 4
IF tadd% > 5 AND tadd% <= 7 THEN tadd% = 5
IF tadd% > 7 AND tadd% < 10 THEN tadd% = 10
IF tadd% > 10 AND tadd% <= 15 THEN tadd% = 10
IF tadd% > 15 THEN tadd% = 20
'----------------------------------------------------------------------------

'======================Analyse VGA Screen Mode===============================
1 : COLOR 15
PRINT "Analysing"
PRINT "VGA Screen"
PRINT "Mode "
SOUND 21000, 50
FOR i = 0 TO 300 STEP 16
FOR j = 0 TO 15
CIRCLE (320, 240), i + j, j
NEXT
NEXT
OPEN "B", #1, "log.log"
IF LOF(1) < 2 THEN t = .3
SEEK #1, 1
af$ = "afffff"
PUT #1, 1, af$
CLOSE #1
  'time speed++++++++++++++++++++++++
FOR i = 0 TO 63 STEP 2
FOR j = 0 TO 15
PALETTE j, 65536 * 0 + 256 * 0 + i
SOUND 21000, t
NEXT
NEXT
FOR i = 0 TO 63 STEP 2
FOR j = 0 TO 15
PALETTE j, 65536 * 0 + 256 * i + 0
SOUND 21000, t
NEXT
NEXT
FOR i = 0 TO 63 STEP 2
FOR j = 0 TO 15
PALETTE j, 65536 * i + 256 * 0 + 0
SOUND 21000, t
NEXT
NEXT
CLS
COLOR 15
PRINT "VGA Screen test complete."
click2

'--------------------------VGA Screen test complete--------------------------



'===============================Main Menu====================================
main: CLS


'///////////////////////////////Creating menu
GOSUB setdefault
GOSUB setcolors
CLS
PALETTE 9, 65536 * 63 + 256 * 32 + 32
DIM SHARED option$(5)
DIM SHARED optionx%(5)
option$(0) = "CREATE A PICTURE"
optionx%(0) = 200
option$(1) = "LOAD A PICTURE": optionx%(1) = 200
option$(2) = "HELP": optionx%(2) = 280
option$(3) = "CREDITS": optionx%(3) = 260
option$(4) = "EXIT": optionx%(4) = 280
DIM SHARED cps$(5)
cps$(0) = "OPTIONS FOR CREATING IDIVIDUAL PICTURE OR PROJECT FOR PICTURE OBJECTS"
cps$(1) = "OPTIONS FOR LOADING INDIVIDUAL PICTURE OR PROJECT FOR PICTURE OBJECTS"
cps$(2) = "PROVIDES HELP"
cps$(3) = "SUBHAJIT SAHU"
cps$(4) = "EXITS TO SYSTEM"
zx% = mainmenu%("PICTURE CREATOR", 9, 0, 200, 14, 0, 80, 115, 400, 7, 6)
ERASE option$, optionx%, cps$
IF zx% <> 5 THEN GOSUB sclear
IF zx% = 1 THEN GOTO opt1
IF zx% = 2 THEN GOTO opt2
IF zx% = 3 THEN GOTO opt3
IF zx% = 4 THEN GOTO opt4
IF zx% = 5 THEN GOTO opt5
'//////////////////////Working menu complete
'------------------------------Main Menu complete----------------------------


'================================Option 3 HELP===============================
opt3: expand "HELP", 25, 5, 5, 4, 11, 38, 21000, 0
frf = FREEFILE
OPEN "B", #1, "help.hlp"
VIEW PRINT 5 TO 27
FOR i = 1 TO LOF(frf)
  SEEK #1, i
  zz$ = INPUT$(1, #1)
  IF (zz$ = CHR$(9) OR zz$ = CHR$(10) OR zz$ = CHR$(13)) OR (ASC(zz$) > 31 AND ASC(zz$) < 127) THEN PRINT zz$;
k$ = INPUT$(1)
NEXT
lns = 0
k$ = INPUT$(1)
opt31: GOSUB sclear
CLEAR
GOTO main
'----------------------------Option 3 complete-------------------------------

'=============================Option 4 CREDITS===============================
opt4: expand "CREDITS", 25, 5, 5, 4, 11, 38, 21000, 0
expand "WELCOME MY FRIENDS, I, SUBHAJIT SAHU INVITE YOU TO THE WORLD OF", 3, 100, 2, 1, 9, 9, 21000, 0
expand "DRAWING PICTURES IN MY NEW PICTURE CREATOR. HAVE FUN.", 3, 150, 2, 1, 9, 9, 21000, 0
k$ = INPUT$(1)
GOSUB sclear
CLEAR
GOTO main
'-----------------------------Option 4 complete------------------------------

'==============================Option 5 EXIT=================================
opt5: acc$ = menu$("EXIT", 150, 100, 390, 250, "ARE YOU SURE YOU WANT TO EXIT TO SYSTEM ", "YES.NO.", 20000, .2, 1, 3, 0)
IF acc$ = "YES" THEN
SYSTEM
ELSE
CLEAR
GOTO main
END IF
'------------------------------Option 5 complete-----------------------------

'=======================Option 1 Create a picture============================
opt1: CLS
setscreens 2
scrarea 42, 52, 538, 438
setscr 2
GOSUB setdefault
GOSUB setcolors
ERASE cps$
DIM SHARED cps$(3)
cps$(0) = "ALLOWS YOU TO CREATE AN IDIVIDUAL PICTURE"
cps$(1) = "ALLOWS YOU TO CREATE A PICTURE PROJECT FOR EACH AS PICTURE ELEMENTS"
cps$(2) = "GO BACK TO MAIN MENU"
PALETTE 9, 65536 * 32 + 256 * 63 + 32
ERASE option$, optionx%
DIM SHARED option$(3)
DIM SHARED optionx%(3)
option$(0) = "CREATE A PICTURE": optionx%(0) = 240
option$(1) = "CREATE A PICTURE PROJECT": optionx%(1) = 200
option$(2) = "BACK": optionx%(2) = 320
zx% = mainmenu%("CREATE A PICTURE", 9, 0, 200, 13, 0, 80, 115, 400, 7, 6)
GOSUB sclear
ERASE option$, optionx%, cps$
IF zx% = 1 THEN GOTO opt11
IF zx% = 2 THEN GOTO opt12
IF zx% = 3 THEN GOTO main
opt11: file$ = menu$("FILE NAME", 50, 50, 400, 200, "WHAT IS THE FILE NAME#", "OK.", 21000, 0, 12, 13, 1)
file$ = LEFT$(file$, LEN(file$) - 3)
ld
CLS
cursorx = 280
cursory = 170
nullify
OPEN "O", #1, "ram.sys"
CLOSE #1
OPEN "B", #1, "ram.sys"
afg$ = CHR$(0)
PUT #1, position(496, 396), afg$
CLOSE #1
tools$ = "pencil:1"
win file$, 14
cfff: k$ = INKEY$
  SELECT CASE k$
    
     CASE CHR$(0) + CHR$(up)
    
    
         cursory = cursory - .5
         IF cursory < 0 THEN cursory = 0
  
     getit cursorx, cursory, cursorx, cursory
     tlpencil etr%
     
      CASE CHR$(0) + CHR$(down)
    
         cursory = cursory + .5
         IF cursory > 386 THEN cursory = 386
    
     getit cursorx, cursory, cursorx, cursory
     tlpencil etr%
  
   CASE CHR$(0) + CHR$(left)
    
         cursorx = cursorx - .5
         IF cursory < 0 THEN cursory = 0
     
     getit cursorx, cursory, cursorx, cursory
     tlpencil etr%
  
   CASE CHR$(0) + CHR$(right)
  
   cursorx = cursorx + .5
   IF cursory > 496 THEN cursory = 496
   
   getit cursorx, cursory, cursorx, cursory
   tlpencil etr%
  
   CASE CHR$(backspc)
  
   pens% = (pens% + 1) MOD 2
   IF pens% = 0 THEN status "PEN UP", 7 ELSE status "PEN DOWN", 7
  
   CASE CHR$(enter)
  
   IF tools$ <> "" THEN
      tzs$ = LEFT$(tools$, 4)
     
      SELECT CASE tzs$
      CASE "penc"
      LINE (realx(cursorx), realy(cursory))-(realx(cursorx), realy(cursory)), clr%
      GET (realx(cursorx - 10), realy(cursory - 10))-(realx(cursorx + 10), realy(cursory + 10)), area
      tlpencil 1
      getit cursorx, cursory, cursorx, cursory
      CASE "brus"
      str% = pens%
      pens% = 1
      tlbrush
      pens% = str%
      CASE "line"
      str% = pens%
      pens% = 1
      tlline
      pens% = str%
      CASE "rect"
      str% = pens%
      pens% = 1
      tlrectangle
      pens% = str%
      CASE "circ"
      str% = pens%
      pens% = 1
      tlcircle (0)
      pens% = str%
      CASE "oval"
      str% = pens%
      pens% = 1
      tlcircle (1)
      pens% = str%
      CASE "spra"
      str% = pens%
      pens% = 1
      tlspray
      pens% = str%
      CASE ELSE
      END SELECT
   
    END IF
   
    IF (spread$ <> "") THEN tlsprdln
    IF (objs1$ <> "") THEN obj1p objs1$, cursorx, cursory
    IF (userobj$ <> "") THEN obj2d userobj$, cursorx, cursory, 1, 1

   CASE CHR$(27)
   GOTO cree
  
   CASE CHR$(0) + CHR$(kf1)
   'savefile
  
   CASE CHR$(0) + CHR$(kf2)
   colour
  
   CASE CHR$(0) + CHR$(kf3)
   intensity FIX(clrs(clr%, 0)), FIX(clrs(clr%, 1)), FIX(clrs(clr%, 2))
   clrs(clr%, 0) = red%
   clrs(clr%, 1) = green%
   clrs(clr%, 2) = blue%
   GOSUB setcolors

   CASE CHR$(0) + CHR$(kf4)
   tool
  
   CASE CHR$(0) + CHR$(kf5)
   obj1
  
   CASE CHR$(0) + CHR$(kf6)
   obj2
  
   CASE CHR$(0) + CHR$(kf7)
   text
  
   CASE CHR$(0) + CHR$(kf8)
   sprdlns
  
   CASE ELSE
   
    END SELECT
GOTO cfff


      


cree:
opt12:





'=========================Option 2 Load a picture============================
opt2: CLS
GOSUB setdefault
GOSUB setcolors
DIM SHARED cps$(3)
cps$(0) = "ALLOWS YOU TO LOAD AN IDIVIDUAL PICTURE"
cps$(1) = "ALLOWS YOU TO LOAD A PICTURE PROJECT FOR EACH AS PICTURE ELEMENTS"
cps$(2) = "GO BACK TO MAIN MENU"
PALETTE 9, 65536 * 32 + 256 * 32 + 63
DIM SHARED option$(3)
DIM SHARED optionx%(3)
option$(0) = "LOAD A PICTURE": optionx%(0) = 240
option$(1) = "LOAD A PICTURE PROJECT": optionx%(1) = 200
option$(2) = "BACK": optionx%(2) = 320
zx% = mainmenu%("LOAD A PICTURE", 9, 0, 200, 13, 0, 80, 115, 400, 7, 6)
GOSUB sclear
ERASE option$, optionx%, cps$
IF zx% = 1 THEN GOTO opt21
IF zx% = 2 THEN GOTO opt22
IF zx% = 3 THEN GOTO main
opt21:
opt22:






END

























'==============================Colour restore================================

setdefault:  ERASE clrs
DIM clrs(16, 3)
RESTORE
FOR i = 0 TO 15
FOR j = 0 TO 2
READ clv
clrs(i, j) = clv
NEXT
NEXT
RETURN

setcolors: RESTORE
FOR i = 0 TO 15
clv1 = clrs(i, 0): clv2 = clrs(i, 1): clv3 = clrs(i, 2)
PALETTE i, 65536 * clv3 + 256 * clv2 + clv1
NEXT
RETURN

sclear: FOR i = 1 TO 63
FOR j = 0 TO 15
clv1 = clrs(j, 0): clv2 = clrs(j, 1): clv3 = clrs(j, 2)
IF clv1 < i THEN clv1 = i
IF clv2 < i THEN clv2 = i
IF clv3 < i THEN clv3 = i
PALETTE j, 65536 * clv3 + 256 * clv2 + clv1
NEXT
SOUND i * 100, .5
NEXT
CLS
GOSUB setdefault
GOSUB setcolors
RETURN

DATA 0,0,0, 0,0,43, 0,43,0, 0,43,43, 43,0,0, 43,0,43, 43,43,0, 43,43,43, 10,10,10, 20,20,63, 20,63,20, 20,63,63, 63,20,20, 63,20,63, 63,63,20, 63,63,63


errs: RESUME NEXT
'============================================================================
DEFSNG A-B, D-Z


REM $STATIC
SUB char (s AS STRING, x AS INTEGER, y AS INTEGER, t AS INTEGER, p AS INTEGER, c AS INTEGER)
SELECT CASE s$
CASE "A"
linez x, y + 9 * p, x, y + 2 * p, t, c
linez x, y + 2 * p, x + 2 * p, y, t, c
linez x + 2 * p, y, x + 4 * p, y, t, c
linez x + 4 * p, y, x + 6 * p, y + 2 * p, t, c
linez x + 6 * p, y + 2 * p, x + 6 * p, y + 9 * p, t, c
linez x, y + 5 * p, x + 6 * p, y + 5 * p, t, c

CASE "B"
linez x, y, x, y + 9 * p, t, c
linez x, y, x + 6 * p, y, t, c
linez x + 6 * p, y, x + 6 * p, y + 6 * p, t, c
linez x, y + 4 * p, x + 6 * p, y + 4 * p, t, c
linez x + 6 * p, y + 5 * p, x + 6 * p, y + 9 * p, t, c
linez x + 6 * p, y + 9 * p, x, y + 9 * p, t, c

CASE "C"
linez x + 6 * p, y, x + 2 * p, y, t, c
linez x + 2 * p, y, x, y + 2 * p, t, c
linez x, y + 2 * p, x, y + 7 * p, t, c
linez x, y + 7 * p, x + 2 * p, y + 9 * p, t, c
linez x + 2 * p, y + 9 * p, x + 6 * p, y + 9 * p, t, c

CASE "D"
linez x, y, x + 4 * p, y, t, c
linez x + 4 * p, y, x + 6 * p, y + 2 * p, t, c
linez x + 6 * p, y + 2 * p, x + 6 * p, y + 7 * p, t, c
linez x + 6 * p, y + 7 * p, x + 4 * p, y + 9 * p, t, c
linez x + 4 * p, y + 9 * p, x, y + 9 * p, t, c
linez x, y + 9 * p, x, y, t, c

CASE "E"
linez x + 6 * p, y, x, y, t, c
linez x, y, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c
linez x, y + 4 * p, x + 6 * p, y + 4 * p, t, c

CASE "F"
linez x + 6 * p, y, x, y, t, c
linez x, y, x, y + 9 * p, t, c
linez x, y + 4 * p, x + 6 * p, y + 4 * p, t, c

CASE "G"
linez x + 6 * p, y, x + 1 * p, y, t, c
linez x + 1 * p, y, x, y + 1 * p, t, c
linez x, y + 1 * p, x, y + 8 * p, t, c
linez x, y + 8 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x + 5 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 6 * p, y + 8 * p, t, c
linez x + 6 * p, y + 8 * p, x + 6 * p, y + 5 * p, t, c
linez x + 6 * p, y + 5 * p, x + 4 * p, y + 5 * p, t, c
linez x + 4 * p, y + 5 * p, x + 4 * p, y + 7 * p, t, c

CASE "H"
linez x, y, x, y + 9 * p, t, c
linez x + 6 * p, y, x + 6 * p, y + 9 * p, t, c
linez x, y + 4 * p, x + 6 * p, y + 4 * p, t, c

CASE "I"
linez x, y, x + 6 * p, y, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c
linez x + 3 * p, y, x + 3 * p, y + 9 * p, t, c

CASE "J"
linez x, y, x + 6 * p, y, t, c
linez x + 4 * p, y, x + 4 * p, y + 8 * p, t, c
linez x + 4 * p, y + 8 * p, x + 3 * p, y + 9 * p, t, c
linez x + 3 * p, y + 9 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x, y + 8 * p, t, c
linez x, y + 8 * p, x, y + 6 * p, t, c

CASE "K"
linez x, y, x, y + 9 * p, t, c
linez x + 6 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x, y + 4 * p, t, c
linez x, y + 4 * p, x + 6 * p, y + 8 * p, t, c
linez x + 6 * p, y + 8 * p, x + 6 * p, y + 9 * p, t, c

CASE "L"
linez x, y, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c

CASE "M"
linez x, y + 9 * p, x, y, t, c
linez x, y, x + 3 * p, y + 4 * p, t, c
linez x + 3 * p, y + 4 * p, x + 6 * p, y, t, c
linez x + 6 * p, y, x + 6 * p, y + 9 * p, t, c

CASE "N"
linez x, y + 9 * p, x, y, t, c
linez x, y, x + 6 * p, y + 9 * p, t, c
linez x + 6 * p, y + 9 * p, x + 6 * p, y, t, c

CASE "O"
linez x, y, x + 6 * p, y, t, c
linez x, y, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c
linez x + 6 * p, y, x + 6 * p, y + 9 * p, t, c

CASE "P"
linez x, y + 5 * p, x + 6 * p, y + 5 * p, t, c
linez x + 6 * p, y, x + 6 * p, y + 5 * p, t, c
linez x, y, x + 6 * p, y, t, c
linez x, y, x, y + 9 * p, t, c

CASE "Q"
linez x, y, x + 4 * p, y, t, c
linez x, y, x, y + 9 * p, t, c
linez x + 4 * p, y, x + 4 * p, y + 9 * p, t, c
linez x, y + 9 * p, x + 4 * p, y + 9 * p, t, c
linez x + 3 * p, y + 5 * p, x + 3 * p, y + 6 * p, t, c
linez x + 3 * p, y + 6 * p, x + 6 * p, y + 9 * p, t, c

CASE "R"
linez x, y + 9 * p, x, y, t, c
linez x, y, x + 6 * p, y, t, c
linez x + 6 * p, y, x + 6 * p, y + 5 * p, t, c
linez x + 6 * p, y + 5 * p, x, y + 5 * p, t, c
linez x, y + 5 * p, x + 6 * p, y + 9 * p, t, c

CASE "S"
linez x, y, x + 6 * p, y, t, c
linez x, y, x, y + 4 * p, t, c
linez x, y + 4 * p, x + 6 * p, y + 4 * p, t, c
linez x + 6 * p, y + 4 * p, x + 6 * p, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c

CASE "T"
linez x, y, x + 6 * p, y, t, c
linez x + 3 * p, y, x + 3 * p, y + 9 * p, t, c

CASE "U"
linez x, y, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c
linez x + 6 * p, y, x + 6 * p, y + 9 * p, t, c

CASE "V"
linez x, y, x + 3 * p, y + 9 * p, t, c
linez x + 3 * p, y + 9 * p, x + 6 * p, y, t, c

CASE "W"
linez x, y, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 3 * p, y + 5 * p, t, c
linez x + 3 * p, y + 5 * p, x + 6 * p, y + 9 * p, t, c
linez x + 6 * p, y + 9 * p, x + 6 * p, y, t, c

CASE "X"
linez x, y, x + 6 * p, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y, t, c

CASE "Y"
linez x, y, x + 3 * p, y + 5 * p, t, c
linez x + 3 * p, y + 5 * p, x + 6 * p, y, t, c
linez x + 3 * p, y + 5 * p, x + 3 * p, y + 9 * p, t, c

CASE "Z"
linez x, y, x + 6 * p, y, t, c
linez x + 6 * p, y, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c

CASE "."
linez x, y + 9 * p, x, y + 9 * p, t, c
CASE ","
linez x + 1 * p, y + 9 * p, x, y + 10 * p, t, c
CASE " "
CASE "1"
linez x + 1 * p, y + 1 * p, x + 2 * p, y, t, c
linez x + 2 * p, y, x + 2 * p, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c
CASE "2"
linez x, y + 1 * p, x + 1 * p, y, t, c
linez x + 1 * p, y, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x + 6 * p, y + 4 * p, t, c
linez x + 6 * p, y + 4 * p, x, y + 9 * p, t, c
linez x, y + 9 * p, x + 6 * p, y + 9 * p, t, c
CASE "3"
linez x, y + 1 * p, x + 1 * p, y, t, c
linez x + 1 * p, y, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x + 6 * p, y + 8 * p, t, c
linez x + 1 * p, y + 4 * p, x + 6 * p, y + 4 * p, t, c
linez x + 1 * p, y + 9 * p, x + 5 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 6 * p, y + 9 * p, t, c
CASE "4"
linez x + 4 * p, y + 9 * p, x + 6 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 5 * p, y, t, c
linez x + 5 * p, y, x, y + 5 * p, t, c
linez x, y + 5 * p, x, y + 6 * p, t, c
linez x, y + 6 * p, x + 6 * p, y + 6 * p, t, c
CASE "5"
linez x, y, x + 6 * p, y, t, c
linez x, y, x, y + 3 * p, t, c
linez x, y + 3 * p, x + 5 * p, y + 3 * p, t, c
linez x + 5 * p, y + 3 * p, x + 6 * p, y + 4 * p, t, c
linez x + 6 * p, y + 4 * p, x + 6 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x + 6 * p, y + 9 * p, t, c
linez x, y + 8 * p, x + 1, y + 9 * p, t, c
CASE "6"
linez x + 6 * p, y + 1 * p, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 1 * p, y, t, c
linez x + 1 * p, y, x, y + 1 * p, t, c
linez x, y + 1 * p, x, y + 8 * p, t, c
linez x, y + 8 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x + 5 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 6 * p, y + 8 * p, t, c
linez x + 6 * p, y + 8 * p, x + 6 * p, y + 5 * p, t, c
linez x + 6 * p, y + 5 * p, x + 5 * p, y + 4 * p, t, c
linez x + 5 * p, y + 4 * p, x, y + 4 * p, t, c
CASE "7"
linez x, y, x + 6 * p, y, t, c
linez x + 6 * p, y, x + 6 * p, y + 3 * p, t, c
linez x + 6 * p, y + 3 * p, x + 4 * p, y + 5 * p, t, c
linez x + 4 * p, y + 5 * p, x + 4 * p, y + 9 * p, t, c
CASE "8"
linez x + 1 * p, y, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x + 6 * p, y + 3 * p, t, c
linez x + 6 * p, y + 3 * p, x + 5 * p, y + 4 * p, t, c
linez x + 5 * p, y + 4 * p, x + 1 * p, y + 4 * p, t, c
linez x + 1 * p, y + 4 * p, x, y + 3 * p, t, c
linez x, y + 3 * p, x, y + 1 * p, t, c
linez x, y + 1 * p, x + 1 * p, y, t, c
linez x, y + 5 * p, x + 1 * p, y + 4 * p, t, c
linez x + 5 * p, y + 4 * p, x + 6 * p, y + 5 * p, t, c
linez x, y + 5 * p, x, y + 8 * p, t, c
linez x + 6 * p, y + 5 * p, x + 6 * p, y + 8 * p, t, c
linez x, y + 8 * p, x + 1 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 6 * p, y + 8 * p, t, c
linez x + 1 * p, y + 9 * p, x + 5 * p, y + 9 * p, t, c
CASE "9"
linez x + 1 * p, y + 4 * p, x + 6 * p, y + 4 * p, t, c
linez x, y + 3 * p, x + 1, y + 4 * p, t, c
linez x, y + 1 * p, x, y + 3 * p, t, c
linez x, y + 1 * p, x + 1 * p, y, t, c
linez x + 1 * p, y, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x + 6 * p, y + 8 * p, t, c
linez x + 6 * p, y + 8 * p, x + 5 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 1 * p, y + 9 * p, t, c
linez x, y + 8 * p, x + 1 * p, y + 9 * p, t, c
CASE "0"
linez x + 1 * p, y, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x + 6 * p, y + 8 * p, t, c
linez x + 5 * p, y + 9 * p, x + 6 * p, y + 8 * p, t, c
linez x + 1 * p, y + 9 * p, x + 5 * p, y + 9 * p, t, c
linez x, y + 8 * p, x + 1 * p, y + 9 * p, t, c
linez x, y + 1 * p, x, y + 8 * p, t, c
linez x, y + 1 * p, x + 1 * p, y, t, c
CASE "["
linez x, y, x + 1 * p, y, t, c
linez x, y + 9 * p, x + 1 * p, y + 9 * p, t, c
linez x, y, x, y + 9 * p, t, c
CASE "]"
linez x, y, x + 1 * p, y, t, c
linez x, y + 9 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y, x + 1 * p, y + 9 * p, t, c
CASE "\"
linez x, y, x + 6 * p, y + 9 * p, t, c
CASE "/"
linez x, y + 9 * p, x + 6 * p, y, t, c
CASE "`"
linez x, y, x + 1 * p, y + 1 * p, t, c
CASE "-"
linez x, y + 4 * p, x + 6 * p, y + 4 * p, t, c
CASE "="
linez x, y + 3 * p, x + 6 * p, y + 3 * p, t, c
linez x, y + 5 * p, x + 6 * p, y + 5 * p, t, c
CASE "|"
linez x + 3 * p, y, x + 3 * p, y + 3 * p, t, c
linez x + 3 * p, y + 5 * p, x + 3 * p, y + 9 * p, t, c
CASE ":"
linez x + 3 * p, y + 3 * p, x + 3 * p, y + 3 * p, t, c
linez x + 3 * p, y + 5 * p, x + 3 * p, y + 5 * p, t, c
CASE "*"
linez x + 2 * p, y + 3 * p, x + 4 * p, y + 5 * p, t, c
linez x + 2 * p, y + 5 * p, x + 4 * p, y + 3 * p, t, c
linez x + 3 * p, y + 3 * p, x + 3 * p, y + 5 * p, t, c
CASE "+"
linez x + 3 * p, y + 3 * p, x + 3 * p, y + 5 * p, t, c
linez x + 2 * p, y + 4 * p, x + 4 * p, y + 4 * p, t, c
CASE "!"
linez x + 1 * p, y, x + 1 * p, y + 7 * p, t, c
linez x + 1 * p, y + 9 * p, x + 1 * p, y + 9 * p, t, c
CASE "@"
linez x + 5 * p, y + 9 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x, y + 8 * p, t, c
linez x, y + 8 * p, x, y + 1 * p, t, c
linez x, y + 1 * p, x + 1 * p, y, t, c
linez x + 1 * p, y, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 6 * p, y + 1 * p, t, c
linez x + 6 * p, y + 1 * p, x + 6 * p, y + 7 * p, t, c
linez x + 6 * p, y + 7 * p, x + 2 * p, y + 7 * p, t, c
linez x + 2 * p, y + 7 * p, x + 2 * p, y + 2 * p, t, c
linez x + 2 * p, y + 2 * p, x + 5 * p, y + 2 * p, t, c
linez x + 5 * p, y + 2 * p, x + 6 * p, y + 3 * p, t, c
CASE "#"
linez x + 2 * p, y, x + 2 * p, y + 9 * p, t, c
linez x + 4 * p, y, x + 4 * p, y + 9 * p, t, c
linez x, y + 3 * p, x + 6 * p, y + 4 * p, t, c
linez x, y + 5 * p, x + 6 * p, y + 5 * p, t, c
CASE "$"
linez x + 6 * p, y + 1 * p, x + 5 * p, y, t, c
linez x + 5 * p, y, x + 1 * p, y, t, c
linez x + 1 * p, y, x, y + 1 * p, t, c
linez x, y + 1 * p, x + 6 * p, y + 8 * p, t, c
linez x + 6 * p, y + 8 * p, x + 5 * p, y + 9 * p, t, c
linez x + 5 * p, y + 9 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x, y + 8 * p, t, c
linez x + 3 * p, y, x + 3 * p, y + 9 * p, t, c
CASE "%"
linez x, y + 9 * p, x + 6 * p, y, t, c
linez x + 2 * p, y + 3 * p, x + 2 * p, y + 3 * p, t, c
linez x + 4 * p, y + 5 * p, x + 4 * p, y + 5 * p, t, c
CASE "^"
linez x + 1 * p, y + 2 * p, x + 3 * p, y, t, c
linez x + 3 * p, y, x + 5 * p, y + 2 * p, t, c
CASE "&"
linez x + 6 * p, y + 9 * p, x, y + 1 * p, t, c
linez x, y + 1 * p, x + 1 * p, y, t, c
linez x + 1 * p, y, x + 2 * p, y, t, c
linez x + 2 * p, y, x, y + 6 * p, t, c
linez x, y + 6 * p, x, y + 8 * p, t, c
linez x, y + 8 * p, x + 1 * p, y + 9 * p, t, c
linez x + 1 * p, y + 9 * p, x + 4 * p, y + 9 * p, t, c
linez x + 4 * p, y + 9 * p, x + 6 * p, y + 7 * p, t, c
CASE "("
linez x + 4 * p, y, x + 3 * p, y + 2 * p, t, c
linez x + 4 * p, y + 9 * p, x + 3 * p, y + 7 * p, t, c
linez x + 4 * p, y + 2 * p, x + 4 * p, y + 7 * p, t, c
CASE ")"
linez x + 2 * p, y, x + 3 * p, y + 2 * p, t, c
linez x + 2 * p, y + 9 * p, x + 3 * p, y + 7 * p, t, c
linez x + 3 * p, y + 2 * p, x + 3 * p, y + 7 * p, t, c
CASE "{"
linez x + 4 * p, y, x + 3 * p, y + 1 * p, t, c
linez x + 4 * p, y + 9 * p, x + 3 * p, y + 8 * p, t, c



CASE " "
CASE ELSE
FOR i% = 0 TO 6
linez x + i% * p, y, x + i% * p, y + 9 * p, t, c
NEXT

END SELECT
END SUB

SUB click1
FOR i% = 2000 TO 6000 STEP 500
SOUND i%, .15
NEXT
FOR i% = 6000 TO 2000 STEP -500
SOUND i%, .15
NEXT
END SUB

SUB click2
FOR i% = 10000 TO 15000 STEP 900
SOUND i%, .05
NEXT
FOR i% = 3000 TO 300 STEP -500
SOUND i%, .05
NEXT
END SUB

SUB click3
FOR i% = 2300 TO 3000 STEP 200
SOUND i%, .05
NEXT
FOR i% = 21000 TO 15000 STEP -700
SOUND i%, .05
NEXT
END SUB

SUB colour
SHARED clr%
click1
dc% = clr%
linex 0, 0, 40, 0, 3, 2
linex 0, 70, 40, 70, 3, 2
liney 1, 0, 1, 70, 3, 2
liney 40, 0, 40, 70, 3, 2
linex 0, 440, 250, 440, 3, 2
linex 0, 479, 250, 479, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
status "SELECTING COLOURS.", 1
GOSUB sclr2

DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

  k$ = INKEY$
SELECT CASE k$
  CASE CHR$(0) + CHR$(up)
  GOSUB sclr3
  clr% = clr% - 2
  IF clr% < 0 THEN clr% = 16 + clr%
  GOSUB sclr2
  CASE CHR$(0) + CHR$(down)
  GOSUB sclr3
  clr% = clr% + 2
  IF clr% > 15 THEN clr% = clr% - 16
  GOSUB sclr2
  CASE CHR$(0) + CHR$(right)
  GOSUB sclr3
  clr% = clr% + 1
  IF clr% > 15 THEN clr% = 0
  GOSUB sclr2
  CASE CHR$(0) + CHR$(left)
  GOSUB sclr3
  clr% = clr% - 1
  IF clr% < 0 THEN clr% = 15
  GOSUB sclr2
  CASE ELSE
END SELECT

LOOP
IF k$ = CHR$(27) THEN
status "COLOUR NOT SELECTED.", 1
r% = 9 + (FIX(ABS(clr% - 1) / 2)) * 7
c% = clr% MOD 2
IF c% = 0 THEN LINE (4, r%)-(16, r% + 6), 0 ELSE LINE (24, r%)-(36, r% + 6), 0
clr% = dc%
ELSEIF k$ = CHR$(enter) THEN
status "COLOUR SELECTED - " + STR$(clr%), 1
r% = 9 + (FIX(ABS(clr% - 1) / 2)) * 7
c% = clr% MOD 2

ELSE
dv$ = k$
END IF
GOTO sclr1
sclr2: r% = 9 + FIX(ABS(clr% / 2)) * 7
r1% = r% + 6
c% = clr% MOD 2
IF c% = 0 THEN
LINE (4, r%)-(16, r%), 15
LINE -(16, r1%), 15
LINE -(4, r1%), 15
LINE -(4, r%), 15
ELSE
LINE (24, r%)-(36, r%), 15
LINE -(36, r1%), 15
LINE -(24, r1%), 15
LINE -(24, r%), 15
END IF
status "CHOOSING COLOUR - " + STR$(clr%), 1
pnt 10, 75, 30, 85, clr%
RETURN
sclr3: r% = 9 + (FIX(ABS(clr% + .5) / 2)) * 7
r1% = r% + 6
c% = clr% MOD 2
IF c% = 0 THEN
LINE (4, r%)-(16, r%), 0
LINE -(16, r1%), 0
LINE -(4, r1%), 0
LINE -(4, r%), 0
ELSE
LINE (24, r%)-(36, r%), 0
LINE -(36, r1%), 0
LINE -(24, r1%), 0
LINE -(24, r%), 0
END IF
RETURN

sclr1: linex 0, 0, 40, 0, 3, 14
linex 0, 70, 40, 70, 3, 14
liney 1, 0, 1, 70, 3, 14
liney 40, 0, 40, 70, 3, 14
linex 0, 440, 250, 440, 3, 14
linex 0, 479, 250, 479, 3, 14
liney 1, 440, 1, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
END SUB

FUNCTION currdir$
STATIC a AS INTEGER
SHELL "dir *.* >ram.pth"
a = FREEFILE
OPEN "I", #a, "ram.pth"
FOR i = 1 TO 3
LINE INPUT #a, z$
NEXT
LINE INPUT #a, z$
CLOSE #a
SHELL "del ram.pth"
currdir$ = RIGHT$(z$, LEN(z$) - 14)
END FUNCTION

SUB cursor (x AS SINGLE, y AS SINGLE, c%, a%, p%)
VIEW SCREEN (42, 22)-(538, 438)'------------Possible error
tx1 = x
ty1 = y
setscr 1
SELECT CASE p%
CASE 1
scrLINE x, y, x, y, c%
savedot x, y, c%
CASE 0
disp x, y, x, y
CASE ELSE
END SELECT
CLOSE #z
c1% = (c% + 1) MOD 16
IF c1% = 0 THEN c1% = 1
c2% = (c1% + 1) MOD 16
IF c2% = 0 THEN c2% = 1
disp x - 8, y - 8, x + 8, y + 8
setscr 2
SELECT CASE a%
CASE 1
scrLINE tx1 - 1, ty1 - 1, tx1 + 1, ty1 - 1, c1%
scrLINE tx1 + 1, ty1 - 1, tx1 + 1, ty1 + 1, c1%
scrLINE tx1 + 1, ty1 + 1, tx1 - 1, ty1 + 1, c1%
scrLINE tx1 - 1, ty1 + 1, tx1 - 1, ty1 - 1, c1%
CASE 2
scrLINE tx1 - 2, ty1 - 2, tx1 + 2, ty1 - 2, c1%
scrLINE tx1 + 2, ty1 - 2, tx1 + 2, ty1 + 2, c1%
scrLINE tx1 + 2, ty1 + 2, tx1 - 2, ty1 + 2, c1%
scrLINE tx1 - 2, ty1 + 2, tx1 - 2, ty1 - 2, c1%
CASE 3
scrLINE tx1, ty1 - 2, tx1 + 2, ty1, c1%
scrLINE tx1 + 2, ty1, tx1, ty1 + 2, c1%
scrLINE tx1, ty1 + 2, tx1 - 2, ty1, c1%
scrLINE tx1 - 2, ty1, tx1, ty1 - 2, c1%
CASE 4
scrCIRCLE tx1, ty1, 3, c1%, 1
CASE 5
scrLINE tx1 - 2, ty1 - 2, tx1 + 2, ty1 - 2, c1%
scrLINE tx1 + 2, ty1 - 2, tx1 + 2, ty1 + 2, c1%
scrLINE tx1 + 2, ty1 + 2, tx1 - 2, ty1 + 2, c1%
scrLINE tx1 - 2, ty1 + 2, tx1 - 2, ty1 - 2, c1%
scrLINE tx1, ty1 - 2, tx1, ty1 - 5, c2%
scrLINE tx1 + 2, ty1, tx1 + 5, ty1, c2%
scrLINE tx1, ty1 + 2, tx1, ty1 + 5, c2%
scrLINE tx1 - 2, ty1, tx1 - 5, ty1, c2%
CASE 6
scrCIRCLE tx1, ty1, 3, c1%, 1
scrLINE tx1, ty1 - 3, tx1, ty1 - 5, c2%
scrLINE tx1 + 3, ty1, tx1 + 5, ty1, c2%
scrLINE tx1, ty1 + 3, tx1, ty1 + 5, c2%
scrLINE tx1 - 3, ty1, tx1 - 5, ty1, c2%
CASE 7
scrLINE tx1 - 2, ty1 - 2, tx1 + 2, ty1 - 2, c1%
scrLINE tx1 + 2, ty1 - 2, tx1 + 2, ty1 + 2, c1%
scrLINE tx1 + 2, ty1 + 2, tx1 - 2, ty1 + 2, c1%
scrLINE tx1 - 2, ty1 + 2, tx1 - 2, ty1 - 2, c1%
scrCIRCLE tx1, ty1, 3, c2%, 1
CASE 8
scrLINE tx1 - 2, ty1 - 2, tx1 + 2, ty1 - 2, c1%
scrLINE tx1 + 2, ty1 - 2, tx1 + 2, ty1 + 2, c1%
scrLINE tx1 + 2, ty1 + 2, tx1 - 2, ty1 + 2, c1%
scrLINE tx1 - 2, ty1 + 2, tx1 - 2, ty1 - 2, c1%
scrLINE tx1 - 2, ty1 - 2, tx1 - 5, ty1 - 5, c2%
scrLINE tx1 + 2, ty1 - 2, tx1 + 5, ty1 - 5, c2%
scrLINE tx1 + 2, ty1 + 2, tx1 + 5, ty1 + 5, c2%
scrLINE tx1 - 2, ty1 + 2, tx1 - 5, ty1 + 5, c2%
CASE 9
scrLINE tx1 - 2, ty1 - 2, tx1 + 2, ty1 - 2, c1%
scrLINE tx1 + 2, ty1 - 2, tx1 + 2, ty1 + 2, c1%
scrLINE tx1 + 2, ty1 + 2, tx1 - 2, ty1 + 2, c1%
scrLINE tx1 - 2, ty1 + 2, tx1 - 2, ty1 - 2, c1%
scrLINE tx1 + 4, ty1 - 2, tx1 + 4, ty1 + 2, c2%
scrLINE tx1 + 5, ty1 - 1, tx1 + 5, ty1 + 1, c2%
scrLINE tx1 + 6, ty1, tx1 + 6, ty1, c2%
scrLINE tx1 + 4, ty1 - 2, tx1 + 4, ty1 + 2, c2%
scrLINE tx1 + 5, ty1 - 1, tx1 + 5, ty1 + 1, c2%
scrLINE tx1 + 6, ty1, tx1 + 6, ty1, c2%
scrLINE tx1 - 2, ty1 - 4, tx1 + 2, ty1 - 4, c2%
scrLINE tx1 - 1, ty1 - 5, tx1 + 1, ty1 - 5, c2%
scrLINE tx1, ty1 - 6, tx1, ty1 - 6, c2%
scrLINE tx1 - 2, ty1 + 4, tx1 + 2, ty1 + 4, c2%
scrLINE tx1 - 1, ty1 + 5, tx1 + 1, ty1 + 5, c2%
scrLINE tx1, ty1 + 6, tx1, ty1 + 6, c2%
CASE 10
scrCIRCLE tx1, ty1, 3, c1%, 1
scrLINE tx1 + 4, ty1 - 2, tx1 + 4, ty1 + 2, c2%
scrLINE tx1 + 5, ty1 - 1, tx1 + 5, ty1 + 1, c2%
scrLINE tx1 + 6, ty1, tx1 + 6, ty1, c2%
scrLINE tx1 + 4, ty1 - 2, tx1 + 4, ty1 + 2, c2%
scrLINE tx1 + 5, ty1 - 1, tx1 + 5, ty1 + 1, c2%
scrLINE tx1 + 6, ty1, tx1 + 6, ty1, c2%
scrLINE tx1 - 2, ty1 - 4, tx1 + 2, ty1 - 4, c2%
scrLINE tx1 - 1, ty1 - 5, tx1 + 1, ty1 - 5, c2%
scrLINE tx1, ty1 - 6, tx1, ty1 - 6, c2%
scrLINE tx1 - 2, ty1 + 4, tx1 + 2, ty1 + 4, c2%
scrLINE tx1 - 1, ty1 + 5, tx1 + 1, ty1 + 5, c2%
scrLINE tx1, ty1 + 6, tx1, ty1 + 6, c2%
CASE ELSE
END SELECT
VIEW SCREEN (0, 0)-(640, 480)
scrgene tx1 - 10, ty1 - 10, tx1 + 10, ty1 + 10
END SUB

FUNCTION dirs$ (s AS STRING)
SHELL "dir >ram.pth"
a = FREEFILE
OPEN "I", #a, "ram.pth"
FOR i = 1 TO 2 STEP 0
IF EOF(1) THEN GOTO dirs
LINE INPUT #a, z$
IF MID$(z$, 14, 5) = "<DIR>" AND LEFT$(z$, 1) <> "." THEN c$ = c$ + LEFT$(z$, 8) + s$
NEXT
dirs: CLOSE #a
KILL "ram.pth"
dirs$ = c$
END FUNCTION

SUB disp (xx1 AS SINGLE, yy1 AS SINGLE, xx2 AS SINGLE, yy2 AS SINGLE)
z = FREEFILE
OPEN "B", #z, "ram.sys"
FOR i = yy1 TO yy2 STEP .5
FOR j = xx1 TO xx2 STEP .5
ips2 = position(i, j)
SEEK #z, ips2
as$ = INPUT$(1, #z)
cl = ASC(as$) - 65
LINE (realx(xx), realy(yy))-(realx(xx), realy(yy)), cl
NEXT
NEXT
CLOSE #z
END SUB

FUNCTION drawmenu$
click1
linex 540, 380, 640, 380, 3, 2
linex 540, 400, 640, 400, 3, 2
liney 540, 380, 540, 400, 3, 2
liney 639, 380, 639, 400, 3, 2
linex 0, 440, 250, 440, 3, 2
linex 0, 479, 250, 479, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
status "1-Save  2-Save as object 3-Back to main menu", 1
sdraw1: k$ = INPUT$(1)
IF k$ = CHR$(27) THEN
  s$ = "Null"
  GOTO sdraw
 ELSEIF k$ = "1" THEN
  s$ = "Save"
  GOTO sdraw
 ELSEIF k$ = "2" THEN
  s$ = "Sobj"
  GOTO sdraw
 ELSEIF k$ = "3" THEN
  s$ = "Back"
  GOTO sdraw
END IF
GOTO sdraw1
sdraw: linex 540, 380, 640, 380, 3, 14
linex 540, 400, 640, 400, 3, 14
liney 540, 380, 540, 400, 3, 14
liney 639, 380, 639, 400, 3, 14
linex 0, 440, 250, 440, 3, 2
linex 0, 479, 250, 479, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
drawnemu$ = s$
END FUNCTION

SUB expand (s AS STRING, x AS INTEGER, y AS INTEGER, t1 AS INTEGER, p1 AS INTEGER, c AS INTEGER, p AS INTEGER, f AS INTEGER, t AS DOUBLE)
FOR i% = 0 TO p
word s$, x, y, t1, p1, 0, i% - 1, f, t
word s$, x, y, t1, p1, c, i%, f, t
NEXT
END SUB

FUNCTION files$ (a AS STRING, s AS STRING)
fl$ = fzl$(a$, s$)
ot$ = ""
ch = 0: cm = 0
FOR i = 1 TO LEN(fl$)
ad$ = MID$(fl$, i, 1)


   IF ad$ = s$ THEN cm = cm + 1
   zz = ASC(ad$)
   IF ((zz > 47 AND zz < 58) OR (zz > 64 AND zz < 91) OR (zz > 96 AND zz < 123)) THEN cm = 0
   IF cm < 2 THEN ot$ = ot$ + ad$
NEXT
fl$ = ot$
ot$ = ""
cm = 0
FOR i = 1 TO LEN(fl$)
ad$ = MID$(fl$, i, 1)
IF (ad$ = " " AND cm = 0) THEN
ot$ = ot$ + "."
cm = cm + 1
ELSEIF (ad$ = " " AND cm > 0) THEN
cm = cm + 1
ELSEIF ad$ = s$ THEN
ot$ = ot$ + s$
cm = 0
ELSE
ot$ = ot$ + ad$
END IF
NEXT
files$ = ot$
END FUNCTION

FUNCTION fzl$ (a AS STRING, s AS STRING)
SHELL "dir *." + a$ + ">ram.pth"
b = FREEFILE
OPEN "I", #b, "ram.pth"
FOR i = 1 TO 2 STEP 0
IF EOF(1) THEN GOTO fils
LINE INPUT #b, z$
IF (MID$(z$, 14, 5) <> "<DIR>" AND MID$(z$, 11, 4) <> "file" AND LEFT$(z$, 11) <> " Volume in " AND LEFT$(z$, 12) <> " Volume Seri" AND LEFT$(z$, 13) <> " Directory of") THEN c$ = c$ + LEFT$(z$, 12) + s$
NEXT
fils: CLOSE #b
KILL "ram.pth"
fzl$ = c$
END FUNCTION

SUB getit (xx1 AS SINGLE, yy1 AS SINGLE, xx2 AS SINGLE, yy2 AS SINGLE)
z = FREEFILE
OPEN "B", #z, "ram.sys"
FOR i = yy1 TO yy2 STEP .5
FOR j = xx1 TO xx2 STEP .5
ips2 = position(i, j)
abc = POINT(realx(i), realy(j))
as$ = CHR$(65 + abc)
SEEK #z, ips2
PUT #z, ips2, as$
NEXT
NEXT
CLOSE #z
END SUB

SUB intensity (r%, g%, b%)
SHARED clr%, red%, green%, blue%
click1
pnt 10, 75, 30, 85, clr%
LINE (9, 74)-(31, 74), 15
LINE -(31, 86), 15
LINE -(9, 86), 15
LINE -(9, 74), 15
linex 0, 70, 40, 70, 3, 2
linex 0, 90, 40, 90, 3, 2
liney 1, 70, 1, 90, 3, 2
liney 40, 70, 40, 90, 3, 2
status "SELECTING COLOUR INTENSITY - " + STR$(clr%), 1
DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)
k$ = INKEY$

SELECT CASE k$
CASE CHR$(0) + CHR$(insert)
r% = r% + 1
IF r% > 63 THEN r% = 63
GOSUB sinte1
CASE "+"
r% = r% + 1
IF r% > 63 THEN r% = 63
GOSUB sinte1

CASE CHR$(0) + CHR$(del)
r% = r% - 1
IF r% < 0 THEN r% = 0
GOSUB sinte1
CASE CHR$(0) + CHR$(home)
g% = g% + 1
IF g% > 63 THEN g% = 63
GOSUB sinte1
CASE CHR$(0) + CHR$(endk)
g% = g% - 1
IF g% < 0 THEN g% = 0
GOSUB sinte1
CASE CHR$(0) + CHR$(pageup)
b% = b% + 1
IF b% > 63 THEN b% = 63
GOSUB sinte1
CASE CHR$(0) + CHR$(pagedn)
b% = b% - 1
IF b% < 0 THEN b% = 0
GOSUB sinte1
CASE ELSE
END SELECT
LOOP
GOTO sinte
sinte1: PALETTE clr%, 65536 * b% + 256 * g% + r%
status "RED - " + STR$(r%) + " GREEN - " + STR$(g%) + " BLUE - " + STR$(b%), 1
RETURN
sinte: IF k$ = CHR$(enter) THEN
blue% = b%
green% = g%
red% = r%
END IF
linex 0, 70, 40, 70, 3, 14
linex 0, 90, 40, 90, 3, 14
liney 1, 70, 1, 90, 3, 14
liney 40, 70, 40, 90, 3, 14
IF k$ = CHR$(enter) THEN status "COLOUR INTENSITY SELECTED.", 1 ELSE status "COLOUR INTENSITY NOT SELECTED.", 1
END SUB

SUB ld
CLS
df$ = "A"
expand "LOADING . . .", 25, 20, 6, 4, 15, 38, 21000, 0
z = FREEFILE
OPEN "B", #z, "ram.sys"
FOR xx = 0 TO 496 STEP .5
FOR yy = 0 TO 386 STEP .5
ips = position(xx, yy)
SEEK #z, ips
PUT #z, ips, df$
NEXT
LINE (0, 300)-(xx, 300), 10
NEXT
END SUB

SUB linex (hx1 AS INTEGER, hy1 AS INTEGER, hx2 AS INTEGER, hy2 AS INTEGER, t AS INTEGER, c AS INTEGER)
IF t >= 1 THEN LINE (hx1, hy1)-(hx2, hy2), c
FOR i = 2 TO t
z% = FIX(i / 2)
s = i MOD 2
IF s = 0 THEN s = 1 ELSE s = -1
z% = z% * s
LINE (hx1, hy1 + z%)-(hx2, hy2 + z%), c
NEXT
END SUB

SUB liney (hx1 AS INTEGER, hy1 AS INTEGER, hx2 AS INTEGER, hy2 AS INTEGER, t AS INTEGER, c AS INTEGER)
IF t >= 1 THEN LINE (hx1, hy1)-(hx2, hy2), c
FOR i = 2 TO t
z% = FIX(i / 2)
s = i MOD 2
IF s = 0 THEN s = 1 ELSE s = -1
z% = z% * s
LINE (hx1 + z%, hy1)-(hx2 + z%, hy2), c
NEXT
END SUB

SUB linez (hx1 AS INTEGER, hy1 AS INTEGER, hx2 AS INTEGER, hy2 AS INTEGER, t AS INTEGER, c AS INTEGER)
IF ABS(hx1 - hx2) > ABS(hy1 - hy2) THEN linex hx1, hy1, hx2, hy2, t, c ELSE liney hx1, hy1, hx2, hy2, t, c
END SUB

FUNCTION mainmenu% (h$, hc%, u%, os%, oc%, bx1%, by1%, bx2%, by2%, bc%, bt%)
SHARED option$, optionx%, cps$, tadd%
PALETTE 0, 65536 * 7 + 256 * 2 + 2
DIM curl%(80), curr%(80)
linez 15, 90, 10, 95, 4, 2
linez 10, 95, 15, 100, 4, 2
linez 20, 90, 15, 95, 4, 2
linez 15, 95, 20, 100, 4, 2
GET (10, 90)-(20, 100), curl%
rub 5, 90, 25, 200
linez 10, 90, 15, 95, 4, 2
linez 15, 95, 10, 100, 4, 2
linez 15, 90, 20, 95, 4, 2
linez 20, 95, 15, 100, 4, 2
GET (10, 90)-(20, 100), curr%
rub 5, 90, 25, 100
bchp% = FIX(ABS(bx2% - bx1%) / 10)
bxas% = bx1% + 5
bxad% = by1% + 5
CLS
expand h$, 25, 5, 5, 4, hc%, 38, 21000, 0
linez 10, 100, 630, 100, 6, u%
opts% = UBOUND(option$)
FOR i% = 0 TO opts% - 1
expand option$(i%), optionx%(i%), os% + (i% * 20), 3, 1, oc%, 15, 21000, 0
NEXT
expand "INTELLIGENT MINDS CORPORATION", 380, 450, 2, 1, 2, 9, 21000, 0
'//////////////////////////////Menu created

linez bx1%, by1%, bx2%, by1%, 5, bc%   '+++++++Drawing text box
linez bx2%, by1%, bx2%, by2%, 5, bc%
linez bx2%, by2%, bx1%, by2%, 5, bc%
linez bx1%, by2%, bx1%, by1%, 5, bc%
PAINT ((bx1% + bx2%) / 2, (by1% + by2%) / 2), 15, bc%
PALETTE 15, 65536 * 2 + 256 * 7 + 2
'////////////////////////////Working menu
cp$ = cps$(0)
cpos% = 1
inst% = 0
DO UNTIL k$ = CHR$(enter)
k$ = INKEY$
IF inst% MOD 5000 = 0 THEN
CIRCLE (10 + FIX(inst% / 1000), 470), 3, 6
CIRCLE (15 + FIX(inst% / 1000), 470), 3, 1
END IF

SELECT CASE k$
CASE CHR$(0) + CHR$(up)
click1
xcv% = optionx%(cpos% - 1)
zxc% = os% + (cpos% - 1) * 20
rub xcv% - 80, zxc% - 5, xcv% - 15, zxc% + 15
xcv% = xcv% + (LEN(option$(cpos% - 1)) * 15)
rub xcv% + 15, zxc% - 5, xcv% + 80, zxc% + 15
pnt bx1% + 3, by1% + 3, bx2% - 3, by2% - 3, 15
cpos% = cpos% - 1
IF cpos% = 0 THEN cpos% = opts%
cp$ = cps$(cpos% - 1)
chpr% = 0
CASE CHR$(0) + CHR$(down)
click1
xcv% = optionx%(cpos% - 1)
zxc% = os% + (cpos% - 1) * 20
rub xcv% - 80, zxc% - 5, xcv% - 15, zxc% + 15
xcv% = xcv% + (LEN(option$(cpos% - 1)) * 15)
rub xcv% + 15, zxc% - 5, xcv% + 80, zxc% + 15
pnt bx1% + 3, by1% + 3, bx2% - 3, by2% - 3, 15
cpos% = cpos% + 1
IF cpos% = opts% + 1 THEN cpos% = 1
cp$ = cps$(cpos% - 1)
chpr% = 0
CASE ELSE
END SELECT

inst% = inst% + tadd%
IF inst% MOD 1000 = 0 THEN
dsp% = FIX(inst% / 1000)
vpos% = 180 + (20 * cpos%)
xcv% = optionx%(cpos% - 1)
zxc% = os% + (cpos% - 1) * 20
rub xcv% - 80, zxc% - 5, xcv% - 15, zxc% + 15
PUT (xcv% - 30 - dsp%, vpos%), curl%
xcv% = xcv% + (LEN(option$(cpos% - 1)) * 15)
rub xcv% + 15, zxc% - 5, xcv% + 80, zxc% + 15
PUT (xcv% + 20 + dsp%, vpos%), curr%
END IF

IF inst% > 30000 THEN inst% = 0
IF chpr% < LEN(cp$) AND inst% MOD 5000 = 0 THEN
chpr% = chpr% + 1
chp$ = MID$(cp$, chpr%, 1)
tx1% = chpr% MOD bchp%
ty1% = FIX(chpr% / bchp%)
tx1% = bxas% + (tx1% * 10)
ty1% = bxad% + (ty1% * 15)
char chp$, tx1%, ty1%, 2, 1, bt%
click2
END IF
LOOP
ERASE curr%, curl%
mainmenu% = cpos%
END FUNCTION

FUNCTION menu$ (s$, hx1%, hy1%, hx2%, hy2%, t$, b$, f%, tt%, c%, bc%, ib%)
SHARED tadd%
'////////////////////Counting buttons
FOR i% = 1 TO LEN(b$)
IF MID$(b$, i%, 1) = "." THEN but = but + 1
NEXT
ebs% = (hx2% - hx1% - 10 - (5 * but)) / but

'///////////////////Reading buttons
DIM button$(but)
DIM xxx1(but)
butt = 1
FOR i% = 1 TO LEN(b$)
zx$ = MID$(b$, i%, 1)
IF zx$ = "." THEN
button$(bu) = MID$(b$, butt, i% - butt)
butt = i% + 1
bu = bu + 1
END IF
NEXT

'////////////////////Opening window
word s$, hx1%, hy1%, 3, 2, c%, 20, 21000, 0
ty1% = hy1% + 25
linez hx1%, ty1%, hx2%, ty1%, 4, (c% + 1) MOD 16
sy1% = ty1% + 5
sx2% = hx1% + 2 * FIX(ABS(hx2% - hx1%) / 3)
tc% = (c% + 2) MOD 16
linez hx1%, sy1%, sx2%, sy1%, 6, tc%
dy1% = sy1% + 3
FOR i% = dy1% TO hy2%
linez hx1%, sy1%, hx1%, i%, 2, tc%
linez hx2%, sy1%, hx2%, i%, 2, tc%
linez hx1%, i%, hx2%, i%, 2, tc%
LINE (hx1% + 1, i%)-(hx2% - 1, i%), 0
SOUND f% / ((i% MOD 2) + 1), tt%
NEXT

'////////////////////Writing text
tc% = (tc% + 1) MOD 16
cha = hx1% + 5
chb = dy1% + 1
chpx = FIX((hx2% - hx1% - 8) / 10)
chpy = FIX((hy2% - hy1% - 28) / 15)
FOR i% = 1 TO LEN(t$)
IF FIX(i% / chpx) > chpy THEN EXIT FOR
ag$ = MID$(t$, i%, 1)
cx1% = cha + (i% MOD chpx) * 10
cy1% = chb + FIX(i% / chpx) * 15
click2
char ag$, cx1%, cy1%, 2, 1, tc%
ff = f% / ((i% MOD 2) + 1)
IF ff > f% THEN ff = f%
SOUND ff, tt%
NEXT

'/////////////////////Creating inputbox
IF ib% <> 0 THEN
ibchs$ = ""
cy1% = cy1% + 19
cx1% = hx1% + 5
cy2% = hy2% - 25
cx2% = hx2% - 5
ibc% = (c% + 10) MOD 16
linez cx1%, cy1%, cx2%, cy1%, 4, ibc%
linez cx2%, cy1%, cx2%, cy2%, 4, ibc%
linez cx2%, cy2%, cx1%, cy2%, 4, ibc%
linez cx1%, cy2%, cx1%, cy1%, 4, ibc%
ibc% = (ibc% + 1) MOD 16
chx1% = cx1% + 3
chy1% = cy1% + 3
chpx% = FIX((cx2% - cx1% - 6) / 9)
chpy% = FIX((cy2% - cy1% - 6) / 15)
END IF

'/////////////////////Creating butt%ons
smenu1: tc% = (c% + 4) MOD 16
uc% = (c% + 5) MOD 16
vc% = (c% + 6) MOD 16
inbx = hx1% + 5
by1% = hy2% - 20
by2% = hy2% - 5
FOR i% = 1 TO but
bx1% = inbx
xxx1(i% - 1) = bx1%
bx2% = inbx + ebs%
LINE (bx1% + 1, by1%)-(bx2% - 1, by1%), tc%
LINE (bx1%, by1% + 1)-(bx1%, by2% - 1), tc%
linez bx2%, by1% + 1, bx2%, by2% - 1, 2, uc%
linez bx1% + 1, by2%, bx2% - 1, by2%, 2, uc%
tx1% = bx1% + 3
ty1% = by1% + 3
inbx = bx2% + 5
word button$(i% - 1), tx1%, ty1%, 2, 1, vc%, 9, 21000, 0
NEXT

'////////////////////Asking user for input
IF ib% <> 0 THEN
ch% = 0
k$ = ""
DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)
k$ = INKEY$
 
IF k$ = CHR$(backspc) THEN
  ch% = ch% - 1
  chx% = chx1% + (ch% MOD chpx%) * 9
  chy% = chy1% + FIX(ch% / chpx%) * 15
  rub chx%, chy%, chx% + 9, chy% + 12
  ibchs$ = LEFT$(ibchs$, LEN(ibchs$) - 1)
  click2
END IF


IF k$ <> "" AND k$ <> CHR$(backspc) THEN
  az = ASC(k$)
  IF az > 31 AND az < 127 THEN
  chx% = chx1% + (ch% MOD chpx%) * 9
  chy% = chy1% + FIX(ch% / chpx%) * 15
  IF FIX(ch% / chpx%) <= chpy% THEN
  char UCASE$(k$), chx%, chy%, 1, 1, ibc%
  click2
  ch% = ch% + 1
  ibchs$ = ibchs$ + k$
  END IF
  END IF
END IF
LOOP
IF k$ = CHR$(27) THEN
GOSUB smenuc
ee$ = "esc"
GOTO smenue
END IF
END IF

'//////////////////////////Asking user
IF ibchs$ <> "" THEN ibchs$ = ibchs$ + ":"
wc% = (c% + 7) MOD 16
bpos% = 1
bx1% = xxx1(bpos% - 1) + 1
bx2% = bx1% + ebs% - 1
by1% = hy2% - 19
by2% = hy2% - 5
ty1% = hy2% - 17
pnt bx1%, by1%, bx2%, by2%, wc%
tx1% = bx1% + 2
word button$(bpos% - 1), tx1%, ty1%, 2, 1, vc%, 9, 21000, 0

inst = 0
by1% = by1 + 1
ty1% = by1 + 2
by2% = by2 - 1
DO
k$ = INKEY$

SELECT CASE k$
CASE CHR$(0) + CHR$(right)
GOSUB smenu2
bpos% = bpos% + 1
IF bpos% > but THEN bpos% = 1
GOSUB smenu3
click3
CASE CHR$(0) + CHR$(left)
GOSUB smenu2
bpos% = bpos% - 1
IF bpos% < 1 THEN bpos% = but
GOSUB smenu3
click3
CASE CHR$(27)
GOSUB smenuc
ee$ = "esc"
click1
CASE CHR$(enter)
click1
GOSUB smenuc
ee$ = ibchs$ + button$(bpos% - 1)
CASE ELSE
END SELECT
IF ee$ <> "" THEN GOTO smenue
pl1% = FIX(inst / 1000)
pl2% = FIX(pl1% / 2)
pl3% = FIX(pl1% / 3)
IF bc% = 1 THEN PALETTE wc%, 65536 * pl1% + 256 * pl2% + pl3%
IF bc% = 2 THEN PALETTE wc%, 65536 * pl3% + 256 * pl1% + pl2%
IF bc% = 3 THEN PALETTE wc%, 65536 * pl2% + 256 * pl3% + pl1%
IF inst >= 62000 THEN inst = 0
inst = inst + tadd%
LOOP

smenu2: bx1% = xxx1(bpos% - 1) + 1
bx2% = bx1% + ebs% - 1
by1% = hy2% - 19
by2% = hy2% - 5
ty1% = hy2% - 17
pnt bx1%, by1%, bx2%, by2%, 0
tx1% = bx1% + 2
word button$(bpos% - 1), tx1%, ty1%, 2, 1, vc%, 9, 21000, 0
RETURN
smenu3: bx1% = xxx1(bpos% - 1) + 1
bx2% = bx1% + ebs% - 1
pnt bx1%, by1%, bx2%, by2%, wc%
tx1% = bx1% + 2
word button$(bpos% - 1), tx1%, ty1%, 2, 1, vc%, 9, 21000, 0
RETURN
smenuc: tc% = (c% + 2) MOD 16
xx1% = hx1% - 5
xx2% = hx2% + 5
yy2% = hy2% + 2
yy1% = hy1% - 2
FOR i% = yy2% TO yy1% STEP -1
linez xx1%, i%, xx2%, i%, 3, tc%
linez xx1%, i%, xx2%, i%, 2, 0
ff = f% / ((i% MOD 2) + 2)
IF ff > f% THEN ff = f%
SOUND ff, tt%
NEXT
LINE (xx1%, yy1% - 1)-(xx2%, yy1% - 1), 0
RETURN
smenue: menu$ = ee$
END FUNCTION

FUNCTION nospc$ (s AS STRING)
add$ = ""
FOR i% = 1 TO LEN(s$)
a$ = MID$(s$, i%, 1)
IF a$ <> " " THEN add$ = add$ + a$
NEXT
nospc$ = add$
END FUNCTION

SUB nullify
SHARED spread$, userobj$, return$, tools$, objs1$
spread$ = ""
userobj$ = ""
return$ = ""
tools$ = ""
objs1$ = ""
END SUB

FUNCTION numdirs
SHELL "dir >ram.pth"
a = FREEFILE
OPEN "I", #a, "ram.pth"
FOR i = 1 TO 2 STEP 0
IF EOF(a) THEN GOTO numdirs
LINE INPUT #a, z$
IF MID$(z$, 14, 5) = "<DIR>" THEN c = c + 1
NEXT
numdirs: CLOSE #a
KILL "ram.pth"
numdirs = c - 2
END FUNCTION

FUNCTION numfiles (s AS STRING)
SHELL "dir *." + s$ + ">ram.pth"
a = FREEFILE
OPEN "I", #a, "ram.pth"
FOR i = 1 TO 2 STEP 0
IF EOF(a) THEN GOTO numfiles
LINE INPUT #a, z$
IF RIGHT$(z$, 5) = "bytes" THEN GOTO numfiles
NEXT
numfiles: CLOSE #a
KILL "ram.pth"
numfiles = VAL(LEFT$(z$, 9))
END FUNCTION

SUB obj1
SHARED objs1$
click1
sobj1s: dc$ = objs1$
tzol% = 1
linex 540, 1, 640, 1, 3, 2
linex 540, 50, 640, 50, 3, 2
liney 540, 0, 540, 50, 3, 2
liney 639, 0, 639, 50, 3, 2
linex 0, 440, 250, 440, 3, 2
linex 0, 478, 250, 478, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
linex 250, 440, 540, 440, 3, 2
linex 250, 478, 540, 478, 3, 2
liney 540, 440, 540, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
status "SELECTING OBJECTS.", 1
GOSUB sobj12

DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

k$ = INKEY$

SELECT CASE k$                      '*********Setting tzol%
  CASE CHR$(0) + CHR$(down)
  GOSUB sobj13
  tzol% = tzol% + 1
  IF tzol% > 4 THEN tzol% = 1
  GOSUB sobj12
  CASE CHR$(0) + CHR$(up)
  GOSUB sobj13
  tzol% = tzol% - 1
  IF tzol% < 1 THEN tzol% = 4
  GOSUB sobj12
  CASE ELSE
END SELECT


LOOP



IF k$ = CHR$(27) THEN
  status "OBJECT NOT SELECTED.", 1
  obj1s$ = dc$
END IF
IF k$ = CHR$(enter) THEN
  status "OBJECT SELECTED - " + UCASE$(obj1s$), 1
  rub 254, 444, 536, 476

      '-------------------------Drawing objectss
      uzx = 256: uzy = 446
      uzx1 = 256
      SELECT CASE obj1s$

      
      CASE "trees"
      obj1p "trees:1", uzx1, uzy
      uzx1 = uzx + 21
      obj1p "trees:2", uzx1, uzy
      uzx1 = uzx1 + 21
      obj1p "trees:3", uzx1, uzy
      uzx1 = uzx1 + 21                                            'grass
      obj1p "trees:4", uzx1, uzy
      tls% = 4

    
      CASE "circuit symbols"
      obj1p "circuitsymbols:1", uzx1, uzy
      uzx1 = uzx + 21                                      'cell
      obj1p "circuitsymbols:2", uzx1, uzy
      uzx1 = uzx1 + 21                                 'key on
      obj1p "circuitsymbols:3", uzx1, uzy
      uzx1 = uzx1 + 21                                'key off
      obj1p "circuitsymbols:4", uzx1, uzy
      uzx1 = uzx1 + 21                                 'switch
      obj1p "circuitsymbols:5", uzx1, uzy
      uzx1 = uzx1 + 21
      obj1p "circuitsymbols:6", uzx1, uzy
      uzx1 = uzx1 + 21
      obj1p "circuitsymbols:7", uzx1, uzy
      uzx1 = uzx1 + 21                                   'ac mains
      obj1p "circuitsymbols:8", uzx1, uzy
      uzx1 = uzx1 + 21                                    'ohm
      obj1p "circuitsymbols:9", uzx1, uzy
      tls% = 9


      CASE "greek symbols"
      obj1p "greeksymbols:1", uzx1, uzy
      uzx1 = uzx + 21                                     'efficiency (n)
      obj1p "greeksymbols:2", uzx1, uzy
      uzx1 = uzx1 + 21                               'refractive index(u)
      obj1p "greeksymbols:3", uzx1, uzy
      uzx1 = uzx1 + 21                                    'alpha (a)
      obj1p "greeksymbols:4", uzx1, uzy
      uzx1 = uzx1 + 21                                    'beta (B)
      obj1p "greeksymbols:5", uzx1, uzy
      uzx1 = uzx1 + 21                                    'gamma(y)
      obj1p "greeksymbols:6", uzx1, uzy
      uzx1 = uzx1 + 21                                  'phi(o)
      obj1p "greeksymbols:7", uzx1, uzy
      tls% = 7

    
      CASE "other symbols"
      obj1p "othersymbols:1", uzx1, uzy
      uzx1 = uzx + 21
      obj1p "othersymbols:2", uzx1, uzy
      uzx1 = uzx1 + 21                                  'minute
      obj1p "othersymbols:3", uzx1, uzy
      uzx1 = uzx1 + 21        ' smaller than equal to
      obj1p "othersymbols:4", uzx1, uzy
      uzx1 = uzx1 + 21        'greater than equal to
      obj1p "othersymbols:5", uzx1, uzy
      uzx1 = uzx1 + 21        ' smaller than
      obj1p "othersymbols:6", uzx1, uzy
      uzx1 = uzx1 + 21        'greater than
      obj1p "othersymbols:7", uzx1, uzy
      tls% = 7
     
      CASE ELSE
      END SELECT

  tl% = 1
  GOSUB sobj15
  k$ = ""
  DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

  k$ = INKEY$

  SELECT CASE k$
  CASE CHR$(0) + CHR$(right)
  GOSUB sobj14
  tl% = tl% + 1
  IF tl% > tls% THEN tl% = 1
  GOSUB sobj15
  CASE CHR$(0) + CHR$(left)
  GOSUB sobj14
  tl% = tl% - 1
  IF tl% < 1 THEN tl% = tls%
  GOSUB sobj15
  CASE ELSE
  END SELECT

  LOOP

  IF k$ = CHR$(27) THEN GOTO sobj1s
  IF k$ = CHR$(enter) THEN
status "OBJECT SELECTED - " + UCASE$(obj1s$), 1
rub 253, 443, 547, 477


   END IF
obj1s$ = obj1s$ + ":" + STR$(tl%)
END IF
GOTO sobj11

sobj14: rx1% = uzx + (tl% - 1) * 21
ry2% = uzy + 16
rx2% = rx1% + 20
LINE (rx1%, uzy - 1)-(rx1% + 20, uzy), 0
LINE (rx1%, uzy)-(rx1% + 20, uzy - 1), 0
LINE -(rx1% + 20, uzy + 16), 0
LINE -(rx1%, uzy + 16), 0
LINE -(rx1%, uzy - 1), 0
RETURN

sobj15: rx1% = uzx + (tl% - 1) * 21
ry2% = uzy + 16
rx2% = rx1% + 20
LINE (rx1%, uzy - 1)-(rx1% + 20, uzy - 1), 15
LINE -(rx1% + 20, uzy + 16), 15
LINE -(rx1%, uzy + 16), 15
LINE -(rx1%, uzy - 1), 15
RETURN

sobj12: SELECT CASE tzol%         '********Setting obj1$
CASE 1
obj1s$ = "trees"
CASE 2
obj1s$ = "circuit symbols"
CASE 3
obj1s$ = "greek symbols"
CASE 4
obj1s$ = "other symbols"
CASE ELSE
END SELECT


r% = 5 + FIX((tzol% - 1) / 2) * 20
c% = (tzol% - 1) MOD 2
c1% = 543 + (c% * 45)
r1% = r% + 19
LINE (c1%, r%)-(c1% + 40, r%), 15
LINE -(c1% + 40, r1%), 15
LINE -(c1%, r1%), 15
LINE -(c1%, r%), 15
status "CHOOSING OBJECT - " + UCASE$(obj1s$), 1
RETURN
sobj13: r% = 5 + FIX((tzol% - 1) / 2) * 20
c% = (tzol% - 1) MOD 2
c1% = 543 + (c% * 45)
r1% = r% + 19
LINE (c1%, r%)-(c1% + 40, r%), 0
LINE -(c1% + 40, r1%), 0
LINE -(c1%, r1%), 0
LINE -(c1%, r%), 0
RETURN

sobj11: rub 252, 442, 538, 478
linex 540, 1, 640, 1, 3, 14
linex 540, 50, 640, 50, 3, 14
liney 540, 0, 540, 50, 3, 14
liney 639, 0, 639, 50, 3, 14
linex 0, 440, 250, 440, 3, 14
linex 0, 479, 250, 479, 3, 14
liney 1, 440, 1, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
linex 250, 440, 540, 440, 3, 14
linex 250, 479, 540, 479, 3, 14
liney 540, 440, 540, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
status "", 0
GOSUB sobj13
zzz$ = obj1s$
nullify
obj1s$ = zzz$
END SUB

SUB obj1p (s AS STRING, x AS SINGLE, y AS SINGLE)
zx1 = x: zy = y
s1$ = nospc$(s$)
SELECT CASE s$
CASE "trees:1"
      FOR i = 3 TO 17 STEP 2                              'pine
      LINE (zx1 + 10, zy)-(zx1 + i, zy + 6), 2
      NEXT
      FOR i = 2 TO 8 STEP 2
      LINE (zx1 + 8, zy + 6)-(zx1 + i, zy + 10.5), 2
      NEXT
      FOR i = 8 TO 12 STEP 2
      LINE (zx1 + i, zy + 6)-(zx1 + i, zy + 10.5), 2
      NEXT
      FOR i = 12 TO 18 STEP 2
      LINE (zx1 + 12, zy + 6)-(zx1 + i, zy + 10.5), 2
      NEXT
      FOR i = 8 TO 12
      LINE (zx1 + i, zy + 10.5)-(zx1 + i, zy + 14), 6
      NEXT

CASE "trees:2"
      LINE (zx1 + 12, zy + 1)-(zx1 + 8, zy + .5), 2      'typical
      LINE -(zx1 + 5, zy + 2), 2
      LINE -(zx1 + 1, zy + 3.5), 2
      LINE -(zx1 + 3, zy + 6.5), 2
      LINE -(zx1 + 2, zy + 8), 2
      LINE -(zx1 + 5, zy + 9.5), 2
      LINE -(zx1 + 8, zy + 9), 2
      LINE -(zx1 + 12, zy + 9), 2
      LINE -(zx1 + 13, zy + 9.5), 2
      LINE -(zx1 + 14, zy + 9), 2
      LINE -(zx1 + 18, zy + 6), 2
      LINE -(zx1 + 17, zy + 4), 2
      LINE -(zx1 + 18, zy + 2), 2
      LINE -(zx1 + 16, zy + 1), 2
      LINE -(zx1 + 12, zy + 1.5), 2
      FOR i = 8 TO 12
      LINE (zx1 + i, zy + 9)-(zx1 + i, zy + 14), 6
      NEXT

CASE "trees:3"
      FOR i = 8 TO 12                                        'coniferous
      LINE (zx1 + 10, zy + 1)-(zx1 + i, zy + 14), 6
      NEXT
      LINE (zx1 + 10, zy)-(zx1 + 4, zy + 4.5), 2
      LINE (zx1 + 10, zy)-(zx1 + 16, zy + 4.5), 2
      LINE (zx1 + 10, zy + 4.5)-(zx1 + 4, zy + 7.5), 2
      LINE (zx1 + 10, zy + 4.5)-(zx1 + 16, zy + 7.5), 2
      LINE (zx1 + 10, zy + 7.5)-(zx1 + 4, zy + 10.5), 2
      LINE (zx1 + 10, zy + 7.5)-(zx1 + 16, zy + 10.5), 2

CASE "trees:4"
      LINE (zx1 + 10, zy + 14)-(zx1 + 14, zy + 6), 2
      LINE (zx1 + 14, zy + 6)-(zx1 + 18, zy + 1.5), 2
      LINE (zx1 + 10, zy + 14)-(zx1 + 10, zy + 4.5), 2
      LINE -(zx1 + 12, zy + 3), 2
      LINE (zx1 + 10, zy + 14)-(zx1 + 8, zy + 4.5), 2
      LINE -(zx1 + 6, zy + 1.5), 2
      LINE (zx1 + 10, zy + 14)-(zx1 + 2, zy + 3), 2

CASE "circuitsymbols:1"
      LINE (zx1 + 1, zy + 6)-(zx1 + 2, zy + 6), 4        'resistance
      LINE -(zx1 + 4, zy + 3), 4
      LINE -(zx1 + 6, zy + 9), 4
      LINE -(zx1 + 8, zy + 3), 4
      LINE -(zx1 + 10, zy + 9), 4
      LINE -(zx1 + 12, zy + 3), 4
      LINE -(zx1 + 14, zy + 9), 4
      LINE -(zx1 + 16, zy + 3), 4
      LINE -(zx1 + 18, zy + 6), 4
      LINE -(zx1 + 19, zy + 6), 4

CASE "circuitsymbols:2"                                     'cell
      LINE (zx1 + 1, zy + 7.5)-(zx1 + 8, zy + 7.5), 4
      LINE (zx1 + 8, zy + 1.5)-(zx1 + 8, zy + 13.5), 4
      LINE (zx1 + 10, zy + 4.5)-(zx1 + 10, zy + 10.5), 4
      LINE (zx1 + 10, zy + 7.5)-(zx1 + 19, zy + 7.5), 4

CASE "circuitsymbols:3"                               'key on
      LINE (zx1 + 1, zy + 7.5)-(zx1 + 6, zy + 7.5), 4
      LINE (zx1 + 6, zy + 4.5)-(zx1 + 6, zy + 10.5), 4
      LINE (zx1 + 6, zy + 4.5)-(zx1 + 8, zy + 1.5), 4
      LINE (zx1 + 6, zy + 10.5)-(zx1 + 8, zy + 13.5), 4
      LINE (zx1 + 12, zy + 1.5)-(zx1 + 14, zy + 4.5), 4
      LINE (zx1 + 12, zy + 13.5)-(zx1 + 14, zy + 10.5), 4
      LINE (zx1 + 14, zy + 4.5)-(zx1 + 14, zy + 10.5), 4
      LINE (zx1 + 14, zy + 7.5)-(zx1 + 19, zy + 7.5), 4
      FOR i = 6 TO 9 STEP .5
      LINE (zx1 + 8, zy + i)-(zx1 + 12, zy + i), 5
      NEXT

CASE "circuitsymbols:4"
      LINE (zx1 + 1, zy + 7.5)-(zx1 + 6, zy + 7.5), 4    'key off
      LINE (zx1 + 6, zy + 4.5)-(zx1 + 6, zy + 10.5), 4
      LINE (zx1 + 6, zy + 4.5)-(zx1 + 8, zy + 1.5), 4
      LINE (zx1 + 6, zy + 10.5)-(zx1 + 8, zy + 13.5), 4
      LINE (zx1 + 12, zy + 1.5)-(zx1 + 14, zy + 4.5), 4
      LINE (zx1 + 12, zy + 13.5)-(zx1 + 14, zy + 10.5), 4
      LINE (zx1 + 14, zy + 4.5)-(zx1 + 14, zy + 10.5), 4
      LINE (zx1 + 14, zy + 7.5)-(zx1 + 19, zy + 7.5), 4

CASE "circiutsymbols:5"
      LINE (zx1 + 1, zy + 7.5)-(zx1 + 6, zy + 7.5), 4   'switch
      LINE -(zx1 + 14, zy + 3), 4
      LINE (zx1 + 12, zy + 7.5)-(zx1 + 19, zy + 19), 4

CASE "circuitsymbols:6"
      LINE (zx1 + 1, zy + 6)-(zx1 + 2, zy + 6), 4        'bulb
      LINE -(zx1 + 4, zy + 3), 4
      LINE -(zx1 + 6, zy + 9), 4
      LINE -(zx1 + 8, zy + 3), 4
      LINE -(zx1 + 10, zy + 9), 4
      LINE -(zx1 + 12, zy + 3), 4
      LINE -(zx1 + 14, zy + 9), 4
      LINE -(zx1 + 16, zy + 3), 4
      LINE -(zx1 + 18, zy + 6), 4
      LINE -(zx1 + 19, zy + 6), 4
      CIRCLE (zx1 + 10, zy + 7.5), 9, 5, , , .75

CASE "circuitsymbols:7"
      LINE (zx1 + 1, zy + 6)-(zx1 + 2, zy + 6), 4        'rheostat
      LINE -(zx1 + 4, zy + 3), 4
      LINE -(zx1 + 6, zy + 9), 4
      LINE -(zx1 + 8, zy + 3), 4
      LINE -(zx1 + 10, zy + 9), 4
      LINE -(zx1 + 12, zy + 3), 4
      LINE -(zx1 + 14, zy + 9), 4
      LINE -(zx1 + 16, zy + 3), 4
      LINE -(zx1 + 18, zy + 6), 4
      LINE -(zx1 + 19, zy + 6), 4
      LINE (zx1 + 10, zy + 1)-(zx1 + 19, zy + 1), 5
      LINE (zx1 + 10, zy + 1)-(zx1 + 10, zy + 4.5), 5
      LINE -(zx1 + 12, zy + 3), 5
      LINE (zx1 + 10, zy + 4.5)-(zx1 + 8, zy + 3), 5

CASE "circuitsymbols:8"
      LINE (zx1 + 1, zy + 7.5)-(zx1 + 4, zy + 7.5), 4       'ac mains
      LINE (zx1 + 16, zy + 7.5)-(zx1 + 19, zy + 7.5), 4
      CIRCLE (10, 7.5), 6, 5
      LINE (zx1 + 6, zy + 7.5)-(zx1 + 8, zy + 5.5), 6
      LINE -(zx1 + 12, zy + 9.5), 6
      LINE -(zx1 + 14, zy + 7.5), 6

CASE "cicuitsymbols:9"
      LINE (zx1 + 4, zy + 12)-(zx1 + 8, zy + 12), 4   'ohm
      LINE -(zx1 + 4, zy + 7.5), 4
      LINE -(zx1 + 4, zy + 4.5), 4
      LINE -(zx1 + 8, zy + 1.5), 4
      LINE -(zx1 + 12, zy + 1.5), 4
      LINE -(zx1 + 16, zy + 4.5), 4
      LINE -(zx1 + 16, zy + 7.5), 4
      LINE -(zx1 + 12, zy + 12), 4
      LINE -(zx1 + 16, zy + 12), 4

CASE "greeksymbols:1"
      CIRCLE (10, 7.5), 4.5, 3, , , 1.5                 'theta (0)
      LINE (zx1 + 7, zy + 7.5)-(zx1 + 13, zy + 7.5), 3

CASE "greeksymbols:2"
      LINE (zx1 + 4, zy + 1.5)-(zx1 + 4, zy + 9), 3    'efficiency(n)
      LINE (zx1 + 4, zy + 3)-(zx1 + 6, zy + 1.5), 3
      LINE -(zx1 + 8, zy + 1.5), 3
      LINE -(zx1 + 10, zy + 3), 3
      LINE -(zx1 + 12, zy + 14), 3

CASE "greeksymbols:3"
                                             'refractive index(u)
      LINE (zx1 + 2, zy + 14)-(zx1 + 4, zy + 3), 3
      LINE -(zx1 + 4, zy + 7.5), 3
      LINE -(zx1 + 6, zy + 9), 3
      LINE -(zx1 + 10, zy + 9), 3
      LINE -(zx1 + 12, zy + 7.5), 3
      LINE -(zx1 + 12, zy + 3), 3
      LINE -(zx1 + 12, zy + 9), 3
      LINE -(zx1 + 13, zy + 10.5), 3

CASE "greeksymbols:4"
                                                   'alpha (a)
      LINE (zx1 + 12.5, zy + 2.5)-(zx1 + 12, zy + 3), 3
      LINE -(zx1 + 10, zy + 1.5), 3
      LINE -(zx1 + 6, zy + 1.5), 3
      LINE -(zx1 + 4, zy + 3), 3
      LINE -(zx1 + 4, zy + 7.5), 3
      LINE -(zx1 + 6, zy + 9), 3
      LINE -(zx1 + 10, zy + 9), 3
      LINE -(zx1 + 12, zy + 7.5), 3
      LINE -(zx1 + 12, zy + 3), 3
      LINE (zx1 + 12, zy + 7.5)-(zx1 + 13, zy + 10.5), 3

CASE "greeksymbols:5"
                                                       'beta (B)
      LINE (zx1 + 4, zy + 14)-(zx1 + 4, zy + 3), 3
      LINE -(zx1 + 6, zy + 1.5), 3
      LINE -(zx1 + 10, zy + 1.5), 3
      LINE -(zx1 + 12, zy + 3), 3
      LINE -(zx1 + 12, zy + 4.5), 3
      LINE -(zx1 + 10, zy + 6), 3
      LINE -(zx1 + 12, zy + 7.5), 3
      LINE -(zx1 + 12, zy + 9), 3
      LINE -(zx1 + 10, zy + 10.5), 3
      LINE -(zx1 + 4, zy + 10.5), 3
      LINE (zx1 + 4, zy + 6)-(zx1 + 10, zy + 6), 3

CASE "greeksymbols:6"
                                                  'gamma(y)
      LINE (zx1 + 4, zy + 1.5)-(zx1 + 10, zy + 6), 3
      LINE -(zx1 + 11, zy + 14), 3
      LINE -(zx1 + 9, zy + 14), 3
      LINE -(zx1 + 10, zy + 6), 3
      LINE -(zx1 + 16, zy + 1.5), 3
     
CASE "greeksymbols:7"
                                              'phi(o)
      CIRCLE (zx1 + 10, zy + 7.5), 3, 3
      LINE (zx1 + 10, zy + 1)-(zx1 + 10, zy + 14), 3

CASE "othersymbols:1"
      LINE (zx1 + 16, zy + 1)-(zx1 + 2, zy + 13), 3      'angle
      LINE -(zx1 + 16, zy + 13), 3

CASE "othersymbols:2"
      CIRCLE (zx1 + 16, zy + 4), 3                  'degree

CASE "othersymbols:3"
                                                  'minute
      LINE (zx1 + 14, zy + 4.5)-(zx1 + 18, zy + 1), 3

CASE "othersymbols:4"
                         ' smaller than equal to
      LINE (zx1 + 14, zy + 1.5)-(zx1 + 4, zy + 7.5), 3
      LINE -(zx1 + 14, zy + 13.5), 3
      LINE (zx1 + 2, zy + 9)-(zx1 + 10, zy + 13.5), 3

CASE "othersymbols:5"
                        'greater than equal to
      LINE (zx1 + 4, zy + 1.5)-(zx1 + 14, zy + 7.5), 3
      LINE -(zx1 + 4, zy + 13.5), 3
      LINE (zx1 = 8, zy + 13.5)-(zx1 + 16, zy + 9), 3

CASE "othersymbols:6"
                       ' smaller than
      LINE (zx1 + 14, zy + 1.5)-(zx1 + 4, zy + 7.5), 3
      LINE -(zx1 + 14, zy + 13.5), 3

CASE "othersymbols:7"
                       'greater than
      LINE (zx1 + 4, zy + 1.5)-(zx1 + 14, zy + 7.5), 3
      LINE -(zx1 + 4, zy + 13.5), 3

CASE ELSE
END SELECT
END SUB

SUB obj2
SHARED userobj$
click1
SHELL "cd OBJ"
fi = numfiles("obj")
SHELL "cd.."
IF fi = 0 THEN GOTO sobj2ee
sobj2s: dc$ = userobj$
tzol% = 1

linex 540, 50, 640, 50, 3, 2
linex 540, 380, 640, 380, 3, 2
liney 540, 50, 540, 380, 3, 2
liney 639, 50, 639, 380, 3, 2
linex 0, 440, 250, 440, 3, 2
linex 0, 478, 250, 478, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
linex 250, 440, 540, 440, 3, 2
linex 250, 478, 540, 478, 3, 2
liney 540, 440, 540, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
status "SELECTING USER OBJECTS.", 1

SHELL "cd OBJ"                                'storing files
fls$ = files$("obj", ":")
DIM objfile$(fi)
scp = 0
a$ = ""
FOR i = 1 TO LEN(fls$)
b$ = MID$(fls$, i, 1)
IF (b$ = ":" AND a$ <> "") THEN
  objfile$(scp) = a$
  scp = scp + 1
  a$ = ""
END IF
NEXT

DIM names$(fi)                'generating names (chr$(27))
scp = 0
FOR i = 1 TO fi
b$ = objfile$(i)
IF b$ <> "" THEN
      a$ = ""
      z = FREEFILE
      OPEN "B", #z, b$
      p = 1
      DO UNTIL k$ = CHR$(27)
      SEEK #z, p
      p = p + 1
      k$ = INPUT$(1, #z)
      a$ = a$ + k$
      LOOP
      names$(scp) = LEFT$(a$, LEN(a$) - 1)
      scp = scp + 1
END IF
NEXT

tlspy = FIX(320 / 25)                         'drawing tools
tlsppg = tlspy * 2
IF tlsppg > fi THEN vbn = fi ELSE vbn = tlsppg
FOR i = 1 TO vbn
r = (55 + FIX((i - 1) / 2) * 25) + 1
c = (i - 1) MOD 2
c1 = (543 + (c * 45)) + 1
r1 = r + 19
obj2d objfile$(i - 1), c1, r, 43, 17
NEXT

GOSUB sobj22

DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

k$ = INKEY$

SELECT CASE k$                      '*********Setting tzol%
  CASE CHR$(0) + CHR$(down)
  GOSUB sobj23
  tzol% = tzol% + 1
  IF tzol% > fi THEN tzol% = fi
  GOSUB sobj22
  CASE CHR$(0) + CHR$(up)
  GOSUB sobj23
  tzol% = tzol% - 1
  IF tzol% < 1 THEN tzol% = 1
  GOSUB sobj22
  CASE ELSE
END SELECT


LOOP



IF k$ = CHR$(27) THEN
  status "USER OBJECT NOT SELECTED.", 1
  userobj$ = dc$
END IF
IF k$ = CHR$(enter) THEN
  status "OBJECT SELECTED - " + UCASE$(userobj$), 1
  rub 254, 444, 536, 476
END IF
GOTO sobj21

sobj22: userobj$ = name$(tzol% - 1)   '********Setting userobj$
IF (tzol% MOD tlsppg = 0) THEN
   rub 543, 54, 637, 377
   ts = tzol% - tlsppg + 1
   IF ts < 1 THEN ts = 1
   FOR i = ts TO tzol%
   r = (55 + FIX((i - ts) / 2) * 25) + 1
   c = (i - ts) MOD 2
   c1 = (543 + (c * 45)) + 1
   r1 = r + 19
   obj2d objfile$(i - 1), c1, r, 43, 17
   NEXT
END IF
r% = 55 + FIX((tzol% - 1) / 2) * 25
c% = (tzol% - 1) MOD 2
c1% = 543 + (c% * 45)
r1% = r% + 19
LINE (c1%, r%)-(c1% + 40, r%), 15
LINE -(c1% + 40, r1%), 15
LINE -(c1%, r1%), 15
LINE -(c1%, r%), 15
status "CHOOSING USER OBJECT - " + UCASE$(userobj$), 1
RETURN
sobj23: r% = 55 + FIX((tzol% - 1) / 2) * 20
c% = (tzol% - 1) MOD 2
c1% = 543 + (c% * 45)
r1% = r% + 19
LINE (c1%, r%)-(c1% + 40, r%), 0
LINE -(c1% + 40, r1%), 0
LINE -(c1%, r1%), 0
LINE -(c1%, r%), 0
RETURN

sobj21: rub 54, 543, 637, 377
linex 540, 50, 640, 50, 3, 14
linex 540, 380, 640, 380, 3, 14
liney 540, 50, 540, 380, 3, 14
liney 639, 50, 639, 380, 3, 14
linex 0, 440, 250, 440, 3, 14
linex 0, 479, 250, 479, 3, 14
liney 1, 440, 1, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
linex 250, 440, 540, 440, 3, 14
linex 250, 479, 540, 479, 3, 14
liney 540, 440, 540, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
status "", 0
GOSUB sobj23

sobj2ee: nullify
userobj$ = objfile$(tzol% - 1)
END SUB

SUB obj2d (f$, x AS SINGLE, y AS SINGLE, rx AS SINGLE, ry AS SINGLE)
VIEW SCREEN (42, 22)-(538, 438)'------------Possible error
as1 = 2 * rx
as2 = 2 * ry
z = FREEFILE
OPEN "B", #z, f$
r = 0: c = 0: cl = 0: i = 1
DO UNTIL k$ = CHR$(27)
SEEK #z, i
k$ = INPUT$(1, #1)
i = i + 1
LOOP

DO UNTIL i = LOF(z)
SEEK #z, i
a1$ = INPUT$(1, #z)
IF a1$ = CHR$(254) THEN
i = i + 1
SEEK #z, i
a1$ = INPUT$(1, #z)
r = r + ASC(a1$)
i = i + 1
SEEK #z, i
a1$ = INPUT$(1, #z)
r = r + ASC(a1$)
c = 0
ELSEIF a1$ = CHR$(253) THEN
i = i + 1
SEEK #z, i
a1$ = INPUT$(1, #z)
c = c + ASC(a1$)
ELSE
cl = ASC(a1$)
LINE (x + (c / as1), y + (r / as2))-(x + (c / as1), y + (r / as2)), cl
i = i + 1
END IF
LOOP
CLOSE #z
VIEW SCREEN (0, 0)-(640, 480)
END SUB

SUB obj2p (f$, x AS SINGLE, y AS SINGLE)
SHARED imagex, imagey
objimgsize f$
cx = imagex / 18
cy = imagey / 13
obj2d f$, x, y, cx, cy
END SUB

SUB objimgsize (f$)
SHARED imagex, imagey
z = FREEFILE
OPEN "B", #z, f$
r = 0
c = 0
i = 1
DO UNTIL i = LOF(z)
SEEK #z, i
a1$ = INPUT$(1, #1)
IF a1$ = CHR$(254) THEN
i = i + 1
SEEK #z, i
a1$ = INPUT$(1, #z)
rr = rr + ASC(a1$)
i = i + 1
SEEK #z, i
a1$ = INPUT$(1, #z)
rr = rr + ASC(a1$)
cc = 0
IF rr > r THEN r = rr
ELSEIF a1$ = CHR$(253) THEN
i = i + 1
SEEK #z, i
a1$ = INPUT$(1, #z)
cc = cc + ASC(a1$)
IF cc > c THEN c = cc
ELSE
i = i + 1
END IF
LOOP
CLOSE #z
imagex = c
imagey = r
END SUB

SUB pnt (tx1 AS INTEGER, ty1 AS INTEGER, tx2 AS INTEGER, ty2 AS INTEGER, c%)
FOR i% = tx1 TO tx2
LINE (i%, ty1)-(i%, ty2), c%
NEXT
END SUB

FUNCTION position (x AS SINGLE, y AS SINGLE)
position = (y * 2 * 496) + (2 * x) + 1
END FUNCTION

FUNCTION randnum (s1 AS SINGLE, s2 AS SINGLE)
a = RND(3) * s2
randnum = s1 + (a MOD (s2 - s1 + 1))
END FUNCTION

FUNCTION realx (x AS SINGLE)
realx = x + 42
END FUNCTION

FUNCTION realy (y AS SINGLE)
realy = y + 22
END FUNCTION

SUB rub (hx1 AS INTEGER, hy1 AS INTEGER, hx2 AS INTEGER, hy2 AS INTEGER)
FOR i% = hx1 TO hx2
LINE (i%, hy1)-(i%, hy2), 0
NEXT
END SUB

SUB savedot (x, y, c%)
ips = position(x, y)
ch$ = CHR$(c% + 65)
z = FREEFILE
OPEN "B", #z, "ram.sys"
PUT #z, ips, ch$
CLOSE #z
END SUB

SUB scrarea (x1, y1, x2, y2)
SHARED screens, scxx1, scxx2, scyy1, scyy2
scxx1 = x1: scxx2 = x2: scyy1 = y1: scyy2 = y2
z = FREEFILE
FOR i% = z + 1 TO z + screens
OPEN "B", #i%, nospc$("scr" + STR$(i% - z) + ".scr")
NEXT
ps = 1
as$ = CHR$(0)
FOR i = x1 TO x2
FOR j = y1 TO y2
FOR i1% = z + 1 TO z + screens
PUT #i1%, ps, as$
NEXT
ps = ps + 1
NEXT
NEXT

FOR i% = z + 1 TO z + screens
CLOSE #i%
NEXT
END SUB

SUB scrCIRCLE (x, y, rad, c%, rat)
SHARED screens, scxx1, scyy1, scxx2, scyy2, cscreen
s$ = nospc$("scr" + STR$(cscreen) + ".scr")
OPEN "B", #z, s$
IF (x < 0 OR y < 0) THEN EXIT SUB
ax = scxx1 + x%
ay = scyy1 + y%
IF (ax > scxx2 OR ay > scyy2) THEN EXIT SUB
dx = ABS(hx1 - x)
dy = ABS(hy1 - y)

stp = 90 / r
ang = 0
FOR i = 0 TO 360 STEP stp
   IF (i MOD 91 = 0) THEN ang = 1
   radi = ((22 / 7) / 180) * ang
   ht = SIN(radi) * rad * rat
   lt = COS(radi) * rad
     SELECT CASE i
      CASE IS >= 0 AND i <= 90
       ht = ht * -1
      CASE IS > 90 AND i <= 180
       SWAP lt, ht
       lt = lt * -1
       ht = ht * -1
      CASE IS > 180 AND i <= 270
       lt = lt * -1
      CASE IS > 270
       SWAP lt, ht
      CASE ELSE
     END SELECT
ax = x + lt: ay = y + ht
ps = ax * (scyy2 - scyy1 + 1) + ay + 1
ch$ = CHR$(c%)
PUT #z, ps, ch$
NEXT
CLOSE #z
END SUB

SUB scrdot (x, y, c%)
SHARED cscreen, scxx1, scxx2, scyy1, scyy2
z = FREEFILE
s$ = nospc$("scr" + STR$(cscreen) + ".scr")
OPEN "B", #z, s$
IF (x < 0 OR y < 0) THEN EXIT SUB
ax = scxx1 + x
ay = scyy1 + y
IF (ax > scxx2 OR ay > scyy2) THEN EXIT SUB
ps = ax * (scyy2 - scyy1 + 1) + ay + 1
as$ = CHR$(c%)
PUT #z, ps, as$
CLOSE #z
END SUB

SUB scrend
SHARED screens
FOR i = 1 TO screens
a$ = nospc$("scr" + STR$(i) + ".scr")
SHELL "del " + a$
NEXT
END SUB

SUB scrgene (x1, y1, x2, y2)
SHARED screens, scyy1, scxx1
z = FREEFILE
FOR i% = z + 1 TO z + screens
OPEN "B", #i%, nospc$("scr" + STR$(i% - z) + ".scr")
NEXT

ps = 1
FOR i = x1 TO x2
FOR j = y1 TO y2
IF (i < 0) THEN EXIT FOR
cl = 0
FOR i1 = z + screens TO z + 1 STEP -1
IF (j < 0) THEN EXIT FOR
ps = ps = i * (scyy2 - scyy1 + 1) + j + 1
SEEK #i1, ps
a1$ = INPUT$(1, #i1)
IF a1$ <> "" THEN cc = ASC(a1$)
IF cc <> 0 THEN cl = cc
NEXT
LINE (i, j)-(i, j), cl
NEXT
NEXT

FOR i% = z + 1 TO z + screens
CLOSE #i%
NEXT

END SUB

SUB scrgenerate
SHARED screens, scxx1, scyy1, scxx2, scyy2
z = FREEFILE
FOR i% = z + 1 TO z + screens
OPEN "B", #i%, nospc$("scr" + STR$(i% - z) + ".scr")
NEXT

ps = 1
FOR i = scxx1 TO scxx2
FOR j = scyy1 TO scyy2
cl = 0
FOR i1 = z + screens TO z + 1 STEP -1
SEEK #i1, ps
a1$ = INPUT$(1, #i1)
IF a1$ <> "" THEN cc = ASC(a1$)
IF cc <> 0 THEN cl = cc
NEXT
LINE (i, j)-(i, j), cl
ps = ps + 1
NEXT
NEXT

FOR i% = z + 1 TO z + screens
CLOSE #i%
NEXT
END SUB

SUB scrLINE (x1, y1, x2, y2, c%)
SHARED cscreen, scxx1, scxx2, scyy1, scyy2
z = FREEFILE
s$ = nospc$("scr" + STR$(cscreen) + ".scr")

OPEN "B", #z, s$
IF (x1 < 0 OR y1 < 0 OR x2 < 0 OR y2 < 0) THEN EXIT SUB

xx = ABS(x2 - x1)
yy = ABS(y2 - y1)
FOR iz = 1 TO xx
rty = FIX((iz * yy) / xx)
ax = scxx1 + x1 + iz
ay = scyy1 + y1 + rty
IF (ax <= scxx2 OR ay <= scyy2) THEN
ps = ax * (scyy2 - scyy1 + 1) + ay + 1
as$ = CHR$(c%)
PUT #z, ps, as$
END IF
NEXT
CLOSE #z
END SUB

SUB scrlinex (hx1, hy1, hx2, hy2, t AS INTEGER, c AS INTEGER)
IF t >= 1 THEN scrLINE hx1, hy1, hx2, hy2, c
FOR i = 2 TO t
z% = FIX(i / 2)
s = i MOD 2
IF s = 0 THEN s = 1 ELSE s = -1
z% = z% * s
scrLINE hx1, hy1 + z%, hx2, hy2 + z%, c
NEXT
END SUB

SUB scrliney (hx1, hy1, hx2, hy2, t AS INTEGER, c AS INTEGER)
IF t >= 1 THEN scrLINE hx1, hy1, hx2, hy2, c
FOR i = 2 TO t
z% = FIX(i / 2)
s = i MOD 2
IF s = 0 THEN s = 1 ELSE s = -1
z% = z% * s
scrLINE hx1 + z%, hy1, hx2 + z%, hy2, c
NEXT
END SUB

SUB scrlinez (hx1, hy1, hx2, hy2, t AS INTEGER, c AS INTEGER)
IF ABS(hx1 - hx2) > ABS(hy1 - hy2) THEN scrlinex hx1, hy1, hx2, hy2, t, c ELSE scrliney hx1, hy1, hx2, hy2, t, c
END SUB

SUB scrpnt (x1, y1, x2, y2, c%)
FOR i = y1 TO y2 STEP SGN(y2 - y1)
scrLINE x1, i, x2, i, c%
NEXT
END SUB

SUB setscr (s%)
SHARED screens, cscreen
IF s% > 0 AND s% <= screens THEN cscreen = s%
END SUB

SUB setscreens (n AS INTEGER)
SHARED screens
screens = n
FOR i% = 1 TO n
s$ = nospc$("scr" + STR$(i%) + ".scr")
z = FREEFILE
OPEN "O", #z, s$
CLOSE #z
NEXT
END SUB

SUB sprdlns
SHARED spread$
click1
ssprdlnss: dc$ = spread$
tzol% = 1
linex 540, 440, 640, 440, 3, 2
linex 540, 478, 640, 478, 3, 2
liney 540, 440, 540, 480, 3, 2
liney 639, 440, 639, 480, 3, 2
linex 0, 440, 250, 440, 3, 2
linex 0, 478, 250, 478, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
status "SELECTING SPREAD LINES.", 1
GOSUB ssprdlns2

DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

k$ = INKEY$

SELECT CASE k$                      '*********Setting tzol%
  CASE CHR$(0) + CHR$(down)
  GOSUB ssprdlns3
  tzol% = tzol% + 1
  IF tzol% > 4 THEN tzol% = 1
  GOSUB ssprdlns2
  CASE CHR$(0) + CHR$(up)
  GOSUB ssprdlns3
  tzol% = tzol% - 1
  IF tzol% < 1 THEN tzol% = 4
  GOSUB ssprdlns2
  CASE ELSE
END SELECT


LOOP



IF k$ = CHR$(27) THEN
  status "SPREAD LINE NOT SELECTED.", 1
  spread$ = dc$
END IF
IF k$ = CHR$(enter) THEN
  status "SPREAD LINE SELECTED - " + UCASE$(spread$), 1
  rub 254, 444, 536, 476
END IF
GOTO ssprdlns1


ssprdlns2:     SELECT CASE tzol%
CASE 1
spread$ = "normal line"
CASE 2
spread$ = "squared line"
CASE 3
spread$ = "zizag line"
CASE 4
spread$ = "curved line"
CASE ELSE
END SELECT

r% = 442 + FIX((tzol% - 1) / 2) * 15
c% = (tzol% - 1) MOD 2
c1% = 543 + (c% * 45)
r1% = r% + 15
LINE (c1%, r%)-(c1% + 40, r%), 15
LINE -(c1% + 40, r1%), 15
LINE -(c1%, r1%), 15
LINE -(c1%, r%), 15
status "CHOOSING SPREAD LINE - " + UCASE$(spread$), 1
RETURN
ssprdlns3: r% = 442 + FIX((tzol% - 1) / 2) * 20
c% = (tzol% - 1) MOD 2
c1% = 543 + (c% * 45)
r1% = r% + 15
LINE (c1%, r%)-(c1% + 40, r%), 0
LINE -(c1% + 40, r1%), 0
LINE -(c1%, r1%), 0
LINE -(c1%, r%), 0
RETURN

ssprdlns1: rub 252, 442, 538, 478
linex 540, 440, 640, 440, 3, 14
linex 540, 478, 640, 478, 3, 14
liney 540, 440, 540, 480, 3, 14
liney 639, 440, 639, 480, 3, 14
linex 0, 440, 250, 440, 3, 14
linex 0, 479, 250, 479, 3, 14
liney 1, 440, 1, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
status "", 0
GOSUB ssprdlns3
zzz$ = spread$
nullify
spread$ = zzz$
END SUB

SUB status (s AS STRING, c%)
zz$ = UCASE$(s$)
rub 4, 442, 247, 476
FOR i% = 1 TO LEN(zz$)
IF FIX(i% / 24) > 2 THEN EXIT FOR
ag$ = UCASE$(MID$(zz$, i%, 1))
cx1% = 5 + (i% MOD 24) * 10
cy1% = 445 + FIX(i% / 24) * 15
char ag$, cx1%, cy1%, 2, 1, c%
NEXT
END SUB

SUB text
SHARED cursorx, cursory, clr%
click1
linex 540, 420, 640, 420, 3, 2
linex 540, 440, 640, 440, 3, 2
liney 540, 420, 540, 440, 3, 2
liney 639, 420, 639, 420, 3, 2
DIM ar1(1000), ar2(1000), ar3(1000), ar4(1000), ar5(1000), ar6(1000)
GET (100, 40)-(300, 100), ar1
GET (100, 100)-(300, 150), ar2
GET (100, 150)-(300, 200), ar3
GET (100, 200)-(300, 250), ar4
GET (100, 250)-(200, 300), ar5
GET (100, 300)-(300, 350), ar6
rub 100, 40, 300, 100
txt$ = menu$("TEXT", 100, 40, 300, 350, "ENTER TEXT :", "ENTER.", 21000, 0, 1, 2, 1)
tx$ = ""
FOR i = 1 TO LEN(txt$)
zx$ = MID$(txt$, i, 1)
IF zx$ = ":" THEN EXIT FOR
tx$ = tx$ + zx$
NEXT
txt$ = tx$
txt$ = UCASE$(txt$)
tx$ = ""
rub 100, 40, 300, 100
tx$ = menu$("SIZE", 100, 40, 300, 350, "ENTER SIZE (1-10):", "ACCEPT.", 21000, 0, 2, 1, 1)
size% = FIX(VAL(tx$))
rub 100, 40, 300, 100
tx$ = menu$("THICKNESS", 100, 40, 300, 350, "ENTER THICKNESS (1-10):", "SURE.", 21000, 0, 15, 2, 1)
thk% = FIX(VAL(tx$))
sp% = (size% * 6) + thk% + 1
rub 100, 40, 300, 100
tx$ = menu$("SPACING", 100, 40, 300, 350, "ENTER SPACING (1-20):", "PERFECT.", 21000, 0, 15, 2, 1)
sp% = sp% + FIX(VAL(tx$))
xc% = realx(FIX(cursorx)): yc% = realy(FIX(cursory)): cc% = clr%
rub 100, 40, 300, 100
PUT (100, 40), ar1
PUT (100, 100), ar2
PUT (100, 150), ar3
PUT (100, 200), ar4
PUT (100, 250), ar5
PUT (100, 300), ar6
ERASE ar1, ar2, ar3, ar4, ar5, ar6
word txt$, xc%, yc%, thk%, size%, cc%, sp%, 21000, 0
getit FIX(x), FIX(y), FIX((x + 6) * size%), FIX((y + 9) * size%)
linex 540, 420, 640, 420, 3, 14
linex 540, 440, 640, 440, 3, 14
liney 540, 420, 540, 440, 3, 14
liney 639, 420, 639, 420, 3, 14
status txt$ + " TEXT DRAWN", 15
END SUB

SUB tlbrush
SHARED tools$, pens%, cursorx, cursory, clr%
STATIC x, y, hx1, hy1, c%
x = cusorx
y = cursory
c% = clr%
IF (LCASE$(LEFT$(tools$, 5)) = "brush") THEN
   VIEW SCREEN (42, 22)-(538, 438)'------------Possible error   
IF (pens% = 1) THEN setscr 2 ELSE setscr 1
SELECT CASE tools$
          CASE "brush:1"
             scrCIRCLE x, y, 3, c%, 1
             getit x - 3, y - 3, x + 3, y + 3
          CASE "brush:2"
             scrLINE x, y + 9, x + 10, y, c%
             getit x, y, x + 10, y + 9
          CASE "brush:3"
             scrLINE x, y, x + 10, y + 9, c%
             getit x, y, x + 10, y + 9
          CASE "brush:4"
             scrlinex x, y + 3, x + 6, y + 3, 6, c%
             getit x, y, x + 6, y + 6
          END SELECT
END IF
VIEW SCREEN (0, 0)-(640, 480)
scrgene x - 10, y - 10, x + 10, y + 10
END SUB

SUB tlcircle (o%)
SHARED tools$, cursorx, cursory, clr%
STATIC x, y, hx1, hy1, c%
x = cursorx
y = cursory
c% = clr%
IF (LEFT$(tools$, 6) = "circle") THEN o% = 0 ELSE o% = 1
VIEW SCREEN (42, 22)-(538, 438)'------------Possible error
setscr 1
       FOR i = 1 TO 10
       scrCIRCLE x, y, i, 15, 1
       scrCIRCLE x, y, i - 1, 0, 1
       scrgene x - 10, y - 10, x + 10, y + 10
       NEXT
     scrCIRCLE x, y, 10, 0, 1
     scrpnt x - 10, y - 10, x + 10, y + 10, 0
   

     hx1 = x: hy1 = y
     DO UNTIL k$ = CHR$(enter) OR k$ = CHR$(27)
     k$ = INKEY$
     SELECT CASE k$
     CASE CHR$(0) + CHR$(up)
     GOSUB tlcircle1
     hy1 = hy1 - 1
     GOSUB tlcircle3
     GOSUB tlcircle2
     CASE CHR$(0) + CHR$(down)
     GOSUB tlcircle1
     hy1 = hy1 + 1
     GOSUB tlcircle3
     GOSUB tlcircle2
     CASE CHR$(0) + CHR$(left)
     GOSUB tlcircle1
     hx1 = hx1 - 1
     GOSUB tlcircle3
     GOSUB tlcircle2
     CASE CHR$(0) + CHR$(right)
     GOSUB tlcircle1
     hx1 = hx1 + 1
     GOSUB tlcircle3
     GOSUB tlcircle2
     END SELECT
     LOOP
     click1
     scrpnt hx1 - 10, hy1 - 10, hx1 + 10, hy1 + 10, 0
     IF k$ = CHR$(27) THEN GOSUB tlcircle1
     IF k$ = CHR$(enter) THEN GOSUB tlcircle2
     
GOTO tlcirclee
tlcircle1: dx = ABS(hx1 - x)
dy = ABS(hy1 - y)
IF (o% = 0) THEN
  rad = SQR((dx * dx) + (dy * dy))
  rat = 1
  ELSE
  rad = dx
  rat = dy / dx
END IF
stp = 90 / r
ang = 0
FOR i = 0 TO 360 STEP stp
   IF (i MOD 91 = 0) THEN ang = 1
   radi = ((22 / 7) / 180) * ang
   ht = SIN(radi) * rad * rat
   lt = COS(radi) * rad
     SELECT CASE i
      CASE IS >= 0 AND i <= 90
       ht = ht * -1
      CASE IS > 90 AND i <= 180
       SWAP lt, ht
       lt = lt * -1
       ht = ht * -1
      CASE IS > 180 AND i <= 270
       lt = lt * -1
      CASE IS > 270
       SWAP lt, ht
      CASE ELSE
     END SELECT
    xx = x + lt
    yy = y + ht
    scrpnt xx - 1, yy - 1, xx + 1, yy + 1, 0
NEXT
RETURN

tlcircle2:
setscr 2
dx = ABS(hx1 - x)
dy = ABS(hy1 - y)
IF (o% = 0) THEN
  rad = SQR((dx * dx) + (dy * dy))
  rat = 1
  ELSE
  rad = dx
  rat = dy / dx
END IF

stp = 90 / r
ang = 0
scrLINE x + r, y, x + r, y, c%
FOR i = 0 TO 360 STEP stp
   IF (i MOD 91 = 0) THEN ang = 1
   radi = ((22 / 7) / 180) * ang
   ht = SIN(radi) * rad * rat
   lt = COS(radi) * rad
     SELECT CASE i
      CASE IS >= 0 AND i <= 90
       ht = ht * -1
      CASE IS > 90 AND i <= 180
       SWAP lt, ht
       lt = lt * -1
       ht = ht * -1
      CASE IS > 180 AND i <= 270
       lt = lt * -1
      CASE IS > 270
       SWAP lt, ht
      CASE ELSE
     END SELECT
    xx = x + lt
    yy = y + ht
    scrLINE xx, yy, xx, yy, c%
    scrgene xx, yy, xx, yy
NEXT
RETURN

tlcircle3: setscr 1
scrpnt hx1 - 5, hy1 - 5, hx1 + 5, hy1 + 5, 0
scrLINE hx1, hy1 - 5, hx1, hy1 + 5, 15
scrLINE hx1 - 5, hy1, hx1 + 5, hy1, 15
RETURN




tlcirclee: dx = ABS(hx1 - x)
dy = ABS(hy1 - y)
IF dx > dy THEN radd = dx ELSE radd = dy
getit x - radd, y - radd, x + radd, y + radd
VIEW SCREEN (0, 0)-(640, 480)
END SUB

SUB tlline
SHARED tools$, cursorx, cursory, clr%
STATIC x, y
x = cursorx
y = cursory
IF (LEFT$(tools$, 4) = "line") THEN
     VIEW SCREEN (42, 22)-(538, 438)'------------Possible error 
setscr 1
   FOR i = 1 TO 10
       scrCIRCLE x, y, i, 15, 1
       scrCIRCLE x, y, i - 1, 0, 1
scrgene x - 10, y - 10, x + 10, y + 10
NEXT
     scrCIRCLE x, y, 10, 0, 1
     scrpnt x - 11, y - 11, x + 11, y + 11, 0
     scrLINE x - 5, y, x + 5, y, clr%
     scrLINE x, y - 5, x, y + 5, clr%
scrgene x - 5, y - 5, x + 5, y + 5
     

     hx1 = x: hy1 = y
     DO UNTIL k$ = CHR$(enter) OR k$ = CHR$(27)
     k$ = INKEY$
     SELECT CASE k$
     CASE CHR$(0) + CHR$(up)
     GOSUB tlline1
     hy1 = hy1 - 1
     GOSUB tlline2
     CASE CHR$(0) + CHR$(down)
     GOSUB tlline1
     hy1 = hy1 + 1
     GOSUB tlline2
     CASE CHR$(0) + CHR$(left)
     GOSUB tlline1
     hx1 = hx1 - 1
     GOSUB tlline2
     CASE CHR$(0) + CHR$(right)
     GOSUB tlline1
     hx1 = hx1 + 1
     GOSUB tlline2
     END SELECT
     LOOP
     click1
     IF k$ = CHR$(27) THEN GOSUB tlline3
     IF k$ = CHR$(enter) THEN GOSUB tlline4
END IF
GOTO tllinee
tlline1:
setscr 1
scrpnt hx1 - 5, hy1 - 5, hx1 + 5, hy1 + 5, 0
RETURN
tlline2: scrLINE hx1 - 5, hy1, hx1 + 5, hy1, clr%
scrLINE hx1, hy1 - 5, hx1, hy1 + 5, clr%
tlline3: xsign = SGN(hx1 - x)
IF xsign = 0 THEN xsign = 1
ysign = SGN(hy1 - y)
yrange = ABS(hy1 - y1)
xrange = ABS(hx1 - x)
IF xrange > yrange THEN
loopval = xrange
inval = yrange
ELSE
loopval = yrange
inval = xrange
END IF

FOR ij = 0 TO loopval
iy = FIX((inval * ij) / xrange)
IF xrange > yrange THEN
xvalue = (ij * xsign) + x
yvalue = (iy * ysign) + y
ELSE
xvalue = (iy * xsign) + x
yvalue = (ij * ysign) + y
END IF
scrpnt xvalue, yvalue, xvalue, yvalue, 0
NEXT
RETURN
tlline4:
setscr 2
scrlinez FIX(x), FIX(y), FIX(hx1), FIX(hy1), 1, clr%
xsign = SGN(hx1 - x)
IF xsign = 0 THEN xsign = 1
ysign = SGN(hy1 - y)
yrange = ABS(hy1 - y1)
xrange = ABS(hx1 - x)

IF xrange > yrange THEN
loopval = xrange
inval = yrange
ELSE
loopval = yrange
inval = xrange
END IF

FOR ij = 0 TO loopval
iy = FIX((inval * ij) / loopval)
IF xrange > yrange THEN
xvalue = (ij * xsign) + x
yvalue = (iy * ysign) + y
ELSE
xvalue = (iy * xsign) + x
yvalue = (ij * ysign) + y
END IF
getit xvalue, yvalue, xvalue, yvalue
NEXT

RETURN

tllinee: xsign = SGN(hx1 - x)
IF xsign = 0 THEN xsign = 1
ysign = SGN(hy1 - y)
yrange = ABS(hy1 - y1)
xrange = ABS(hx1 - x)

IF xrange > yrange THEN
loopval = xrange
inval = yrange
ELSE
loopval = yrange
inval = xrange
END IF

FOR ij = 0 TO loopval
iy = FIX((inval * ij) / loopval)
IF xrange > yrange THEN
xvalue = (ij * xsign) + x
yvalue = (iy * ysign) + y
ELSE
xvalue = (iy * xsign) + x
yvalue = (ij * ysign) + y
END IF
getit xvalue, yvalue, xvalue, yvalue
NEXT


VIEW SCREEN (0, 0)-(640, 480)
click3
END SUB

SUB tlpencil (fp%)
SHARED tools$, cursorx, cursory, clr%, pens%
STATIC x, y, hx1, hy1, c%
VIEW SCREEN (42, 52)-(538, 438)
tp% = VAL(RIGHT$(tools$, 1))
IF tp% = 0 THEN tp% = 10
pt% = SGN(fp% + pens%)
IF (LEFT$(tools$, 6) <> "pencil") THEN pt% = 0
cursor cursorx, cursory, clr%, tp%, pt%
scrgene cursorx - 10, cursory - 10, cursorx + 10, cursory + 10
VIEW SCREEN (0, 0)-(640, 480)
END SUB

SUB tlrectangle
SHARED tools$, cursorx, cursory, clr$
STATIC x, y
x = cursorx
y = cursory


IF (LEFT$(tools$, 9) = "rectangle") THEN
     VIEW SCREEN (42, 22)-(538, 438)'------------Possible error
setscr 1
FOR i = 1 TO 10
       scrCIRCLE x, y, i, 15, 1
       scrCIRCLE x, y, i - 1, 0, 1
scrgene x - 10, y - 10, x + 10, y + 10
NEXT
     scrCIRCLE x, y, 10, 0, 1
     scrpnt x - 10, y - 10, x + 10, y + 10, 0
     scrLINE x - 5, y, x + 5, y, clr%
     scrLINE x, y - 5, x, y + 5, clr%
   

     hx1 = x: hy1 = y
     DO UNTIL k$ = CHR$(enter) OR k$ = CHR$(27)
     k$ = INKEY$
     SELECT CASE k$
     CASE CHR$(0) + CHR$(up)
     GOSUB tlrect1
     hy1 = hy1 - 1
     IF hy1 < 0 THEN hy1 = 0
     GOSUB tlrect2
     CASE CHR$(0) + CHR$(down)
     GOSUB tlrect1
     hy1 = hy1 + 1
     IF hy1 > 386 THEN hy1 = 386
     GOSUB tlrect2
     CASE CHR$(0) + CHR$(left)
     GOSUB tlrect1
     hx1 = hx1 - 1
     IF hx1 < 0 THEN hx = 0
     GOSUB tlrect2
     CASE CHR$(0) + CHR$(right)
     GOSUB tlrect1
     hx1 = hx1 + 1
     IF hx1 > 496 THEN hx1 = 496
     GOSUB tlrect2
     END SELECT
     LOOP
     click1
     IF k$ = CHR$(27) THEN GOSUB tlrect3
     IF k$ = CHR$(enter) THEN GOSUB tlrect4
END IF
GOTO tlrecte
tlrect1: setscr 1
scrpnt hx1 - 5, hy1 - 5, hx1 + 5, hy1 + 5, 0
RETURN
tlrect2: setscr 1
scrLINE hx1 - 5, hy1, hx1 + 5, hy1, clr%
scrLINE hx1, hy1 - 5, hx1, hy1 + 5, clr%

tlrect3: IF (RIGHT$(tools$, 1) = "1") THEN
setscr 2
scrpnt x, y, hx1, y, 0
scrpnt hx1, y, hx1, hy1, 0
scrpnt x, hy1, hx1, y, 0
scrpnt x, y, x, hy1, 0
ELSE
setscr 2
scrpnt x, y, hx1, hy1, 0
END IF
RETURN
tlrect4: setscr 2
scrLINE x, y, hx1, y, clr%
scrgene x, y, hx1, y
scrLINE hx1, y, hx1, hy1, clr%
scrLINE hx1, hy1, x, hy1, clr%
scrLINE x, y, x, hy1, clr%
scrgene hx1, y, hx1, hy1
scrgene hx1, hy1, x, hy1
scrgene x, y, x, hy1

IF (RIGHT$(tools$, 1) = "2") THEN pnt realx(x), realy(y), realx(hx1), realy(hy1), clr%
RETURN

tlrecte:
IF (RIGHT$(tools$, 1) = "1") THEN
getit x, y, hx1, y
getit hx1, y, hx1, hy1
getit hx1, hy1, x, hy1
getit x, y, x, hy1
ELSE
getit x, y, hx1, hy1
END IF
VIEW SCREEN (0, 0)-(640, 480)
END SUB

SUB tlspray
SHARED tools$, pens%, cursorx, cursory, clr%
STATIC x, y, hx1, hy1, c%
x = cusorx
y = cursory
c% = clr%
IF (LCASE$(LEFT$(tools$, 5)) = "spray" AND pens% = 1) THEN
   VIEW SCREEN (42, 22)-(538, 438)'------------Possible error 
   k$ = INPUT$(1)
   IF k$ = CHR$(enter) THEN
   SELECT CASE tools$
          CASE "spray:1"
      FOR i = x + 1 TO x + 17 STEP .5
      FOR j = 1 TO 2
      a = randnum(y + 1, y + 14)
      LINE (realx(i), realy(a))-(realx(i), realy(a)), c%
      NEXT
      NEXT
      getit x, y, x + 18, y + 15
          CASE "brush:2"
      FOR i = x + 1 TO x + 7 STEP .5
      a = randnum(y + 1, y + 8)
      LINE (realx(i), realy(a))-(realx(i), realy(a)), c%
      NEXT
      getit x, y, x + 8, y + 9
          END SELECT
END IF
END IF
VIEW SCREEN (0, 0)-(640, 480)
END SUB

SUB tlsprdln
SHARED spread$, cursorx, cursory, clr%, tadd%
STATIC x, y, hx1, hy1, c%
x = cursorx
y = cursory
IF (spread$ <> "") THEN
     VIEW SCREEN (42, 22)-(538, 438)'------------Possible error
       k$ = INPUT$(1)
       IF k$ = CHR$(enter) THEN
       FOR i = 1 TO 10
       CIRCLE (realx(x), realy(y)), i, 15
       CIRCLE (realx(x), realy(y)), i - 1, 0
         FOR j = 3000 TO 5000 STEP 400
         SOUND j, .05
         NEXT
       NEXT
     CIRCLE (realx(x), realy(y)), 10, 0
     disp x - 10, y - 10, x + 10, y + 10
     status "Enter direction :", 15
     PSET (realx(x), realy(y))

tlsprd8: k$ = INKEY$
     SELECT CASE k$
     CASE CHR$(0) + CHR$(right)
     DRAW "a0"
     GOTO tlsprd7
     CASE CHR$(0) + CHR$(up)
     DRAW "a1"
     GOTO tlsprd7
     CASE CHR$(0) + CHR$(left)
     DRAW "a2"
     GOTO tlsprd7
     CASE "chr$(0)+chr$(down)"
     DRAW "a3"
     GOTO tlsprd7
     CASE ELSE
     END SELECT
     GOTO tlsprd8
tlsprd7: click1
dist = 0
DRAW nospc$("c" + STR$(clr%))
DRAW "s1"
IF spread$ = "zizag line" THEN DRAW "ta45"
IF spread$ = "curved line" THEN DRAW "ta90"
     hx1 = x: hy1 = y
     DO UNTIL k$ = CHR$(enter) OR k$ = CHR$(27)
     inst = inst + tadd%
     k$ = INKEY$
     SELECT CASE k$
     CASE CHR$(0) + CHR$(up)
     DRAW "ta5"
     CASE CHR$(0) + CHR$(down)
     DRAW "ta-5"
     CASE CHR$(0) + CHR$(left)
     DRAW "ta5"
     CASE CHR$(0) + CHR$(right)
     DRAW "ta-5"
     END SELECT
  IF inst = 500 THEN
    
     SELECT CASE spread$
    
     CASE "normal line"
     dist = dist + 1

     CASE "squared line"
     IF dist MOD 10 = 0 THEN
     stp = stp + 1
     IF stp = 4 THEN stp = 0
        SELECT CASE stp
        CASE 0
        DRAW "ta90"
        CASE 1
        DRAW "ta90"
        CASE 2
        DRAW "ta-90"
        CASE 3
        DRAW "ta-90"
        CASE ELSE
        END SELECT
      END IF
      CASE "zizag line"
      IF dist MOD 10 = 0 THEN
      stp = stp + 1
      IF stp = 2 THEN stp = 0
          SELECT CASE stp
          CASE 0
          DRAW "ta270"
          CASE 1
          DRAW "ta-90"
          END SELECT
       END IF
      CASE "curved line"
      IF dist MOD 36 = 0 THEN stp = (stp + 1) MOD 2
      IF stp = 0 THEN DRAW "ta-5"
      IF stp = 1 THEN DRAW "ta5"
      CASE ELSE
      END SELECT
   DRAW "r1"
     status "Distance : " + STR$(dist), 7
     inst = 0
     END IF

     LOOP
     getit 0, 0, 496, 396
     click1
END IF
END IF
VIEW SCREEN (0, 0)-(640, 480)
END SUB

SUB tool
SHARED tools$
click1
stools: zx = 256
zy = 446
dc$ = tools$
tzol% = 1
linex 0, 90, 40, 90, 3, 2
linex 0, 440, 40, 440, 3, 2
liney 1, 90, 1, 440, 3, 2
liney 40, 90, 40, 440, 3, 2
linex 0, 440, 250, 440, 3, 2
linex 0, 478, 250, 478, 3, 2
liney 1, 440, 1, 480, 3, 2
liney 250, 440, 250, 480, 3, 2
linex 250, 440, 540, 440, 3, 2
linex 250, 478, 540, 478, 3, 2
liney 540, 440, 540, 480, 3, 2
liney 250, 440, 25, 480, 3, 2
status "SELECTING TOOLS.", 1
GOSUB stool2

DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

k$ = INKEY$

SELECT CASE k$                      '*********Setting tzol%
  CASE CHR$(0) + CHR$(down)
  GOSUB stool3
  tzol% = tzol% + 1
  IF tzol% > 7 THEN tzol% = 1
  GOSUB stool2
  CASE CHR$(0) + CHR$(up)
  GOSUB stool3
  tzol% = tzol% - 1
  IF tzol% < 1 THEN tzol% = 8
  GOSUB stool2
  CASE ELSE
END SELECT


LOOP



IF k$ = CHR$(27) THEN
  status "TOOL NOT SELECTED.", 1
  r% = 99 + (FIX(ABS(tzol% - 1) / 2)) * 7
  c% = tzol% MOD 2
  IF c% = 0 THEN LINE (4, r%)-(16, r% + 6), 0 ELSE LINE (24, r%)-(36, r% + 6), 0
  tools$ = dc$
END IF
IF k$ = CHR$(enter) THEN
  status "TOOL SELECTED - " + UCASE$(tools$), 1
  rub 254, 444, 536, 476
 
      '-------------------------Drawing tools
      SELECT CASE tools$

      CASE "pencil"
      tx1 = zx + 10: ty1 = zy + 7
      LINE (tx1 - 1, ty1 - 1)-(tx1 + 1, ty1 - 1), 1
      LINE -(tx1 + 1, ty1 + 1), 1
      LINE -(tx1 - 1, ty1 + 1), 1
      LINE -(tx1 - 1, ty1 - 1), 1
      tx1 = tx1 + 21
      LINE (tx1 - 2, ty1 - 2)-(tx1 + 2, ty1 - 2), 1
      LINE -(tx1 + 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 - 2), 1
      tx1 = tx1 + 21
      LINE (tx1, ty1 - 2)-(tx1 + 2, ty1), 1
      LINE -(tx1, ty1 + 2), 1
      LINE -(tx1 - 2, ty1), 1
      LINE -(tx1, ty1 - 2), 1
      tx1 = tx1 + 21
      CIRCLE (tx1, ty1), 3, 1
      tx1 = tx1 + 21
      LINE (tx1 - 2, ty1 - 2)-(tx1 + 2, ty1 - 2), 1
      LINE -(tx1 + 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 - 2), 1
      LINE (tx1, ty1 - 2)-(tx1, ty1 - 5), 2
      LINE (tx1 + 2, ty1)-(tx1 + 5, ty1), 2
      LINE (tx1, ty1 + 2)-(tx1, ty1 + 5), 2
      LINE (tx1 - 2, ty1)-(tx1 - 5, ty1), 2
      tx1 = tx1 + 21
      CIRCLE (tx1, ty1), 3, 1
      LINE (tx1, ty1 - 3)-(tx1, ty1 - 5), 2
      LINE (tx1 + 3, ty1)-(tx1 + 5, ty1), 2
      LINE (tx1, ty1 + 3)-(tx1, ty1 + 5), 2
      LINE (tx1 - 3, ty1)-(tx1 - 5, ty1), 2
      tx1 = tx1 + 21
      LINE (tx1 - 2, ty1 - 2)-(tx1 + 2, ty1 - 2), 1
      LINE -(tx1 + 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 - 2), 1
      CIRCLE (tx1, ty1), 3, 2
      tx1 = tx1 + 21
      LINE (tx1 - 2, ty1 - 2)-(tx1 + 2, ty1 - 2), 1
      LINE -(tx1 + 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 - 2), 1
      LINE (tx1 - 2, ty1 - 2)-(tx1 - 5, ty1 - 5), 2
      LINE (tx1 + 2, ty1 - 2)-(tx1 + 5, ty1 - 5), 2
      LINE (tx1 + 2, ty1 + 2)-(tx1 + 5, ty1 + 5), 2
      LINE (tx1 - 2, ty1 + 2)-(tx1 - 5, ty1 + 5), 2
      tx1 = tx1 + 21
      LINE (tx1 - 2, ty1 - 2)-(tx1 + 2, ty1 - 2), 1
      LINE -(tx1 + 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 + 2), 1
      LINE -(tx1 - 2, ty1 - 2), 1
      LINE (tx1 + 4, ty1 - 2)-(tx1 + 4, ty1 + 2), 2
      LINE (tx1 + 5, ty1 - 1)-(tx1 + 5, ty1 + 1), 2
      LINE (tx1 + 6, ty1)-(tx1 + 6, ty1), 2
      LINE (tx1 + 4, ty1 - 2)-(tx1 + 4, ty1 + 2), 2
      LINE (tx1 + 5, ty1 - 1)-(tx1 + 5, ty1 + 1), 2
      LINE (tx1 + 6, ty1)-(tx1 + 6, ty1), 2
      LINE (tx1 - 2, ty1 - 4)-(tx1 + 2, ty1 - 4), 2
      LINE (tx1 - 1, ty1 - 5)-(tx1 + 1, ty1 - 5), 2
      LINE (tx1, ty1 - 6)-(tx1, ty1 - 6), 2
      LINE (tx1 - 2, ty1 + 4)-(tx1 + 2, ty1 + 4), 2
      LINE (tx1 - 1, ty1 + 5)-(tx1 + 1, ty1 + 5), 2
      LINE (tx1, ty1 + 6)-(tx1, ty1 + 6), 2
      tx1 = tx1 + 21
      CIRCLE (tx1, ty1), 3, 1
      LINE (tx1 + 4, ty1 - 2)-(tx1 + 4, ty1 + 2), 2
      LINE (tx1 + 5, ty1 - 1)-(tx1 + 5, ty1 + 1), 2
      LINE (tx1 + 6, ty1)-(tx1 + 6, ty1), 2
      LINE (tx1 + 4, ty1 - 2)-(tx1 + 4, ty1 + 2), 2
      LINE (tx1 + 5, ty1 - 1)-(tx1 + 5, ty1 + 1), 2
      LINE (tx1 + 6, ty1)-(tx1 + 6, ty1), 2
      LINE (tx1 - 2, ty1 - 4)-(tx1 + 2, ty1 - 4), 2
      LINE (tx1 - 1, ty1 - 5)-(tx1 + 1, ty1 - 5), 2
      LINE (tx1, ty1 - 6)-(tx1, ty1 - 6), 2
      LINE (tx1 - 2, ty1 + 4)-(tx1 + 2, ty1 + 4), 2
      LINE (tx1 - 1, ty1 + 5)-(tx1 + 1, ty1 + 5), 2
      LINE (tx1, ty1 + 6)-(tx1, ty1 + 6), 2
      tls% = 10

     
      CASE "spray"
      FOR i = zx + 1 TO zx + 17 STEP .5
      FOR j = 1 TO 2
      a = randnum(zy + 1, zy + 14)
      LINE (i, a)-(i, a), 1
      NEXT
      NEXT
     
      zx1 = zx + 21
      FOR i = zx1 + 6 TO zx1 + 12 STEP .5
      FOR j = 1 TO 1
      a = randnum(zy + 4, zy + 11)
      LINE (i, a)-(i, a), 1
      NEXT
      NEXT
      tls% = 2


      CASE "rectangle"
      LINE (zx + 3, zy + 3)-(zx + 17, zy + 3), 1
      LINE -(zx + 17, zy + 12), 1
      LINE -(zx + 3, zy + 12), 1
      xc% = zx + 3: xc1% = zy + 12: xc2% = zy + 3
      linez xc%, xc1%, xc%, xc2%, 1, 1
     
      zx1 = zx + 21
      pnt zx1 + 3, zy + 3, zx1 + 17, zy + 12, 1
      tls% = 2

     
      CASE "line"
      LINE (zx + 3, zy + 12)-(zx + 17, zy + 3), 1
      zx1% = zx + 21
      zy1% = zy
      linez zx1% + 3, zy1% + 12, zx1% + 17, zy1% + 3, 2, 1
      FOR i% = 3 TO 10
      zx1% = zx1% + 21
      ch% = FIX(i% / 2 + .5)
      linez zx1% + ch%, zy1% + 15 - ch%, zx1% + 20 - ch%, zy1% + ch%, i%, 1
      NEXT
      tls% = 10


      CASE "brush"
      CIRCLE (zx + 10, zy + 7), 3, 1
      zx1 = zx + 21
      LINE (zx1 + 5, zy + 12)-(zx1 + 15, zy + 3), 1
      zx1 = zx1 + 21
      LINE (zx1 + 5, zy + 3)-(zx1 + 15, zy + 12), 1
      zx1 = zx1 + 21
      linex zx1 + 7, zy + 7, zx1 + 13, zy + 7, 6, 1
      tls% = 4

      CASE "circle"
      CIRCLE (zx + 10, zy + 7), 5, 1
      tls% = 1

      CASE "oval"
      CIRCLE (zx + 10, zy + 7), 6, 1, , , .5
      tls% = 1

      CASE ELSE
      END SELECT

  tl% = 1
  GOSUB stool5
  k$ = ""
  DO UNTIL k$ = CHR$(27) OR k$ = CHR$(enter)

  k$ = INKEY$

  SELECT CASE k$
  CASE CHR$(0) + CHR$(right)
  GOSUB stool4
  tl% = tl% + 1
  IF tl% > tls% THEN tl% = 1
  GOSUB stool5
  CASE CHR$(0) + CHR$(left)
  GOSUB stool4
  tl% = tl% - 1
  IF tl% < 1 THEN tl% = tls%
  GOSUB stool5
  CASE ELSE
  END SELECT

  LOOP

  IF k$ = CHR$(27) THEN GOTO stools
  IF k$ = CHR$(enter) THEN
status "TOOL SELECTED - " + UCASE$(tools$), 1
rub 253, 443, 547, 477


   END IF
tools$ = tools$ + ":" + STR$(tl%)
END IF
GOTO stool1

stool4: rx1% = zx + (tl% - 1) * 21
ry2% = zy + 16
rx2% = rx1% + 20
LINE (rx1%, zy)-(rx1% + 20, zy), 0
LINE -(rx1% + 20, zy + 16), 0
LINE -(rx1%, zy + 16), 0
LINE -(rx1%, zy), 0
RETURN

stool5: rx1% = zx + (tl% - 1) * 21
ry2% = zy + 16
rx2% = rx1% + 20
LINE (rx1%, zy)-(rx1% + 20, zy), 15
LINE -(rx1% + 20, zy + 16), 15
LINE -(rx1%, zy + 16), 15
LINE -(rx1%, zy), 15
RETURN

stool2: SELECT CASE tzol%         '********Setting tool$
CASE 1
tools$ = "pencil"
CASE 2
tools$ = "brush"
CASE 3
tools$ = "line"
CASE 4
tools$ = "rectangle"
CASE 5
tools$ = "circle"
CASE 6
tools$ = "oval"
CASE 7
tools$ = "spray"
CASE ELSE
END SELECT

r% = 93 + (tzol% - 1) * 16
r1% = r% + 15
LINE (3, r%)-(25, r%), 15
LINE -(25, r1%), 15
LINE -(3, r1%), 15
LINE -(3, r%), 15
status "CHOOSING TOOL - " + UCASE$(tools$), 1
RETURN
stool3: r% = 93 + (tzol% - 1) * 16
r1% = r% + 15
LINE (3, r%)-(25, r%), 0
LINE -(25, r1%), 0
LINE -(3, r1%), 0
LINE -(3, r%), 0
RETURN

stool1: linex 0, 90, 40, 90, 3, 14
linex 0, 440, 40, 440, 3, 14
liney 1, 90, 1, 440, 3, 14
liney 40, 90, 40, 440, 3, 14
linex 0, 440, 250, 440, 3, 14
linex 0, 479, 250, 479, 3, 14
liney 1, 440, 1, 480, 3, 14
liney 250, 440, 250, 480, 3, 14
linex 250, 440, 540, 440, 3, 14
linex 250, 479, 540, 479, 3, 14
liney 540, 440, 540, 480, 3, 14
liney 250, 440, 25, 480, 3, 14
status "", 0
GOSUB stool3
zzz$ = tools$
nullify
tools$ = zzz$
END SUB

FUNCTION volname$
SHELL "dir >ram.pth"
a = FREEFILE
OPEN "I", #a, "ram.pth"
LINE INPUT #a, z$
LINE INPUT #a, z$
CLOSE #a
KILL "ram.pth"
volname$ = RIGHT$(z$, LEN(z$) - 22)
END FUNCTION

FUNCTION volsnum$
SHELL "dir >ram.pth"
a = FREEFILE
OPEN "I", #a, "ram.pth"
FOR i = 1 TO 3
LINE INPUT #1, z$
NEXT
CLOSE #a
KILL "ram.pth"
volsnum$ = RIGHT$(z$, LEN(z$) - 25)
END FUNCTION

SUB win (h$, c%)
'=================================Drawing borders============================
linex 0, 0, 640, 0, 3, c%
linex 0, 478, 640, 478, 3, c%
linex 0, 440, 640, 440, 3, c%
linex 40, 20, 540, 20, 3, c%
linex 0, 70, 40, 70, 3, c%
linex 0, 90, 40, 90, 3, c%
linex 540, 420, 640, 420, 3, c%
word "TEXT", 543, 425, 2, 1, 1, 9, 21000, 0
linex 540, 400, 640, 400, 3, c%
linex 540, 380, 640, 380, 3, c%
word "MENU", 543, 385, 2, 1, 2, 9, 21000, 0
linex 540, 50, 640, 50, 3, c%
liney 1, 0, 1, 480, 3, c%
liney 40, 0, 40, 440, 3, c%
liney 250, 440, 250, 480, 3, c%
liney 540, 0, 540, 480, 3, c%
liney 639, 0, 639, 480, 3, c%
word "FILE NAME - " + UCASE$(h$), 150, 3, 2, 1, 15, 9, 21000, 0
'--------------------------Drawing borders complete--------------------------

'============================Colour box======================================
FOR i% = 0 TO 7
z% = 10 + (i% * 7)
y% = i% * 2
pnt 5, z%, 15, z% + 4, y%
pnt 25, z%, 35, z% + 4, y% + 1
NEXT
pnt 10, 75, 30, 85, 1
'----------------------------Colour box complete-----------------------------

'============================Tools box=======================================
zx = 3
zy = 93
      LINE (zx + 12, zy + 4)-(zx + 2, zy + 12), 1
      LINE -(zx + 6, zy + 14), 1
      LINE -(zx + 16, zy + 6), 1
      LINE -(zx + 12, zy + 4), 1
      LINE -(zx + 14, zy + 1), 2
      LINE -(zx + 12, zy + 4), 2
      LINE (zx + 13, zy + 2)-(zx + 14, zy + 3), 3
      LINE (zx + 14, zy + 5)-(zx + 4, zy + 13), 4

zy = zy + 16
      LINE (zx + 3, zy + 12)-(zx + 5, zy + 13), 1
      LINE -(zx + 9, zy + 6), 1
      LINE -(zx + 7, zy + 4), 1
      LINE -(zx + 3, zy + 12), 1
      LINE (zx + 7, zy + 4)-(zx + 5, zy + 2), 2
      LINE -(zx + 8, zy + 1), 2
      LINE -(zx + 10, zy + 2), 2
      LINE -(zx + 9, zy + 6), 3

zy = zy + 16
      LINE (zx + 3, zy + 12)-(zx + 17, zy + 3), 1
zy = zy + 16
      LINE (zx + 3, zy + 3)-(zx + 17, zy + 3), 1
      LINE -(zx + 17, zy + 12), 1
      LINE -(zx + 3, zy + 12), 1
      xc% = zx + 3: xc1% = zy + 12: xc2% = zy + 3
      linez xc%, xc1%, xc%, xc2%, 1, 1
zy = zy + 16
      CIRCLE (zx + 10, zy + 7), 5, 1
zy = zy + 16
      CIRCLE (zx + 10, zy + 7), 6, 1, , , .5
zy = zy + 16
      FOR i = zx + 1 TO zx + 19 STEP .5
      FOR j = 1 TO 4
      a = randnum(zy + 1, zy + 14)
      LINE (i, a)-(i, a), 1
      NEXT
      NEXT
zy = zy + 16
jk = 0
jk1 = 0
'---------------------------Drawing tools complete---------------------------

'===========================Drawing objects box==============================
zx1 = 546
zy1 = 6
obj1p "trees:1", zx1, zy1
zx1 = 591
obj1p "circuitsymbols:1", zx1, zy1
zy1 = 26
obj1p "othersymbols:1", zx1, zy1
zx1 = 546
obj1p "greeksymbols:1", zx1, zy1
'---------------------------Drawing objects box complete---------------------

'===========================Drawing spreadlines box==========================
zx1% = 543
zy1% = 443
linez zx1% + 2, zy1% + 12, zx1% + 42, zy1% + 2, 2, 4      'normal

zx1% = zx1% + 46
LINE (zx1% + 2, zy1% + 12)-(zx1% + 4, zy1% + 12), 4       'squared
LINE -(zx1% + 4, zy1% + 6), 4
LINE -(zx1% + 10, zy1% + 6), 4
LINE -(zx1% + 10, zy1% + 12), 4
LINE -(zx1% + 16, zy1% + 12), 4
LINE -(zx1% + 16, zy1% + 6), 4
LINE -(zx1% + 22, zy1% + 6), 4
LINE -(zx1% + 22, zy1% + 12), 4
LINE -(zx1% + 28, zy1% + 12), 4
LINE -(zx1% + 28, zy1% + 6), 4
LINE -(zx1% + 34, zy1% + 6), 4
LINE -(zx1% + 34, zy1% + 12), 4
LINE -(zx1% + 40, zy1% + 12), 4
LINE -(zx1% + 40, zy1% + 6), 4
LINE -(zx1% + 42, zy1% + 6), 4

zx1% = zx1% - 46
zy1% = zy1% + 16
LINE (zx1% + 2, zy1% + 2)-(zx1% + 6, zy1% + 12), 4    'zigzag
LINE -(zx1% + 12, zy1% + 2), 4
LINE -(zx1% + 18, zy1% + 12), 4
LINE -(zx1% + 24, zy1% + 2), 4
LINE -(zx1% + 30, zy1% + 12), 4
LINE -(zx1% + 36, zy1% + 2), 4
LINE -(zx1% + 42, zy1% + 12), 4

zx1% = zx1% + 46
LINE (zx1% + 2, zy1% + 6)-(zx1% + 4, zy1% + 3), 4      'curved
LINE -(zx1% + 6, zy1% + 3), 4
LINE -(zx1% + 8, zy1% + 6), 4
LINE -(zx1% + 10, zy1% + 9), 4
LINE -(zx1% + 12, zy1% + 9), 4
LINE -(zx1% + 14, zy1% + 6), 4
LINE -(zx1% + 16, zy1% + 3), 4
LINE -(zx1% + 18, zy1% + 3), 4
LINE -(zx1% + 20, zy1% + 6), 4
LINE -(zx1% + 22, zy1% + 9), 4
LINE -(zx1% + 24, zy1% + 9), 4
LINE -(zx1% + 26, zy1% + 6), 4
LINE -(zx1% + 28, zy1% + 3), 4
LINE -(zx1% + 30, zy1% + 3), 4
LINE -(zx1% + 32, zy1% + 6), 4
LINE -(zx1% + 34, zy1% + 9), 4
LINE -(zx1% + 36, zy1% + 9), 4
LINE -(zx1% + 38, zy1% + 6), 4
LINE -(zx1% + 40, zy1% + 3), 4
LINE -(zx1% + 42, zy1% + 3), 4
'============================Colour box======================================
rub 2, 2, 36, 66
FOR i% = 0 TO 7
z% = 10 + (i% * 7)
y% = i% * 2
pnt 5, z%, 15, z% + 4, y%
pnt 25, z%, 35, z% + 4, y% + 1
NEXT
pnt 10, 75, 30, 85, 1
'----------------------------Colour box complete-----------------------------

END SUB

SUB word (s AS STRING, x AS INTEGER, y AS INTEGER, t1 AS INTEGER, p1 AS INTEGER, c AS INTEGER, p AS INTEGER, f AS INTEGER, t AS DOUBLE)
hx1% = x
FOR i% = 1 TO LEN(s$)
a$ = MID$(s$, i%, 1)
IF a$ <> " " THEN char a$, hx1%, y, t1, p1, c
hx1% = hx1% + p
SOUND f, t
NEXT
END SUB

