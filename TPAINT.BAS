'
' Check TFill for usage and notes
'
' Jernej Simoncic, jernej.simoncic@guest.arnes.si
' http://www2.arnes.si/~sopjsimo/
' ICQ: 26266467
'

DECLARE SUB TFill (x%, y%, Array() AS INTEGER)
DECLARE SUB SetPal (Attr%, Red%, Green%, Blue%)

DIM Texture(5001) AS INTEGER

TYPE pixtype    'needed by TFill
  x AS INTEGER
  y AS INTEGER
END TYPE

RANDOMIZE TIMER

SCREEN 13
OPEN "texture" FOR BINARY AS 1 'load texture
FOR x% = 0 TO 99
  FOR y% = 0 TO 99
    a$ = " "
    GET 1, , a$
    PSET (x%, y%), ASC(a$) + 1
  NEXT
NEXT

r$ = " "
g$ = " "
B$ = " "
FOR i% = 0 TO 254
 GET 1, , r$
 GET 1, , g$
 GET 1, , B$
 SetPal i% + 1, ASC(r$), ASC(g$), ASC(B$)
NEXT
CLOSE

GET (0, 0)-(99, 99), Texture

SetPal 255, 63, 63, 63

CLS

PRINT "Lets fill this circle with color..."
CIRCLE (159, 99), 100, 255

DO: LOOP WHILE INKEY$ = ""

PAINT (159, 99), 255
LOCATE 1, 1: PRINT "Ever wanted anything more than this?"

DO: LOOP WHILE INKEY$ = ""

LOCATE 1, 1: PRINT "A texture, for example?             "
TFill 109 + INT(RND * 100), 49 + INT(RND * 100), Texture()

DO: LOOP WHILE INKEY$ = ""

DEFINT A-Z
SUB SetPal (Attr%, Red%, Green%, Blue%)
OUT &H3C7, Attr%
OUT &H3C8, Attr%
OUT &H3C9, Red%
OUT &H3C9, Green%
OUT &H3C9, Blue%
END SUB

DEFSNG A-Z
'
' TFill - a texture fill routine
'
' Usage: TFill x%, y%, Array%()
'
'  x%, y%     coordinates to start texture fill; sets texture offset, too.
'  Array%()   a GET integer array with texture
'
'-----------------------------------------------------------------------------
'
' Note: there is a bug in the fill routine: if texture contains the color,
' which is the original background, it will loop forever. If somebody corrects
' this bug, please send me the fixed routine.
'
'=============================================================================
'
' Original FloodFill by Petter Holmberg, petter.holmberg@usa.net
' Texture filling by Jernej Simoncic, jernej.simoncic@guest.arnes.si
'
SUB TFill (x%, y%, Array() AS INTEGER)

DIM pixdata(1278) AS pixtype

xx% = Array(0) / 8
yy% = Array(1)

bcol% = POINT(x%, y%)

firstentry% = 0
lastentry% = 1

DEF SEG = VARSEG(Array(0))

DO

   fx% = pixdata(firstentry%).x
   fy% = pixdata(firstentry%).y

   DO
      IF POINT(x% + fx%, y% + fy%) = bcol% THEN
       cx% = fx%
       IF cx% < 0 THEN
        DO
         cx% = cx% + xx%
        LOOP UNTIL cx% > 0
       END IF
       cx% = cx% MOD xx%
       cy% = fy%
       IF cy% < 0 THEN
        DO
         cy% = cy% + yy%
        LOOP UNTIL cy% > 0
       END IF
       cy% = cy% MOD yy%
       PSET (x% + fx%, y% + fy%), PEEK(4 + cx% + cy% * yy%)
       IF POINT(x% + fx%, y% + fy% - 1) = bcol% THEN
        pixdata(lastentry%).x = fx%
        pixdata(lastentry%).y = fy% - 1
        lastentry% = lastentry% + 1
        IF lastentry% = 1279 THEN lastentry% = 0
       END IF
       IF POINT(x% + fx%, y% + fy% + 1) = bcol% THEN
        pixdata(lastentry%).x = fx%
        pixdata(lastentry%).y = fy% + 1
        lastentry% = lastentry% + 1
        IF lastentry% = 1279 THEN lastentry% = 0
       END IF
      ELSE
       EXIT DO
      END IF
      fx% = fx% + 1
   LOOP

   fx% = pixdata(firstentry%).x - 1
   fy% = pixdata(firstentry%).y

   DO
      IF POINT(x% + fx%, y% + fy%) = bcol% THEN
       cx% = fx%
       IF cx% < 0 THEN
        DO
         cx% = cx% + xx%
        LOOP UNTIL cx% > 0
       END IF
       cx% = cx% MOD xx%
       cy% = fy%
       IF cy% < 0 THEN
        DO
         cy% = cy% + yy%
        LOOP UNTIL cy% > 0
       END IF
       cy% = cy% MOD yy%
       PSET (x% + fx%, y% + fy%), PEEK(4 + cx% + cy% * yy%)
       IF POINT(x% + fx%, y% + fy% - 1) = bcol% THEN
        pixdata(lastentry%).x = fx%
        pixdata(lastentry%).y = fy% - 1
        lastentry% = lastentry% + 1
        IF lastentry% = 1279 THEN lastentry% = 0
       END IF
       IF POINT(x% + fx%, y% + fy% + 1) = bcol% THEN
        pixdata(lastentry%).x = fx%
        pixdata(lastentry%).y = fy% + 1
        lastentry% = lastentry% + 1
        IF lastentry% = 1279 THEN lastentry% = 0
       END IF
      ELSE
       EXIT DO
      END IF
      fx% = fx% - 1
   LOOP

   firstentry% = firstentry% + 1
   IF firstentry% = 1279 THEN firstentry% = 0

LOOP UNTIL firstentry% = lastentry%

DEF SEG

END SUB

