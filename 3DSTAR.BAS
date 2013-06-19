DECLARE SUB ProcessLight2 (A AS INTEGER, B AS INTEGER, C AS INTEGER, d AS INTEGER)
DECLARE SUB ProcessSort2 ()
'$INCLUDE: '3_d.bi'
'$INCLUDE: 'DirectQB.BI'

CONST CenterX = 160
CONST CenterY = 100

DECLARE SUB Drawing (N AS INTEGER)
DECLARE SUB IntroScreen ()
DECLARE SUB Demo ()

CLEAR , , 4096

speedfpu ''<-- do the magic ;)

IntroScreen

IF DQBinit(1, 0, 0) THEN DQBclose: END
DQBinitVGA
DQBinstallKeyboard

DIM SHARED s AS INTEGER
DIM SHARED OptionP AS INTEGER
DIM SHARED OptionF AS INTEGER
DIM SHARED OptionC AS INTEGER
DIM SHARED OptionM AS INTEGER
DIM SHARED OptionV AS INTEGER
DIM SHARED OptionO AS INTEGER
DIM SHARED OptionI AS INTEGER
DIM SHARED KeyP AS INTEGER
DIM SHARED KeyF AS INTEGER
DIM SHARED KeyC AS INTEGER
DIM SHARED KeyM AS INTEGER
DIM SHARED KeyV AS INTEGER
DIM SHARED KeyO AS INTEGER
DIM SHARED KeyI AS INTEGER
DIM SHARED nrLines AS INTEGER
DIM SHARED FPS AS INTEGER
DIM SHARED steadyfps AS INTEGER
DIM SHARED fpstime AS LONG
DIM SHARED Een AS INTEGER
DIM SHARED eenx AS INTEGER
DIM SHARED eeny AS INTEGER
DIM SHARED twee AS INTEGER
DIM SHARED tweex AS INTEGER
DIM SHARED tweey AS INTEGER
DIM SHARED drie AS INTEGER
DIM SHARED driex AS INTEGER
DIM SHARED driey AS INTEGER
DIM SHARED Licht1Hoek1 AS SINGLE
DIM SHARED licht1hoek2 AS SINGLE
DIM SHARED licht2hoek1 AS SINGLE
DIM SHARED licht2hoek2 AS SINGLE
DIM SHARED licht1hoek1erbij AS SINGLE
DIM SHARED licht1hoek2erbij AS SINGLE
DIM SHARED licht2hoek1erbij AS SINGLE
DIM SHARED licht2hoek2erbij AS SINGLE
DIM A AS INTEGER, B AS INTEGER, C AS INTEGER, d AS INTEGER
DIM i AS INTEGER, l AS INTEGER

OptionP = 1
OptionF = 1
OptionC = 0
OptionM = 1
OptionV = 0
OptionO = 0
OptionI = 0

s = 50

        FOR A = 0 TO 63
          DQBsetCol A, 0, 0, 0
          DQBsetCol A + 64, A \ 2, A \ 2, A \ 2
          DQBsetCol A + 128, 31 + A \ 2, 31 + A \ 2, 31 + A \ 2
          DQBsetCol A + 192, 63, 63, 63
        NEXT A

viewport.x = 0
viewport.y = 0
viewport.Z = 0
viewport.VerticalAngle = 0
viewport.WidthAngle = 0
viewport.DepthAngle = 0
MinDistance = 70

RESTORE ObjectData

READ A
 REDIM objects(0 TO A) AS ObjectType
FOR B = 1 TO A
  READ objects(B).x
  READ objects(B).y
  READ objects(B).Z
  READ objects(B).VerticalAngle
  READ objects(B).WidthAngle
  READ objects(B).DepthAngle
NEXT B

RESTORE LightData

READ A
REDIM LightSource(0 TO A) AS LightSourceType
FOR B = 1 TO A
  READ LightSource(B).x
  READ LightSource(B).y
  READ LightSource(B).Z
  READ LightSource(B).R
  READ LightSource(B).G
  READ LightSource(B).B
  READ LightSource(B).Strength
NEXT B

RESTORE PointData

READ A
REDIM points(0 TO A) AS PointType
REDIM PointsOutput(0 TO A) AS LoosePointType
FOR B = 1 TO A
  READ points(B).x, points(B).y, points(B).Z, points(B).Object
  points(B).x = points(B).x * 10
  points(B).y = points(B).y * 10
  points(B).Z = points(B).Z * 10
NEXT B

RESTORE PolygonData

READ A, C

A = A * 4
C = C * 4

REDIM PolygonS(0 TO A) AS PolygonPointType
REDIM PolygonN(0 TO C - A) AS PolygonNextPointType
REDIM DrawOrder(0 TO A) AS INTEGER

A = A \ 4
C = C \ 4

FOR d = 0 TO 3
FOR B = 1 + A * d TO A + A * d
  READ PolygonS(B).PPoint
  PolygonS(B).PPoint = PolygonS(B).PPoint + 14 * d
  PolygonS(B).NextPoint = B * 2 - 1

  READ PolygonN(B * 2 - 1).PPoint
  PolygonN(B * 2 - 1).PPoint = PolygonN(B * 2 - 1).PPoint + 14 * d
  PolygonN(B * 2 - 1).NextPoint = B * 2

  READ PolygonN(B * 2).PPoint
  PolygonN(B * 2).PPoint = PolygonN(B * 2).PPoint + 14 * d

  'Calculating center
  PolygonS(B).x = (points(PolygonS(B).PPoint).x + points(PolygonN(B * 2 - 1).PPoint).x + points(PolygonN(B * 2).PPoint).x) / 3
  PolygonS(B).y = (points(PolygonS(B).PPoint).y + points(PolygonN(B * 2 - 1).PPoint).y + points(PolygonN(B * 2).PPoint).y) / 3
  PolygonS(B).Z = (points(PolygonS(B).PPoint).Z + points(PolygonN(B * 2 - 1).PPoint).Z + points(PolygonN(B * 2).PPoint).Z) / 3

  'setting distances, for the light routine
  PolygonS(B).Distance = 9
  PolygonN(PolygonS(B).NextPoint).Distance = 9
  PolygonN(PolygonN(PolygonS(B).NextPoint).NextPoint).Distance = 9
  
NEXT B
NEXT d

ObjectData:
'Only one object    well, yeah, the stuff is devided with 4 but you shouldnt be bothered
DATA 4
'Info about center and angles
DATA 0, 0, 150, 0, 0, 0
DATA 0, 0, -150, 0, 0, 0
DATA 150, 0, 0, 0, 0, 0
DATA -150, 0, 0, 0, 0, 0
'simple huh?

LightData:
'only two lights
DATA 2
'a white light, at position (-70, 0, 0)
DATA -50, 0, 0, 5, 255, 255
'50 Strength
DATA 13
'a white light, at position (70, 0, 0)
DATA 50, 0, 0, 255, 255, 255
'50 Strength
DATA 13

PointData:

'Number of points
DATA 56

DATA -4.6184 , -2.6666 , -2 , 1
DATA -4.6184 , 2.6666  , 2  , 1
DATA -2.3094 , -1.3333 , 2  , 1
DATA -2.3094 , 1.3333  , -2 , 1
DATA 0       , -5.3333 , 2  , 1
DATA 0       , -2.6666 , -2 , 1
DATA 0       , 0       , -6 , 1
DATA 0       , 0       , 6  , 1
DATA 0       , 2.6666  , 2  , 1
DATA 0       ,  5.3333 , -2 , 1
DATA 2.3094  , -1.3333 , 2  , 1
DATA 2.3094  , 1.3333  , -2 , 1
DATA 4.6184  , -2.6666 , -2 , 1
DATA 4.6184  , 2.6666  , 2  , 1

DATA -4.6184 , -2.6666 , -2 , 2
DATA -4.6184 , 2.6666  , 2  , 2
DATA -2.3094 , -1.3333 , 2  , 2
DATA -2.3094 , 1.3333  , -2 , 2
DATA 0       , -5.3333 , 2  , 2
DATA 0       , -2.6666 , -2 , 2
DATA 0       , 0       , -6 , 2
DATA 0       , 0       , 6  , 2
DATA 0       , 2.6666  , 2  , 2
DATA 0       ,  5.3333 , -2 , 2
DATA 2.3094  , -1.3333 , 2  , 2
DATA 2.3094  , 1.3333  , -2 , 2
DATA 4.6184  , -2.6666 , -2 , 2
DATA 4.6184  , 2.6666  , 2  , 2

DATA -4.6184 , -2.6666 , -2 , 3
DATA -4.6184 , 2.6666  , 2  , 3
DATA -2.3094 , -1.3333 , 2  , 3
DATA -2.3094 , 1.3333  , -2 , 3
DATA 0       , -5.3333 , 2  , 3
DATA 0       , -2.6666 , -2 , 3
DATA 0       , 0       , -6 , 3
DATA 0       , 0       , 6  , 3
DATA 0       , 2.6666  , 2  , 3
DATA 0       ,  5.3333 , -2 , 3
DATA 2.3094  , -1.3333 , 2  , 3
DATA 2.3094  , 1.3333  , -2 , 3
DATA 4.6184  , -2.6666 , -2 , 3
DATA 4.6184  , 2.6666  , 2  , 3

DATA -4.6184 , -2.6666 , -2 , 4
DATA -4.6184 , 2.6666  , 2  , 4
DATA -2.3094 , -1.3333 , 2  , 4
DATA -2.3094 , 1.3333  , -2 , 4
DATA 0       , -5.3333 , 2  , 4
DATA 0       , -2.6666 , -2 , 4
DATA 0       , 0       , -6 , 4
DATA 0       , 0       , 6  , 4
DATA 0       , 2.6666  , 2  , 4
DATA 0       ,  5.3333 , -2 , 4
DATA 2.3094  , -1.3333 , 2  , 4
DATA 2.3094  , 1.3333  , -2 , 4
DATA 4.6184  , -2.6666 , -2 , 4
DATA 4.6184  , 2.6666  , 2  , 4

PolygonData:
'Number of PolygonS, Number of points totaly on every polygon
DATA 24, 72

'format: nr of the point, number of the light, groupindex. the groupindex determines if it is the same polygon, or another one
DATA 8, 9, 3
DATA 8, 11, 9
DATA 8, 3, 11
'right back above
DATA 14, 9, 11
DATA 14, 11, 12
DATA 14, 12, 9
'left back above
DATA 2, 3, 9
DATA 2, 4, 3
DATA 2, 9, 4
'front (above)
DATA 5, 11, 3
DATA 5, 6, 11
DATA 5, 3, 6
'left front below
DATA 1, 4, 6
DATA 1, 3, 4
DATA 1, 6, 3
'right front below
DATA 13, 6, 12
DATA 13, 11, 6
DATA 13,12, 11
'back
DATA 10, 12, 4
DATA 10, 9, 12
DATA 10, 4, 9
'bottom
DATA 7, 12, 6
DATA 7, 4, 12
DATA 7, 6, 4

DATA 8, 9, 3
DATA 8, 9, 11
DATA 8, 3, 11
DATA 9, 11, 14
DATA 12, 11, 14
DATA 9, 12, 14
DATA 9, 3, 2
DATA 4, 3, 2
DATA 9, 4, 2
DATA 5, 3, 11
DATA 5, 6, 11
DATA 5, 3, 6
DATA 6, 4, 1
DATA 3, 4, 1
DATA 6, 3, 1
DATA 6, 12, 13
DATA 11,12, 13
DATA 6, 11, 13
DATA 10, 12, 4
DATA 9, 10, 4
DATA 9, 12, 10
DATA 7, 6, 12
DATA 7, 6, 4
DATA 7, 12, 4

DATA 8, 9, 3
DATA 8, 9, 11
DATA 8, 3, 11
DATA 9, 11, 14
DATA 12, 11, 14
DATA 9, 12, 14
DATA 9, 3, 2
DATA 4, 3, 2
DATA 9, 4, 2
DATA 5, 3, 11
DATA 5, 6, 11
DATA 5, 3, 6
DATA 6, 4, 1
DATA 3, 4, 1
DATA 6, 3, 1
DATA 6, 12, 13
DATA 11,12, 13
DATA 6, 11, 13
DATA 10, 12, 4
DATA 9, 10, 4
DATA 9, 12, 10
DATA 7, 6, 12
DATA 7, 6, 4
DATA 7, 12, 4

DATA 8, 9, 3
DATA 8, 9, 11
DATA 8, 3, 11
DATA 9, 11, 14
DATA 12, 11, 14
DATA 9, 12, 14
DATA 9, 3, 2
DATA 4, 3, 2
DATA 9, 4, 2
DATA 5, 3, 11
DATA 5, 6, 11
DATA 5, 3, 6
DATA 6, 4, 1
DATA 3, 4, 1
DATA 6, 3, 1
DATA 6, 12, 13
DATA 11,12, 13
DATA 6, 11, 13
DATA 10, 12, 4
DATA 9, 10, 4
DATA 9, 12, 10
DATA 7, 6, 12
DATA 7, 6, 4
DATA 7, 12, 4

RANDOMIZE TIMER

Demo

DQBremoveKeyboard
DQBclose

CLS

COLOR 11
PRINT "That were 24 triangles"
COLOR 9
PRINT "Together 14 points"
COLOR 10
PRINT "With one dynamic light"
COLOR 6
PRINT "Calculated mostly in ASM now"
PRINT
COLOR 7
PRINT "All drawing functions were done with DQB (DQBGTri, DQBGLine and DQBPrint)"
PRINT "As well as its keyboard handler"
PRINT
COLOR 12
PRINT "Press any key except F to quit or"
COLOR 4
PRINT "If you want to go to the Future Software homepage press F"
COLOR 7

WHILE INKEY$ <> "": WEND

k$ = INPUT$(1)

IF UCASE$(k$) = "F" THEN
  PRINT
  COLOR 15
  PRINT "The assumption is made that you installed windows in c:\windows"
  PRINT "So if you haven't, this wont work."
  COLOR 7
  SLEEP
  SHELL "c:"
  SHELL "cd\windows\command"
  SHELL "C:\windows\command\Start.exe /m http://www.qb45.com/main.htm"
END IF

REM $DYNAMIC
DEFSNG A-Z
SUB Demo
 
 

  DO
 
  IF DQBkey(KEYUP) THEN
  objects(1).WidthAngle = objects(1).WidthAngle - PI / s
  IF objects(1).WidthAngle <= 0 THEN objects(1).WidthAngle = objects(1).WidthAngle + PI * 2
  END IF

  IF DQBkey(KEYDOWN) THEN
  objects(1).WidthAngle = objects(1).WidthAngle + PI / s
  IF objects(1).WidthAngle >= PI * 2 THEN objects(1).WidthAngle = objects(1).WidthAngle - PI * 2
  END IF

  IF DQBkey(KEYLEFT) THEN
  objects(1).VerticalAngle = objects(1).VerticalAngle - PI / s
  IF objects(1).VerticalAngle <= 0 THEN objects(1).VerticalAngle = objects(1).VerticalAngle + PI * 2
  END IF

  IF DQBkey(KEYRIGHT) THEN
  objects(1).VerticalAngle = objects(1).VerticalAngle + PI / s
  IF objects(1).VerticalAngle >= PI * 2 THEN objects(1).VerticalAngle = objects(1).VerticalAngle - PI * 2
  END IF

  IF DQBkey(44) THEN
  objects(1).DepthAngle = objects(1).DepthAngle - PI / s
  IF objects(1).DepthAngle <= 0 THEN objects(1).DepthAngle = objects(1).DepthAngle + PI * 2
  END IF

  IF DQBkey(45) THEN
  objects(1).DepthAngle = objects(1).DepthAngle + PI / s
  IF objects(1).DepthAngle >= PI * 2 THEN objects(1).DepthAngle = objects(1).DepthAngle - PI * 2
  END IF
 
  IF DQBkey(47) THEN
    IF KeyV THEN OptionV = 1 - OptionV
    KeyV = 0
  ELSE
    KeyV = 1
  END IF

  IF DQBkey(16) THEN
    viewport.DepthAngle = viewport.DepthAngle - PI / s
    IF viewport.DepthAngle < 0 THEN viewport.DepthAngle = viewport.DepthAngle + PI * 2
  END IF
 
  IF DQBkey(18) THEN
    viewport.DepthAngle = viewport.DepthAngle + PI / s
    IF viewport.DepthAngle >= PI * 2 THEN viewport.DepthAngle = viewport.DepthAngle - PI * 2
  END IF

  IF DQBkey(17) THEN
    viewport.WidthAngle = viewport.WidthAngle - PI / s
    IF viewport.WidthAngle < 0 THEN viewport.WidthAngle = viewport.WidthAngle + PI * 2
  END IF
 
  IF DQBkey(21) THEN
    viewport.VerticalAngle = viewport.VerticalAngle + PI / s
    viewport.Z = 150 * COS(PI + viewport.VerticalAngle) + 150
    viewport.x = 150 * SIN(PI + viewport.VerticalAngle)
    IF viewport.VerticalAngle >= PI * 2 THEN viewport.VerticalAngle = viewport.VerticalAngle - PI * 2
  END IF

  IF DQBkey(31) THEN
    viewport.WidthAngle = viewport.WidthAngle + PI / s
    IF viewport.WidthAngle >= PI * 2 THEN viewport.WidthAngle = viewport.WidthAngle - PI * 2
  END IF

  IF DQBkey(30) THEN
    viewport.VerticalAngle = viewport.VerticalAngle - PI / s
    IF viewport.VerticalAngle < 0 THEN viewport.VerticalAngle = viewport.VerticalAngle + PI * 2
  END IF

  IF DQBkey(32) THEN
    viewport.VerticalAngle = viewport.VerticalAngle + PI / s
    IF viewport.VerticalAngle >= PI * 2 THEN viewport.VerticalAngle = viewport.VerticalAngle - PI * 2
  END IF
 
  IF DQBkey(19) THEN
    'doing stuff for the VerticalAngle
    deler = COS(viewport.WidthAngle)
    viewport.x = viewport.x - SIN(viewport.VerticalAngle) * deler
    viewport.Z = viewport.Z - COS(viewport.VerticalAngle) * deler
    'and only one thing for the widthangle
    viewport.y = viewport.y - SIN(viewport.WidthAngle)
  END IF

  IF DQBkey(20) THEN
    'doing stuff for the VerticalAngle
    deler = COS(viewport.WidthAngle)
    viewport.x = viewport.x + SIN(viewport.VerticalAngle) * deler
    viewport.Z = viewport.Z + COS(viewport.VerticalAngle) * deler
    viewport.y = viewport.y + SIN(viewport.WidthAngle)
  END IF

  IF DQBkey(9) THEN 'toets 8
    viewport.y = viewport.y + 1
  END IF

  IF DQBkey(3) THEN 'toets 2
    viewport.y = viewport.y - 1
  END IF

  IF DQBkey(5) THEN 'toets 4
    viewport.x = viewport.x + 1
  END IF

  IF DQBkey(7) THEN 'toets 6
    viewport.x = viewport.x - 1
  END IF

  IF DQBkey(10) THEN 'toets 9
    viewport.Z = viewport.Z + 1
  END IF

  IF DQBkey(4) THEN 'toets 3
    viewport.Z = viewport.Z - 1
  END IF

 
  IF DQBkey(50) THEN
    IF KeyM THEN OptionM = 1 - OptionM
    KeyM = 0
  ELSE
    KeyM = 1
  END IF

  IF DQBkey(23) THEN
    IF KeyI THEN OptionI = 1 - OptionI
    KeyI = 0
  ELSE
    KeyI = 1
  END IF

  IF DQBkey(24) THEN
    IF KeyO THEN OptionO = 1 - OptionO
    KeyO = 0
  ELSE
    KeyO = 1
  END IF

  IF DQBkey(25) THEN
    IF KeyP THEN OptionP = 1 - OptionP
    KeyP = 0
  ELSE
    KeyP = 1
  END IF

  IF DQBkey(33) THEN
    IF KeyF THEN OptionF = 1 - OptionF
    KeyF = FALSE
  ELSE
    KeyF = TRUE
  END IF

  IF DQBkey(46) THEN
    IF KeyC THEN
      OptionC = OptionC MOD 9 + 1
      SELECT CASE OptionC
      CASE 1
        FOR A = 0 TO 63
          DQBsetCol A, 0, A, 0
          DQBsetCol A + 64, 0, 63, A
          DQBsetCol A + 128, 0, 63, 63 - A
          DQBsetCol A + 192, 0, 63 - A, 0
        NEXT A
      CASE 2
        FOR A = 0 TO 63
          DQBsetCol A, A \ 2, 0, 0
          DQBsetCol A + 64, 32 - A \ 2, A, 0
          DQBsetCol A + 128, 0, 63 - A, A
          DQBsetCol A + 192, 0, 0, 63 - A
        NEXT A
      CASE 3
        FOR A = 0 TO 63
          DQBsetCol A, A \ 4, A \ 4, A \ 4
          DQBsetCol A + 64, A \ 4 + 16, A \ 4 + 16, A \ 4 + 16
          DQBsetCol A + 128, A \ 4 + 32, A \ 4 + 32, A \ 4 + 32
          DQBsetCol A + 192, A \ 4 + 48, A \ 4 + 48, A \ 4 + 48
        NEXT A
      CASE 4
        FOR A = 0 TO 63
          IF A > 0 THEN DQBsetCol A, 0, A, 63 - A
          DQBsetCol A + 64, A, 63, 0
          DQBsetCol A + 128, 63, 63, A
          DQBsetCol A + 192, 63, 63, 63
        NEXT A
      CASE 5
        FOR A = 0 TO 63
          DQBsetCol A, A \ 4, A \ 4, 0
          DQBsetCol A + 64, 15 + A \ 4, 15 + A \ 4, 0
          DQBsetCol A + 128, 31 + A \ 4, 31 + A \ 4, 0
          DQBsetCol A + 192, 47 + A \ 4, 47 + A \ 4, 0
        NEXT A
      CASE 6
        FOR A = 0 TO 63
          DQBsetCol A, 0, A \ 4, A \ 4
          DQBsetCol A + 64, 0, 15 + A \ 4, 15 + A \ 4
          DQBsetCol A + 128, 0, 31 + A \ 4, 31 + A \ 4
          DQBsetCol A + 192, 0, 47 + A \ 4, 47 + A \ 4
        NEXT A
      CASE 7
        FOR A = 0 TO 63
          DQBsetCol A, A \ 4, 0, A \ 4
          DQBsetCol A + 64, 15 + A \ 4, 0, 15 + A \ 4
          DQBsetCol A + 128, 31 + A \ 4, 0, 31 + A \ 4
          DQBsetCol A + 192, 47 + A \ 4, 0, 47 + A \ 4
        NEXT A
      CASE 8
        FOR A = 0 TO 63
          DQBsetCol A, 0, 0, 0
          DQBsetCol A + 64, A \ 2, A \ 2, A \ 2
          DQBsetCol A + 128, 31 + A \ 2, 31 + A \ 2, 31 + A \ 2
          DQBsetCol A + 192, 63, 63, 63
        NEXT A
      CASE 9
        FOR A = 1 TO 255
          DQBsetCol A, ABS(A MOD 8 - 4) * 15, ABS(A MOD 8 - 4) * 15, ABS(A MOD 8 - 4) * 15
        NEXT A
      END SELECT
    END IF
    KeyC = FALSE
  ELSE
    KeyC = TRUE
  END IF

  IF DQBkey(KEYESC) THEN EXIT DO

  'hoeken van licht... gedoe
  Licht1Hoek1 = Licht1Hoek1 + licht1hoek1erbij
  licht1hoek2 = licht1hoek2 + licht1hoek2erbij
  licht2hoek1 = licht2hoek1 + licht2hoek1erbij
  licht2hoek2 = licht2hoek2 + licht2hoek2erbij
  licht1hoek1erbij = licht1hoek1erbij + (RND - .5) / 5000
  licht1hoek2erbij = licht1hoek2erbij + (RND - .5) / 5000
  licht2hoek1erbij = licht2hoek1erbij + (RND - .5) / 5000
  licht2hoek2erbij = licht2hoek2erbij + (RND - .5) / 5000
  
  LightSource(1).x = COS(Licht1Hoek1) * COS(licht1hoek2) * 100
  LightSource(1).Z = SIN(Licht1Hoek1) * COS(licht1hoek2) * 100
  LightSource(1).y = SIN(licht1hoek2) * 100
  LightSource(2).x = COS(licht2hoek1) * COS(licht2hoek2) * 100
  LightSource(2).Z = SIN(licht2hoek1) * COS(licht2hoek2) * 100
  LightSource(2).y = SIN(licht2hoek2) * 100

  'Keep the processing in this specific order
  ProcessObjects objects(0), 1, 1, viewport
  ProcessPoints points(0), PointsOutput(0), objects(0), 1, 14
  ProcessPolygons points(0), PolygonS(0), objects(0), 1, 24, viewport
  ProcessLight2 1, 1, 1, 24
  ProcessViewport points(0), PointsOutput(0), objects(0), 1, 14, viewport
  IF OptionP THEN ProcessPerspective PointsOutput(0), 1, 14
 
  ProcessSort PolygonS(), MinDistance, DrawOrder(0)
 
  'FOR a = 1 TO 24
  '  DrawOrder(a) = a
  'NEXT a

  FPS = FPS + 1
  IF timer2 > fpstime + 18 THEN
    steadyfps = FPS
    FPS = 0
    fpstime = timer2
  END IF

  IF OptionV THEN DQBwait 1
  DQBcopyLayer 1, VIDEO
  DQBclearLayer 1

  IF OptionI THEN
  FOR A = 1 TO 24
    DQBprint 1, "ú", A * 8 + 127, 0, 128
  NEXT A
  END IF

  FOR A = 1 TO 24

    l = DrawOrder(A)

    IF l > -1 THEN
      Drawing (l)
      IF OptionI THEN DQBprint 1, "þ", A * 8 + 127, 0, 128
    END IF
  NEXT A

  IF OptionM THEN DQBprint 1, STR$(steadyfps), 0, 0, 128
  IF OptionO THEN
    DQBprint 1, STR$(viewport.x), 0, 16, 128
    DQBprint 1, STR$(viewport.y), 0, 24, 128
    DQBprint 1, STR$(viewport.Z), 0, 32, 128
    DQBprint 1, STR$(viewport.VerticalAngle), 0, 48, 128
    DQBprint 1, STR$(viewport.WidthAngle), 0, 56, 128
    DQBprint 1, STR$(viewport.DepthAngle), 0, 64, 128
    DQBprint 1, STR$(objects(1).VerticalAngle), 0, 80, 128
    DQBprint 1, STR$(objects(1).WidthAngle), 0, 88, 128
    DQBprint 1, STR$(objects(1).DepthAngle), 0, 96, 128
  END IF

  LOOP

END SUB

REM $STATIC
SUB Drawing (N AS INTEGER)

  eenx = PointsOutput(PolygonS(N).PPoint).x + CenterX
  eeny = PointsOutput(PolygonS(N).PPoint).y + CenterY
  tweex = PointsOutput(PolygonN(PolygonS(N).NextPoint).PPoint).x + CenterX
  tweey = PointsOutput(PolygonN(PolygonS(N).NextPoint).PPoint).y + CenterY
  driex = PointsOutput(PolygonN(PolygonN(PolygonS(N).NextPoint).NextPoint).PPoint).x + CenterX
  driey = PointsOutput(PolygonN(PolygonN(PolygonS(N).NextPoint).NextPoint).PPoint).y + CenterY
 
  Een = PolygonS(N).R
  twee = PolygonN(PolygonS(N).NextPoint).R
  drie = PolygonN(PolygonN(PolygonS(N).NextPoint).NextPoint).R

  IF OptionF THEN
    DQBgtri 1, eenx, eeny, Een, tweex, tweey, twee, driex, driey, drie
  ELSE
    DQBgline 1, eenx, eeny, tweex, tweey, Een, twee
    DQBgline 1, tweex, tweey, driex, driey, twee, drie
    DQBgline 1, driex, driey, eenx, eeny, drie, Een
  END IF

END SUB

DEFINT A-Z
SUB IntroScreen

CLS
COLOR 11
PRINT "    3D STAR      by Badjas  from -+= Future Software =+-"
PRINT "   version 2     and V1ctor"
PRINT
COLOR 10
PRINT "commands:"
COLOR 3
PRINT "Lets see, I better give a list. The commands to give have been increased"
PRINT "a little since the first demo."
PRINT
COLOR 9
PRINT "Arrow keys plus Z and X: turn object."
COLOR 8
PRINT "A, W, S, D, Q, E to turn 'yourself' i.e.: te viewport."
COLOR 9
PRINT "R and T to fly forewards or backwards. Y is also for viewport movement"
PRINT "but I made that movement to test out stuff. What it does?"
PRINT "turn you around the object and set the viewport to look at it."
COLOR 8
PRINT "and 2, 3, 4, 6, 8 and 9 are to move the viewport the basic way"
PRINT "but I am afraid you cant use the right numbers... :)"
COLOR 9
PRINT "P lets you switch between perspective and isometric."
COLOR 8
PRINT "F lets you switch between a solid object or a wireframe."
PRINT
COLOR 10
PRINT "erm, other keys to show info:"
COLOR 8
PRINT "M shows the framerate."
COLOR 9
PRINT "I shows what to draw and what not to draw. there is a filter for that which"
PRINT "you can set via a simple variable."
COLOR 8
PRINT "C changes the color."
COLOR 9
PRINT "V if you want to wait for the vertical retrace."
COLOR 8
PRINT "O shows some stuff about the angles and coordinates";

WHILE INKEY$ <> "": WEND

k$ = INPUT$(1)
CLS

COLOR 11
PRINT "    3D STAR      by Badjas  from -+= Future Software =+-"
PRINT "   version 2     and V1ctor"
PRINT
COLOR 4
PRINT "Remarks:"
COLOR 3
PRINT "You may get a devide by zero error because of the DQBGTri function"
PRINT "the DQBGLine has no problems, so switching to wireframe mode is safe"
PRINT "the error shows up when there are triangles drawn below the bottom of"
PRINT "the screen. The 'what to draw filter' helps to get rid of this problem"
PRINT "but it is not accurate enough to get rid of it completely"
PRINT
COLOR 10
PRINT "Credit:"
COLOR 2
PRINT "Done with the help of DirectQB with its nice DQBGTRI function"
PRINT "DirectQB was made by Angelo Mottola, who once joined our forces"
PRINT "with his programming group Enhanced Creations"
PRINT
COLOR 4
PRINT "   Visit Future Software  at  HTTP://WWW.QB45.COM"
PRINT
COLOR 14
PRINT "Greetings,"
PRINT "Badjas"
PRINT
COLOR 3
PRINT "Please send any comments to:    Badjas @ QB45.COM";
COLOR 7

WHILE INKEY$ <> "": WEND

k$ = INPUT$(1)

END SUB

