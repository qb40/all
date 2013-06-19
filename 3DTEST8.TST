'3d Formula for Coordinate Axes
'perspx=perspective             ;declarations
'perspy=(perspx*xres)/yres
'objectx,objecty,objectz
'xpos,ypos,zpos
'camerax,cameray,cameraz
'objanga,objangb,objangc
'camanga,camangb,camangc
'xcentre,ycentre

'x1=xpos-objectx
'y1=ypos-objecty
'z1=zpos-objectz
'y2=y1*cos(objanga)+z1*sin(objanga)
'z2=z1*cos(objanga)-y1*sin(objanga)
'x2=x1*cos(objangb)+z2*sin(objangb)
'z3=z2*cos(objangb)-x1*sin(objangb)
'y3=y2*cos(objangc)+x2*sin(objangc)
'x3=x2*cos(objangc)-y2*sin(objangc)
'x1=x3+objectx-camerax
'y1=y3+objecty-cameray
'z1=z3+objectz-cameraz
'y2=y1*cos(camanga)+z1*sin(camanga)
'z2=z1*cos(camanga)-y1*sin(camanga)
'x2=x1*cos(camangb)+z2*sin(camangb)
'z3=z2*cos(camangb)-x1*sin(camangb)
'y3=y2*cos(camangc)+x2*sin(camangc)
'x3=x2*cos(camangc)-y2*sin(camangc)
'xf=(perspx*x3)/(z3+perspx)
'yf=(perspy*y3)/(z3+perspy)
'xdone=xf+xcentre
'ydone=yf+ycentre

'objectx,objecty,objectz - point of rotation of object
'camerax,cameray,cameraz - point of camera looking at the world you make
'xpos,ypos,zpos - position of pixel in the world coords
'xcentre,ycentre - centre of vision on the screen

'Coordinate Axes
'   ^y
'   |
'   |
'   |
'   ----->x
' /
'z
'Computer Axes
'----->x
'|\
'|  \z
'|
'y


'Sampling Computer Axes ...
'Press a key
DIM sine(359), cosine(359)
FOR i% = 0 TO 359
sine(i%) = SIN((CSNG(i%) / 180) * 3.14)
cosine(i%) = COS((CSNG(i%) / 180) * 3.14)
NEXT
SCREEN 9, 1, 0
TYPE objects
x AS SINGLE
y AS SINGLE
z AS SINGLE
clr AS INTEGER
END TYPE
DIM obj(18) AS objects
FOR i% = 0 TO UBOUND(obj)
RANDOMIZE TIMER
READ obj(i%).x, obj(i%).y, obj(i%).z
obj(i%).clr = i% MOD 256
NEXT
k$ = INPUT$(1)

'Some initializations
perspx = 250
perspy = (perspx * 320) / 200
objectx = 0
objecty = 0
objectz = 0
camerax = -100
cameray = -100
cameraz = -100
xcentre = 150
ycentre = 200
objanga = 0
objangb = 0
objangc = 0
camanga = 0
camangb = 0
camangc = 0

DIM xdone(UBOUND(obj)), ydone(UBOUND(obj))
i% = 1
a1 = TIMER
t = .01
DO
FOR j% = 0 TO UBOUND(obj)
x1 = obj(j%).x - objectx
y1 = (obj(j%).y - objecty)
z1 = (obj(j%).z - objectz)
y2 = y1 * COS(objanga) + z1 * SIN(objanga)
z2 = z1 * COS(objanga) - y1 * SIN(objanga)
x2 = x1 * COS(objangb) + z2 * SIN(objangb)
z3 = z2 * COS(objangb) - x1 * SIN(objangb)
y3 = y2 * COS(objangc) + x2 * SIN(objangc)
x3 = x2 * COS(objangc) - y2 * SIN(objangc)
x1 = x3 + objectx - camerax
y1 = (y3 + objecty - cameray)
z1 = (z3 + objectz - cameraz)
y2 = y1 * COS(camanga) + z1 * SIN(camanga)
z2 = z1 * COS(camanga) - y1 * SIN(camanga)
x2 = x1 * COS(camangb) + z2 * SIN(camangb)
z3 = z2 * COS(camangb) - x1 * SIN(camangb)
y3 = y2 * COS(camangc) + x2 * SIN(camangc)
x3 = x2 * COS(camangc) - y2 * SIN(camangc)
xf = (perspx * x3) / (z3 + perspx)
yf = (perspx * y3) / (z3 + perspx)
xdone(j%) = xf + xcentre
ydone(j%) = yf + ycentre
NEXT
CLS
SELECT CASE i%
CASE 0
FOR k% = 0 TO UBOUND(obj)
LINE (xcentre, ycentre)-(xdone(k%), ydone(k%)), obj(k%).clr
NEXT
CASE 1
LINE (xcentre, ycentre)-(xcentre, ycentre), 0
FOR k% = 0 TO UBOUND(obj)
LINE -(xdone(k%), ydone(k%)), obj(k%).clr
NEXT
CASE ELSE
END SELECT
PCOPY 1, 0
k$ = INKEY$
IF (k$ = "") THEN GOTO cont
k$ = LCASE$(k$)
SELECT CASE k$
CASE CHR$(27)
i% = (i% + 1) MOD 2
CASE "w"
cameray = cameray - 3
CASE "s"
cameray = cameray + 3
CASE "a"
camerax = camerax - 3
CASE "d"
camerax = camerax + 3
CASE "r"
cameraz = cameraz - 10
CASE "f"
cameraz = cameraz + 10
CASE "y"
camanga = camanga + .01
CASE "h"
camanga = camanga - .01
CASE "u"
camangb = camangb + .01
CASE "j"
camangb = camangb - .01
CASE "i"
camangc = camangc + .01
CASE "k"
camangc = camangc - .01
CASE "4"
objanga = objanga + .01
CASE "1"
objangb = objangb - .01
CASE "5"
objangb = objangb + .01
CASE "2"
objanga = objanga - .01
CASE "6"
objangc = objangc + .01
CASE "3"
objangc = objangc - .01
CASE ELSE
END SELECT
cont:
LOOP
'zxy
DATA 0,0,0
DATA 0,50,0
DATA 0,50,50
DATA 0,0,50
DATA 0,0,0
DATA 50,0,0
DATA 50,50,0
DATA 0,50,0
DATA 0,0,0
DATA 50,0,0
DATA 50,0,50
DATA 50,50,50
DATA 50,50,0
DATA 50,0,0
DATA 50,0,50
DATA 0,0,50
DATA 0,50,50
DATA 50,50,50
DATA 50,0,50


