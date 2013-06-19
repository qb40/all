'3d Formula for Coordinate Axes
'x2=-z*sin@+x*cos@
'y2=-z*cos@*sin#-x*sin@*sin#-y*cos#
'z2=-z*cos@*cos#-x*sin@*cos#+y*sin#
'x3=256*(x2/(z2+zcenter))+xcenter
'y3=256*(y2/(z2+zcenter))+ycenter
'3d Formula for Computer Axes
'x2=z*sin@+x*cos@
'y2=z*cos@*sin#-x*sin@*sin#+y*cos#
'z2=z*cos@*cos#-x*sin@*cos#-y*sin#
'x3=256*(x2/(z2+zcenter))+xcenter
'y3=256*(y2/(z2+zcenter))+ycenter
'@ = theta = Left-Right cockscrew up       (cockscrew=clockwise turn)
'# = phi   = Up-Down    cockscrew right
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
SCREEN 9, , 1, 0
TYPE objects
x AS SINGLE
y AS SINGLE
z AS SINGLE
clr AS INTEGER
END TYPE
DIM obj(30) AS objects
FOR i% = 0 TO UBOUND(obj)
RANDOMIZE TIMER
obj(i%).x = RND * 100
obj(i%).y = RND * 100
obj(i%).z = RND * 100
obj(i%).clr = i% MOD 256
NEXT
k$ = INPUT$(1)
xcentre = 250
ycentre = 150
zcentre = 256
theta = 0
phi = 0
DIM x3(UBOUND(obj)), y3(UBOUND(obj))
i% = 0
a = TIMER
t = .01
DO
FOR j% = 0 TO UBOUND(obj)
x2 = obj(j%).z * SIN(theta) + obj(j%).x * COS(theta)
y2 = obj(j%).z * COS(theta) * SIN(phi) - obj(j%).x * SIN(theta) * SIN(phi) + obj(j%).y * COS(phi)
z2 = obj(j%).z * COS(theta) * COS(phi) - obj(j%).x * SIN(theta) * COS(phi) - obj(j%).y * SIN(phi)
x3(j%) = 256 * (x2 / (z2 + zcentre)) + xcentre
y3(j%) = 256 * (y2 / (z2 + zcentre)) + ycentre
NEXT
IF (INKEY$ = CHR$(27)) THEN i% = (i% + 1) MOD 2
CLS
SELECT CASE i%
CASE 0
FOR k% = 0 TO UBOUND(obj)
LINE (xcentre, ycentre)-(x3(k%), y3(k%)), obj(k%).clr
NEXT
CASE 1
LINE (xcentre, ycentre)-(xcentre, ycentre), 0
FOR k% = 0 TO UBOUND(obj)
LINE -(x3(k%), y3(k%)), obj(k%).clr
NEXT
CASE ELSE
END SELECT
PCOPY 1, 0
theta = theta + .01
phi = phi + .01
LOOP
PRINT "Vic Luce="; TIMER - a
'zxy
DATA 50,0,0
DATA 0,50,0
DATA 0,0,50
DATA 0,0,0

