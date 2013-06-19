'Vic Luce 3D formula
'x2=-x*sin@+y*cos@
'y2=-x*cos@*sin#-y*sin@*sin#-z*cos#  +p
'z2=-x*cos@*cos#-y*sin@*cos#+z*sin#
'x3=256*(x2/(z2+zcenter))+xcenter
'y3=256*(y2/(z2+zcenter))+ycenter
DIM sine(359), cosine(359)
FOR i% = 0 TO 359
sine(i%) = SIN((CSNG(i%) / 180) * 3.14)
cosine(i%) = COS((CSNG(i%) / 180) * 3.14)
NEXT
SCREEN 13
TYPE objects
x AS SINGLE
y AS SINGLE
z AS SINGLE
clr AS INTEGER
END TYPE
k$ = INPUT$(1)
DIM obj(3) AS objects
FOR i% = 0 TO UBOUND(obj)
READ obj(i%).x, obj(i%).y, obj(i%).z
obj(i%).clr = i% MOD 256
NEXT
xcentre = 150
ycentre = 100
zcentre = 256
theta = 0
phi = 0
DIM x3(3), y3(3)

a = TIMER
t = 360 / (62800)
FOR i = 0 TO 359 STEP t
FOR j% = 0 TO UBOUND(obj)
x2 = -obj(j%).x * SIN(theta) + obj(j%).y * COS(theta)
y2 = -obj(j%).x * COS(theta) * SIN(phi) - obj(j%).y * SIN(theta) * SIN(phi) - obj(j%).z * COS(phi)
z2 = -obj(j%).x * COS(theta) * COS(phi) - obj(j%).y * SIN(theta) * COS(phi) + obj(j%).z * SIN(phi)
x3(j%) = 1256 * (x2 / (z2 + zcentre)) + xcentre
y3(j%) = 1256 * (y2 / (z2 + zcentre)) + ycentre
NEXT
LINE (x3(0), y3(0))-(x3(1), y3(1)), 1
LINE -(x3(2), y3(2)), 2
LINE -(x3(3), y3(3)), 3
LINE -(x3(0), y3(0)), 4
phi = phi + .0001
theta = theta + .0001
NEXT
PRINT "Vic Luce="; TIMER - a
DATA 0,0,0
DATA 0,10,0
DATA 10,10,0
DATA 10,0,0

