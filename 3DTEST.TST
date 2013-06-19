'Vic Luce Rotation formula
'x2=x*cos@+y*sin@
'y2=y*cos@-x*sin@
'My Rotation formula
'd=sqr(x*x+y*y)
'x2=d*cos@
'y2=d*sin@
'Both give exactly same result
'@ = theta = anti-clockwise angle(+ve angle)
'Vic Luce = 2.80
'Mine =1.64
SCREEN 13
k$ = INPUT$(1)
x = 100
y = 0
xcentre = 150
ycentre = 100
a = TIMER
FOR i = 0 TO 6.28 STEP .00001
PSET (xcentre + x * COS(i) + y * SIN(i), ycentre + y * COS(i) - x * SIN(i)), 1
NEXT
PRINT "Vic Luce="; TIMER - a
k$ = INPUT$(1)
a = TIMER
'd = SQR(x * x + y * y)
FOR i = 0 TO 6.28 STEP .00001
d = SQR(x * x + y * y)
PSET (xcentre + d * COS(i), ycentre - d * SIN(i)), 2
NEXT
PRINT "Mine="; TIMER - a

