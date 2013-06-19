'Vic Luce Rotation formula
'x2=x*cos@+y*sin@
'y2=y*cos@-x*sin@
'My Rotation formula
'd=sqr(x*x+y*y)
'x2=d*cos@
'y2=d*sin@
'Both give exactly same result
'Vic Luce = 6.43
'Mine =6.27(Mine doesnt work)(I am a fool)
'@ = theta = anti-clockwise angle(+ve angle)
DIM sine(359), cosine(359)
FOR i% = 0 TO 359
sine(i%) = SIN((CSNG(i%) / 180) * 3.14)
cosine(i%) = COS((CSNG(i%) / 180) * 3.14)
NEXT
SCREEN 13

k$ = INPUT$(1)
x = 50
y = 100
xcentre = 150
ycentre = 100
a = TIMER
t = 360 / (6280000)
FOR i = 0 TO 359 STEP t
PSET (xcentre + x * cosine(i) + y * sine(i), ycentre + y * cosine(i) - x * sine(i)), 1
NEXT
PRINT "Vic Luce="; TIMER - a
k$ = INPUT$(1)
a = TIMER
'd = SQR(x * x + y * y)
FOR i = 0 TO 359 STEP t
d = SQR(x * x + y * y)
PSET (xcentre + d * cosine(i), ycentre - d * sine(i)), 2
NEXT
PRINT "Mine="; TIMER - a

