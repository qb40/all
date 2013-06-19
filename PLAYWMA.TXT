'check
SCREEN 13
INPUT "Movie file name"; ff$
INPUT "Time gap"; t

OPEN "B", #3, ff$ + ".wmp"
SEEK #3, 1
read$ = INPUT$(2, #3)
ss = ASC(LEFT$(read$, 1)) + ASC(RIGHT$(read$, 1))

SEEK #3, 3
read$ = INPUT$(2, #3)
xx = ASC(LEFT$(read$, 1)) + ASC(RIGHT$(read$, 1))
SEEK #3, 5
read$ = INPUT$(2, #3)
yy = ASC(LEFT$(read$, 1)) + ASC(RIGHT$(read$, 1))

CLS
SCREEN 13

FOR i = 1 TO ss
FOR y = yy TO 0 STEP -1
FOR x = 0 TO xx
cl = ASC(INPUT$(1, #3) + null$)
LINE (x, y)-(x, y), cl
NEXT
NEXT
SOUND 21000, t
NEXT

CLOSE #3
SYSTEM

