DECLARE SUB sclear (mx AS INTEGER, my AS INTEGER, f AS INTEGER, t AS DOUBLE)
DECLARE SUB controls ()
CLS
SCREEN 12
COLOR 12
PRINT SPC(20); "Basic Piano"
COLOR 11
PRINT SPC(20); "-----------"
COLOR 10
PRINT ""
PRINT SPC(50); "- by Subhajit sahu"
COLOR 9
k$ = INPUT$(1)
controls

SUB controls
CLS
COLOR 12
PRINT "Controls"
COLOR 4
PRINT "--------"
COLOR 5
PRINT ""
PRINT ""
PRINT "(A-Z) - Music drop and back"
PRINT "8 - Increase current frequency"
PRINT "2 - Decrease current frequnecy"
PRINT "6 - Increase frequency change rate"
PRINT "4 - Decrease frequency change rate"
PRINT "+ - Increase time passage speed"
PRINT "- - Decrease time passage speed"
PRINT "[ - Increase frequency speading range"
PRINT "] - Decrease frequency spreading range"
PRINT "\ - Mute music(on/off)"
PRINT ", - Music drop up"
PRINT ". - Music drop down"
PRINT "Esc - Save and exit"
PRINT ""
COLOR 1
PRINT "Press any key to continue"
k$ = INPUT$(1)
CLS
END SUB


SUB sclear (mx AS INTEGER, my AS INTEGER, f AS INTEGER, t AS DOUBLE)
xi = mx / my
FOR y = my TO 0 STEP -1
x = FIX(xi * y)
LINE (0, my)-(x, y), 1: LINE (mx, 0)-(x, y), 1
y1 = y + 1
x1 = FIX(xi * y1)
LINE (0, my)-(x1, y1), 0: LINE (mx, 0)-(x1, y1), 0
LINE (0, my)-(x1 - 1, y - 1), 1: LINE (mx, 0)-(x - 1, y - 1), 1
y1 = y - 5
x1 = FIX(xi * y1): x = x1
LINE (0, my)-(x1, y1), 2: LINE (mx, 0)-(x1, y1), 2
y1 = y1 + 1
x1 = FIX(xi * y1)
LINE (0, my)-(x1, y1), 0: LINE (mx, 0)-(x1, y1), 0
LINE (0, my)-(x - 1, y - 6), 2: LINE (mx, 0)-(x - 1, y - 6), 2
y1 = y - 10
x1 = FIX(xi * y1): x = x1
LINE (0, my)-(x1, y1), 3: LINE (mx, 0)-(x1, y1), 3
y1 = y1 + 1
x1 = FIX(xi * y1)
LINE (0, my)-(x1, y1), 0: LINE (mx, 0)-(x1, y1), 0
LINE (0, my)-(x - 1, y - 11), 3: LINE (mx, 0)-(x - 1, y - 11), 3
SOUND f, t
NEXT
END SUB
