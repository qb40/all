'successful test
CLS
OPEN "B", #1, "mouse4.dll"
FOR i = 1 TO LOF(1)
SEEK #1, i
k$ = INPUT$(1, #1)
mouse$ = mouse$ + k$
NEXT
CLOSE #1
DEF SEG = VARSEG(mouse$)
a% = SADD(mouse$)
xx% = 1
yy% = 1

DO
CALL absolute(a%)
'LOCATE 1, 1
a1& = PEEK(0)
a2& = PEEK(1)
a1& = a1& * 256 + a2&
'PRINT "Unknown value = "; a1&; "      "
'LOCATE 2, 1
a1% = PEEK(2)
a2% = PEEK(3)
PRINT "x-axis = "; a1%; "      ",
PRINT "Y-axis = "; a2%; "      ",
xx% = xx% + (a1% AND 127)
yy% = xx% + (a2% AND 127)
IF (xx% < 1) THEN xx% = 1
IF (yy% < 1) THEN yy% = 1
IF (xx% > 79) THEN xx% = 79
IF (yy% > 24) THEN yy% = 24
'LOCATE 4, 1
a1& = PEEK(4)
a2& = PEEK(5)
a1& = a1& * 256 + a2&
PRINT "Button inf = "; a1&; "      "
k% = INP(&H60)
LOCATE yy%, xx%
PRINT CHR$(200)
LOOP UNTIL k% = 129

