'successful test
OPEN "B", #1, "mouse2.dll"
FOR i = 1 TO LOF(1)
SEEK #1, i
k$ = INPUT$(1, #1)
mouse$ = mouse$ + k$
NEXT
CLOSE #1
DEF SEG = VARSEG(mouse$)
a% = SADD(mouse$)
CALL absolute(a%)
DO
k% = INP(&H60)
LOOP UNTIL k% = 129

