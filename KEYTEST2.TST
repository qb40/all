'max=887 keys /s
DO
a& = 0
t$ = TIME$
DO
k = INP(&H60)
PRINT max&  ',k&,a&
a& = a& + 1
IF (TIME$ <> t$) THEN EXIT DO
LOOP
IF (a& > max&) THEN max& = a&
LOOP


