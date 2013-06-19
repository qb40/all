'handling the pc speaker
'connect speaker to timer2
val1% = INP(&H61)
val1% = val1% OR 3
OUT &H61, val1%
stp% = 10
FOR freq% = 8000 TO 12000 STEP stp%
countdown& = 1193180 \ freq%
'tell timer2 that we are about to load a new countdown value
OUT &H43, &HB6
'send the low byte and high byte of new countdown value
low& = countdown& MOD 256
high& = countdown& \ 256
OUT &H42, low&
OUT &H42, high&
FOR i% = 1 TO 10000
NEXT
IF (freq% = 11990) THEN freq% = 8000
NEXT
'disconnect speaker from timer2
val1% = INP(&H61)
val1% = val1% AND 252
OUT &H61, val1%



