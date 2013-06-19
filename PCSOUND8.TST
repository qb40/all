DECLARE SUB snd.start ()
DECLARE SUB snd.lohi (low%, high%)
DECLARE SUB snd.freq (freq%)
DECLARE SUB snd.stop ()
DECLARE SUB delay (seconds!)
DECLARE SUB delays (seconds!)
DECLARE SUB delayf (times&)
'handling the pc speaker
'file sound effect
CLS
INPUT "File Name"; fl1$
stp% = 10
freq% = 2000
time! = 1 / 70
time& = 50000
OPEN "B", #1, fl1$
pos1& = 1
length& = LOF(1)
snd.start
DO UNTIL (pos1& >= length&)
GET #1, pos1&, freq%
pos1& = pos1& + 4
freq% = ABS(freq%)
IF (freq% < 100) THEN freq% = 100
snd.freq ABS(freq%)
delayf time&
IF (INKEY$ = CHR$(27)) THEN EXIT DO
LOOP
snd.stop
CLOSE #1


SUB delay (seconds!)
FOR lv1% = 0 TO INT(70 * seconds!)
WAIT &H3DA, 8
WAIT &H3DA, 8, 8
NEXT
END SUB

SUB delayf (times&)
FOR i& = 1 TO times&
NEXT
END SUB

SUB delays (seconds!)
tm% = INT(seconds! * 19)
DEF SEG = 0
POKE 1132, 0
DO WHILE (PEEK(1132) < tm%)
LOOP
DEF SEG
END SUB

SUB snd.freq (freq%)
countdown& = 1193180 \ freq%  'calculate countdown
low& = countdown& MOD 256'send the lowbyte and highbyte of new countdown value
high& = countdown& \ 256
OUT &H43, &HB6    'tell timer2 that we are about to load a new countdown value
OUT &H42, low&
OUT &H42, high&
END SUB

SUB snd.lohi (low%, high%)
OUT &H43, &HB6    'tell timer2 that we are about to load a new countdown value
OUT &H42, low%
OUT &H42, high%
END SUB

SUB snd.start
val1% = INP(&H61)       'connect speaker to timer2
val1% = val1% OR 3
OUT &H61, val1%
END SUB

SUB snd.stop
'disconnect speaker from timer2
val1% = INP(&H61)
val1% = val1% AND 252
OUT &H61, val1%
END SUB

