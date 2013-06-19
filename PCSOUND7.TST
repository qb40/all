DECLARE SUB snd.start ()
DECLARE SUB snd.lohi (low%, high%)
DECLARE SUB snd.freq (freq%)
DECLARE SUB snd.stop ()
DECLARE SUB delay (seconds!)
DECLARE SUB delays (seconds!)
DECLARE SUB delayf (times&)
'handling the pc speaker
'whirring sound effect
stp% = 10
freq% = 2000
time! = 1 / 70
time& = 50000
snd.start
DO
freq2% = freq% * 3
freq3% = freq% \ 3
freq4% = (freq2% + freq3%) \ 2
freq5% = (freq4% + freq%) \ 2
snd.freq freq%
delayf time&
snd.freq freq2%
delayf time&
snd.freq freq3%
delayf time&
snd.freq freq4%
delayf time&
snd.freq freq5%
delayf time&
freq% = freq% + stp%
LOOP UNTIL (freq% > 3000)
snd.stop



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

