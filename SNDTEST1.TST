'[Detecting a SOUND CARD]
'Detecting for AdLib compatible board

TYPE SoundBlaster
        address AS LONG
        datas AS LONG
END TYPE

DIM snd AS SoundBlaster

CLS
COLOR 10
PRINT "[Detecting a SOUND CARD] . . ."
COLOR 11
PRINT "Detecting for AdLib compatible board . . ."
COLOR 8
PRINT ""

snd.address = &H388
snd.datas = &H389

PRINT "Checking process started."
PRINT ""

'reset both timers
OUT snd.address, &H4
OUT snd.datas, &H60

'enable the interrupts
OUT snd.address, &H4
OUT snd.datas, &H80

'read the status register
result1% = INP(snd.address)

'write FFh to register 2(Timer 1)
OUT snd.address, &H2
OUT snd.datas, &HFF

'start timer1
OUT snd.address, &H4
OUT snd.datas, &H21


'delay for at least 80 microseconds
FOR delay% = 1 TO 20
dummy% = INP(snd.address)
NEXT

'read the status register
result2% = INP(snd.address)

'reset both timers and interrupts
OUT snd.address, &H4
OUT snd.datas, &H60
OUT snd.address, &H4
OUT snd.datas, &H80

'test the stored results
result1% = result1% AND &HE0
result2% = result2% AND &HE0
IF (result1% = &H0 AND result2% = &HC0) THEN
COLOR 14
PRINT "Sound Card detected."
PRINT "The Sound card is an AdLib compatible board."
ELSE
COLOR 14
PRINT "Sound Card not detected."
PRINT "The Sound card is not an AdLib compatible board."
END IF

