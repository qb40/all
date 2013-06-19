DECLARE FUNCTION sndtest% ()

TYPE SoundBlaster
        address AS LONG
        datas AS LONG
END TYPE
DIM snd AS SoundBlaster
snd.address = &H388
snd.datas = &H389

card% = sndtest%
k$ = INPUT$(1)

'IF (INP(snd.address) >= 128) THEN PRINT "Timer Expired" ELSE PRINT "Timer not expired"

'set LSI device
OUT snd.address, &H1      'FM waveform control enabled
OUT snd.datas, &H20

'set timer1 data
OUT snd.address, &H2      'set timer for max. time sample play
OUT snd.datas, &H0

'set timer2 data
OUT snd.address, &H3      'set timer for max. time sample play
OUT snd.datas, &H0

'set CSM mode/ Keyboard split
OUT snd.address, &H8      'select FM music mode
OUT snd.datas, &H0


'set Amp Mod,Vib,EG Typ,KSR,Modulator Frequency multiple
FOR reg% = &H20 TO &H35
OUT snd.address, reg%      'select all
OUT snd.datas, &HFF
NEXT

'set Level Key Scaling/Total level
FOR reg% = &H40 TO &H55
OUT snd.address, reg%      'set for loudest sound
OUT snd.datas, &H0
NEXT

'set Attack Rate,Decay Rate
FOR reg% = &H60 TO &H75
OUT snd.address, reg%      'set for slowest sound
OUT snd.datas, &H0
NEXT

'set Sustain Level,Release Rate
FOR reg% = &H80 TO &H95
OUT snd.address, reg%      'set for loudest and slowest sound
OUT snd.datas, &H0
NEXT

'set F-number
FOR reg% = &HA0 TO &HA8
OUT snd.address, reg%      'set for sound
OUT snd.datas, &HAA
NEXT

'set Octave,F-number,Key-On
FOR reg% = &HB0 TO &HB8
OUT snd.address, reg%      'set for sound
OUT snd.datas, &HF
NEXT

'set Feedback,Alogarithm
FOR reg% = &HC0 TO &HC8
OUT snd.address, reg%      'set for sound
OUT snd.datas, &HF
NEXT

'set Amplitude Modulation Depth,Vibratio Depth,Rhythm
OUT snd.address, &HBD      'set for good sound
OUT snd.datas, &HFF

'select waveform
FOR reg% = &HE0 TO &HF5
OUT snd.address, reg%      'set for changing waveform of sound
OUT snd.datas, &H0
NEXT

'set timer control byte
OUT snd.address, &H4      'set timer2
OUT snd.datas, &H2

'IF (INP(snd.address) >= 128) THEN PRINT "Timer Expired" ELSE PRINT "Timer not expired"

PRINT "Playing . . . "
FOR i% = 1 TO 30000
'set timer2 data
OUT snd.address, &H3      'set timer for max. time sample play
OUT snd.datas, &H0
'set timer control byte
OUT snd.address, &H4      'set timer2
OUT snd.datas, &H3
NEXT

FUNCTION sndtest%
'[Detecting a SOUND CARD]
'Detecting for AdLib compatible board

SHARED snd AS SoundBlaster

CLS
COLOR 10
PRINT "[Detecting a SOUND CARD] . . ."
COLOR 11
PRINT "Detecting for AdLib compatible board . . ."
COLOR 8
PRINT ""

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
FOR delay% = 1 TO 10
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
ans% = 0
ELSE
COLOR 14
PRINT "Sound Card not detected."
PRINT "The Sound card is not an AdLib compatible board."
ans% = -1
END IF
sndtest% = ans%
END FUNCTION

