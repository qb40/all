'[Detecting a SOUND CARD]
'Detecting for AdLib compatible board

TYPE SoundBlaster
        address AS LONG
        datas AS LONG
END TYPE

DIM snd AS SoundBlaster

CLS
COLOR 10
PRINT "[Playing a simple sound using SOUND CARD] . . ."
COLOR 11
PRINT "Playing from AdLib compatible board . . ."
COLOR 8
PRINT ""

snd.address = &H338
snd.datas = &H339
voice% = &H11

PRINT "Initiating process."
PRINT ""

'clearing all registers
FOR i% = 1 TO 244      '244 total registers of sound card
OUT snd.address, i%
OUT snd.datas, &H0
NEXT

'setting normal values
OUT snd.address, &H20   'set modulator's multiple to 1
OUT snd.datas, &H1
OUT snd.address, &H40   'set modulator's level to about 40dB
OUT snd.datas, &H10
OUT snd.address, &H60   'modulator attack: quick; decay: long
OUT snd.datas, &HF0
OUT snd.address, &H80   'modulator sustain: medium; release: medium
OUT snd.datas, &H77
OUT snd.address, &HA0   'set voice frequency's LSB (D#)
OUT snd.datas, &H98
OUT snd.address, &H23   'set carrier's multiple to 1
OUT snd.datas, &H1
OUT snd.address, &H43   'set carrier to maximum volume(about 47 dB)
OUT snd.datas, &H0
OUT snd.address, &H63   'carrier attack: quick; decay: long
OUT snd.datas, &HF0
OUT snd.address, &H83   'carrier sustain: medium; release: medium
OUT snd.datas, &H77

OUT snd.address, &HB0   'set on\off
OUT snd.datas, voice%

'set timer control
OUT snd.address, &H4
OUT snd.datas, &H3

