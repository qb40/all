DECLARE FUNCTION bin1$ (a%)
DECLARE FUNCTION bin& (a%)
'maxkeys per second = 857 INP(&H60) command
'bit7=0 then the following key is kept pressed
'bit7=1 the pressed button is released
'byte=224 special key pressed,keycode

CLS
k1% = 0
k1$ = ""
DO
k% = INP(&H60)            'recieve key from buffer
IF (k% <> k1%) THEN
k1% = k%
k1$ = k1$ + STR$(k1%) + STR$(bin&(k1%))
LOCATE 1, 1
PRINT k1$
END IF
'PRINT k%, bin1$(k%); "b" 'Print keycode, binary code


DEF SEG = 0               'stop beep sound
POKE &H41A, PEEK(&H41C)   '(by clearing keyboard buffer)
DEF SEG                   '(Qbasic tip)

LOOP

FUNCTION bin& (a%)
b& = 0
c% = 128
DO
b& = (b& * 10) + SGN((a% AND c%))
c% = c% \ 2
LOOP UNTIL c% = 0
bin& = b&
END FUNCTION

FUNCTION bin1$ (a%)
b& = bin&(a%)
c$ = STR$(b&)
c$ = RTRIM$(LTRIM$(c$))
FOR i = LEN(c$) TO 7
c$ = "0" + c$
NEXT
bin1$ = c$
END FUNCTION

