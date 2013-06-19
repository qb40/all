DECLARE FUNCTION bin1$ (a%)
DECLARE FUNCTION bin& (a%)
'maxkeys per second = 857 INP(&H60) command

CLS
OPEN "O", #1, "test.log"
DO
k% = INP(&H60)            'recieve key from buffer
PRINT k%, bin1$(k%); "b" 'Print keycode, binary code
PRINT #1, k%, bin1$(k%); "b"

DEF SEG = 0               'stop beep sound
POKE &H41A, PEEK(&H41C)   '(by clearing keyboard buffer)
DEF SEG                   '(Qbasic tip)
IF (k% = 129) THEN EXIT DO
LOOP
CLOSE #1

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

