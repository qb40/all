DECLARE FUNCTION crc (a AS INTEGER)
DECLARE FUNCTION nospc$ (s$)

INPUT "File name"; ff$
INPUT "Time gap"; t
CLS
SCREEN 13
OPEN "B", #1, ff$

x = 0
SEEK #1, 1
LINE (0, 0)-(0, 0), 1
FOR i = 1 TO LOF(1)
p = ASC(INPUT$(1, #1))
LINE -(x, p), i MOD 256
SOUND 21000, t
x = x + 10
IF (x > 300) THEN
x = 0
CLS
LINE (0, 0)-(0, 0), 1
END IF
NEXT
CLOSE #1

FUNCTION crc (a AS INTEGER)
IF (a < 0) THEN crc = 0 ELSE crc = a
END FUNCTION

FUNCTION nospc$ (s$)
FOR i = 1 TO LEN(s$)
a$ = MID$(s$, i, 1)
IF (a$ <> " ") THEN b$ = b$ + a$
NEXT
nospc$ = b$
END FUNCTION

