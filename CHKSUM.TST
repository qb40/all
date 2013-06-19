DECLARE FUNCTION sum# (a$)

ON ERROR GOTO errs
CLS
INPUT "File name"; f1$
OPEN "I", #1, f1$
CLOSE #1
OPEN "B", #1, f1$
length& = LOF(1)
PRINT "Length of file ="; length&; "bytes"
times& = length& \ 32000
SEEK #1, 1
FOR i& = 1 TO times&
checksum# = checksum# + sum#(INPUT$(32000, #1))
NEXT
checksum# = checksum# + sum#(INPUT$(length& MOD 32000, #1))
CLOSE #1
PRINT "Checksum:"; checksum#
END

errs:
CLOSE #1
PRINT "File not found."
SYSTEM

FUNCTION sum# (a$)
sm# = 0
FOR i% = 1 TO LEN(a$)
sm# = sm# + ASC(MID$(a$, i%, 1))
NEXT
sum# = sm#
END FUNCTION

