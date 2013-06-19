DECLARE FUNCTION peekb$ (addr&)
DECLARE FUNCTION peekw$ (addr&)
DECLARE FUNCTION peekd$ (addr&)
DECLARE FUNCTION tell$ (flag%, bit%)

CLS
COLOR 15
PRINT "Computer information"
PRINT "--------------------"
PRINT ""
COLOR 14
DEF SEG = &H40
PRINT "COM1 address:"; peekw$(&H0)
PRINT "COM2 address:"; peekw$(&H2)
PRINT "COM3 address:"; peekw$(&H4)
PRINT "COM4 address:"; peekw$(&H6)
PRINT "LPT1 address:"; peekw$(&H8)
PRINT "LPT2 address:"; peekw$(&HA)
PRINT "LPT3 address:"; peekw$(&HC)
PRINT "LPT4 address:"; peekw$(&HE)
PRINT ""
PRINT "Equipment List:"
flag% = PEEK(&H10)
PRINT "1 IPL diskette"; tell$(flag%, 0)
PRINT "2 Math coprocessor"; tell$(flag%, 1)
PRINT "3 Pointing device(PS/2)"; tell$(flag%, 2)
PRINT "4 Old PC system board RAM < 256K"; tell$(flag%, 3)
PRINT "5 Initial video mode"; (flag% AND &H30) \ &H10
PRINT "6 Number of diskette drives"; ((flag% AND &HC0) \ 64) + 1
flag% = PEEK(&H11)
PRINT "7 Direct Memory Access(DMA)"; tell$(NOT (flag%), 0)
PRINT "8 Number of serial ports"; (flag% AND &HE) \ 2
PRINT "9 Game adapter"; tell$(flag%, 4)
PRINT "10 Internal modem(PS/2)"; tell$(flag%, 5)
PRINT "11 Number of printer ports"; flag% \ 64
PRINT ""
PRINT "PCjr: Infrared keyboard link error count:"; peekb$(&H12)
PRINT "Memory Size in KB:"; PEEK(&H13) + CLNG(PEEK(&H14)) * &H100
PRINT "PS/2 BIOS control state:"; peekb$(&H16)

FUNCTION peekb$ (addr&)
peekb$ = " " + HEX$(PEEK(addr&)) + "h"
END FUNCTION

FUNCTION peekd$ (addr&)
peekd$ = " " + HEX$(PEEK(addr&) + PEEK(addr& + 1) * &H100 + CLNG(PEEK(addr& + 2)) * &H10000 + CLNG(PEEK(addr& + 3)) * &H1000000) + "h"
END FUNCTION

FUNCTION peekw$ (addr&)
peekw$ = " " + HEX$(PEEK(addr&) + CLNG(PEEK(addr& + 1)) * &H100) + "h"
END FUNCTION

FUNCTION tell$ (flag%, bit%)
IF ((flag% AND (2 ^ bit%)) = 1) THEN tell$ = " is present." ELSE tell$ = " is absent."
END FUNCTION

