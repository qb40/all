DECLARE FUNCTION c.loadstring$ (fl1$)
DECLARE SUB dma.makeready (channel%, seg1%, off1%, transfer%, initialize%, address%, mode%, length%)
DECLARE SUB dma.dotask ()
DECLARE SUB dma.pause ()
DECLARE SUB dma.resume ()
DECLARE SUB dma.stop ()

Engine$ = c.loadstring$("dma.dll")
DIM Status%(11)


CLS
dma.makeready 1, &HB800, &H0, 1, 0, 0, 1, &HFFFF
FOR i% = 0 TO UBOUND(Status%)
PRINT Status%(i%);
NEXT
dma.dotask
dma.pause

FUNCTION c.loadstring$ (fl1$)
fr% = FREEFILE
OPEN "B", #fr%, fl1$
c.loadstring$ = INPUT$(LOF(fr%), #fr%)
CLOSE #fr%
END FUNCTION

SUB dma.dotask
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 265)
END SUB

SUB dma.makeready (channel%, seg1%, off1%, transfer%, initialize%, address%, mode%, length%)
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), BYVAL channel%, BYVAL eg1%, BYVAL off1%, BYVAL transfer%, BYVAL initialize%, BYVAL address%, BYVAL mode%, BYVAL length%, SADD(Engine$))
END SUB

SUB dma.pause
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 336)
END SUB

SUB dma.resume
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 357)
END SUB

SUB dma.stop
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 378)
END SUB

