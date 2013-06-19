DECLARE FUNCTION c.loadstring$ (fl1$)

DECLARE FUNCTION ems.error$ (reason%)
DECLARE SUB ems.err ()
DECLARE FUNCTION ems.check% ()
DECLARE FUNCTION ems.version$ ()
DECLARE FUNCTION ems.pageframe% ()
DECLARE FUNCTION ems.totalhandles% ()
DECLARE FUNCTION ems.usedhandles% ()
DECLARE FUNCTION ems.noofpages% ()
DECLARE FUNCTION ems.gethandle% (pages%)
DECLARE SUB ems.releasehandle (handle%)
DECLARE SUB ems.mapmemory (physicalpage%, logicalpage%, handle%)
DECLARE SUB ems.mappages (PageBuffer%(), mappages%, handle%)
DECLARE SUB ems.copymemory (PageBuffer$)
DECLARE SUB ems.xchgmemory (PageBuffer$)

Engine$ = c.loadstring$("ems.dll")
DIM Status%(2)


CLS
IF (ems.check% = -1) THEN
PRINT "EMS memory was not found. Supply EMS by changing properties."
k$ = INPUT$(1)
SYSTEM
END IF
COLOR 15
PRINT "Congratulations! EMS memory was found."
COLOR 14
PRINT "Extended Memory Manager (EMM)"
PRINT "Version:"; ems.version$
PRINT "PageFrame: "; HEX$(ems.pageframe%) + "h"
PRINT "Total EMM handles:"; ems.totalhandles%
PRINT "Used EMM handles:"; ems.usedhandles%
PRINT "Number of EMS pages:"; ems.noofpages%
PRINT "Total EMS Memory:"; (ems.noofpages% * 16) / 1024; "M"
COLOR 13
a1% = ems.pageframe%
a2% = ems.gethandle%(2)
ems.mapmemory 0, 0, a2%
DEF SEG = a1%
POKE 0, 1
PRINT "1 poked to EMS page 0"
ems.mapmemory 0, 1, a2%
PRINT "Data found at page 1"; PEEK(0)
ems.mapmemory 0, 0, a2%
PRINT "Data found at page 0"; PEEK(0)
ems.releasehandle a2%
DEF SEG
PRINT "Handle released"

FUNCTION c.loadstring$ (fl1$)
fr% = FREEFILE
OPEN "B", #fr%, fl1$
c.loadstring$ = INPUT$(LOF(fr%), #fr%)
CLOSE #fr%
END FUNCTION

FUNCTION ems.check%
SHARED Engine$
a1$ = SPACE$(8)
CALL absolute(BYVAL VARSEG(a1$), BYVAL SADD(a1$), SADD(Engine$))
ems.check% = -1
IF (a1$ = "EMMXXXX0") THEN ems.check% = 0
END FUNCTION

SUB ems.copymemory (PageBuffer$)
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), BYVAL VARSEG(PageBuffer$), BYVAL SADD(PageBuffer$), SADD(Engine$) + 288)
ems.err
END SUB

SUB ems.err
SHARED Status%()
IF (Status%(0) <> &H0) THEN PRINT "EMS Error:"; ems.error$(Status%(0))
END SUB

FUNCTION ems.error$ (reason%)
SELECT CASE reason%
CASE &H0: ems.error$ = "successful"
CASE &H80: ems.error$ = "internal error"
CASE &H81: ems.error$ = "hardware malfunction"
CASE &H82: ems.error$ = "busy -- retry later"
CASE &H83: ems.error$ = "invalid handle"
CASE &H84: ems.error$ = "undefined function requested by application"
CASE &H85: ems.error$ = "no more handles available"
CASE &H86: ems.error$ = "error in save or restore of mapping context"
CASE &H87: ems.error$ = "insufficient memory pages in system"
CASE &H88: ems.error$ = "insufficient memory pages available"
CASE &H89: ems.error$ = "zero pages requested"
CASE &H8A: ems.error$ = "invalid logical page number encountered"
CASE &H8B: ems.error$ = "invalid physical page number encountered"
CASE &H8C: ems.error$ = "page-mapping hardware state save area is full"
CASE &H8D: ems.error$ = "save of mapping context failed"
CASE &H8E: ems.error$ = "restore of mapping context failed"
CASE &H8F: ems.error$ = "undefined subfunction"
CASE &H90: ems.error$ = "undefined attribute type"
CASE &H91: ems.error$ = "feature not supported"
CASE &H92: ems.error$ = "successful, but a portion of the source region has been overwritten"
CASE &H93: ems.error$ = "length of source or destination region exceeds length of region allocated to either source or destination handle"
CASE &H94: ems.error$ = "conventional and expanded memory regions overlap"
CASE &H95: ems.error$ = "offset within logical page exceeds size of logical page"
CASE &H96: ems.error$ = "region length exceeds 1 MB"
CASE &H97: ems.error$ = "source and destination EMS regions have same handle and overlap"
CASE &H98: ems.error$ = "memory source or destination type undefined"
CASE &H9A: ems.error$ = "specified alternate map register or DMA register set not supported"
CASE &H9B: ems.error$ = "all alternate map register or DMA register sets currently allocated"
CASE &H9C: ems.error$ = "alternate map register or DMA register sets not supported"
CASE &H9D: ems.error$ = "undefined or unallocated alternate map register or DMA register set"
CASE &H9E: ems.error$ = "dedicated DMA channels not supported"
CASE &H9F: ems.error$ = "specified dedicated DMA channel not supported"
CASE &HA0: ems.error$ = "no such handle name"
CASE &HA1: ems.error$ = "a handle found had no name, or duplicate handle name"
CASE &HA2: ems.error$ = "attempted to wrap around 1M conventional address space"
CASE &HA3: ems.error$ = "source array corrupted"
CASE &HA4: ems.error$ = "operating system denied access"
CASE ELSE: ems.error$ = "undefined error"
END SELECT
END FUNCTION

FUNCTION ems.gethandle% (pages%)
SHARED Engine$, Status%
CALL absolute(SEG Status%(0), BYVAL pages%, SADD(Engine$) + 171)
ems.gethandle% = Status%(1)
ems.err
END FUNCTION

SUB ems.mapmemory (physicalpage%, logicalpage%, handle%)
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), BYVAL physicalpage%, BYVAL logicalpage%, BYVAL handle%, SADD(Engine$) + 223)
ems.err
END SUB

SUB ems.mappages (PageBuffer%(), mappages%, handle%)
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SEG PageBuffer%(0), BYVAL mappages%, BYVAL handle%, SADD(Engine$) + 252)
ems.err
END SUB

FUNCTION ems.noofpages%
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 141)
ems.noofpages% = Status%(1)
ems.err
END FUNCTION

FUNCTION ems.pageframe%
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 62)
ems.pageframe% = Status%(1)
ems.err
END FUNCTION

SUB ems.releasehandle (handle%)
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), BYVAL handle%, SADD(Engine$) + 200)
ems.err
END SUB

FUNCTION ems.totalhandles%
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 88)
ems.totalhandles% = Status%(1)
ems.err
END FUNCTION

FUNCTION ems.usedhandles%
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 115)
ems.usedhandles% = Status%(1)
ems.err
END FUNCTION

FUNCTION ems.version$
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), SADD(Engine$) + 38)
a1% = Status%(1)
ems.version$ = STR$((a1% AND &HF0) \ &H10) + "." + STR$(.1 * (a1% AND &HF))
ems.err
END FUNCTION

SUB ems.xchgmemory (PageBuffer$)
SHARED Engine$, Status%()
CALL absolute(SEG Status%(0), BYVAL VARSEG(PageBuffer$), BYVAL SADD(PageBuffer$), SADD(Engine$) + 318)
ems.err
END SUB

