'MKBios16:
'
'This program comes with VFont3 and its sole purpose in life is to create the
'file "BIOS16.FNT". I had to make this so I could get VFont3 to work so I
'could get VFontEdit to work. It's a long-running chain of work to do.
'
'This needs at least QB4.5 to run, unlike the other programs in this package,
'simply because it's not an important program and I can't be bothered to write
'some ASM just to call one interrupt.
'
'This isn't copyrighted, but if it does any damage to anything it's not my
'fault.
'
'   -- Sam Thursfield, 18 December 1999.

DEFINT A-Z

TYPE RegType
    AX      AS INTEGER
    BX      AS INTEGER
    CX      AS INTEGER
    DX      AS INTEGER
    BP      AS INTEGER
    SI      AS INTEGER
    DI      AS INTEGER
    FLAGS   AS INTEGER
    DS      AS INTEGER
    ES      AS INTEGER
END TYPE

DIM Font(255, 16) AS INTEGER
DIM Regs AS RegType

Regs.AX = &H1130
Regs.BX = &H600
CALL interruptx(&H10, Regs, Regs)
FontSeg = Regs.ES
FontAdrs = Regs.BP


FOR I = 0 TO 255
    MemPtr = 16 * I + FontAdrs
    FOR l = 0 TO 15             'Do each line
        Font(I, 0) = 8
        DEF SEG = FontSeg
            BitPattern = PEEK(MemPtr + l)
        DEF SEG
        Font(I, l + 1) = BitPattern
    NEXT
NEXT

DEF SEG = VARSEG(Font(0, 0))
    BSAVE "BIOS16.FNT", VARPTR(Font(0, 0)), 256 * 17 * 2
DEF SEG = 0


