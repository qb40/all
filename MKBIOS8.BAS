'MKBios8:
'
'This program comes with VFont3 and its sole purpose in life is to create the
'file "BIOS8.FNT". I had to make this so I could get VLoadFont to work so I
'could get VFontEdit to work. It's a long-running chain of work to do. You may
'have noticed that this file is almost the same as MKBios16.BAS. This file
'came first but then I made MKBios16 and started again on this. So, anyone
'that is sad enough to want to know, MKBios16 is the original.
'
'This only needs QBasic to run, unlike the MKBios16, because MKBios16 used an
'interrupt to find out the address of the 8*16 font. But I know the address of
'the 8*8 font anyway, so I don't need to call it.
'
'This isn't copyrighted, but if it does any damage to anything it's not my
'fault.
'
'   -- Sam Thursfield, 22 December 1999.
'
'For some reason, this seems to mess up all the characters after 127. I'll
'look into it some more (look out for VFont3.1). For now, you'll have to make
'the characters yourself somehow. If anyone knows how to fix this bug, email
'me on sam.thursfield@btinternet.com.
'
'   -- Sam Thursfield, 2 January 2000. (Yes, my comp rolled over right, but I
'                                       didn't watch it going ;^)


DEFINT A-Z

DIM Font(255, 16) AS INTEGER

FOR I = 0 TO 255
    MemPtr = 8 * I + &HE
    Font(I, 0) = 8
    FOR l = 0 TO 7             'Do each line
        DEF SEG = &HFFA6
            BitPattern = PEEK(MemPtr + l)
        DEF SEG
        Font(I, l + 1) = BitPattern
    NEXT
NEXT

DEF SEG = VARSEG(Font(0, 0))
    BSAVE "BIOS8.FNT", VARPTR(Font(0, 0)), 256 * 17 * 2
DEF SEG = 0


