DECLARE SUB dma.init (seg1&, off1&)
DECLARE SUB dma.setchannel (chn1%)

TYPE dma
        page AS LONG
        offset AS LONG
        channel AS INTEGER
END TYPE
DIM Dma1 AS dma

dma.init 52456, 6534
dma.setchannel 3
PRINT Dma1.page, Dma1.offset, Dma1.channel

SUB dma.init (seg1&, off1&)
SHARED Dma1 AS dma
Dma1.page = (seg1& AND &HF000) \ &H1000
a1& = (seg1& AND &HFFF) * &H10
a1& = a1& + off1&
IF (a1& > &HFFFF) THEN
Dma1.page = Dma1.page + 1
'May be error is here, if so, then just delete the line
Dma1.offset = a1& AND &HFFFF
END IF
END SUB

SUB dma.setchannel (chn1%)
SHARED Dma1 AS dma
Dma1.channel = -1
IF (chn1% >= 0 AND chn1% <= 3) THEN
        vl% = INP(&HA)
        vl% = (vl% AND &HFC) OR chn1%
        OUT &HA, vl%
        vl% = INP(&HB)
        vl% = (vl% AND &HFC) OR chn1%
        OUT &HB, vl%
        Dma1.channel = INP(&HA) AND &H3
ELSEIF (chn1% >= 4 AND chn1% <= 7) THEN
        vl% = INP(&HD4)
        vl% = (vl% AND &HFC) OR chn1%
        OUT &HD4, vl%
        vl% = INP(&HD6)
        vl% = (vl% AND &HFC) OR chn1%
        OUT &HD6, vl%
        Dma1.channel = (INP(&HD4) AND &H3) + 4
END IF
END SUB

