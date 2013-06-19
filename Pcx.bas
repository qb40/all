DEFINT A-Z

DECLARE SUB ShowPCX (file$)

SCREEN 13
PRINT "AAAAAAAA"

ShowPCX "m2.pcx"

SLEEP
DO WHILE LEN(INKEY$): LOOP
END
SCREEN 0

SUB ShowPCX (file$)


DIM pcxpal      AS STRING * 768
DIM pcxversion  AS STRING * 1

  pcxnum = FREEFILE
  OPEN file$ FOR BINARY AS #pcxnum

  GET #pcxnum, 2, pcxversion

  IF ASC(pcxversion) = 5 THEN
    GET #1, LOF(pcxnum) - 767, pcxpal
   
    pal = 0

    FOR p = 1 TO 768 STEP 3
      OUT &H3C8, pal
      red% = INT(ASC(MID$(pcxpal, p, 1)) / 4)
      OUT &H3C9, red%
      green% = INT(ASC(MID$(pcxpal, p + 1, 1)) / 4)
      OUT &H3C9, green%
      blue% = INT(ASC(MID$(pcxpal, p + 2, 1)) / 4)
      OUT &H3C9, blue%
      pal = pal + 1
    NEXT p

  END IF
  
  SEEK #pcxnum, 129
 
  fxoffset = 0: fxsegment = &HA000
  datasize = 2000

  pcxdata$ = INPUT$(datasize, pcxnum)
  datacount = 1

  FOR half = 1 TO 2

    fxtotal = fxsegment + fxoffset
    DEF SEG = fxtotal

    FOR c = 0 TO 31999

      IF datacount > datasize THEN
        pcxdata$ = INPUT$(datasize, pcxnum)
        datacount = 1
      END IF

      clr = ASC(MID$(pcxdata$, datacount, 1) + CHR$(0))
      datacount = datacount + 1

      IF datacount > datasize THEN
        pcxdata$ = INPUT$(datasize, pcxnum)
        datacount = 1
      END IF
                    
      IF clr > 192 THEN
        LPS = clr - 192
        clr = ASC(MID$(pcxdata$, datacount, 1) + CHR$(0))
        datacount = datacount + 1
        FOR L = LPS TO 1 STEP -1
          POKE c, clr
          c = c + 1
        NEXT L
        c = c - 1
      ELSE
        POKE c, clr
      END IF
    NEXT c
    fxoffset = fxoffset + &H7D0
  NEXT half

  CLOSE pcxnum

  DEF SEG

END SUB

