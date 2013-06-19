DEFINT A-Z

'$INCLUDE: 'mylib.bi'

DECLARE SUB ShowPCX (file$)


DIM OldX, OldY
DIM NewX, NewY

'$DYNAMIC
DIM img(0) AS INTEGER
DIM Bak(0) AS INTEGER

SCREEN 13

MouseInit

IF LEN(COMMAND$) THEN ShowPCX COMMAND$

XorLine OldX, OldY

DO

  NewX = MouseX%
  NewY = MouseY%

  IF OldX <> NewX OR OldY <> NewY THEN

    XorLine OldX, OldY
    XorLine NewX, NewY

    OldX = NewX
    OldY = NewY

  END IF

  IF lButton% THEN
     
    XorLine OldX, OldY
       
    StartX = NewX
    StartY = NewY
       
    XorBox StartX, StartY, NewX, NewY

    DO WHILE lButton%
         
      NewX = MouseX%
      NewY = MouseY%

      IF OldX <> NewX OR OldY <> NewY THEN
        XorBox StartX, StartY, OldX, OldY
        XorBox StartX, StartY, NewX, NewY
           
        OldX = NewX
        OldY = NewY
      END IF

    LOOP

    XorBox StartX, StartY, OldX, OldY

    IF StartX > OldX THEN SWAP StartX, OldX
    IF StartY > OldY THEN SWAP StartY, OldY

    W = OldX - StartX + 1: H = OldY - StartY + 1
    max = (W * H + 5) / 2
    REDIM img(0 TO max)  AS SINGLE
    REDIM Bak(0 TO max)           AS SINGLE

    MyGet &HA000, StartX, StartY, W, H, img(0)

    OldX = NewX
    OldY = NewY

    MyGet &HA000, NewX, NewY, W, H, Bak(0)
    MyPut &HA000, NewX, NewY, img(0)
  
    DO UNTIL lButton OR rButton

      NewX = MouseX%
      NewY = MouseY%

      IF OldX <> NewX OR OldY <> NewY THEN
        DO: LOOP UNTIL INP(&H3DA) AND 8
        SolidPut &HA000, OldX, OldY, Bak(0)
        MyGet &HA000, NewX, NewY, img(0) / 8, img(1), Bak(0)
       
        MyPut &HA000, NewX, NewY, img(0)
          
        OldX = NewX
        OldY = NewY
      END IF
         

    LOOP

    DO WHILE lButton OR rButton: LOOP

    SolidPut &HA000, OldX, OldY, Bak(0)

    XorLine NewX, NewY
  END IF

  a$ = INKEY$
  IF LEN(a$) THEN
    a$ = UCASE$(a$)
    SELECT CASE ASC(a$)
    CASE 27, 81: EXIT DO
    CASE 76
      REDIM Bak(0 TO 2561)
      MyGet &HA000, 0, 0, 320, 16, Bak(0)

      LOCATE 1, 1, 1
      INPUT "Filename"; a$
      IF LEN(a$) THEN
        ShowPCX a$
        XorLine NewX, NewY
      ELSE
        SolidPut &HA000, 0, 0, Bak(0)
      END IF

    CASE 83
      REDIM Bak(0 TO 2561)
      MyGet &HA000, 0, 0, 320, 16, Bak(0)

      LOCATE 1, 1, 1
      INPUT "Filename"; a$
      IF LEN(a$) THEN
        OPEN a$ FOR BINARY AS 1
        FOR t = 0 TO UBOUND(img)
          PUT #1, , img(t)
        NEXT t
        CLOSE
      END IF
   
      SolidPut &HA000, 0, 0, Bak(0)
    END SELECT
  END IF

LOOP

SCREEN 0

