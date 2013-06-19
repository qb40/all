DECLARE SUB VocVolume (Right%, Left%, Getvol%)
DECLARE SUB DMAState (StopGo%)
DECLARE SUB Titres ()
DECLARE SUB FMVolume (Right%, Left%, Getvol%)
DECLARE SUB WaitKey ()
DECLARE SUB RenderStars (X%, Y%, DeltaStars%, Speed%)
DECLARE SUB MakeExplosion (X%, Y%, Xp%, Yp%, DeltaStars%)
DECLARE SUB GoSound (SEvent%, PlayingSound%, StartedSB%)
DECLARE SUB StopMIDI ()
DECLARE SUB KeySupport (INK$, X%, Y%, Side%, ChangedShip%, StGX%, StGY%, GameOver%)
DECLARE SUB ReadShipHands (Hnds%())
DECLARE SUB RenderShip (X%, Y%, Side%, Pl%, k%, Hnds%())
DECLARE FUNCTION CheckIntersection% (X1a%, Y1a%, X2a%, Y2a%, X1b%, Y1b%, X2b%, Y2b%)
DECLARE SUB ReadSprite (Sprite%(), Number%, i%)
DECLARE SUB MyPCXOPEN (Page%, FileName$)
DECLARE SUB ClearKeys ()
DECLARE SUB WaitVSync ()
DECLARE SUB InitAndDetect ()
DECLARE FUNCTION GetKey$ ()
DECLARE SUB MatchPalette (PAL$, NUMCOL%)
DECLARE FUNCTION CheckIn% (Xc%, Yc%, X1%, Y1%, X2%, Y2%)
DECLARE SUB DMAPlay (Segment&, Offset&, Length&, Freq&)
DECLARE FUNCTION DSPVersion! ()
DECLARE SUB GetBLASTER (DMA%, BasePort%, IRQ%)
DECLARE SUB MasterVolume (Right%, Left%, Getvol%)
DECLARE FUNCTION ReadDSP% ()
DECLARE FUNCTION ResetDsp% ()
DECLARE SUB SpeakerState (OnOff%)
DECLARE FUNCTION SpeakerStatus% ()
DECLARE SUB WriteDSP (byte%)
DECLARE SUB PlaySB (NumSound%, PlayingSound%, StartedSB%)
DECLARE SUB LoadAndPlayMIDI (FileName$, MIDISegment%, MIDIOffset%)
DECLARE SUB WriteToDSP (v%)
DECLARE FUNCTION GetBlasterAddr% ()
DECLARE FUNCTION BytesRequired& (FileName$)
DECLARE FUNCTION SBReset% ()
''$INCLUDE: 'SVGABC.BI'
''$INCLUDE: 'MY_SVGA.BI'
'$DYNAMIC
COMMON SHARED BlasterAddr%, DMA%, repeats%, PlayingSound%
COMMON SHARED Path$, PathWav$, PathG$
COMMON SHARED BasePort%, LenPort%, Channel%, MaxNumStars%, MouseON%
'OPTION BASE 1
DEFINT A-Z
RANDOMIZE TIMER

NIL = SBReset

Path$ = "C:\GAMES\COSMIC2\"       'MAIN-путь
Pat$ = Path$ + "MIDI\"            'MIDI-путь
PathWav$ = Path$ + "FX\"          'SBFX-путь
PathG$ = Path$ + "GFX\"           'GRAF-путь

MouseON = 0                                 'вкл/выкл управление мышью

InitAndDetect

MyPCXOPEN 0, "COSM_ADV" 'Загружаем картинку со спрайтами
'DRWSTRING 1, 15, 8, "By ACA Company, Ryabchenko A.M. Inc.", 500, 550
'DRWSTRING 1, 15, 8, "CALL 590-20-04", 500, 570
SLEEP

NIL = RES640

MyPCXOPEN 1, "KORABLES" 'Загружаем картинку со спрайтами

NumfMIDI$ = LTRIM$(STR$(CINT(RND * 16 + 1)))
IF RND > .7 THEN
 IF NumfMIDI$ <= "10" THEN NumfMIDI$ = LTRIM$(STR$(VAL(NumfMIDI$) + 5))
END IF
FileMIDI$ = "MUSIC" + NumfMIDI$ + ".MID"
DIM MIDI%(BytesRequired&(Pat$ + FileMIDI$) \ 2)
DIM SHARED Hnds(9, 50), HandTemp(50)
DIM SHARED StartX(10), StartY(10), StepX!(10), DeltaSizY(10), Forms(10), CorrX!(10)
DIM Sprite(4000), Sprite1(4000), Sprite2(4000)
DIM SHARED StarColors(50), StarX(50), StarY(50), StarSpeed(50), StarC(50)
DIM SHARED StarDeltaX!(50)
DIM WavBuffer(1 TO 1) AS STRING * 32767 'Make a 32k buffer for file.
DIM SHARED MIDI.PLAYTIME AS SINGLE
DIM SHARED MIDI.ERROR AS INTEGER
MIDI.PLAYTIME = 0
MIDI.ERROR = 0


GOSUB SetArraysOfData

k = 1
GOSUB SetSB
ReadShipHands Hnds()

StGX = 5             '\ Показатели скорости
StGY = 4             '/ движения корабля по экрану
Pl = 4               'Положение корабля (номер кадра),
X = 320: Y = 400     'его координаты
FactP = 0            'Fакт присутствия препятствия
PozT! = 1             'Номер кадра препятствия
Tip = 4              'Тип препятствия
ChangedShip = 1      'Факт перемещения корабля
Speed = 6            'Скорость корабля
Pzs = 1              '1 <-> -1
FutureFactP = 1      'Предсказание столкновения
Freq& = 14000        'Частота звуковых эффектов
LoadAndPlayMIDI Pat$ + FileMIDI$, VARSEG(MIDI%(0)), VARPTR(MIDI%(0))

'IF MouseON = 1 THEN MOUSELOCSET X, Y
StartTIME& = TIMER

DO
 INK$ = GetKey$

 CALL KeySupport(INK$, X%, Y%, Side%, ChangedShip%, StGX%, StGY%, GameOver%)

 IF PlayingSound = 1 THEN
  IF DMADone OR StartedSB = 1 THEN
   REDIM WavBuffer(1 TO 1) AS STRING * 32767 'Make a 32k buffer for file.
   GET #7, 44 + (32767 * CurrentPlay!), WavBuffer(1)'Get 32k from file (skip header on WAV)
   CurrentPlay! = CurrentPlay! + 1
   Length& = LOF(7) - 44
   IF Length& > 32767 THEN Length& = 32767 'Adjust length if needed to 32k
   DMAPlay VARSEG(WavBuffer(1)), VARPTR(WavBuffer(1)), Length&, Freq&
   IF EOF(7) THEN PlayingSound = 0: CLOSE #7: CurrentPlay! = 0
   StartedSB = 0
  END IF
 END IF

 IF FactTremble = 1 THEN 'Трясучка
  OX = X: OY = Y
  Y = CINT(Y + RND * 4 - 2.5)
  X = CINT(X + RND * 4 - 2)
  IF Tip = 3 OR Tip = 4 THEN X = X + SGN(Xp - X) * 2
  IF X > OX THEN Side = Side + 2
  IF X < OX THEN Side = Side - 2
  ChangedShip = 1
 END IF

 X1a = Xp
 Y1a = Yp
 X2a = Xp + StepX!(Tip)
 Y2a = Yp + StepX!(Tip) + DeltaSizY(Tip)
 X1b = X
 Y1b = Y
 X2b = X + 55
 Y2b = Y + 55
 IntSec = CheckIntersection(X1a, Y1a, X2a, Y2a, X1b, Y1b, X2b, Y2b)

 IF ChangedShip = 1 OR IntSec = 1 THEN
  IF IntSec = 0 THEN
   RenderShip X, Y, Side, Pl, k, Hnds()
  ELSE
   NIL = PAGEACTIVE(1)
   RenderShip 580, 1, Side, Pl, k, Hnds()
   'BLKGET 580, 1, 580 + 55, 1 + 55, Sprite(1)
   NIL = PAGEACTIVE(0)
   'SPRITEPUT 1, 0, X, Y, Sprite(1)
  END IF
  'IF FactTremble = 1 THEN DRWFILLBOX 1, 0, X - 3, Y, X, Y + 55
 END IF

 IF FactP = 0 AND FactOvr = 0 THEN 'Няма препятствия - ща сделаем!
  Xp = X + RND * 100 - 50
  Yp = -80
  FactP = 1
  DO
   Tip = RND * 7 + 3
   IF Tip = 6 AND k = 5 THEN Tip = 5
   IF Tip = 9 AND FactTremble = 1 THEN Tip = 5
   IF Tip = 7 AND k <> 2 AND FactTremble <> 1 THEN Tip = 5
   IF Tip <> 5 AND Tip < 10 THEN EXIT DO
  LOOP
  IF RND > .7 THEN Tip = 8
  IF RND > .5 THEN Tip = 3 + RND * 1
 ELSE
 
  OXp = Xp: OYp = Yp 'Запомним старые координаты
  IF Pzs = 1 THEN
   ReadSprite Sprite1(), INT(PozT!), Tip
  ELSE
   ReadSprite Sprite2(), INT(PozT!), Tip
  END IF
  PozT! = PozT! + .16  'Смена кадра
  IF PozT! > Forms(Tip) THEN PozT! = 1
  Yp = Yp + Speed            'Препятствие падает вниз
  IF Yp > 480 THEN FactP = 0 'Препятствие пролетело весь экран
 
  IF IntSec = 0 THEN  'Если пересечений с кораблем нет
   'DRWFILLBOX 1, 0, OXp, OYp - Speed, OXp + StepX!(Tip), OYp + Speed
   IF Pzs = 1 THEN
    'SPRITEPUT 1, 1, Xp, Yp, Sprite2(1)
    'SPRITEPUT 1, 0, Xp, Yp, Sprite1(1)
   ELSE
    'SPRITEPUT 1, 1, Xp, Yp, Sprite1(1)
    'SPRITEPUT 1, 0, Xp, Yp, Sprite2(1)
   END IF
  ELSE
   'DRWFILLBOX 1, 0, OXp, OYp - Speed, OXp + StepX!(Tip), OYp + Speed
   IF Pzs = 1 THEN
    'SPRITEPUT 2, 0, OXp, OYp, Sprite2(1)
    Coll = SPRITECOLLDETECT(0, X, Y, Xp, Yp, Sprite(1), Sprite1(1))
    IF FutureFactP <> 0 THEN
     'SPRITEPUT 1, 0, Xp, Yp, Sprite1(1)
    ELSE
     'DRWFILLBOX 1, 0, OXp, OYp - StepX!(Tip) - Speed, OXp + StepX!(Tip), OYp + Speed + StepX!(Tip)
     RenderShip X, Y, Side, Pl, k, Hnds()
    END IF
    IF Coll = 2 THEN    'Если произошло столкновение
     IF FutureFactP <> 0 THEN FutureFactP = 0 ELSE FactP = FutureFactP: FutureFactP = 1: Collis = 1
    END IF
   ELSE
    'SPRITEPUT 2, 0, OXp, OYp, Sprite1(1)
    Coll = SPRITECOLLDETECT%(0, X, Y, Xp, Yp, Sprite(1), Sprite2(1))
    IF FutureFactP <> 0 THEN
     'SPRITEPUT 1, 0, Xp, Yp, Sprite2(1)
    ELSE
     'DRWFILLBOX 1, 0, OXp, OYp - StepX!(Tip) - Speed, OXp + StepX!(Tip), OYp + Speed + StepX!(Tip)
     RenderShip X, Y, Side, Pl, k, Hnds()
    END IF
    IF Coll = 2 THEN    'Если произошло столкновение
     IF FutureFactP <> 0 THEN FutureFactP = 0 ELSE FactP = FutureFactP: FutureFactP = 1: Collis = 1
    END IF
   END IF
  END IF
  Pzs = -Pzs
  IF Collis = 1 THEN 'Анализ столкновения
   Collis = 0
   SELECT CASE Tip
   CASE 3, 4 'Камушки
    IF k <> 2 THEN
     IF FactProt = 0 THEN
      k = 2: Y = Y + 5
      RenderShip X, Y, Side, Pl, k, Hnds()
      GoSound 1, PlayingSound, StartedSB
      HStolk = HStolk + 1
     ELSE 'Из ЗАЩИЩЕННОГО делается простым:
      FactProt = 0
      k = 1: Y = Y - 8
      RenderShip X, Y, Side, Pl, k, Hnds()
      GoSound 2, PlayingSound, StartedSB
     END IF
    ELSE 'Каюк...
     GameOver = 1
     GoSound 3, PlayingSound, StartedSB
     Speed = 0
    END IF
    DeltaStars = 10
    MakeExplosion X, Y, Xp, Yp, DeltaStars
   CASE IS = 6 'ЗАЩИТА
    HSheeld = HSheeld + 1
    IF FactProt <> 1 THEN
    IF k = 1 THEN
     FactProt = 1
     k = 5: Y = Y + 8
     RenderShip X, Y, Side, Pl, k, Hnds()
     GoSound 4, PlayingSound, StartedSB
    ELSE
     GoSound 8, PlayingSound, StartedSB
    END IF
    END IF
   CASE IS = 7 'АПТЕЧКА
    FactTremble = 0
    IF k = 2 THEN k = 1: Y = Y - 5: GoSound 5, PlayingSound, StartedSB
   CASE IS = 8 ' СУПЕР-ЧАЙНИК-УСКОРИТЕЛЬ
    HSpeed = HSpeed + 1
    Speed = Speed + 3: IF Speed > 20 THEN Speed = 20
    GoSound 6, PlayingSound, StartedSB
   CASE IS = 9
    HTras = HTras + 1
    FactTremble = 1 'От ТОРУСА - трясучка...
    GoSound 7, PlayingSound, StartedSB
   END SELECT
  END IF
 END IF

 RenderStars X, Y, DeltaStars, Speed

 'Вычисляем пройденный путь
 PathLast = PathLast + Speed / 3


 IF PathLast > 25555 AND FactP = 0 AND FactOvr = 0 THEN
  FactOvr = 1: Tip = 10: FactP = 1: Yp = -200: Xp = 270
  MyPCXOPEN 1, "JUPITER" 'Загружаем картинку со спрайтами
 END IF
 IF FactOvr = 3 THEN GameOver = 2: WinOK = 1
 OPathLast = PathLast

 IF FactOvr = 1 THEN
  IF PathLast MOD 12 = 0 THEN
   Speed = Speed / 2
   IF Speed < 3 THEN Speed = 3: FactOvr = 2
   IF Speed > 10 THEN Speed = Speed / 2
  END IF
 END IF
 IF FactOvr = 2 OR FactOvr = 1 THEN
  IF Yp > Y - 150 OR Yp > 300 THEN FactOvr = 3
 END IF

 ChangedShip = 0
 IF GameOver > 0 AND PlayingSound = 0 THEN EXIT DO
 IF GameOver = -1 THEN EXIT DO

LOOP
'?????????????????????????????????????????????????????????????????????????????
'GAMEOVERGAMEOVERGAMEOVERGAMEOVERGAMEOVERGAMEOVERGAMEOVERGAMEOVERGAMEOVERGAMEO
'!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!

FOR k = 1 TO 9
FOR ai = 1 TO Forms(k)
 IF Hnds(k, ai) <> 0 THEN NIL = XMSFREE(Hnds(k, ai))
NEXT ai
NEXT k
FOR k = 1 TO 50
 IF HandTemp(k) <> 0 THEN NIL = XMSFREE(HandTemp(k))
NEXT k

StopMIDI
SLEEP

FMVolume 15, 15, 0

SELECT CASE GameOver
CASE IS = 1
 'NIL = PCXGETINFO(PathG$ + "gover.pcx", NIL, NIL, NIL, PAL)
 MatchPalette PAL$, 255
 NIL = PAGEACTIVE(1)
 'NIL = PCXPUT(1, 0, 0, PathG$ + "gover.pcx")
 'PALSET PAL, 0, 255
 NIL = PAGEDISPLAY(0, 0, 1)

 FMVolume 10, 10, 0
 REDIM MIDI%(BytesRequired&(Pat$ + "over.mid") \ 2)
 LoadAndPlayMIDI Pat$ + "over.mid", VARSEG(MIDI%(0)), VARPTR(MIDI%(0))

 WaitKey
CASE IS = 2
 'PALCHGAUTO PAL, PAL2, 0, 255, 10
 'NIL = PCXGETINFO(PathG$ + "win.pcx", NIL, NIL, NIL, PAL)
 MatchPalette PAL$, 255
 NIL = PAGEACTIVE(1)
 'NIL = PCXPUT(1, 0, 0, PathG$ + "win.pcx")
 NIL = PAGEDISPLAY(0, 0, 1)
 'PALCHGAUTO PAL2, PAL, 0, 255, 10
 DTime = CINT(TIMER - StartTIME&)
 STime$ = LTRIM$(RTRIM$(STR$(DTime)))

 ERASE Sprite, Sprite1, Sprite2
 DIM Counter(5000)

 NIL = PAGEACTIVE(0)
 'NIL = PCXPUT(1, 1, 0, PathG$ + "counter.pcx")
 s = 14
 FOR i = 1 TO LEN(STime$)
  cr = VAL(MID$(STime$, i, 1))
  NIL = PAGEACTIVE(0)
  'BLKGET 9 + cr * s, 2, 9 + cr * s + s - 1, 50, Counter(1)
  NIL = PAGEACTIVE(1)
  'SPRITEPUT 1, 0, 15 + (i - 1) * (s + 2), 374, Counter(1)
 NEXT i

 ERASE Counter

 IF HStolk >= 2 THEN Mdl = 1
 IF HStolk <= 1 AND HTras = 0 AND HSheeld >= 1 THEN Mdl = 2

 IF Mdl > 0 THEN
  NIL = PAGEACTIVE(0)
  NmFm$ = PathG$ + "m" + LTRIM$(RTRIM$(STR$(Mdl))) + ".pcx"
  'NIL = PCXPUT(1, 0, 0, NmFm$)
  DIM Medal(7000)

  'BLKGET 45, 35, 190, 190, Medal(0)
  NIL = PAGEACTIVE(1)
  'SPRITEPUT 1, 252, 10, 170, Medal(0)
  ERASE Medal
 END IF

 OMdl = Mdl
 IF HStolk = 0 THEN
  IF HTras = 0 THEN Mdl = 3 ELSE IF HSpeed > 5 THEN Mdl = 4 ELSE IF HTras = 1 THEN Mdl = 5
 END IF

 IF Mdl > 0 THEN
  NIL = PAGEACTIVE(0)
  NmFm$ = PathG$ + "m" + LTRIM$(RTRIM$(STR$(Mdl))) + ".pcx"
  'NIL = PCXPUT(1, 0, 0, NmFm$)
  DIM Medal(7000)

  'BLKGET 45, 35, 190, 190, Medal(0)
  NIL = PAGEACTIVE(1)
  IF OMdl <> 0 THEN
   'SPRITEPUT 1, 252, 110, 170, Medal(0)
  ELSE
   'SPRITEPUT 1, 252, 10, 170, Medal(0)
  END IF
  ERASE Medal
 END IF

 FMVolume 10, 10, 0
 REDIM MIDI%(BytesRequired&(Pat$ + "win.mid") \ 2)
 LoadAndPlayMIDI Pat$ + "win.mid", VARSEG(MIDI%(0)), VARPTR(MIDI%(0))

 '=================
 Titres '         |
 '=================
END SELECT

StopMIDI
NIL = RESTEXT

CLEAR
END


SetArraysOfData:

' Матрица: StartX, StartY, StepX!, DeltaSizY, Forms, CorrX!:
DATA  0, 10, 55 ,  0, 7 ,0
DATA  0, 60, 55 ,  0, 7 ,0
DATA  7,115, 27 ,  0, 16,-.3
DATA  7,155, 29 ,  0, 15,0
DATA  0,195, 55 ,  0, 7 ,0
DATA  0,248,53.3, -5, 12,0
DATA  0,300,53.3, -5, 12,0
DATA  0,354,53.3, -5, 12,0
DATA  0,409,53.4,-15, 12,0
DATA  0,250,100 ,0  , 6,0

RESTORE
FOR i = 1 TO 10
 READ StartX(i)
 READ StartY(i)
 READ StepX!(i)
 READ DeltaSizY(i)
 READ Forms(i)
 READ CorrX!(i)
NEXT i

'Матрица с цветами звезд
DATA 10 ,23 ,70 ,95 ,123,219,245
DATA 59 ,135,109,135,194,197,214

FOR i = 1 TO 14
 READ StarColors(i)
NEXT i

MaxNumStars = 15
FOR i = 1 TO MaxNumStars
 StarX(i) = RND * 640
 StarY(i) = RND * 480
 StarSpeed(i) = RND * 1 + 1
 StarC(i) = StarColors(INT(RND * 14 + 1))
NEXT i

'Medal1 = 0


RETURN

SetSB:

GetBLASTER Channel%, BasePort%, IRQ% ' Parses BLASTER environment
NIL = ResetDsp%

SpeakerState 1 'turn the speaker on
MasterVolume 15, 15, -1 'this puts the mixer volumes in Right% and Left%
'MasterVolume 1, 13, 0 '15,15,0 cranks the master volume all the way up.

RETURN

REM $STATIC
FUNCTION BytesRequired& (FileName$)

'Open the file.
FF% = FREEFILE
OPEN FileName$ FOR BINARY AS #FF%
'Store the length of the file.
FileLen& = LOF(FF%)
'Close the file.
CLOSE FF%
'If the length of the file is 0, assume it does not exist and delete it.
IF FileLen& = 0 THEN KILL FileName$
'Return the length of the file as the number of bytes required.
BytesRequired& = FileLen&
MIDI.ERROR = 0

END FUNCTION

FUNCTION CheckIn (Xc, Yc, X1, Y1, X2, Y2)

IF Xc > X1 AND Yc > Y1 AND Xc < X2 AND Yc < Y2 THEN
 CheckIn = 1
ELSE
 CheckIn = 0
END IF

END FUNCTION

FUNCTION CheckIntersection (X1a, Y1a, X2a, Y2a, X1b, Y1b, X2b, Y2b)
'Проверка пересечения 2-х прямоугольников:

CheckIntersection = 1
IF X1a > X2b OR X1b > X2a THEN CheckIntersection = 0
IF Y1a > Y2b OR Y1b > Y2a THEN CheckIntersection = 0

END FUNCTION

SUB ClearKeys
FOR j = 1 TO 32: i$ = INKEY$: NEXT j
END SUB

SUB DMAPlay (Segment&, Offset&, Length&, Freq&)

' Transfers and plays the contents of the buffer.
Length& = Length& - 1
Page% = 0
MemLoc& = Segment& * 16 + Offset&
SELECT CASE Channel%
    CASE 0
       PgPort% = &H87
       AddPort% = &H0
       LenPort% = &H1
       ModeReg% = &H48
    CASE 1
       PgPort% = &H83
       AddPort% = &H2
       LenPort% = &H3
       ModeReg% = &H49
    CASE 2
       PgPort% = &H81
       AddPort% = &H4
       LenPort% = &H5
       ModeReg% = &H4A
    CASE 3
       PgPort% = &H82
       AddPort% = &H6
       LenPort% = &H7
       ModeReg% = &H4B
    CASE ELSE
       'PRINT "DMA channels 0-3 only are supported."
       EXIT SUB
END SELECT

OUT &HA, &H4 + Channel%
OUT &HC, &H0
OUT &HB, ModeReg%
OUT AddPort%, MemLoc& AND &HFF
OUT AddPort%, (MemLoc& AND &HFFFF&) \ &H100
IF (MemLoc& AND 65536) THEN Page% = Page% + 1
IF (MemLoc& AND 131072) THEN Page% = Page% + 2
IF (MemLoc& AND 262144) THEN Page% = Page% + 4
IF (MemLoc& AND 524288) THEN Page% = Page% + 8
OUT PgPort%, Page%
OUT LenPort%, Length& AND &HFF
OUT LenPort%, (Length& AND &HFFFF&) \ &H100
OUT &HA, Channel%

IF Freq& < 23000 THEN
   timeconst% = 256 - 1000000 \ Freq&
   WriteDSP &H40
   WriteDSP timeconst%
   WriteDSP &H14
   WriteDSP (Length& AND &HFF)
   WriteDSP ((Length& AND &HFFFF&) \ &H100)
ELSE
   IF DSPVersion! >= 3 THEN
      timeconst% = ((65536 - 256000000 \ Freq&) AND &HFFFF&) \ &H100
      WriteDSP &H40
      WriteDSP timeconst%
      WriteDSP (Length& AND &HFF)
      WriteDSP ((Length& AND &HFFFF&) \ &H100)
      WriteDSP &H91
   ELSE
      'PRINT "You need a Sound Blaster with a DSP v3.x+ to play at high speed."
      EXIT SUB
   END IF
END IF

END SUB

SUB DMAState (StopGo%)

' Stops or continues DMA play.
IF StopGo% THEN WriteDSP &HD4 ELSE WriteDSP &HD0

END SUB

FUNCTION DSPVersion!

' Gets the DSP version.
WriteDSP &HE1
Temp% = ReadDSP%
Temp2% = ReadDSP%
DSPVersion! = VAL(STR$(Temp%) + "." + STR$(Temp2%))


END FUNCTION

SUB FMVolume (Right%, Left%, Getvol%)
OUT BasePort% + 4, &H26
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF
END SUB

SUB GetBLASTER (DMA%, BasePort%, IRQ%)

' This subroutine parses the BLASTER environment string and returns settings.
IF LEN(ENVIRON$("BLASTER")) = 0 THEN PRINT "BLASTER environment variable not set.": EXIT SUB
FOR Length% = 1 TO LEN(ENVIRON$("BLASTER"))
   SELECT CASE MID$(ENVIRON$("BLASTER"), Length%, 1)
      CASE "A"
        BasePort% = VAL("&H" + MID$(ENVIRON$("BLASTER"), Length% + 1, 3))
      CASE "I"
        IRQ% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
      CASE "D"
        DMA% = VAL(MID$(ENVIRON$("BLASTER"), Length% + 1, 1))
   END SELECT
NEXT

END SUB

FUNCTION GetBlasterAddr%

'Get Blaster Address and DMA channel from Environment Variable
tmp% = 0  'No Environment Variable Set...default
blast$ = UCASE$(ENVIRON$("BLASTER"))
IF LEN(blast$) THEN
   tmp% = INSTR(blast$, "A")
   tmp1$ = MID$(blast$, tmp% + 1, 3)
   tmp% = VAL("&H" + tmp1$)
   IF tmp% = 203 THEN tmp% = -1    'If there is no value assigned
   IF tmp% > 0 THEN
      tmp2% = INSTR(blast$, "D")
      DMA% = VAL(MID$(blast$, tmp2% + 1))        'dma% is a global variable
      IF DMA% < 0 OR DMA% > 7 THEN tmp% = -2
   END IF
END IF
GetBlasterAddr% = tmp%

END FUNCTION

FUNCTION GetKey$

Code = INP(&H60)
IF CHR$(Code) <> "p" THEN
 Res = (INP(&H61) OR &H82)
 OUT &H61, Res
 OUT &H61, (Res AND &H7F)
END IF
'OUT &H61, &H61
OUT &H20, &H20
OUT &H61, 0

GK$ = CHR$(Code)
IF ASC(GK$) > 127 THEN ClearKeys
GetKey$ = GK$

END FUNCTION

SUB GoSound (SEvent, PlayingSound, StartedSB)

SELECT CASE SEvent
CASE IS = 1: IF RND > .5 THEN NS = 36 ELSE NS = 19 'Ухудшение
CASE IS = 2: IF RND > .5 THEN NS = 35 ELSE NS = 20 'Обнормаливание (5->1)
CASE IS = 3                                        'Каюк
 SELECT CASE CINT(RND * 3)
 CASE IS = 0: NS = 33
 CASE IS = 1: NS = 27
 CASE IS = 2: NS = 56
 CASE IS = 3: NS = 55
 END SELECT
CASE IS = 4: IF RND > .5 THEN NS = 31 ELSE NS = 50 'Защита
CASE IS = 5: IF RND > .5 THEN NS = 51 ELSE NS = 44 'Аптечка
CASE IS = 6: IF RND > .5 THEN NS = 38 ELSE NS = 10 'Ускорение
CASE IS = 7: IF RND > .5 THEN NS = 32 ELSE NS = 37 'Tpясучка
CASE IS = 8: IF RND > .5 THEN NS = 25 ELSE NS = 29 'Защита не берется...
END SELECT

PlaySB NS, PlayingSound, StartedSB

END SUB

SUB InitAndDetect
    CLS
    '*************************************************************************
    '* LET'S ID THE VIDEO CARD/CHIP AS THE LIBRARY REQUIRES A SUPERVGA ADAPTER.
    '*************************************************************************
    NIL = RESTEXT
    CPU = WHICHCPU
    VGA = WHICHVGA
    IF VGA < 1 THEN
      STOP:  END
    END IF
    '*************************************************************************
    '* LET'S SEE THE HOW MUCH MEMORY IS INSTALLED ON THE SUPER VGA ADAPTER.
    '*************************************************************************
    VIDEOMEMORY = WHICHMEM
    MEM$ = STR$(VIDEOMEMORY)
    MEM$ = RIGHT$(MEM$, LEN(MEM$) - 1) + " kB"
    IF VIDEOMEMORY < 1 THEN
        STOP: END
    END IF

    '*************************************************************************
    '* LET'S LOOK FOR A MOUSE. IF WE FIND ONE, TELL ALL ABOUT IT
    '*************************************************************************
    mouse = WHICHMOUSE
    IF mouse > 0 THEN
        MOUSEINFO MJV, MNV, TP, i
        VER$ = STR$(MNV)
        l = LEN(VER$)
        VER$ = STR$(MJV) + "." + RIGHT$(VER$, l - 1)
        VER$ = "Software driver version is" + VER$ + " (Microsoft equivalent version)."
    ELSE
        STOP: END
    END IF
    PRINT

    '*************************************************************************
    '* XMS MEMORY TEST
    '*************************************************************************
    Er = WHICHXMS(KB, HND)
    IF Er = 0 THEN
     STOP: END
    END IF
  
    '*************************************************************************
    '* LET'S GET DOWN TO BUSINESS!
    '*************************************************************************
    Res = 1
    ENDIT = 0

    '*************************************************************************
    '* INIT ALTERNATE COLOR PALETTES
    '*************************************************************************
    'Er = RES640
    Er = RES800
    IF MouseON = 1 THEN MOUSEENTER: MOUSESENSSET 14, 14, 1
END SUB

SUB InputSource (InputSrc%, GetSrc%)

OUT BasePort% + 4, &HC
IF GetSrc% THEN
   InputSrc% = INP(BasePort% + 5) AND 2 + INP(BasePort% + 5) AND 4
ELSE
   OUT BasePort% + 5, InputSrc% AND 7
END IF

END SUB

SUB KeySupport (INK$, X, Y, Side, ChangedShip, StGX, StGY, GameOver)
 
WaitVSync

SELECT CASE INK$
CASE IS = "K"
 DRWFILLBOX 1, 0, X, Y, X + 55, Y + 55
 X = X - StGX
 Side = Side - 1
 IF Side < -9 THEN Side = -9
 ChangedShip = 1
 FMVolume CINT(X / 50), CINT((640 - X) / 50), 0
CASE IS = "M"
 DRWFILLBOX 1, 0, X, Y, X + 55, Y + 55
 X = X + StGX
 Side = Side + 1
 IF Side > 9 THEN Side = 9
 ChangedShip = 1
 FMVolume CINT(X / 50), CINT((640 - X) / 50), 0
CASE IS = "H"
 DRWFILLBOX 1, 0, X, Y, X + 55, Y + 55
 Y = Y - StGY
 ChangedShip = 1
CASE IS = "P"
 DRWFILLBOX 1, 0, X, Y, X + 55, Y + 55
 Y = Y + StGY
 ChangedShip = 1
CASE IS = CHR$(1): GameOver = -1
CASE ELSE
 ClearKeys
 IF Y < 400 THEN Y = Y + (400 - Y) / 30 + 1: ChangedShip = 1
 IF Y > 420 THEN Y = Y - 1: ChangedShip = 1
 IF X < 50 THEN X = X + 1: ChangedShip = 1
 IF X > 550 THEN X = X - 1: ChangedShip = 1
 IF Side < 0 THEN Side = Side + 1: ChangedShip = 1
 IF Side > 0 THEN Side = Side - 1: ChangedShip = 1
END SELECT

END SUB

SUB LineVolume (Right%, Left%, Getvol%)

OUT BasePort% + 4, &H2E
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF

END SUB

SUB LoadAndPlayMIDI (FileName$, MIDISegment%, MIDIOffset%)

'See if an extension was supplied, and if not, add one.
IF INSTR(FileName$, ".") = 0 THEN FileName$ = FileName$ + ".MID"
'Open the file
FF% = FREEFILE
OPEN FileName$ FOR BINARY AS #FF%
FileLen& = LOF(1)
CLOSE #FF%
'If the file is empty, delete it and exit now.
IF FileLen& = 0 THEN KILL FileName$: MIDI.ERROR = 1: EXIT SUB
'If the file is too large, exit now.
IF FileLen& > 65535 THEN MIDI.ERROR = 2: EXIT SUB
'Make the filename an ASCIIZ string.
FileName$ = FileName$ + CHR$(0)
'Check if the MIDI loading code has already been loaded;
'if not, do it now.
IF LoadCodeLoaded% = 0 THEN
        asm1$ = asm1$ + CHR$(&H1E)
        asm1$ = asm1$ + CHR$(&H55)
        asm1$ = asm1$ + CHR$(&H89) + CHR$(&HE5)
        asm1$ = asm1$ + CHR$(&HB8) + CHR$(&H0) + CHR$(&H3D)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HE)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H17)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H10)
        asm1$ = asm1$ + CHR$(&H8E) + CHR$(&H1F)
        asm1$ = asm1$ + CHR$(&HCD) + CHR$(&H21)
        asm1$ = asm1$ + CHR$(&H89) + CHR$(&HC6)
        asm1$ = asm1$ + CHR$(&HB4) + CHR$(&H3F)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&HF)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HA)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H17)
        asm1$ = asm1$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&HC)
        asm1$ = asm1$ + CHR$(&H8E) + CHR$(&H1F)
        asm1$ = asm1$ + CHR$(&H89) + CHR$(&HF3)
        asm1$ = asm1$ + CHR$(&HCD) + CHR$(&H21)
        asm1$ = asm1$ + CHR$(&HB4) + CHR$(&H3E)
        asm1$ = asm1$ + CHR$(&HCD) + CHR$(&H21)
        asm1$ = asm1$ + CHR$(&H5D)
        asm1$ = asm1$ + CHR$(&H1F)
        asm1$ = asm1$ + CHR$(&HCA) + CHR$(&HA) + CHR$(&H0)
        LoadCodeLoaded% = 1
END IF
'Call the assembly language routine.
DEF SEG = VARSEG(asm1$)
CALL ABSOLUTE(VARSEG(FileName$), SADD(FileName$), MIDISegment%, MIDIOffset%, &HFFFF, SADD(asm1$))
MIDI.ERROR = 0
'Check to see if the MIDI playing code has previously been loaded.
'If not, load it now.
IF PlayCodeLoaded% = 0 THEN
        'Load the machine codes into a string.
        asm2$ = asm2$ + CHR$(&H55)
        asm2$ = asm2$ + CHR$(&H89) + CHR$(&HE5)
        asm2$ = asm2$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H8)
        asm2$ = asm2$ + CHR$(&H8B) + CHR$(&H17)
        asm2$ = asm2$ + CHR$(&H8B) + CHR$(&H5E) + CHR$(&H6)
        asm2$ = asm2$ + CHR$(&H8B) + CHR$(&H7)
        asm2$ = asm2$ + CHR$(&HBB) + CHR$(&H4) + CHR$(&H0)
        asm2$ = asm2$ + CHR$(&HCD) + CHR$(&H80)
        asm2$ = asm2$ + CHR$(&HBB) + CHR$(&H5) + CHR$(&H0)
        asm2$ = asm2$ + CHR$(&HCD) + CHR$(&H80)
        asm2$ = asm2$ + CHR$(&H5D)
        asm2$ = asm2$ + CHR$(&HCA) + CHR$(&H4) + CHR$(&H0)
        'Indicate that the code has been loaded.
        PlayCodeLoaded% = 1
END IF
'Call the machine language routine to play the music.
DEF SEG = VARSEG(asm2$)
CALL ABSOLUTE(MIDISegment%, MIDIOffset%, SADD(asm2$))
'Start the MIDI timer.
MIDI.PLAYTIME = TIMER
MIDI.ERROR = 0

END SUB

SUB MakeExplosion (X, Y, Xp, Yp, DeltaStars)
'Эта процедура делает взрыв (по сути, из звезд...)

FOR i = MaxNumStars + 1 TO MaxNumStars + DeltaStars
 StarX(i) = (Xp + X + 35) / 2 + RND * 25 - 12.5
 StarY(i) = (Yp + Y) / 2 + RND * 30
 StarSpeed(i) = RND * 2 + 6
 StarC(i) = StarColors(INT(RND * 14 + 1))
 StarDeltaX!(i) = (StarX(i) - (Xp + X + 35) / 2) * RND / 10
 'DRWPOINT 1, StarC(i), StarX(i), StarY(i)
NEXT i

END SUB

SUB MasterVolume (Right%, Left%, Getvol%)

OUT BasePort% + 4, &H22
'PRINT BasePort%
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF

END SUB

SUB MatchPalette (PAL$, NUMCOL)
'Исправление некоторых ошибок в палитре PCX:
MIN& = (255 ^ 2) * 3
MAX& = 0
'*********************************************************************
'* WE NEED TO CHECK THE PCX COLOR PALETTE ENTRIES TO SEE IF ANY COLORS
'* ARE GREATER THE SIX BITS IN LENGTH AS THE VGA COLOR PALETTE
'* REGISTERS ARE ONLY SIX BITS WIDE. WE ALSO LOOK FOR THE BRIGHTEST
'* AND DARKEST COLORS TO USE AS OUR TEXT AND BACKGROUND COLORS
'*********************************************************************
FIXIT = 0
FOR a = 1 TO NUMCOL * 3 STEP 3
 R = ASC(MID$(PAL$, a, 1))
 G = ASC(MID$(PAL$, a + 1, 1))
 b = ASC(MID$(PAL$, a + 2, 1))
 IF R > 63 THEN
   FIXIT = 1
 END IF
 IF G > 63 THEN
   FIXIT = 1
 END IF
 IF b > 63 THEN
   FIXIT = 1
 END IF
 TEST& = R ^ 2 + G ^ 2 + b ^ 2
 IF TEST& < MIN& THEN
 '* FIND THE DARKEST COLOR FOR THE BACKGROUND
 MIN& = TEST&
 MINCOLOR = a / 3
END IF
IF TEST& > MAX& THEN
  '* FIND THE BRIGHTEST COLOR FOR THE TEXT
  MAX& = TEST&
  MAXCOLOR = a / 3
END IF
NEXT a

'*********************************************************************
'* IF THE PCX USES 8 BIT COLOR THEN WE SHIFT EACH COLOR ENTRY RIGHT
'* BY 2 BITS (THIS REDUCES IT TO 6 BITS OF COLOR)
'*********************************************************************
IF FIXIT = 1 THEN
 FOR a = 1 TO NUMCOL * 3
   C = ASC(MID$(PAL$, a, 1))
   MID$(PAL$, a, 1) = CHR$(C \ 4)
 NEXT a
END IF

'*********************************************************************
'* IF THE PCX HAS A PALETTE OF 128 COLORS OR LESS THEN WE CAN USE
'* OUR OWN COLORS FOR THE TEXT AND BACKGROUND
'*********************************************************************
IF NUMCOL < 128 THEN
 MID$(PAL$, 763, 1) = CHR$(0)  '* THIS IS THE COLOR BLACK
 MID$(PAL$, 764, 1) = CHR$(0)
 MID$(PAL$, 765, 1) = CHR$(0)
 MINCOLOR = 254
 MID$(PAL$, 766, 1) = CHR$(32) '* THIS IS THE COLOR MED WHITE
 MID$(PAL$, 767, 1) = CHR$(32)
 MID$(PAL$, 768, 1) = CHR$(32)
 NUMCOL = 255
END IF

END SUB

SUB MyPCXOPEN (Page, FileName$)
'Открытие полноэкранного PCX-файла в требуемую видеостраницу

FileName$ = PathG$ + FileName$ + ".PCX"
NIL = PCXGETINFO(FileName$, PCXXSize%, PCXYSize%, NumColors%, PAL)
MatchPalette PAL, 256
PALSET PAL, 0, 255
NIL = PAGEACTIVE(Page)
FILLVIEW (0)
NIL = PCXPUT(1, 0, 0, FileName$)
NIL = PAGEACTIVE(0)

END SUB

SUB PlayBack (buffer$, size%, Freq&, BytesPerSec&, chans%, Num%)

size% = size% - 1
Segment& = VARSEG(buffer$)
Offset& = SADD(buffer$)
IF Segment& < 0 THEN Segment& = Segment& + 65536
IF Offset& < 0 THEN Offset& = Offset& + 65536
baseaddr& = Segment& * 16 + Offset&
look1% = VARPTR(baseaddr&)
look2% = VARPTR(size%)
SELECT CASE DMA%
   CASE 0
      dmapage% = &H87   '135 decimal
      dmaaddr% = 0
      dmalen% = 1
   CASE 1
      dmapage% = &H83   '131 decimal
      dmaaddr% = 2
      dmalen% = 3
   CASE 2
      dmapage% = &H81
      dmaaddr% = 4
      dmalen% = 5
   CASE 3
      dmapage% = &H82
      dmaaddr% = 6
      dmalen% = 7
   CASE 4
      dmapage% = &H8F
      dmaaddr% = &HC0
      dmalen% = &HC2
   CASE 5
      dmapage% = &H8B
      dmaaddr% = &HC4
      dmalen% = &HC6
   CASE 6
      dmapage% = &H89
      dmaaddr% = &HC8
      dmalen% = &HCA
   CASE 7
      dmapage% = &H8A
      dmaaddr% = &HCC
      dmalen% = &HCE
END SELECT
SELECT CASE DMA%
   CASE 0 TO 3
      dmamask% = &HA
      dmamode% = &HB
      dmaclear% = &HC
      dmastatus% = &H8
   CASE 4 TO 7
      dmamask% = &HD4
      dmamode% = &HD6
      dmaclear% = &HD8
      dmastatus% = &HD0
END SELECT
SELECT CASE DMA%
   CASE 0, 4
      dmaterminal% = 1   'bit 0 of status register (&H08 or &HD0)
   CASE 1, 5
      dmaterminal% = 2   'bit 1
   CASE 2, 6
      dmaterminal% = 4   'bit 2
   CASE 3, 7
      dmaterminal% = 8   'bit 3
END SELECT

OUT dmamask%, DMA% + 4   'mask the dma channel
OUT dmaclear%, &H0       '(clear the internal DMA flip/flop)
OUT dmamode%, 72 + DMA%  '  72=010010XX where XX=dmachannel%
OUT dmaaddr%, PEEK(look1%)      'bits 0-7 of  the 20bit address
OUT dmaaddr%, PEEK(look1% + 1)  'bits 8-15 of the 20bit address
OUT dmapage%, PEEK(look1% + 2)  'bits 16-19 of the 20 bit address
OUT dmalen%, PEEK(look2%)       'bits 0-7 of size%
OUT dmalen%, PEEK(look2% + 1)   'bits 8-15  of size%
OUT dmamask%, DMA%              'enable channel

IF Num% = 1 THEN  'only need to Write out time constant once
   timeconst% = 256 - 1000000 / (Freq& * chans%)
   CALL WriteToDSP(&H40)
   CALL WriteToDSP(timeconst%)
   'Reset Mixer    DSPmixeraddress = Blasteraddr% + &H4
   OUT BlasterAddr% + &H4, &H0
   OUT BlasterAddr% + &H4 + 1, 0
   'Set Volume to Maximum...255
   OUT BlasterAddr% + &H4, &H22
   OUT BlasterAddr% + &H4 + 1, 255
   IF chans% = 2 THEN
      'Set mixer to Stereo Output
      OUT BlasterAddr% + &H4, &HE
      OUT BlasterAddr% + &H4 + 1, 34      '34=2^5+2^1
   END IF
END IF
IF BytesPerSec& > 22000 THEN
   CALL WriteToDSP(&H48)   'Set Block Size
ELSE
   CALL WriteToDSP(&H14)   'DMA Mode 8-bit DAC
END IF
CALL WriteToDSP(PEEK(look2%))      'Lo byte of address
CALL WriteToDSP(PEEK(look2% + 1))  'High byte of address
IF BytesPerSec& > 22000 THEN CALL WriteToDSP(&H91)  'High Speed DMA mode 8-bit
dummy% = INP(dmastatus%)    'Read status byte once to make sure DMA is going.
WAIT dmastatus%, dmaterminal%   'Loop until terminal count bit set in DMA status register
'DMA Transfer is Now Complete
'Acknowledge the DSP interrupt by reading the DATA AVAILABLE port once
dummy% = INP(BlasterAddr% + &HE)    'DSP Available address

END SUB

SUB PlaySB (NumSound, PlayingSound, StartedSB)

IF PlayingSound = 0 THEN
 WavFilename$ = PathWav$ + LTRIM$(STR$(NumSound)) + ".WAV"
 OPEN WavFilename$ FOR BINARY AS #7: PlayingSound = 1: StartedSB = 1
END IF

END SUB

SUB ReadCapture (HandTemp(), k)
'Считываем кадры

FOR i = 1 TO 50
 IF HandTemp(i) <> 0 THEN NIL = XMSFREE(HandTemp(i))
NEXT i

NIL = PAGEACTIVE(1)
FOR X! = StartX(k) TO StartX(k) + StepX!(k) * (Forms(k) - 1) STEP StepX!(k)
 N = N + 1
 HandTemp(N) = XMSBLKGET(X! + 1.5, StartY(k) + 1, X! + StepX!(k) - 1, StartY(k) + StepX!(k) + DeltaSizY(k) - 1)
NEXT X!
NIL = PAGEACTIVE(0)

END SUB

FUNCTION ReadDSP%

' Reads a byte from the DSP
DO
LOOP UNTIL INP(BasePort% + 14) AND &H80
ReadDSP% = INP(BasePort% + 10)

END FUNCTION

SUB ReadShipHands (Hnds())

NIL = PAGEACTIVE(1)
FOR k = 1 TO 9              'Тип корабля
 IF k = 1 OR k = 2 OR k = 5 THEN
  N = 0
  FOR X! = StartX(k) TO StartX(k) + StepX!(k) * (Forms(k) - 1) STEP StepX!(k)
   N = N + 1
   H = XMSBLKGET(X! + 1.5, StartY(k) + 1, X! + StepX!(k) - 1, StartY(k) + StepX!(k) + DeltaSizY(k) - 1)
   IF H = 0 THEN STOP
   Hnds(k, N) = H
  NEXT X!
 END IF
NEXT k
NIL = PAGEACTIVE(0)

END SUB

SUB ReadSprite (Sprite(), Number, i)
'Считываем кадр в спрайт
'i - строка, Number - колонка (номер кадра)

NIL = PAGEACTIVE(1)
IF i <> 10 THEN
 FOR X! = StartX(i) TO StartX(i) + StepX!(i) * (Forms(i) - 1) STEP StepX!(i)
  N = N + 1
  IF N = Number THEN
   CALL BLKGET(X! + CorrX!(i) * (N - 1) + 1, StartY(i) + 1, X! + StepX!(i) - 1, StartY(i) + CorrX!(i) * (N - 1) + StepX!(i) + DeltaSizY(i) - 1, Sprite(1))
   EXIT FOR
  END IF
 NEXT X!
ELSE
 IF Number <= 6 THEN
  FOR X! = StartX(i) TO StartX(i) + StepX!(i) * (Forms(i) - 1) STEP StepX!(i)
   N = N + 1
   IF N = Number THEN
    CALL BLKGET(X! + CorrX!(i) * (N - 1) + 1, StartY(i) + 1, X! + StepX!(i) - 1, StartY(i) + CorrX!(i) * (N - 1) + StepX!(i) + DeltaSizY(i) - 1, Sprite(1))
    EXIT FOR
   END IF
  NEXT X!
 ELSE
  FOR X! = StartX(i) TO StartX(i) + StepX!(i) * (Forms(i) - 1) STEP StepX!(i)
   N = N + 1
   IF N = Number - 6 THEN
    CALL BLKGET(X! + CorrX!(i) * (N - 1) + 1, StartY(i) + 101, X! + StepX!(i) - 1, StartY(i) + CorrX!(i) * (N - 1) + StepX!(i) + DeltaSizY(i) - 99, Sprite(1))
    EXIT FOR
   END IF
  NEXT X!
 END IF
END IF
NIL = PAGEACTIVE(0)

END SUB

SUB RenderShip (X, Y, Side, Pl, k, Hnds())
'Изображаем кораблик

Pl = 4 + Side / 3
IF Pl < 1 THEN Pl = 1
IF Pl > 7 THEN Pl = 7
NIL = XMSBLKPUT(1, X, Y, Hnds(k, Pl))
DRWBOX 1, 0, X - 1, Y - 1, X + StepX!(k) + 1, Y + StepX!(k) + 1

END SUB

SUB RenderStars (X, Y, DeltaStars, Speed)

FOR Si = 1 TO MaxNumStars 'Рендер звезд
 P = GETPOINT(StarX(Si), StarY(Si))
 'IF P = StarC(Si) THEN DRWPOINT 1, 0, StarX(Si), StarY(Si)
 StarY(Si) = StarY(Si) + StarSpeed(Si) + Speed / 7
 StarX(Si) = StarX(Si)
 IF StarY(Si) > 480 THEN
  StarY(Si) = RND * 50 - 100
  StarX(Si) = RND * 640
  StarC(Si) = StarColors(INT(RND * 14 + 1))
 END IF
 P = GETPOINT(StarX(Si), StarY(Si))
 'IF P = 0 THEN DRWPOINT 1, StarC(Si), StarX(Si), StarY(Si)
NEXT Si

IF DeltaStars > 0 THEN
 FOR Si = MaxNumStars + 1 TO MaxNumStars + DeltaStars
  P = GETPOINT(StarX(Si), StarY(Si))
  'IF P = StarC(Si) THEN DRWPOINT 1, 0, StarX(Si), StarY(Si)
  StarY(Si) = StarY(Si) + StarSpeed(Si) + Speed / 7
  StarX(Si) = StarX(Si) + StarDeltaX!(Si)
  IF StarY(Si) > 500 THEN
   'DRWPOINT 1, 0, StarX(Si), StarY(Si)
   DeltaStars = DeltaStars - 1
   IF DeltaStars < 2 THEN
    DeltaStars = 0
    'DRWFILLBOX 1, 0, X - 70, Y + 40, X + 125, 480
    EXIT FOR
   END IF
  END IF
  P = GETPOINT(StarX(Si), StarY(Si))
  'IF P = 0 THEN DRWPOINT 1, StarC(Si), StarX(Si), StarY(Si)
 NEXT Si
END IF

END SUB

FUNCTION ResetDsp%

' Resets the DSP
OUT BasePort% + 6, 1
FOR Count% = 1 TO 4
   Junk% = INP(BasePort% + 6)
NEXT
OUT BasePort% + 6, 0
IF INP(BasePort% + 14) AND &H80 = &H80 AND INP(BasePort% + 10) = &HAA THEN
   ResetDsp% = -1
ELSE
   ResetDsp% = 0
END IF

END FUNCTION

FUNCTION SBReset%

OUT BlasterAddr% + &H6, 1   'Reset address
'delay .1
OUT BlasterAddr% + &H6, 0
time1! = TIMER: noreset% = 0
DO
   'Read Data Available port until bit 7 is set
   'This should take about 100 micro seconds...give it 1 full second
   IF TIMER - time1! > 1! THEN noreset% = -1
LOOP UNTIL ((INP(BlasterAddr% + &HE) AND 128) = 128) OR noreset%
IF NOT noreset% THEN
   IF INP(BlasterAddr% + &HA) = &HAA THEN
      SBReset% = -1
   ELSE
      SBReset% = 0
   END IF
ELSE
   SBReset% = 0
END IF

END FUNCTION

SUB SetStereo (OnOff%)

OUT BasePort% + 4, &HE
IF OnOff% THEN OUT BasePort% + 5, 2 ELSE OUT BasePort% + 5, 0

END SUB

SUB SpeakerState (OnOff%)

' Turns speaker on or off.
IF OnOff% THEN WriteDSP &HD1 ELSE WriteDSP &HD3

END SUB

FUNCTION SpeakerStatus%

OUT BasePort% + 4, &HD8
IF INP(BasePort% + 5) = &HFF THEN SpeakerStatus% = -1 ELSE SpeakerStatus% = 0

END FUNCTION

SUB StopMIDI

asm$ = asm$ + CHR$(&HBB) + CHR$(&H4) + CHR$(&H0)
asm$ = asm$ + CHR$(&HCD) + CHR$(&H80)
asm$ = asm$ + CHR$(&HCB)

DEF SEG = VARSEG(asm$)
Offset% = SADD(asm$)
CALL ABSOLUTE(Offset%)

MIDI.PLAYTIME = 0
MIDI.ERROR = 0

END SUB

SUB Titres

HNfon = XMSBLKGET(300, 1, 639, 479)
NIL = PAGEACTIVE(0)
G = 1
'NIL = PCXPUT(1, 0, 1, PathG$ + "tits1.pcx")

DIM Gfx(5000) AS INTEGER

ReDr:
Y = 450
ClearKeys
DO
 NIL = PAGEACTIVE(0)
 NIL = XMSBLKPUT(1, 300, 1, HNfon)
 IF G = 1 THEN
  FOR d = 0 TO 8
   'BLKGET 55, d * 40 + 1, 265, d * 40 + 40, Gfx(0)
   'SPRITEPUT 1, 0, 315, d * 40 + Y, Gfx(0)
  NEXT d
 ELSE
  FOR d = 0 TO 8
   'BLKGET 55, d * 20 + 1, 345, d * 20 + 20, Gfx(0)
   'SPRITEPUT 1, 0, 315, d * 20 + Y, Gfx(0)
  NEXT d
 END IF

 HNtit = XMSBLKGET(300, 1, 639, 479)
 NIL = PAGEACTIVE(1)
 'SetView 300, 140, 639, 450
 NIL = XMSBLKPUT(1, 300, 1, HNtit)
 NIL = XMSFREE(HNtit)
 'SetView 1, 1, 639, 479

 INK$ = INKEY$
 Y = Y - 3
 IF Y < -250 OR INK$ <> "" THEN EXIT DO
LOOP
G = G + 1

IF G < 3 AND INK$ = "" THEN
 NIL = PAGEACTIVE(0)
 'NIL = PCXPUT(1, 0, 1, PathG$ + "tits2.pcx")
 GOTO ReDr
END IF

NIL = XMSFREE(HNfon)

ERASE Gfx

END SUB

SUB VocVolume (Right%, Left%, Getvol%)

OUT BasePort% + 4, &H4
IF Getvol% THEN
   Left% = INP(BasePort% + 5) \ 16
   Right% = INP(BasePort% + 5) AND &HF
   EXIT SUB
ELSE
   OUT BasePort% + 5, (Right% + Left% * 16) AND &HFF
END IF

END SUB

SUB WaitKey

ClearKeys
DO: LOOP WHILE INKEY$ = ""
ClearKeys

END SUB

SUB WaitVSync
'Ожидание обратного хода луча монитора:
DO: a = INP(&H3DA): LOOP WHILE a <> 9
DO: a = INP(&H3DA): LOOP WHILE a = 9
END SUB

SUB WriteDSP (byte%)

' Writes a byte to the DSP
DO
LOOP WHILE INP(BasePort% + 12) AND &H80
OUT BasePort% + 12, byte%

END SUB

SUB WriteToDSP (v%)

DO
LOOP UNTIL (INP(BlasterAddr% + &HC) AND 128) = 0
OUT BlasterAddr% + &HC, v%

END SUB

