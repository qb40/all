'Video Poker - by Viktor Rootselainen  1997-2000  version - see ver$
'QB 4.5 game  SVGA 640x480x256, XMS, mouse and more...
'E-mail: viktor.rootselainen@pp.inet.fi
'www: http://come.to/crashsoft

'This program is abandonware. And i am not going to update it.
'If you improve this game, then e-mail me; i'd like to see it ;)
'The game is in Finnish language, but i am sure it's easy to translate
'text to English =)
'This program needs a svgaqb library, if you don't have it
'then go to http://zephyrsoftware.com and get it there...

DEFINT A-Z
'$DYNAMIC
'$INCLUDE: 'SVGABC.BI'
DECLARE FUNCTION PlayCards ()
DECLARE SUB Game ()
DECLARE FUNCTION CfgSub% (Action%)
DECLARE SUB Rc4Init (Key$)
DECLARE FUNCTION Rc4$ (txt$)
DECLARE FUNCTION xInput% (d$, Row%, col%, VisLen%, MaxLen%, Mask$, TxtChar$, Fgrnd%, Bgrnd%)
DECLARE FUNCTION ChkButton% (X1%, Y1%, X2%, Y2%)
DECLARE SUB Button (X%, Y%, X2%, txt$, Colr%, FColr%, butstat%)
DECLARE SUB DrawMenu ()
DECLARE SUB BuildDeck (Deck() AS ANY)
DECLARE FUNCTION CheckCards ()
DECLARE SUB EndProg ()
DECLARE FUNCTION FileExist% (FileName$)
DECLARE SUB InitGame ()
DECLARE SUB Intro ()
DECLARE SUB LoadCards ()
DECLARE SUB Menu ()
DECLARE SUB ScrollTekst (text$, X%, Y%, position%, textwidth%, gfxpos%)
DECLARE SUB ShowCard (Card%, Mode%)
DECLARE SUB Shuffle (Deck() AS ANY)
DECLARE SUB Tuplaus ()
DECLARE SUB xPUT (X1%, Y1%, X2%, Y2%, SSegment%, Soffset%, DSegment%, Doffset%)
DECLARE SUB delay (time%)
DECLARE SUB Pal (r%, g%, b%, c%)
DECLARE SUB palsub (Act$, delaytime%, palet$)
DECLARE SUB ShowPCX (PCXFileName$, X%, Y%, PCXPAL AS STRING)
DECLARE FUNCTION VrMenu% (X%, Y%, item$(), selcolr%, menucolr%, bgcolr%, sel%)

TYPE Card
  Value       AS INTEGER
  Suit        AS INTEGER
  Hold        AS INTEGER
  GfxNum      AS INTEGER
END TYPE

TYPE CfgType
  Version     AS STRING * 4
  CardBackNum AS STRING * 1
  Reserved    AS STRING * 35
END TYPE

TYPE PlayerType                                   'encrypted with RC4 :)
  Nick        AS STRING * 16                      'player's name
  Pass        AS STRING * 16                      'password
  Money       AS STRING * 4
  Flags       AS STRING * 1                       'unused here
END TYPE


CONST ver$ = "0.61", CfgFile$ = "Poker.cfg"
COMMON SHARED Mx, My, MbNum, bet, Wins, Bucks, Credits, voitto
COMMON SHARED crd1, PokerSCR, CardBackNum, Gstat, CurScroll
COMMON SHARED FileExists, Easymode, SKey$, Rotpal, Rotpalet

DIM SHARED blk2(0 TO 32000), tmpblk(4000)
DIM SHARED GamePal AS STRING * 768, TempPal AS STRING * 768, font1 AS STRING * 4068
DIM SHARED Deck(1 TO 54) AS Card, Player AS PlayerType, tPlayer AS PlayerType
DIM SHARED Cfg AS CfgType, butt(10), but(10), ScrollTxt$(4)


DEF FnKeyBuf : DEF SEG = 0: POKE &H41A, PEEK(&H41C): END DEF  'clear keyboard buffer
DEF FnTrim$ (texti$) : texti$ = LTRIM$(RTRIM$(texti$)): END DEF
DEF FnColr                'default colors of buttons
    Pal 0, 63, 0, 216: Pal 63, 63, 0, 217: Pal 0, 0, 63, 218
    FOR i = 219 TO 226: Pal 63, 0, 0, i: NEXT
END DEF

DEF FnInGame              'palrotate for buttons, text scrolling                                                                       
  Rotpal = Rotpal + Rotpalet: IF Rotpal > 62 THEN Rotpalet = -Rotpalet
  IF Rotpal < 25 THEN Rotpalet = -Rotpalet
  scroll = scroll + 1: IF scroll > 7 THEN scroll = 0: scrolli = scrolli + 1
  IF CurScroll = 2 THEN ScrollTxt$(2) = "Voitit" + STR$(voitto) + " mk. Tuplataanko?  TAB = tuplaus  ESC = voiton maksu  SPACE, ENTER = Jako "    'yes, it's long :)
  IF CurScroll <> 3 THEN ScrollTekst ScrollTxt$(CurScroll), 1, 467, scrolli, 90, scroll
  WAIT &H3DA, 8
END DEF

scrolli = 1: CardBackNum = 0: Rotpalet = 1: Rotpal = 50
ScrollTxt$(0) = "Poker v." + ver$ + "   by Viktor Rootselainen  1997-2000      ESC = lopetaa pelin   1-5 kortin lukitus   SPACE, ENTER - Jako."
ScrollTxt$(1) = "Poker v." + ver$ + "   by Viktor Rootselainen  1997-2000      ESC = voiton maksu   SPACE, ENTER = Jako, <-- panos."
ScrollTxt$(3) = "Paina: vas=Pieni oik=Suuri | Pieni=2,3,4,5,6,7  Suuri=9,10,J,Q,K,A | 8,Joker=??  "


'CHDIR "d:\qb\poker"
InitGame

DO
  Menu                                      'game menu
  Game                                      'play the game
  a = CfgSub(3)                             'save player's account
LOOP



CaptureScreen: a = PCXMAKE(0, 0, GETMAXX, GETMAXY, "pokercap.pcx")
RETURN

FileExistErr: FileExists = 0
RESUME NEXT


XMSCardData:
DATA 1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,1,1,1,1,1
DATA 1,1,1,7
DATA 1,1,1,1,1,1,1,1,1
DATA 1,1,1,1,5

MouseData:
DATA 2,2
DATA 0,0,255,255,255,255,255,255,255,255,255,255,255,255,255,255
DATA 0,215,0,255,255,255,255,255,255,255,255,255,255,255,255,255
DATA 0,215,215,0,255,255,255,255,255,255,255,255,255,255,255,255
DATA 0,215,215,215,0,255,255,255,255,255,255,255,255,255,255,255
DATA 0,215,215,215,215,0,255,255,255,255,255,255,255,255,255,255
DATA 0,215,215,215,215,215,0,255,255,255,255,255,255,255,255,255
DATA 0,215,215,215,215,215,215,0,255,255,255,255,255,255,255,255
DATA 0,215,215,215,215,215,215,215,0,255,255,255,255,255,255,255
DATA 0,215,215,215,215,215,215,215,215,0,255,255,255,255,255,255
DATA 0,215,215,215,215,215,215,215,215,215,0,255,255,255,255,255
DATA 0,215,215,215,215,215,215,215,215,215,215,0,255,255,255,255
DATA 0,215,215,215,215,215,215,215,0,0,0,255,255,255,255,255
DATA 0,215,215,215,215,215,215,215,0,255,255,255,255,255,255,255
DATA 0,215,215,215,0,215,215,215,215,0,255,255,255,255,255,255
DATA 0,215,215,0,255,0,215,215,215,0,255,255,255,255,255,255
DATA 0,215,0,255,255,255,0,215,215,215,0,255,255,255,255,255
DATA 0,0,255,255,255,255,0,215,215,215,0,255,255,255,255,255
DATA 0,255,255,255,255,255,255,0,215,215,215,0,255,255,255,255
DATA 255,255,255,255,255,255,255,0,215,215,215,0,255,255,255,255
DATA 255,255,255,255,255,255,255,255,0,215,215,215,0,255,255,255
DATA 255,255,255,255,255,255,255,255,255,0,215,215,0,255,255,255
DATA 255,255,255,255,255,255,255,255,255,0,215,215,215,0,255,255
DATA 255,255,255,255,255,255,255,255,255,255,0,0,0,255,255,255
DATA 255,255,255,255,255,255,255,255,255,255,255,255,255,255,255,255

REM $STATIC
SUB BuildDeck (Deck() AS Card) STATIC

RESTORE XMSCardData
brb = -1

FOR i = 1 TO 4               'This loop controls the face value.
    FOR j = 2 TO 14          'Figure out which card (1...52) you're creating.
	cardnum = j + (i - 1) * 13
	READ temp: brb = brb + temp
			     'Place the face value and suit into the Card.
	Deck(cardnum).Value = j
	Deck(cardnum).Suit = i
	Deck(cardnum).GfxNum = brb
    NEXT j
NEXT i

Deck(1).Value = 1: Deck(1).Suit = 1: Deck(1).GfxNum = brb + 1

END SUB

FUNCTION CfgSub (Action)
  
free = FREEFILE
IF FileExist(CfgFile$) = 0 THEN          'create it
   OPEN CfgFile$ FOR BINARY AS free: loh& = LOF(free)
   Cfg.Version = ver$: Cfg.CardBackNum = CHR$(CardBackNum)
   PUT free, , Cfg: CLOSE free
END IF


OPEN CfgFile$ FOR BINARY AS free: loh& = LOF(free)

SELECT CASE Action
 CASE 0                                  'load settings
   GET free, , Cfg
   CardBackNum = ASC(Cfg.CardBackNum)

 CASE 1                                  'save settings
   Cfg.CardBackNum = CHR$(CardBackNum)
   PUT free, , Cfg
 
 CASE 2                                  'get player's stats
   GET free, , Cfg
   Num = (loh& - LEN(Cfg)) / LEN(Player)
   FOR i = 1 TO Num
       GET #free, , tPlayer
       Rc4Init (SKey$): tPlayer.Nick = Rc4(tPlayer.Nick)
       IF tPlayer.Nick = Player.Nick THEN Ok = 1: EXIT FOR
   NEXT
   IF Ok THEN
      tPlayer.Pass = Rc4(tPlayer.Pass)
      Credits = CVI(Rc4(tPlayer.Money))
   END IF
   CfgSub = Ok

 CASE 3                                   'Save player's stats
   GET free, , Cfg
   Num = (loh& - LEN(Cfg)) / LEN(Player)  'num. of records
   FOR i = 1 TO Num
       lastpos& = SEEK(free): GET #free, , tPlayer
       Rc4Init (SKey$): tPlayer.Nick = Rc4(tPlayer.Nick)
       tPlayer.Pass = Rc4(tPlayer.Pass)
       IF tPlayer.Nick = Player.Nick THEN SEEK #free, lastpos&: EXIT FOR
   NEXT
  
   Rc4Init (SKey$)
   Credits = Credits + Wins + Bucks
   tPlayer.Nick = Rc4(Player.Nick): tPlayer.Pass = Rc4(Player.Pass)
   tPlayer.Money = Rc4(MKI$(Credits))
   PUT #free, , tPlayer

END SELECT
CLOSE #free


END FUNCTION

FUNCTION CheckCards
DIM asum(15), msum(10), yhd(10)

 ass = 0: jokeri = 0: MSUMMAX = 0: SuoraMax = 0
 FOR kortti = 1 TO 5
     arvo = Deck(kortti).Value: maa = Deck(kortti).Suit
     IF arvo = 14 THEN ass = maa
     IF arvo = 1 THEN jokeri = 1 ELSE asum(arvo) = asum(arvo) + 1: msum(maa) = msum(maa) + 1
 NEXT
 FOR arvo = 2 TO 14: yhd(asum(arvo)) = yhd(asum(arvo)) + 1: NEXT
 FOR maa = 1 TO 4
     IF msum(maa) > MSUMMAX THEN MSUMMAX = msum(maa)
 NEXT maa

 FOR i = 1 TO 10
     temp = 0
     FOR ii = i TO i + 4
	 IF asum(ii - 14 * (ii = 1)) >= 1 THEN temp = temp + 1
     NEXT
     IF temp > SuoraMax THEN
	SuoraMax = temp: SuoraAlku = i
	IF asum(SuoraAlku + 4) <> 0 AND asum(i + 4) <> 1 THEN
	   SuoraMax = temp: SuoraAlku = i
	END IF
     END IF
 NEXT i

 FOR i = 1 TO 5
     IF Deck(i).Value >= 2 AND Deck(i).Value <= 5 THEN jee = jee + 1
 NEXT
 IF jee = 4 AND ass THEN SuoraMax = 5


IF yhd(2) = 1 OR jokeri THEN tulos = 1                              ' 2
IF yhd(2) = 2 THEN tulos = 2                                        ' 2+2
IF yhd(3) = 1 OR (yhd(2) = 1 AND jokeri) THEN tulos = 3             ' 3
IF SuoraMax = 5 OR (SuoraMax = 4 AND jokeri) THEN tulos = 4         ' S
IF MSUMMAX = 5 OR (MSUMMAX = 4 AND jokeri) THEN tulos = 5           ' V
IF yhd(2) = 2 AND jokeri THEN tulos = 6                             ' TK
IF yhd(3) = 1 AND yhd(2) = 1 THEN tulos = 6                         ' TK
IF yhd(4) = 1 OR (yhd(3) = 1 AND jokeri) THEN tulos = 7             ' 4
IF MSUMMAX = 4 AND SuoraMax = 4 AND jokeri THEN tulos = 8           ' VS
IF MSUMMAX = 5 AND SuoraMax = 5 THEN tulos = 8                      ' VS
IF yhd(4) = 1 AND jokeri THEN tulos = 9                             ' 5

CheckCards = tulos

END FUNCTION

SUB DrawMenu
MouseHide

DRWFILLBOX 1, 150, 200, 100, 450, 300
DRWBOX 1, 172, 200, 100, 450, 300
DRWBOX 1, 152, 201, 101, 449, 299
DRWBOX 1, 178, 202, 102, 448, 298

MouseShow

END SUB

SUB EndProg

 a = CfgSub(3)
 DrwString 2, 10, 0, "Copyleft  Viktor Rootselainen", 230, 50
 PALIOAUTO GamePal, 0, 255, -9: MouseExit
 delay 100

 a = XMSFREE(crd1): a = XMSFREE(PokerSCR)
 a = RESTEXT
 END


END SUB

FUNCTION FileExist (FileName$)

ON ERROR GOTO FileExistErr

PRINT "þ "; FileName$ + "...";
FileExists = 1: free = FREEFILE
OPEN FileName$ FOR INPUT AS #free: CLOSE free

IF FileExists THEN FileExist = 1: PRINT "Ok" ELSE PRINT "Not found!"

END FUNCTION

SUB Game

10                                                      'goto s(HELL)!!!
 FOR i = 1 TO 10: butt(i) = 0: but(i) = 0: NEXT i       'clear some variables
 FOR i = 1 TO 54: Deck(i).Hold = 0: NEXT
 Gstat = 0: Shuffle Deck()
 IF Easymode = 0 THEN FOR i = 1 TO 5: SWAP Deck(i), Deck(RND * 48 + 5): NEXT
 'FOR i = 1 TO 5: Deck(i).Value = 7 + i: Deck(i).Suit = 1: NEXT

 IF Bucks > 0 THEN Bucks = Bucks - bet ELSE Wins = Wins - bet
 IF Bucks + Wins > 0 AND Bucks + Wins < bet THEN bet = Bucks + Wins

 CurScroll = 0: Picked = 0: MouseHide
 a = XMSSCREENPUT(PokerSCR): GOSUB 100: MouseHide
 ShowCard 0, 0: a = FnColr
 DrwString 1, 70, 0, "Valitse kortit", 250, 80
 a = FnKeyBuf: MouseShow


50
DO
 k$ = UCASE$(INKEY$)
 MouseStatus Mx, My, MbNum: MouseButPress 1, Mx, My, MbNum, mb
 'DRWSTRING 1, 140, 0, STR$(mx) + STR$(my) + "  ", 10, 5
 a = FnInGame
		       
 FOR i = 1 TO 5
     temp = 90 * i
     Button temp, 365, temp + 80, "Lukitus", 221 + i, 215, butt(i)
 NEXT

 Button 70, 410, 70 + 80, "Tuplaus", 221, 215, but(1)
 Button 160, 410, 160 + 60, "Pieni", 220, 215, but(2)
 Button 230, 410, 230 + 60, "Suuri", 219, 215, but(3)
 Button 300, 410, 300 + 60, "Panos", 218, 215, but(4)
 Button 370, 410, 370 + 120, "Voiton maksu", 217, 0, but(5)
 Button 500, 410, 500 + 60, "Jako", 216, 0, but(6)


 IF Gstat = 0 THEN                       'glavnyi musor v etom esli...

  IF Picked THEN
     Pal 0, Rotpal, 0, 216
     IF k$ = CHR$(13) OR k$ = CHR$(32) OR but(6) = 2 THEN a = PlayCards
     IF a = -1 THEN EXIT SUB
     ELSE Pal 0, 63, 0, 216
  END IF

  FOR i = 1 TO 5
   IF Deck(i).Hold THEN Pal 63, 0, 0, 221 + i ELSE Pal Rotpal, 0, 0, 221 + i
   ix = 2 + (90 * i)
   IF ChkButton(ix, 233, ix + 70, 326) OR butt(i) = 2 THEN
      Deck(i).Hold = Deck(i).Hold XOR 1
      IF Deck(i).Hold THEN Picked = Picked + 1 ELSE Picked = Picked - 1
      GOSUB 100
   END IF
  NEXT

  IF LEN(k$) THEN
   ascii = ASC(k$): temp = ascii - 48
   IF ascii > 48 AND ascii < 54 THEN
      Deck(temp).Hold = Deck(temp).Hold XOR 1
      IF Deck(temp).Hold THEN Picked = Picked + 1 ELSE Picked = Picked - 1
      GOSUB 100
   END IF
  END IF
  IF k$ = CHR$(27) THEN EndProg

 ELSE

  Pal 0, Rotpal, 0, 216: Pal Rotpal, Rotpal, 0, 217: Pal 0, 0, Rotpal, 218
  IF Gstat = 2 THEN                                          'Double
     Pal Rotpal, 0, 0, 221
     IF but(1) = 2 OR k$ = CHR$(9) THEN Tuplaus: IF Gstat = 1 THEN GOSUB 100
  END IF

  IF but(5) = 2 OR k$ = CHR$(27) THEN                        'take money/exit
     IF Gstat = 1 THEN
	EXIT SUB
     ELSE
	Gstat = 1: Wins = Wins + voitto: CurScroll = 1: a = FnColr: GOSUB 100
     END IF
  END IF

  IF but(4) = 2 OR k$ = CHR$(8) THEN
     IF Bucks + Wins < 10 THEN temp = Bucks + Wins ELSE temp = 10
     bet = bet MOD temp + 1: GOSUB 100
  END IF

  IF but(6) = 2 OR k$ = CHR$(13) OR k$ = CHR$(32) THEN
     IF Gstat = 2 THEN Wins = Wins + voitto
     GOTO 10
  END IF

 END IF

LOOP


100 MouseHide
IF Gstat = 0 THEN
   FOR i = 1 TO 5
       IF Deck(i).Hold THEN
	  DrwString 1, 170, 30, "LUKITTU", 5 + (i * 91), 335
	  ELSE DrwString 1, 0, 0, "       ", 5 + (i * 91), 335
       END IF
   NEXT
END IF
		
IF bet > 9 THEN xpos = 439 ELSE xpos = 441
DrwString 1, 210, 17, STR$(bet) + " ", xpos, 15: FONTSET font1
DrwString 1, 30, 204, LTRIM$(STR$(Bucks)), 85, 15
DrwString 1, 30, 204, LTRIM$(STR$(Wins)), 560, 15: FONTSYSTEM: MouseShow
RETURN


END SUB

SUB InitGame

DIM WinMouseCursor AS STRING * 386
SKey$ = "Poker key" + CHR$(205)

RANDOMIZE TIMER
IF WHICHVGA < 0 THEN PRINT "What a fu..? No VGA monitor?": END
CLS : COLOR 15, 9: PRINT STRING$(80, 32)
LOCATE 1, 34: PRINT "Poker v."; ver$; SPC(18); "1997-2000(c) V.R.": COLOR 7, 0

PRINT CHR$(254); WHICHMEM; "kb video memory detected."

cpu = WHICHCPU: cpu$ = LTRIM$(STR$(cpu)): txt$ = " CPU detected."
PRINT CHR$(254); " 80"; cpu$; txt$
IF cpu < 386 THEN PRINT "386 or better processor required!": END

IF WHICHXMS(MEM, handles) = 1 THEN
   IF MEM < 1 OR handles < 1 THEN PRINT "SORRY...THERE IS EITHER NO FREE XMS MEMORY OR NO FREE XMS HANDLES": END
ELSE
   PRINT "Sorry...no active XMS memory manager found"
   PRINT "Make sure you have a XMS memory manager (such as 'HIMEM.SYS' installed)": END
END IF
crd1 = XMSALLOCATE(480)
IF crd1 = 0 THEN PRINT "Not enough XMS memory avialable.": END
PRINT CHR$(254); MEM; "kb XMS memory block max."
PRINT CHR$(254); 480; "kbytes of XMS memory allocated."

BuildDeck Deck()
IF FileExist("Cards.pcx") = 0 THEN END
IF FileExist("Cards2.pcx") = 0 THEN END
IF FileExist("Poker.pcx") = 0 THEN END
IF FileExist("Lcd.fon") = 0 THEN END
OPEN "LCD.FON" FOR BINARY AS #1: GET #1, , font1: CLOSE #1

notsuck = WHICHMOUSE
IF notsuck THEN
   PRINT CHR$(254); notsuck; "button mouse detected."
   ELSE PRINT CHR$(254); "No mouse detected!"
END IF

PRINT "Starting the game..."

palsub "save", 0, TempPal
palsub "fadeout", 15, TempPal
 
 
Dummy = RES640
RESTORE MouseData: MouseEnter
FOR i = 1 TO 386: READ a: MID$(WinMouseCursor, i, 1) = CHR$(a): NEXT i
MOUSECURSORSET WinMouseCursor

CALL VideoOFF: LoadCards
FileName$ = "poker.pcx": ShowPCX FileName$, 0, 0, GamePal
DRWBOX 1, 10, 0, 0, GETMAXX, GETMAXY - 14
DRWFILLBOX 1, 0, 0, 480 - 14, 640, 480
DRWSTRINGLT 1, 162, 0, "Poker v" + ver$, 1, 90
DRWFILLCIRCLE 1, 17, 454, 21, 17
DRWFILLELLIPSE 1, 204, 80, 22, 55, 14: DrwString 1, 24, 204, "Rahat:", 35, 15
DRWFILLELLIPSE 1, 204, 552, 22, 60, 14: DrwString 1, 24, 204, "Voitot:", 500, 15
FOR i = 0 TO 255: DRWPOINT 1, i, i, 460: NEXT
PokerSCR = XMSSCREENGET
a = FnColr: PALGET GamePal, 0, 255

Intro
VideoON
MOUSERANGESET 0, 0, GETMAXX, GETMAXY - 32
ON KEY(31) GOSUB CaptureScreen: KEY(31) ON


END SUB

SUB Intro

FileName$ = "logo.pcx": ShowPCX FileName$, 0, 0, TempPal
VideoON

PALCHGAUTO TempPal, GamePal, 0, 255, 10
'PALIOAUTO gamepal, 0, 255, -5
'PALIOAUTO gamepal, 0, 255, 40

END SUB

SUB LoadCards
'cards are ripped from dll :)

FileName$ = "cards.pcx": ShowPCX FileName$, 0, 0, GamePal

i = 0
FOR Y = 0 TO GETMAXY STEP 96
    FOR X = 0 TO GETMAXX - 71 STEP 71
	BlkGet X, Y, X + 70, Y + 95, blk2(0)
	je = XMSPut(blk2(0), crd1, i * 6800&, 6800)
	i = i + 1
    NEXT
    IF je = 0 THEN SCREEN 0, 0: PRINT "XMS Error "; XMSERROR; i * 6800&: END
NEXT
FILLSCREEN 0


FileName$ = "cards2.pcx": ShowPCX FileName$, 0, 0, GamePal
FOR Y = 0 TO 96 STEP 96
    FOR X = 0 TO GETMAXX - 71 STEP 71
	BlkGet X, Y, X + 70, Y + 95, blk2(0)
	je = XMSPut(blk2(0), crd1, i * 6800&, 6800)
	i = i + 1
    NEXT
    IF je = 0 THEN SCREEN 0, 0: PRINT "XMS Error"; XMSERROR: END
NEXT


END SUB

SUB Menu
DIM item1$(5), item2$(5), item3$(5)

item1$(1) = "Aloita pelin": item1$(2) = "Poista Pelaajan": item1$(3) = "Top 10"
item1$(4) = "Asetukset": item1$(5) = "Lopeta pelin"
item2$(1) = "Pakka": item2$(2) = "Easy mode": item2$(3) = "Ž„niasetukset"
item2$(4) = "Animaatiot": item2$(5) = "P„„valikkoon"
item3$(1) = "Ž„nikortti": item3$(2) = "Ž„ni": item3$(3) = "Voimakkuus": item3$(4) = "P„„valikkoon"

menu.main:
DrawMenu
DrwString 3, 90, 150, "Video poker", 280, 110

DO
  k$ = UCASE$(INKEY$)
  keys = VrMenu(240, 140, item1$(), 176, 90, 150, sel)
  IF keys = -27 THEN EXIT DO
  IF sel = 1 THEN
     DrawMenu
     DrwString 3, 90, 150, "Aloita pelin", 280, 110

     DrwString 1, 90, 150, "Anna nimesi:", 210, 160           'player's name
     a = xInput(Player.Nick, 230, 180, 16, 16, "", "", 90, 200)
    
     DrwString 1, 90, 150, "Anna salasanasi:", 210, 220       'password
     a = xInput(Player.Pass, 230, 240, 10, 16, "", "", 90, 200)
    
     IF CfgSub(2) = 0 THEN                                    'check user
	DrwString 1, 90, 210, "Olet uusi pelaaja.", 210, 220
	tPlayer.Pass = Player.Pass: Credits = 30              'create new user
     END IF
    
     IF tPlayer.Pass = Player.Pass THEN
	temp$ = "20": DrawMenu: MouseHide
	DrwString 1, 90, 150, "Sinulla on" + STR$(Credits) + " mk tilill„", 210, 240
	DrwString 1, 90, 150, "Kuinka monta rahaa otat?", 210, 160
	a = xInput(temp$, 220, 180, 8, 8, "1234567890 ", "", 90, 200)
	Bucks = VAL(temp$): IF Bucks > Credits THEN Bucks = Credits
	Credits = Credits - Bucks: bet = 5: Wins = 0       'do not cheat here!
	CALL MouseShow: EXIT SUB
     ELSE
	DrwString 1, 90, 150, "V„„r„ salasana", 210, 220      'bad pssword
     END IF
  END IF
  IF sel = 4 THEN GOSUB menu.options
  IF sel = UBOUND(item1$) THEN EXIT DO
LOOP
EndProg


menu.options: DrawMenu
IF Easymode THEN txt$ = "ON" ELSE txt$ = "OFF"
DrwString 1, 90, 150, txt$, 380, 160
DrwString 3, 90, 150, "Asetukset", 280, 110
DO
  je = XMSGet(crd1, (39 + CardBackNum) * 6800&, blk2(0), 6800)
  BlkPut 1, 300, 5, blk2(0)

  keys = VrMenu(240, 130, item2$(), 176, 90, 150, sel2)
  IF keys = -27 OR sel2 = 5 THEN GOTO menu.main
  IF sel2 = 1 THEN
     CardBackNum = CardBackNum + 1
     IF CardBackNum > 5 THEN CardBackNum = 0
  END IF
  IF sel2 = 2 THEN Easymode = Easymode XOR 1: GOTO menu.options
LOOP


END SUB

FUNCTION PlayCards

 a = FnColr: MouseHide
 FOR i = 1 TO 5
     IF Deck(i).Hold = 0 THEN SWAP Deck(i), Deck(RND * 48 + 6)
 NEXT
 ShowCard 1, 1 + i

 text$ = "": voitto = 0: tulos = CheckCards
 SELECT CASE tulos
     CASE 1
	'text$ = "Pari (ei voittioa)"
     CASE 2
	text$ = "2 Paria": voitto = 3 * bet                '2 pairs
     CASE 3
	text$ = "Kolmoset": voitto = 3 * bet               '3 of kind
     CASE 4
	text$ = "Suora": voitto = 4 * bet                  'straight
     CASE 5
	text$ = "V„ri": voitto = 5 * bet                   'flush
     CASE 6
	text$ = "T„ysk„si": voitto = 10 * bet              'full house
     CASE 7
	text$ = "Neloset": voitto = 15 * bet               '4 of kind
     CASE 8
	text$ = "V„risuora": voitto = 30 * bet             'straight flush
     CASE 9
	text$ = "Viitoset": voitto = 50 * bet              '4 of kind+joker!
     CASE ELSE
	'text$ = "nuthing"
 END SELECT

 Gstat = 1: scrolli = 0
 IF tulos > 1 THEN Gstat = 2: CurScroll = 2 ELSE CurScroll = 1

 IF Wins + Bucks < 1 AND voitto = 0 THEN                   'out of money :)
    DrwString 1, 30, 0, "GAME OVER!", 1, 200
    BlkGet 1, 200, 90, 210, tmpblk(0): DRWFILLBOX 1, 0, 1, 200, 90, 210
    FOR i = 10 TO 200
	BLKRESIZE i + 100, i, tmpblk(0), blk2(0)
	BlkPut 1, 230, 140, blk2(0)
	WAIT &H3DA, 8
    NEXT
    delay 1000: PlayCards = -1
 END IF

 DRWFILLBOX 1, 0, 80, 335, 520, 350
 DrwString 1, 180, 0, text$, 255, 335
 a = FnKeyBuf: MouseShow


END FUNCTION

SUB ScrollTekst (text$, X, Y, position, textwidth, gfxpos)

IF X = 0 AND Y = 0 THEN X = tempx: Y = tempy
'IF x <= 0 OR x > 25 OR y <= 0 OR y > 80 OR position <= 0 THEN EXIT SUB

txt$ = SPACE$(textwidth - 1) + text$ + " "
print$ = MID$(txt$, position, textwidth)
DrwString 1, 140, 0, print$, X - gfxpos, Y
IF position > LEN(txt$) THEN position = 1

END SUB

SUB ShowCard (Card, Mode)
'This sub 'animates' cards

DRWFILLBOX 1, 0, 83, 61, 90 + 74, 61 + 100
je = XMSGet(crd1, (39 + CardBackNum) * 6800&, blk2(6800), 6800)
IF Mode = 0 THEN
 X = 90: xx = RND * 10 + 5: a = RND * 20 + 30
 FOR qwe = 1 TO a
     X = X + xx: IF X > 110 OR X < 70 THEN xx = -xx
     BlkPut 1, X, 61, blk2(6800): BlkPut 1, X + 2, 61 + 2, blk2(6800)
     WAIT &H3DA, 8: DRWFILLBOX 1, 0, X, 61, X + 71, 61 + 96
 NEXT
END IF

FOR i = 1 TO 5
    IF Deck(i).Hold <> 1 THEN totl = totl + 1
NEXT
IF totl = 0 AND Mode THEN GOTO ShowcardEnd

SETVIEW 0, 0, GETMAXX, 325
xx = 90: Y = 230
FOR Y1 = 61 TO Y - 95 STEP 1
    BlkPut 1, 90, Y1, blk2(6800): delay 2
NEXT


FOR i = 1 TO 5
 X = 90 * i: Card = i: xx = X - 90: IF i = 1 THEN xx = 90
 tm = Deck(Card).GfxNum

 FOR X1 = xx TO X STEP 1
     BlkPut 1, X1 + 2, Y - 95, blk2(6800)
     BlkPut 1, X1, Y - 95, blk2(6800): delay 1
 NEXT

 IF Deck(i).Hold <> 1 THEN
    FOR Y1 = Y - 95 TO Y STEP 1
	BlkPut 1, X1, Y1, blk2(6800): delay 1
    NEXT
   
    je = XMSGet(crd1, tm * 6800&, blk2(0), 6800)
    sg = VARSEG(blk2(0)): of = VARPTR(blk2(0))
    totl = totl - 1: IF totl = 0 AND Mode THEN temp = 75 ELSE temp = 1
    FOR Y1 = Y TO Y + 95 STEP 2
	xPUT 0, Y1 - Y, 80, Y1 - Y + 1, sg, of, VARSEG(tmpblk(0)), VARPTR(tmpblk(0))
	BlkPut 1, X1, Y1, tmpblk(0): BlkPut 1, X1, Y1 + 2, blk2(6800)
	IF totl = 0 AND Mode THEN temp = temp - 2
	delay temp
    NEXT
    BlkPut 1, X1, Y, blk2(0)
    'DrwString 1, x1, y, STR$(Deck(Card).Value), x1, y
 END IF
IF totl = 0 AND Mode THEN EXIT FOR
NEXT i

ShowcardEnd:
DRWFILLBOX 1, 0, 450, 105, 521, 229
FOR i = 1 TO 7 STEP 2: BlkPut 1, 83 + i, 61 + i, blk2(6800): NEXT
SETVIEW 0, 0, GETMAXX, GETMAXY


END SUB

SUB Shuffle (Deck() AS Card) STATIC

  'Shuffle by transposing 1000 randomly selected pairs of cards.
  FOR i = 1 TO 1000
      CardOne = INT(53 * RND + 1)
      CardTwo = INT(53 * RND + 1)
      SWAP Deck(CardOne), Deck(CardTwo)
  NEXT i

END SUB

SUB Tuplaus
SHARED scroll, scrolli

DRWFILLBOX 1, 0, 90, 61, 90 + 74, 61 + 100: DRWFILLBOX 1, 0, 90, 230, 520, 325
DRWFILLBOX 1, 0, 80, 335, 520, 350
scrolli = 10: CurScroll = 3: Y = 230: kortti = (RND * 52) + 1
tm = Deck(kortti).GfxNum: tm3 = Deck(kortti).Value

FOR Y1 = 61 TO Y STEP 1
    BlkPut 1, Y1 + 20, Y1, blk2(6800)
NEXT


DrwString 1, 70, 0, STR$(tm3), 120, 100            'this is just for debug ;)
DrwString 1, 140, 0, ScrollTxt$(CurScroll), 10, 467

DO
 k$ = UCASE$(INKEY$): MouseStatus Mx, My, MbNum: MouseButPress 1, Mx, My, MbNum, mb
 Button 160, 410, 160 + 60, "Pieni", 219, 215, but(2)
 Button 230, 410, 230 + 60, "Suuri", 220, 215, but(3)
 IF but(2) = 2 OR k$ = CHR$(0) + CHR$(75) THEN result = 1: EXIT DO
 IF but(3) = 2 OR k$ = CHR$(0) + CHR$(77) THEN result = 2: EXIT DO

 Pal Rotpal, 0, 0, 219: Pal Rotpal, 0, 0, 220
 a = FnInGame
LOOP


IF result = 1 AND tm3 <= 7 AND tm3 > 1 THEN oikein = 1
IF result = 2 AND tm3 > 8 THEN oikein = 1
IF oikein THEN
   voitto = voitto * 2
   IF voitto > 200 THEN Gstat = 1: Wins = Wins + voitto: CurScroll = 1 ELSE CurScroll = 2
 ELSE
   voitto = 0: Gstat = 1: CurScroll = 1
END IF

je = XMSGet(crd1, tm * 6800&, blk2(0), 6800)
BlkPut 1, Y1 + 20, Y1, blk2(0)
a = FnColr


END SUB

