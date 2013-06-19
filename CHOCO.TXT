'ฎฏฐฒณดตถทธนบปผฝพฟม ภ ม ย รฤลฦวศษสหฬอฮฯะัาำิีึืฺุูÛÜÝÞ฿เแโ๐๑๒๓๔๕๖๗๘๙๚๛üýþÿ
'ฝ ผ ป บ น ธ ท ถ ต ด ณ พ ฟ ภ ม ย ร ฤ ล ฦ ว ศ ษ ส ห ฬ อ ฮ ฯ ะ ั า ำ ิ ี ึ ื
COMMON SHARED player$, money, port, portstate()

'player$=player name
'money=money you have
'port=number of menu4
'portstate()=open or closed
'inventory()=what you have


DECLARE FUNCTION nospc$ (s$)
DECLARE SUB box (s$, x1, y1, x2, y2, c1, c2, c3)
DECLARE SUB accomodate (s$, x1, x2)
DECLARE SUB menu (x, n)
DECLARE SUB click1 ()

DECLARE FUNCTION world ()

DECLARE SUB requirement (s$)

DECLARE SUB d1item (x, y, n, c, t)
DECLARE SUB d1bars (x, y, n, c)
DECLARE SUB d1squares (x, y, n, c)
DECLARE SUB d1infusions (x, y, n, c)
DECLARE SUB d1truffles (x, y, n, c)

DECLARE FUNCTION r1item$ (n, x, t)
DECLARE FUNCTION r1bars$ (n, x)
DECLARE FUNCTION r1squares$ (n, x)
DECLARE FUNCTION r1infusions$ (n, x)
DECLARE FUNCTION r1truffles$ (n, x)

DECLARE FUNCTION r1world$ (n, x)
DECLARE FUNCTION cost (n, x)

DECLARE SUB i1bars ()
DECLARE SUB i1squares ()
DECLARE SUB i1infusions ()
DECLARE SUB i1truffles ()

DECLARE FUNCTION win1$ (n)
DECLARE FUNCTION win2$ (n, x)

DECLARE SUB dnplayer (s$)
DECLARE SUB dnmoney (m AS LONG)
DECLARE FUNCTION dnitem$ ()
DECLARE FUNCTION pause ()

DEFLNG M
'keys
CONST backspc = 8, enter = 13, htab = 9, esc = 27
CONST left = 75, right = 77, up = 72, down = 80
CONST uplt = 71, uprt = 73, dnlt = 79, dnrt = 81
CONST insert = 82, home = 73, pageup = 71, del = 83, endk = 81, pagedn = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134


GOTO game
CLS
COLOR 1
PRINT "ษอออออออออออออออออออป"
PRINT "บ Loading game ...  ำฤฤฤฤฤฤฤฤฤฤท"
PRINT "ศออออออออออออออออออออออออออออออผ"

FOR i = 1 TO 78 STEP 3
LOCATE 8, 1
COLOR 12
'FOR j = 1 TO 79
'PRINT CHR$(178);
'SOUND 21000, .1
'NEXT
box CHR$(176), i, 10, i + 3, 14, 4, 3, 2
click1
LOCATE 8, 1
PRINT SPC(79);
NEXT


nn: CLS
COLOR 12
PRINT SPC(25); "  บ                     บ"
PRINT SPC(25); "ออฮอออออออออออออออออออออฮออ"
PRINT SPC(25); "  วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT SPC(25); "  บ  CHOCOLATE FACTORY  บ"
PRINT SPC(25); "  วฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤฤถ"
PRINT SPC(25); "ออฮอออออออออออออออออออออฮออ"
PRINT SPC(25); "  บ                     บ"
COLOR 15
LOCATE 4, 31
PRINT "CHOCOLATE FACTORY";

'main menu
box "                                Main menu", 1, 10, 80, 25, 7, 5, 4
menu 1, 1

ps = 1
DO UNTIL (k$ = CHR$(enter))
k$ = ""
WHILE k$ = ""
k$ = INKEY$
WEND

SELECT CASE k$
CASE CHR$(0) + CHR$(up)
ps = ps - 1
IF (ps < 1) THEN ps = 5
menu 1, ps
CASE CHR$(0) + CHR$(down)
ps = ps + 1
IF (ps > 5) THEN ps = 1
menu 1, ps
CASE CHR$(esc)
GOTO quit
CASE CHR$(enter)
EXIT DO
CASE ELSE
END SELECT
LOOP

GOTO n1
quit: CLS
PRINT "Credits : Subhajit Sahu"
SOUND 21000, 40
SYSTEM

n1:
IF (ps = 4) THEN
CLS
COLOR 15, 0
box "Credits", 1, 10, 40, 20, 7, 6, 4
COLOR 15
LOCATE 13, 15
PRINT "Subhajit Sahu";
LOCATE 14, 15
PRINT "And my freinds.";
k$ = INPUT$(1)
CLS
GOTO nn
END IF
IF (ps = 3) THEN
CLS
COLOR 15, 0
box "Objective", 1, 1, 40, 20, 7, 6, 4
LOCATE 5, 3
COLOR 15
PRINT "Your objective is to";
LOCATE 6, 3
PRINT "make more and more money";
LOCATE 7, 3
PRINT "by selling chocolates";
k$ = INPUT$(1)
box "Controls", 1, 1, 40, 20, 7, 6, 4
LOCATE 5, 3
COLOR 15
PRINT "Controls are  easy";
LOCATE 6, 3
PRINT "you will know it while playing";
LOCATE 7, 3
PRINT "Ctrl + letter is is the";
LOCATE 8, 3
PRINT "combination key for any task";
k$ = INPUT$(1)
CLS
GOTO nn
END IF
IF (ps = 1) THEN GOTO game
IF (ps = 2) THEN GOTO cont
SYSTEM



'ฝ ผ ป บ น ธ ท ถ ต ด ณ พ ฟ ภ ม ย ร ฤ ล ฦ ว ศ ษ ส ห ฬ อ ฮ ฯ ะ ั า ำ ิ ี ึ ื
game:
'printing boxes
CLS
COLOR 10
LOCATE 23, 1
PRINT "ษออออออออหอออออออออออออหออออออหอออออออหออออออออออหออออออออออออออออหอออออออออออป";
LOCATE 24, 1
PRINT "บ Player บ 0000000000$ บ Item บ World บ Messages บ Factory Status บ Inventory บ";
LOCATE 25, 1
PRINT "ศออออออออสอออออออออออออสออออออสอออออออสออออออออออสออออออออออออออออสอออออออออออผ";
d1bars 1, 1, 14, 0
a$ = dnitem$


        


cont:

DEFSNG M
SUB accomodate (s$, x1, x2)
ln = CSRLIN
LOCATE ln, x1
FOR i = 1 TO LEN(s$)
FOR j = i TO LEN(s$)
k$ = MID$(s$, j, 1)
IF (k$ = " " OR k$ = ".") THEN
wl = j - i + 1
p = POS(ln)
IF (x2 - p >= wl) THEN
        PRINT MID$(s$, i, j - i + 1);
        i = j
        EXIT FOR
        ELSE
        ln = ln + 1
        LOCATE ln, x1
        PRINT MID$(s$, i, j - i + 1);
        i = j
        EXIT FOR
END IF
END IF
NEXT
NEXT
END SUB

SUB box (s$, x1, y1, x2, y2, c1, c2, c3)
FOR i = 1 TO x2 - x1 - 2
        COLOR c3, 0
        LOCATE y1, x1
        PRINT "ษ";
        LOCATE y1 + 1, x1
        PRINT "บ";
        LOCATE y1 + 2, x1
        PRINT "ฬ";

                FOR j = 1 TO i
                SOUND 21000, .03
                COLOR c3, 0
                LOCATE y1, x1 + j
                PRINT "อ";
                LOCATE y1 + 2, x1 + j
                PRINT "อ";
                COLOR c1, c2
                LOCATE y1 + 1, x1 + j
                PRINT " ";
                NEXT
        COLOR c3, 0
        LOCATE y1, x1 + i + 1
        PRINT "ป";
        LOCATE y1 + 1, x1 + i + 1
        PRINT "บ";
        LOCATE y1 + 2, x1 + i + 1
        PRINT "น";
        LOCATE y1 + 3, x1
        PRINT "ศ";
        FOR k = x1 + 1 TO x1 + i
        PRINT "อ";
        NEXT
        PRINT "ผ";
NEXT

LOCATE y1 + 1, x1 + 1
COLOR c1, c2
PRINT s$;

d = x2 - x1 - 2

COLOR c3, 0

FOR i = y1 + 3 TO y2
        FOR j = y1 + 3 TO i - 1
                SOUND 21000, .1
                LOCATE j, x1
                PRINT "บ"; SPC(d); "บ";
        NEXT
        LOCATE i, x1
        PRINT "ศ";
        FOR k = 1 TO d
                PRINT "อ";
        NEXT
        PRINT "ผ";
NEXT

END SUB

SUB click1
FOR i = 200 TO 700 STEP 100
SOUND i, .05
SOUND i * 10, .05
SOUND i * 5, .05
NEXT
END SUB

FUNCTION cost (n, x)
SHARED port

DIM plc(15)
plc(1) = 1005
plc(2) = 1505
plc(3) = 1208
plc(4) = 1710
plc(5) = 1312
plc(6) = 2220
plc(7) = 3201
plc(8) = 3210
plc(9) = 5020
plc(10) = 4505
plc(11) = 6010
plc(12) = 6506
plc(13) = 7013
plc(14) = 7522
plc(15) = 6805


pp1 = INT(plc(port) / 100)
pp2 = plc(port) MOD 100
lp1 = INT(plc(n) / 100)
lp2 = plc(n) MOD 100
dis = INT(SQR(ABS(pp1 - lp1) * ABS(pp1 - lp1) + ABS(pp2 - lp2) * ABS(pp2 - lp2)))

SELECT CASE x
CASE 1
rt = INT(dis / 5)
IF (rt = 0) THEN rt = 1
CASE 2
rt = INT(31.4 * dis)
CASE 3
IF (dis <= 5) THEN rt = 1 ELSE rt = 2
CASE ELSE
END SELECT
cost = rt
END FUNCTION

SUB d1bars (x, y, n, c)
IF (c = 1) THEN z = 16
SELECT CASE n
CASE 1
c1 = 6
c2 = 4
c3 = 6
s$ = "ฤ"
CASE 2
c1 = 4
c2 = 6
c3 = 4
s$ = "ฤ"
CASE 3
c1 = 6
c2 = 7
c3 = 15
s$ = "ฤ"
CASE 4
c1 = 6
c2 = 4
c3 = 5
s$ = "๘"
CASE 5
c1 = 6
c2 = 4
c3 = 14
s$ = "๖"
CASE 6
c1 = 4
c2 = 6
c3 = 0
s$ = "๖"
CASE 7
c1 = 6
c2 = 4
c3 = 14
s$ = "๐"
CASE 8
c1 = 6
c2 = 4
c3 = 10
s$ = "ื"
CASE 9
c1 = 4
c2 = 6
c3 = 0
s$ = "๐"
CASE 10
c1 = 4
c2 = 6
c3 = 0
s$ = "ื"
CASE 11
c1 = 4
c2 = 6
c3 = 0
s$ = "๗"
CASE 12
c1 = 4
c2 = 6
c3 = 0
s$ = "ฐ"
CASE 13
c1 = 4
c2 = 0
c3 = 6
s$ = "ฒ"
CASE 14
c1 = 15
c2 = 7
c3 = 7
s$ = "ฤ"
CASE 15
c1 = 4
c2 = 6
c3 = 0
s$ = "๘"
CASE ELSE
END SELECT

COLOR c1 + z, c2
LOCATE y, x
PRINT "ฺฤยฤยฤยฤยฤฟ";
FOR i = 1 TO 2
LOCATE y + i, x
PRINT "ร";
        FOR j = 1 TO 4
        COLOR c3 + z, c2
        PRINT s$;
        COLOR c1 + z, c2
        PRINT "ล";
        NEXT
COLOR c3 + z, c2
PRINT s$;
COLOR c1 + z, c2
PRINT "ด";
NEXT
COLOR c1 + z, c2
LOCATE y + 3, x
PRINT "ภฤมฤมฤมฤมฤู";
COLOR 15, 0
END SUB

SUB d1infusions (x, y, n, c)
IF (c = 1) THEN z = 16
SELECT CASE n
CASE 1
c1 = 6
c3 = 4
s$ = "๕"
CASE 2
c1 = 6
c3 = 7
s$ = "อ"
CASE 3
c1 = 6
c3 = 4
s$ = "฿"
CASE 4
c1 = 4
c3 = 6
s$ = "ุ"
CASE 5
c1 = 4
c3 = 5
s$ = "๗"
CASE 6
c1 = 7
c3 = 5
s$ = "þ"
CASE 7
c1 = 4
c3 = 14
s$ = "ฤ"
CASE 8
c1 = 6
c3 = 6
s$ = "Û"
CASE 9
c1 = 8
c3 = 10
s$ = "ฐ"
CASE 10
c1 = 4
c3 = 13
s$ = "Ü"
CASE 11
c1 = 4
c3 = 4
s$ = "โ"
CASE 12
c1 = 13
c3 = 4
s$ = "Ü"
CASE 13
c1 = 10
c3 = 4
s$ = "พ"
CASE 14
c1 = 14
c3 = 6
s$ = "๓"
CASE 15
c1 = 4
c3 = 15
s$ = "๘"
CASE ELSE
END SELECT


COLOR c1 + z
LOCATE y, x
PRINT "ึะท";
LOCATE y + 1, x
PRINT "บ";
COLOR c3 + z, c1
PRINT s$;
COLOR c1 + z, 0
PRINT "บ";
LOCATE y + 2, x
PRINT "ศอผ";

END SUB

SUB d1item (y, x, n, c, t)
SELECT CASE t
CASE 1
d1bars x, y, n, c
CASE 2
d1squares x, y, n, c
CASE 3
d1infusions x, y, n, c
CASE 4
d1truffles x, y, n, c
CASE ELSE
END SELECT
END SUB

SUB d1squares (x, y, n, c)
IF (c = 1) THEN z = 16
SELECT CASE n
CASE 1
c1 = 8
c3 = 4
s$ = "ฐ"
CASE 2
c1 = 8
c3 = 4
s$ = "ฒ"
CASE 3
c1 = 8
c3 = 4
s$ = "บ"
CASE 4
c1 = 4
c3 = 6
s$ = "๘"
CASE 5
c1 = 4
c3 = 6
s$ = "๗"
CASE 6
c1 = 6
c3 = 4
s$ = "Û"
CASE 7
c1 = 4
c3 = 5
s$ = "๐"
CASE 8
c1 = 6
c3 = 7
s$ = "ื"
CASE 9
c1 = 4
c3 = 14
s$ = "þ"
CASE 10
c1 = 4
c3 = 10
s$ = "๔"
CASE 11
c1 = 4
c3 = 14
s$ = "โ"
CASE 12
c1 = 15
c3 = 4
s$ = "Ü"
CASE 13
c1 = 7
c3 = 6
s$ = "พ"
CASE 14
c1 = 6
c3 = 7
s$ = "๓"
CASE 15
c1 = 4
c3 = 14
s$ = "๘"
CASE ELSE
END SELECT

COLOR c1 + z
LOCATE y, x
PRINT "ษอป";
LOCATE y + 1, x
PRINT "บ";
COLOR c3 + z, c1
PRINT s$;
COLOR c1 + z, 0
PRINT "บ";
LOCATE y + 2, x
PRINT "ศอผ";
END SUB

SUB d1truffles (x, y, n, c)
IF (c = 1) THEN z = 16
SELECT CASE n
CASE 1
c1 = 4
c3 = 4
s$ = "Û"
CASE 2
c1 = 6
c3 = 6
s$ = "Û"
CASE 3
c1 = 4
c3 = 10
s$ = "ม"
CASE 4
c1 = 4
c3 = 5
s$ = "๘"
CASE 5
c1 = 4
c3 = 7
s$ = "๗"
CASE 6
c1 = 6
c3 = 7
s$ = "๐"
CASE 7
c1 = 4
c3 = 15
s$ = "฿"
CASE 8
c1 = 4
c3 = 14
s$ = "ฒ"
CASE 9
c1 = 4
c3 = 6
s$ = "ฐ"
CASE 10
c1 = 4
c3 = 7
s$ = "ฮ"
CASE 11
c1 = 4
c3 = 8
s$ = "โ"
CASE 12
c1 = 13
c3 = 6
s$ = "โ"
CASE 13
c1 = 4
c3 = 10
s$ = "พ"
CASE 14
c1 = 4
c3 = 13
s$ = "๓"
CASE 15
c1 = 3
c3 = 3
s$ = "๘"
CASE ELSE
END SELECT


COLOR c1 + z
LOCATE y, x
PRINT "ึฤท";
LOCATE y + 1, x
PRINT "บ";
COLOR c3 + z, c1
PRINT s$;
COLOR c1 + z, 0
PRINT "บ";
LOCATE y + 2, x
PRINT "ศอผ";
END SUB

FUNCTION dnitem$
DIM area(25, 80), cl(25, 80)
FOR i = 1 TO 25
FOR j = 1 TO 80
area(i, j) = SCREEN(i, j)
cl(i, j) = SCREEN(i, j, 1)
NEXT
NEXT
dnitem1: box "Items", 1, 1, 81, 25, 12, 13, 14
menu 2, 1
ps = 1
k$ = ""
DO UNTIL (k$ = CHR$(enter) OR k$ = CHR$(esc))
k$ = ""
WHILE k$ = ""
k$ = INKEY$
WEND
SELECT CASE k$
CASE CHR$(0) + CHR$(up)
ps = ps - 1
IF (ps < 1) THEN ps = 4
menu 2, ps
CASE CHR$(0) + CHR$(down)
ps = ps + 1
IF (ps > 4) THEN ps = 1
menu 2, ps
CASE "`"
EXIT DO
CASE CHR$(esc)
EXIT DO
CASE CHR$(enter)
EXIT DO
CASE ELSE
END SELECT
LOOP

IF (k$ = CHR$(esc)) THEN GOTO eitem
IF (k$ = "`") THEN
CLS
FOR i = 1 TO 25
FOR j = 1 TO 80
LOCATE i, j
cl1 = INT(cl(i, j) / 128) + (cl(i, j) MOD 16)
cl2 = INT(cl(i, j) / 16) MOD 8
COLOR cl1, cl2
PRINT CHR$(area(i, j));
NEXT
NEXT
ERASE area, cl
GOTO eitem
END IF

IF (k$ = CHR$(enter)) THEN
k$ = win1$(ps)
IF (k$ = CHR$(esc)) THEN
GOTO eitem
ELSEIF (k$ = "`") THEN
GOTO dnitem1
ELSE
GOTO eitem
END IF
END IF

eitem: dnitem$ = k$
END FUNCTION

SUB dnmoney (m AS LONG)
mm$ = STR$(m)
IF (m > 0) THEN mm$ = RIGHT$(mm$, LEN(mm$) - 1)
FOR i = LEN(mm$) TO 10
mm$ = " " + mm$
NEXT
LOCATE 24, 12
COLOR 13, 2
PRINT mm$;
COLOR , 0
END SUB

SUB dnplayer (s$)
IF (LEN(s$) > 6) THEN a$ = LEFT$(s$, 6) ELSE a$ = s$
LOCATE 24, 3
COLOR 15, 14
PRINT a$;
COLOR , 0
END SUB

SUB i1bars

box "                 Bars", 1, 1, 80, 25, 6, 5, 4
COLOR 3
LOCATE 4, 3
PRINT "Luscious, luxurious bars made with the finest";
LOCATE 5, 5
PRINT "chocolates accented in a wide variety of";
LOCATE 6, 17
PRINT "flavours.";

'1basic chocolate bars
d1bars 8, 3, 1, 0

'2dark chocolate bars
d1bars 8, 18, 2, 0

'3milk chocolate bars
d1bars 8, 33, 3, 0

'4chocolate bars w/almonds
d1bars 8, 48, 4, 0

'5chocolate bars w/cashews
d1bars 8, 63, 5, 0

'6chocolate bars w/coffee
d1bars 13, 3, 6, 0

'7chocolate bars w/trinidad lemons
d1bars 13, 18, 7, 0

'8chocolate bars w/fresh mint
d1bars 13, 33, 8, 0

'9exclusive quito cacao chocolate bars
d1bars 13, 48, 9, 0

'10special triniad cacao chocolate bars
d1bars 13, 63, 10, 0

'11mahajanga cacao chocolate bars
d1bars 18, 3, 11, 0

'12colombo cacao chocolate bars
d1bars 18, 18, 12, 0

'13extra dark chocolate bars
d1bars 18, 33, 13, 0

'14white chocolate bars
d1bars 18, 48, 14, 0

'15dark bars w/almonds
d1bars 18, 63, 15, 0

LOCATE 23, 3
PRINT "Click on a chocolate to view its page in the";
LOCATE 24, 16
PRINT "Recipie Book.";

END SUB

SUB i1infusions

box "                 Infusions", 1, 1, 80, 25, 6, 5, 4
COLOR 3
LOCATE 4, 3
PRINT "Creamy European-style Ganache centers in a";
LOCATE 5, 5
PRINT "range of exotic spices, fruits and nuts that";
LOCATE 6, 13
PRINT "simply melt in the mouth.";

'1milk chocolate infusions
d1infusions 8, 3, 1, 0

'2coconut chocolate infusions
d1infusions 8, 18, 2, 0

'3cherry chocolate infusions
d1infusions 8, 33, 3, 0

'4coffee chocolate infusions
d1infusions 8, 48, 4, 0

'5raspberry chocolate infusions
d1infusions 8, 63, 5, 0

'6vanilla almond infusions
d1infusions 13, 3, 6, 0

'7lemon chocolate infusions
d1infusions 13, 18, 7, 0

'8cinnamon chocolate infusions
d1infusions 13, 33, 8, 0

'9mint chocolate infusions
d1infusions 13, 48, 9, 0

'10mahajanga cherry chocolate infusions
d1infusions 13, 63, 10, 0

'11quito chocolate coffee infusions
d1infusions 18, 3, 11, 0

'12raspberry colombo cacao chocolate infusions
d1infusions 18, 18, 12, 0

'13raspberry mint mahajanga chocolate infusions
d1infusions 18, 33, 13, 0

'14colombo cacao cofee & almond infusions
d1infusions 18, 48, 14, 0

'15sulawesi cacao macadamia infusions
d1infusions 18, 63, 15, 0

LOCATE 23, 3
PRINT "Click on a chocolate to view its page in the";
LOCATE 24, 16
PRINT "Recipie Book.";

END SUB

SUB i1squares

box "                 Squares", 1, 1, 80, 25, 6, 5, 4
COLOR 3
LOCATE 4, 3
PRINT "Perfect for gifts and special occasion, these";
LOCATE 5, 5
PRINT "indulgent treats won't be soon forgotten.";

'1extra dark chocolate squares
d1squares 8, 3, 1, 0

'2dark chocolate squares w/cashews
d1squares 8, 18, 2, 0

'3dark chocolate squares w/almonds
d1squares 8, 33, 3, 0

'4hazelnut squares
d1squares 8, 48, 4, 0

'5cherry chocolate squares
d1squares 8, 63, 5, 0

'6cinnamon chocolate squares
d1squares 13, 3, 6, 0

'7caramel chocolate squares
d1squares 13, 18, 7, 0

'8milk chocolate hazelnut squares
d1squares 13, 33, 8, 0

'9coffee chocolate squares
d1squares 13, 48, 9, 0

'10mint chocolate squares
d1squares 13, 63, 10, 0

'11almond chocolate squares
d1squares 18, 3, 11, 0

'12cherry squares w/vanilla
d1squares 18, 18, 12, 0

'13cinnamon chocolate squares w/vanilla
d1squares 18, 33, 13, 0

'14milk chocolate squares
d1squares 18, 48, 14, 0

'15almond chocolate squres
d1squares 18, 63, 15, 0

LOCATE 23, 3
PRINT "Click on a chocolate to view its page in the";
LOCATE 24, 16
PRINT "Recipie Book.";


END SUB

SUB i1truffles

box "                 Truffles", 1, 1, 80, 25, 6, 5, 4
COLOR 3
LOCATE 4, 3
PRINT "Classic concoctions, created by blending the";
LOCATE 5, 3
PRINT "finest indrigents into creamy,divine balls of";
LOCATE 6, 13
PRINT "pure bliss.";

'1dark chocolate truffles
d1truffles 8, 3, 1, 0

'2milk chocolate truffles
d1truffles 8, 18, 2, 0

'3chocolate mint truffles
d1truffles 8, 33, 3, 0

'4almond chocolate truffles
d1truffles 8, 48, 4, 0

'5caramel chocolate truffles
d1truffles 8, 63, 5, 0

'6hazelnut milk truffles
d1truffles 13, 3, 6, 0

'7coconut lemon truffles
d1truffles 13, 18, 7, 0

'8coffee cinnamon truffles
d1truffles 13, 33, 8, 0

'9mahajanga cacao cinnamon cherry truffles
d1truffles 13, 48, 9, 0

'10sulawesi cacao lemon macadamia truffles
d1truffles 13, 63, 10, 0

'11colombo cacao coffee caramel truffles
d1truffles 18, 3, 11, 0

'12trinidadian cacao almond vanilla truffles
d1truffles 18, 18, 12, 0

'13blended cacao mint coffee truffles
d1truffles 18, 33, 13, 0

'14blended cacao raspberry coconut truffles
d1truffles 18, 48, 14, 0

'15blended cacao milk chocolate truffles
d1truffles 18, 63, 15, 0

LOCATE 23, 3
PRINT "Click on a chocolate to view its page in the";
LOCATE 24, 16
PRINT "Recipie Book.";

END SUB

SUB menu (x, n)
SELECT CASE x
CASE 1
IF (n = 1) THEN COLOR 3, 1 ELSE COLOR 3, 0
LOCATE 14, 35
PRINT "New game";
IF (n = 2) THEN COLOR 3, 1 ELSE COLOR 3, 0
LOCATE 16, 35
PRINT "Continue";
IF (n = 3) THEN COLOR 3, 1 ELSE COLOR 3, 0
LOCATE 18, 35
PRINT "Options";
IF (n = 4) THEN COLOR 3, 1 ELSE COLOR 3, 0
LOCATE 20, 35
PRINT "Credits";
IF (n = 5) THEN COLOR 3, 1 ELSE COLOR 3, 0
LOCATE 22, 35
PRINT "Quit";
CASE 2

IF (n = 1) THEN COLOR 17 ELSE COLOR 1
LOCATE 5, 30
PRINT "  ฎ Bars ฏ";
IF (n = 2) THEN COLOR 17 ELSE COLOR 1
LOCATE 7, 30
PRINT " ฎ Squares ฏ";
IF (n = 3) THEN COLOR 17 ELSE COLOR 1
LOCATE 9, 30
PRINT "ฎ Infusions ฏ";
IF (n = 4) THEN COLOR 17 ELSE COLOR 1
LOCATE 11, 30
PRINT "ฎ Truffles ฏ";
CASE 3

IF (n = 1) THEN COLOR 10, 11 ELSE COLOR 10, 0
LOCATE 8, 20
PRINT "Resume game";
IF (n = 2) THEN COLOR 10, 11 ELSE COLOR 10, 0
LOCATE 9, 20
PRINT "Main menu";

CASE 4
IF (n = 1) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 4, 20
PRINT "San Francisco";
IF (n = 2) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 5, 20
PRINT "New York";
IF (n = 3) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 6, 20
PRINT "Merida";
IF (n = 4) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 7, 20
PRINT "Trinidad";
IF (n = 5) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 8, 20
PRINT "Quito";
IF (n = 6) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 9, 20
PRINT "Rio de Janerio";
IF (n = 7) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 10, 20
PRINT "London";
IF (n = 8) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 11, 20
PRINT "Accra";
IF (n = 9) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 12, 20
PRINT "Istanbul";
IF (n = 10) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 13, 20
PRINT "Mahajanga";
IF (n = 11) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 14, 20
PRINT "Colombo";
IF (n = 12) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 15, 20
PRINT "Hong Kong";
IF (n = 13) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 16, 20
PRINT "Sulawesi";
IF (n = 14) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 17, 20
PRINT "Sydney";
IF (n = 15) THEN COLOR 10, 15 ELSE COLOR 10, 0
LOCATE 18, 20
PRINT "Rourkela";

CASE ELSE
END SELECT

END SUB

FUNCTION nospc$ (s$)
a$ = ""
FOR i = 1 TO LEN(s$)
b$ = MID$(s$, i, 1)
IF (b$ <> " ") THEN a$ = a$ + b$
NEXT
nospc$ = a$
END FUNCTION

FUNCTION pause
box "Pause", 10, 4, 50, 12, 4, 5, 6
menu 3, 1
k$ = ""
ps = 1
DO UNTIL (k$ = CHR$(esc) OR k$ = CHR$(enter))
k$ = ""
WHILE k$ = ""
k$ = INKEY$
WEND
SELECT CASE k$
CASE CHR$(0) + CHR$(up)
IF (ps = 1) THEN ps = 2 ELSE ps = 1
menu 3, ps
CASE CHR$(0) + CHR$(down)
IF (ps = 1) THEN ps = 2 ELSE ps = 1
menu 3, ps
CASE CHR$(esc)
EXIT DO
CASE CHR$(enter)
EXIT DO
CASE ELSE
END SELECT
LOOP
CLS
IF (k$ = CHR$(esc)) THEN
ps = 1
GOTO pausee
ELSEIF (k$ = CHR$(enter)) THEN
GOTO pausee
END IF
pausee:
pause = ps
END FUNCTION

FUNCTION r1bars$ (n, x)
'Sugar                  s
'cacao baeans           c
'milk solids            m
'almonds                a
'cashew nuts            n
'coffee beans           b
'trinidad lemons        t
'mint leaves            l
'quito cacao            q
'trinidad cacao         r
'mahajanga cacao        h
'colombo cacao          o


'2 after this recipie requires:
'3 after this You make -- per week at --
'ok


SELECT CASE n
CASE 1
        SELECT CASE x
        CASE 1
        r$ = "Basic Chocolate Bars"
        CASE 2
        r$ = "Simple chocolate bars that are simply delicious."
        CASE 3
        r$ = "sc"
        CASE ELSE
        END SELECT
CASE 2
        SELECT CASE x
        CASE 1
        r$ = "Dark Chocolate Bars"
        CASE 2
        r$ = "Higher cacao content makes these a bit fancier then Basic Chocolate Bars."
        CASE 3
        r$ = "scc"
        CASE ELSE
        END SELECT
CASE 3
        SELECT CASE x
        CASE 1
        r$ = "Milk Chocolate Bars"
        CASE 2
        r$ = "A rich and creamy blend of fine milk solids, cacao and sugar."
        CASE 3
        r$ = "scm"
        CASE ELSE
        END SELECT
CASE 4
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Bars w/Almonds"
        CASE 2
        r$ = "Fresh roasted almonds on a bed of the finest chocolate."
        CASE 3
        r$ = "sca"
        CASE ELSE
        END SELECT
CASE 5
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Bars w/Cashews"
        CASE 2
        r$ = "These scrumptious bars are made with roasted cashews, sugar and cacao beans."
        CASE 3
        r$ = "scn"
        CASE ELSE
        END SELECT
CASE 6
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Bars w/Coffee"
        CASE 2
        r$ = "Dense chocolate with real roasted coffee you will savor at the finish."
        CASE 3
        r$ = "scb"
        CASE ELSE
        END SELECT
CASE 7
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Bars w/Trinidad Lemons"
        CASE 2
        r$ = "Fresh lemons from Trinidad provide an intense burst of tangy, sweet flavour."
        CASE 3
        r$ = "sct"
        CASE ELSE
        END SELECT
CASE 8
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Bars w/Fresh Mint"
        CASE 2
        r$ = "Sharp mint gives these bars a wonderful after taste."
        CASE 3
        r$ = "scl"
        CASE ELSE
        END SELECT
CASE 9
        SELECT CASE x
        CASE 1
        r$ = "Exclusive Quito Cacao Chocolate Bars"
        CASE 2
        r$ = "The finest Quito cacao lends these bars a unique taste that lingers on the plate."
        CASE 3
        r$ = "sqq"
        CASE ELSE
        END SELECT
CASE 10
        SELECT CASE x
        CASE 1
        r$ = "Special Trinidad Cacao Chocolate Bars"
        CASE 2
        r$ = "Cacao beans imported from Trinidad give their distinct, incompareable temper and taste."
        CASE 3
        r$ = "srr"
        CASE ELSE
        END SELECT
CASE 11
        SELECT CASE x
        CASE 1
        r$ = "Mahajanga Cacao Chocolate Bars"
        CASE 2
        r$ = "Only the finest Mahajanga cacao harvested in Madagascar is used to craft these dark delectable bars."
        CASE 3
        r$ = "shh"
        CASE ELSE
        END SELECT
CASE 12
        SELECT CASE x
        CASE 1
        r$ = "Colombo Cacao Chocolate Bars"
        CASE 2
        r$ = "Sri Lankan cacao beans give these bars a wonderful aroma and remarkable finish."
        CASE 3
        r$ = "soo"
        CASE ELSE
        END SELECT
CASE 13
        SELECT CASE x
        CASE 1
        r$ = "Extra Dark Chocolate Bars"
        CASE 2
        r$ = "These Extra Dark Chocolate Bars are made with 75% pure cacao."
        CASE 3
        r$ = "sccc"
        CASE ELSE
        END SELECT
CASE 14
        SELECT CASE x
        CASE 1
        r$ = "White Chocolate Bars"
        CASE 2
        r$ = "Blending milk solids, sugar and cacao, these untraditional bars have become popular the world over."
        CASE 3
        r$ = "sscm"
        CASE ELSE
        END SELECT
CASE 15
        SELECT CASE x
        CASE 1
        r$ = "Dark Bars w/Almonds"
        CASE 2
        r$ = "The snap of these sleek, extra dark and nutty bars is beyond compare."
        CASE 3
        r$ = "scca"
        CASE ELSE
        END SELECT
CASE ELSE
END SELECT
r1bars$ = r$
END FUNCTION

FUNCTION r1infusions$ (n, x)
'Sugar                  s
'cacao baeans           c
'milk solids            m
'almonds                a
'cashew nuts            n
'coffee beans           b
'trinidad lemons        t
'mint leaves            l
'quito cacao            q
'trinidad cacao         r
'mahajanga cacao        h
'colombo cacao          o
'hazelnuts              z
'bing cherris           i
'cinnamon               d
'caramel                e
'vanilla                v
'coconut shavings       u
'raspberries            p
'sulawesi cacao         w
'macadamia nuts         x


'2 after this recipie requires:
'3 after this You make -- per week at --
'ok


SELECT CASE n
CASE 1
        SELECT CASE x
        CASE 1
        r$ = "Milk Chocolate Infusions"
        CASE 2
        r$ = "Velvety milk chocolate filling in a pure milk chocolate shell."
        CASE 3
        r$ = "ssmmc"
        CASE ELSE
        END SELECT
CASE 2
        SELECT CASE x
        CASE 1
        r$ = "Coconut Chocolate Infusions"
        CASE 2
        r$ = "Rich, smooth chocolate infused with fresh coconut ganache."
        CASE 3
        r$ = "sccmu"
        CASE ELSE
        END SELECT
CASE 3
        SELECT CASE x
        CASE 1
        r$ = "Cherry Chocolate Infusions"
        CASE 2
        r$ = "Rich chocolate surrounds a fangy cherry ganache."
        CASE 3
        r$ = "sccmi"
        CASE ELSE
        END SELECT
CASE 4
        SELECT CASE x
        CASE 1
        r$ = "Coffee Chocolate Infusions"
        CASE 2
        r$ = "Strong, dark coffee liqour enrobed in subtle, sublime chocolate."
        CASE 3
        r$ = "sccmb"
        CASE ELSE
        END SELECT
CASE 5
        SELECT CASE x
        CASE 1
        r$ = "Raspberry Chocolate Infusions"
        CASE 2
        r$ = "Using only the freshest raspberries are the secret to these wonderful infusions."
        CASE 3
        r$ = "sccmp"
        CASE ELSE
        END SELECT
CASE 6
        SELECT CASE x
        CASE 1
        r$ = "Vanilla Almond Infusions"
        CASE 2
        r$ = "Ganache of roasted almonds accented by the finest vanilla, surrounded by the finest chocolate compromise these detecttable infusions."
        CASE 3
        r$ = "sccav"
        CASE ELSE
        END SELECT
CASE 7
        SELECT CASE x
        CASE 1
        r$ = "Lemon Chocolate Infusions"
        CASE 2
        r$ = "The finest chocolate encases a sweet, fresh lemon ganache."
        CASE 3
        r$ = "sccmt"
        CASE ELSE
        END SELECT
CASE 8
        SELECT CASE x
        CASE 1
        r$ = "Cinnamon Chocolate Infusions"
        CASE 2
        r$ = "Fresh cinnamon lends these delightful infusions a long,slow finish and lasting aftertaste."
        CASE 3
        r$ = "sccmd"
        CASE ELSE
        END SELECT
CASE 9
        SELECT CASE x
        CASE 1
        r$ = "Mint Chocolate Infusions"
        CASE 2
        r$ = "Sharp mint gives these infusions a wonderful extra zing at the finish."
        CASE 3
        r$ = "sccml"
        CASE ELSE
        END SELECT
CASE 10
        SELECT CASE x
        CASE 1
        r$ = "Mahajanga Cherry Chocolate Infusions"
        CASE 2
        r$ = "Exclusive Mahajanga chocolate surrounds a tangy cherry-infused ganache."
        CASE 3
        r$ = "ssihh"
        CASE ELSE
        END SELECT
CASE 11
        SELECT CASE x
        CASE 1
        r$ = "Quito Chocolate Coffee Infusions"
        CASE 2
        r$ = "Coffee liquor wrapped in a chocolate shell made from exclusive Quito cacao."
        CASE 3
        r$ = "ssbqq"
        CASE ELSE
        END SELECT
CASE 12
        SELECT CASE x
        CASE 1
        r$ = "Raspberry Colombo Cacao Chocolate Infusions"
        CASE 2
        r$ = "Dark chocolate made with a varietal cacao from Sri Lanka encases a tart, fresh raspberry ganache."
        CASE 3
        r$ = "sspoo"
        CASE ELSE
        END SELECT
CASE 13
        SELECT CASE x
        CASE 1
        r$ = "Raspberry Mint Mahajanga Chocolate Infusions"
        CASE 2
        r$ = "Exclusive Mahajanga cacao chocolate encases a raspberry ganache filling with a breath of fresh mint."
        CASE 3
        r$ = "splhh"
        CASE ELSE
        END SELECT
CASE 14
        SELECT CASE x
        CASE 1
        r$ = "Colombo Cacao Coffee & Almond Infusions"
        CASE 2
        r$ = "Coffee and almond-infused ganache, surrounded by a chocolate shell from the finest Sri Lankan cacao."
        CASE 3
        r$ = "sooab"
        CASE ELSE
        END SELECT
CASE 15
        SELECT CASE x
        CASE 1
        r$ = "Sulawesi Cacao Macadamia Infusions"
        CASE 2
        r$ = "The finest chocolate shell fashioned exclusively from Sulawesi cacao around a crunchy macadamia nut ganache filling."
        CASE 3
        r$ = "svwwx"
        CASE ELSE
        END SELECT
CASE ELSE
END SELECT
r1infusions$ = r$

END FUNCTION

FUNCTION r1item$ (n, x, t)
SELECT CASE t
CASE 1
z$ = r1bars$(n, x)
CASE 2
z$ = r1squares$(n, x)
CASE 3
z$ = r1infusions$(n, x)
CASE 4
z$ = r1truffles$(n, x)
CASE ELSE
END SELECT
r1item$ = z$
END FUNCTION

FUNCTION r1squares$ (n, x)
'Sugar                  s
'cacao baeans           c
'milk solids            m
'almonds                a
'cashew nuts            n
'coffee beans           b
'trinidad lemons        t
'mint leaves            l
'quito cacao            q
'trinidad cacao         r
'mahajanga cacao        h
'colombo cacao          o
'hazelnuts              z
'bing cherris           i
'cinnamon               d
'caramel                e
'vanilla                v

'2 after this recipie requires:
'3 after this You make -- per week at --
'ok


SELECT CASE n
CASE 1
        SELECT CASE x
        CASE 1
        r$ = "Extra Dark Chocolate Squares"
        CASE 2
        r$ = "The finest extra dark chocolate shaped into squares."
        CASE 3
        r$ = "sccc"
        CASE ELSE
        END SELECT
CASE 2
        SELECT CASE x
        CASE 1
        r$ = "Dark Chocolate Squares w/Cashews"
        CASE 2
        r$ = "Fresh cashews, encased in rich, dark chocolate, shaped into squares."
        CASE 3
        r$ = "sccn"
        CASE ELSE
        END SELECT
CASE 3
        SELECT CASE x
        CASE 1
        r$ = "Dark Chocolate Squares w/Almonds"
        CASE 2
        r$ = "These squares are made with fresh almonds, and exhibit a nutty, earthy finish."
        CASE 3
        r$ = "scca"
        CASE ELSE
        END SELECT
CASE 4
        SELECT CASE x
        CASE 1
        r$ = "Hazelnut Squares"
        CASE 2
        r$ = "A whole, toasted hazelnut awaits in these squares of the finest dark chocolate."
        CASE 3
        r$ = "sccz"
        CASE ELSE
        END SELECT
CASE 5
        SELECT CASE x
        CASE 1
        r$ = "Cherry Chocolate Squares"
        CASE 2
        r$ = "One luscious, sweet cherry surrounded by the finest chocolate."
        CASE 3
        r$ = "scci"
        CASE ELSE
        END SELECT
CASE 6
        SELECT CASE x
        CASE 1
        r$ = "Cinnamon Chocolate Squares"
        CASE 2
        r$ = "Dark chocolate squares with a sweet cinnamon flare that evokes warmth and good cheer."
        CASE 3
        r$ = "sccd"
        CASE ELSE
        END SELECT
CASE 7
        SELECT CASE x
        CASE 1
        r$ = "Caramel Cocolate Squares"
        CASE 2
        r$ = "Light and buttery caramel centers enrobed in pure, dark chocolate."
        CASE 3
        r$ = "scce"
        CASE ELSE
        END SELECT
CASE 8
        SELECT CASE x
        CASE 1
        r$ = "Milk Chocolate Hazelnut Squares"
        CASE 2
        r$ = "Roasted hazelnuts in creamy, smooth milk chocolate, fashioned into squares."
        CASE 3
        r$ = "scmz"
        CASE ELSE
        END SELECT
CASE 9
        SELECT CASE x
        CASE 1
        r$ = "Coffee Chocolate Squares"
        CASE 2
        r$ = "Rich, coffee-flavoured chocolate squares are perfect for entertaining."
        CASE 3
        r$ = "sccb"
        CASE ELSE
        END SELECT
CASE 10
        SELECT CASE x
        CASE 1
        r$ = "Mint Chocolate Squares"
        CASE 2
        r$ = "Fragrant fresh mint is the key to these delightful after-dinner chocolate squares."
        CASE 3
        r$ = "sccl"
        CASE ELSE
        END SELECT
CASE 11
        SELECT CASE x
        CASE 1
        r$ = "Almond Chocolate Squares"
        CASE 2
        r$ = "Dark chocolate enrobes a crisp nougat of roasted almonds."
        CASE 3
        r$ = "scaa"
        CASE ELSE
        END SELECT
CASE 12
        SELECT CASE x
        CASE 1
        r$ = "Cherry Squares w/Vanilla"
        CASE 2
        r$ = "A hint of vanilla kisses a fresh cherry in these luscious chocolate squares."
        CASE 3
        r$ = "sciv"
        CASE ELSE
        END SELECT
CASE 13
        SELECT CASE x
        CASE 1
        r$ = "Cinnamon Chocolate Squares w/Vanilla"
        CASE 2
        r$ = "A wisp of Madagascar vanilla, combined with cinnamon, give these squares a uniquely satisfying flavour and hue."
        CASE 3
        r$ = "scdv"
        CASE ELSE
        END SELECT
CASE 14
        SELECT CASE x
        CASE 1
        r$ = "Milk Chocolate Squares w/Vanilla"
        CASE 2
        r$ = "Creamy milk chocolate squares with a surprising whiff of vanilla."
        CASE 3
        r$ = "scmv"
        CASE ELSE
        END SELECT
CASE 15
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Hazelnut Squares w/Vanilla"
        CASE 2
        r$ = "Rich chocolate-encased fresh roasted hazelnuts, with a hint of vanilla make these squares hard to resist."
        CASE 3
        r$ = "scvz"
        CASE ELSE
        END SELECT
CASE ELSE
END SELECT
r1squares$ = r$

END FUNCTION

FUNCTION r1truffles$ (n, x)
'Sugar                  s
'cacao baeans           c
'milk soli              m
'almonds                a
'cashew nuts            n
'coffee beans           b
'trinidad lemons        t
'mint leaves            l
'quito cacao            q
'trinidad cacao         r
'mahajanga cacao        h
'colombo cacao          o
'hazelnuts              z
'bing cherris           i
'cinnamon               d
'caramel                e
'vanilla                v
'coconut shavings       u
'raspberries            p
'sulawesi cacao         w
'macadamia nuts         x
'truffle powder         f

'2 after this recipie requires:
'3 after this You make -- per week at --
'ok


SELECT CASE n
CASE 1
        SELECT CASE x
        CASE 1
        r$ = "Dark Chocolate Truffles"
        CASE 2
        r$ = "Rich, dark chocolate filling in a dark chocolate case, dusted with truffle powder."
        CASE 3
        r$ = "sscccf"
        CASE ELSE
        END SELECT
CASE 2
        SELECT CASE x
        CASE 1
        r$ = "Milk Chocolate Truffles"
        CASE 2
        r$ = "A creamy milk chocolate filling, a milk chocolate shell, dusted with truffle powder: simple pleasure."
        CASE 3
        r$ = "ssccmf"
        CASE ELSE
        END SELECT
CASE 3
        SELECT CASE x
        CASE 1
        r$ = "Chocolate Mint Truffles"
        CASE 2
        r$ = "Fresh mint cream filling in a subtle chocolate shell, dusted with truffle powder."
        CASE 3
        r$ = "sscclf"
        CASE ELSE
        END SELECT
CASE 4
        SELECT CASE x
        CASE 1
        r$ = "Almond Chocolate Truffles"
        CASE 2
        r$ = "Roasted almond filling in a dark chocolate case, dusted with truffle powder."
        CASE 3
        r$ = "ssccaf"
        CASE ELSE
        END SELECT
CASE 5
        SELECT CASE x
        CASE 1
        r$ = "Caramel Chocolate Truffles"
        CASE 2
        r$ = "Silky smooth caramel filling in a chocolate case, dusted with truffle powder."
        CASE 3
        r$ = "sccfee"
        CASE ELSE
        END SELECT
CASE 6
        SELECT CASE x
        CASE 1
        r$ = "Hazelnut Milk Truffles"
        CASE 2
        r$ = "Crunchy, roasted whole hazelnuts enrobed in creamy milk chocolate, dusted with truffle powder."
        CASE 3
        r$ = "sccmfz"
        CASE ELSE
        END SELECT
CASE 7
        SELECT CASE x
        CASE 1
        r$ = "Coconut Lemon Truffles"
        CASE 2
        r$ = "A delightful paste bursting with hints of coconut and lemon, inside a delicious chocolate shell."
        CASE 3
        r$ = "sccfut"
        CASE ELSE
        END SELECT
CASE 8
        SELECT CASE x
        CASE 1
        r$ = "Coffee Cinnamon Truffles"
        CASE 2
        r$ = "Truffles filled with a wonderful nougat swirling with coffee and cinnamon, perfect to warm any heart."
        CASE 3
        r$ = "sccfbd"
        CASE ELSE
        END SELECT
CASE 9
        SELECT CASE x
        CASE 1
        r$ = "Mahajanga Cacao Cinnamon Cherry Truffles"
        CASE 2
        r$ = "Mahajanga cacao is the secret to these scrumptious truffles with a spicy cinnamon and sweet cherry-flavoured filling."
        CASE 3
        r$ = "sdfihh"
        CASE ELSE
        END SELECT
CASE 10
        SELECT CASE x
        CASE 1
        r$ = "Sulawesi Cacao Lemon Macadamia Truffles"
        CASE 2
        r$ = "Cacao from Sulawesi provides a unique temper to these truffles, filled with lemon macadamia liquor, dusted with truffle powder."
        CASE 3
        r$ = "sftwwx"
        CASE ELSE
        END SELECT
CASE 11
        SELECT CASE x
        CASE 1
        r$ = "Colombo Cacao Coffe Caramel Truffles"
        CASE 2
        r$ = "Distinctive Colombo cacao lends these truffles an inimitable finish. Their filling is a blend of coffee and caramel."
        CASE 3
        r$ = "sbfooe"
        CASE ELSE
        END SELECT
CASE 12
        SELECT CASE x
        CASE 1
        r$ = "Trinidadian Cacao Almond Vanilla Truffles"
        CASE 2
        r$ = "Trinidadian cacao and a cream filling made from roasted almonds with a touch of vanilla make these truffles one of a kind."
        CASE 3
        r$ = "svfarr"
        CASE ELSE
        END SELECT
CASE 13
        SELECT CASE x
        CASE 1
        r$ = "Blended Cacao Mint Coffee Truffles"
        CASE 2
        r$ = "A blend of cacao beans from Trinidad and Colombo, combined with a creamy mint coffee inside makes these truffles famous."
        CASE 3
        r$ = "sforbl"
        CASE ELSE
        END SELECT
CASE 14
        SELECT CASE x
        CASE 1
        r$ = "Blended Cacao Raspberry Coconut Truffles"
        CASE 2
        r$ = "Fresh raspberry, coconut and a blend of cacaos from Sulawesi and Mahajanga make these truffles impossible to forget."
        CASE 3
        r$ = "sfpwhu"
        CASE ELSE
        END SELECT
CASE 15
        SELECT CASE x
        CASE 1
        r$ = "Blended Cacao Milk Chocolate Truffles"
        CASE 2
        r$ = "The world's finest milk chocolate is made by combining stock cacaos from Colombo, Sulawesi and Quito."
        CASE 3
        r$ = "smfoqw"
        CASE ELSE
        END SELECT
CASE ELSE
END SELECT
r1truffles$ = r$

END FUNCTION

FUNCTION r1world$ (n, x)
SHARED port, portstate()
SELECT CASE n
CASE 1
        SELECT CASE x
        CASE 1
        r$ = "San Francisco"
        CASE 2
        r$ = "North America"
        CASE ELSE
        END SELECT
CASE 2
        SELECT CASE x
        CASE 1
        r$ = "New York"
        CASE 2
        r$ = "North America"
        CASE ELSE
        END SELECT
CASE 3
        SELECT CASE x
        CASE 1
        r$ = "Merida"
        CASE 2
        r$ = "North America"
        CASE ELSE
        END SELECT
CASE 4
        SELECT CASE x
        CASE 1
        r$ = "Trinidad"
        CASE 2
        r$ = "South America"
        CASE ELSE
        END SELECT
CASE 5
        SELECT CASE x
        CASE 1
        r$ = "Quito"
        CASE 2
        r$ = "South America"
        CASE ELSE
        END SELECT
CASE 6
        SELECT CASE x
        CASE 1
        r$ = "Rio de Janerio"
        CASE 2
        r$ = "South America"
        CASE ELSE
        END SELECT
CASE 7
        SELECT CASE x
        CASE 1
        r$ = "London"
        CASE 2
        r$ = "Europe"
        CASE ELSE
        END SELECT
CASE 8
        SELECT CASE x
        CASE 1
        r$ = "Accra"
        CASE 2
        r$ = "Africa"
        CASE ELSE
        END SELECT
CASE 9
        SELECT CASE x
        CASE 1
        r$ = "Istanbul"
        CASE 2
        r$ = "Asia"
        CASE ELSE
        END SELECT
CASE 10
        SELECT CASE x
        CASE 1
        r$ = "Mahahjanga"
        CASE 2
        r$ = "Africa"
        CASE ELSE
        END SELECT
CASE 11
        SELECT CASE x
        CASE 1
        r$ = "Colombo"
        CASE 2
        r$ = "Asia"
        CASE ELSE
        END SELECT
CASE 12
        SELECT CASE x
        CASE 1
        r$ = "Hong Kong"
        CASE 2
        r$ = "Asia"
        CASE ELSE
        END SELECT
CASE 13
        SELECT CASE x
        CASE 1
        r$ = "Sulawesi"
        CASE 2
        r$ = "Asia"
        CASE ELSE
        END SELECT
CASE 14
        SELECT CASE x
        CASE 1
        r$ = "Sydney"
        CASE 2
        r$ = "Australia"
        CASE ELSE
        END SELECT
CASE 15
        SELECT CASE x
        CASE 1
        r$ = "Rourkela"
        CASE 2
        r$ = "Asia"
        CASE ELSE
        END SELECT
CASE ELSE
END SELECT

IF (x = 3) THEN
IF (portstate(n) = 0) THEN r$ = "Port is closed."
IF (port = n) THEN r$ = "You are here." ELSE r$ = "A " + nospc$(STR$(cost(n, 1))) + "-week journey."
ELSEIF (x = 4) THEN
IF (port <> n) THEN r$ = "This trip will cost $" + nospc$(STR$(cost(n, 2)))
END IF
r1world$ = r$
END FUNCTION

SUB requirement (s$)
'Sugar                  s
'cacao baeans           c
'milk soli              m
'almonds                a
'cashew nuts            n
'coffee beans           b
'trinidad lemons        t
'mint leaves            l
'quito cacao            q
'trinidad cacao         r
'mahajanga cacao        h
'colombo cacao          o
'hazelnuts              z
'bing cherris           i
'cinnamon               d
'caramel                e
'vanilla                v
'coconut shavings       u
'raspberries            p
'sulawesi cacao         w
'macadamia nuts         x
'truffle powder         f
ln = CSRLIN
cl = POS(ln)
COLOR 9
FOR i = 1 TO LEN(s$)
LOCATE ln, cl
k$ = MID$(s$, i, 1)

SELECT CASE k$
CASE "s"
z$ = "Sugar"
CASE "c"
z$ = "Cacao beans"
CASE "m"
z$ = "Milk solids"
CASE "a"
z$ = "Almonds"
CASE "n"
z$ = "Cashew nuts"
CASE "b"
z$ = "Coffee beans"
CASE "t"
z$ = "Trinidad lemons"
CASE "l"
z$ = "Mint leaves"
CASE "q"
z$ = "Quito cacao"
CASE "r"
z$ = "Trinidad cacao"
CASE "h"
z$ = "Mahajanga cacao"
CASE "o"
z$ = "Colombo cacao"
CASE "z"
z$ = "Hazelnuts"
CASE "i"
z$ = "Bing cherries"
CASE "d"
z$ = "Cinnamon"
CASE "e"
z$ = "Caramel"
CASE "v"
z$ = "Vanilla"
CASE "u"
z$ = "Coconut shavings"
CASE "p"
z$ = "Raspberries"
CASE "w"
z$ = "Sulawesi cacao"
CASE "x"
z$ = "Macadamia nuts"
CASE "f"
z$ = "Truffle powder"
CASE ELSE
END SELECT
PRINT "๘  "; z$;
ln = ln + 1
NEXT

END SUB

FUNCTION win1$ (n)
win13: CLS

COLOR 1
LOCATE 1, 1
PRINT "ษอออออออออออป";
LOCATE 2, 1
PRINT "บ           บ";
LOCATE 3, 1
PRINT "ศอออออออออออผ";
COLOR 2
LOCATE 1, 16
PRINT "ษ";
FOR i = 17 TO 78
PRINT "อ";
NEXT
PRINT "ป";
LOCATE 2, 16
PRINT "บ"; SPC(62); "บ";
LOCATE 3, 16
PRINT "ศ";
FOR i = 17 TO 78
PRINT "อ";
NEXT
PRINT "ผ";
FOR i = 4 TO 6
LOCATE i, 1
PRINT "Û";
LOCATE i, 80
PRINT "Û";
NEXT
LOCATE 25, 1
PRINT "Û";
LOCATE 25, 80
PRINT "Û";

COLOR 4
LOCATE 2, 3

SELECT CASE n
CASE 1
PRINT "Bars";
COLOR 3
LOCATE 4, 3
PRINT "Luscious, luxurious bars made with the finest";
LOCATE 5, 5
PRINT "chocolates accented in a wide variety of";
LOCATE 6, 17
PRINT "flavours.";

CASE 2
PRINT "Squares";
COLOR 3
LOCATE 4, 3
PRINT "Perfect for gifts and special occasion, these";
LOCATE 5, 5
PRINT "indulgent treats won't be soon forgotten.";

CASE 3
PRINT "Infusions";
COLOR 3
LOCATE 4, 3
PRINT "Creamy European-style Ganache centers in a";
LOCATE 5, 5
PRINT "range of exotic spices, fruits and nuts that";
LOCATE 6, 13
PRINT "simply melt in the mouth.";

CASE 4
PRINT "Truffles";
COLOR 3
LOCATE 4, 3
PRINT "Classic concoctions, created by blending the";
LOCATE 5, 3
PRINT "finest indrigents into creamy,divine balls of";
LOCATE 6, 13
PRINT "pure bliss.";

CASE ELSE
END SELECT

d1item 8, 3, 1, 0, n
d1item 8, 18, 2, 0, n
d1item 8, 33, 3, 0, n
d1item 8, 48, 4, 0, n
d1item 8, 63, 5, 0, n
d1item 13, 3, 6, 0, n
d1item 13, 18, 7, 0, n
d1item 13, 33, 8, 0, n
d1item 13, 48, 9, 0, n
d1item 13, 63, 10, 0, n
d1item 18, 3, 11, 0, n
d1item 18, 18, 12, 0, n
d1item 18, 33, 13, 0, n
d1item 18, 48, 14, 0, n
d1item 18, 63, 15, 0, n

COLOR 5
LOCATE 23, 3
PRINT "Select a chocolate to view its page in the";
LOCATE 24, 16
PRINT "Recipie Book.";

COLOR 6
LOCATE 2, 18
PRINT r1item$(1, 1, n);
d1item 8, 3, 1, 1, n

ps = 1
k$ = ""
DO UNTIL (k$ = "`" OR k$ = CHR$(esc) OR k$ = CHR$(enter))
k$ = ""
WHILE k$ = ""
k$ = INKEY$
WEND

SELECT CASE k$

CASE CHR$(0) + CHR$(left)
GOSUB win11
ps = ps - 1
IF (ps < 1) THEN ps = 15
GOSUB win12
CASE CHR$(0) + CHR$(right)
GOSUB win11
ps = ps + 1
IF (ps > 15) THEN ps = 1
GOSUB win12
CASE CHR$(0) + CHR$(up)
GOSUB win11
ps = ps - 5
IF (ps < 1) THEN ps = 15 - 1 + ps
GOSUB win12
CASE CHR$(0) + CHR$(down)
GOSUB win11
ps = (((ps - 1) + 5) MOD 15) + 1
GOSUB win12
CASE CHR$(esc)
EXIT DO
CASE "`"
EXIT DO
CASE CHR$(enter)
EXIT DO
CASE ELSE
END SELECT
LOOP
IF (k$ = CHR$(esc)) THEN GOTO win1e
IF (k$ = "`") THEN GOTO win1e
IF (k$ = CHR$(enter)) THEN
k$ = win2$(n, ps)
IF (k$ = CHR$(esc)) THEN GOTO win1e
IF (k$ = "`") THEN GOTO win13
IF (k$ = CHR$(enter)) THEN GOTO win1e
END IF
GOTO win1e

win11:
l = (INT((ps / 5) - .1) * 5) + 8
c = ((ps - 1) MOD 5) * 15 + 3
d1item l, c, ps, 0, n
RETURN
win12:
l = (INT((ps / 5) - .1) * 5) + 8
c = ((ps - 1) MOD 5) * 15 + 3
d1item l, c, ps, 1, n
COLOR 6
LOCATE 2, 18
PRINT r1item$(ps, 1, n);
RETURN

win1e:
win1$ = k$
END FUNCTION

FUNCTION win2$ (na, xa)
CLS
COLOR 1
LOCATE 1, 1
PRINT "ษอออออออออออป";
LOCATE 2, 1
PRINT "บ           บ";
LOCATE 3, 1
PRINT "ศอออออออออออผ";
COLOR 2
LOCATE 1, 16
PRINT "ษ";
FOR i = 17 TO 78
PRINT "อ";
NEXT
PRINT "ป";
LOCATE 2, 16
PRINT "บ"; SPC(62); "บ";
LOCATE 3, 16
PRINT "ศ";
FOR i = 17 TO 78
PRINT "อ";
NEXT
PRINT "ผ";
LOCATE 2, 3
COLOR 3
SELECT CASE na
CASE 1
PRINT "Bars";
CASE 2
PRINT "Squares";
CASE 3
PRINT "Infusions";
CASE 4
PRINT "Truffles";
CASE ELSE
END SELECT
COLOR 6
LOCATE 2, 18
PRINT r1item$(xa, 1, na);


d1item 4, 5, xa, 0, na
COLOR 7
LOCATE 6, 20
accomodate r1item$(xa, 2, na), 20, 70
LOCATE 10, 20
COLOR 9
PRINT "This recipie requires:";
LOCATE 12, 25
requirement r1item$(xa, 3, na)

DO UNTIL (k$ = CHR$(esc) OR k$ = "`" OR k$ = CHR$(enter))
k$ = ""
WHILE k$ = ""
k$ = INKEY$
WEND
SELECT CASE k$
CASE CHR$(esc)
EXIT DO
CASE "'"
EXIT DO
CASE CHR$(enter)
EXIT DO
CASE ELSE
END SELECT
LOOP
IF (k$ = CHR$(esc)) THEN GOTO win2e
IF (k$ = "`") THEN GOTO win2e
IF (k$ = CHR$(enter)) THEN k$ = nospc$(STR$(na) + STR$(xa))

win2e:
win2$ = k$
END FUNCTION

FUNCTION world
COLOR 1
LOCATE 1, 1
PRINT "ษอออออออออออป";
LOCATE 2, 1
PRINT "บ           บ";
LOCATE 3, 1
PRINT "ศอออออออออออผ";
COLOR 2
LOCATE 2, 3
PRINT "World";
COLOR 3
FOR i = 20 TO 25
LOCATE i, 1
PRINT "Û"; SPC(78); "Û";
NEXT

ps = 1
menu 1, 4
DO UNTIL (k$ = CHR$(enter) OR k$ = "`")
k$ = ""
WHILE (k$ = "")
k$ = INKEY$
WEND

SELECT CASE k$
CASE CHR$(0) + CHR$(up)
ps = ps - 1
IF (ps < 1) THEN ps = 15
menu ps, 4
CASE CHR$(0) + CHR$(down)
ps = ps + 1
IF (ps > 15) THEN ps = 1
menu ps, 4
CASE "`"
EXIT DO
CASE CHR$(enter)
EXIT DO
CASE ELSE
END SELECT
LOOP

IF (k$ = CHR$(enter)) THEN x = ps
CLS
world = x
END FUNCTION

