'ฝ ผ ป บ น ธ ท ถ ต ด ณ พ ฟ ภ ม ย ร ฤ ล ฦ ว ศ ษ ส ห ฬ อ ฮ ฯ ะ ั า ำ ิ ี ึ ื

COMMON SHARED file$, fpos AS LONG, style%, mode$, mode%, colour%, freq%, rate%, level%, dblg%, time#, ef%, lmode%, xpos%, bar%, process%(), work%(), sign%(), fmode AS LONG, ftime#, slct%

DECLARE FUNCTION inpt$ (s$, l)
DECLARE FUNCTION nospc$ (s$)
DECLARE SUB wrtf (f%)
DECLARE SUB wrtt (t#)
DECLARE SUB click1 ()
DECLARE SUB status (s$)
DECLARE SUB vstyle1 (c%, op%)
DECLARE SUB vstyle ()
DECLARE SUB modes1 (c%, op%)
DECLARE SUB modes ()
DECLARE SUB tools1 (c%)
DECLARE SUB tools ()
DECLARE SUB flname1 (c%)
DECLARE SUB flname ()

DECLARE SUB v.line1 ()
DECLARE SUB v.line2 ()
DECLARE SUB v.spll1 ()
DECLARE SUB v.spll2 ()
DECLARE SUB v.bars1 ()
DECLARE SUB v.bars2 ()
DECLARE SUB v.clmn1 ()
DECLARE SUB v.clmn2 ()
DECLARE SUB v.crcl ()
DECLARE SUB v.rect ()
DECLARE SUB v.show ()
DECLARE SUB v.chng ()

DECLARE SUB s.pkup ()
DECLARE SUB s.pkdn ()
DECLARE SUB s.pkupen ()
DECLARE SUB s.pkdnen ()
DECLARE SUB s.cvup ()
DECLARE SUB s.cvdn ()
DECLARE SUB s.cvupen ()
DECLARE SUB s.cvdnen ()
DECLARE SUB s.hlup ()
DECLARE SUB s.hldn ()
DECLARE SUB s.hlupen ()
DECLARE SUB s.hldnen ()
DECLARE SUB s.zinga ()
DECLARE SUB s.zingb ()
DECLARE SUB s.zingc ()
DECLARE SUB s.zingd ()
DECLARE SUB s.zinge ()
DECLARE SUB s.zingf ()
DECLARE SUB s.play ()
DECLARE SUB s.assgn (k$)
DECLARE SUB s.uassgn ()
DECLARE SUB s.playall (k$)
DECLARE SUB sd.play (k$)
DECLARE SUB sd.stpmd ()
DECLARE SUB box1 ()


'============================Declaring Keys==================================
CONST backspc = 8, enter = 13, htab = 9
CONST left = 75, right = 77, up = 72, down = 80
CONST uplt = 71, uprt = 73, dnlt = 79, dnrt = 81
CONST insert = 82, home = 73, pageup = 71, del = 83, endk = 81, pagedn = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134
'-----------------------------Keys declared---------------------------------

xpos% = 165
bar% = 1


'===================================Reading modes
OPEN "R", #1, "Data\Mode\Mode.cab"
FIELD #1, 12 AS zz1$, 16 AS zz2$
lmode% = LOF(1)
CLOSE #1
'===========================================

CLS
SCREEN 12

'================================Logs
OPEN "B", #2, "logs.log"

IF LOF(2) < 2 THEN
a$ = "aaaaaaaa"
PUT #2, 1, a$
ELSE
GOTO main
END IF

COLOR 14
PRINT "ษออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ  Your CPU sound will now be analysed.  บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออผ"


LINE (0, 200)-(10, 300), 10, BF
LINE (630, 200)-(640, 300), 10, BF
VIEW SCREEN (11, 200)-(629, 300)
FOR i = 1 TO 15
LINE (11, 250)-(11, 250), i
FOR j = 12 TO 629 STEP 2
a = INT(20000 - 37) * RND + 37
ht = 300 - (a / 200)
LINE -(j, ht), i
SOUND a, .25
NEXT
CLS
NEXT
CLS
VIEW
LOCATE 1, 1
COLOR 14
PRINT "ษออออออออออออออออออออออออออออออออออออออออป"
PRINT "บ  Your CPU sound has been analysed.     บ"
PRINT "ศออออออออออออออออออออออออออออออออออออออออผ"
k$ = INPUT$(1)
CLS
'================================================

main: CLOSE #1, #2
COLOR 14
LOCATE 1, 1
PRINT "ษอออออออออออออออป"
PRINT "บ SOUND CREATOR บ"
PRINT "ศอออออออออออออออผ"
status ""
vstyle1 12, 1
modes1 11, 0
tools1 10
flname1 9
LINE (136, 54)-(156, 256), 9, B
LINE (480, 370)-(580, 460), 9, B
LINE (581, 370)-(631, 460), 9, B
LINE (164, 49)-(636, 311), 6, B
LINE (163, 48)-(637, 312), 6, B
                                     '(165,50)-(630,310)
flname
IF LOF(5) > 2 THEN GOTO load
LINE (165, 50)-(630, 310), 0, BF
fpos = 1
style% = 1
mode$ = ""
mode% = 1
colour% = 1
freq% = 37
rate% = 0
level% = 0
dblg% = 0
time# = 0
ef% = 37
tools
status "File started."


crt:
DO UNTIL k$ = CHR$(27)
k$ = INKEY$
IF freq% < 37 THEN freq% = 21000

IF k$ <> "" THEN

SELECT CASE k$

CASE CHR$(enter)
wrtf level%
SOUND level%, time#
box1
v.show

CASE "\"
wrtf 21000
SOUND 21000, time#
box1
v.show

CASE CHR$(0) + CHR$(up)
level% = level% + rate%
IF level% > 31999 THEN level% = 31999
status "Level -> " + STR$(level%)

CASE CHR$(0) + CHR$(down)
level% = level% - rate%
IF level% < 37 THEN level% = 37
status "Level -> " + STR$(level%)

CASE CHR$(0) + CHR$(kf1)
slct% = 0

CASE CHR$(0) + CHR$(kf2)
slct% = 1

CASE CHR$(0) + CHR$(kf3)
vstyle

CASE CHR$(0) + CHR$(kf4)
modes

CASE CHR$(0) + CHR$(kf5)
tools

CASE "*"
colour% = colour% + 1
IF colour% > 15 THEN colour% = 1
CASE CHR$(0) + CHR$(kf6)
flname

IF LOF(5) > 2 THEN GOTO load
LINE (165, 50)-(630, 310), 0, BF
status "File started."

CASE "+"
rate% = rate% + 2
CASE "-"
rate% = rate% - 2
IF rate% < 1 THEN rate% = 1

CASE ELSE
s.playall (k$)
 
END SELECT
END IF


LOOP





load:
errs: RESUME NEXT

SUB box1
SHARED freq%, colour%
LINE (137, 55)-(155, 255), 0, BF
ht = 255 - ((freq% - 37) / 32730) * 200
LINE (137, ht)-(155, 255), colour%, BF
END SUB

SUB box2 (c%)
CIRCLE (530, 415), 40, c%
PAINT (530, 415), c%, c%
END SUB

SUB box3 (c%)
LINE (590, 380)-(620, 450), c%, BF
END SUB

SUB click1
FOR i% = 2000 TO 6000 STEP 500
SOUND i%, .15
NEXT
FOR i% = 6000 TO 2000 STEP -500
SOUND i%, .15
NEXT
END SUB

SUB flname
SHARED file$, fpos AS LONG
flname1 15
LOCATE 22, 63
file$ = inpt$("->", 8)
flname1 9
click1
CLOSE #5
OPEN "B", #5, file$ + ".wav"
fpos = 1
END SUB

SUB flname1 (c%)
COLOR c%
LOCATE 21, 61
PRINT "ษFile nameอออออออออป";
LOCATE 22, 61
PRINT "บ                  บ";
LOCATE 23, 61
PRINT "ศออออออออออออออออออผ";
END SUB

FUNCTION inpt$ (s$, l)
add$ = ""
PRINT s$;
ln = CSRLIN
cl = POS(ln)
cl1 = 1
DO UNTIL k$ = CHR$(enter)

k$ = INPUT$(1)
kc = ASC(k$)

IF (kc > 31 AND kc < 127) THEN
  IF cl1 <= l THEN
  LOCATE ln, cl1 + cl - 1
  PRINT k$;
  add$ = add$ + k$
  cl1 = cl1 + 1
  IF cl1 > l THEN cl1 = l + 1
  END IF
ELSEIF (kc = backspc) THEN
  cl1 = cl1 - 1
  IF cl1 < 1 THEN cl1 = 1
  IF (cl1 >= 1 AND add$ <> "") THEN
  add$ = LEFT$(add$, LEN(add$) - 1)
  LOCATE ln, cl + cl1 - 1
  PRINT " ";
  END IF
END IF
LOOP
inpt$ = add$
END FUNCTION

SUB modes
SHARED mode%, lmode%
modes1 15, mode%
DO UNTIL k$ = CHR$(enter)
k$ = INKEY$
SELECT CASE k$
CASE CHR$(0) + CHR$(up)
mode% = mode% - 1
IF mode% < 1 THEN mode% = 1
modes1 15, mode%
click1
CASE CHR$(0) + CHR$(down)
mode% = mode% + 1
IF mode% > lmode% THEN mode% = lmode%
modes1 15, mode%
click1
CASE ELSE
END SELECT
LOOP
modes1 11, mode%
click1
END SUB

SUB modes1 (c%, op%)
SHARED mode$, mode%
mode% = op%
LOCATE 17, 1
COLOR c%
PRINT "ษModesอออออออออออออป";
LOCATE 18, 1
PRINT "บ                  บ";
LOCATE 19, 1
PRINT "บ                  บ";
LOCATE 20, 1
PRINT "บ                  บ";
LOCATE 21, 1
PRINT "บ                  บ";
LOCATE 22, 1
PRINT "บ                  บ";
LOCATE 23, 1
PRINT "บ                  บ";
LOCATE 24, 1
PRINT "บ                  บ";
LOCATE 25, 1
PRINT "บ                  บ";
LOCATE 26, 1
PRINT "บ                  บ";
LOCATE 27, 1
PRINT "บ                  บ";
LOCATE 28, 1
PRINT "บ                  บ";
LOCATE 29, 1
PRINT "ศออออออออออออออออออผ";

IF op% <> 0 THEN
zz = FREEFILE
OPEN "R", #zz, "Data\Mode\Mode.cab"
FIELD #zz, 12 AS zz1$, 16 AS zz2$
lv1 = op%
lv2 = op% + 9
IF (lv2 > LOF(zz)) THEN
lv1 = lv2 - 9 - LOF(zz)
lv2 = LOF(zz)
END IF
FOR i = lv1 TO lv2
LOCATE (i - lv1) + 18, 3
GET #zz, i
mode$ = nospc$(zz1$)
IF (i = lv1) THEN COLOR 12 ELSE COLOR c%
PRINT zz2$;
NEXT
END IF
END SUB

FUNCTION nospc$ (s$)
add$ = ""
FOR i = 1 TO LEN(s$)
d$ = MID$(s$, i, 1)
IF d$ <> " " THEN add$ = add$ + d$
NEXT
nospc$ = add$
END FUNCTION

SUB s.assgn (k$)
SHARED active%
kc% = ASC(UCASE$(k$)) - 64
IF kc% <= 18 THEN
s.uassgn
active% = kc%
END IF
END SUB

SUB s.cvdn
SHARED freq%, level%, time#, rate%

FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = -2 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = -1 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = 1 * rate%
IF (a% = 30 OR a% = 31) THEN sn% = 2 * rate%
wk% = wk% + sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.cvdnen
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 25
IF (a% = 0 OR a% = 1) THEN sn% = -2 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = -1 * rate%
freq% = level% + sn%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
level% = freq%
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.cvup
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 2 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = 1 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = -1 * rate%
IF (a% = 30 OR a% = 31) THEN sn% = -2 * rate%
wk% = wk% + sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.cvupen
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 25
IF (a% = 0 OR a% = 1) THEN sn% = 2 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = 1 * rate%
freq% = level% + sn%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
level% = freq%
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.hldn
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = -1 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = -2 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = 2 * rate%
IF (a% = 30 OR a% = 31) THEN sn% = 1 * rate%
wk% = wk% + sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.hldnen
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 25
IF (a% = 0 OR a% = 1) THEN sn% = -1 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = -2 * rate%
freq% = level% + sn%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
level% = freq%
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.hlup
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = 2 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = -2 * rate%
IF (a% = 30 OR a% = 31) THEN sn% = -1 * rate%
wk% = wk% + sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.hlupen
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 25
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
IF (a% = 20 OR a% = 21) THEN sn% = 2 * rate%
freq% = level% + sn%
level% = freq%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.pkdn
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = -1 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = 1 * rate%
wk% = wk% + sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.pkdnen
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 25
IF (a% = 0 OR a% = 1) THEN sn% = -1 * rate%
freq% = level% + sn%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
level% = freq%
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.pkup
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = -1 * rate%
wk% = wk% + sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.pkupen
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 25
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
freq% = level% + sn%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
level% = freq%
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.play
SHARED active%
IF (active% = 1) THEN s.pkup
IF (active% = 2) THEN s.cvup
IF (active% = 3) THEN s.hlup
IF (active% = 4) THEN s.pkdn
IF (active% = 5) THEN s.cvdn
IF (active% = 6) THEN s.hldn
IF (active% = 7) THEN s.pkupen
IF (active% = 8) THEN s.cvupen
IF (active% = 9) THEN s.hlupen
IF (active% = 10) THEN s.pkdnen
IF (active% = 11) THEN s.cvdnen
IF (active% = 12) THEN s.hldnen
IF (active% = 13) THEN s.zinga
IF (active% = 14) THEN s.zingb
IF (active% = 15) THEN s.zingc
IF (active% = 16) THEN s.zingd
IF (active% = 17) THEN s.zinge
IF (active% = 18) THEN s.zingf
END SUB

SUB s.playall (k$)
SHARED slct%
IF slct% = 0 THEN
s.assgn k$
s.play
ELSE
sd.play k$
END IF
END SUB

SUB s.uassgn
SHARED active%, process%, work%, sign%
active% = 0
process% = 0
work% = 0
sign% = 0
END SUB

SUB s.zinga
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
sn% = sn% * -1
wk% = sn% * 20
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.zingb
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
sn% = sn% * -1
wk% = sn% * 5
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.zingc
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 20 * rate%
IF (a% = 25 OR a% = 26) THEN sn% = 5 * rate%
sn% = sn% * -1
wk% = sn%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.zingd
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
sn% = sn% * -1
wk% = sn% * a%
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.zinge
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
sn% = sn% * -1
wk% = sn% * (50 - a%)
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB s.zingf
SHARED freq%, level%, time#, rate%
FOR a% = 1 TO 50
IF (a% = 0 OR a% = 1) THEN sn% = 1 * rate%
sn% = sn% * -1
IF (a% < 25) THEN wk% = sn% * a% ELSE wk% = sn% * (50 - a%)
freq% = level% + wk%
IF freq% < 37 THEN freq% = 37
IF freq% > 21000 THEN freq% = 21000
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
s.uassgn
END SUB

SUB sd.mode
SHARED fmode AS LONG, ftime#, freq%, mode$
IF mode$ <> "" THEN
SEEK #6, fmode
k$ = INPUT$(2, #1)
vl = ASC(LEFT$(k$, 1)) * 256 + ASC(RIGHT$(k$, 1))
IF vl > 32767 THEN
vl1 = vl - 16384
ftime# = 18.2 / vl1
ELSE
freq% = vl
IF freq% < 37 THEN freq% = 37
IF freq% > 37 AND freq% < 32767 THEN SOUND freq%, ftime#
box1
v.show
END IF
fmode = fmode + 2
IF fmode > LOF(6) THEN sd.stpmd
END IF
END SUB

SUB sd.play (k$)
SHARED level%, freq%, fln%, ps AS LONG, fl$
vl = ASC(UCASE$(k$)) - 64
IF vl <= 26 THEN
fln% = FREEFILE
fl$ = k$
OPEN "B", #fln%, "Data\Part\" + k$ + ".lvl"
ps = 1
zz = fln%
p = ps
FOR p = 1 TO LOF(zz) STEP 2
SEEK #zz, p
k$ = INPUT$(2, #zz)
freq% = level% + (ASC(LEFT$(k$, 1)) * 256 + ASC(RIGHT$(k$, 1)) - 32767)
IF freq% < 37 THEN freq% = 37
wrtf (freq%)
IF (freq% > 37 AND freq% < 32767) THEN SOUND freq%, time#
box1
v.show
NEXT
END IF
CLOSE #zz
fl$ = ""
ps = 0
fln% = 0
END SUB

SUB sd.stpmd
SHARED mode$, fmode AS LONG, ftime#
CLOSE #6
mode$ = ""
fmode = 0
ftime# = 0
END SUB

SUB sd.strmd
SHARED mode$, fmode AS LONG, ftime#
OPEN "B", #6, "Data\Mode\" + mode$ + ".snd"
fmode = 1
ftime# = .05
END SUB

SUB status (s$)
LOCATE 1, 18
COLOR 13
PRINT "ษอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออป";
LOCATE 2, 18
PRINT "บ Status:                                                     บ";
LOCATE 3, 18
PRINT "ศอออออออออออออออออออออออออออออออออออออออออออออออออออออออออออออผ";
LOCATE 2, 28
COLOR 12
PRINT s$;
END SUB

SUB tools
SHARED level%, rate%, dblg%, time#
tools1 15
COLOR 12
LOCATE 23, 23
level% = VAL(inpt$("Frequency level -> ", 5))
click1
LOCATE 24, 23
rate% = VAL(inpt$("Frequency rate -> ", 5))
click1
LOCATE 25, 23
dblg% = VAL(inpt$("Doubling rate -> ", 5))
LOCATE 26, 23
k$ = inpt$("Speed -> ", 5)
time# = VAL(k$)
tools1 10
click1
END SUB

SUB tools1 (c%)
LOCATE 21, 21
COLOR c%
PRINT "ษToolsอออออออออออออออออออออออออออออออออป";
LOCATE 22, 21
PRINT "บ                                      บ";
LOCATE 23, 21
PRINT "บ                                      บ";
LOCATE 24, 21
PRINT "บ                                      บ";
LOCATE 25, 21
PRINT "บ                                      บ";
LOCATE 26, 21
PRINT "บ                                      บ";
LOCATE 27, 21
PRINT "บ                                      บ";
LOCATE 28, 21
PRINT "บ                                      บ";
LOCATE 29, 21
PRINT "ศออออออออออออออออออออออออออออออออออออออผ";

END SUB

SUB v.bars1
SHARED ef%, freq%, bar%, colour%
IF (bar% + 1 > 12) THEN
bar% = 0
END IF
bar% = bar% + 1
ht1 = 50 + bar% * 20
ht2 = ht1 + 15
xt = 165 + (47 / 3273) * (freq% - 37)
LINE (165, ht1)-(635, ht2), 0, BF
LINE (165, ht1)-(xt, ht2), colour%, BF
ef% = freq%
END SUB

SUB v.bars2
SHARED ef%, freq%, bar%, colour%
IF (bar% + 1 > 250) THEN
bar% = 0
END IF
bar% = bar% + 1
ht1 = 55 + bar%
xt = 165 + (47 / 3273) * (freq% - 37)
LINE (165, ht1)-(635, ht1), 0
LINE (165, ht1)-(xt, ht1), colour%
ef% = freq%
END SUB

SUB v.chng
SHARED xpos%, bar%
LINE (165, 50)-(635, 310), 0, BF
xpos% = 165
bar% = 0
END SUB

SUB v.clmn1
SHARED ef%, freq%, bar%, colour%
IF (bar% + 1 > 22) THEN
bar% = 0
END IF
bar% = bar% + 1
xt1 = 165 + bar% * 20
xt2 = xt1 + 15
ht = 310 - (26 / 3273) * (freq% - 37)
LINE (xt1, 310)-(xt2, 50), 0, BF
LINE (xt1, 310)-(xt2, ht), colour%, BF
ef% = freq%
END SUB

SUB v.clmn2
SHARED ef%, freq%, bar%, colour%
IF (bar% + 1 > 460) THEN
bar% = 0
END IF
bar% = bar% + 1
xt1 = 168 + bar%
ht = 310 - (26 / 3273) * (freq% - 37)
LINE (xt1, 310)-(xt1, 50), 0
LINE (xt1, 310)-(xt1, ht), colour%
ef% = freq%
END SUB

SUB v.crcl
SHARED ef%, freq%, bar%, colour%
IF (bar% + 1 > 120) THEN
bar% = 0
END IF
bar% = bar% + 1
ht = (36 / 3273) * (freq% - 37)
rad = (ht * (22 / 7)) / 180
CIRCLE (400, 180), bar%, 0
CIRCLE (400, 180), bar%, colour%, 0, rad
ef% = freq%
END SUB

SUB v.line1
SHARED ef%, freq%, xpos%, colour%
IF (xpos% + 2 > 635) THEN
LINE (165, 50)-(635, 310), 0, BF
xpos% = 165
END IF
xpos% = xpos% + 2
ht1 = 310 - (26 / 3273) * (ef% - 37)
ht2 = 310 - (26 / 3273) * (freq% - 37)
LINE (xpos% - 2, ht1)-(xpos%, ht2), colour%
ef% = freq%
END SUB

SUB v.line2
SHARED ef%, freq%, xpos%, colour%
DIM area(25000)
GET (167, 50)-(632, 310), area
LINE (165, 50)-(632, 310), 0, BF
PUT (165, 50), area
ht1 = 310 - (26 / 3273) * (ef% - 37)
ht2 = 310 - (26 / 3273) * (freq% - 37)
LINE (630, ht1)-(632, ht2), colour%
ef% = freq%
ERASE area
END SUB

SUB v.rect
SHARED ef%, freq%, bar%, colour%

IF bar% > 100 THEN bar% = 0
bar% = bar% + 1
dist = bar% * 8
ds = (dist / 32730) * (freq% - 37)
es = bar% * 2

LINE (400 - bar%, 180 - bar%)-(400 + bar%, 180 + bar%), 0, B
IF (ds > 0) THEN
IF (ds > es) THEN
LINE (400 + bar%, 180 + bar%)-(400 + bar%, 180 - bar%), colour%
ds = ds - es
ELSE
LINE (400 + bar%, 180 + bar%)-(400 + bar%, 180 + bar% - ds), colour%
ds = 0
END IF
END IF
IF (ds > 0) THEN
IF (ds > es) THEN
LINE (400 + bar%, 180 - bar%)-(400 - bar%, 180 - bar%), colour%
ds = ds - es
ELSE
LINE (400 + bar%, 180 - bar%)-(400 + bar% - ds, 180 - bar%), colour%
ds = 0
END IF
END IF
IF (ds > 0) THEN
IF (ds > es) THEN
LINE (400 - bar%, 180 - bar%)-(400 - bar%, 180 + bar%), colour%
ds = ds - es
ELSE
LINE (400 - bar%, 180 - bar%)-(400 - bar%, 180 - bar% + ds), colour%
ds = 0
END IF
END IF
IF (ds > 0) THEN
IF (ds > es) THEN
LINE (400 - bar%, 180 + bar%)-(400 + bar%, 180 + bar%), colour%
ds = ds - es
ELSE
LINE (400 - bar%, 180 + bar%)-(400 - bar% + ds, 180 + bar%), colour%
ds = 0
END IF
END IF

ef% = freq%
END SUB

SUB v.show
SHARED style%
SELECT CASE style%
CASE 1
v.line1
CASE 2
v.line2
CASE 3
v.spll1
CASE 4
v.spll2
CASE 5
v.bars1
CASE 6
v.bars2
CASE 7
v.clmn1
CASE 8
v.clmn2
CASE 9
v.crcl
CASE 10
v.rect
CASE ELSE
END SELECT
END SUB

SUB v.spll1
SHARED ef%, freq%, xpos%, colour%
IF (xpos% + 1 > 635) THEN
LINE (165, 50)-(635, 310), 0, BF
xpos% = 165
END IF
xpos% = xpos% + 1
ht2 = (13 / 3273) * (freq% - 37)
LINE (xpos%, 180 - ht2)-(xpos%, 180 + ht2), colour%
ef% = freq%
END SUB

SUB v.spll2
SHARED ef%, freq%, xpos%, colour%
DIM area(25000)
GET (166, 50)-(632, 310), area
LINE (165, 50)-(632, 310), 0, BF
PUT (165, 50), area
ht2 = (13 / 3273) * (freq% - 37)
LINE (632, 180 - ht2)-(632, 180 + ht2), colour%
ef% = freq%
ERASE area
END SUB

SUB vstyle
vstyle1 15, 1
o% = 1
DO UNTIL k$ = CHR$(enter)
k$ = INKEY$
SELECT CASE k$
CASE CHR$(0) + CHR$(up)
o% = o% - 1
IF o% < 1 THEN o% = 1
vstyle1 15, o%
click1
CASE CHR$(0) + CHR$(down)
o% = o% + 1
IF o% > 10 THEN o% = 10
vstyle1 15, o%
click1
CASE ELSE
END SELECT
LOOP
style% = o%
click1
v.chng
vstyle1 12, o%
END SUB

SUB vstyle1 (c%, op%)
COLOR c%
LOCATE 4, 1
PRINT "ษView Stylesออออป";
LOCATE 5, 1
PRINT "บ               บ";
LOCATE 6, 1
PRINT "บ  Lines - 1    บ";
LOCATE 7, 1
PRINT "บ  Lines - 2    บ";
LOCATE 8, 1
PRINT "บ  Spill - 1    บ";
LOCATE 9, 1
PRINT "บ  Spill - 2    บ";
LOCATE 10, 1
PRINT "บ  Bars - 1     บ";
LOCATE 11, 1
PRINT "บ  Bars - 2     บ";
LOCATE 12, 1
PRINT "บ  Columns - 1  บ";
LOCATE 13, 1
PRINT "บ  Columns - 2  บ";
LOCATE 14, 1
PRINT "บ  Circles      บ";
LOCATE 15, 1
PRINT "บ  Rectangles   บ";
LOCATE 16, 1
PRINT "ศอออออออออออออออผ";
COLOR 11
LOCATE op% + 5, 4
SELECT CASE op%
CASE 1
PRINT "Lines - 1";
CASE 2
PRINT "Lines - 2";
CASE 3
PRINT "Spill - 1";
CASE 4
PRINT "Spill - 2";
CASE 5
PRINT "Bars - 1";
CASE 6
PRINT "Bars - 2";
CASE 7
PRINT "Columns - 1";
CASE 8
PRINT "Columns - 2";
CASE 9
PRINT "Circles";
CASE 10
PRINT "Rectangles";
CASE ELSE
END SELECT
END SUB

SUB wrtf (f%)
SHARED fpos AS LONG
IF f% > 0 THEN
vl1 = INT(f% / 256)
vl2 = f% - (256 * vl1)
a$ = CHR$(vl1) + CHR$(vl2)
PUT #5, fpos, a$
fpos = fpos + 2
END IF
END SUB

SUB wrtt (t#)
SHARED fpos AS LONG
x = INT(18.2 / t#)
vl1 = INT(x / 256)
vl2 = x - vl1 * 256
vl1 = vl1 + 128
a$ = CHR$(vl1) + CHR$(vl2)
PUT #5, fpos, a$
fpos = fpos + 2
END SUB

