DECLARE SUB vwcir (f AS INTEGER, c AS INTEGER)
DECLARE SUB vwfreq (f AS INTEGER, p AS INTEGER)
DECLARE SUB frcr (a AS INTEGER)
DECLARE SUB ers (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER)
DECLARE SUB mainfreq (f1 AS INTEGER, f2 AS INTEGER, t AS INTEGER)
DECLARE SUB cirfreq (f AS INTEGER, c AS INTEGER)
DECLARE FUNCTION comp$ (f AS INTEGER)
DECLARE SUB time1 (m AS INTEGER, s1 AS INTEGER, s2 AS INTEGER)
DECLARE SUB level (f AS INTEGER)
DECLARE SUB freq (f AS INTEGER)
DECLARE SUB status1 (f AS STRING)
DECLARE SUB filenm (f AS STRING)
DECLARE SUB status ()
DECLARE SUB sclear (mx AS INTEGER, my AS INTEGER, f AS INTEGER, t AS DOUBLE, c1 AS INTEGER, c2 AS INTEGER, c3 AS INTEGER)
DECLARE SUB controls ()
ON ERROR GOTO errs
CLS
SCREEN 12
COLOR 12
sclear 640, 480, 21000, .1, 1, 2, 13
man: LOCATE 1, 20
PRINT "Basic Piano"
COLOR 11
LOCATE 2, 20
PRINT "-----------"
COLOR 10
LOCATE 4, 50
PRINT "- by Subhajit sahu"
COLOR 9
k$ = INPUT$(1)
IF k$ = CHR$(27) THEN SYSTEM
controls
LOCATE 1, 1
PRINT "Options"
COLOR 8
PRINT "-------"
PRINT ""
PRINT ""
COLOR 7
PRINT "1 - Create a sound file"
PRINT "2 - Load a sound file"
PRINT ""
PRINT ""
a1: k$ = INPUT$(1)
IF k$ = "1" THEN GOTO cre
IF k$ = "2" THEN GOTO loa
IF k$ = CHR$(27) THEN SYSTEM
GOTO a1

cre: INPUT "File name"; f$
INPUT "Initial frequency level"; fl%
CLS
status
FOR iz% = 40 TO fl%
cirfreq iz%, 9
NEXT
filenm f$
level fl%
status1 "Saving"
time1 0, 0, 0

OPEN "O", #1, f$
tt = 0
fc% = fl%
fcr% = 10
tp% = 1
fs% = 1
cc = 1
mt = 0
mainfreq fc%, fl%, tp%
mn: k$ = UCASE$(INKEY$)
mainfreq fc%, fl%, tp%
fc% = fl%
IF k$ = "" THEN GOTO zz
k1 = ASC(k$)
IF k1 >= 65 AND k1 <= 90 THEN GOTO pl
IF k$ = "8" THEN fl% = fl% + fcr%
IF k$ = "2" THEN fl% = fl% - fcr%
IF fl% < 40 THEN fl% = 40
IF fl% > 20000 THEN fl% = 20000
IF k$ = "6" THEN fcr% = fcr% + 5
IF k$ = "4" THEN fcr% = fcr% - 5
IF fcr% < 5 THEN fcr% = 5
IF k$ = "+" THEN tp% = tp% + 1
IF k$ = "-" THEN tp% = tp% - 1
IF tp% < 1 THEN tp% = 1
IF tp% > 2 THEN t = 2 ELSE t = 3
IF k$ = "[" THEN fs% = fs% + 1
IF k$ = "]" THEN fs% = fs% - 1
IF fs% < 1 THEN fs% = 1
IF fs% > 5 THEN fs% = 5
IF k$ = "\" AND mt = 0 THEN mt = 1
IF k$ = "\" AND mt = 1 THEN mt = 0
zz: IF k$ = CHR$(27) THEN GOTO cl
cirfreq fl%, 5
freq fc%
level fl%
frcr fcr%
tt = tt + tp%
tm% = FIX(tt / 60)
ts% = tt MOD 60
ts1% = RND(3) * 100
time1 tm%, ts%, ts1%
IF mt = 1 THEN GOTO mu
SELECT CASE fs%
CASE 1
SOUND fl%, t
WRITE #1, fl%, t
CASE 2
tt2 = t / 2
SOUND fl%, tt2
SOUND fl% - 100, tt2
WRITE #1, fl%, tt2
WRITE #1, fl% - 100, tt2
CASE 3
tt2 = t / 3
SOUND fl%, tt2
SOUND fl% - 100, tt2
SOUND fl% + 100, tt2
WRITE #1, fl%, tt2
WRITE #1, fl% - 100, tt2
WRITE #1, fl% + 100, tt2
CASE 4
tt2 = t / 4
SOUND fl%, tt2
SOUND fl% - 100, tt2
SOUND fl% + 100, tt2
SOUND fl% - 200, tt2
WRITE #1, fl%, tt2
WRITE #1, fl% - 100, tt2
WRITE #1, fl% + 100, tt2
WRITE #1, fl% - 200, tt2
CASE 5
tt2 = t / 5
SOUND fl%, tt2
SOUND fl% - 100, tt2
SOUND fl% + 100, tt2
SOUND fl% - 200, tt2
SOUND fl% + 200, tt2
WRITE #1, fl%, tt2
WRITE #1, fl% - 100, tt2
WRITE #1, fl% + 100, tt2
WRITE #1, fl% - 200, tt2
WRITE #1, fl% + 200, tt2
END SELECT
GOTO mn
mu: SOUND 21000, t
WRITE #1, 21000, t
GOTO mn
pl: k2 = k1 - 64
f2% = FIX(((200 - .4) / .26) * k2)
stp1 = ABS(f2% - fl%) / 20
tp1 = t / 20
IF f2% < fl% THEN stp = -1 * stp1 ELSE stp = stp1
FOR iz% = fl% TO f2% STEP stp
SOUND iz%, tp1
WRITE #1, iz%, tp1
freq iz%
cirfreq iz%, 11
NEXT
stp = -1 * stp
FOR iz% = f2% TO fl% STEP stp
SOUND iz%, tp1
WRITE #1, iz%, tp1
freq iz%
cirfreq iz%, 10
NEXT
GOTO mn
cl: CLOSE #1
sclear 640, 480, 21000, 0, 1, 2, 3
PRINT "Thank You for using BASIC PIANO"
k$ = INPUT$(1)
SYSTEM

errs: RESUME NEXT

loa: sclear 640, 480, 21000, 0, 3, 4, 5
COLOR 2
INPUT "File name"; f$
LOCATE 3, 1
COLOR 3
PRINT "Select option -->"
COLOR 4
PRINT "1 - View file"
PRINT "2 - Continue playing"
PRINT "Option(1/2)-->"
zx1: k$ = INPUT$(1)
IF k$ = "1" THEN GOTO vw
IF k$ = "2" THEN GOTO con
GOTO zx1

vw: OPEN "I", #1, f$
FOR i = 1 TO 2 STEP 0
IF EOF(1) THEN GOTO za1
INPUT #1, f, t
tt = tt + t
NEXT
za1: CLOSE #1
sclear 640, 480, 21000, 0, 8, 7, 6
CLS
p% = 0
status
filenm f$
status1 "Playing"
LOCATE 29, 40
COLOR 4
tm% = FIX(tt / 60)
s1% = tt MOD 60
PRINT "Total time :"; tm%; ":"; s1%; ": 00";
tt = 0
OPEN "I", #1, f$
FOR i = 1 TO 2 STEP 0
IF EOF(1) THEN GOTO zu1
INPUT #1, f%, t
tt = tt + t
m% = FIX(tt / 60)
s1% = tt MOD 60
s2% = (tt MOD 20) + (tt MOD 30)
time1 m%, s1%, s2%
zz% = (m% MOD 15) + 1
vwcir f%, zz%
vwfreq f%, p%
p% = p% + 1
IF p% > 450 THEN ers 0, 20, 450, 420
IF p% > 450 THEN p% = 0
SOUND f%, t
NEXT
zu1: CLOSE #1
k$ = INPUT$(1)
sclear 640, 480, 21000, 0, 5, 2, 7
GOTO man
con: CLS
status
OPEN "I", #1, f$
INPUT #1, f%, t
CLOSE #1
FOR iz% = 40 TO f%
cirfreq iz%, 9
NEXT
filenm f$
level f%
frcr 0
status1 "Loading"
time1 0, 0, 0
OPEN "I", 1, f$
tt = 0
FOR i = 1 TO 2 STEP 0
IF EOF(1) THEN GOTO df
INPUT #1, f%, t
cirfreq f%, 5
freq f%
level f%
tt = tt + t
m% = FIX(tt / 60)
s1% = tt MOD 60
s2% = RND(3) * 100
time1 m%, s1%, s2%
mainfreq f%, f%, 1
NEXT
df: CLOSE #1
OPEN "A", #1, f$
fl% = f%
fc% = fl%
mt = 0
cc = 1
fs% = 1
tp% = 1
fcr% = 10
GOTO mn

SUB cirfreq (f AS INTEGER, c AS INTEGER)
r = (.14 / 40) * (f - 40)
CIRCLE (570, 350), r, c
CIRCLE (570, 350), r + 1, 0
END SUB

FUNCTION comp$ (f AS INTEGER)
z$ = STR$(f)
IF LEN(z$) < 2 THEN z$ = "0" + z$
comp$ = z$
END FUNCTION

SUB controls
sclear 640, 480, 21000, 0, 5, 6, 7
COLOR 12
PRINT "Controls"
COLOR 4
PRINT "--------"
COLOR 6
PRINT ""
PRINT ""
PRINT "(A-Z) - Music drop and back"
PRINT "8 - Increase current frequency"
PRINT "2 - Decrease current frequnecy"
PRINT "6 - Increase frequency change rate"
PRINT "4 - Decrease frequency change rate"
PRINT "+ - Increase time passage speed"
PRINT "- - Decrease time passage speed"
PRINT "[ - Increase frequency speading range"
PRINT "] - Decrease frequency spreading range"
PRINT "\ - Mute music(on/off)"
PRINT "Esc - Save and exit"
PRINT ""
COLOR 2
PRINT "Press any key to continue"
k$ = INPUT$(1)
sclear 640, 480, 21000, 0, 10, 9, 8
END SUB

SUB ers (x1 AS INTEGER, y1 AS INTEGER, x2 AS INTEGER, y2 AS INTEGER)
FOR i = y1 TO y2
LINE (x1, i)-(x2, i), 0
NEXT
END SUB

SUB filenm (f AS STRING)
LOCATE 1, 20
COLOR 12
PRINT "File name : " + f$
END SUB

SUB frcr (a AS INTEGER)
LOCATE 29, 40
COLOR 5
PRINT "Frequency change rate :"; a; "Hz    ";
END SUB

SUB freq (f AS INTEGER)
LOCATE 28, 1
COLOR 11
PRINT "Sound frequency :"; f; "Hz    ";
END SUB

SUB level (f AS INTEGER)
LOCATE 28, 40
COLOR 9
PRINT "Frequency level :"; f; "Hz    ";
END SUB

SUB mainfreq (f1 AS INTEGER, f2 AS INTEGER, t AS INTEGER)
DIM a(1 TO 7000)
r1% = 420 - FIX(.02 * (f1 - 40))
r2% = 420 - FIX(.02 * (f2 - 40))
x = 450
GET (t, 20)-(x, 120), a
ers 0, 20, 450, 120
PUT (0, 20), a
GET (t, 121)-(x, 220), a
ers 0, 121, 450, 220
PUT (0, 121), a
GET (t, 221)-(x, 320), a
ers 0, 221, 450, 320
PUT (0, 221), a
GET (t, 321)-(x, 420), a
ers 0, 321, 450, 420
PUT (0, 321), a
LINE (451 - t, r1%)-(451, r2%), 10
ers 452, r2% - 10, 456, r2% + 10
LINE (452, r2% - 5)-(456, r2%), 9
LINE -(452, r2% + 5), 9
END SUB

SUB sclear (mx AS INTEGER, my AS INTEGER, f AS INTEGER, t AS DOUBLE, c1 AS INTEGER, c2 AS INTEGER, c3 AS INTEGER)
xi = mx / my
FOR y = my TO 0 STEP -1
x = FIX(xi * y)
LINE (0, my)-(x, y), c1: LINE (mx, 0)-(x, y), c1
y1 = y + 1
x1 = FIX(xi * y1)
LINE (0, my)-(x1, y1), 0: LINE (mx, 0)-(x1, y1), 0
LINE (0, my)-(x1 - 1, y - 1), c1: LINE (mx, 0)-(x - 1, y - 1), c1
y1 = y - 5
x1 = FIX(xi * y1): x = x1
LINE (0, my)-(x1, y1), c2: LINE (mx, 0)-(x1, y1), c2
y1 = y1 + 1
x1 = FIX(xi * y1)
LINE (0, my)-(x1, y1), 0: LINE (mx, 0)-(x1, y1), 0
LINE (0, my)-(x - 1, y - 6), c2: LINE (mx, 0)-(x - 1, y - 6), c2
y1 = y - 10
x1 = FIX(xi * y1): x = x1
LINE (0, my)-(x1, y1), c3: LINE (mx, 0)-(x1, y1), c3
y1 = y1 + 1
x1 = FIX(xi * y1)
LINE (0, my)-(x1, y1), 0: LINE (mx, 0)-(x1, y1), 0
LINE (0, my)-(x - 1, y - 11), c3: LINE (mx, 0)-(x - 1, y - 11), c3
SOUND f, t
NEXT
END SUB

SUB status
LINE (0, 16)-(640, 16), 12
LINE (0, 17)-(640, 17), 12
LINE (0, 18)-(640, 18), 12
LINE (0, 19)-(640, 19), 12
LINE (0, 421)-(640, 421), 12
LINE (0, 422)-(640, 422), 12
LINE (0, 423)-(640, 423), 12
LINE (0, 424)-(640, 424), 12
LINE (499, 420)-(499, 277), 11
LINE (498, 420)-(498, 277), 11
LINE (497, 420)-(497, 277), 11
LINE (500, 277)-(640, 277), 11
LINE (500, 278)-(640, 278), 11
LINE (500, 279)-(640, 279), 11
END SUB

SUB status1 (f AS STRING)
LOCATE 1, 50
COLOR 10
PRINT "Status : " + f$
END SUB

SUB time1 (m AS INTEGER, s1 AS INTEGER, s2 AS INTEGER)
LOCATE 29, 1
COLOR 8
PRINT "Time : " + comp$(m) + "::" + comp$(s1) + "." + comp$(s2);
END SUB

SUB vwcir (f AS INTEGER, c AS INTEGER)
r = (.14 / 40) * (f - 40)
CIRCLE (570, 350), r, c
CIRCLE (570, 350), r + 1, 0
CIRCLE (570, 350), r - 1, c
CIRCLE (570, 350), r - 2, c
CIRCLE (570, 350), r - 3, c
CIRCLE (570, 350), r - 4, c
CIRCLE (570, 350), r - 5, c
END SUB

SUB vwfreq (f AS INTEGER, p AS INTEGER)
y = FIX(.01 * f)
LINE (p, 220 - y)-(p, 220 + y), 10
END SUB

