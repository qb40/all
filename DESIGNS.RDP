DECLARE SUB dsg.model1 ()
DECLARE SUB dsg.model2 ()
DECLARE SUB dsg.model3 ()
DECLARE SUB dsg.model4 ()
DECLARE SUB dsg.model5 ()
DECLARE SUB dsg.model6 ()
DECLARE SUB dsg.model7 ()
DECLARE SUB dsg.model8 ()
DECLARE SUB dsg.model9 ()
DECLARE SUB dsg.model10 ()
DECLARE SUB dsg.model11 ()
DECLARE SUB pal.model1 ()
DECLARE SUB pal.model2 ()
DECLARE SUB pal.model3 ()
DECLARE SUB pal.model4 ()
DECLARE SUB pal.model5 ()
DECLARE SUB pal.model6 ()
DECLARE SUB sew.model1 ()
DECLARE SUB sew.model2 ()
DECLARE SUB sew.model3 ()
DECLARE SUB sew.model4 ()
DECLARE SUB sew.model5 ()
DECLARE SUB sew.model6 ()
DECLARE SUB sew.model7 ()
DECLARE SUB sew.model8 ()
DECLARE SUB sew.model9 ()
DECLARE SUB sew.model10 ()
DECLARE SUB sew.model11 ()
DECLARE SUB sew.model12 ()
DECLARE SUB sew.model13 ()
DECLARE SUB sew.model14 ()
DECLARE SUB sew.model15 ()
DECLARE SUB mig.model1 ()
DECLARE SUB mig.model2 ()
DECLARE SUB mig.model3 ()
DECLARE SUB mig.model4 ()
DECLARE SUB ima.model1 ()
DECLARE SUB ima.model2 ()
DECLARE SUB ima.model3 ()
DECLARE SUB ima.model4 ()
DECLARE SUB ima.model5 ()
DECLARE SUB ima.model6 ()
DECLARE SUB ima.model7 ()
DECLARE SUB ima.model8 ()
DECLARE SUB ima.model9 ()
DECLARE SUB ima.model10 ()
DECLARE SUB ima.model11 ()
DECLARE SUB ima.model12 ()
DECLARE SUB ima.model13 ()
DECLARE SUB ima.model14 ()
DECLARE SUB ima.model15 ()
DECLARE SUB ima.model16 ()
DECLARE SUB ima.model17 ()
DECLARE SUB ima.model18 ()
DECLARE SUB ima.model19 ()
DECLARE SUB ima.model20 ()

DECLARE SUB support.bentline1 (x1%, y1%, x2%, y2%, clr%)

'Declaring Keys
CONST backspc = 8, enter = 13, htab = 9, esc = 27
CONST left = 75, right = 77, up = 72, down = 80
CONST uplt = 71, uprt = 73, dnlt = 79, dnrt = 81
CONST insert = 82, home = 73, pageup = 71, del = 83, endk = 81, pagedn = 79
CONST kf1 = 59, kf2 = 60, kf3 = 61, kf4 = 62, kf5 = 63, kf6 = 64, kf7 = 65, kf8 = 66, kf9 = 67, kf10 = 68, kf11 = 133, kf12 = 134
'Keys declared




DIM sine&(359), cosine&(359)
DIM Names$(55)
FOR i% = 0 TO 359
sine&(i%) = SIN((i% * (22 / 7)) / 180) * 100000
cosine&(i%) = COS((i% * (22 / 7)) / 180) * 100000
NEXT
FOR i% = 0 TO UBOUND(Names$)
READ Names$(i%)
NEXT






SCREEN 13
DO
GOSUB men.menu
SELECT CASE Selection%
CASE 0
dsg.model1
CASE 1
dsg.model2
CASE 2
dsg.model3
CASE 3
dsg.model4
CASE 4
dsg.model5
CASE 5
dsg.model6
CASE 6
dsg.model7
CASE 7
dsg.model8
CASE 8
dsg.model9
CASE 9
dsg.model10
CASE 10
dsg.model11
CASE 11
ima.model1
CASE 12
ima.model2
CASE 13
ima.model3
CASE 14
ima.model4
CASE 15
ima.model5
CASE 16
ima.model6
CASE 17
ima.model7
CASE 18
ima.model8
CASE 19
ima.model9
CASE 20
ima.model10
CASE 21
ima.model11
CASE 22
ima.model12
CASE 23
ima.model13
CASE 24
ima.model14
CASE 25
ima.model15
CASE 26
ima.model16
CASE 27
ima.model17
CASE 28
ima.model18
CASE 29
ima.model19
CASE 30
ima.model20
CASE 31
mig.model1
CASE 32
mig.model2
CASE 33
mig.model3
CASE 34
mig.model4
CASE 35
pal.model1
CASE 36
pal.model2
CASE 37
pal.model3
CASE 38
pal.model4
CASE 39
pal.model5
CASE 40
pal.model6
CASE 41
sew.model1
CASE 42
sew.model2
CASE 43
sew.model3
CASE 44
sew.model4
CASE 45
sew.model5
CASE 46
sew.model6
CASE 47
sew.model7
CASE 48
sew.model8
CASE 49
sew.model9
CASE 50
sew.model10
CASE 51
sew.model11
CASE 52
sew.model12
CASE 53
sew.model13
CASE 54
sew.model14
CASE 55
sew.model15
CASE ELSE
END SELECT
LOOP


men.menu:
COLOR 7
CLS
PRINT "Random Colour Fun"
PRINT "================="
FOR i% = 0 TO 319 STEP 5
support.bentline1 i%, 40, i% * SIN(i% * .01), 40 + SIN(i% * .01) * 10, SIN(i% * .01) * 255
NEXT
LINE (0, 70)-(319, 70), 1, B
LINE (0, 80)-(319, 80), 1, B
LOCATE 10, 1
PRINT Names$(Selection%);
k$ = ""
DO UNTIL k$ = CHR$(enter) OR k$ = CHR$(esc)
        k$ = ""
        WHILE k$ = ""
        k$ = INKEY$
        WEND
SELECT CASE k$
CASE CHR$(0) + CHR$(left)
Selection% = Selection% - 1
IF (Selection% < 0) THEN Selection% = 55
LOCATE 10, 1
PRINT SPACE$(40);
LOCATE 10, 1
PRINT Names$(Selection%)
CASE CHR$(0) + CHR$(right)
Selection% = Selection% + 1
IF (Selection% > 55) THEN Selection% = 0
LOCATE 10, 1
PRINT SPACE$(40);
LOCATE 10, 1
PRINT Names$(Selection%)
CASE CHR$(esc)
CLS
PRINT "Wasn't that crazy?"
PRINT "I did it just for fun . . ."
PRINT "Hope you don't feel bad"
LOCATE 6, 1
PRINT "Tried by: Subhajit Sahu(GamerZ)"
PRINT "Mail: qbasic40@gmail.com"
PRINT "Please mail me."
k$ = INPUT$(1)
SYSTEM
CASE CHR$(enter)
LOCATE 10, 1
COLOR 15
PRINT Names$(Selection%)
FOR i = -10000 TO 10000 STEP .005
NEXT
CASE ELSE
END SELECT
LOOP
CLS
RETURN





DATA "Farmland","Compression","Super Compression","Rise and Fall","Niagra 2","Look At Colours From A Dashing Plane"
DATA "Look At Colours From A Dashing Plane 2","A Fall","Far Away","Blurry Guy","Above"
DATA "Lost Gravity Dust","Bee Dust","Striking Dust Form Sediment","Heavy Particle Sedimentation","Raindrops Form Ice","Rain On A Window Pane"
DATA "Threads","Thread Fall","Sewed Wires","Drifting Wires","Drifting Wires 2","Octopus Flower","Circle Of Time"
DATA "Dripple","Spread","Throw","Festival","Sprinkle Wave","Crackers","Diffused Band"
DATA "Fan","Round","Ved Area","Energy"
DATA "Sudden Waves","Proceeding","Dust","Change","Mesh","Blocked"
DATA "Line Tunnel","Dancing Pipe","Dancing Pipe 2","Dancing Pipe 3","Dancing Pipe 4","Dancing Pipe 5"
DATA "Dancing Pipe 6","Dancing Pipe 7","Sometimes A Pyramid","Colour Area","Way","Want","Want 2","Pillars","Pillars 2"

SUB dsg.model1
stp = 1
DO
k = k + stp
FOR i% = 0 TO 319
FOR j% = 0 TO 199
PSET (i%, j%), CINT(SIN(i%) * COS(j%) * k)
NEXT
NEXT
IF (k > 254) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model10
SHARED sine&(), cosine&()
stp& = 2
DO
k& = k& + stp&
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT((((((i% * cosine&(k&)) \ 100000) * sine&(k&)) \ 100000) * cosine&(k&)) \ 100000 + (j% * sine&(k&)) \ 100000)
NEXT
NEXT
IF (k& > 357 OR k& < 2) THEN stp& = -stp&
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model11
stp = .1
DO
k = k + stp
FOR i% = 0 TO 319
FOR j% = 0 TO 199
PSET (i%, j%), CINT(i% * COS(k) + j% * COS(k))
NEXT
NEXT
IF (k > 30 OR k < .1) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model2
stp = .1
DO
k = k + stp
FOR i% = 0 TO 319
FOR j% = 0 TO 199
PSET (i%, j%), CINT(SIN(k) * i%)
NEXT
NEXT
IF (k > 254) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model3
stp = .1
DO
k = k + stp
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT(LOG(i%) * k)
NEXT
NEXT
IF (k > 30 OR k < .01) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model4
stp = .01
l = 254
DO
k = k + stp
l = l - stp
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(SIN(k) * COS(l) * (i& * j& * .005))
NEXT
NEXT
IF (k > 254) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model5
stp = .1
DO
k = k + stp
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT(LOG(i%) * LOG(j%) * k)
NEXT
NEXT
IF (k > 30 OR k < .01) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model6
stp = .1
DO
k = k + stp
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT(i% * COS(k) + j% * SIN(k))
NEXT
NEXT
IF (k > 30 OR k < .1) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model7
stp = .1
DO
k = k + stp
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT(i% * COS(k) - j% * SIN(k))
NEXT
NEXT
IF (k > 30 OR k < .1) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model8
stp = .1
DO
k = k + stp
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT(i% * COS(k) + j% * SIN(k) * COS(k))
NEXT
NEXT
IF (k > 30 OR k < .1) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB dsg.model9
stp = .1
DO
k = k + stp
FOR i% = 1 TO 319
FOR j% = 1 TO 199
PSET (i%, j%), CINT(i% * COS(k) * LOG(k) + j% * SIN(k) * LOG(k))
NEXT
NEXT
IF (k > 30 OR k < .1) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model1
DIM parx%(200), pary%(200)
DO
FOR i% = 0 TO UBOUND(parx%)
PSET (parx%(i%), pary%(i%)), 0
parx%(i%) = parx%(i%) + RND(1) * 3 - 1
pary%(i%) = pary%(i%) + RND(1)
IF (parx%(i%) < 1 OR parx%(i%) > 319) THEN parx%(i%) = CINT(RND(1) * 319)
IF (pary%(i%) < 1 OR pary%(i%) > 199) THEN pary%(i%) = 1
PSET (parx%(i%), pary%(i%)), 100
NEXT
IF (INKEY$ <> "") THEN EXIT DO
WAIT &H3DA, 8
LOOP
END SUB

SUB ima.model10
DO
WAIT &H3DA, 8
k% = (k% + 1) MOD 256
l% = (l% + 1) MOD 200
m% = (m% + 1) MOD 320
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, 0, m% * COS(i% * .01), l% * COS(i% * .01), k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, m% * SIN(i% * .01), 0, l% * SIN(i% * .01), 0
NEXT
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, l% * COS(i% * .01), m% * COS(i% * .01), 0, k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, l% * SIN(i% * .01), 0, m% * SIN(i% * .01), 0
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model11
DO
WAIT &H3DA, 8
k% = (k% + 1) MOD 256
l% = (l% + 1) MOD 200
m% = (m% + 1) MOD 320
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, 0, m% * COS(i% * .01), l% * COS(i% * .01), k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, m% * SIN(i% * .01), 0, l% * SIN(i% * .01), k% * COS(i% * .01)
NEXT
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, l% * COS(i% * .01), m% * COS(i% * .01), 0, 0
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, l% * SIN(i% * .01), 0, m% * SIN(i% * .01), 0
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model12
DO
r% = (r% + 1) MOD 100
FOR i = 0 TO 6.29 STEP .1
support.bentline1 150, 100, 150 + r% * COS(i), 100 + r% * SIN(i), 255 * SIN(i)
NEXT
WAIT &H3DA, 8
FOR i = 0 TO 12.89 STEP .17
support.bentline1 150, 100, 150 + r% * COS(i), 100 + r% * SIN(i), 0
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model13
DO
r% = (r% + 11) MOD 100
FOR i = 0 TO 6.29 STEP .05
support.bentline1 150, 100, 150 + r% * COS(i), 100 + r% * SIN(i), 255 * SIN(i)
WAIT &H3DA, 8
IF (INKEY$ <> "") THEN EXIT DO
NEXT
FOR i = 0 TO 6.29 STEP .05
support.bentline1 150, 100, 150 + r% * COS(i), 100 + r% * SIN(i), 0
NEXT
LOOP
END SUB

SUB ima.model14
DO
r% = (r% + 11) MOD 100
FOR i = 0 TO 6.29 STEP .05
PSET (150 + RND(1) * 10 + r% * COS(i), RND(2) * 10 + 100 + r% * SIN(i)), 255 * SIN(i)
PSET (150 + RND(1) + r% * COS(i), RND(2) + 100 + r% * SIN(i)), 0
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model15
DO
r% = (r% + 11) MOD 100
FOR i = 0 TO 6.29 STEP .05
PSET (150 + RND(1) + r% * (COS(i) - SIN(r%)), RND(2) + 100 + r% * (SIN(i) + COS(r%))), 255 * SIN(i)
PSET (150 + RND(1) + r% * (COS(i) - SIN(r%)), RND(2) + 100 + r% * (SIN(i) + COS(r%))), 0
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model16
DO
r% = (r% + 1) MOD 100
k = k + .01
IF (k > 3000) THEN k = 0
FOR i = 0 TO 6.29 STEP .05
PSET (150 + r% * (COS(i) - COS(k)), 100 + r% * (SIN(i) + SIN(k))), 0' 255 * SIN(i)
PSET (150 + r% * (COS(i) - SIN(k)), 100 + r% * (SIN(i) + COS(k))), 255 * COS(i)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model17
DO
r% = (r% + 1) MOD 100
k = k + .01
IF (k > 3000) THEN k = 0
FOR i = 0 TO 6.29 STEP .05
PSET (150 + r% * (TAN(i) - TAN(k)), 100 + r% * (TAN(i) + TAN(k))), 0' 255 * SIN(i)
PSET (150 + r% * (COS(i) - TAN(k)), 100 + r% * (SIN(i) + TAN(k))), 255 * COS(i)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model18
DO
c% = c% + 1
r% = (r% + 1) MOD 500
IF (c% = 2000) THEN
CLS
c% = 0
END IF
k = k + .01
IF (k > 3000) THEN k = 0
FOR i = 0 TO 6.29 STEP .05
PSET (150 + r% * (COS(i) - TAN(k)), 50 + r% * (TAN(i) - COS(k))), 255 * COS(i)
'PSET (150 + r% * (COS(i) - TAN(k)), 100 + r% * (SIN(i) + TAN(k))), 255 * COS(i)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model19
DO
c% = c% + 1
r% = (r% + 1) MOD 500
IF (c% = 2000) THEN
CLS
c% = 0
END IF
k = k + .01
IF (k > 3000) THEN k = 0
FOR i = 0 TO 6.29 STEP .05
PSET (150 + r% * (COS(i) - TAN(k)), 100 + r% * (SIN(i) + TAN(k))), 255 * SIN(i)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model2
DIM parx%(200), pary%(200)
DO
FOR i% = 0 TO UBOUND(parx%)
PSET (parx%(i%), pary%(i%)), 0
parx%(i%) = parx%(i%) + RND(1) * 3 - 2
pary%(i%) = pary%(i%) + RND(1) * 3 - 1
IF (parx%(i%) < 1 OR parx%(i%) > 319) THEN parx%(i%) = CINT(RND(1) * 319)
IF (pary%(i%) < 1 OR pary%(i%) > 199) THEN pary%(i%) = 1
PSET (parx%(i%), pary%(i%)), 14
NEXT
IF (INKEY$ <> "") THEN EXIT DO
WAIT &H3DA, 8
LOOP
END SUB

SUB ima.model20
DO
k = k + .01
FOR i% = 0 TO 319
FOR j% = 0 TO 199
a1& = i% * CLNG(j%)
PSET (i% + RND(1), j% + RND(2)), (SIN(a1& * .0001) + COS(a1& * .0001)) * k * 10
NEXT
NEXT
IF (k > 200) THEN k = 0
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model3
DIM parx%(200), pary%(200)
DO
FOR i% = 0 TO UBOUND(parx%)
a1% = parx%(i%)
a2% = pary%(i%)
parx%(i%) = parx%(i%) + RND(1) * 3 - 2
pary%(i%) = pary%(i%) + RND(1) * 3 - 1
IF (parx%(i%) < 1 OR parx%(i%) > 319) THEN parx%(i%) = CINT(RND(1) * 319)
IF (pary%(i%) < 1 OR pary%(i%) > 199) THEN pary%(i%) = 1
IF (POINT(parx%(i%), pary%(i%)) = 0) THEN PSET (a1%, a2%), 0
PSET (parx%(i%), pary%(i%)), 13
PSET (parx%(i%) - 1, pary%(i%)), 0
PSET (parx%(i%) + 1, pary%(i%)), 0
PSET (parx%(i%), pary%(i%) - 1), 0
PSET (parx%(i%), pary%(i%) + 1), 0
NEXT
IF (INKEY$ <> "") THEN EXIT DO
WAIT &H3DA, 8
LOOP
END SUB

SUB ima.model4
DIM parx%(200), pary%(200)
DO
FOR i% = 0 TO UBOUND(parx%)
a1% = parx%(i%)
a2% = pary%(i%)
parx%(i%) = parx%(i%) + RND(1) * 3 - 2
pary%(i%) = pary%(i%) + RND(1) * 3 - 1
IF (parx%(i%) < 1 OR parx%(i%) > 319) THEN parx%(i%) = CINT(RND(1) * 319)
IF (pary%(i%) < 1 OR pary%(i%) > 199) THEN pary%(i%) = 1
IF (POINT(parx%(i%), pary%(i%)) = 0) THEN PSET (a1%, a2%), 0
PSET (parx%(i%), pary%(i%)), 12
NEXT
IF (INKEY$ <> "") THEN EXIT DO
WAIT &H3DA, 8
LOOP
END SUB

SUB ima.model5
DIM parx%(200), pary%(200)
DO
FOR i% = 0 TO UBOUND(parx%)
a1% = parx%(i%)
a2% = pary%(i%)
parx%(i%) = parx%(i%)' + RND(1) * 3 - 2
pary%(i%) = pary%(i%) + RND(1) * 4 - 1
IF (POINT(parx%(i%), pary%(i%)) <> 0) THEN
IF (POINT(parx%(i%), pary%(i%) + 1) <> 0) THEN
DO UNTIL POINT(parx%(i%), 1) = 0
parx%(i%) = CINT(RND(1) * 319)
LOOP
pary%(i%) = 1
PSET (parx%(i%), pary%(i%)), 11
END IF
ELSE
PSET (a1%, a2%), 0
END IF
PSET (parx%(i%), pary%(i%)), 11
NEXT
IF (INKEY$ <> "") THEN EXIT DO
WAIT &H3DA, 8
LOOP
END SUB

SUB ima.model6
DIM parx%(200), pary%(200)
DO
FOR i% = 0 TO UBOUND(parx%)
a1% = parx%(i%)
a2% = pary%(i%)
parx%(i%) = parx%(i%) + RND(1) * 3 - 2
pary%(i%) = pary%(i%) + RND(1) * 4 - 1
IF (POINT(parx%(i%), pary%(i%)) <> 0) THEN
IF (POINT(parx%(i%), pary%(i%) + 1) <> 0) THEN
parx%(i%) = RND(1) * 320
pary%(i%) = RND(1) * 1000
PSET (parx%(i%), pary%(i%)), 11
END IF
ELSE
PSET (a1%, a2%), 0
END IF
PSET (parx%(i%), pary%(i%)), 11
NEXT
IF (INKEY$ <> "") THEN EXIT DO
WAIT &H3DA, 8
LOOP
END SUB

SUB ima.model7
DO
WAIT &H3DA, 8
k% = (k% + 1) MOD 256
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, 0, 319, 199, k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, 0, 0, 199, k% * COS(i% * .01)
NEXT
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, 199, 319, 0, k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, 199, 0, 0, k% * COS(i% * .01)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model8
DO
WAIT &H3DA, 8
k% = (k% + 1) MOD 256
l% = (l% + 1) MOD 200
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, 0, 319, l% * COS(i% * .01), k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, 0, 0, l% * SIN(i% * .01), k% * COS(i% * .01)
NEXT
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, l% * COS(i% * .01), 319, 0, k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, l% * SIN(i% * .01), 0, 0, k% * COS(i% * .01)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB ima.model9
DO
WAIT &H3DA, 8
k% = (k% + 1) MOD 256
l% = (l% + 1) MOD 200
m% = (m% + 1) MOD 320
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, 0, m% * COS(i% * .01), l% * COS(i% * .01), k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, m% * SIN(i% * .01), 0, l% * SIN(i% * .01), k% * COS(i% * .01)
NEXT
FOR i% = 0 TO 320 STEP 5
support.bentline1 i%, l% * COS(i% * .01), m% * COS(i% * .01), 0, k% * SIN(i% * .01)
NEXT
FOR i% = 320 TO 0 STEP -5
support.bentline1 i%, l% * SIN(i% * .01), 0, m% * SIN(i% * .01), k% * COS(i% * .01)
NEXT
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB mig.model1
stp = .01
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
support.bentline1 1, 100, 1 + i& * SIN(k + 1), 100 + i& * SIN(k) * COS(k), SIN(k) * i&
NEXT
IF (k > 3 OR k < -1) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB mig.model2
stp = .05
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
support.bentline1 150, 100, 150 + i& * SIN(k), 100 + i& * COS(k), COS(k) * i&
NEXT
IF (k > 100 OR k < -100) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB mig.model3
stp = .05
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
support.bentline1 150, 100, 150 + i& * SIN(k), 100 + i& * SIN(k) * COS(k), SIN(k) * i&
NEXT
IF (k > 100 OR k < .105) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB mig.model4
stp = .01
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
support.bentline1 150, 100, 150 + i& * SIN(k), 100 + i& * COS(k) * LOG(k), LOG(k) * COS(k) * i&
NEXT
IF (k > 100 OR k < .101) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB pal.model1
stp = .1
k = k + stp
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(SIN(k) * COS(k) * (i& * j& * .005))
NEXT
NEXT
k = 0
stp% = 1
DO
k = k + stp!
OUT &H3C8, cl%
OUT &H3C9, CINT(SIN(k) * 63)
OUT &H3C9, CINT(COS(k) * 63)
OUT &H3C9, CINT(LOG(k) * 63)
WAIT &H3DA, 8
cl% = cl% + stp%
IF (cl% > 254 OR cl% < 1) THEN stp% = -stp%
IF (k > 30 OR k < .1) THEN stp! = -stp!
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB pal.model2
stp = .1
k! = k! + stp!
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(SIN(k!) * COS(k!) * (i& * j& * .005) * LOG(k!))
NEXT
NEXT
k% = 1
stp% = 1
DO
FOR cl% = 0 TO 255
k% = k% + stp%
OUT &H3C8, cl%
OUT &H3C9, CINT(SIN(k%) * 63)
OUT &H3C9, CINT(COS(k%) * 63)
OUT &H3C9, CINT(LOG(k%) * 63)
IF (k% > 300 OR k% < 2) THEN stp% = -stp%
NEXT
WAIT &H3DA, 8
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB pal.model3
stp = .1
k = k + stp
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(SIN(k) * COS(k) * (i& * j& * .005) * RND(1))
NEXT
NEXT
k = 0
stp% = 1
DO
k = k + stp!
OUT &H3C8, cl%
OUT &H3C9, CINT(SIN(k) * 63)
OUT &H3C9, CINT(COS(k) * 63)
OUT &H3C9, CINT(LOG(k) * 63)
WAIT &H3DA, 8
cl% = cl% + stp%
IF (cl% > 254 OR cl% < 1) THEN stp% = -stp%
IF (k > 30 OR k < .1) THEN stp! = -stp!
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB pal.model4
stp = .1
k = k + stp
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(SIN(k) * COS(k) * (i& * j& * .005) * EXP(SQR(k)) * LOG(k * k))
NEXT
NEXT
k = 0
stp% = 1
DO
k = k + stp!
OUT &H3C8, cl%
OUT &H3C9, CINT(SIN(k) * 63)
OUT &H3C9, CINT(COS(k) * 63)
OUT &H3C9, CINT(LOG(k) * 63)
WAIT &H3DA, 8
cl% = cl% + stp%
IF (cl% > 254 OR cl% < 1) THEN stp% = -stp%
IF (k > 30 OR k < .1) THEN stp! = -stp!
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB pal.model5
stp = .1
k = k + stp
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(RND(1) * ((i& * j&) / 100))
NEXT
NEXT
k = 0
stp% = 1
DO
k = k + stp!
OUT &H3C8, cl%
OUT &H3C9, CINT(SIN(k) * 63)
OUT &H3C9, CINT(COS(k) * 63)
OUT &H3C9, CINT(LOG(k) * 63)
WAIT &H3DA, 8
cl% = cl% + stp%
IF (cl% > 254 OR cl% < 1) THEN stp% = -stp%
IF (k > 30 OR k < .1) THEN stp! = -stp!
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB pal.model6
stp = .1
k = k + stp
FOR i& = 0 TO 319
FOR j& = 0 TO 199
PSET (i&, j&), CINT(INT(.5 + RND(1)) * i&)
NEXT
NEXT
k = 0
stp% = 1
DO
k = k + stp!
OUT &H3C8, cl%
OUT &H3C9, CINT(SIN(k) * 63)
OUT &H3C9, CINT(COS(k) * 63)
OUT &H3C9, CINT(LOG(k) * 63)
WAIT &H3DA, 8
cl% = cl% + stp%
IF (cl% > 254 OR cl% < 1) THEN stp% = -stp%
IF (k > 30 OR k < .1) THEN stp! = -stp!
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model1
stp = 1
k = 1
DO
k = k + stp
FOR i& = 0 TO 319
FOR j& = 98 TO 102
LINE -(SIN(i&) * k, COS(j&) * k * .705), LOG(i& + 1) * k
NEXT
NEXT
IF (k > 320 OR k < 2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model10
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + i& * COS(k), 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&)
PSET (150 + i& * SIN(k), 100 + i& * COS(k)), ABS(COS(k) * i&)
NEXT
IF (k > 300 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model11
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + i& * COS(k), 100 + i& * SIN(k)), ABS(COS(k) * i&)
PSET (150 + i& * SIN(k), 100 + i& * COS(k)), ABS(COS(k) * i&)
NEXT
IF (k > 300 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model12
stp = .5
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + i& * COS(k) * LOG(k + 1), 100 + i& * SIN(k) * LOG(k + 1)), ABS(COS(k) * i&)
PSET (150 + i& * SIN(k), 100 + i& * COS(k)), ABS(COS(k) * i&)
NEXT
IF (k > 3000 OR k < .5) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model13
stp = 5
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + i& * COS(k) * LOG(k + 1), 100 + i& * SIN(k) * LOG(k + 1)), ABS(COS(k) * i&)
PSET (150 + i& * SIN(k), 100 + i& * COS(k)), ABS(COS(k) * i&)
NEXT
IF (k > 3000 OR k < .5) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model14
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + i& * COS(k) - TAN(k + 1) * i&, 100 + i& * SIN(k) - TAN(k + 1) * i&), ABS(COS(k) * i&)
PSET (150 + i& * SIN(k) + COS(k) * i&, 100 + i& * COS(k) + SIN(k) * i&), ABS(COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model15
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + -TAN(k + 1) * i&, 100 + -TAN(k + 1) * i&), ABS(COS(k) * i&)
PSET (150 + i& * SIN(k) + COS(k) * SIN(k) * i&, 100 + i& * COS(k) + SIN(k) * COS(k) * i&), ABS(COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model2
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (i&, i& * LOG(k + 1)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model3
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (i&, 100 + i& * SIN(k + 1)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model4
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (i&, 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model5
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (150 + i& * SIN(k), 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model6
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (150 + i& * SIN(k) * COS(k), 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model7
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (15 + i& * LOG(k + 1), 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model8
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
CIRCLE (15 + i& * LOG(k + 1), 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&), ABS(SIN(k) * COS(k) * i&)
LINE -(15 + i& * SIN(k) * COS(k), 100 + i& * COS(k) * LOG(k + 1)), (COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB sew.model9
stp = .1
k = .1
DO
k = k + stp
FOR i& = 0 TO 319
PSET (150 + i& * COS(k), 100 + i& * SIN(k) * COS(k)), ABS(COS(k) * i&)
LINE -(150 + i& * SIN(k), 100 + i& * COS(k)), ABS(COS(k) * i&)
NEXT
IF (k > 30 OR k < .2) THEN stp = -stp
IF (INKEY$ <> "") THEN EXIT DO
LOOP
END SUB

SUB support.bentline1 (x1%, y1%, x2%, y2%, clr%)
IF (x2% - x1% <> 0) THEN
y! = (y2% - y1%) / (x2% - x1%)
cosstep! = -((11 / 7) / (x2% - x1%))
cs! = 11 / 7
FOR i% = x1% TO x2% STEP SGN(x2% - x1%)
PSET (i%, y1% + COS(cs!) * yy!), clr%
cs! = cs! + cosstep!
yy! = yy! + y!
NEXT
ELSEIF (y2% - y1% <> 0) THEN
y! = (x2% - x1%) / (y2% - y1%)
cosstep! = -((11 / 7) / (y2% - y1%))
cs! = 11 / 7
FOR i% = y1% TO y2% STEP SGN(y2% - y1%)
PSET (x1% + COS(cs!) * yy!, i%), clr%
cs! = cs! + cosstep!
yy! = yy! + y!
NEXT
ELSE
PSET (x1%, y1%), clr%
END IF
END SUB

