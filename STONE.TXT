'½ ¼ » º ¹ ¸ · ¶ µ ´ ³ ¾ ¿ À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö ×

DECLARE SUB box (s$, x1, y1, x2, y2, c1, c2, c3)
DECLARE SUB printer (s$, pr, sd)
DECLARE FUNCTION choice ()
DECLARE SUB attack (n, sd)
DECLARE SUB victory (sd)
DECLARE SUB clr ()
DECLARE SUB punish ()
DECLARE SUB messages (vic)
DECLARE SUB lose ()
DECLARE SUB win ()


CLS
box "Loading . . .", 1, 1, 50, 5, 14, 11, 10
LOCATE 4, 2
COLOR 3, 0
PRINT "Please wait while the game is loading . . ."


x1 = 4
x2 = 76
s = 1
FOR k = 1 TO 200
FOR l = 10 TO 12
LOCATE l, 1
PRINT SPC(x1 - 2);
LOCATE l, x2 + 1
PRINT SPC(x1 - 2);
NEXT
FOR i = x1 TO x2
COLOR 4
LOCATE 10, x1
PRINT "É";
LOCATE 11, x1
PRINT "º";
LOCATE 12, x1
PRINT "È";
FOR j = x1 + 1 TO x2 - 1
LOCATE 10, j
PRINT "Í";
LOCATE 11, j
COLOR 5
PRINT CHR$(177);
COLOR 4
LOCATE 12, j
PRINT "Í";
NEXT
LOCATE 10, x2
PRINT "»";
LOCATE 11, x2
PRINT "º";
LOCATE 12, x2
PRINT "¼";
NEXT
x1 = x1 + 1 * s
x2 = x2 - 1 * s
IF (ABS(x2 - x1) <= 4) THEN s = -1
IF (ABS(x2 - x1) >= 76) THEN s = 1
SOUND 21000, .02
NEXT
CLS

box "                        Stone-Paper-Scissor Game", 1, 1, 80, 25, 15, 7, 1
box "Player", 2, 5, 30, 21, 2, 3, 4
box "Computer", 52, 5, 79, 21, 2, 3, 4

box "message", 3, 8, 29, 12, 0, 1, 2
box "message", 53, 8, 78, 12, 0, 1, 2
box "money", 3, 12, 29, 16, 0, 1, 2
box "money", 53, 12, 78, 16, 0, 1, 2
box "choice", 3, 16, 29, 20, 0, 1, 2
box "choice", 53, 16, 78, 20, 0, 1, 2

box "Battle ground", 32, 6, 50, 20, 10, 11, 12

OPEN "i", #1, "money.txt"
LINE INPUT #1, z$
CLOSE #1
umon = VAL(z$)
cmon = VAL(z$)

'contrlos
printer "1=Stone", 1, 1
SOUND 21000, 20
printer "1=Stone", 1, 2
SOUND 21000, 20
printer "2=Paper", 1, 1
SOUND 21000, 20
printer "2=Paper", 1, 2
SOUND 21000, 20
printer "3=Scissor", 1, 1
SOUND 21000, 20
printer "3=Scissor", 1, 2
SOUND 21000, 20

printer STR$(umon) + "$", 2, 1
SOUND 21000, 20
printer STR$(cmon) + "$", 2, 2
SOUND 21000, 20

printer "Ready", 1, 1
SOUND 21000, 20
printer "Ready", 1, 2
SOUND 21000, 20
printer "Start", 1, 1
SOUND 21000, 20
printer "Start", 1, 2
SOUND 21000, 20

game:

'input
k$ = ""
DO UNTIL (k$ = "1" OR k$ = "2" OR k$ = "3" OR k$ = CHR$(27))
k$ = INKEY$
LOOP

'check key
IF (k$ = CHR$(27)) THEN GOTO quit
uchoice = VAL(k$)

'computer chooses
cchoice = choice
cheat = INT(RND * 20)
IF (cheat = 0) THEN cheat = 1 ELSE cheat = 0

'displaying choices
SELECT CASE uchoice
CASE 1
printer "Stone", 3, 1
CASE 2
printer "Paper", 3, 1
CASE 3
printer "Scissor", 3, 1
CASE ELSE
END SELECT
SELECT CASE cchoice
CASE 1
printer "Stone", 3, 2
CASE 2
printer "Paper", 3, 2
CASE 3
printer "Scissor", 3, 2
CASE ELSE
END SELECT
SOUND 21000, 10

attack uchoice, 1
attack cchoice, 2

FOR m = 400 TO 3000 STEP 500
SOUND m, .05
SOUND m + 3000, .05
NEXT

umon = umon - 5
cmon = cmon - 5
printer STR$(umon) + "$", 2, 1
SOUND 21000, 20
printer STR$(cmon) + "$", 2, 2
SOUND 21000, 20


'choosing victor
IF (uchoice = 1) THEN
        IF (cchoice = 1) THEN victor = 0
        IF (cchoice = 2) THEN victor = 2
        IF (cchoice = 3) THEN victor = 1
ELSEIF (uchoice = 2) THEN
        IF (cchoice = 1) THEN victor = 1
        IF (cchoice = 2) THEN victor = 0
        IF (cchoice = 3) THEN victor = 2
ELSEIF (uchoice = 3) THEN
        IF (cchoice = 1) THEN victor = 2
        IF (cchoice = 2) THEN victor = 1
        IF (cchoice = 3) THEN victor = 0
END IF

'check cheating
IF (cheat = 1) THEN victor = 1
IF (cheat = 1) THEN
punish
cmon = cmon - 10
printer STR$(umon) + "$", 2, 1
SOUND 21000, 20
printer STR$(cmon) + "$", 2, 2
SOUND 21000, 20
END IF
'applying victory
victory victor

FOR m = 2400 TO 5000 STEP 500
SOUND m, .05
SOUND m + 3000, .05
NEXT

clr
IF (victor = 1) THEN umon = umon + 10
IF (victor = 2) THEN cmon = cmon + 10
printer STR$(umon) + "$", 2, 1
SOUND 21000, 20
printer STR$(cmon) + "$", 2, 2
SOUND 21000, 20

messages victor            'messages to each other

'checking looser
IF (umon <= 0) THEN
lose                   'lost game
GOTO quit
ELSEIF (cmon <= 0) THEN
win                     'win game
GOTO quit
END IF
GOTO game

quit:
CLS
PRINT "Credits   :    Subhajit Sahu"
SOUND 21000, 20
SYSTEM

SUB attack (n, sd)

SELECT CASE sd
CASE 1
x = 40
y = 10
CASE 2
x = 40
y = 15
CASE ELSE
END SELECT

SELECT CASE n
CASE 3
COLOR 13
LOCATE y, x
PRINT "º  º";
LOCATE y + 1, x
PRINT "º  º";
LOCATE y + 2, x
PRINT "È»É¼";
LOCATE y + 3, x
PRINT "Ì¼È¹";
CASE 2
COLOR 7
LOCATE y, x
PRINT "ÉÍÍ»";
LOCATE y + 1, x
PRINT "º  º";
LOCATE y + 2, x
PRINT "º  º";
LOCATE y + 3, x
PRINT "ÈÍÍ¼";
CASE 1
COLOR 1
LOCATE y, x
PRINT "Õ·";
LOCATE y + 1, x
PRINT "ÇÈÑ»";
LOCATE y + 2, x
PRINT "¶ÆÎÇ";
LOCATE y + 3, x
PRINT "ÈÏÍ½";
CASE ELSE
END SELECT
END SUB

SUB box (s$, x1, y1, x2, y2, c1, c2, c3)

'½ ¼ » º ¹ ¸ · ¶ µ ´ ³ ¾ ¿ À Á Â Ã Ä Å Æ Ç È É Ê Ë Ì Í Î Ï Ð Ñ Ò Ó Ô Õ Ö ×



FOR i = 1 TO x2 - x1 - 2
        COLOR c3, 0
        LOCATE y1, x1
        PRINT "É";
        LOCATE y1 + 1, x1
        PRINT "º";
        LOCATE y1 + 2, x1
        PRINT "Ì";

                FOR j = 1 TO i
                SOUND 21000, .03
                COLOR c3, 0
                LOCATE y1, x1 + j
                PRINT "Í";
                LOCATE y1 + 2, x1 + j
                PRINT "Í";
                COLOR c1, c2
                LOCATE y1 + 1, x1 + j
                PRINT " ";
                NEXT
        COLOR c3, 0
        LOCATE y1, x1 + i + 1
        PRINT "»";
        LOCATE y1 + 1, x1 + i + 1
        PRINT "º";
        LOCATE y1 + 2, x1 + i + 1
        PRINT "¹";
        LOCATE y1 + 3, x1
        PRINT "È";
        FOR k = x1 + 1 TO x1 + i
        PRINT "Í";
        NEXT
        PRINT "¼";
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
                PRINT "º"; SPC(d); "º";
        NEXT
        LOCATE i, x1
        PRINT "È";
        FOR k = 1 TO d
                PRINT "Í";
        NEXT
        PRINT "¼";
NEXT

END SUB

FUNCTION choice
RANDOMIZE TIMER
a = VAL(RIGHT$(TIME$, 2))
a = ((RND * 10 * a) MOD 3) + 1
choice = a
END FUNCTION

SUB clr
COLOR , 0
FOR i = 10 TO 19
LOCATE i, 34
FOR j = 34 TO 48
PRINT " ";
NEXT
NEXT
END SUB

SUB lose
CLS
box "You lose!", 1, 1, 40, 20, 1, 2, 3
LOCATE 4, 3
COLOR 5
PRINT "Computer: You foolish looser";
LOCATE 5, 3
PRINT "          I win,i win,i win.";
SOUND 21000, 50
END SUB

SUB messages (vic)
uans = INT(RND * 5) + 1
cans = INT(RND * 5) + 1

IF (vic = 1) THEN
        SELECT CASE uans
        CASE 1
        u$ = "Yoohoo!"
        CASE 2
        u$ = "Yaah!"
        CASE 3
        u$ = "Unbelievebable!"
        CASE 4
        u$ = "Thats right."
        CASE 5
        u$ = "Done it!"
        CASE ELSE
        END SELECT
        SELECT CASE cans
        CASE 1
        c$ = "You idoit!"
        CASE 2
        c$ = "You rascal!"
        CASE 3
        c$ = "You mental!"
        CASE 4
        c$ = "You doggie!"
        CASE 5
        c$ = "You nonsense!"
        CASE ELSE
        END SELECT
ELSEIF (vic = 2) THEN
SELECT CASE uans
CASE 1
u$ = "Oh no!"
CASE 2
u$ = "Ooof!"
CASE 3
u$ = "Impossible!"
CASE 4
u$ = "Next time."
CASE 5
u$ = "No more!"
CASE ELSE
END SELECT
SELECT CASE cans
CASE 1
c$ = "Ha ha ha!"
CASE 2
c$ = "You loser!"
CASE 3
c$ = "You ant!"
CASE 4
c$ = "He he i win!"
CASE 5
c$ = "Lose lose!"
CASE ELSE
END SELECT
END IF

IF (vic = 1) THEN
printer u$, 1, 1
SOUND 21000, 20
printer c$, 1, 2
SOUND 21000, 40
ELSE
printer c$, 1, 2
SOUND 21000, 20
printer u$, 1, 1
SOUND 21000, 40
END IF
END SUB

SUB printer (s$, pr, sd)
SELECT CASE sd
CASE 1
        SELECT CASE pr
        CASE 1
        x1 = 4
        x2 = 28
        y = 11
        CASE 2
        x1 = 4
        x2 = 28
        y = 15
        CASE 3
        x1 = 4
        x2 = 28
        y = 19
        CASE ELSE
        END SELECT
CASE 2
        SELECT CASE pr
        CASE 1
        x1 = 54
        x2 = 77
        y = 11
        CASE 2
        x1 = 54
        x2 = 77
        y = 15
        CASE 3
        x1 = 54
        x2 = 77
        y = 19
        CASE ELSE
        END SELECT
CASE ELSE
END SELECT

COLOR , 0
LOCATE y, x1
FOR i = x1 TO x2
PRINT " ";
NEXT
LOCATE y, x1
PRINT s$;
END SUB

SUB punish
clr
FOR i = 10000 TO 1000 STEP 1000
SOUND i, .05
SOUND i * .5, .05
NEXT
COLOR 3
LOCATE 10, 35
PRINT "Computer";
SOUND 21000, 20
clr
LOCATE 10, 35
PRINT "You have cheated";
SOUND 21000, 20
clr
LOCATE 10, 35
PRINT "How do you";
SOUND 21000, 20
clr
LOCATE 10, 35
PRINT "Dare to do so";
SOUND 21000, 20
clr
LOCATE 10, 35
PRINT "Pay Fine now";
SOUND 21000, 20
clr
END SUB

SUB victory (sd)
a$ = CHR$(240)
x1 = 34
x2 = 48
SELECT CASE sd
CASE 2
y1 = 10
y2 = 14
CASE 1
y1 = 15
y2 = 19
CASE ELSE
y1 = 10
y2 = 19
END SELECT

COLOR 6
FOR i = y1 TO y2
LOCATE i, x1
FOR j = x1 TO x2
PRINT a$;
SOUND 21000, .1
NEXT
NEXT
END SUB

SUB win
CLS
box "You Win!", 1, 1, 40, 20, 1, 2, 3
LOCATE 4, 3
COLOR 5
PRINT "Computer: You idiot,";
LOCATE 5, 3
PRINT "          How dare you win, idiot.";
SOUND 21000, 50

END SUB

