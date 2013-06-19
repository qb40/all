'==========================================
' EARTHQUAKE by William Yu (07-23-94)
' Creates an Earthquake effect
' Works on any graphics mode
' Here's a short demo, modify as you wish
'==========================================

DECLARE SUB EarthQuake ()
DIM CRAFT(500)
CLS
SCREEN 12
LINE (100, 100)-(150, 100), 10
CIRCLE (100, 100), 140, 10
k$ = INPUT$(1)
FOR i = 1 TO 100
'OUT &H3D4, 9: OUT &H3D5, 1
EarthQuake
NEXT
OUT &H3D4, 8: OUT &H3D5, 0
LINE (0, 0)-(100, 0)

SUB EarthQuake

Delay = 5500       ' Increase this or decrease for earthquake delay

FOR X = 1 TO Delay
  OUT &H3D4, 8: OUT &H3D5, X
NEXT X
END SUB

