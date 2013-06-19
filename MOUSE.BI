'successful test
DECLARE SUB mouse.loadprog ()
DECLARE FUNCTION mouse.init% ()
DECLARE SUB mouse.show ()
DECLARE SUB mouse.start2 ()
DECLARE SUB mouse.show2 ()
DECLARE SUB mouse.start3 ()
DECLARE SUB mouse.show3 ()
DECLARE SUB mouse.hide ()
DECLARE SUB mouse.setrange (byval x1%,byval  y1%,byval  x2%,byval  y2%)
DECLARE SUB mouse.put (byval x%,byval  y%)
DECLARE SUB mouse.status ()
DECLARE SUB mouse.relativestatus ()

DECLARE FUNCTION dat.datum% (fl1$, pos1&)
DECLARE SUB dat.loaddata (fl1$, pos1&, pos2&, segment&)

TYPE mouse
        left AS INTEGER
        right AS INTEGER
        oldleft AS INTEGER
        oldright AS INTEGER
        xpos AS LONG
        ypos AS LONG
        oldxpos AS LONG
        oldypos AS LONG
        mousetype AS INTEGER
        mouseattrib AS INTEGER
        virtualattrib AS INTEGER
END TYPE
DIM Jerry AS mouse
mouse$ = ""
Jerry.mousetype = 127
Jerry.mouseattrib = 20
Jerry.virtualattrib = 20

TYPE filestring
        byte AS STRING * 1
END TYPE
DIM file AS filestring
