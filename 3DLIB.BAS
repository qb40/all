'$INCLUDE: '3_d.bi'

'':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
'':::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
SUB ProcessLight2 (a AS INTEGER, B AS INTEGER, c AS INTEGER, d AS INTEGER)

'A and B are borders of the lightsource aray
'C and D are borders of the PolgyonS aray

FOR E = c TO d
  PolygonS(E).R = 10
  PolygonS(E).G = 0
  PolygonS(E).B = 0
  N = PolygonS(E).NextPoint
  WHILE N <> 0
    PolygonN(N).R = 10
    PolygonN(N).G = 0
    PolygonN(N).B = 0
    N = PolygonN(N).NextPoint
  WEND
  FOR F = a TO B
    PolygonS(E).Distance = Distance3(PointsOutput(PolygonS(E).PPoint).X - Lightsource(F).X, PointsOutput(PolygonS(E).PPoint).Y - Lightsource(F).Y, PointsOutput(PolygonS(E).PPoint).Z - Lightsource(F).Z)
    counter = 1
    Afstand = PolygonS(E).Distance
    N = PolygonS(E).NextPoint
    WHILE N <> 0
      PolygonN(N).Distance = Distance3(PointsOutput(PolygonN(N).PPoint).X - Lightsource(F).X, PointsOutput(PolygonN(N).PPoint).Y - Lightsource(F).Y, PointsOutput(PolygonN(N).PPoint).Z - Lightsource(F).Z)
      counter = counter + 1
      Afstand = Afstand + PolygonN(N).Distance
      N = PolygonN(N).NextPoint
    WEND
  
    Afstand = ABS(Afstand / counter)
    temp = ABS(Afstand - PolygonS(E).Distance)
   
    temp2 = 255 - Lightsource(F).R * temp
    IF temp2 > 255 THEN temp2 = 255

    PolygonS(E).R = temp2  'PolygonS(E).R *
    N = PolygonS(E).NextPoint
   
    WHILE N <> 0
  
      temp = ABS(Afstand - PolygonN(N).Distance)
  
      temp2 = 255 - Lightsource(F).R * temp

      PolygonN(N).R = temp2  'PolygonN(N).R *

      N = PolygonN(N).NextPoint
    WEND
  NEXT F
NEXT E

END SUB

