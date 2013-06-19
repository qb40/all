''                          ____            ____
''                         /~~~~\          \~~~~\
''                         `\/~~>)   \|/    )/~~.)
''                            <(    - o -   ||  ||
''                         ./\__>)   /|\    )|__')
''                         \____/   _ _ _  /____/
''                          ~~~~    L I B   ~~~~
''                                  ~ ~ ~
''                              version 1E-32a
''                            (c) 2000 by Badjas
''
'sorry... this is still in Dutch... ((its still version 0.1))
'well... some is in Enlgish as well, I see...
'Because this is still version 0.1 some variables in the types are not used.
'you could use it yourself, because the ASM routines dont use it, but
'they DO need them at the spot, because of memory position calculations (I guess, this is more V1ctors doing :)
'
'Credits:
'
'Code and example program by Badjas
'Converted to ASM by V1ctor
'And thanks to Angelo for the DirectQB stuff
'
'Next build will include some stuff where QB-MRK helped me out a bit :)
'for example some decent light routines.
'
'oh, before I forget: Thanks to V1ctor for the 3*D ascii logo :) I like it.
'
'If you need any help, wait for a later build where I will include some better help (well, cant promise this, can I? you dont buy it!)
'But if you need to know the workings of the library, study the example program. it isnt really hard to use. (that was some goal actualy)
'
'Greetings,
'Badjas

'constant(s)

CONST PI = 3.1415926535#   'constants, also used by the engine


'type definitions

TYPE LoosePointType
  X AS INTEGER             'x punt
  Y AS INTEGER             'y punt
  Z AS INTEGER             'z punt
END TYPE

TYPE ObjectType           'Algemeen object, als een aaneenhangend geheel
  VerticalAngle AS SINGLE 'Hoek die het voorwerp maakt over de verticale as
  WidthAngle AS SINGLE    'hoek van het voorwerp over de breedte as
  DepthAngle AS SINGLE    'hoek van het voorwerp over de diepte as
  X AS SINGLE             'x punt
  Y AS SINGLE             'y punt
  Z AS SINGLE             'z punt
  OutputX AS INTEGER      'x punt gedraaid
  OutputY AS INTEGER      'y punt gedraaid
  OutputZ AS INTEGER      'z punt gedraaid
  OutputDepthAngle AS SINGLE 'aangepaste hoek
END TYPE

TYPE PointType            'een punt van een polygoon
  X AS SINGLE             'x punt
  Y AS SINGLE             'y punt
  Z AS SINGLE             'z punt
  Object AS INTEGER       'Pointer naar de index van de objecten
END TYPE

TYPE LightSourceType      'een lichtbron
  X AS SINGLE             'x punt
  Y AS SINGLE             'y punt
  Z AS SINGLE             'z punt
  R AS INTEGER            'percentage rood
  G AS INTEGER            'percentage groen
  B AS INTEGER            'percentage blauw
  Strength AS INTEGER     'Bereik van het licht. (Strength afstand = 1 % licht, hopelijk)
END TYPE

TYPE PolygonPointType     'een beginpunt van een polygoon
  PPoint AS INTEGER       'pointer naar een index in de array van points
  X AS SINGLE             'x punt van centrum
  Y AS SINGLE             'y punt
  Z AS SINGLE             'z punt
  Distance AS SINGLE      'V1CTOR, GET RID OF THIS! :) (well... maybe leave it out when I am sure :)
  OutputX AS INTEGER
  OutputY AS INTEGER
  OutputZ AS INTEGER
  R AS INTEGER            'percentage rood
  G AS INTEGER            'percentage groen
  B AS INTEGER            'percentage blauw
  NextPoint AS INTEGER    'pointer naar het volgende punt op de polygoon
  ShinySurface AS INTEGER 'Of het opervlak speigeld
END TYPE

TYPE PolygonNextPointType 'geen beginpunt maar een ander punt van een polygoon
  PPoint AS INTEGER       'pointer naar een index in de array van points
  Distance AS SINGLE      'afstand centrum tot punt
  R AS INTEGER            'percentage rood
  G AS INTEGER            'percentage groen
  B AS INTEGER            'percentage blauw
  NextPoint AS INTEGER    'pointer naar het volgende punt op de polygoon
END TYPE

TYPE Person               'De viewport
  X AS SINGLE             'x punt
  Y AS SINGLE             'y punt
  Z AS SINGLE             'z punt
  VerticalAngle AS SINGLE 'Hoek die het voorwerp maakt over de verticale as
  WidthAngle AS SINGLE    'hoek van het voorwerp over de breedte as
  DepthAngle AS SINGLE    'hoek van het voorwerp over de diepte as
END TYPE


'global variabeles

COMMON SHARED viewport AS Person                 'definitie van de viewport opgeslagen
COMMON SHARED objects() AS ObjectType            'aantal = aantal objecten
COMMON SHARED LightSource() AS LightSourceType   'aantal = aantal lichtbronnen
COMMON SHARED polygonS() AS PolygonPointType     'aantal = aantal polygonen
COMMON SHARED PolygonN() AS PolygonNextPointType 'aantal = aantal hoeken van een alle polygonen - aantal polygonen
COMMON SHARED points() AS PointType              'aantal = aantal punten (dubbele punten weggelaten)
COMMON SHARED PointsOutput() AS LoosePointType   'Uitvoer van de engine zodat de orginele waarden bewaard blijven
COMMON SHARED drawOrder() AS INTEGER             'Volgorde van tekenen (z-sort) van de POLYGONEN, aantal = dus aantal polygonen
COMMON SHARED minDistance AS SINGLE              'Field of view. used in ProcessSort


'declares

'ASM version
''
'' functions prototypes for math.obj
''
DECLARE FUNCTION ArcTan! (BYVAL deltax AS INTEGER, BYVAL deltay AS INTEGER)
DECLARE FUNCTION Distance2! (BYVAL deltax AS INTEGER, BYVAL deltay AS INTEGER)
DECLARE FUNCTION Distance3! (BYVAL deltax AS INTEGER, BYVAL deltay AS INTEGER, BYVAL deltaz AS INTEGER)
DECLARE SUB TurnOnAngle (deltax AS INTEGER, deltay AS INTEGER, BYVAL turnAngle AS SINGLE)

''
'' functions prototypes for process.obj
''
DECLARE SUB ProcessObjects (SEG objects AS ObjectType, BYVAL frstObj AS INTEGER, BYVAL lastObj AS INTEGER, SEG viewport AS Person)

DECLARE SUB ProcessPerspective (SEG pntsOutp AS LoosePointType, BYVAL frstPnt AS INTEGER, BYVAL lastPnt AS INTEGER)

DECLARE SUB ProcessPoints (SEG points AS PointType, SEG pntsOutp AS LoosePointType, SEG objects AS ObjectType, BYVAL frstPnt AS INTEGER, BYVAL lastPnt AS INTEGER)

DECLARE SUB ProcessPolygons (SEG points AS PointType, SEG polygonS AS PolygonPointType, SEG objects AS ObjectType, BYVAL frstPnt AS INTEGER, BYVAL lastPnt AS INTEGER, SEG viewport AS Person)

DECLARE SUB ProcessSort (polygonS() AS PolygonPointType, BYVAL minDistance AS SINGLE, SEG drawOrder AS INTEGER)

DECLARE SUB ProcessViewport (SEG points AS PointType, SEG pntsOutp AS LoosePointType, SEG objects AS ObjectType, BYVAL frstPnt AS INTEGER, BYVAL lastPnt AS INTEGER, SEG viewport AS Person)

