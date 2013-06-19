'
'                               V F o n t  3
'
'Hi. This is the third version of VFont. Loads of people (well, 3 actually)
'wanted multispacing, and custom fonts.
'
'Unfortunatly, this can no longer read fonts straight from the BIOS, so you
'must have at least one font file for this to work. However, I have made some
'example fonts and a font editor for you to make more.
'
'Sub/Function list
'=-=-=-=-=-=-=-=-=
'
'VLen:
'
'   Declaration:
'
'       DECLARE FUNCTION VLen% (Text$, FontArray%())
'
'   Parameters:
'
'       Text$ - Text to return length of
'       FontArray%() - Font that Text$ is in
'
'   Function:
'
'       Returns pixel length of text Text$ in font FontArray%()
'
'   Notes:
'
'       This returns the number of pixels this string takes up (in font
'       FontArray%()), not the number of characters in it (think about it: it
'       would be useless if it did because that wouldn't take the multispacing
'       into account, and anyway then you could use LEN(Text$)).
'
'VFont:
'
'   Declaration:
'
'       DECLARE SUB VFont (XPos%, YPos%, Text$, Col%, BCol%, Style%, _
'                           FontArray%())
'   Parameters:
'
'       XPos%, YPos% - Pixel coords for text
'       Text$ - String to print
'       Col% - Main colour of text
'       BCol% - Secondary colour - Shadow colour
'       Style% - Style of text
'       FontArray%() - Font array for text
'
'   Function:
'
'       Writes Col%- and BCol%-coloured Text$ at XPos%, YPos% in FontArray%()
'       font with style Style%.
'
'   Notes:
'
'       FontArray%() *MUST* be dimensioned exactly from 0 TO 255,0 TO 16. See
'       below about font arrays. Use the fonts supplied with VFont3 or make
'       your own with VFontEdit. To centre text, use -32767 as the X position
'       and set ScreenWidth to the width of the screen somewhere before you
'       centre text with VFont. Also, you need the line
'
'           DIM SHARED ScreenWidth AS INTEGER
'
'       in your program before you set ScreenWidth.
'
'       To use the Shaded style, supply the top colour of the letter as the
'       colour. If you want use Shaded + Shadowed, the BCol is the top colour
'       of the shadow. If you use Shadowed and not Shaded then the BCol is the
'       shadow colour.
'
'VLoadFont:
'
'   Declaration:
'
'       DECLARE SUB VLoadFont (File$, Array%())
'
'   Parameters:
'
'       File$ - File to load
'       Array%() - Font array to load into
'
'   Function:
'
'       Loads File$ into Array%()
'
'   Notes:
'
'       Array%() *MUST* be dimensioned exactly from 0 TO 255,0 TO 16. See
'       below about font arrays. Use the fonts supplied with VFont3 or make
'       your own with VFontEdit.
'
'Structure of a VFont array
'=-=-=-=-=-=-=-=-=-=-=-=-=-
'
'Each font is stored in an array dimension thus:
'
'DIM SHARED FontBios(255,16):
'
'0-255 in the first section is each character.
'0 in the second is the height of the character.
'1-16 is each line stored like a line in the BIOS font memory.
'
'Structure of a VFont file
'=-=-=-=-=-=-=-=-=-=-=-=-=
'
'A VFont font file is just a font array BSaved.
'
'Legal Crap
'=-=-=-=-=-
'
'Copyright (c) 1999-2000 Sam Thursfield
'
'YOU CAN USE THIS CODE IN YOUR OWN PROGRAMS IF YOU WISH, BUT GIVE ME CREDIT
'SOMEWHERE. THE FONTS AND EDITOR PROVIDED WITH THIS FILE CAN BE FREELY
'DISTRIBUTED. IF THIS DOES ANY HARM TO YOU, YOUR COMPUTER, ANYTHING THEN I,
'THE AUTHOR, SAM THURSFIELD, CANNOT BE HELD RESPONSIBLE. DO NOT CONVERT THIS
'TO LOWER CASE, IT IS THE CURRENT TREND FOR ALL THE LEGAL STUFF TO BE IN
'CAPITAL LETTERS.
'
'To use this in your programs, put a line in the credits such as:
'   Font: Sam Thursfield (sam.thursfield@btinternet.com)
'
'All this by Sam Thursfield (sam.thursfield@btinternet.com)
'5 December 1999-9 January 2000
'

'$DYNAMIC
DEFINT A-Z

DECLARE SUB VFont (Xpos%, YPos%, Text$, Col%, BCol%, Style%, FontArray%())
DECLARE FUNCTION VLen% (Text$, FontArray%())
DECLARE SUB VLoadFont (File$, Array%())

CONST Shaded = 1
CONST Italic = 2
CONST Shadowed = 4
CONST Underlined = 8
CONST Centered = -32767

DIM SHARED ScreenWidth AS INTEGER

DIM SHARED FontDefault(255, 16) AS INTEGER
DIM SHARED FontThin(255, 16) AS INTEGER
DIM SHARED FontLines(255, 16) AS INTEGER

'=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
'Begin Demo Code                                                             '
'=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

SCREEN 12                   'Screen 12 so we can fit more on the screen
ScreenWidth = 640

PRINT "Loading fonts...";
VLoadFont "DEFAULT.FNT", FontDefault()
VLoadFont "THIN.FNT", FontThin()
VLoadFont "LINES.FNT", FontLines()
PRINT "done"

LINE (0, 0)-(639, 15), 1, BF
LINE (0, 15)-(639, 463), 7, BF
LINE (0, 464)-(639, 479), 4, BF
VFont Centered, 0, "VFont3 - Multispaced fonts in QBasic", 15, 9, Shadowed + Italic, FontDefault()
VFont Centered, 464, "By Sam Thursfield - sam.thursfield@btinternet.com", 15, 12, Shadowed, FontDefault()

VFont 2, 17, "Hi. This is the demo of VFont3. You've probably already noticed the new features:", 0, 0, 0, FontThin()
VFont 2, 41, "     Multispacing", 0, 0, 0, FontLines()
VFont 2, 57, "     Custom Fonts", 0, 0, 0, FontLines()
VFont 2, 73, "     4 supplied fonts", 0, 0, 0, FontLines()
VFont 2, 97, "Wow. Big list. Now, you probably know how badly I write documentation and how much I hate", 0, 0, 0, FontThin()
VFont 2, 113, "documenting things. Well, I had to here, so you'll just have to put up with it.", 0, 0, 0, FontThin()
VFont 2, 145, "Sub/Functions List:", 0, 0, Underlined, FontLines()
VFont 34, 177, "VLen:", 0, 0, Underlined, FontDefault()
VFont 66, 209, "Declaration:", 0, 0, Underlined, FontThin()
VFont 98, 241, "DECLARE FUNCTION VLen% (Text$, FontArray%())", 0, 0, 0, FontThin()
VFont 66, 273, "Parameters:", 0, 0, Underlined, FontThin()
VFont 98, 305, "Text$ - String to return length of", 0, 0, 0, FontThin()
VFont 98, 321, "FontArray%() - Font that Text$ is in", 0, 0, 0, FontThin()
VFont 66, 353, "Function:", 0, 0, Underlined, FontThin()
VFont 98, 385, "Returns pixel length of text Text$ in font FontArray%()", 0, 0, 0, FontThin()
VFont Centered, 417, "Press a key...", 0, 0, 0, FontLines()
SLEEP
LINE (0, 15)-(639, 463), 7, BF
VFont 66, 17, "Notes:", 0, 0, Underlined, FontThin()
VFont 98, 49, "This returns the number of pixels this string takes up (in font", 0, 0, 0, FontThin()
VFont 98, 65, "FontArray%()), not the number of characters in it (think about it: it", 0, 0, 0, FontThin()
VFont 98, 81, "would be useless if it did because that wouldn't take the multispacing", 0, 0, 0, FontThin()
VFont 98, 97, "into account, and anyway then you could use LEN(Text$)).", 0, 0, 0, FontThin()
VFont 34, 129, "VFont:", 0, 0, Underlined, FontDefault()
VFont 66, 161, "Declaration:", 0, 0, Underlined, FontThin()
VFont 98, 193, "DECLARE SUB VFont (XPos%, YPos%, Text$, Col%, BCol%, Style%, FontArray%())", 0, 0, 0, FontThin()
VFont 66, 225, "Parameters:", 0, 0, Underlined, FontThin()
VFont 98, 257, "XPos%, YPos% - Pixel coords for text", 0, 0, 0, FontThin()
VFont 98, 273, "Text$ - String to print", 0, 0, 0, FontThin()
VFont 98, 289, "Col% - Main colour of text", 0, 0, 0, FontThin()
VFont 98, 305, "BCol% - Secondary colour - Shadow colour", 0, 0, 0, FontThin()
VFont 98, 321, "Style% - Style of text", 0, 0, 0, FontThin()
VFont 98, 337, "FontArray%() - Font array for text", 0, 0, 0, FontThin()
VFont 66, 369, "Function:", 0, 0, Underlined, FontThin()
VFont Centered, 417, "Press a key...", 0, 0, 0, FontLines()
SLEEP
LINE (0, 15)-(639, 463), 7, BF
VFont 98, 17, "Writes Col%- and BCol%-coloured Text$ at XPos%, YPos% in FontArray%()", 0, 0, 0, FontThin()
VFont 98, 33, "font with style Style%.", 0, 0, 0, FontThin()
VFont 66, 65, "Notes:", 0, 0, Underlined, FontThin()
VFont 98, 97, "FontArray%() *MUST* be dimensioned exactly from 0 TO 255, 0 TO 16. See", 0, 0, 0, FontThin()
VFont 98, 113, "below about font arrays. Use the fonts supplied with VFont3 or make", 0, 0, 0, FontThin()
VFont 98, 129, "your own with VFontEdit. To centre text, use -32767 as the X position", 0, 0, 0, FontThin()
VFont 98, 145, "and set ScreenWidth to the width of the screen somewhere before you", 0, 0, 0, FontThin()
VFont 98, 161, "centre text with VFont. Also, you need the line", 0, 0, 0, FontThin()
VFont 194, 193, "DIM SHARED ScreenWidth AS INTEGER", 0, 0, 0, FontDefault()
VFont 98, 225, "in your program before you set ScreenWidth.", 0, 0, 0, FontThin()
VFont 98, 241, "To use the Shaded style, supply the top colour of the letter as the", 0, 0, 0, FontThin()
VFont 98, 257, "colour. If you want use Shaded + Shadowed, the BCol is the top colour", 0, 0, 0, FontThin()
VFont 98, 273, "of the shadow. If you use Shadowed and not Shaded then the BCol is the", 0, 0, 0, FontThin()
VFont 98, 289, "shadow colour.", 0, 0, 0, FontThin()
VFont 34, 321, "VLoadFont:", 0, 0, Underlined, FontDefault()
VFont 66, 353, "Declaration:", 0, 0, Underlined, FontThin()
VFont 98, 385, "DECLARE SUB VLoadFont (File$, Array%())", 0, 0, 0, FontThin()
VFont Centered, 417, "Press a key...", 0, 0, 0, FontLines()
SLEEP
LINE (0, 15)-(639, 463), 7, BF
VFont 66, 17, "Parameters:", 0, 0, Underlined, FontThin()
VFont 98, 49, "File$ - File to load", 0, 0, 0, FontThin()
VFont 98, 65, "Array%() - Font array to load into", 0, 0, 0, FontThin()
VFont 66, 97, "Function:", 0, 0, Underlined, FontThin()
VFont 98, 129, "Loads File$ into Array%()", 0, 0, 0, FontThin()
VFont 66, 161, "Notes:", 0, 0, Underlined, FontThin()
VFont 98, 193, "Array%() *MUST* be dimensioned exactly from 0 TO 255, 0 TO 16. Use the fonts", 0, 0, 0, FontThin()
VFont 98, 209, "supplied with VFont3 or make your own with VFontEdit.", 0, 0, 0, FontThin()
VFont 2, 241, "There are 8 different styles you can use as the Style parameter. They're numbers, but are also", 0, 0, 0, FontThin()
VFont 2, 257, "defined as constants:", 0, 0, 0, FontThin()
VFont 34, 289, "CONST Shaded = 1", 15, 0, Shaded, FontDefault()
VFont 34, 305, "CONST Italic = 2", 0, 0, Italic, FontDefault()
VFont 34, 321, "CONST Shadowed = 4", 0, 0, Shadowed, FontDefault()
VFont 34, 337, "CONST Underlined = 8", 0, 7, Underlined, FontDefault()
VFont 2, 369, "Shaded looks crap there, but in more colours it's good.", 0, 0, 0, FontThin()
VFont Centered, 417, "Press a key...", 0, 0, 0, FontLines()
SLEEP
SCREEN 13
ScreenWidth = 320
LINE (0, 0)-(319, 15), 1, BF
LINE (0, 15)-(319, 182), 7, BF
LINE (0, 183)-(319, 199), 4, BF
VFont Centered, 0, "VFont3 - Multispaced fonts in QBasic", 15, 9, Shadowed + Italic, FontDefault()
VFont Centered, 184, "By Sam Thursfield - sam.thursfield@btinternet.com", 15, 12, Shadowed, FontDefault()
VFont 2, 17, "If you think this font looks too big in this mode,", 0, 0, 0, FontThin()
VFont 2, 33, "that's probably because it is. There is an 8*8", 0, 0, 0, FontThin()
VFont 2, 49, "font (BIOS8.FNT), but I can't be bothered to load", 0, 0, 0, FontThin()
VFont 2, 65, "it. Anyway, we're in this mode to test fonts:", 0, 0, 0, FontThin()
VFont 34, 97, "This is shaded and shadowed...", 32, 30, Shaded + Shadowed, FontLines()
VFont 2, 145, "Wow. This mode is cramped for fonts this size!", 0, 0, 0, FontThin()
VFont Centered, 167, "Press a key...", 0, 0, 0, FontLines()
SLEEP
SCREEN 12
ScreenWidth = 80
LINE (0, 0)-(639, 15), 1, BF
LINE (0, 15)-(639, 463), 7, BF
LINE (0, 464)-(639, 479), 4, BF
VFont Centered, 0, "VFont3 - Multispaced fonts in QBasic", 15, 9, Shadowed + Italic, FontDefault()
VFont Centered, 464, "By Sam Thursfield - sam.thursfield@btinternet.com", 15, 12, Shadowed, FontDefault()
VFont 2, 17, "There. Looks better in that mode, doesn't it? I'll answer that for you - yes ;^)", 0, 0, 0, FontThin()
VFont 2, 33, "I've just about run out of things to say, apart from read the comments in this program and make", 0, 0, 0, FontThin()
VFont 2, 49, "your own fonts with VFontEdit. If you think you've made a really good font, email it to me at", 0, 0, 0, FontThin()
VFont 2, 65, "sam.thursfield@btinternet.com. Send me comments as well. I like email! Also, look at my website", 0, 0, 0, FontThin()
VFont 2, 81, "which is at epsite.hypermart.net.", 0, 0, 0, FontThin()
VFont 2, 113, "Lastly, here are some of the features for VFont4:", 0, 0, 0, FontThin()
VFont 2, 129, "More fonts (obviously), a better editor, and dynamic style changes. These are when you can put codes", 0, 0, 0, FontThin()
VFont 2, 145, "in your text such as ^B (which would make text bold) and ^\B (which would stop it being bold). They're", 0, 0, 0, FontThin()
VFont 2, 161, "like HTML tags, I suppose.", 0, 0, 0, FontThin()
VFont 2, 193, "Lastly, SEND ME EMAIL!!! I'm at sam.thursfield@btinternet.com, and I like email. Also, visit my website", 0, 0, 0, FontThin()
VFont 2, 209, "at epsite.hypermart.net. See you in VFont4! Or Aradina (clue of next project).", 0, 0, 0, FontThin()
SLEEP
'=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'
'End Demo Code                                                               '
'=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-'

REM $STATIC
SUB VFont (Xpos, YPos, Text$, Col, BCol, Style, FontArray())

IF Xpos = -32767 THEN Xpos = ScreenWidth \ 2 - VLen(Text$, FontArray()) \ 2

Xx = Xpos + 7 - (5 * (Style AND 2))
Yy = YPos

IF Style AND 4 THEN
    VFont Xpos - 1, YPos - 1, Text$, BCol, 0, Style - 4, FontArray()
END IF

FOR Chr = 1 TO LEN(Text$)
    'MemPtr = 8 * ASC(MID$(Text$, Chr, 1)) + &HE
    FOR L = 0 TO 15
        BitPattern = FontArray(ASC(MID$(Text$, Chr, 1)), L + 1)
        IF Style AND 2 THEN
            IF (BitPattern AND 1) THEN PSET (Xx + (15 - (L \ 2)), Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 2) THEN PSET (Xx + (15 - (L \ 2)) - 1, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 4) THEN PSET (Xx + (15 - (L \ 2)) - 2, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 8) THEN PSET (Xx + (15 - (L \ 2)) - 3, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 16) THEN PSET (Xx + (15 - (L \ 2)) - 4, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 32) THEN PSET (Xx + (15 - (L \ 2)) - 5, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 64) THEN PSET (Xx + (15 - (L \ 2)) - 6, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 128) THEN PSET (Xx + (15 - (L \ 2)) - 7, Yy + L), Col - (15 - L) * (Style AND 1)
            IF Style AND 8 AND L = 15 THEN LINE (Xx + (15 - (L \ 2)) - 8, Yy + L)-(Xx + (15 - (L \ 2)) - 8 + FontArray(ASC(MID$(Text$, Chr, 1)), 0), Yy + L), Col - (15 - L) * (Style AND 1)
        ELSE
            IF (BitPattern AND 1) THEN PSET (Xx, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 2) THEN PSET (Xx - 1, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 4) THEN PSET (Xx - 2, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 8) THEN PSET (Xx - 3, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 16) THEN PSET (Xx - 4, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 32) THEN PSET (Xx - 5, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 64) THEN PSET (Xx - 6, Yy + L), Col - (15 - L) * (Style AND 1)
            IF (BitPattern AND 128) THEN PSET (Xx - 7, Yy + L), Col - (15 - L) * (Style AND 1)
            IF Style AND 8 AND L = 15 THEN LINE (Xx - 8, Yy + L)-(Xx - 8 + FontArray(ASC(MID$(Text$, Chr, 1)), 0), Yy + L), Col - (15 - L) * (Style AND 1)
        END IF
    NEXT
    Xx = Xx + FontArray(ASC(MID$(Text$, Chr, 1)), 0)
NEXT
END SUB

FUNCTION VLen (Text$, FontArray())
TempLen = 0
FOR I = 1 TO LEN(Text$)
    TempLen = TempLen + FontArray(ASC(MID$(Text$, I, 1)), 0)
NEXT I
VLen = TempLen
END FUNCTION

SUB VLoadFont (File$, Array())
DEF SEG = VARSEG(Array(0, 0))
    BLOAD File$, VARPTR(Array(0, 0))
DEF SEG
END SUB

