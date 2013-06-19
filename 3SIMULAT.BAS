'ASSEMBLY IN QBASIC 4: INTERRUPT HANDLERS
'QBASIC ABOVE AND BEYOND PART....[DISCUSSION INTERRUPTHANDLERS]
'--------------------------------------------------------------------------
'Rick Elbers november 1996
'-------------------------
DECLARE FUNCTION newvec& (segnewint%, offnewint%)
DECLARE FUNCTION oldvec& (nr%)
DECLARE SUB getvec (Svec%, Ovec%, nr%)
DECLARE SUB pokeW (pokeseg%, pokeoff%, word%)
DECLARE SUB pokeDW (pokeseg%, pokeoff%, dword&)
                                                
'INTRODUCTION
'-------------
'When we are concerned with interrupthandlers, then our first goal has to be
'to establish the current way interrupts are handled. When some interrupt is
'processed the processor retrieves the place to jump to (among other things).
'This place he retrieves from 0:INTNR*4. That is to say, in the lower memory
'all interrupts are mapped with a far pointer to the code for that interrupt.
'Of course, since we spoke about FAR pointers every interrupt pointer is
'a double word. That is were the INTNR*4 is coming from. The interrupt pointers
'are commonly referred to as VECTORS.

'GETTING THE INTERRUPT VECTORS
'-----------------------------
'From inside our QBASIC ide we can get all vectors, but we have to keep
'in mind that we only retrieve vectors as QBASIC looks at them. That is to
'say that some interrupts are handled by qbasic itself, instead of by DOS.
'For instance the values for INT 0 and INT 4 are not the same that you
'get from dumping the lower memory in debug...(you are encouraged to do so..)
'But otherwise the program to execute now is very instructive...

CLS
FOR nr% = 0 TO &HFF
getvec Vecseg%, Vecoff%, nr%
PRINT " Int "; STRING$(2 - LEN(HEX$(nr%)), "0"); HEX$(nr%); "->";
IF Vecseg% = 0 AND Vecoff% = 0 THEN
  PRINT "Not used ";
ELSE
  PRINT STRING$(4 - LEN(HEX$(Vecseg%)), "0"); HEX$(Vecseg%); ":";
  PRINT STRING$(4 - LEN(HEX$(Vecoff%)), "0"); HEX$(Vecoff%);
END IF
IF nr% MOD 4 = 3 THEN PRINT
IF nr% MOD 80 = 79 THEN SLEEP
NEXT
CLS : PRINT "press a key for next demo part"
SLEEP

'We have seen that a lot of INTERRUPT VECTORS are free, so we can
'experiment with them. Even when after controlling the so called "FREE
'VECTORS" with debug they appear indeed free. So it is sure and safe
'to experiment with INT 83h for instance. So be it: we will start with
'developing a testcode for our newinterrupt, so that there will be no
'misunderstandings wether we execute int 83h or not. Afterwards we will
'do what is necessary to make it possible to execute int 83h. Our
'handler will do nothing fancy. He will simply print the character 'a' .
'-------
'NEWINT
'-------
DIM newint%(6)
newint%(0) = &H5052              'PUSH AX,DX
newint%(1) = &H41B2              'MOV  DL,41
newint%(2) = &H2B4               'MOV  AH,02
newint%(3) = &H21CD              'INT  21
newint%(4) = &H5A58              'POP DX,AX
newint%(5) = &HCB                'RETF

segnewint% = VARSEG(newint%(0)): offnewint% = VARPTR(newint%(0))
'Agreed ? Well let us control him:
DEF SEG = segnewint%: CALL absolute(offnewint%)
SLEEP
'Seems to be oke, not ?
'--------------------------
'SETTING INTERRUPT VECTORS
'--------------------------
'Since we have a working handler by now, we might process with the part of
'resetting the interrupt 83h vector( currently 0:0 ) to the adress of our
'handler( currently segnewint%:offnewint%). As it turns out QBASIC
'enables our capacity to set a vector with the DOScall INT 21, 25h. At least
'on my PC this call bumps( when somebody show me otherwise i'll be delighted).
'But for know let us take the more direct route, which is: changing the adress
'stored at 0:83h*4

setvec:
DEF SEG = 0
POKE (&H83 * 4), offnewint% AND &HFF
POKE (&H83 * 4 + 1), offnewint% \ &HFF
POKE (&H83 * 4 + 2), segnewint% AND &HFF
POKE (&H83 * 4 + 3), segnewint% \ &HFF
SLEEP

'OKE What we did here was storing the adress of our handler
'in the interrupt vector table in the form:
'                       DW  SEG:OFF

'Interesting enough in assembler you won't risk such a move since there
'could and in fact will be an interrupt during the writing to the vector
'adress and the vector will be stored inadekwately. One other thing:
'The interrupts are all far return adresses, which is just difficult
'language for that they are words in the form segment:offset. THIS IS
'NOT THE STANDARD WAY OF STORING A WORD! but INTEL designed it so...
'About the formulas used: ANDING AN INTEGER WITH &HFF will give you the
'High Byte since 0000 0000 1111 1111 AND .... .... .... .... will result
'in zeroing the higher byte(agreed ?). The other way around works the
'division which returns the higher byte in the lower byte.

'[in fact it is not ! for signed integers, luckely i know the segment of
'QBASIC is not a signed value so everything will role smoothly....For code
'that is always okay for signed integers look at the POKEW and POKEDW routines]

'controle
FOR i% = 0 TO 3
  PRINT HEX$(PEEK(&H20C + i%));
NEXT
DEF SEG
SLEEP

'So far we seems to be oke: allright ? Well lets review a bit. It is
'called SETVEC wat we have done. It should be easy for you to make this
'into a callable sub. In general you have to poke to INTNR*4+something.
'May be it is just ok to do this first right now before going further...

'Okay so far,so good. Now the miracle itself. Will it work ? It is easy to
'be so eager to try that you overlook one major change you have to make
'to our handler. It ended so far with RETF, but an interrupt restores the
'flags also so we have to return with an Interrupt-return(IRET):
newint%(5) = &HCF       'IRET replaces RETF

'Okay now we are ready for the miracle. Have we developed an INTERRUPT ?
'Let's just call INT 83h and look what happens. PFFF........
DIM test%(2)
test%(0) = &H83CD      'INT 83
test%(1) = &HCB        'RETF

CLS : DEF SEG = VARSEG(test%(0))
CALL absolute(VARPTR(test%(0)))
DEF SEG

'I do not know what your machine is saying but mine is printing 'a' !
'Is that all ? Yes, that is all. We have passed our first exam!


'INTERRUPT HANDLERS:part one
'----------------------------
'When we want to intercept an important interrupt like DOS INT 21 then
'it is obvious that we first must have the ability to store the vector
'so that we can put the original vector back after we are done. Since our
'int 83 can serve further as example interrupt let us call getvec for
'int 83 and then store the vector.


getvec segvec%, offvec%, &H83
Savevec:
oldint83& = segvec% * 65536 + offvec%

'We are going to store the vector in one WORDvalue since 1)It reduces our
'variables with one. Therefore adding to clarity. 2) In essence the vector
'is just one variable. And while we store him this way we can with just one
'instruction like JMP OLDint83 return to our former handler.
'For control again:
PRINT HEX$(oldint83&)

'Agreed ? No ? Well you are right. This long integer is not really a DW
'since INTEL choosed to put the vectors in the form off(flipped):seg(flipped)
'and not in the expected seg(flipped):off(flipped) way. This might be
'a little confusing at first. But look at it this way: Because we store it
'this way oldint83& is stored in the form off:seg.When later getvec looks at
'the vectors in the table it reads excactly the offset at INTnr*4 and the
'segment at INTnr*4+2. So indeed excactly our OLDINT83&!


'A SHORT_LONGCUT :
'--------------------
'Well,okay say we want to interrupt int 21h because we do not like the way
'function 0ah handles function-KEYS like <F1>( it do'nt). We are capable of
'getting the vector already. Also we can store the vector in an adekwate way
'and set the vector to our code. At a certain point we want to restore the
'vector table again(Agreed ?), because we do not want to write code for all
'INt 21h functions. At least not for free.....<G>. So we have to practice one
'other thing. That is :resetting a vector from the one we stored.

'Now let us return to our practice. We are writing int 83h. Let us now try to
'reset the vector first to 0000:0000 what it was before we were so arrogant to
'take over. This should be simple. Use setvec with SEGnewint%=0 AND OFFnewint%=0
SLEEP
DEF SEG = 0: POKE (&H83 * 4), 0: POKE (&H83 * 4 + 1), 0
POKE (&H83 * 4 + 2), 0: POKE (&H83 * 4 + 3), 0:

'CONTROL:
getvec segvec%, offvec%, &H83
PRINT HEX$(segvec%), HEX$(offvec%)
DEF SEG
'agreed again ?
SLEEP

'What was again the question: Oh,yes after Hooking DOS we will reset the
'vector. Since we not put an int handler which interrupted our int 83H at
'0000:0000, which we should by the way never even try, and since the concept
'of interrupting ourselves never managed to come clear to me, we just have
'to imagine we just stored our vector 0:0 at int 83h adres. So what is
'left is that we want to restore the vector after done..Since we have
'oldint83& this should be simple not ? But poke just only writes bytes!
'this means you should write your own pokeDW. Not so..?

'DEF SEG = 0: POKEDW &H83 * 4, oldint83&: DEF SEG
'and thats it.

'Control is now important.By the way does everybody knows that doshelp.com
'and edit.com are QBASIC programs started up with QBASIC/edcom and
'QBASIC/qhelp respectively ( so much for not compiler QBASIC ?)

getvec segvec%, offvec%, &H83
PRINT HEX$(segvec%); HEX$(offvec%): DEF SEG = 0
FOR i% = 0 TO 3: PRINT HEX$(PEEK((&H83 * 4) + i%)): NEXT

'Okay while this whole document might seem a little bit childish in nature
'you must agree that we are advancing...At this moment we know we can
'restore vectors after using them. A very important feature...
'Also you could have stored the two procedures SETVEC and GETVEC by now i
'guess, making your code even more evident. vecRestore is just a specific
'Setvec and Savevec& is a)evident and b) needs POKE_DW

'To advance we have to know some basics of an Interrupthandler
'--------------------------------------------------------------
'What an interrupt does is a whole lot, so be aware: An interrupt
'does push first the flags, then pushes cs:ip(return adress),then calculates
'vector adress as we do INTnr*4 loads up ip: then cs of the INTvec. After
'done there is an IRET executed: meaning that first cs then ip then the
'flags are restored from the stack( minus the interrupt and trap flag, which
'are cleared on return).
'In CODE: 'PUSHF
          'PUSH CS
          'PUSH IP
          'vectoradressing: leave the MUL etc for know
          'JMP VECTORADRESS
          '...................
          'DOES WHATEVER HERE IS DONE
          '...................
          'POP IP
          'POP CS
          'POPF
'Although not entiraly true this is sufficient knowledge for know. From
'above it could be concluded that when writing an interrupthandler we have
'to do one extra thing when we define our handler as a far procedure( meaning
'that apart from IP also CS is pushed). We have to do manually PUSHF
'before jumping to the OLDINT and PopF them again as the OLDINT returns
'since the IRET at the end has POPF 'ed . After that we simulated the
'INT call.!

'Okay before actually going into writing a real interrupt-handler we just
'have to do lots more in the sector of making procedures,using labels
'in our asm-subs. I can tell one thing on before hand : the string way
'of going does not seem to survive procedures,using Data_array,labels etc.
'At least i am returning to a better way of the old code%().It is not
'necessary to use datastatements, and clearity could even be incremented
'using also ASM comments( necessary for bigger asmsubs) in a header like
'in normal assemblerprograms with some modifications. Furthermore, arrays
'does not wander around in mem, so they are adressable as procedures,datas
'etc. At last, since you can use datas directly in your assemblersubs,the
'external use of stack is dropping down to zero. This diminished a lot of
'code_space. Making the code more visible. Okay, i will return on the subject of Hooking Interrupt
'later......

'Let's round the whole thing off with rewriting our RestoreVEC. We should
'be possible to directly write the VEC back using OLDINT83& ,not?
'Okay let us try.
getvec segvec%, offvec%, &H83: PRINT HEX$(segvec%), HEX$(offvec%)
'reset the vec to 0?
pokeDW 0, &H83 * 4, 0
getvec segvec%, offvec%, &H83: PRINT HEX$(segvec%), HEX$(offvec%)
'we seems to have it done.
'Now restore our vector ?
pokeDW 0, &H83 * 4, oldint83&
getvec segvec%, offvec%, &H83: PRINT HEX$(segvec%), HEX$(offvec%)

'Maybe you agree that we accurataly restored a vector. Since probably
'the only purpose for getting a vector is to restore him at some time
'we might even change Getvec so that it reads one long integer in the
'format segvec:offvec in our variable OlDINT&
PRINT HEX$(oldvec&(&H83))'seems to be ok?

'Ok now to summarize we can do the whole thing with enlightening speed
'and shortness:
CLS
pokeDW 0, &H83 * 4, 0           'restore situation to start
oldint83& = oldvec&(&H83)       'get the original int83
pokeDW 0, &H83 * 4, segnewint% * 65536 + offnewint%
                                'set our int83 vector
DEF SEG = VARSEG(test%(0))
CALL absolute(VARPTR(test%(0))) 'execute our handler
DEF SEG
pokeDW 0, &H83 * 4, oldint83&   'restore the original vector


'Seems to be oke, not ?  Apart from doing the whole thing in an assembler
'procedure i can see only one thing to improve codeshortness being to
'write a function newvec&(segnewint%,offnewint%) which is trivial
'but in the long run it rules out the flip INTEL confusion. Note that we
'prepared us already on using assembler for the whole thing by consequently
'loading up registers first,before writing to a BASIC variable( which is,
'can be argued, bad code viewed solely from here and now QBASICcode)

'part 2 to be released on my homepage.some time later will handle the
'actual INT 21 handler.


SUB getvec (d%, v%, nr%)

d% = 1: v% = 2'variabele initialisatie
dataseg% = VARSEG(d%): offset% = VARPTR(d%)
datasg$ = CHR$(dataseg% AND &HFF) + CHR$(dataseg% \ 256)
offset1$ = CHR$(offset% AND &HFF) + CHR$(offset% \ 256)
offset2$ = CHR$((VARPTR(v%) AND &HFF)) + CHR$((VARPTR(v%) \ 256))

'CODE IN QBASIC
'**************************************


ASM$ = ""
ASM$ = ASM$ + CHR$(&HB4) + CHR$(&H35)              'MOV  AH,35        
ASM$ = ASM$ + CHR$(&HB0) + CHR$(nr%)               'MOV  AL,INTnr    
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H21)              'INT  21           
ASM$ = ASM$ + CHR$(&HB8) + dataseg$                'MOV  AX,dataseg$     
ASM$ = ASM$ + CHR$(&H8E) + CHR$(&HD8)              'MOV  DS,AX        
ASM$ = ASM$ + CHR$(&H8C) + CHR$(&H6) + offset1$    'MOV  ptr[seg],ES
ASM$ = ASM$ + CHR$(&H89) + CHR$(&H1E) + offset2$'  'mov ptr[off],BX 
ASM$ = ASM$ + CHR$(&HCB)                           'RETF            


'____________________________
 Codeoff% = SADD(ASM$)
 DEF SEG = VARSEG(ASM$)
 CALL absolute(Codeoff%)
'____________________________
DEF SEG

END SUB

FUNCTION newvec& (segnewint%, offnewint%)
newvec& = segnewint% * 65536 + offnewint%
END FUNCTION

FUNCTION oldvec& (nr%)
'-------------------------------------------------------------------'
'This function is a replacement of GETVEC. It stores the old vector
'in INTEL format in an long integer. INTEL format means in the format
'segment:offset here.
'The procedure first stores the vector in ES[BX], before we access it
'IN:  INTnr%
'OUT: oldvec&
'-------------------------------------------------------------------'
s% = 1: o% = 2'variabele initialisatie
dataseg% = VARSEG(s%): offset% = VARPTR(s%)
datasg$ = CHR$(dataseg% AND &HFF) + CHR$(dataseg% \ 256)
offset1$ = CHR$(VARPTR(s%) AND &HFF) + CHR$(VARPTR(s%) \ 256)
offset2$ = CHR$(VARPTR(o%) AND &HFF) + CHR$(VARPTR(o%) \ 256)


'CODE IN QBASIC
'**************************************


ASM$ = ""
ASM$ = ASM$ + CHR$(&HB4) + CHR$(&H35)              'MOV  AH,35      
ASM$ = ASM$ + CHR$(&HB0) + CHR$(nr%)               'MOV  AL,INTnr  
ASM$ = ASM$ + CHR$(&HCD) + CHR$(&H21)              'INT  21         
ASM$ = ASM$ + CHR$(&HB8) + dataseg$                'MOV  AX,dataseg$   
ASM$ = ASM$ + CHR$(&H8E) + CHR$(&HD8)              'MOV  DS,AX      
ASM$ = ASM$ + CHR$(&H8C) + CHR$(&H6) + offset1$    'MOV  ptr[seg],ES
ASM$ = ASM$ + CHR$(&H89) + CHR$(&H1E) + offset2$'  'mov ptr[off],BX
ASM$ = ASM$ + CHR$(&HCB)                           'RETF          


'____________________________
 Codeoff% = SADD(ASM$)
 DEF SEG = VARSEG(ASM$)
 CALL absolute(Codeoff%)
'____________________________
DEF SEG
oldvec& = s% * 65536 + o%
END FUNCTION

SUB pokeDW (pokeseg%, pokeoff%, dword&)
'This function will just poke a Dword into memory, just like
'the standard function Poke does it, with one enhancement.
'While poke needs a def seg before it we will transfer that to
'the function also! So :
'DW segment to poke word to
'DW offset to poke word to
'DD Dwordvalue to poke
'---------------------------------------------------------------
DEF SEG = VARSEG(dword&)
ptr% = VARPTR(dword&)
LowWlowbyte% = PEEK(ptr%): LowWhighbyte% = PEEK(ptr% + 1)
HighWlowbyte% = PEEK(ptr% + 2): HighWhighbyte% = PEEK(ptr% + 3)

DEF SEG = pokeseg%
  POKE pokeoff%, LowWlowbyte%
  POKE pokeoff% + 1, LowWhighbyte%
  POKE pokeoff% + 2, HighWlowbyte%
  POKE pokeoff% + 3, HighWhighbyte%
DEF SEG
END SUB

SUB pokeW (pokeseg%, pokeoff%, word%)
'This function will just poke a word into memory, just like
'the standard function Poke does it, with one enhancement.
'While poke needs a def seg before it we will transfer that to
'the function also! So :
' DW segment to poke word to
' DW offset to poke word to
' DW wordvalue to poke
'Of course you should use this only for reasons of putting all your
'variables in one DATAsegment, since otherwise just defining a integer
'is enough.
'---------------------------------------------------------------
DEF SEG = VARSEG(word%)
ptr% = VARPTR(word%)
highbyte% = PEEK(ptr% + 1): lowbyte% = PEEK(ptr%)
DEF SEG = pokeseg%
  POKE pokeoff%, lowbyte%
  POKE pokeoff% + 1, highbyte%
DEF SEG

END SUB
