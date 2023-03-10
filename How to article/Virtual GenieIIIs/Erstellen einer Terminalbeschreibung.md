# Erstellen einer Terminalbeschreibung ##

              Am Beispiel Genie IIIs unter Kaempf CP/M 3.0
                     Egbert Schroeer, November 1992

1. Vorwort

Nachdem  nun in einigen CLUB INFO's schon }ber Z3PLUS und  auch  die
Erstellung  einer  TCAP  -  Info  32,  Seite  35  ff,  F.Chwolka   -
geschrieben  wurde, m|chte ich diese mit folgendem Artikel  erg{nzen
und  damit  allen Neulingen die Erstellung einer  eigenen  Terminal-
Beschreibung  unter  Z3PLUS  erleichtern. Das User  Manual  sagt  zu
diesem   Thema  leider  nichts  und  hinkt  der  Entwicklung   etwas
hinterher.

2. Terminal unter Z3PLUS

Als  Standard-Installtion  wird  bei Kauf von  Z3PLUS  das  Programm
TCSELECT  mit einer aktuellen TCAP Library ausgeliefert. Damit  wird
das  Terminal-Capability-File (1) f}r das entsprechende  CP/M-System
bzw.  den  Computer-Typ  ausgew{hlt. Beim CP/M 3.0  von  K{mpf  wird
beispielsweise ein Televideo 920 Terminal emuliert.
Wenn  man  die Grafikf{higkeit des Genie IIIs unter Gdos  kennt  ist
diese  Installation  mehr  als  mager,  da  ja  auch  Z3PLUS  Grafik
verarbeiten kann.

3. VLIBxx - Grafikfaehigkeit unter Z3PLUS

VLIB  ist  eine Library von Routinen }rspr}nglich von  Richard  Conn
geschrieben.
Inzwischen wurden viele [nderungen und Erg{nzungen durchgef}hrt.
VLIB liegt jetzt in der Version 4.2d vor (2).
Programme,  die  diese Routinen nutzen, sind in der Lage  das  Video
Display um die Funktionen

- direkte Cursor-Adressierung
- spezielle Video Eigenschaften
- Pull-Down Menues

zu erweitern.
Dazu  muessen Informationen ueber den Rechner und/oder die  verwendete
CP/M 3.0 Implementation vorliegen (z.B. Character-Set etc.).

4. Erstellung eines Source Codes fuer das Terminal

Mit dem Programm `TCSRC14.COM` (3) erzeugt man -  nachdem mit `TCSELECT`
ein Terminal gewaehlt wurde - einen kommentierten Z80 Source Code des
TCAP  im  Environment. Dieser File wird als Default  mit  `Z3TCAP.Z80`
benannt und sieht fuer das Televideo 920 Terminal wie folgt aus:

``` as
;
; Z3TCAP file:  Z3TCAP.Z80
;
ESC     EQU     27              ; Escape character
;
; The first character in the terminal name must not be a space.  For
; Z3TCAP.TCP library purposes only, the name terminates with a space
; and must be unique in the first eight characters.
;
TNAME:  DB      'TVI920       ' ; Name of terminal (13 chars)
;
GOFF:   DB      GOELD-TNAME     ; Graphics offset from Z3TCAP start
;
;  Terminal configuration bytes B14 and B15 are defined and  bits  
;  assigned
; as follows.  The remaining bits are not currently assigned.  Set 
;  these
; bits according to your terminal configuration.
;
;       B14 b7: Z3TCAP Type.... 0 = Standard TCAP  1 = Extended TCAP
;
;       bit:    76543210
B14:    DB      00100000B       ; Configuration byte B14
;
;    B15 b0: Standout....... 0 = Half-Intensity 1 = Reverse Video
;    B15 b1: Power Up Delay. 0 = None           1 = Ten-second delay
;    B15 b2: No Auto Wrap... 0 = Auto Wrap      1 = No Auto Wrap
;    B15 b3: No Auto Scroll. 0 = Auto Scroll    1 = No Auto Scroll
;    B15 b4: ANSI........... 0 = ASCII          1 = ANSI
;
;       bit:    76543210
B15:    DB      00100000B       ; Configuration byte B15
;
; Single character arrow keys or WordStar diamond
;
        DB      'K'-40H         ; Cursor up
        DB      'J'-40H         ; Cursor down
        DB      'L'-40H         ; Cursor right
        DB      'H'-40H         ; Cursor left
;
; Delays (in ms) after sending terminal control strings
;
        DB      50              ; CL delay
        DB      0               ; CM delay
        DB      0               ; CE delay
;
; Strings start here
;
CL:     DB      'Z'-40H,0       ; Home cursor and clear screen
CM:     DB      ESC,'=%+ %+ ',0 ; Cursor motion macro
CE:     DB      ESC,'T',0       ; Erase from cursor to end-of-line
SO:     DB      ESC,')',0       ; Start standout mode
SE:     DB      ESC,'(',0       ; End standout mode
_
```

___________________________________________________________________
Ab hier werden die eigenen Terminal Definitionen statt 0 eingefuegt.
_____________________________________________________________________

``` as
TI:     DB      0               ; Terminal initialization
TE:     DB      0               ; Terminal deinitialization
;
; Extensions to standard Z3TCAP
;
LD:     DB      0               ; Delete line at cursor position
LI:     DB      0               ; Insert line at cursor position
CD:     DB      0               ; Erase from cursor to end-of-screen
;
; The attribute string contains the four command characters to set
; the following four attributes for this terminal in the following
; order:        Normal, Blink, Reverse, Underscore
;
SA:     DB      0               ; Set screen attributes macro
AT:     DB      0               ; Attribute string
RC:     DB      0               ; Read current cursor position
RL:     DB      0               ; Read line until cursor
;
; Graphics TCAP area
;
GOELD:  DB      0               ; Graphics On/Off delay in ms
;
; Graphics strings
;
GO:     DB      0               ; Graphics mode On
GE:     DB      0               ; Graphics mode Off
CDO:    DB      0               ; Cursor Off
CDE:    DB      0               ; Cursor On
;
; Graphics characters
;
GULC:   DB      0               ; Upper left corner
GURC:   DB      0               ; Upper right corner
GLLC:   DB      0               ; Lower left corner
GLRC:   DB      0               ; Lower right corner
GHL:    DB      0               ; Horizontal line
GVL:    DB      0               ; Vertical line
GFB:    DB      0               ; Full block
GHB:    DB      0               ; Hashed block
GUI:    DB      0               ; Upper intersect
GLI:    DB      0               ; Lower intersect
GIS:    DB      0               ; Mid intersect
GRTI:   DB      0               ; Right intersect
GLTI:   DB      0               ; Left intersect
;
;  Fill remaining space with zeros
;
         REPT   128-($-TNAME)
         DB     0
         ENDM

        END
;
; End of Z3TCAP
;
```

Fuer das Genie IIIs unter Kaempf CP/M 3.0  sieht der geaenderte  Source
Code nun wie folgt aus:

``` as
;
; Z3TCAP file:  G3SK.Z80
;
ESC     EQU     27              ; Escape character
;
; The first character in the terminal name must not be a space.  For
; Z3TCAP.TCP library purposes only, the name terminates with a space
; and must be unique in the first eight characters.
;
TNAME:  DB      'G3S-KAEMPF   ' ; Name of terminal (13 chars)
;
GOFF:   DB      GOELD-TNAME     ; Graphics offset from Z3TCAP start
;
;  Terminal configuration bytes B14 and B15 are defined and  bits  
;  assigned
; as follows.  The remaining bits are not currently assigned.  Set 
;  these
;  bits according to your terminal configuration.
;
;       B14 b7: Z3TCAP Type.... 0 = Standard TCAP  1 = Extended TCAP
;
;       bit:    76543210
B14:    DB      10000000B       ; Configuration byte B14
;
; B15 b0: Standout....... 0 = Half-Intensity 1 = Reverse Video
; B15 b1: Power Up Delay. 0 = None           1 = Ten-second delay
; B15 b2: No Auto Wrap... 0 = Auto Wrap      1 = No Auto Wrap
; B15 b3: No Auto Scroll. 0 = Auto Scroll    1 = No Auto Scroll
; B15 b4: ANSI........... 0 = ASCII          1 = ANSI
;
;       bit:    76543210
B15:    DB      00000001B       ; Configuration byte B15
;
; Single character arrow keys or WordStar diamond
;
        DB      'K'-40H         ; Cursor up
        DB      'V'-40H         ; Cursor down
        DB      'L'-40H         ; Cursor right
        DB      'H'-40H         ; Cursor left
;
; Delays (in ms) after sending terminal control strings
;
        DB      0               ; CL delay
        DB      0               ; CM delay
        DB      0               ; CE delay
;
; Strings start here
;
CL:     DB      ESC,'*',0       ; Home cursor and clear screen
CM:     DB      ESC,'=%+ %+ ',0 ; Cursor motion macro
CE:     DB      ESC,'T',0       ; Erase from cursor to end-of-line
SO:     DB      ESC,')',0       ; Start standout mode
SE:     DB      ESC,'(',0       ; End standout mode
TI:     DB      0               ; Terminal initialization
TE:     DB      0               ; Terminal deinitialization
;
; Extensions to standard Z3TCAP
;
LD:     DB      ESC,'R',0       ; Delete line at cursor position
LI:     DB      ESC,'E',0       ; Insert line at cursor position
CD:     DB      ESC,'Y',0       ; Erase from cursor to end-of-screen
;
; The attribute string contains the four command characters to set
; the following four attributes for this terminal in the following
; order:        Normal, Blink, Reverse, Underscore
;
;  I had no informations about the screen attribute macro  of  
; Kaempf CP/M ; 3.0.
; I wrote to Klaus Kaempf for more informations and for the source 
; codes in september 1992, but there was no response until now
; (February 1993).
; If anybody is able to complete this TCAP please inform me or call:
; Egbert Schroeer
; Joachimstrasse 18
; W-4270 Dorsten
; Germany
; Phone: private: 02362/75311
;        on business: 02365/49/9649
;   
SA:     DB      0               ; Set screen attributes macro
AT:     DB      0               ; Attribute string
RC:     DB      0               ; Read current cursor position
RL:     DB      0               ; Read line until cursor
;
; Graphics TCAP area
;
GOELD:  DB      0               ; Graphics On/Off delay in ms
;
; Graphics strings
;
GO:     DB      ESC,'$',0       ; Graphics mode On
GE:     DB      ESC,'%',0       ; Graphics mode Off
CDO:    DB      ESC,'.0',0      ; Cursor Off
CDE:    DB      ESC,'.4',0      ; Cursor On
;
; Graphics characters
;
GULC:   DB      86H             ; Upper left corner
GURC:   DB      87H             ; Upper right corner
GLLC:   DB      85H             ; Lower left corner oK
GLRC:   DB      88H             ; Lower right corner oK
GHL:    DB      8BH             ; Horizontal line oK
GVL:    DB      8AH             ; Vertical line oK
GFB:    DB      160D            ; Full block oK
GHB:    DB      7FH             ; Hashed block Testing
GUI:    DB      8EH             ; Upper intersect oK
GLI:    DB      8FH             ; Lower intersect oK
GIS:    DB      89H             ; Mid intersect oK
GRTI:   DB      8CH             ; Right intersect oK
GLTI:   DB      8DH             ; Left intersect oK
;
;  Fill remaining space with zeros
;
         REPT   128-($-TNAME)
         DB     0
         ENDM

        END
;
; End of Z3TCAP
;
```

Da  sich  Herr Kaempf leider bezueglich Informationen zu  seinem  CP/M
bedeckt  haelt  -  trotz  mehrmaliger Anschreiben  -  ,  konnten  die
Character Sets nur mit dem Programm `CHARSET` ermittelt werden. Weiter
Informationen  liegen  zum  Terminal nicht vor, bzw.  sind  aus  der
`Dokumentation zum CP/M 2.2`, erschienen bei Roeckrath, zu entnehmen.
Die  Disketten Sage xxx bekommt man bei dem naechsten Verteiler  oder
direkt bei mir.

Egbert

```
(1) siehe User Manual
(2) siehe auch VLIB-Help Files auf Sage 41-Disk
(3) Sage 38-Disk
```