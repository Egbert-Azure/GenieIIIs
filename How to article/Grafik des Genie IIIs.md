# Grafik des Genie IIIs unter Holte CP/M+ #
## Volker Dose, Egbert Schröer, Juli 1993 ##

Die  Grafikmöglichkeiten des Genie IIIs unter Holte CP/M+ zu  nutzen 
wird eine kleine Artikelserie beschreiben. Dies ist in den Programmiersprachen
Turbo Pascal 3.0, ANSI C und Assembler realisiert worden.  
Beginnen möchten wir mit einem  Assembler-Programm,  das  die 
Möglichkeit bietet Grafik aus dem Speicher auszulesen und als File 
zu schreiben und diese wieder einzulesen. Ausgiebig Gebrauch wird 
dabei von der USERF Funktion gemacht.

``` asm
;******************************************************************
;*                  H R G . C O M                                 *
;******************************************************************
;*                                                                *
;* Programm um HRG-Bilder des Genie IIIs von Diskette zu laden    *    
;* und zu speichern.                                              *
;* Der Bildinhalt wird mit einer Systemroutine des Bios in den    *
;* HRG-Speicher des G IIIs übertragen.                            *
;*                                                                *
;******************************************************************
;*      Volker Dose, Egbert Schröer     Februar-Juli 1993         *
;******************************************************************     

; Das Programm muss zunächst an den jeweiligen Monitor
; angepasst werden!
; Die Anzahl der Scanzeilen und die Anzahl der angezeigten 
; Zeilen mu~ hier eingetragen werden.

scanzeilen      equ     11
zeilenzahl      equ     25
maxy            equ     scanzeilen*zeilenzahl

;BIOS
;BDOS

bel     equ     07h
lf      equ     0ah
cr      equ     0dh
esc     equ     1bh
clrscr  equ     1ah     ;Bildschirm löschen

bdos    equ     0005h
wboot   equ     0000h

userf   equ     30

prtstr  equ     9
open    equ     15
close   equ     16
searchf equ     17
next    equ     18
write   equ     21
delete  equ     19
makef   equ     22
parse   equ     152
read    equ     20
multio  equ     44
adrdma  equ     26
grafik  equ     25
fnein   equ     01h
fnaus   equ     02h

fcb     equ     5ch
;--------------------------------
        org     100h            ;Start der TPA
;--------------------------------
;       Hauptprogramm
;--------------------------------
start   ld      de,hello        ;Begrüßung ausgegeben
        ld      c,prtstr        ;mittels BDOS-Call
        call    bdos            ;Auswahl Laden/Speichern anbieten
echo    ld      c,fnein         ;Funktionsnummer für Eingabe von
                                ;der Tastatur laden
        call    bdos            ;Zeichen von der Tastatur ins 
                                ;A-Register holen
        sub     a,20h           ;in Großbuchstabe umwandeln
        ld      e,l             ;Zeichen in Parameterregister
                                ;bringen
        cp      'L'             ;Was will Sie/Er denn ? 
        jr      nz,weiter
        ld      de,frage3
        ld      c,prtstr
        call    bdos
        ld      c,fnein         ;Welche HRG Seite ?
        call    bdos            ;
        ld      (hrgsei),a      ;speichern      
        call    liesstr         ;Filenamen holen
        call    loadhrg

ende    call    graoff          ;Nun machen wir die Grafik
                                ;wieder aus
        jp      wboot           ;Rücksprung ins Betriebssystem 
                                ;über Warmstart
weiter  cp      'S'
        jr      nz,ende         ;Was hat'er nu' gedrückt ?
        ld      de,frage3
        ld      c,prtstr
        call    bdos
        ld      c,fnein         ;Welche HRG Seite ?
        call    bdos            ;
        ld      (hrgsei),a      ;speichern
        call    liesstr         ;Filenamen holen
        call    savehrg 

        jr      ende            ;Programm beenden

;----------------------------------------
;       Unterprogramme
;----------------------------------------

;----------------------------------------
;       Eingabe Filename
;----------------------------------------               

liesstr ld      de,frage2
        ld      c,prtstr
        call    bdos
        ld      hl,fname        ;Zeiger auf Speicher für Name

lies    ld      c,fnein
        push    hl              ;Zeiger sichern
        call    bdos            ;Zeichen von Tastatur holen
        pop     hl              ;Zeiger restaurieren
        ld      (hl),a          ;Zeichen in Speicherbereich
                                ;ablegen
        inc     hl              ;auf nächsten freien Speicher-
                                ;platz zeigen
        cp      cr              ;Zeichen mit Endmarkierung ver-
                                ;gleichen
        jp      nz,lies         ;Lesen, bis Endmarkierung gelesen
                                ;wurde
        ld      hl,fname        ;Zunaechst wird der Filename des
        ld      de,nambuf       ;Bildes gesichert
        ldir                    ;Speichertransfer
        ret                     ;Zurück zur aufrufenden Stelle
                                         
;----------------------------------------
;       Aufruf über USERF 30
;----------------------------------------
system: push    bc              ;zunaechst BC sichern
        ld      ix,(wboot+1)    ;(IX) Start der BIOS Jump Table 
        ld      bc,3*(userf-1)  ;Offset zum JP USERF
        add     ix,bc           ;dazu addieren  
        pop     bc              ;USERF-Nr. wieder zurück
        jp      (ix)            ;Sprung nach USERF und zurück zum
                                ;Aufrufer von SYSTEM
;--------------------------------
;       L O A D H R G
;--------------------------------
loadhrg ld      bc,0            ;zunächst wird die Länge des 
                                ;Filenames geholt
        ld      hl,nambuf       ;Start der Zeichenkette
loop    inc     c               ;C einen weiterzählen
        inc     hl              ;Zeiger auch
        ld      a,(hl)          ;Ist das Terminierungszeichen 
                                ;erreicht ?
        cp      ':'             ;ist ein Drive angegeben? 
                                ;das darf nicht sein !
        jp      z,error
        cp      00h             ;dann stimmt BC 
        jr      nz,loop         ;Schleife durchlaufen bis Schluss
        ld      a,(hrgsei)      ;Welche Seite soll gespeichert
                                ;werden ?
        cp      '0'             ;ist es Null ?
        jr      z,goon          ;dann zunächst nur speichern
        cp      '1'             ;oder Seite 1 ?
        jp      nz,error        ;Falsche Eingabe der Seiten #

goon    ld      (seite),a       ;In Meldung unterbringen
        ld      de,schlu        ;Jetzt wird eine Meldung ausgegeben
        ld      c,prtstr        ;mittels BDOS-Call
        call    bdos
        ld      de,schlu2
        ld      c,prtstr
        call    bdos
        call    graon           ;Grafik anknippsen
aufdsk  ld      de,pfcb         ;PARSE Filename will einen Control-
                                ;block sehen
        ld      c,parse         ;der Filename ist jetzt aufbereitet
                                ;worden
        call    bdos            ;jetzt wird geguckt, ob das File 
                                ;schon existiert
        ld      de,fcb          ;File Control Block
        ld      c,searchf       ;suche Eintrag
        call    bdos
        cp      0ffh            ;Eintrag gefunden = File existiert
                                ;bereits ?
        jp       z,keins        ;FILE NOT EXIST ausgeben 
holes   ld      de,fcb          ;jetzt soll das File von Disk 
                                ;gelesen werden
        ld      c,open          ;es wird zunächst geöffnet
        call    bdos            
        cp      00h             ;ist alles gut gegangen ?
        jp      nz,nixis        ;wenn nicht Fehlermeldung ausgeben
        ld      (fcb+32),a      ;cr-feld des FCB auf 0
        ld      (fcb+12),a      ;ext-feld auf 00
        ld      e,128           ;Multisektor I/O mit 16 K Bloecken
        ld      c,multio        ;Bdos-Call
        call    bdos
        ld      de,buffer       ;Zieladresse ist der Buffer
        ld      c,adrdma        ;Set DMA Adress                         
        call    bdos
        ld      de,fcb          ;jetzt werden 16 KB gelesen
        ld      c,read
        call    bdos
        cp      00h             ;alles gut gegangen ?
        jp      nz,nixis
        ld      e,128           ;wieder 16 KB schreiben
        ld      c,multio
        call    bdos
        ld      hl,buffer
        ld      bc,4000h        ;16kB dazuaddieren
        add     hl,bc
        ex      de,hl           ;DMA Adresse nach DE
        ld      c,adrdma        ;set DAM Adresse
        call    bdos
        ld      de,fcb
        ld      c,read
        call    bdos
        cp      00h
        jp      nz,nixis        
gutgut  ld      de,fcb          ;das File schließen
        ld      c,close
        call    bdos            ;das wars dann!!!
trans   ld      a,(seite)       ;jetzt wird das Bild in den 
                                ;HRG-Speicher übertragen
        sub     '0'             ;ASCII => binär umwandeln
        add     0f0h            ;Quelle ist der Buffer
        ld      hl,parablock 
        ld      de,buffer       
        ld      c,33            ;Systemfunktion Bereich kopieren
        call    system
        ret                     ;Zurück zum Hauptprogramm 
;--------------------------------
;       S A V E H R G
;--------------------------------
savehrg ld      bc,0            ;zunächst wird die Länge des
                                ;Filenames geholt
        ld      hl,nambuf       ;Start der Zeichenkette
loop1   inc     c               ;C einen weiterzählen
        inc     hl              ;Zeiger auch
        ld      a,(hl)          ;Ist das Terminierungszeichen 
                                ;erreicht ?
        cp      ':'             ;ist ein Drive angegeben?->Fehler
        jp      z,error
        cp      00h             ;dann stimmt BC 
        jr      nz,loop1        ;Schleife durchlaufen bis Schluss
        ld      a,(hrgsei)      ;Welche Seite soll gespeichert
                                ;werden ?
        cp      '0'             ;ist es Null ?
        jr      z,goon1         ;dann zunächst nur speichern
        cp      '1'             ;oder Seite 1 ?
        jp      nz,error        ;Falsche Eingabe der Seiten #
goon1   ld      (seite),a       ;In Hello-Meldung unterbringen
        ld      de,schlu        ;Meldung ausgegeben
        ld      c,prtstr        ;mittels BDOS-Call
        call    bdos
        ld      de,schlu2
        ld      c,prtstr
        call    bdos
        call    graon           ;Grafik anknippsen
trans1  ld      a,(seite)       ;jetzt wird das Bild in die
                                ;TPA übertragen
        sub     '0'             ;ASCII => binär
        rrca                    ;Quellseite ins obere Nibble schieben
        rrca
        rrca
        rrca
        add     0fh             ;Ziel ist der Buffer
        ld      hl,parablock 
        ld      de,buffer       
        ld      c,33            ;Systemfunktion Bereich kopieren
        call    system
aufdsk1 ld      de,pfcb         ;PARSE Filename will einen 
                                ;Controlblock sehen
        ld      c,parse         ;der Filename ist aufbereitet
        call    bdos            ;jetzt wird gekuckt, ob das File 
                                ;schon existiert
        ld      de,fcb          ;File Control Block
        ld      c,searchf       ;suche Eintrag
        call    bdos
        cp      0ffh            ;Eintrag gefunden = File 
                                ;existiert bereits ?
        jr      z,keins1        ;nicht ? dann weiter im Text
        ld      de,frage1       ;Frage 1 ausgeben
        ld      c,prtstr
        call    bdos            ;
        ld      c,01h           ;hole Zeichen von der Konsole
        call    bdos
        cp      'n'             ;wenn anderer Filename soll, 
                                ;dann Abbruch
        jp      z,wboot
loefil  ld      de,fcb          ;sonst File loeschen
        ld      c,delete                 
        call    bdos            ;und neues File erzeugen
keins1  ld      de,fcb          ;jetzt das File erzeugen
        ld      c,makef         ;
        call    bdos            
        cp      00h             ;ist alles gut gegangen ?
        jp      nz,nixis        ;wenn nicht Fehlermeldung ausgeben
        ld      e,128           ;Multisektor I/O mit 16 K Bloecken
        ld      c,multio        ;Bdos-Call
        call    bdos
        ld      de,buffer       ;Quelladresse ist der Buffer
        ld      c,adrdma        ;Set DMA Adress                         
        call    bdos
        ld      de,fcb          ;jetzt wird echt geschrieben
        ld      c,write
        call    bdos
        cp      00h             ;alles gut gegangen ?
        jp      nz,nixis
        ld      e,128           ;wieder 16 KB schreiben
        ld      c,multio
        call    bdos
        ld      hl,buffer
        ld      bc,4000h        ;16kB dazuaddieren
        add     hl,bc
        ex      de,hl           ;DMA Adresse nach DE
        ld      c,adrdma        ;set DAM Adresse
        call    bdos
        ld      de,fcb
        ld      c,write
        call    bdos
        cp      00h
        jp      nz,nixis        
        ld      a,(fcb+32)
        ld      (0010h),a
        ld      de,fcb          ;das File schliessen
        ld      c,close
        call    bdos            ;das wars dann!!!
        ret                     ;Rücksprung zum Hauptprogramm

;-------------------------------------------------------------
;       Knippst den gewählten HRG Bildschirm des Genie IIIs an
;-------------------------------------------------------------  
graon:  ld      a,(hrgsei)      ;hier steht die angegebene 
                                ;Seitennummer
        sub     '0'             ;ASCII -> binär
        cp      1               ;ist Seite 1 gefragt?
        jr      z,sei1          ;dann diese Seite einschalten 
        cp      0               ;ist Seite 0 gefragt?
        ret     nz              ;wenn nicht 0 oder 1 raus
        di                      ;jetzt wird Seite 0 eingeschaltet
        in      a,(0f9h)        ;bit 4 von Sysport 0 entscheidet, 
        res     4,a             ;welche von beiden Seiten
        out     (0f9h),a        ;selektier werden kann
        jr      on
sei1    di                      ;Grafik-Seite 1 ist gewünscht
        in      a,(0f9h)
        set     4,a
        out     (0f9h),a
on      ei
        ld      a,1             ;A<>0  => Grafik an !
        ld      c,grafik        ;C enthält die USERF Nummer
        call    system          ;Ruft USERFunktion auf und RET
        ret                     ;zurück zum Aufrufer von GRAON

;-----------------------------------------------------
;       Knippst den HRG Bildschirm des Genie IIIs aus
;-----------------------------------------------------  
graoff: ld      a,0             ;A=0  => Grafik aus !
        ld      c,grafik        ;C enthält die USERF Nummer
        call    system          ;Ruft USERFunktion auf und RET
        ret                     ;zurück ins Betriebssystem

;-------------------------------
;       Fehlermeldungen
;-------------------------------
nixis   ld      de,mist         ;es ist ein Fehler aufgetreten !
        ld      c,prtstr        ;Fehlermeldung ausgeben
        call    bdos
        jp      wboot
;--------------------------------
keins   ld      de,nichtda      ;Fehlermeldung ausgeben
        ld      c,prtstr
        call    bdos
        jp      wboot
;--------------------------------
error   ld      a,'$'           ;Zunächst einen Teil ausgeben
        ld      (schlu),a       ;String dort terminieren
        ld      de,hello        ;den dann ausgeben
        ld      c,prtstr
        call    bdos
        ld      de,falsch       ;Fehlermeldung ausgeben
        ld      c,prtstr
        call    bdos
        jp      wboot

;--------------------------------
;       Nachrichten an den User
;--------------------------------                                
nichtda defb    bel,'   Es ist kein File dieses Namens vorhanden.',cr,lf,'$'

falsch  defb    bel,'   Syntax: HRG-Seite 0 oder 1; filename.ext',lf,cr
        defb    '$'

hello   defb    clrscr,cr,lf,esc,'R','HRG.COM',esc,'S',' liest und schreibt'
        defb    ' HRG-Bilder im aktuellen Disk/User-Bereich.',lf,cr
        defb    '            Eingestellt sind 11 Scan-Zeilen und 25 Zeilen.',lf,cr
        defb    '            Nur lauffähig auf Genie IIIs mit Holte CP/M +!'
        defb    cr,lf,lf

wahl    defb    'Grafik ',esc,'R','l',esc,'S','aden oder',esc,'R','S',esc,'s'
        defb    'peichern ?',lf,cr
        defb    '$'
        
schlu   defb    'Das Bild mit dem Namen ',lf,cr

nambuf  defb    00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h,00h
        defb    ' '
        defb    '$'

schlu2  defb    cr,lf
        defb    'wird in die HRG Seite '
seite   defb    '0'
        defb    ' eingelesen. ',cr,lf,lf
        defb    '$'

mist    defb    bel,' Es ist ein Fehler aufgetreten. Abbruch !!','$'

frage1  defb    bel,'File dieses Namens existiert bereits. File l|schen ? (J/N) '
        defb    '$'

frage2  defb    bel,cr,lf,'Filename: ',cr,lf
        defb    '$'

frage3  defb    bel,cr,lf,'Welche Seite ? ',cr,lf
        defb    '$' 
;----------------------------------------
parablock                               ;hier stehen die Parameter
                                        ;für die Systemroutine, 
        defw    0000h                   ;hier Startpunkt X
        defw    0000h                   ;Startpunkt Y
        defw     640                    ;horizontale Kantenlänge
        defw    maxy+1                  ;vertikale Kantenlänge
                        
pfcb    defw    nambuf                  ;Filename in ASCII-Format
        defw    fcb                     ;hier liegt der 'wahre' FCB

hrgsei  defs    1
fname   defs    12

buffer  equ     $                       ;Buffer für das Bild !
                        
        end     start
```  
