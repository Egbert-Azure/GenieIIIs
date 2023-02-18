*                      STATUS AENDERN
*
*                    use cpmuser.dbf
set talk off
set eject off
set print on
*
use user.dbf
*
go top
set print on
store "========================================================================"to li
STORE 0 TO ZAHL
STORE 0 TO Nr
store 0 to ind
*                       DRUCKPROGRAMM
*
do while .NOT. EOF
STORE ZAHL + 1 TO ZAHL
store ind +1 to ind
* NEUES BLATT
IF ZAHL > 4
  ?
  STORE  Nr + 1 TO Nr
  TEXT
                           Seite :
  ENDTEXT
  ?? Nr

  ?? CHR(12)
  STORE 1 TO ZAHL
ENDIF
? TRIM(VORNAME),NAME
? STRASSE
? PLZ,ORT
TEXT
Telefonnr.:
ENDTEXT
?? TEL
TEXT
SYSTEM :
ENDTEXT
?? SYSTEM
TEXT
CP/M Version :
ENDTEXT
?? CPM
TEXT
Ich biete an :
ENDTEXT
?? BIETE
TEXT
Ich h{tte gerne :
ENDTEXT
?? WILL
TEXT
Firma :
ENDTEXT
??firma
Text
EMAIL :
ENDTEXT
??Email1
??Email2
?
?? li
* naechstes Fragezeichen ist Extrazeile fuer Papierlaenge 72 !!!!!!!
skip
enddo
*
*                       DRUCKPROGRAMM FERTIG --- ALTEN STATUS WIEDERHERSTELLEN
*
set print off
set talk on
return
















































rint off
set talk on
return


























rn Helmut Jungkunz
CLUB 80 (TRS-80,CP/M)         ---> Herrn Fritz Chwolka 

INFO: Hardware und nat}rlich Hilfe zu Festplatte,Drives etc. kann man (frau)
bei Fa. Orbitronik, Herrn Herzceg bekommen..
(Die entsprechenden Adressen sind in der Liste nachz