
* Program..: ASVLBL.CMD
* Author...: Egbert Schr|er
* Date.....: 25/08/92
* Notice...: Copyright 1992, All Rights Reserved
* Reserved.: select, condition, extra
* Am 16.09.92 auf neues Aufkleberformat eingestellt
* Dazu werden die Daten mit Leerzeilen vor/nachher
* ausgerichtet. Bei Format{nderung neu ausprobieren !
SET TALK OFF
SET BELL OFF
STORE " " TO select
USE c:ASV index asvind
ERASE
@ 2, 0 SAY "D : A S V    A D R E S S - A U K L E B E R"
@ 2,72 SAY DATE()
@ 3, 0 SAY "________________________________________"
@ 3,40 SAY "________________________________________"
STORE " " TO select
@ 5,0 SAY "Ausgabe auf (M)onitor oder (D)rucker  ";
      GET select PICTURE "!"
READ
DO CASE
   CASE select = "M"
      ERASE
   CASE select = "D"
      SET CONSOLE OFF
      SET PRINT ON
   OTHERWISE
      ERASE
      RETURN
ENDCASE
* ---Enter FOR <expression> for the labels, such as,
* ---STORE "STATE = 'CA'" TO condition
STORE " " TO condition
DO WHILE .NOT. EOF
   IF condition <> " "
      IF .NOT. (&condition)
         SKIP
         LOOP
      ENDIF
   ENDIF
   STORE 0 TO extra
   ?
   ?
   ? "An Frau/Herrn"
   ?
   ? " "+name
   ? " "+strasse
   ? plz,ort
   ? 
   ?
   ?
   ?
   ?
   DO WHILE extra > 0
      ?
      STORE extra - 1 TO extra
   ENDDO
   SKIP
ENDDO
*
SET PRINT OFF
SET CONSOLE ON
?
? "Ende der Liste..."
CLEAR
ERASE
RETURN
* EOF: ASVLBL.CMD
