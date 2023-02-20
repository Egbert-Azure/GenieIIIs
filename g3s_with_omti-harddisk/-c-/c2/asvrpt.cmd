
* Program..: ASVRPT.CMD
* Author...: Egbert Schr|er
* Date.....: 06/09/92
* Notice...: Copyright 1992, All Rights Reserved
* Reserved.: pagenum, zeile, pagehdg, col:hdg, condition,
*            lastrec
*
SET TALK OFF
SET BELL OFF
SET MARGIN TO 7
STORE 1 TO pagenum
STORE 254 TO zeile
STORE "Mitgliederliste ASV Dorsten e.V." TO pagehdg
STORE (72-LEN(pagehdg))/2 TO col:hdg
*
* ---Open the datafile and print the report.
USE C:ASV
ERASE
@ 2, 0 SAY pagehdg
@ 2,65 SAY DATE()
@ 3, 0 SAY "___________________________________________________________________________"
STORE " " TO select
@ 5,0 SAY "Ausgabe auf (M)onitor oder (D)rucker   ";
      GET select PICTURE "!"
READ
DO CASE
   CASE select = "M"
      ERASE
      STORE 22 TO pagelen
   CASE select = "D"
      SET FORMAT TO PRINT
      STORE 62 TO pagelen
      DO INITPRT 
   OTHERWISE
      ERASE
      SET BELL ON
      SET TALK ON
      RETURN
ENDCASE
* ---Enter FOR <expression> for the report, such as,
* ---STORE "STATE = 'CA'" TO condition
STORE " " TO condition
DO WHILE .NOT. EOF
   IF zeile > pagelen
      IF select = "M"
         ERASE
      ELSE
      ENDIF
      @ 1,0 SAY "Seite "
      @ 1,9 SAY STR(pagenum,3)
      @ 1,65 SAY DATE()
      @ 2,col:hdg SAY pagehdg
      *
      * ---Generate column headings.
      @ 4,  0 SAY "Name  Vorname"
      @ 4, 30 SAY "Stra~e"
      @ 4, 56 SAY "PLZ  Ort"
      @ 5,  0 SAY "_______________________________________________________________________________"
      STORE  pagenum+1 TO pagenum
      STORE 7 TO zeile
   ENDIF
   * ---Test to see if the condition exists.
   IF condition <> " "
      IF .NOT. (&condition)
         SKIP
         LOOP
      ENDIF
   ENDIF
   *
   * ---Print detail zeile.
   @ zeile,  0 SAY $(name,1, 30)
   @ zeile, 30 SAY $(strasse,1, 24)
   @ zeile, 55 SAY plz 
   @ zeile, 61 SAY $(ort,1, 27)
   STORE zeile+1 TO zeile
   SKIP
ENDDO
@ zeile+1,0 SAY " "
SET FORMAT TO SCREEN
ERASE
RETURN
* EOF: ASVRPT.CMD
