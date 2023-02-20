
* Program..: JUBIL.CMD
* Author...: Egbert Schr|er
* Date.....: 04/17/94
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
USE C:ASV INDEX ASVIND
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
      IF select = "M"
         ERASE
      ELSE
      ENDIF
      @ 1,0 SAY "Seite "
      @ 1,9 SAY STR(pagenum,3)
      @ 1,65 SAY DATE()
      @ 2,col:hdg SAY pagehdg
      *
      * die Alten
      *
      COUNT TO anzahl FOR val($(eintr,7,2))=94-25
      *
      * ---Print detail zeile.
      @ 4, 0 SAY "Mitglieder mit 25 Jahren Vereinszugeh|rigkeit in '94:"
      @ 4,50 SAY anzahl
      STORE  pagenum+1 TO pagenum
      STORE 7 TO zeile      
      * ------------
      * Alter = 10
      * ------------

      COUNT TO anzahl FOR VAL($(eintr,7,2))=94-10
      *
      * ---Print detail zeile.
      @ 6, 0 SAY "Mitglieder mit 10 Jahren Vereinszugeh|rigkeit in '94:"
      @ 6,50 SAY anzahl
      *
      * ---Generate column headings.
      @ 8,  0 SAY "Name  Vorname"
      @ 8, 30 SAY "Stra~e"
      @ 8, 56 SAY "PLZ  Ort"
      @ 9, 0 SAY "_______________________________________________________________________________"
      STORE  pagenum+1 TO pagenum
      STORE 7 TO zeile
  
   * ---Test to see if the condition exists.
   *
   * ---Print detail zeile.
set format to print
set alternate to jubil10.txt
set alternate on
   display name strasse plz ort FOR VAL($(eintr,7,2))=94-10
set alternate off
set alternate to jubil25.txt
set alternate on
   display name strasse plz ort FOR VAL($(eintr,7,2))=94-25
set alternate off
SET FORMAT TO SCREEN
RETURN
* EOF: JUBIL.CMD
