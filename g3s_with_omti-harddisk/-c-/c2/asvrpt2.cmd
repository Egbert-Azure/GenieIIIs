
* Program..: ASVRPT2.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
* Reserved.: pagenum, line, pagehdg, col:hdg, condition,
*            lastrec
*
SET TALK OFF
SET BELL OFF
SET MARGIN TO 5
STORE 1 TO pagenum
STORE 254 TO line
STORE "ASV Mitgliederliste" TO pagehdg
STORE (80-LEN(pagehdg))/2 TO col:hdg
*
* ---Open the datafile and print the report.
USE C:ASV
ERASE
@ 2, 0 SAY pagehdg
@ 2,72 SAY DATE()
@ 3, 0 SAY "========================================"
@ 3,40 SAY "========================================"
STORE " " TO select
@ 5,0 SAY "Output to the screen or printer? [S/P] ";
      GET select PICTURE "!"
READ
DO CASE
   CASE select = "S"
      ERASE
      STORE 22 TO pagelen
   CASE select = "P"
      SET FORMAT TO PRINT
      STORE 72 TO pagelen
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
   IF line > pagelen
      IF select = "S"
         ERASE
      ELSE
      ENDIF
      @ 0,0 SAY "PAGE NO."
      @ 0,9 SAY STR(pagenum,3)
      @ 2,col:hdg SAY pagehdg
      *
      * ---Generate column headings.
      @ 4,  0 SAY "Name  Vorname"
      @ 5,  0 SAY "========================================"
      STORE  pagenum+1 TO pagenum
      STORE 7 TO line
   ENDIF
   * ---Test to see if the condition exists.
   IF condition <> " "
      IF .NOT. (&condition)
         SKIP
         LOOP
      ENDIF
   ENDIF
   *
   * ---Print detail line.
   @ line,  0 SAY $(name,1, 40)
   STORE line+1 TO line
   SKIP
ENDDO
@ line+1,0 SAY " "
SET FORMAT TO SCREEN
RELEASE ALL
SET TALK ON
SET BELL ON
RETURN
* EOF: ASVRPT2.CMD
