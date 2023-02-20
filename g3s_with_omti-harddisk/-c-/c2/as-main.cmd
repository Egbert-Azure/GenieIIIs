
* Program..: AS-MAIN.CMD
* Author...: Egbert Schr|er
* Date.....: 25/08/92
* Notice...: Copyright 1992, All Rights Reserved
* Reserved.: select, selectnum, editchoice, poschoice,
*            error, findkey, expression, string, is:eof
*            clearline, addchoice, is:some, lastrecord
*
SET TALK OFF
SET BELL OFF
SET COLON OFF
* ---Use blanks to clear-to-end-of-line.
STORE $(STR(0,81),1,80) TO clearline

DO WHILE T

* ERASE
DO ASVMSK2
STORE 5 TO selectnum
DO WHILE selectnum < 0 .OR. selectnum > 4
   STORE " " TO select
   @ 14,32 GET select PICTURE "#"
   READ
   STORE VAL(select) TO selectnum
ENDDO

DO CASE
   CASE selectnum= 0
*      CLEAR
      ERASE
      RETURN
   CASE selectnum= 1
   *  DO view
      USE C:ASV INDEX asvind
      ERASE
      @ 1, 0 SAY "Anzeige    A S V-Dorsten e.V."
      @ 1,72 SAY DATE()
      DO AS-frame
      IF # = 0
         * ---The data file is empty.
         STORE " " TO select
         @ 14,0 SAY "Leere Datenbank"
         @ 15,0 SAY "Weiter mit einer Taste ...";
                GET select
         READ NOUPDATE
      ELSE
         * ---The data file contains records.
         DO AS-gets
         CLEAR GETS
         STORE "X" TO poschoice
         DO WHILE poschoice <> " "
            DO AS-posn
         ENDDO
      ENDIF
      USE
   CASE selectnum= 2
   *  DO add
      USE C:ASV INDEX asvind
      COPY STRUCTURE TO C:ASV.add
      SELECT SECONDARY
      USE C:ASV.add
      ERASE
      @ 1, 0 SAY "Neueingabe    A S V-Dorsten e.V."
      @ 1,72 SAY DATE()
      DO AS-frame
      @ 14,0 SAY "Dr}cke <control-W> f}r Ende"
      STORE "X" TO addchoice
      DO WHILE addchoice <> " "
         APPEND BLANK
         DO AS-gets
         READ
         * ---NAME cannot be blank.
         STORE TRIM( NAME ) TO addchoice
      ENDDO
      DELETE
      USE
      SELECT PRIMARY
      APPEND FROM C:ASV.add
      USE
   CASE selectnum= 3
   *  DO edit
      USE C:ASV INDEX asvind
      ERASE
      @ 1, 0 SAY "Editieren    A S V-Dorsten e.V."
      @ 1,72 SAY DATE()
      DO AS-frame
      IF # = 0
         * ---The data file is empty.
         STORE " " TO select
         @ 14,0 SAY "Leere Datenbank"
         @ 15,0 SAY "Weiter mit einer Taste ...";
                GET select
         READ NOUPDATE
      ELSE
         * ---The data file contains records.
         DO AS-gets
         CLEAR GETS
         DO AS-edit
      ENDIF
      USE
   CASE selectnum= 4
   *  DO pack
      DO AS-pack
ENDCASE

ENDDO T
* EOF: AS-MAIN.CMD
