
* Program..: FI-MAIN.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
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

ERASE
@  1, 0 SAY "========================================"
@  1,40 SAY "========================================"
@  2, 0 SAY "||"
@  2,23 SAY "F I N A N Z    M A I N    M E N U"
@  2,78 SAY "||"
@  3, 0 SAY "========================================"
@  3,40 SAY "========================================"
@  4, 0 SAY "||"
@  4,78 SAY "||"
@  5, 0 SAY "||"
@  5,78 SAY "||"
@  6, 0 SAY "||"
@  6,78 SAY "||"
@  7, 0 SAY "||"
@  7,78 SAY "||"
@  8, 0 SAY "||"
@  8,78 SAY "||"
@  9, 0 SAY "||"
@  9,78 SAY "||"
@ 10, 0 SAY "||"
@ 10,78 SAY "||"
@ 11, 0 SAY "||"
@ 11,78 SAY "||"
@ 12, 0 SAY "========================================"
@ 12,40 SAY "========================================"
@  5,35 SAY " 0. exit"
@  6,35 SAY " 1. view"
@  7,35 SAY " 2. add"
@  8,35 SAY " 3. edit"
@  9,35 SAY " 4. pack"
STORE 5 TO selectnum
DO WHILE selectnum < 0 .OR. selectnum > 4
   STORE " " TO select
   @ 12,33 SAY " select : : "
   @ 12,42 GET select PICTURE "#"
   READ
   STORE VAL(select) TO selectnum
ENDDO

DO CASE
   CASE selectnum= 0
      CLEAR
      SET COLON ON
      SET BELL ON
      SET TALK ON
      RETURN
   CASE selectnum= 1
   *  DO view
      USE FINANZ INDEX DATE
      ERASE
      @ 1, 0 SAY "V I E W    F I N A N Z"
      @ 1,72 SAY DATE()
      DO FI-frame
      IF # = 0
         * ---The data file is empty.
         STORE " " TO select
         @ 12,0 SAY "EMPTY DATA FILE"
         @ 13,0 SAY "Strike any key to continue...";
                GET select
         READ NOUPDATE
      ELSE
         * ---The data file contains records.
         DO FI-gets
         CLEAR GETS
         STORE "X" TO poschoice
         DO WHILE poschoice <> " "
            DO FI-posn
         ENDDO
      ENDIF
      USE
   CASE selectnum= 2
   *  DO add
      USE FINANZ INDEX DATE
      COPY STRUCTURE TO FINANZ.add
      SELECT SECONDARY
      USE FINANZ.add
      ERASE
      @ 1, 0 SAY "A D D    F I N A N Z"
      @ 1,72 SAY DATE()
      DO FI-frame
      @ 12,0 SAY "Press <control-W> to exit"
      STORE "X" TO addchoice
      DO WHILE addchoice <> " "
         APPEND BLANK
         DO FI-gets
         READ
         * ---DATE cannot be blank.
         STORE TRIM( DATE ) TO addchoice
      ENDDO
      DELETE
      USE
      SELECT PRIMARY
      APPEND FROM FINANZ.add
      USE
   CASE selectnum= 3
   *  DO edit
      USE FINANZ INDEX DATE
      ERASE
      @ 1, 0 SAY "E D I T    F I N A N Z"
      @ 1,72 SAY DATE()
      DO FI-frame
      IF # = 0
         * ---The data file is empty.
         STORE " " TO select
         @ 12,0 SAY "EMPTY DATA FILE"
         @ 13,0 SAY "Strike any key to continue...";
                GET select
         READ NOUPDATE
      ELSE
         * ---The data file contains records.
         DO FI-gets
         CLEAR GETS
         DO FI-edit
      ENDIF
      USE
   CASE selectnum= 4
   *  DO pack
      DO FI-pack
ENDCASE

ENDDO T
* EOF: FI-MAIN.CMD
ﬂﬂ∂πﬂ‹ﬂ¬ﬂœ¢
ã§ﬂﬂﬂﬂﬂﬂﬂﬂﬂ’ﬂ“““´óöﬂõûãûﬂôñìöﬂñåﬂöíèãÜ—¢
ã§ﬂﬂﬂﬂﬂﬂﬂﬂﬂ¨´∞≠∫ﬂ›ﬂ›ﬂ´∞ﬂåöìöúã¢
ã§ﬂﬂﬂﬂﬂﬂﬂﬂﬂøﬂ¢‘¨´≠◊ìûåãìñëö‘Œ”Õ÷‘§”œﬂ¨æ¶