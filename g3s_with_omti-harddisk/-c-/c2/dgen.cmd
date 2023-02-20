* Program..: dGEN.CMD (version 1.0)
* Author...: Luis A. Castro
* Date.....: 02/25/84
* Notice...: Copyright 1984, All Rights Reserved
* Notes....: This program is the menu driver for the
*            dGEN utilities.  Three of the modules
*            (MENUGEN, FILEGEN, LABELGEN) were written
*            by yours truly and the fourth (FORMGEN)
*            was co-authored with Roy M. Moore.
* Reserved.: select, selectnum
*
SET TALK OFF
SET BELL OFF
SET COLON OFF

DO WHILE T

ERASE
@  1, 0 SAY "========================================"
@  1,40 SAY "========================================"
@  2, 0 SAY "||"
@  2,25 SAY "d G E N    M A I N    M E N U"
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
@  5,27 SAY " 0. exit"
@  6,27 SAY " 1. MENU generator"
@  7,27 SAY " 2. FILE generator"
@  8,27 SAY " 3. REPORT FORM generator"
@  9,27 SAY " 4. LABEL generator"
STORE  5 TO selectnum
DO WHILE selectnum < 0 .OR. selectnum >  4
   STORE " " TO select
   @ 12,33 SAY " select : : "
   @ 12,42 GET select PICTURE "#"
   READ
   STORE VAL(select) TO selectnum
ENDDO

DO CASE
   CASE selectnum= 0
      SET COLON ON
      SET BELL ON
      SET TALK ON
      CLEAR
      RETURN
   CASE selectnum= 1
   *  DO MENU generator
      DO Menugen
   CASE selectnum= 2
   *  DO FILE generator
      DO Filegen
   CASE selectnum= 3
   *  DO REPORT FORM generator
      DO Formgen
   CASE selectnum= 4
   *  DO LABEL generator
      DO Labelgen
ENDCASE

* ---The following flags have to be reset, because Menugen,
* ---Filegen, Formgen, and Labelgen set the flags to their
* ---default values.
SET TALK OFF
SET BELL OFF
SET COLON OFF
?
?
STORE " " TO select
@ 23,0 SAY "Strike any key to continue... " GET select 
READ

ENDDO T
* EOF: dGEN.CMD
