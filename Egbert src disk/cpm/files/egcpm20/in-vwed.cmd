* Program..: IN-VWED.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program is called by INMAIN.PRG. It is a sub-menu
*	     which allows the user to choose what further action to
*	     take.
USE In-main INDEX In-pnum,In-pdesc
DO WHILE T
ERASE
@  1, 0 SAY "========================================"
@  1,40 SAY "========================================"
@  2, 0 SAY "||"
@  2,78 SAY "||"
@  2,29 SAY "U P D A T E    M E N U"
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
@ 11, 0 SAY "========================================"
@ 11,40 SAY "========================================"
@  5,26 SAY " 0. exit"
@  6,26 SAY " 1. Add New Records"
@  7,26 SAY " 2. View Existing Records"
@  8,26 SAY " 3. Update Existing Records"
STORE  4 TO select
DO WHILE select < 0 .OR. select >  3
   STORE " " TO mselect
   @ 11,33 SAY " select : : "
   @ 11,42 GET mselect PICTURE "#"
   READ
   STORE VAL(mselect) TO select
ENDDO
DO CASE
   CASE select= 0
      USE
      ERASE
      RETURN
   CASE select= 1
      DO In-add
   CASE select= 2
      DO In-view
   CASE select= 3
      DO In-updte
ENDCASE
ENDDO T
* EOF IN-VWED.PRG
