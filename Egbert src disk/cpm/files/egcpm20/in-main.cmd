* Program..: IN-MAIN.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This is the main calling program.
*
CLEAR
SET TALK OFF
SET BELL OFF
SET COLON OFF
STORE "========================================" + ;
      "========================================" TO equals
STORE "________________________________________" + ;
      "________________________________________" TO line
DO WHILE T
ERASE
@  1, 0 SAY "========================================"
@  1,40 SAY "========================================"
@  2, 0 SAY "||"
@  2,78 SAY "||"
@  2,14 SAY "I N V E N T O R Y    C O N T R O L    P R O G R A M"
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
@ 12, 0 SAY "||"
@ 12,78 SAY "||"
@ 13, 0 SAY "||"
@ 13,78 SAY "||"
@ 14, 0 SAY "========================================"
@ 14,40 SAY "========================================"
@  5,22 SAY " 0. exit"
@  6,22 SAY " 1. Run Inventory Report"
@  7,22 SAY " 2. Reorder Information"
@  8,22 SAY " 3. Update/View Inventory Database"
@  9,22 SAY " 4. Summary of Inventory Totals"
@ 10,22 SAY " 5. Run File Maintenance Routine"
@ 11,22 SAY " 6. Help"
STORE  7 TO select
DO WHILE select < 0 .OR. select >  6
   STORE " " TO mselect
   @ 14,33 SAY " select : : "
   @ 14,42 GET mselect PICTURE "#"
   READ
   STORE VAL(mselect) TO select
ENDDO
DO CASE
   CASE select= 0
      SET COLON ON
      CLEAR
      RETURN
   CASE select= 1
      DO In-rpt
   CASE select= 2
      DO In-reodr
   CASE select= 3
      DO In-vwed
   CASE select= 4
      DO In-dtail
   CASE select= 5
      DO In-clean
   CASE select= 6
      DO In-help
ENDCASE
ENDDO T
SET TALK ON
SET BELL ON
SET COLON ON
*EOF IN-MAIN.PRG
