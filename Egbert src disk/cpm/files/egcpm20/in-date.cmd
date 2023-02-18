* Program..: IN-DATE.PRG
* Author...: Luis A. Castro (modified by RMM)
* Date.....: 10-09-83
* Notice...: Copyright 1983, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without written permission.
* Notes....: This program will read and verify the date that was input.
*
*
*
STORE VAL($(mreodr:dte,1,2)) TO month
STORE VAL($(mreodr:dte,4,2)) TO day
STORE VAL($(mreodr:dte,7,2)) TO year
DO WHILE month<1 .OR. month>12 .OR. day<1 .OR.;
day>VAL($("312931303130313130313031",(month-13*INT(month/13))*2-1,2)) .OR.;
(month=2 .AND. day>28 .AND. year/4.0>INT(year/4.0)) .OR. datechng
   IF .NOT. datechng
      STORE "  /  /  " TO mreodr:dte
   ENDIF
   @ 15,62 GET mreodr:dte PICTURE "XX/XX/XX"
   READ
   STORE VAL($(mreodr:dte,1,2)) TO month
   STORE VAL($(mreodr:dte,4,2)) TO day
   STORE VAL($(mreodr:dte,7,2)) TO year
   STORE F TO datechng
ENDDO
STORE STR(month,2)+"/"+STR(day,2)+"/"+STR(year,2) TO mreodr:dte
* EOF IN-DATE.PRG