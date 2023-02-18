* Program..: IN-RPT.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*            May not be reproduced without permission.
* Notes....: This program uses the REPORT FORM command to produce
*            a master inventory report. The report is control breaked
*            on the first three characters of the PRD:NUM field.
ERASE
@ 2,0 SAY "Inventory Report"
@ 3,0 SAY equals
USE In-main INDEX In-pnum
STORE " " TO choice
@ 5,0 SAY "Output to (S)creen or (P)rinter? [S/P]" GET choice PICTURE "!"
READ
@ 5,0
IF choice = "P"
   @ 5,0 SAY "Printing..."
   SET CONSOLE OFF
   SET EJECT OFF
   REPORT FORM In-rpt1 TO PRINT
   SET CONSOLE ON
   SET EJECT ON
   SET PRINT ON
   EJECT
   SET PRINT OFF
   @ 5,0
ELSE
   REPORT FORM In-rpt1
ENDIF
USE
IF choice <> "P"
   STORE " " TO pause
   ?
   ?
   @ 23,0 SAY "Strike any key to continue..." GET pause
   READ
ENDIF
ERASE
RELEASE choice,pause
RETURN
* EOF IN-RPT.PRG