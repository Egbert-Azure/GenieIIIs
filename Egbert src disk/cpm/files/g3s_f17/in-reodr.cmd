* Program..: IN-REODR.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program checks the file to find which products need to
*	     be reordered. A product is flagged if the quantity on hand
*	     plus the quantity on order is less than the reorder point.
ERASE
@ 2,0 SAY "Reorder Information"
@ 3,0 SAY equals
USE In-main
IF # = 0
   @ 5,0 SAY "There are no records in the database"
ELSE
   STORE " " TO mchoice
   @ 5,0 SAY "Output to the (S)creen or (P)rinter? [S/P]" GET mchoice ;
     PICTURE "!"
   READ
   IF mchoice = "P"
      @ 5,0
      @ 5,0 SAY "Printing..."
      SET PRINT ON
      SET CONSOLE OFF
   ENDIF
   SET EJECT OFF
   REPORT FORM In-rpt2 FOR Reodr:pt > (On:hand + On:Order)
   IF mchoice = "P"
      SET PRINT OFF
      SET CONSOLE ON
   ELSE
      ?
      ?
      ?
   ENDIF
ENDIF
USE
STORE " " TO mpause
@ 23,0 SAY "Strike any key to continue..." GET mpause
READ
ERASE
RELEASE mpause,mchoice
RETURN
* EOF IN-REODR.PRG