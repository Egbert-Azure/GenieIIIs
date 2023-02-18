* Program..: IN-DELTE.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*            May not be reproduced without permission.
* Notes....: This program marks a record for deletion or RECALLs the
*            the record if already marked for deletion.
IF mchoice = "D"
   DELETE
ELSE
   RECALL
   @ 1,50 SAY "       "
ENDIF
RETURN
* EOF IN-DELTE.PRG
