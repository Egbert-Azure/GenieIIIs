* Program..: IN-CLEAN.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*            May not be reproduced without permission.
* Notes....: This program PACKs the file and permanently removes all
*            records marked for deletion. Note the indexes were not
*            in use which avoids possible index corruption.
ERASE
@ 2,0 SAY "File Maintenance"
@ 2,72 SAY DATE()
@ 3,0 SAY equals
TEXT

    This routine will permanently remove all records that are presently
    marked for deletion. Type "YES" if you wish to continue or strike
    <RETURN> to abort.

ENDTEXT
STORE "   " TO action
@ 20,0 SAY "Continue?" GET action PICTURE "!!!"
READ
IF action = "YES"
   @ 6,0
   @ 7,0
   @ 8,0
   @ 20,0
   @ 20,0 SAY "Working..."
   USE In-main
   SET TALK ON
   PACK
   SET TALK OFF
   USE
   USE In-main
   INDEX ON Prd:desc TO In-pdesc
   INDEX ON $(Prd:num,1,3) TO In-pnum
   USE
ENDIF
RELEASE action
ERASE
RETURN
* EOF IN-CLEAN.PRG