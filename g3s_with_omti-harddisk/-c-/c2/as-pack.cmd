
* Program..: AS-PACK.CMD
* Author...: Your Name
* Date.....: 25/08/92
* Notice...: Copyright 1992, All Rights Reserved
*
ERASE
@ 2, 0 SAY "P A C K    A S V"
@ 2,72 SAY DATE()
@ 3, 0 SAY "========================================"
@ 3,40 SAY "========================================"
STORE "NO " TO select
@ 5,0 SAY "PACK the entire file? [YES/NO] ";
      GET select PICTURE "!!!"
READ NOUPDATE
IF select <> "YES"
   RETURN
ENDIF
@ 6,0 SAY "C:ASV.OLD will be your backup data file."
IF FILE( "C:ASV.OLD" )
   STORE " " TO select
   @ $+1,0 SAY "Delete the old backup file? (Y/N) ";
           GET select PICTURE "!"
   READ NOUPDATE
   IF select <> "Y"
      RETURN
   ENDIF
   DELETE FILE C:ASV.OLD
ENDIF
USE
RENAME C:ASV.DBF TO C:ASV.OLD
@ $+1,0 SAY " "
*
USE C:ASV.OLD
SET TALK ON
SET ECHO ON
COPY TO C:ASV
USE
USE C:ASV
* ---Recreate index file.
INDEX ON NAME TO ASVIND
USE
SET ECHO OFF
SET TALK OFF
STORE " " TO select
@ 22,0 SAY clearline
@ 22,0 SAY "Strike any key to continue...";
       GET select
READ NOUPDATE
RETURN
* EOF: AS-PACK.CMD
