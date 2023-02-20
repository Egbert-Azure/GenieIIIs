
* Program..: CP-PACK.CMD
* Author...: Your Name
* Date.....: 03/10/93
* Notice...: Copyright 1993, All Rights Reserved
*
ERASE
@ 2, 0 SAY "P A C K    C P M L I B S"
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
@ 6,0 SAY "CPMLIBS.OLD will be your backup data file."
IF FILE( "CPMLIBS.OLD" )
   STORE " " TO select
   @ $+1,0 SAY "Delete the old backup file? (Y/N) ";
           GET select PICTURE "!"
   READ NOUPDATE
   IF select <> "Y"
      RETURN
   ENDIF
   DELETE FILE CPMLIBS.OLD
ENDIF
USE
RENAME CPMLIBS.DBF TO CPMLIBS.OLD
@ $+1,0 SAY " "
*
USE CPMLIBS.OLD
SET TALK ON
SET ECHO ON
COPY TO CPMLIBS
USE
USE CPMLIBS
* ---Recreate index file.
INDEX ON NAME TO CPMIND
USE
SET ECHO OFF
SET TALK OFF
STORE " " TO select
@ 22,0 SAY clearline
@ 22,0 SAY "Strike any key to continue...";
       GET select
READ NOUPDATE
RETURN
* EOF: CP-PACK.CMD
D

¹–š“›Å‘ž’š
‚
ŠœŠ‘‹šßÔßÎß«°ßœŠ‘‹š
º
„
‹¤­º«ª­±¢
‹¤Õßº°¹Åß¢ßÔßŠ‹™–“š
‹
¹¾³«º­±¾«ºß°¹¹
Ÿ
Š™–“šŒ†Œ‹š’ßÔßÝÒº»¶«ÝßÔßš‡‹š‘Œ–‘ß«°ßŠ‹™–“š
¹¾³«º­±¾«ºß«°ßÙŠ‹™–“š
¹¾³«º­±¾«ºß°±
‹¤Õß¯˜ž’ÑÑÅß¢ÔŠ‹™–“š
‹¤Õß¾Š‹—ÑÑÑÅß¢Ô†Š‘ž’š
‹¤Õß»ž‹šÑÑÑÑÑÅß¢Ô»¾«º×Ö
‹¤Õß±‹–œšÑÑÑÅß¼†–˜—‹ßÎÆ¢ÔÛ×»¾«º×ÖÓÈÓÍÖÔßßßßßßßßßßßßßßßß¤Óß¾““ß­–˜—‹Œß­šŒš‰š›¢
‹¤Õ¢
‹¤»°ß¨·¶³ºß«¢
‹¤ßßß¬«°­ºßÝßÝß«°ßš›–‹œ—–œš¢
‹¤ßßß¿