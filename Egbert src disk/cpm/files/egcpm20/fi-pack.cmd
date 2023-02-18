
* Program..: FI-PACK.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
*
ERASE
@ 2, 0 SAY "P A C K    F I N A N Z"
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
@ 6,0 SAY "FINANZ.OLD will be your backup data file."
IF FILE( "FINANZ.OLD" )
   STORE " " TO select
   @ $+1,0 SAY "Delete the old backup file? (Y/N) ";
           GET select PICTURE "!"
   READ NOUPDATE
   IF select <> "Y"
      RETURN
   ENDIF
   DELETE FILE FINANZ.OLD
ENDIF
USE
RENAME FINANZ.DBF TO FINANZ.OLD
@ $+1,0 SAY " "
*
USE FINANZ.OLD
SET TALK ON
SET ECHO ON
COPY TO FINANZ
USE
USE FINANZ
* ---Recreate index file.
INDEX ON DATE TO DATE
USE
SET ECHO OFF
SET TALK OFF
STORE " " TO select
@ 22,0 SAY clearline
@ 22,0 SAY "Strike any key to continue...";
       GET select
READ NOUPDATE
RETURN
* EOF: FI-PACK.CMD
ß«°ß“žŒ‹šœ›¢ß
‹¤ßßßßßßßßß¼°±«¶±ªº¢
‹¤ßßßßßßßßßÕßÒÒÒ¼—šœ”ß™ßº±»Ò°¹Ò¹¶³ºÑ¢
‹¤ßßßßßßßßß¶¹ßÑ±°«Ñßº°¹¢
‹¤ßßßßßßßßßßßß»°ß¢ßÔß™–“šŒ†Œ‹š’ßÔß¤Ò˜š‹Œ¢
‹¤ßßßßßßßßßßßß¼³º¾­ß¸º«¬¢
‹¤ßßßßßßßßßº³¬º¢
‹¤ßßßßßßßßßßßßÕßÒÒÒº°¹ßš‘œŠ‘‹šš›Ñ¢
‹¤ßßßßßßßßßßßß¸°«°ß“žŒ‹šœ›¢
‹¤ßßßßßßßßßßßß¿ß¢Ô¬«­×“žŒ‹“–‘šÔÎÓÍÖÔ¤ÓÏß¬¾¦ßœ“šž“–‘š¢
‹¤ßßßßßßßßßßßß¿ß¢Ô¬«­×“žŒ‹“–‘šÔÎÓÍÖÔ¤ÓÏß¢Ôßßßßßßßßßßßßßßßßßß¤¬¾¦ßÝº±»Ò°¹Ò¹¶³ºßš‘œŠ‘‹šš›Ý¢
‹¤ßßßßßßßß