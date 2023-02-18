
* Program..: FI-POSN.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
*
STORE " " TO poschoice
@ 12,0 SAY clearline
@ 12,0 SAY "COMMAND: (D)isplay, (F)ind, "+;
           "(L)ocate, (C)ontinue, (S)kip ";
       GET poschoice PICTURE "!"
READ NOUPDATE
CLEAR GETS
@ 12,0 SAY clearline
IF .NOT. (poschoice $ "DFLCS")
   RETURN
ENDIF
IF poschoice = "F"
   * ---(F)ind
   @ 11,0 SAY "-"
   ACCEPT "Enter DATE " TO findkey
   @ 12,0 SAY clearline
   STORE TRIM(findkey) TO findkey
   IF findkey = " "
      RETURN
   ENDIF
   STORE # TO lastrecord
   FIND &findkey
   IF (# <> 0)
      DO FI-gets
      CLEAR GETS
   ELSE
      * ---NO FIND.
      GOTO lastrecord
      @ 12,0 SAY clearline
      @ 12,0 SAY '"'+findkey+'"'+" not in index"
      STORE " " TO select
      @ 13,0 SAY "Strike any key to continue...";
             GET select
      READ NOUPDATE
      @ 12,0 SAY clearline
      @ 13,0 SAY clearline
   ENDIF
ELSE
   * ---(S)kip, (C)ontinue, (D)isplay, or (L)ocate
   STORE # TO lastrecord
   DO CASE
      CASE poschoice = "S"
      * ---(S)kip.
         SKIP
      CASE poschoice = "C"
      * ---(C)ontinue.
         CONTINUE
      OTHERWISE
      * ---(D)isplay or (L)ocate
         DO FI-locat
         IF expression = " "
            RETURN
         ENDIF
         IF poschoice = "D"
            IF string = " "
               RETURN
            ENDIF
            DO FI-frame
         ENDIF
   ENDCASE
   * ---Check for END-OF-FILE.
   IF .NOT. EOF
      DO FI-gets
      CLEAR GETS
   ELSE
      * ---EOF encountered.
      GOTO lastrecord
      @ 12,0 SAY clearline
      @ 12,0 SAY "END-OF-FILE encountered"
      STORE " " TO select
      @ 13,0 SAY "Strike any key to continue...";
             GET select
      READ NOUPDATE
      @ 12,0 SAY clearline
      @ 13,0 SAY clearline
   ENDIF
ENDIF
RETURN
* EOF: FI-POSN.CMD
’ﬂªûãö—————≈ﬂ¢‘ªæ´∫◊÷
ã§’ﬂ±êãñúö———≈ﬂºêèÜçñòóãﬂŒ∆¢‘€◊ªæ´∫◊÷”»”Õ÷‘ﬂﬂﬂﬂﬂﬂ