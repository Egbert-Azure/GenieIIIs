
* Program..: CP-POSN.CMD
* Author...: Your Name
* Date.....: 03/10/93
* Notice...: Copyright 1993, All Rights Reserved
*
STORE " " TO poschoice
@ 16,0 SAY clearline
@ 16,0 SAY "COMMAND: (D)isplay, (F)ind, "+;
           "(L)ocate, (C)ontinue, (S)kip ";
       GET poschoice PICTURE "!"
READ NOUPDATE
CLEAR GETS
@ 16,0 SAY clearline
IF .NOT. (poschoice $ "DFLCS")
   RETURN
ENDIF
IF poschoice = "F"
   * ---(F)ind
   @ 15,0 SAY "-"
   ACCEPT "Enter NAME " TO findkey
   @ 16,0 SAY clearline
   STORE TRIM(findkey) TO findkey
   IF findkey = " "
      RETURN
   ENDIF
   STORE # TO lastrecord
   FIND &findkey
   IF (# <> 0)
      DO CP-gets
      CLEAR GETS
   ELSE
      * ---NO FIND.
      GOTO lastrecord
      @ 16,0 SAY clearline
      @ 16,0 SAY '"'+findkey+'"'+" not in index"
      STORE " " TO select
      @ 17,0 SAY "Strike any key to continue...";
             GET select
      READ NOUPDATE
      @ 16,0 SAY clearline
      @ 17,0 SAY clearline
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
         DO CP-locat
         IF expression = " "
            RETURN
         ENDIF
         IF poschoice = "D"
            IF string = " "
               RETURN
            ENDIF
            DO CP-frame
         ENDIF
   ENDCASE
   * ---Check for END-OF-FILE.
   IF .NOT. EOF
      DO CP-gets
      CLEAR GETS
   ELSE
      * ---EOF encountered.
      GOTO lastrecord
      @ 16,0 SAY clearline
      @ 16,0 SAY "END-OF-FILE encountered"
      STORE " " TO select
      @ 17,0 SAY "Strike any key to continue...";
             GET select
      READ NOUPDATE
      @ 16,0 SAY clearline
      @ 17,0 SAY clearline
   ENDIF
ENDIF
RETURN
* EOF: CP-POSN.CMD
D
ãö—————≈ﬂ¢‘ªæ´∫◊÷
ã§’ﬂ±êãñúö———≈ﬂºêèÜçñòóãﬂŒ∆¢‘€◊ªæ´∫◊÷”»”Õ÷‘ﬂﬂﬂﬂﬂﬂ