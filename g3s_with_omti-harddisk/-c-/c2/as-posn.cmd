
* Program..: AS-POSN.CMD
* Author...: Egbert Schr|er
* Date.....: 25/08/92
* Notice...: Copyright 1992, All Rights Reserved
*
STORE " " TO poschoice
@ 14,0 SAY clearline
@ 14,0 SAY "Befehl : (D)isplay, (F)ind, "+;
           "(L)ocate, (C)ontinue, (S)kip ";
       GET poschoice PICTURE "!"
READ NOUPDATE
CLEAR GETS
@ 14,0 SAY clearline
IF .NOT. (poschoice $ "DSFCS")
   RETURN
ENDIF
IF poschoice = "F"
   * ---(F)ind
   @ 12,0 SAY "_"
   ACCEPT "Enter NAME " TO findkey
   @ 14,0 SAY clearline
   STORE TRIM(findkey) TO findkey
   IF findkey = " "
      RETURN
   ENDIF
   STORE # TO lastrecord
   FIND &findkey
   IF (# <> 0)
      DO AS-gets
      CLEAR GETS
   ELSE
      * ---NO FIND.
      GOTO lastrecord
      @ 14,0 SAY clearline
      @ 14,0 SAY '"'+findkey+'"'+" nicht in Indexdatei"
      STORE " " TO select
      @ 15,0 SAY "Weiter mit einer Taste ......";
             GET select
      READ NOUPDATE
      @ 14,0 SAY clearline
      @ 15,0 SAY clearline
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
         DO AS-locat
         IF expression = " "
            RETURN
         ENDIF
         IF poschoice = "D"
            IF string = " "
               RETURN
            ENDIF
            DO AS-frame
         ENDIF
   ENDCASE
   * ---Check for END-OF-FILE.
   IF .NOT. EOF
      DO AS-gets
      CLEAR GETS
   ELSE
      * ---EOF encountered.
      GOTO lastrecord
      @ 14,0 SAY clearline
      @ 14,0 SAY "END-OF-FILE encountered"
      STORE " " TO select
      @ 15,0 SAY "Strike any key to continue...";
             GET select
      READ NOUPDATE
      @ 14,0 SAY clearline
      @ 15,0 SAY clearline
   ENDIF
ENDIF
RETURN
* EOF: AS-POSN.CMD
