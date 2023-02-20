
* Program..: AS-EDIT.CMD
* Author...: Egbert Schr|er
* Date.....: 29/12/92
* Notice...: Copyright 1992, All Rights Reserved
*
DO WHILE T
   STORE " " TO editchoice
   @ 14,0 SAY "Befehl : (E)dit, (D)elete, (U)ndelete, "+;
              "(C)ontinue, (P)osition ";
          GET editchoice PICTURE "!"
   READ NOUPDATE
   CLEAR GETS
   @ 14,0 SAY clearline
   DO CASE
      CASE editchoice = " "
      * ---Exit.
         RETURN
      CASE editchoice = "P"
      * ---(P)osition
         STORE "X" TO poschoice
         DO WHILE poschoice <> " "
            DO AS-posn
         ENDDO
      CASE editchoice = "D"
      * ---(D)elete
         DELETE
         @ 1,55 SAY "DELETED"
      CASE editchoice = "U"
      * ---(U)ndelete
         RECALL
         @ 1,55 SAY "       "
      CASE editchoice = "E"
      * ---(E)dit
         @ 14,0 SAY "Press <control-W> to exit"
         IF # <> 0
            DO AS-some
            READ
         ENDIF
      CASE editchoice = "C"
      * ---(C)ontinue to the next record.
         STORE # TO lastrecord
         CONTINUE
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
            @ 15,0 SAY "Weiter mit einer Taste ......";
                   GET select
            READ NOUPDATE
            @ 14,0 SAY clearline
            @ 15,0 SAY clearline
         ENDIF
   ENDCASE
ENDDO
* EOF: AS-EDIT.CMD
