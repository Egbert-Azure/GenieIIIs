
* Program..: CP-EDIT.CMD
* Author...: Your Name
* Date.....: 03/10/93
* Notice...: Copyright 1993, All Rights Reserved
*
DO WHILE T
   STORE " " TO editchoice
   @ 16,0 SAY "COMMAND: (E)dit, (D)elete, (U)ndelete, "+;
              "(C)ontinue, (P)osition ";
          GET editchoice PICTURE "!"
   READ NOUPDATE
   CLEAR GETS
   @ 16,0 SAY clearline
   DO CASE
      CASE editchoice = " "
      * ---Exit.
         RETURN
      CASE editchoice = "P"
      * ---(P)osition
         STORE "X" TO poschoice
         DO WHILE poschoice <> " "
            DO CP-posn
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
         @ 16,0 SAY "Press <control-W> to exit"
         IF # <> 0
            DO CP-some
            READ
         ENDIF
      CASE editchoice = "C"
      * ---(C)ontinue to the next record.
         STORE # TO lastrecord
         CONTINUE
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
   ENDCASE
ENDDO
* EOF: CP-EDIT.CMD
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