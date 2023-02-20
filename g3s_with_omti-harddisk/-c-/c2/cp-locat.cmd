
* Program..: CP-LOCAT.CMD
* Author...: Your Name
* Date.....: 03/10/93
* Notice...: Copyright 1993, All Rights Reserved
*
STORE " " TO expression,string
* ---LOCATE section.
DO WHILE expression = " "
   @ 14,0 SAY 'EXAMPLE :STATE="CA"'
   @ 16,0 SAY clearline
   *
   @ 15,0 SAY "-"
   ACCEPT "Enter LOCATE expression " TO expression
   @ 14,0 SAY clearline
   STORE TRIM(expression) TO expression
   DO CASE
      CASE expression = " "
      * ---Exit.
         RETURN
      CASE 0 = TEST(&expression)
      * ---INVALID EXPRESSION.
         @ 17,0 SAY clearline
         STORE " " TO select
         @ 17,0 SAY "INVALID EXPRESSION: "+;
                    "Strike any key to continue... ";
                GET select
         READ NOUPDATE
         @ 17,0 SAY clearline
         STORE " " TO expression
      OTHERWISE
      * ---LOCATE the record.
         * ---Close index file for a faster LOCATE.
         SET INDEX TO
         LOCATE FOR &expression
         IF .NOT. EOF
            * ---Found a matching record.
            STORE # TO lastrecord
            * ---Reopen index file.
            SET INDEX TO CPMIND
            GOTO lastrecord
         ELSE
            * ---Reopen index file and
            * ---reset EOF marker to true (.T.).
            SET INDEX TO CPMIND
            GO BOTTOM
            SKIP
         ENDIF
   ENDCASE
ENDDO
IF poschoice = "L"
   * ---Return to calling program if only the LOCATE was desired.
   RETURN
ENDIF
*
* ---DISPLAY section.
STORE " " TO string
DO WHILE string = " "
   @ 14,0 SAY "EXAMPLE :NAME+ADDRESS"
   @ 16,0 SAY "E"
   ACCEPT "Enter DISPLAY string " TO string
   @ 14,0 SAY clearline
   STORE TRIM(string) TO string
   DO CASE
      CASE string = " "
      * ---Exit.
         @ 16,0 SAY clearline
         @ 17,0 SAY clearline
         RETURN
      CASE 0 = TEST(&string)
      * ---INVALID EXPRESSION.
         @ 17,0 SAY clearline
         STORE " " TO select
         @ 17,0 SAY "INVALID DISPLAY EXPRESSION: "+;
                    "Strike any key to continue... ";
                GET select
         READ NOUPDATE
         @ 17,0 SAY clearline
         STORE " " TO string
   ENDCASE
ENDDO
* ---Now, DISPLAY the expression.
STORE F TO is:eof,is:some
DO WHILE .NOT. is:eof
   * ---The following set of dBASE II commands are to 
   * ---clear-to-end-of-screen.  If you have an IBM-PC,
   * ---you can replace these commands with the single
   * ---command,  @ 4,0 ERASE
   *
   @  4,0 SAY clearline
   @  5,0 SAY clearline
   @  6,0 SAY clearline
   @  7,0 SAY clearline
   @  8,0 SAY clearline
   @  9,0 SAY clearline
   @ 10,0 SAY clearline
   @ 11,0 SAY clearline
   @ 12,0 SAY clearline
   @ 13,0 SAY clearline
   @ 14,0 SAY clearline
   @ 15,0 SAY clearline
   @ 16,0 SAY clearline
   @ 17,0 SAY clearline
   @ 18,0 SAY clearline
   @ 19,0 SAY clearline
   @ 20,0 SAY clearline
   @ 21,0 SAY clearline
   @ 22,0 SAY clearline
   @ 20, 0 SAY "----------------------------------------"
   @ 20,40 SAY "----------------------------------------"
   STORE 4 TO row
   DO WHILE .NOT. EOF .AND. row-3 <= 15
      STORE T TO is:some
      @ row,0 SAY &string
      STORE row + 1 TO row
      CONTINUE
   ENDDO
   * ---A logical memory variable is used to detect the
   * ---end-of-file.  The EOF function cannot be used since
   * ---it is reset by the READ statement below.
   STORE EOF TO is:eof
   IF .NOT. is:some
      * ---No matching records.
      @ 4,0 SAY "*** NO MATCHING RECORDS ***"
   ENDIF
   STORE " " TO select
   @ 21,0 SAY "Strike any key to continue... ";
          GET select
   READ NOUPDATE
   @ 21,0 SAY clearline
ENDDO
* ---The following set of dBASE II commands are to 
* ---clear-to-end-of-screen.  If you have an IBM-PC,
* ---you can replace these commands with the single
* ---command,  @ 4,0 ERASE
*
@  4,0 SAY clearline
@  5,0 SAY clearline
@  6,0 SAY clearline
@  7,0 SAY clearline
@  8,0 SAY clearline
@  9,0 SAY clearline
@ 10,0 SAY clearline
@ 11,0 SAY clearline
@ 12,0 SAY clearline
@ 13,0 SAY clearline
@ 14,0 SAY clearline
@ 15,0 SAY clearline
@ 16,0 SAY clearline
@ 17,0 SAY clearline
@ 18,0 SAY clearline
@ 19,0 SAY clearline
@ 20,0 SAY clearline
@ 21,0 SAY clearline
@ 22,0 SAY clearline
GO TOP
RETURN
* EOF: CP-LOCAT.CMD
ßßßßßßßß¬«°­ºßÝßÝß«°ßŒ‹–‘˜¢
‹¤ßßßº±»¼¾¬º¢
‹¤º±»»°¢
‹¤ÕßÒÒÒ±ˆÓß»¶¬¯³¾¦ß‹—šßš‡šŒŒ–‘Ñ¢
‹¤¬«°­ºß¹ß«°ß–ŒÅš™Ó–ŒÅŒ’š¢
‹¤»°ß¨·¶³ºßÑ±°«Ñß–ŒÅš™¢
‹¤ßßßÕßÒÒÒ«—šß™““ˆ–‘˜ßŒš‹ß™ß›½¾¬ºß¶¶ßœ’’ž‘›Œßžšß‹ß¢
‹¤ßßßÕßÒÒÒœ“š