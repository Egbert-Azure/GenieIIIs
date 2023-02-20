
* Program..: AS-LOCAT.CMD
* Author...: Egbert Schr|er
* Date.....: 25/08/92
* Notice...: Copyright 1992, All Rights Reserved
*
STORE " " TO expression,string
* ---LOCATE section.
DO WHILE expression = " "
   @ 11,0 SAY 'EXAMPLE :STATE="CA"'
   @ 14,0 SAY clearline
   @ 12,0 SAY "_"
   ACCEPT "Enter LOCATE expression " TO expression
   @ 11,0 SAY clearline
   STORE TRIM(expression) TO expression
   DO CASE
      CASE expression = " "
      * ---Exit.
         RETURN
      CASE 0 = TEST(&expression)
      * ---INVALID EXPRESSION.
         @ 15,0 SAY clearline
         STORE " " TO select
         @ 15,0 SAY "AUSDRUCK FEHLERHAFT: "+;
                    "Weiter mit einer Taste ...... ";
                GET select
         READ NOUPDATE
         @ 15,0 SAY clearline
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
            SET INDEX TO ASVIND
            GOTO lastrecord
         ELSE
            * ---Reopen index file and
            * ---reset EOF marker to true (.T.).
            SET INDEX TO ASVIND
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
   @ 11,0 SAY "EXAMPLE :NAME+ADDRESS"
   @ 14,0 SAY "E"
   ACCEPT "Enter DISPLAY string " TO string
   @ 11,0 SAY clearline
   STORE TRIM(string) TO string
   DO CASE
      CASE string = " "
      * ---Exit.
         @ 14,0 SAY clearline
         @ 15,0 SAY clearline
         RETURN
      CASE 0 = TEST(&string)
      * ---INVALID EXPRESSION.
         @ 15,0 SAY clearline
         STORE " " TO select
         @ 15,0 SAY "INVALID DISPLAY EXPRESSION: "+;
                    "Strike any key to continue... ";
                GET select
         READ NOUPDATE
         @ 15,0 SAY clearline
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
   @ 20, 0 SAY line+hline+hline
   @ 20,40 SAY line+hline
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
   @ 21,0 SAY "Weiter mit einer Taste ...... ";
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
* EOF: AS-LOCAT.CMD
