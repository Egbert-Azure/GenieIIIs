* Program.: CB-CANCL.CMD (.PRG)
* Author..: Anonymous
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Enter cancelled checks
*           Called from CB-MENU.CMD (.PRG)
* enter cancelled check data
*
USE Cb-check
* draw check mask
DO Cb-mask
@  1,26 SAY "CHECK CANCELLATION ROUTINE"
STORE T TO more
DO WHILE more
   * more is designed for repeated check cancellations
   STORE 0 TO mcan
   * input check number to be cancelled
   @ 20, 0
   @ 18,11 SAY "Enter Check Number to be cancelled (or 0 to exit)";
           GET mcan PICTURE "####"
   READ
   * testing for exit condition
   IF mcan = 0
      STORE F TO more
      LOOP
   ENDIF
   * search for cancelled check number
   LOCATE FOR Chkno = mcan
   IF .NOT. Can .AND. .NOT. EOF
      * if not already cancelled and not at end of file, display check data
      @  4,71 SAY Chkno
      @  6,68 SAY Date
      @  8,25 SAY Payto
      @  8,66 SAY Amt PICTURE "#######.##"
      @ 14,10 SAY Memo
      @ 18, 0
      STORE " " TO answer
      @ 18,23 SAY "Is this the right check? [Y/N] " GET answer PICTURE "!"
      READ
      IF answer = "Y"
        REPLACE Can WITH T
      ENDIF
   ELSE
      * if not located, clear entry from previous check on screen
      @  4,71 SAY "    "
      @  6,68 SAY "        "
      @  8,25 SAY "                              "
      @  8,66 SAY "          "
      @ 14,10 SAY "                         "
      IF EOF
         * check is not in file
         @ 20,21 SAY "Check "+STR(mcan,4)+" cannot be found."
      ELSE
         * check is already cancelled
         @ 20,19 SAY "Check "+STR(mcan,4)+" is already cancelled."
      ENDIF
      STORE " " TO wait
      @ 21,20 SAY "Press any key to continue " GET wait
      READ
      @ 20, 0
      @ 21, 0
   ENDIF
   CLEAR GETS
ENDDO more
ERASE
* display to the screen a list of remaining uncleared checks
? "                 LIST OF REMAINING UNCLEARED CHECKS"
?
? "CHKNO             PAID TO               "+;
  "AMOUNT     DATE        MEMO"
? "----- ------------------------------- ---------- -------- --------------"
DISPLAY OFF ALL FOR .NOT. Can Chkno, Payto, Amt, Date, TRIM(Memo)
?
?
STORE " " TO answer
@ 22,13 SAY "Press any key to return to the main menu " GET answer
READ
* close out open files and variables
USE
RELEASE mcan,answer,more,wait
RETURN
* EOF: CB-CANCL.CMD (.PRG)
