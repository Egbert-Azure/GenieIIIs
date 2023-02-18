* Program.: CB-MAIN.CMD (.PRG)
* Author..: Anonymous
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Main menu for checkbook management system
* Turn off display of commands to the screen
SET TALK OFF
SET BELL OFF
SET INTENSITY OFF
* Pull retained variables from memory file CHXBOOK.MEM
RESTORE FROM Cb-book.mem
DO WHILE T
* DO WHILE T means DO WHILE TRUE  i.e. DO FOREVER
* The DO WHILE will be terminated by a RETURN command
   * clear the screen and display the main menu
   ERASE
   @  1, 0 SAY "========================================"
   @  1,40 SAY "========================================"
   @  2, 0 SAY "||"
   @  3, 0 SAY "========================================"
   @  3,40 SAY "========================================"
   @  4, 0 SAY "||"
   @  5, 0 SAY "||"
   @  6, 0 SAY "||"
   @  7, 0 SAY "||"
   @  8, 0 SAY "||"
   @  9, 0 SAY "||"
   @ 10, 0 SAY "||"
   @ 11, 0 SAY "||"
   @ 12, 0 SAY "||"
   @ 13, 0 SAY "||"
   @ 14, 0 SAY "||"
   @ 15, 0 SAY "========================================"
   @ 15,40 SAY "========================================"
   @ 14,78 SAY "||"
   @ 13,78 SAY "||"
   @ 12,78 SAY "||"
   @ 11,78 SAY "||"
   @ 10,78 SAY "||"
   @  9,78 SAY "||"
   @  8,78 SAY "||"
   @  7,78 SAY "||"
   @  6,78 SAY "||"
   @  5,78 SAY "||"
   @  4,78 SAY "||"
   @  2,78 SAY "||"
   @  2,12 SAY "C H E C K B O O K    M A N A G E M E N T    S Y S T E M"
   @  5,25 SAY " 0 - Exit"
   @  6,25 SAY " 1 - Enter new checks"
   @  7,25 SAY " 2 - Enter deposits"
   @  8,25 SAY " 3 - Enter cancelled checks"
   @  9,25 SAY " 4 - Enter cleared deposits"
   @ 10,25 SAY " 5 - Reconcile bank statement"
   @ 11,25 SAY " 6 - Reports"
   @ 12,25 SAY " 7 - Help"
   * display the current checkbook balance
   @ 18,20 SAY "Current Checkbook balance $ "+STR(balance,10,2)
   STORE 8 TO select
   * check for non-valid entry to menu
   DO WHILE select < 0 .OR. select > 7
      STORE " " TO mselect
      @ 15,33 SAY " select : : "
      @ 15,41 GET mselect PICTURE "#"
      READ
      * reading user response
      STORE VAL(mselect) TO select
   ENDDO
   * process user's response
   DO CASE
   * test for exit condition
      CASE select = 0
         RELEASE mselect,select
         * retain variables - 'balance', 'lastchk' and 'lastdep'
         SAVE TO Cb-book.mem
         * clear variables and return to calling program or dbase system
         CLEAR
         ERASE
         RETURN
   * test for new checks
      CASE select = 1
         DO Cb-check
   * test for new deposits
      CASE select = 2
         DO Cb-depst
   * test for cancelled checks
      CASE select = 3
         DO Cb-cancl
   * test for clearing deposits in transit
      CASE select = 4
         DO Cb-clear
   * test for reconcile with bank statement
      CASE select = 5
         DO Cb-recon
   * test for reports
      CASE select = 6
         DO Cb-reprt
   * test for help screens
      CASE select = 7
         DO Cb-help
      OTHERWISE
   ENDCASE
ENDDO
RETURN
* EOF: CB-MAIN.CMD (.PRG)
