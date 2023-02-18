* Program.: CB-DEPST.CMD (.PRG)
* Author..: Anonymous        
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Enter deposits
*           Called from CB-MAIN.CMD
* enter new deposit data
USE Cb-depst
GO BOTTOM
STORE #+1 TO firstdep
* initialize logical memory variables
STORE F TO special,again
STORE T TO continue
DO WHILE continue
* continue loop is for continuing entering a deposit slip
   ERASE
   @  3, 0 SAY "+--------------------------------------------+"
   @  4, 0 SAY "|"
   @  5, 0 SAY "|"
   @  6, 0 SAY "|"
   @  7, 0 SAY "|"
   @  8, 0 SAY "|"
   @  9, 0 SAY "|"
   @ 10, 0 SAY "|"
   @ 11, 0 SAY "|"
   @ 12, 0 SAY "|"
   @ 13, 0 SAY "|"
   @ 14, 0 SAY "|"
   @ 15, 0 SAY "|"
   @ 16, 0 SAY "|"
   @ 17, 0 SAY "|"
   @ 18, 0 SAY "|"
   @ 19, 0 SAY "|"
   @ 20, 0 SAY "+--------------------------------------------+"
   @ 19,45 SAY "|"
   @ 18,45 SAY "|"
   @ 17,45 SAY "|"
   @ 16,45 SAY "|"
   @ 15,45 SAY "|"
   @ 14,45 SAY "|"
   @ 13,45 SAY "|"
   @ 12,45 SAY "|"
   @ 11,45 SAY "|"
   @ 10,45 SAY "|"
   @  9,45 SAY "|"
   @  8,45 SAY "|"
   @  7,45 SAY "|"
   @  6,45 SAY "|"
   @  5,45 SAY "|"
   @  4,45 SAY "|"
   @  6, 3 SAY "========================================"
   @  7,30 SAY "|"
   @  8,30 SAY "|"
   @  9,30 SAY "|"
   @ 10,30 SAY "|"
   @ 11,30 SAY "|"
   @ 12,30 SAY "|"
   @ 13,30 SAY "|"
   @ 14,30 SAY "|"
   @  2,18 SAY "DEPOSIT SLIP"
   @  5, 9 SAY "RECEIVED FROM"
   @  5,34 SAY "AMOUNT"
   @ 16,15 SAY "SUBTOTAL"
   * initialize memory variables
   STORE 0.00 TO subtotal,total,cash
   @ 16,32 SAY subtotal
   * enter date if first time through loop
   IF .NOT. again
      STORE F TO is:valid,c
      DO WHILE .NOT. is:valid
         STORE "        " TO mdate
         @ 4,5 SAY "ENTER DATE OF DEPOSIT " GET mdate PICTURE "##/##/##"
         READ
         @ 4,48
         IF mdate="  /  /  "
           @ 4,55 SAY "NO DATE ENTERED"
           STORE " " TO ans
           @ 5,48 SAY "Do you wish to abort? [Y/N] " GET ans PICTURE "!"
           READ
           IF ans = "Y"
             USE
             RELEASE firstdep,special,again,continue,subtotal,total,;
                     cash,is:valid,mdate,ans
             RETURN
           ENDIF
           @ 4,48
           @ 5,48
         ELSE
            DO Cb-date
            IF .NOT. is:valid
              @ 4,48 SAY "Invalid Date - please reenter"
            ELSE
              STORE " " TO answer
              @ 4,48 SAY "Is this correct? [Y/N] " GET answer PICTURE "!"
              READ
              @ 4,48
              IF answer = "N"
                STORE F TO is:valid
              ENDIF
            ENDIF
         ENDIF
      ENDDO is:valid
   ENDIF
   @ 4,4 SAY "    Date of Deposit:             "
   @ 4,25 SAY mdate
   STORE 7 TO line
   STORE F TO restart
   STORE T TO loop2
   DO WHILE loop2
   * loop2 is for updating incorrect deposit slip
      STORE T TO more
      DO WHILE more
      * more is for entering and updating individual deposit entries
         IF line > 14
            @ 16,15 SAY "                            "
            @ 20, 0
            @ 23, 0
            @  0, 0
            @ 19, 0 SAY "|"
            @ 19,45 SAY "|"
            @ 20, 0 SAY "+--------------------------------------------+"
            @ 14,30 SAY "|"
            @ 16,15 SAY "SUBTOTAL"
            @ 16,32 SAY subtotal
            STORE 14 TO line
         ENDIF
         * if second time through loop
         IF again
            IF .NOT. special
               STORE Paidfrom TO mpayfrom
               STORE Amt TO mamt
            ENDIF
            * display deposit information
            @ line,4 SAY mpayfrom
            @ line,32 SAY mamt
            IF .NOT. special
              STORE " " TO ans
              @ line,48 SAY "Is this correct? [Y/N] " GET ans PICTURE "!"
              READ
            ENDIF
            @ line,48
            CLEAR GETS
            IF ans = "N" .OR. special
              STORE " " TO an
              @ line,52 SAY "Do you wish to delete"
              @ line+1,50 SAY "this transaction? [Y/N] " GET an PICTURE "!"
              READ
              @ line,48
              @ line+1,48
              IF an = "Y"
                * erase current deposit from screen...
                @ line,3 SAY "                           "
                @ line,31 SAY "            "
                DELETE
                SKIP
                IF EOF
                  STORE F TO more
                ENDIF
                LOOP
              ELSE
                IF special
                  REPLACE Paidfrom WITH mpayfrom,Amt WITH mamt
                ENDIF
              ENDIF
            ELSE
              STORE subtotal + mamt TO subtotal
              @ 16,32 SAY subtotal
              SKIP
              IF EOF
                STORE F TO more
              ENDIF
              STORE line+1 TO line
              LOOP
            ENDIF
         ELSE
           STORE "                         " TO mpayfrom
           STORE 0.00 TO mamt
           STORE F TO loopamt
         ENDIF
         STORE F TO special
         * this routine is to enter deposit data
         @ line,3 GET mpayfrom
         READ
         IF mpayfrom="                         "
            @ line,3 SAY " "
            @ line,29 SAY " "
            @ line,31 SAY "            "
            IF again
               STORE T TO special
               LOOP
            ENDIF
            STORE F TO more
            LOOP
         ENDIF
         STORE T TO loopamt
         DO WHILE loopamt
            * make sure a positive amount is inputted
            @ line,31 GET mamt
            READ
            IF mamt <= 0
               STORE 0.00 TO mamt
            ELSE
               STORE F TO loopamt
            ENDIF
         ENDDO
         * display subtotal to screen
         STORE subtotal + mamt to test
         @ 16,32 SAY test
         STORE " " TO ans
         @ line,48 SAY "Is this correct? [Y/N] " GET ans PICTURE "!"
         READ
         @ line,48
         CLEAR GETS
         IF ans = "N"
           @ 16,32 SAY subtotal
           @ line,31 SAY "            "
           LOOP
         ENDIF
         * delete colons from the screen surrounding each input
         @ line,3 SAY " "
         @ line,29 SAY " "
         @ line,31 SAY " "
         @ line,42 SAY " "
         STORE subtotal + mamt TO subtotal
         * now add a record (if first time through)...
         IF .NOT. again
            APPEND BLANK
         ENDIF
         REPLACE Paidfrom WITH mpayfrom,Date WITH mdate,Amt WITH mamt
         IF again
           SKIP
           IF EOF
             STORE F TO more
           ENDIF
         ENDIF
         STORE line+1 TO line
      ENDDO more
      IF subtotal = 0
        @ line,52 SAY "NO DEPOSITS ENTERED"
        STORE " " TO ans
        @ line+1,48 SAY "Do you wish to abort? [Y/N] " GET ans PICTURE "!"
        READ
        @ line,52
        @ line+1,48
        IF ans = "Y"
          PACK
          USE
          RELEASE firstdep,special,again,continue,subtotal,total,cash,c,;
                  is:valid,mdate,line,restart,loop2,more,mpayfrom,mamt
          RELEASE loopamt,test,ans,answer,an
          RETURN
         ELSE
           IF again
             PACK
             STORE T TO restart
             STORE F TO loop2
           ELSE
             LOOP
           ENDIF
         ENDIF
      ELSE
         STORE F TO loop2
      ENDIF
   ENDDO loop2
   IF restart
      STORE F TO again,special
      RELEASE line,restart,loop2,more,mpayfrom,mamt,loopamt,test,answer,an
      LOOP
   ENDIF
   @ 18,30 SAY "-------------"
   @ 17,15 SAY "LESS CHANGE"
   STORE 0.00 TO total
   DO WHILE total <= 0.00
      @ 17,31 GET cash
      READ
      @ 22,0
      STORE subtotal-cash TO total
      IF total <= 0.00
         @ 22,10 SAY "Cash withdrawal exceeds deposit - please reenter"
      ENDIF
   ENDDO
   IF cash <> 0
      @ 17,31 SAY "<"
      @ 17,42 SAY " >"
   ELSE
      @ 17,31 SAY " "
      @ 17,42 SAY " "
   ENDIF
   @ 19,15 SAY "TOTAL DEPOSIT"
   @ 19,32 SAY total
   STORE " " TO answer
   @ 22, 6 SAY "Is the above deposit slip correct? "+;
               "[ (Y)es / (N)o / (A)bort ] " GET answer PICTURE "!"
   READ
   IF answer = "N"
      GOTO firstdep
      STORE T TO again
      LOOP
   ENDIF
   IF answer = "A"
      @ 22,0
      @ 22,12 SAY "ABORTING ABOVE DEPOSIT"
      GOTO firstdep
      DO WHILE .NOT. EOF
         DELETE
         SKIP
      ENDDO
      PACK
   ENDIF
   IF answer$"Y "
     STORE balance + total TO balance
     IF cash > 0
       APPEND BLANK
       REPLACE Date WITH mdate,Amt WITH -cash,Paidfrom WITH "Cash Withdrawal"
     ENDIF
     IF again
       PACK
       STORE F TO again
     ENDIF
     STORE lastdep+1 TO lastdep
     USE Cb-bank
     APPEND BLANK
     REPLACE Date WITH mdate,Amt WITH total,Num WITH lastdep,Clear WITH F
     USE Cb-depst
   ENDIF
   STORE " " TO answer
   @ 22,0
   @ 22, 3 SAY "Would you like to enter another deposit? [Y/N] ";
           GET answer PICTURE "!"
   READ
   IF answer = "N"
      STORE F TO continue
   ELSE
      GO BOTTOM
      STORE #+1 TO firstdep
      RELEASE line,restart,loop2,more,mpayfrom,mamt,loopamt,test,answer,an
   ENDIF
   CLEAR GETS
ENDDO continue
USE
RELEASE firstdep,again,continue,subtotal,total,cash,c,is:valid,mdate,;
        line,more,mpayfrom,mamt,loopamt,test,ans,an,answer
RELEASE special,loop2,restart
RETURN
* EOF: CB-DEPST.CMD (.PRG)