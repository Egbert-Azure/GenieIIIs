* Program.: CB-REPRT.CMD (.PRG)
* Author..: Anonymous
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Print reports          
DO WHILE T
   ERASE
   @  2, 0 SAY "========================================"
   @  2,40 SAY "========================================"
   @  3, 0 SAY "||"
   @  4, 0 SAY "========================================"
   @  4,40 SAY "========================================"
   @  5, 0 SAY "||"
   @  6, 0 SAY "||"
   @  7, 0 SAY "||"
   @  8, 0 SAY "||"
   @  9, 0 SAY "||"
   @ 10, 0 SAY "||"
   @ 11, 0 SAY "========================================"
   @ 11,40 SAY "========================================"
   @ 10,78 SAY "||"
   @  9,78 SAY "||"
   @  8,78 SAY "||"
   @  7,78 SAY "||"
   @  6,78 SAY "||"
   @  5,78 SAY "||"
   @  3,78 SAY "||"
   @ 3,34 SAY "REPORTS MENU"
   @ 6,25 SAY " 0 - Exit"
   @ 7,25 SAY " 1 - Checks"
   @ 8,25 SAY " 2 - Bank Deposits"
   @ 9,25 SAY " 3 - Individual Deposits"
   STORE 4 TO choice
   DO WHILE choice < 0 .OR. choice > 3
      STORE " " TO mchoice
      @ 11,33 SAY " select : : "
      @ 11,41 GET mchoice
      READ
      STORE VAL(mchoice) TO choice
   ENDDO
   STORE 6 TO row
   STORE 0 TO count
   DO WHILE count < 4
      @ row+count,25 SAY "                         "
      STORE count+1 TO count
   ENDDO
   IF choice = 0
      RELEASE choice,mchoice,row,count
      USE
      RETURN
   ENDIF
   @ 3,30 SAY "                    "
   IF choice = 1
      STORE "Checks" TO option
      STORE 31 TO col
      STORE 5 TO ub
      @ 11,0
      @ 11, 0 SAY "||"
      @ 11,78 SAY "||"
      @ 12, 0 SAY "||"
      @ 12,78 SAY "||"
      @ 13, 0 SAY "========================================"
      @ 13,40 SAY "========================================"
      @  7,24 SAY "               "
   ELSE
      IF choice = 2
         STORE "Bank Deposits" TO option
         STORE 28 TO col
         STORE 4 TO ub
         @ 11,0
         @ 11, 0 SAY "||"
         @ 11,78 SAY "||"
         @ 12, 0 SAY "========================================"
         @ 12,40 SAY "========================================"
         @  8,24 SAY "                    "
      ELSE
         STORE "Individual Deposits" TO option
         STORE 25 TO col
         STORE 3 TO ub
         @ 9,24 SAY "                              "
      ENDIF
   ENDIF
   @ 3,col SAY !(option) + " SUB-MENU"
   @ row+choice,24 SAY "                              "
   @ 6,20 SAY "0 - Return to REPORTS MENU"
   @ 7,20 SAY "1 - List all "+option
   @ 8,20 SAY "2 - List all "+option+" between two dates"
   @ 9,20 SAY "3 - List all "+option+" over a certain amount"
   IF choice = 1
      @ 10,20 SAY "4 - List all taxable checks"
      @ 11,20 SAY "5 - List certain taxable checks"
      USE Cb-check
      STORE "   CHKNO    DATE         PAY TO THE ORDER OF          AMOUNT";
            TO command1
      STORE "   -----  --------  ------------------------------  ----------";
            TO command2
      STORE "'    '+STR(Chkno,4,0)+'  '+Date+'  '+Payto+'  '+STR(Amt,10,2)";
            TO command
   ELSE
      IF choice = 2
         @ 10,20 SAY "4 - List all deposits in transit"
         USE Cb-bank
         STORE "     DATE      AMOUNT" TO command1
         STORE "   --------  ----------" TO command2
         STORE "'   '+Date+'  '+STR(Amt,10,2)" TO command
      ELSE
         USE Cb-depst
         STORE "     DATE            PAIDFROM             AMOUNT" TO command1
         STORE "   --------  -------------------------  ----------";
               TO command2
         STORE "'   '+Date+'  '+Paidfrom+'  '+STR(Amt,10,2)" TO command
      ENDIF
   ENDIF
   STORE 6 TO choice2
   DO WHILE choice2 < 0 .OR. choice2 > ub
      STORE " " TO mchoice2
      @ 8+ub,33 SAY " select : : "
      @ 8+ub,41 GET mchoice2
      READ
      STORE VAL(mchoice2) TO choice2
   ENDDO
   IF choice2 = 0
      * release some memory variables and return to the REPORTS MENU
      RELEASE option,col,ub,command1,command2,command,choice2,mchoice2
      LOOP
   ENDIF
   STORE " " TO printer
   @ 22,6 SAY "Do you want the output sent to the printer "+;
              "or the screen? (P/S) " GET printer PICTURE "!"
   READ
   @ 22,0
   IF printer = "P"
     SET PRINT ON
   ELSE
     STORE "S" TO printer
   ENDIF
   STORE row+ub+4 TO newrow
   IF choice2 = 1
      STORE "LIST ALL "+!(option) TO header
      ERASE
      ?
      ? "                        "+header
      ? command1
      ? command2
      GO TOP
      DO WHILE .NOT. EOF
         ? &command
         SKIP
      ENDDO
   ENDIF
   IF choice2 = 2
      STORE T TO sandy
      DO WHILE sandy
         STORE F TO is:valid
         DO WHILE .NOT. is:valid
            STORE "        " TO mdate
            @ newrow,23 SAY "Enter starting date ";
                            GET mdate PICTURE "##/##/##"
            READ
            DO CB-DATE
            IF is:valid
              STORE mdate TO date1
              STORE $(mdate,7,2)+$(mdate,1,2)+$(mdate,4,2) to begin
            ENDIF
         ENDDO is:valid
         STORE F TO is:valid
         DO WHILE .NOT. is:valid
           STORE "        " TO mdate
           @ newrow+2,23 SAY "Enter ending date   ";
                         GET mdate PICTURE "##/##/##"
           READ
           DO CB-DATE
           IF is:valid
             STORE mdate to date2
             STORE $(mdate,7,2)+$(mdate,1,2)+$(mdate,4,2) to ending
           ENDIF
         ENDDO is:valid
         IF begin > ending
           @ 20,13 SAY "Ending date occurs before starting date - "+;
                       "please reenter"
           LOOP
         ENDIF
         STORE F TO sandy
         ERASE
         STORE 0.00 TO amount
         GO TOP
         STORE "LIST ALL "+!(option)+" BETWEEN "+date1+" AND "+date2;
               TO header
         ?
         ? "            "+header
         ? command1
         ? command2
         DO WHILE .NOT. EOF
            STORE $(Date,7,2)+$(Date,1,2)+$(Date,4,2) TO mdate
            IF mdate > begin .AND. mdate < ending
               ? &command
               STORE amount + Amt TO amount
            ENDIF
            SKIP
         ENDDO
         IF choice = 1
            STORE "                                           " TO first
         ELSE
            IF choice = 2
               STORE "    " TO first
            ELSE
               STORE "                               " TO first
            ENDIF
         ENDIF
         ?
         ? first+"TOTAL   $"+STR(amount,10,2)
      ENDDO sandy
      RELEASE sandy,is:valid,mdate,date1,begin,date2,ending,amount,first
   ENDIF
   IF choice2 = 3
     STORE -1.00 TO amount
     DO WHILE amount < 0.00
       STORE 0.00 TO amount
       @ newrow,20 SAY "Enter in amount " GET amount
       READ
     ENDDO
     STORE newrow+3 TO newrow
     ERASE
     GO TOP
     STORE "LIST ALL "+!(option)+" OVER $"+STR(amount,10,2) TO header
     ?
     ? "             "+header
     ? command1
     ? command2
     DO WHILE .NOT. EOF
       IF Amt > amount
         ? &command
       ENDIF
       SKIP
     ENDDO
     RELEASE amount,answer
   ENDIF
   IF choice2 = 4 .AND. choice = 2
      STORE "LIST ALL DEPOSITS IN TRANSIT" TO header
      GO TOP
      LOCATE FOR .NOT. Clear
      IF EOF
         @ newrow,30 SAY "No Deposits in transit"
      ELSE
         ERASE
         STORE 0.00 TO amount
         ?
         ? "               "+header
         ? command1
         ? command2
         DO WHILE .NOT. EOF
            IF .NOT. Clear
               ? &command
               STORE amount + Amt TO amount
            ENDIF
            SKIP
         ENDDO
         ?
         ? "    TOTAL   $"+STR(amount,10,2)
      ENDIF
      RELEASE amount
   ENDIF
   IF choice = 1 .AND. choice2 > 3
      STORE "      Business expenses       " TO m1
      STORE "  Medical or Dental expenses  " TO m2
      STORE "Payments for Medicine or Drugs" TO m3
      STORE "       Alimony payments       " TO m4
      STORE "     Child Care expenses      " TO m5
      STORE "     Automotive expenses      " TO m6
      STORE "        Contributions         " TO m7
      STORE " Miscellaneous tax deductions " TO m8
      IF choice2 = 5
         @ newrow, 1 SAY "(1) Business expenses         (4) Alimony"
         @ newrow,55 SAY "(7) Contributions"
         @ newrow+1,1 SAY "(2) Medical or Dental         (5) Child Care"
         @ newrow+1,55 SAY "(8) Miscellaneous"
         @ newrow+2,1 SAY "(3) Medicine or Drugs         (6) Automotive"
         STORE 9 TO mtax
         DO WHILE mtax < 0 .OR. mtax > 8
            STORE " " TO taxchar
            @ newrow+4,6 SAY "Enter specific tax expense (or 0 to return "+;
                              "to the REPORTS MENU) " GET taxchar
            READ
            STORE VAL(taxchar) TO mtax
         ENDDO
         IF mtax = 0
            RELEASE option,col,ub,command1,command2,command,choice2,;
                    mchoice2,newrow,m1,m2,m3,m4,m5,m6,m7,m8,mtax,taxchar
            LOOP
         ENDIF
         STORE "m"+taxchar TO string2
         STORE "LIST ALL "+&string2 TO header
      ELSE
         STORE "LIST ALL TAXABLE CHECKS" TO header
      ENDIF
      ERASE
      ?
      ? "           " + header
      ?
      IF choice2 = 4
         ? "                     "+m1
      ENDIF
      ? command1
      ? command2
      IF choice2 = 4
         STORE 1 TO count
         STORE T TO again
         DO WHILE again
            STORE 0.00 TO amount
            GO TOP
            DO WHILE .NOT. EOF
               IF Tax = count
                  ? &command
                  STORE amount+Amt TO amount
               ENDIF
               SKIP
            ENDDO
            ?
            ? "                                          TOTAL    $" +;
              STR(amount,10,2)
            * need routine to keep track of totalling print out totals
            ?
            ?
            STORE count+1 TO count
            IF count > 8
               STORE F TO again
            ELSE
               STORE "m"+STR(count,1) TO string1
               ? "                   "+&string1
               ? command1
               ? command2
            ENDIF
         ENDDO again
      ELSE
         STORE 0.00 TO amount
         GO TOP
         DO WHILE .NOT. EOF
            IF Tax = mtax
               ? &command
               STORE amount+Amt TO amount
            ENDIF
            SKIP
         ENDDO
         ?
         ? "                                          TOTAL    $"+;
           STR(amount,10,2)
      ENDIF
      RELEASE m1,m2,m3,m4,m5,m6,m7,m8,amount
      IF choice2 = 4
         RELEASE again,string1
      ELSE
         RELEASE mtax,taxchar,string2
      ENDIF
   ENDIF
   IF printer = "P"
     SET PRINT OFF
   ELSE
     ?
     ?
   ENDIF
   STORE " " TO ret
   @ 23,15 SAY "Press any key to return to the REPORTS MENU " GET ret
   READ
   RELEASE option,col,ub,command1,command2,command,choice2,;
           mchoice2,newrow,header,printer,ret
ENDDO T
