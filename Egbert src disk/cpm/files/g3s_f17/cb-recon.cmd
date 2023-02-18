* Program.: CB-RECON.CMD (.PRG)
* Author..: Anonymous
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Reconcile the bank statement
ERASE
* total outstanding checks
USE Cb-check
SUM Amt TO outstand FOR .NOT. Can
@  2,15 SAY " PROGRAM TO RECONCILE WITH BANK STATEMENT"
@  3,15 SAY " ----------------------------------------"
* total deposits in transit
USE Cb-bank
SUM Amt TO notclear FOR .NOT. Clear
STORE -1.00 TO bank:bal
STORE T TO loop1
DO WHILE loop1
   DO WHILE bank:bal < 0.00
      STORE 0.00 TO bank:bal
      @  5,5 SAY "Enter ending balance from bank statement  $ " GET bank:bal
      READ
   ENDDO
   STORE " " TO answer
   @ 7,25 SAY "Is this correct? [Y/N] " GET answer PICTURE "!"
   READ
   @ 7,0
   IF answer $ "Y "
      STORE F TO loop1
   ELSE
      STORE -1.00 TO bank:bal
   ENDIF
ENDDO
* erase colons surrounding bank:bal input
@  5,49 SAY " "
@  5,60 SAY " "
@  7,18 SAY "ADD: "+STR(lastdep,2,0)+" deposits in transit  $"
@  7,50 SAY notclear
@  8,49 SAY "------------"
@  9,49 SAY bank:bal+notclear
@ 11,11 SAY "SUBTRACT: Total outstanding checks  $"
@ 11,50 SAY outstand
@ 12,49 SAY "------------"
IF outstand > 0.00
   @ 11,49 SAY "<"
   @ 11,61 SAY ">"
ENDIF
STORE bank:bal+notclear-outstand TO truebal
@ 13,27 SAY "True cash balance = $"
@ 13,50 SAY truebal
@ 15,27 SAY "Checkbook balance = $"
@ 15,50 SAY balance
* initialize memory variables
STORE F TO decision
IF truebal = balance
   STORE T TO equal
   @ 19,14 SAY "Checkbook and bank statement exactly balance"
   STORE " " TO answer
   @ 21,16 SAY "Press any key to return to main menu " GET answer PICTURE "!"
   READ
   RELEASE outstand,notclear,bank:bal,loop1,answer,truebal,decision,equal
   USE
   RETURN
ELSE
   STORE F TO equal
   @ 18,14 SAY "Bank Statement and Checkbook do not balance"
   USE Cb-depst
   IF truebal > balance
      STORE T TO greater
   ELSE
      STORE F TO greater
   ENDIF
ENDIF
STORE F TO is:valid
DO WHILE .NOT. is:valid
   STORE "        " TO mdate
   @ 20,17 SAY "Enter Bank Statement Date  " GET mdate PICTURE "##/##/##"
   READ
   @ 21,20
   DO Cb-date
   IF .NOT. is:valid
      @ 21,20 SAY "Invalid date - please reenter"
   ELSE
     STORE " " TO answer
     @ 21,22 SAY "Is this correct? [Y/N] " GET answer PICTURE "!"
     READ
     @ 21,20
     IF answer = "N"
       STORE F TO is:valid
     ENDIF
   ENDIF
ENDDO
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
@ 11, 0 SAY "||"
@ 12, 0 SAY "||"
@ 13, 0 SAY "||"
@ 14, 0 SAY "========================================"
@ 14,40 SAY "========================================"
@ 13,78 SAY "||"
@ 12,78 SAY "||"
@ 11,78 SAY "||"
@ 10,78 SAY "||"
@  9,78 SAY "||"
@  8,78 SAY "||"
@  7,78 SAY "||"
@  6,78 SAY "||"
@  5,78 SAY "||"
@  3,78 SAY "||"
@  3,27 SAY "BANK CREDITS AND CHARGES"
@  5,17 SAY "Credits"
@  5,57 SAY "Charges"
@  6,17 SAY "-------"
@  6,57 SAY "-------"
@ 12, 4 SAY "------------------------------------"
@ 12,40 SAY "------------------------------------"
@  5,40 SAY "|"
@  6,40 SAY "|"
@  7,40 SAY "|"
@  8,40 SAY "|"
@  9,40 SAY "|"
@ 10,40 SAY "|"
@ 11,40 SAY "|"
@ 13, 4 SAY "True cash balance"
@ 13,43 SAY "Checkbook balance"
@  8, 4 SAY "Interest earned"
@  8,43 SAY "Service charges"
@  9, 4 SAY "Bank collections"
@  9,43 SAY "Collection fees"
@ 10, 4 SAY "Miscellaneous addition"
@ 10,43 SAY "Miscellaneous charge"
@ 13,25 SAY truebal
DO WHILE .NOT. equal .AND. .NOT. decision
   @ 13,65 SAY balance
   STORE balance TO test
   STORE 1 TO count
   STORE 8 TO row
   STORE 27 TO col
   STORE 0.00 TO n1,n2,n3,n4,n5,n6
   STORE -1.00 TO amount
   DO WHILE count < 7
      DO WHILE amount < 0.00
         STORE 0.00 TO amount
         @ row,col GET amount
         READ
      ENDDO
      IF count = 1
         STORE amount TO n1
      ELSE
         IF count = 2
            STORE amount TO n2
         ELSE
            IF count = 3
               STORE amount TO n3
            ELSE
               IF count = 4
                  STORE amount TO n4
               ELSE
                  IF count = 5
                     STORE amount TO n5
                  ELSE
                     STORE amount TO n6
                  ENDIF 5
               ENDIF 4
            ENDIF 3
         ENDIF 2
      ENDIF 1
      IF count < 4
         STORE test + amount TO test
      ELSE
         STORE test - amount TO test
      ENDIF
      IF amount > 0
         @ 13,65 SAY test
      ENDIF
      STORE count+1 to count
      IF count = 4
         STORE 8 TO row
         STORE 65 TO col
      ELSE
         STORE row + 1 TO row
      ENDIF
      STORE -1.00 TO amount
      CLEAR GETS
   ENDDO
   STORE " " TO answer
   @ 16,20 SAY "Are the above entries correct? [Y/N] ";
           GET answer PICTURE "!"       
   READ
   IF answer = "N"
      @ 16, 0
      @  8,27 SAY "            "
      @  9,27 SAY "            "
      @ 10,27 SAY "            "
      @  8,65 SAY "            "
      @  9,65 SAY "            "
      @ 10,65 SAY "            "
      LOOP
   ENDIF
   IF truebal = test
      @ 18,17 SAY "Checkbook and bank statement exactly balance"
      STORE T TO equal
   ELSE
      @ 18,15 SAY "Checkbook and bank statement still do not balance"
      IF truebal > test
         STORE truebal - test TO diff
      ELSE
         STORE test - truebal TO diff
      ENDIF
      @ 19,23 SAY "The difference is $"+STR(diff,10,2)
      STORE " " TO answer
      @ 21,11 SAY "Do you still want to include the above changes? [Y/N] ";
              GET answer PICTURE "!"
      READ
      IF answer = "N "
         USE
         RELEASE outstand,notclear,bank:bal,loop1,answer,truebal,decision,;
                 equal,greater,is:valid,mdate,test,count,row,col,n1,n2,n3
         RELEASE n4,n5,n6,amount,diff
         RETURN
      ELSE
         STORE T TO decision
      ENDIF
   ENDIF
ENDDO
IF n1 > 0
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH n1,Paidfrom WITH "Interest earned"
ENDIF
IF n2 > 0
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH n2,Paidfrom WITH "Bank Collections"
ENDIF
IF n3 > 0
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH n3,Paidfrom WITH "Miscellaneous addition"
ENDIF
IF n4 > 0
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH -n4,Paidfrom WITH "Service charge"
ENDIF
IF n5 > 0
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH -n5,Paidfrom WITH "Collection fees"
ENDIF
IF n6 > 0
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH -n6,Paidfrom WITH "Miscellaneous charges"
ENDIF
STORE test - balance TO mamt
IF mamt <> 0
   USE Cb-bank
   APPEND BLANK
   REPLACE Date WITH mdate,Amt WITH mamt,Num WITH 0,Clear WITH T
ENDIF
STORE test TO balance
USE
RELEASE outstand,notclear,bank:bal,loop1,answer,truebal,decision,equal,;
        greater,is:valid,mdate,test,count,row,col,n1,n2,n3,n4,n5,n6
RELEASE amount,mamt,diff
RETURN
* EOF: CB-RECON.CMD (.PRG)
