* Program.: CB-CLEAR.CMD (.PRG)
* Author..: Anonymous       
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Clear deposits
*           Called from CB-MENU.CMD (.PRG)
USE Cb-bank   
ERASE
@  1,20 SAY "Clear Deposits with Bank Statement"
@  2,20 SAY "----------------------------------"
@  3, 5 SAY "  Date       Amount  "
@  4, 5 SAY "--------   ----------"
IF lastdep>10
   @  3,45 SAY "  Date       Amount  "
   @  4,45 SAY "--------   ----------"
ENDIF
IF lastdep > 20
   STORE 20 TO stop
ELSE
   STORE lastdep TO stop
ENDIF
STORE 1 TO col,count
STORE 5 TO row
DO WHILE count <= stop
   LOCATE FOR Num=count
   @ row,col SAY Num
   @ row,col+4 SAY Date
   @ row,col+15 SAY Amt
   IF count = 10
      STORE 41 TO col
   ENDIF
   STORE count+1 TO count
   IF count > 10
      STORE count-6 TO row
   ELSE
      STORE count+4 TO row
   ENDIF
ENDDO
STORE T TO loop1
DO WHILE loop1
   STORE -1 TO number
   DO WHILE number < 0 .OR. number > stop
      STORE "  " TO mnumber
      @ 16,15 SAY "Enter number of corresponding deposit listed on"
      @ 17,21 SAY "bank statement (or 0 to exit) " GET mnumber
      READ
      STORE VAL(mnumber) TO number
   ENDDO
   @ 19,0
   IF number = 0
      STORE F TO loop1
      LOOP
   ENDIF
   IF number > 10
      STORE 39 TO col
      STORE number-6 TO row
   ELSE
      STORE 0 TO col
      STORE number+4 TO row
   ENDIF
   LOCATE FOR Num=number
   IF EOF
      @ 19,22 SAY "Already cleared - please reenter"
   ELSE
     STORE " " TO answer
     @ 19,25 SAY "Is this correct? [Y/N] " GET answer PICTURE "!"
     READ
     @ 19,0
     IF answer $ "Y "
       REPLACE Clear WITH T,Num WITH 0
       @ row,col SAY "                           "
     ELSE
       @ row,col SAY " "
     ENDIF
   ENDIF
   CLEAR GETS
ENDDO
STORE 0 TO count
LOCATE FOR .NOT. Clear
IF EOF
   @ 22,22 SAY "There are no deposits in transit"
   STORE 0 TO lastdep
ELSE
   STORE T TO loop2
   DO WHILE loop2
      STORE count+1 TO count
      REPLACE Num WITH count
      CONTINUE
      IF EOF
         STORE count TO lastdep
         STORE F TO loop2
      ENDIF
   ENDDO
   @ 22,22 SAY "There are "+STR(lastdep,2,0)+" deposits in transit"
ENDIF
USE
* release memory variables
RELEASE stop,col,row,count,loop1,number,mnumber,answer,loop2

* EOF: CB-CLEAR.CMD (.PRG)

