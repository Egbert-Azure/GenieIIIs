* Program.: CB-CHECK.CMD (.PRG)
* Author..: Anonymous
* Date....: February 9, 1984
* Notice..: Copyright 1984, Ashton-Tate and RSP, Inc.
* Notes...: Enter checks
*           Called from CB-MAIN.CMD (.PRG)
* enter new check data
USE Cb-check
* draw the check mask on the screen
DO Cb-mask
STORE T TO loop1
DO WHILE loop1
   * loop1 is designed to continue entering checks into the database
   @  1,60 SAY "LAST CHECK #"+STR(lastchk,4,0)
   * initialize memory variables
   STORE lastchk+1 TO mchkno
   STORE "        " TO mdate
   STORE "                              " TO mpayto
   STORE 0 TO mamt
   STORE "                         " TO mmemo
   STORE F TO mcan
   STORE T TO loop2
   DO WHILE loop2
      * loop2 is designed to enter and reenter correct data
      STORE T TO loop3
      DO WHILE loop3
         * loop3 is designed to check for duplicate check numbers
         @  4,42 SAY "(Enter 0 to exit)"
         @  4,70 GET mchkno PICTURE "####"
         READ
         @  4,42 SAY "                 "
         @ 18, 0
         IF mchkno = 0
            USE
            RELEASE loop1,loop2,loop3,answer,is:valid,valid,ans,taxchar,;
                    mchkno,mdate,mpayto,mamt,mmemo,mcan,mtax,word1,word2
            RELEASE amt:full,length,amt:word,is:error,count,true,string
            RETURN
         ENDIF
         * test for negative check number
         IF mchkno < 0
            STORE lastchk+1 TO mchkno
            LOOP
         ENDIF
         * check for duplicate check number
         LOCATE FOR Chkno=mchkno
         IF .NOT. EOF
            @ 18,15 SAY "Check number already exists.  Please reenter"
         ELSE
            STORE F TO loop3
            @ 18,0
         ENDIF
      ENDDO loop3
      STORE F TO is:valid
      DO WHILE .NOT. is:valid
         @  6,67 GET mdate PICTURE "##/##/##"
         READ
         * check for valid date
         DO Cb-date
         IF .NOT. is:valid
            @ 18,25 SAY "Invalid date - please reenter"
         ELSE
            @ 18, 0
         ENDIF
      ENDDO
      STORE F TO valid
      DO WHILE .NOT. valid
         * test for name on check - no blank checks allowed
         @  8,24 GET mpayto
         READ
         IF mpayto = "                              "
            @ 18,15 SAY "No blank checks allowed - please reenter"
         ELSE
            @ 18, 0
            STORE T TO valid
         ENDIF
      ENDDO
      STORE F TO valid
      DO WHILE .NOT. valid
        * test for amount less than or equal to 0
        @  8,65 GET mamt PICTURE "#######.##"
        READ
        IF mamt = 0.00
          @ 18,15 SAY "Check must have an amount - please reenter       "
        ELSE
          IF mamt < 0.00
            @ 18,15 SAY "Check must be a positive amount - "+;
                        "please reenter"
          ELSE
            @ 18, 0
            * put numbers into words!!
            STORE mamt TO amt:full
            DO Cb-nbwd
            IF is:error
              @ 20,20 SAY "Number is too large - please reenter"
            ELSE
              STORE T TO valid
            ENDIF
          ENDIF
        ENDIF
      ENDDO
      IF balance < mamt
        @ 18,10 SAY "There are not sufficient amount of funds "+;
                    "to cover this check."
        * erase current check display
        @ 6,67 SAY "          "
        @ 8,24 SAY "                                "
        @ 8,65 SAY "            "
        STORE lastchk+1 TO mchkno
        STORE "        " TO mdate
        STORE "                              " TO mpayto
        STORE 0.00 TO mamt
        LOOP
      ENDIF
      * display words on check
      STORE "                                                              ";
            TO string
      STORE LEN(amt:word) TO length
      IF length > 75
         STORE 74 TO count
         STORE T TO true
         DO WHILE true
            IF $(amt:word,count,1)=" " .OR. $(amt:word,count,1)="-"
               STORE F TO true
            ELSE
               STORE count-1 TO count
            ENDIF
         ENDDO
         STORE $(amt:word,1,count)+$(string,1,75-count) TO word1
         STORE $(amt:word,count+1,length-count) TO word2
         @ 11,4 SAY word1
         @ 12,10 SAY word2
      ELSE
         STORE amt:word + $(string,1,75-length) TO word1
         @ 11,4 SAY word1
         @ 12,10 SAY "                                 "
      ENDIF
      * enter memo data
      @ 14, 9 GET mmemo
      READ
      @ 18,0
      STORE " " TO answer
      @ 18,10 SAY "IS THE ABOVE CHECK CORRECT? "+;
                  "[ (Y)es / (N)o / (A)bort ] " GET answer PICTURE "!"
      READ
      CLEAR GETS
      @ 18,0
      IF answer$"YA "
        STORE F TO loop2
      ENDIF
   ENDDO loop2
   IF answer $ "Y "
      APPEND BLANK
      REPLACE Chkno WITH mchkno,Date WITH mdate,Payto WITH mpayto,;
              Amt WITH mamt,Memo WITH mmemo,Can WITH mcan
      STORE balance - Amt TO balance
      STORE Chkno TO lastchk
      STORE " " TO answer
      @ 18,5 SAY "Do you need a record of this check for income"+;
                 " tax purposes? [Y/N] " GET answer PICTURE "!"
      READ
      IF answer $ "Y "
         @ 18, 0
         @ 18, 1 SAY "(1) Business expenses         (4) Alimony"
         @ 18,55 SAY "(7) Contributions"
         @ 19, 1 SAY "(2) Medical or Dental         (5) Child Care"
         @ 19,55 SAY "(8) Miscellaneous"
         @ 20, 1 SAY "(3) Medicine or Drugs         (6) Automotive"
         STORE 9 TO mtax
         DO WHILE mtax < 0 .OR. mtax > 8
            * enter respective tax deduction
            STORE " " TO taxchar
            @ 22, 5 SAY "Enter number which corresponds to tax "+;
                        "deduction (or 0 for none) " GET taxchar
            READ
            STORE VAL(taxchar) TO mtax
         ENDDO
      ELSE
         STORE 0 TO mtax
      ENDIF
      REPLACE Tax WITH mtax
      @ 18,0
      @ 19,0
      @ 20,0
      @ 22,0
   ENDIF
   STORE " " TO answer
   @ 18,10 SAY "Would you like to enter another check? [Y/N] ";
           GET answer PICTURE "!"
   READ
   @ 18,0
   IF answer = "N"
      STORE F TO loop1
   ELSE
      @  6,67 SAY "          "
      @  8,24 SAY "                                "
      @  8,65 SAY "            "
      @ 14, 9 SAY "                           "
      @ 11, 4 SAY "__________________________________________________"
      @ 11,54 SAY "_______________Dollars"
      @ 12,10 SAY "                              "
   ENDIF
ENDDO loop1
USE
RELEASE loop1,loop2,loop3,answer,is:valid,valid,ans,taxchar,mchkno,mdate,;
        mpayto,mamt,mmemo,mcan,mtax
RELEASE amt:full,length,amt:word,is:error,count,true,word1,word2,string
RETURN
* EOF: CB-CHECK.CMD (.PRG)
