* Program..: CB-NBWD.CMD (.PRG)
* Author...: Tom Rettig
* Date.....: August 26, 1983
* Revised..: January 11, 1984
* Notice...: Copyright 1983, ASHTON-TATE, All Rights Reserved.
*            May not be reproduced without written permission.
* Notes....: Subroutine for converting a numeric dollar amount
*            (to $999,999.99) to words.
* Parameters passed:
*       name         type   length   description
*       ----------   ----   ------   ---------------------------
*   in: amt:full      N      3 - 9   Including 2 decimal places.
*  out: amt:word      C     23 -86   Depending on the amount.
*       is:error      L      1       .T. if error in conversion.
* Initialize memory variables...
STORE "ONE  TWO  THREEFOUR FIVE SIX  SEVENEIGHTNINE TEN  " TO t:unit
STORE "ELEVEN   TWELVE   THIRTEEN FOURTEEN FIFTEEN  SIXTEEN  "+;
      "SEVENTEEENEIGHTEEN NINETEEN " TO t:teen
STORE "TEN    TWENTY THIRTY FORTY  FIFTY  SIXTY  SEVENTYEIGHTY NINETY";
      TO t:decade
STORE " " TO amt:word
STORE F TO is:error
*
* Convert decimal places to a string containing cents amount...
STORE STR((amt:full-INT(amt:full))*100,2) TO t:cent:str
IF t:cent:str = " "
   STORE "0" + $(t:cent:str,2,1) TO t:cent:str
ENDIF
* Change tthe environment for upcoming branches...
SET EXACT ON
* Conditional branch...
DO CASE
   * Branch for amounts too high or too low...
   CASE amt:full > 999999.99 .OR. amt:full < 0.00
      STORE T TO is:error
      RELEASE ALL LIKE t:*
      SET EXACT OFF
      RETURN
   * Branch for zero dollars...
   CASE amt:full < 1.00
      STORE " NO " TO amt:word
   * Branch for other conditions...
   OTHERWISE
      * Convert dollar amount to a character string...
      STORE STR(INT(amt:full),6) TO t:amt:str
      * Branch for hundred thousands...
      IF $(t:amt:str,1,1) > "0"
         STORE $(t:amt:str,1,1) TO t:hunthous
         STORE amt:word+TRIM($(t:unit,(VAL(t:hunthous)-1)*5+1,5))+;
               " HUNDRED " TO amt:word
      ENDIF
      * Branch for ten-thousands and thousands...
      IF $(t:amt:str,2,2) > "  "
         STORE $(t:amt:str,2,1) TO t:tenthous
         STORE $(t:amt:str,3,1) TO t:thousand
         * Branch for combinations of ten-thousands and thousands...
         DO CASE
            CASE $(t:amt:str,1,1)>"0" .AND. VAL($(t:amt:str,2,2))=0
               STORE amt:word + "THOUSAND" TO amt:word
            CASE t:tenthous=" " .OR. t:tenthous="0"
               STORE amt:word+TRIM($(t:unit,(VAL(t:thousand)-1)*5+1,5))+;
                     " THOUSAND" TO amt:word
            CASE t:thousand="0"
               STORE amt:word+TRIM($(t:decade,(VAL(t:tenthous);
                     -1)*7+1,7))+" THOUSAND" TO amt:word
            CASE t:tenthous="1"
               STORE amt:word+TRIM($(t:teen,(VAL(t:thousand)-1)*9+1,9))+;
                     " THOUSAND" TO amt:word
            CASE t:tenthous>"1"
               STORE amt:word+TRIM($(t:decade,(VAL(t:tenthous);
                     -1)*7+1,7))+"-"+TRIM($(t:unit,(VAL(t:thousand);
                     -1)*5+1,5))+" THOUSAND" TO amt:word
         ENDCASE
         * Branch for comma or space after thousands...
         IF VAL($(t:amt:str,4,3)) > 0
            STORE amt:word + ", " TO amt:word
         ELSE
            STORE amt:word + " " TO amt:word
         ENDIF
      ENDIF
      * Branch for hundreds...
      IF $(t:amt:str,4,1) > "0"
         STORE $(t:amt:str,4,1) TO t:hundred
         STORE amt:word+TRIM($(t:unit,(VAL(t:hundred)-1)*5+1,5))+;
               " HUNDRED " TO amt:word
      ENDIF
      * Branch for tens and ones...
      IF VAL($(t:amt:str,5,2)) > 0
         STORE $(t:amt:str,5,1) TO t:tens
         STORE $(t:amt:str,6,1) TO t:ones
         * Branch for combinations of tens and ones...
         DO CASE
            CASE t:tens=" " .OR. t:tens="0"
               STORE amt:word+TRIM($(t:unit,(VAL(t:ones)-1)*5+1,5))+" ";
                     TO amt:word
            CASE t:ones="0"
               STORE amt:word+TRIM($(t:decade,(VAL(t:tens)-1)*7+1,7))+" ";
                     TO amt:word
            CASE t:tens="1"
               STORE amt:word+TRIM($(t:teen,(VAL(t:ones)-1)*9+1,9))+" ";
                     TO amt:word
            CASE t:tens>"1"
               STORE amt:word+TRIM($(t:decade,(VAL(t:tens)-1)*7+1,7));
                     +"-"+TRIM($(t:unit,(VAL(t:ones)-1)*5+1,5))+" ";
                     TO amt:word
            ENDCASE
         ENDIF
ENDCASE
* Branch for one dollar or more, and put the word string together
IF amt:word = " ONE"
   STORE $(amt:word,2,LEN(TRIM(amt:word))+1) + "DOLLAR and " +;
         t:cent:str + "CENTS" TO amt:word
ELSE
   STORE $(amt:word,2,LEN(TRIM(amt:word))+1) + "DOLLARS and " +;
         t:cent:str + " CENTS" TO amt:word
ENDIF
* Restore the environment and return to the calling program
RELEASE ALL LIKE t:*
SET EXACT OFF
RETURN
* EOF: CB-NBWD.CMD (.PRG)
