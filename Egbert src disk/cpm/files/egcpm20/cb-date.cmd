* Program.: CB-DATE.CMD (.PRG)
* Author..: Luis A. Castro
* Date....: 8/2/83, 11/20/83.
* Notice..: Copyright 1983, ASHTON-TATE, All Rights Reserved.
*           May not be reproduced without permission.
* Notes...: Subroutine to verify a date.
* PARAMETERS------------------------------------------------+
*       name      typ len picture        description
*      ========== === === ==========  =======================
*      mdate       C   8  MM/DD/YY    Calendar date
* (or) mdate       C  10  MM/DD/YYYY  Calendar date
*      is:valid    L   1              Validation flag.
* ----------------------------------------------------------+
STORE F TO is:valid
IF 0 = TEST(MDATE) .OR.;
   ( LEN(mdate) <> 8 .AND. LEN(mdate) <> 10 ) .OR.;
   @(" ",mdate) > 0 .OR. @("-",mdate) > 0 .OR.;
   @(".",mdate) > 0 .OR. @("+",mdate) > 0
   * ---The memory variable "mdate" must exist and
   * ---must not contain special characters.
   RETURN
ENDIF
*
STORE VAL( $(mdate,1,2) ) TO t:month
STORE VAL( $(mdate,4,2) ) TO t:day
IF LEN(mdate) = 8
   STORE VAL( $(mdate,7,2) ) TO t:year
ELSE
   STORE VAL( $(mdate,7,4) ) TO t:year
ENDIF
*
DO CASE
   CASE t:month < 1 .OR. t:month > 12 .OR. t:day < 1 .OR.;
      t:day > VAL( $("312931303130313130313031",;
      ( t:month - 13*INT(t:month/13))*2 - 1, 2 ) )
      *
   CASE LEN(mdate)= 8 .AND. t:month=2 .AND.;
      t:day > 28 .AND. t:year/4 > int(t:year/4)
      *
   CASE LEN(mdate)=10 .AND. t:month=2 .AND.;
      t:day > 28 .AND. ( (t:year/4 > INT(t:year/4) .AND.;
      t:year/100 = INT(t:year/100)) .OR.;
      t:year/400 > INT(t:year/400) )
      *
   OTHERWISE
      STORE T TO is:valid
ENDCASE
*
RELEASE t:month,t:day,t:year
RETURN
* EOF: CB-DATE.PRG (.CMD)

