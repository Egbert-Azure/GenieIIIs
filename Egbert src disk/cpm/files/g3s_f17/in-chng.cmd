* Program..: IN-CHNG.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program allows the user to change the contents of
*	     any record in the file.
STORE T TO more:rec
STORE Prd:num TO mprd:num
STORE Prd:desc TO mprd:desc
STORE Prd:cost TO mprd:cost
STORE Prd:price TO mprd:price
STORE On:hand TO mon:hand
STORE On:order TO mon:order
STORE Reodr:pt TO mreodr:pt
STORE Reodr:dte TO mreodr:dte
STORE Distrib TO mdistrib
STORE Phone TO mphone
DO WHILE more:rec
   CLEAR GETS
   STORE T TO datechng
   @ 5,16 GET mprd:num PICTURE "XXX-XXXXX"
   @ 7,21 GET mprd:desc
   @ 9,14 GET mprd:cost PICTURE "999,999.99"
   @ 11,15 GET mprd:price PICTURE "999,999.99"
   @ 13,19 GET mon:hand PICTURE "99999"
   @ 13,65 GET mon:order PICTURE "99999"
   @ 15,15 GET mreodr:pt PICTURE "99999"
   @ 15,62 GET mreodr:dte PICTURE "XX/XX/XX"
   DO In-date
   @ 17,13 GET mdistrib
   @ 17,53 GET mphone PICTURE "(XXX) XXX-XXXX XXXX"
   READ
   STORE " " TO action
   @ 22,0 SAY "Is this correct? [Y/N]" GET action PICTURE "!"
   READ
   @ 22,0
   IF action <> "Y"
      LOOP
   ENDIF
   REPLACE Prd:num WITH mprd:num, Prd:desc WITH mprd:desc, Prd:cost WITH ;
	   mprd:cost, Prd:price WITH mprd:price, On:hand WITH mon:hand ;
	   On:order WITH mon:order
   REPLACE Reodr:pt WITH mreodr:pt, Reodr:dte WITH mreodr:dte, Distrib WITH ;
	   mdistrib, Phone WITH mphone
STORE F TO more:rec
ENDDO
RELEASE action
RELEASE ALL LIKE m*
RETURN
* EOF IN-CHNG.PRG