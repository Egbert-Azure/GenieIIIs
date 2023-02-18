* Program..: IN-ADD.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program allows the user to add records to the file.
*	     If the first field is blank, no record will be added and
*	     the program is exited.
*
ERASE
DO In-init
@ 1,0 SAY "Inventory Add"
@ 1,72 SAY DATE()
@ 2,0 SAY line
@ 20,0 SAY line
STORE T TO more:rec

DO WHILE more:rec
   @ 5,0 SAY "Product Number:"
   @ 7,0 SAY "Product Description:"
   @ 7,21 SAY mprd:desc
   @ 9,0 SAY "Product Cost:"
   @ 9,14 SAY mprd:cost
   @ 11,0 SAY "Product Price:"
   @ 11,15 SAY mprd:price
   @ 13,0 SAY "Quantity On Hand:"
   @ 13,19 SAY mon:hand USING "99999"
   @ 13,46 SAY "Quantity On Order:"
   @ 13,65 SAY mon:order USING "99999"
   @ 15,0 SAY "Reorder Point:"
   @ 15,15 SAY mreodr:pt USING "99999"
   @ 15,46 SAY "Reorder Date:"
   @ 15,62 SAY mreodr:dte
   @ 17,0 SAY "Distributor:"
   @ 17,13 SAY mdistrib
   @ 17,46 SAY "Phone:"
   @ 17,53 SAY mphone
   STORE T TO datechng
   CLEAR GETS
   @ 5,16 GET mprd:num PICTURE "XXX-XXXXX"
   READ
   IF mprd:num = " "
      STORE F TO more:rec
      LOOP
   ENDIF
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
   CLEAR GETS
   STORE " " TO action
   @ 22,0 SAY "Is this data correct? [Y/N]" GET action PICTURE "!"
   READ
   @ 22,0
   IF action <> "Y"
      LOOP
   ENDIF
   APPEND BLANK
   REPLACE Prd:num WITH mprd:num, Prd:desc WITH mprd:desc, Prd:cost WITH ;
	   mprd:cost, Prd:price WITH mprd:price, On:hand WITH mon:hand ;
	   On:order WITH mon:order
   REPLACE Reodr:pt WITH mreodr:pt, Reodr:dte WITH mreodr:dte, Distrib WITH ;
	   mdistrib, Phone WITH mphone
   DO In-init
   STORE " " TO action
   @ 22,0 SAY "Add another record? [Y/N]" GET action PICTURE "!"
   READ
   @ 22,0
   IF !(action) = "Y"
      LOOP
   ENDIF
   STORE F TO more:rec
ENDDO
RELEASE action
RELEASE ALL LIKE m*
RETURN
* EOF IN-ADD.PRG