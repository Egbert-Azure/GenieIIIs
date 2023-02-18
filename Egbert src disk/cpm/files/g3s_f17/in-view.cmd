* Program..: IN-VIEW.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program allows the user only to view selected records
*	     in the file without the fear of modifying the data.
ERASE
@ 1,0 SAY "Inventory View"
@ 2,0 SAY line
@ 19,0 SAY line
IF # = 0
   @ 5,0 SAY "Sorry, there are no records in the database"
   STORE 1 TO count
   DO WHILE count < 50
      STORE count + 1 TO count
   ENDDO
   RELEASE count
   ERASE
   RETURN
ENDIF
SET INDEX TO
GO TOP
DO WHILE T
   STORE " " TO pause
   STORE T TO badans
   STORE " " TO mchoice
   IF *
     @ 1,50 SAY "DELETED"
   ELSE
     @ 1,50 SAY "       "
   ENDIF
   @ 1,72 SAY DATE()
   @ 5,0 SAY "Product Number:"
   @ 5,16 SAY Prd:num
   @ 7,0 SAY "Product Description:"
   @ 7,21 SAY Prd:desc
   @ 9,0 SAY "Product Cost:"
   @ 9,14 SAY Prd:cost USING "999,999.99"
   @ 11,0 SAY "Product Price:"
   @ 11,15 SAY Prd:price USING "999,999.99"
   @ 13,0 SAY "Quantity On Hand:"
   @ 13,19 SAY On:hand USING "99999"
   @ 13,46 SAY "Quantity On Order:"
   @ 13,65 SAY On:order USING "99999"
   @ 15,0 SAY "Reorder Point:"
   @ 15,15 SAY Reodr:pt USING "99999"
   @ 15,46 SAY "Reorder Date:"
   @ 15,62 SAY Reodr:dte USING "XX/XX/XX"
   @ 17,0 SAY "Distributor:"
   @ 17,13 SAY Distrib
   @ 17,46 SAY "Phone:"
   @ 17,53 SAY Phone USING "(XXX) XXX-XXXX XXXX"
   @ 21,30 SAY "(F)ind, (S)kip:"
   DO WHILE badans
      @ 21,45 GET mchoice PICTURE "!"
      READ
      IF mchoice$"FS "
	 STORE F TO badans
      ENDIF
   ENDDO
   @ 21,0
   DO CASE
      CASE mchoice = "S"
	SKIP
	IF .NOT. EOF
	   LOOP
	ELSE
	   @ 21,27 SAY "This is the last record"
	   GO BOTTOM
	   STORE 1 TO count
	   DO WHILE count < 50
	      STORE count + 1 TO count
	   ENDDO
	   RELEASE count
	   @ 21,0
	   LOOP
	ENDIF
      CASE mchoice = "F"
	STORE T TO badans
	STORE " " TO mchoice
	@ 21,22 SAY "Find on (D)escription or (N)umber?"
	DO WHILE badans
	   @ 21,57 GET mchoice PICTURE "!"
	   READ
	   IF mchoice$"DN"
	      STORE F TO badans
	   ENDIF
	ENDDO
	@ 21,0
	IF mchoice = "N"
	   STORE # TO holdrec
	   SET INDEX TO In-pnum,In-pdesc
	   STORE "   " TO key
	   @ 22,14 SAY "Enter first three digits of product number:"
	   @ 22,59 GET key PICTURE "XXX"
	   READ
	   @ 22,0
	   STORE TRIM(key) TO key
	   FIND '&key'
	   IF # = 0
	      @ 22,5 SAY "Item number " + "&key" + " was not found"
	      @ 22,44 SAY "(Strike any key to continue...)"
	      @ 22,75 GET pause PICTURE "X"
	      READ
	      @ 22,0
	      GOTO holdrec
	      LOOP
	   ELSE
	      LOOP
	   ENDIF
	ELSE
	   STORE # TO holdrec
	   SET INDEX TO In-pdesc,In-pnum
	   STORE "                         " TO key
	   @ 22,14 SAY "Enter description:"
	   @ 22,34 GET key PICTURE "XXXXXXXXXXXXXXXXXXXXXXXXX"
	   READ
	   @ 22,0
	   STORE TRIM(key) TO key
	   FIND '&key'
	   IF # = 0
	      @ 22,0 SAY "Item " + "&key" + " was not found"
	      @ 22,47 SAY "(Strike any key to continue...)"
	      @ 22,78 GET pause PICTURE "X"
	      READ
	      @ 22,0
	      GOTO holdrec
	      LOOP
	   ELSE
	      LOOP
	   ENDIF
	ENDIF
      CASE mchoice = " "
	RELEASE holdrec,pause,key,mchoice,badans
	RETURN
  ENDCASE
ENDDO
* EOF IN-VIEW.PRG