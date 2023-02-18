* Program..: IN-DTAIL.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*            May not be reproduced without permission.
* Notes....: This program produces a summary report based on the
*            present stock on hand.
ERASE
@ 2,0 SAY "Inventory Details"
@ 3,0 SAY equals
USE In-main
IF # = 0
   @ 5,0 SAY "There are no records in the database"
ELSE
   @ 5,13 SAY "Summing inventory details, please be patient..."
   SUM Prd:cost * On:hand, Prd:price * On:hand, On:hand, On:order TO ;
       xtotcost, xtotprice, xtotonhand, xtotorder
   STORE (xtotcost / xtotprice) * 100.00 TO xratio
   GO BOTTOM
   STORE # TO xitems
   @ 5,0
   @ 8,0 SAY xitems USING "Number of unique items:          99999"
   @ 9,0 SAY xtotonhand USING "Total number of items:          999999"
   @ 10,0 SAY xtotorder USING "Total number of items on order: 999999"
   @ 15,0 SAY xtotcost USING ;
     "Total cost of all inventoried items is:     9,999,999.99"
   @ 17,0 SAY xtotprice USING ;
     "Total price of all inventoried items is:    9,999,999.99"
   @ 19,0 SAY xratio USING ;
     "Ratio of inventory cost to selling price:    999.99%"
ENDIF
STORE " " TO xpause
@ 23,0 SAY "Strike any key to continue..." GET xpause
READ
@ 22,0
RELEASE ALL LIKE x*
USE
ERASE
RETURN
EOF IN-DTAIL.PRG