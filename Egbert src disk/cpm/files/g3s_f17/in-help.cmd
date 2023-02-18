* Program..: IN-HELP.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program uses the TEXT ENDTEXT commands to
*	     present text information to the user.
*
*
ERASE
@ 2,0 SAY "Inventory Program Help Facility"
@ 3,0 SAY equals
TEXT

Option #1, Run Inventory Report, will produce a report on 6 of the 10 fields
in the IN-MAIN database. The report is intended to provide the user with raw
information regarding the company's current inventory. The report will con-
trol-break on the first three characters of the PRODUCT NUMBER field and will
subtotal all numeric fields at that point. You will be prompted prior to the
report whether or not output should be directed to the screen or the printer.

Option #2, Reorder Information, will produce a report which contains all the
items that should be reordered. dBASE will flag an item for reorder if the
quantity ON HAND plus the quantity ON ORDER is less than the REORDER POINT.
The report will display the distributor's name and phone number and will also
suggest the minimum amount of any one item to order. You will be prompted
prior to the report whether or not output should be directed to the screen or
the printer.

ENDTEXT
@ 23,0 SAY "Strike any key to continue..."
SET CONSOLE OFF
WAIT
SET CONSOLE ON

ERASE
@ 2,0 SAY "Inventory Program Help Facility"
@ 3,0 SAY equals
TEXT

Option #3, Update/View Inventory Database, allows the user to view as well as
modify individual records in the database. Two index files allow the user to
quickly find a particular record based on either the product description or
the product number. Records can be marked for deletion and later permanently
removed.

Option #4, Summary of Inventory Totals, will display aggregate totals for the
inventory.

Option #5, Run File Mantenance Routine, will permanently remove all records
that have previously been marked for deletion. The user will be prompted be-
fore any action is taken.

ENDTEXT
@ 23,0 SAY "Strike any key to continue..."
SET CONSOLE OFF
WAIT
SET CONSOLE ON
RETURN
*EOF IN-HELP.PRG