* Program..: IN-INIT.PRG
* Author(s): Roy M. Moore
* Date.....: 01-10-84
* Notice...: Copyright 1984, ASHTON-TATE, All Rights Reserved.
*	     May not be reproduced without permission.
* Notes....: This program initializes the memory varibles which are used
*	     to hold data until the data is placed in the file.
STORE "         " TO mprd:num
STORE "                         " TO mprd:desc
STORE 0 TO mprd:cost,mprd:price,mon:hand,mon:order,mreodr:pt
STORE "        " TO mreodr:dte
STORE "                    " TO mdistrib
STORE "                   " TO mphone
RETURN
* EOF IN-INIT.PRG