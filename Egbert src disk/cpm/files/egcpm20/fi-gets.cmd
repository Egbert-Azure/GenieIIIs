
* Program..: FI-GETS.CMD
* Author...: Your Name
* Date.....: 00/00/00
* Notice...: Copyright 1900, All Rights Reserved
*
IF *
   @ 1,55 SAY "DELETED"
ELSE
   @ 1,55 SAY "       "
ENDIF
@  4,11 GET Date      
@  5,11 GET Input     
@  6,11 GET Output    
@  7,11 GET Grund     
@  8,11 GET Club      
RETURN
* EOF: FI-GETS.CMD
"
@  8, 0 SAY "Club......:"
@ 11, 0 SAY "----------------------------------------"
@ 11,40 SAY "----------------------------------------"
RETURN
* EOF: FI-FRAME.