* Program..: AS-GETS.CMD
* Author...: Egbert Schr|er
* Date.....: 25/08/92
* Notice...: Copyright 1992, All Rights Reserved
*

IF *
   @ 1,55 SAY "Gel|scht"
ELSE
   @ 1,55 SAY "        "
ENDIF
@  4,11 GET Name      
@  5,11 GET Strasse   
@  6,11 GET PLZ
@  6,18 GET Ort       
@  7,11 GET Eintr PICTURE "##.##.##"     
@  8,11 GET Geb   PICTURE "##.##.##"    
@  9,11 GET Gesch
@ 10,11 Get LFVNr 
RETURN
* EOF: AS-GETS.CMD
