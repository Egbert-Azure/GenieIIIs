
* Program..: ASVMSK1.CMD
* Author...: Egbert Schr|er
* Date.....: 31/08/92
* Notice...: Copyright 1992, All Rights Reserved
* Notes....: Hauptprogramm
* Reserved.: select, selectnum

*  Auswahl-Men} 1 -> Bearbeitung

@  1, 0 SAY chr(138)+line+chr(137)
@  2, 0 SAY vline
@  2,39 SAY vline+chr(151)
@  3, 0 SAY chr(132)+line+chr(131)+chr(151)
@  4, 0 SAY vline
@  4,39 SAY vline+chr(151)
@  5, 0 SAY vline
@  5,39 SAY vline+chr(151)
@  6, 0 SAY vline
@  6,39 SAY vline+chr(151)
@  7, 0 SAY vline
@  7,39 SAY vline+chr(151)
@  8, 0 SAY vline
@  8,39 SAY vline+chr(151)
@  9, 0 SAY vline
@  9,39 SAY vline+chr(151)
@ 10, 0 SAY vline
@ 10,39 SAY vline+chr(151)
@ 11, 0 SAY vline
@ 11,39 SAY vline+chr(151)
@ 12, 0 SAY vline
@ 12,39 SAY vline+chr(151)
@ 13, 0 SAY vline
@ 13,39 SAY vline+chr(151)
@ 14, 0 SAY vline
@ 14,39 SAY vline+chr(151)
@ 15, 0 SAY chr(136)+line+chr(135)+chr(151)
@ 16, 1 SAY schatten
@  2, 3 SAY "A S V    D O R S T E N    E . V ." 
@  6, 2 SAY hand+" 0. Ende"
@  7, 5 SAY " 1. Bearbeiten"
@  8, 5 SAY " 2. Listen"
@  9, 5 SAY " 3. Adressaufkleber"
@ 10, 5 SAY " 4. Auswertung Landessportbund NW"
@ 11, 5 SAY " 7. Hilfe"
SET INTENSITY ON
@ 15, 10 SAY " Auswahl: : "
set intensity off

* EOF: ASVMSK1.CMD