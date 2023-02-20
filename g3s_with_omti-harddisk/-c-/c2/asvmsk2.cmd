
* Program..: ASVMSK2.CMD
* Author...: Egbert Schr|er
* Date.....: 31/08/92
* Notice...: Copyright 1992, All Rights Reserved
* Notes....: Maske 2
* Reserved.:
*  Auswahl-Men} 2 -> Bearbeitung

@  3, 15 SAY chr(138)+line+chr(137)
@  4, 15 SAY vline+"                                      "
@  4, 54 SAY vline+chr(151)
@  5, 15 SAY chr(132)+line+chr(131)+chr(151)
@  6, 15 SAY vline+"                                      "
@  6,54 SAY vline+chr(151)
@  7,15 SAY vline+"                                      "
@  7,54 SAY vline+chr(151)
@  8,15 SAY vline+"                                      "
@  8,54 SAY vline+chr(151)
@  9,15 SAY vline+"                                      "
@  9,54 SAY vline+chr(151)
@ 10,15 SAY vline+"                                      "
@ 10,54 SAY vline+chr(151)
@ 11,15 SAY vline+"                                      "
@ 11,54 SAY vline+chr(151)
@ 12,15 SAY vline+"                                      "
@ 12,54 SAY vline+chr(151)
@ 13,15 SAY vline+"                                      "
@ 13,54 SAY vline+chr(151)
@ 14,15 SAY vline+"                                      "
@ 14,54 SAY vline+chr(151)
@ 15,15 SAY vline+"                                      "
@ 15,54 SAY vline+chr(151)
@ 16,15 SAY chr(136)+line+chr(135)+chr(151)
@ 17,16 SAY schatten
@  4, 16 SAY " A S V   Men} Bearbeitung"
@  7,25 SAY " 0. Zur}ck"
@  8,25 SAY " 1. Anzeige"
@  9,25 SAY " 2. Neueingabe"
@ 10,25 SAY " 3. Editieren"
@ 11,25 SAY " 4. L|schen"
SET INTENSITY ON
@ 14, 23 SAY " Auswahl: : "
set intensity off

* EOF: ASVMSK2.CMD