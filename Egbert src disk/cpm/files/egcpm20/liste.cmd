ERASE
SET TALK OFF
STORE "  " to wahl

DO WHILE WAHL <>"E"
erase
@  5,10 SAY 'Programm for working with the USER-List of '
@  6,10 SAY '              CLUB 80 '
@  8,10 SAY ' (created by Fritz Chwolka in 1989) '
@ 15,20 SAY 'A -- Ausdruck'
@ 16,20 SAY 'B -- Bearbeiten'
@ 18,20 SAY 'E -- Ende'
@ 19,0  SAY ' '

ACCEPT 'W{hlen Sie bitte !' TO Wahl

DO CASE
    CASE !(Wahl) = "A"
     DO KWDRUCK
    CASE !(Wahl) = "B"
     ERASE
     STORE " " TO such
erase
@  5,10 SAY 'Programm for working with the USER-List of '
@  6,10 SAY '              CLUB 80 '
@  8,10 SAY ' (created by Fritz Chwolka in 1989) '
@ 15,20 SAY 'Bearbeiten der USER Liste'
@ 16,20 SAY 'A - Append'
@ 17,20 SAY 'F - Find'
@ 18,20 SAY 'T - Teilliste erstellen'
@ 20,20 SAY 'E - ENDE'

ACCEPT 'W{hlen Sie bitte !' TO Wahl
  DO case
 Case !(Wahl) = "A"
  USE User index NAME,FREI,CLUB,PLZ,ORT,SYSTEM
         Append
        Case !(Wahl) = "F"
          USE User INDEX name
          ERASE
          STORE "                      " TO such
          @ 5,10 SAY "Name nach dem gesucht werden soll ?" get such
          READ
          FIND &such
          ? chr(07)
          edit #
         ENDCASE
       ENDCASE
     ENDDO




enddo

SAY 'A -- Ausdruck'
@ 16,20 SAY 'B -- Bearbeiten'
@ 18,20 SAY 'E -- Ende'
@ 19,0  SAY ' '

ACCEPT 'W{hlen Sie bitte !' TO Wahl

DO CASE
    CASE !(Wahl) = "A"
     DO KWDRUCK
    CASE !(Wahl) = "B"
     ERASE
     STORE " " TO such
erase
@  5,10 SAY 'Programm for wo