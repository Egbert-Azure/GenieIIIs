*                      STATUS AENDERN
*
*                    use cpmuser.dbf
erase
clear
set talk off
set eject off
*
*
store " " to wahl
*
do while T


@ 5,1 SAY "Ausgabe der Userliste"
@ 6,1 SAY "---------------------"
@ 7,0
@ 8,0
@ 9,1  SAY "A = Alle User           I = Nur freie Adressen der User"
@ 10,1 SAY "C = Clubmitglieder      T = Telefonliste "
@ 11,1 SAY " "
@ 12,3 SAY "Q = QUIT"
@ 14,0
@ 14,10 SAY "Bitte w{hlen Sie" GET wahl Picture "A"
    read

DO CASE
CASE !(Wahl) = "A"
   do ALLPRINT

CASE !(WAHL) ="I"
   do Freilist
   return

CASE !(WAHL) ="C"
   do CLUBLISTE
   return

CASE !(Wahl) = "Q"
   return

 ENDCASE
ENDDO


? Kuhn                     	 H Loesch                   
 Q Mauch                     Z M}ller                    c Oppmann                   l Rehn                      u Ruschinski                ~ Schober                   � Schr|er                   � Sommerlatte               � Thiel                     � Wagner        