erase
set talk off
STORE "  " to wahl

DO WHILE T
erase
text



             Programm for working with the USER-List of
                         Fritz Chwolka




             A -- Ausdruck
             B -- Bearbeiten
             E -- Ende

Endtext

wait to wahl
do case
    CASE !(wahl) = "A"
         do KWDRUCK
    CASE !(wahl) = "B"
          do bearbeit
    CASE !(WAHL) = "E"
          RETURN
    ENDCASE
ENDDO

    ',1,teil1) to ze21
*31
store len(ze31) to wert
sto