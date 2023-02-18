erase
store "                       " to such
use user index name
go top

TEXT
            Bearbeiten der Userliste
            ------------------------


       A - Append
       F - Find  &  Edit




Endtext

Wait to WAHL

DO case

      Case !(Wahl) = "A"
           USE User index Name
           Append
           TEXT

           Wait While new Indexing


           endtext
           set talk on

           do indexneu

           set talk off

   	return
      Case !(Wahl) = "F"
           use User index name
           erase
           @ 5,10 SAY "Name nach dem gesucht werden soll ?" get such
            read
           find &such
           ? chr(07)
           edit #

      Endcase

return

