* Clublist - listet Mitglieder des Club80 aus userlist.dbf
erase
clear

set talk off
set alternate to Clublist
Set alternate  on

TEXT
.MT 0
.mb 12 
.h1_______________________________________________________________________
.H2                     CLUB 80 - Mitgliederliste                   
.H3
endtext

store  0 to zahl
store 0 to mkz
use user index Club
go top
find Club 80

Do while T
IF !(CLUB) <> "CLUB 80"
      return
   else
Store "                                                  " to ze11
Store "                                                  " to ze12
store "                                                  " to ze21
store "                                                  " to ze22
store "                                                  " to ze31
store "                                                  " to ze32
store "                                                  " to ze41
store "                                                  " to ze42
store "                                                  " to ze51
store "                                                  " to ze52
store "                                                  " to ze61
store "                                                  " to ze62

store 1 to mkz
store "  " to such

do while mkz <3 .and. mkz <> 0

   store "ze1"  +  str(mkz,1,0) to such
   store trim(vorname) + " " +TRIM(NAME) + "    "+ trim(CLUB) to &such
   store "ze2"  +  str(mkz,1,0) to such
   store strasse to &such
   store "ze3"  +  str(mkz,1,0) to such
   store  trim(land) + "-" + trim(PLZ) +" "+ ort to &such
   store "ze4"  +  str(mkz,1,0) to such
   store TELP to  &such
   store "ze5"  +  str(mkz,1,0) to such
   store trim(system) to &such

   store mkz + 1 to mkz

   skip
   if eof
      store 0 to mkz
   endif eof

enddo while mkz <3 .and. mkz <>0
*11
store len(ze11) to wert
store 40-wert to teil1
store ze11 +$('                                            ',1,teil1) to ze11
* 21
store len(ze21) to wert
store 40-wert to teil1
store ze21 +$('                                            ',1,teil1) to ze21
*31
store len(ze31) to wert
store 40-wert to teil1
store ze31 +$('                                            ',1,teil1) to ze31
*41
store len(ze41) to wert
store 40-wert to teil1
store ze41 +$('                                            ',1,teil1) to ze41
*51
store len(ze51) to wert
store 40-wert to teil1
store ze51 +$('                                            ',1,teil1) to ze51

? ze11+ ze12
? ze21+ ze22
? ze31+ ze32
? ze41+ ze42
? ze51+ ze52
? "------------------------------------------------------------------------"
endif
Enddo while .not. eof
? chr(13)
? chr(10)

return


