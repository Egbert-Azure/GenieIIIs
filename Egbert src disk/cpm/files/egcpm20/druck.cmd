use cpmuser
set talk off
store " " to zeile1
store " " to zeile2
store " " to zeile3

DO WHILE T
Store trim(vorname) + " " + trim(name) to zeile1
Store trim(strasse) to zeile2
store trim(plz) + " " + trim(ort) to zeile3

?
? "** D R U C K S A C H E **"
?
? zeile1
? zeile2
? zeile3
?
?
?
skip +1
enddo
return
































                                                                                                                  