use finanz index date
set alternate to finanz
set alternate on
go top


store "    " to ausgabe
store "    " to rein
store "    " to raus
do while .not. eof

sum input for club ="J" to rein
? date
? "Einname"
? rein
sum output for club ="J" to raus
? date
? "ausgabe"
store raus - rein to ausgabe
? "Gesammt"
? gesammt
enddo

+ Liste                                  J 28.06.90   0.00   3.20Thiel,Brief + Liste                                         J 28.06.90   5.00   3.20Littmann ,In