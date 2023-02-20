1   REM DATVER
2   REM Richard Mauch  Mannheimer Str. 12 7514 Eggenst.-Leopoldshafen
50  BP = 0
60  PRINT CHR$(26):PRINT:PRINT
70  PRINT "Bit - fuer Bit - Vergleich zweier Dateien - Rm 1989"
75  PRINT
80  PRINT "Start ist immer am Datei-Anfang auf der Diskette.
81  PRINT "Ausserdem wird die entsprechende Adresse im Debugger (z.B.DDT)"
82  PRINT "angezeigt."
85  PRINT
90  LINE INPUT "Dateiname Eingabedatei 1 (Mit Dateityp) ? ";FILE1$
100 LINE INPUT "Dateiname Eingabedatei 2 (Mit Dateityp) ? ";FILE2$
103 PRINT
104 PRINT "Endadresse mit Debugger, z.B.DDT, vorher feststellen"
105 PRINT "Eingegeben wird die zu NEXT gehoerende Adresse"
106 PRINT "Wenn groesser &H7FF, dann DEZIMAL eingeben!"
110 INPUT "Endadresse (HEX, Form &Hxxxx) : ";EADR
120 PRINT
130 PRINT "Druckerausgabe (J) ";:INPUT D$
160 OPEN "R",#1,FILE1$,1
170 FIELD 1,1 AS A$
180 OPEN "R",#2,FILE2$,1
190 FIELD 2,1 AS B$
200 GET 1,1
210 GET 2,1
240 PRINT "DISKADR";TAB(10);"DDTADR";TAB(30);FILE1$;TAB(50);FILE2$
245 IF D$ = "J" THEN LPRINT "DISKADR";TAB(10);"DDTADR";TAB(30);FILE1$;TAB(50);FILE2$
250 WHILE BP <= (EADR-&H100)
260    BP = BP+1
280 IF A$ = B$ THEN 350
300    AA$ = HEX$(ASC(A$)):BB$ = HEX$(ASC(B$))
301    IF LEN(AA$) = 1 THEN AA$ = "0"+AA$
302    IF LEN(BB$) = 1 THEN BB$ = "0"+BB$
310 BP$ = HEX$(BP-1+BIAS):BPD$ = HEX$(BP+255)
320 IF LEN(BP$) = 3 THEN BP$ = "0" + BP$
330 IF LEN(BPD$) = 3 THEN BPD$ = "0" + BPD$
340 PRINT BP$;TAB(10);BPD$;TAB(30);AA$;TAB(50);BB$
345 IF D$ = "J" THEN LPRINT BP$;TAB(10);BPD$;TAB(30);AA$;TAB(50);BB$
350    GET 1,BP+1
360    GET 2,BP+1
390 WEND
400 CLOSE
450 PRINT "ENDADRESSE : ";HEX$(BP);"H / ";HEX$(BP+128);"H":PRINT
460 PRINT "RECORDS    : ";(INT((BP/128)+.5)):PRINT
500 PRINT "DURCHGELAUFEN!!!"
 : ";HEX$(BP);"H / ";HEX$(BP+128