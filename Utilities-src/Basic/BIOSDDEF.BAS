1    REM interaktive BIOS-Aenderung (DISKDEF-Leiste)
2    REM Programmname: BIOSDDEF.BAS
3    REM Richard Mauch Mannheimer Str. 12 7514 Eggenstein-Leopoldshafen
5    ADR = 60002!   'ADRESSE DPB EA62H
6    START = 0      'RUECKSPRUNG INS BIOS
10   PRINT CHR$(26)
20   PRINT "                          Interaktive Biosaenderung"
21   PRINT "                          ========================="
22   PRINT 
30   PRINT "Eingegeben werden die Werte aus der 'DISKDEF'-Leiste."
40   PRINT "Geaendert wird der DISK-Parameter-Block fuer Laufwerk B:"
60   PRINT
70   PRINT "Eingabe:"
80   LINE INPUT "FSC (1.logischer Sektor)              : ";FSC$:FSC = VAL(FSC$)
90   LINE INPUT "LSC (letzter logischer Sektor)        : ";LSC$:LSC = VAL(LSC$)
100  LINE INPUT "SKF (Skew-Faktor, 0 fuer 5 1/4 Zoll)  : ";SKF$:SKF = VAL(SKF$)
110  LINE INPUT "BLS (Block Size -1024,2048,4096,8192) : ";BLS$:BLS = VAL(BLS$)
120  LINE INPUT "DKS (Disk-Kapazitaet in BLS)          : ";DKS$:DKS = VAL(DKS$)
130  LINE INPUT "DIR (max. Direktory-Eintraege)        : ";DIR$:DIR = VAL(DIR$)
140  LINE INPUT "CKS (ueberpruefte Directory-Eintraege): ";CKS$:CKS = VAL(CKS$)
150  LINE INPUT "OFS (reservierte Systemspuren)        : ";OFS$:OFS = VAL(OFS$)
160  POKE &H40,78
170  LINE INPUT "Druckerausgabe (Eingabewerte und Speicher)  (J/N) : ";DR$
180  LINE INPUT "40-Spur-Laufwerk (J/N) : ";LW$
200  IF FSC = 0 THEN SPT = LSC+1 ELSE SPT = LSC
210  SPT1 = SPT\256:SPT2 = SPT MOD 256
220  IF SKF = 0 THEN 230
221  PRINT:PRINT "Disketten mit SKEW-Faktor koennen nicht bearbeitet werden!"
222  FOR N = 1 TO 3000:NEXT:GOTO 1
230  DSM1 = DKS\256:DSM2 = DKS MOD 256
240  DRM1 = DIR\256:DRM2 = DIR MOD 256
250  CKS1 = (CKS/4)\256:CKS2 = (CKS/4) MOD 256
260  OFF1 = OFS\256:OFF2 = OFS MOD 256
270  IF BLS = 1024 THEN BSH = 3:BLM = 7:IF DSM < 256 THEN EXM = 0
280  IF BLS = 2048 THEN BSH = 4:BLM = 15:IF DSM =< 255 THEN EXM = 1 ELSE EXM = 0
290  IF BLS = 4096 THEN BSH = 5:BLM = 31:IF DSM =< 255 THEN EXM = 3 ELSE EXM = 1
300  IF BLS = 8192 THEN BSH = 6:BLM = 63:IF DSM =< 255 THEN EXM = 7 ELSE EXM = 3
350  HI = DIR*32/BLS
360  AL1 = 0
370  IF HI = 1 THEN AL0 = &H80
380  IF HI = 2 THEN AL0 = &HC0
390  IF HI = 4 THEN AL0 = &HF0
400  IF HI = 8 THEN AL0 = &HFF
450  POKE ADR+0,SPT2
460  POKE ADR+2,BSH
470  POKE ADR+3,BLM
480  POKE ADR+4,EXM
490  POKE ADR+5,DSM2
500  POKE ADR+6,DSM1
510  POKE ADR+7,DRM2
520  POKE ADR+8,DRM1
530  POKE ADR+9,AL0
540  POKE ADR+10,AL1
550  POKE ADR+11,CKS2
560  POKE ADR+13,OFF2
570  POKE ADR+14,OFF1
580  IF LW$ = "J" OR LW$ = "j" THEN POKE &H40,74
590  IF DR$ <> "J" AND DR$ <> "j" THEN 1000
595  LPRINT:LPRINT "Drive B:":LPRINT "========":LPRINT
600  LPRINT "FSC (1.logischer Sektor)              : ";:LPRINT USING ("#####");FSC
610  LPRINT "LSC (letzter logischer Sektor)        : ";:LPRINT USING ("#####");LSC
620  LPRINT "SKF (Skew-Faktor, 0 fuer 5 1/4 Zoll)  : ";:LPRINT USING ("#####");SKF
630  LPRINT "BLS (Block Size -1024,2048,4096,8192) : ";:LPRINT USING ("#####");BLS
640  LPRINT "DKS (Disk-Kapazitaet in BLS)          : ";:LPRINT USING ("#####");DKS
650  LPRINT "DIR (max. Direktory-Eintraege)        : ";:LPRINT USING ("#####");DIR
660  LPRINT "CKS (ueberpruefte Directory-Eintraege): ";:LPRINT USING ("#####");CKS
670  LPRINT "OFS (reservierte Systemspuren)        : ";:LPRINT USING ("#####");OFS
680  LPRINT
683  LPRINT "DISKDEF 2,";FSC;",";LSC;",";SKF;",";BLS;",";DKS;",";DIR;",";CKS;",";OFF:LPRINT
690  FOR N = 59904! TO 60159!
700  IF N/16 <> INT(N/16) THEN 720
710  LPRINT:LPRINT HEX$(N);"   ";
720  A$ = HEX$(PEEK(N)):IF PEEK(N) < 16 THEN A$ = "0" + A$
730  LPRINT A$;" ";
740  NEXT N
750  LPRINT:LPRINT
1000 CALL START
