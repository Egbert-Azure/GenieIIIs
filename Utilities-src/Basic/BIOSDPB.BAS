1    REM Interaktive Biosaenderung (DISK-PARAMETER-BLOCK)
2    REM Programmname: BIOSDPB.BAS
3    REM Richard Mauch Mannheimer Str. 12 7514 Eggenst.-Leopoldshafen
5    REM 100  ADR = 60002!      ' ADRESSE DPB LW B: EA62H
100  ADR = &H10DD      ' ADRESSE DPB LW B: 10DDH
110  START = 0        ' RUECKSPRUNG INS BIOS
120  PRINT CHR$(26):PRINT "                    Interaktive BIOS-Aenderung"
130  PRINT "                    =========================="
140  PRINT
150  PRINT "Eingegeben werden die Werte des DPB fuer Laufwerk B: "
160  PRINT "Diese Werte koennen dezimal oder sedezimal in der Form &Hxx eingegeben werden"
170  PRINT 
180  PRINT "Eingabe:
190  PRINT
1110 INPUT "SECTORS PER TRACK         : ";SPT2
1111 INPUT "                EXTENSION : ";SPT1
1120 INPUT "BLOCK SHIFT               : ";BSH
1130 INPUT "BLOCK MASK                : ";BLM
1140 INPUT "EXTENTION MASK            : ";EXM
1150 INPUT "DISK STORAGE MAX.         : ";DSM2
1151 INPUT "                EXTENSION : ";DSM1
1160 INPUT "DIRECTORY MAX.            : ";DRM2
1161 INPUT "                EXTENSION : ";DRM1
1170 INPUT "ALLOC.VECTOR 0            : ";AL0
1180 INPUT "ALLOC.VECTOR 1            : ";AL1
1190 INPUT "CHECKED DIRECTORY ENTRIES : ";CKS2
1191 INPUT "                EXTENSION : ";CKS1
1200 INPUT "OFFSET                    : ";OFF2
1201 INPUT "                EXTENSION : ";OFF1
1210 POKE &H40,78
1215 LINE INPUT "Druckerausgabe (Eingabewerte und Speicher) (J/N) : ";DR$
1220 INPUT "40-Spur-Laufwerk (J/N) : ";LW$
2010 POKE ADR+0,SPT2
2020 POKE ADR+1,SPT1
2030 POKE ADR+2,BSH
2040 POKE ADR+3,BLM
2050 POKE ADR+4,EXM
2060 POKE ADR+5,DSM2
2070 POKE ADR+6,DSM1
2080 POKE ADR+7,DRM2
2090 POKE ADR+8,DRM1
2100 POKE ADR+9,AL0
2110 POKE ADR+10,AL1
2120 POKE ADR+11,CKS2
2130 POKE ADR+12,CKS1
2140 POKE ADR+13,OFF2
2150 POKE ADR+14,OFF1
2160 IF LW$ = "J" OR LW$ = "j" THEN POKE &H40,74
2170 IF DR$ <>"J" AND DR$ <> "j" THEN 3000
2200 LPRINT "Drive B:":LPRINT "========":LPRINT
2210 LPRINT "Sectors per Track         : ";:LPRINT USING ("####");SPT2,
2211 A$ = HEX$(SPT2)+"H":IF SPT2 < 16 THEN A$ = "0" + A$
2212 LPRINT ,A$
2213 LPRINT "                EXTENSION : ";:LPRINT USING ("####");SPT1,
2214 A$ = HEX$(SPT1)+"H":IF SPT1 < 16 THEN A$ = "0" + A$
2215 LPRINT ,A$
2220 LPRINT "Block Shift               : ";:LPRINT USING ("####");BSH,
2221 A$ = HEX$(BSH)+"H":IF BSH < 16 THEN A$ = "0" + A$
2222 LPRINT ,A$
2230 LPRINT "Block Mask                : ";:LPRINT USING ("####");BLM,
2231 A$ = HEX$(BLM)+"H":IF BLM < 16 THEN A$ = "0" + A$
2232 LPRINT ,A$
2240 LPRINT "Extension Mask            : ";:LPRINT USING ("####");EXM,
2241 A$ = HEX$(EXM)+"H":IF EXM < 16 THEN A$ = "0" + A$
2242 LPRINT ,A$
2250 LPRINT "Disk Storage max.(Bloecke): ";:LPRINT USING ("####");DSM2,
2251 A$ = HEX$(DSM2)+"H":IF DSM2 < 16 THEN A$ = "0" + A$
2252 LPRINT ,A$
2253 LPRINT "                EXTENSION : ";:LPRINT USING ("####");DSM1,
2254 A$ = HEX$(DSM1)+ "H":IF DSM1 < 16 THEN A$ = "0" + A$
2255 LPRINT ,A$
2260 LPRINT "Directory Entries max.    : ";:LPRINT USING ("####");DRM2,
2261 A$ = HEX$(DRM2)+"H":IF DRM2 < 16 THEN A$ = "0" + A$
2262 LPRINT ,A$
2263 LPRINT "                EXTENSION : ";:LPRINT USING ("####");DRM1,
2264 A$ = HEX$(DRM1)+"H":IF DRM1 < 16 THEN A$ = "0"+A$
2265 LPRINT ,A$
2270 LPRINT "Alloc Vector 0            : ";:LPRINT USING ("####");AL0,
2271 A$ = HEX$(AL0)+"H":IF AL0 < 16 THEN A$ = "0" + A$
2272 LPRINT ,A$
2280 LPRINT "Alloc Vector 1            : ";:LPRINT USING ("####");AL1,
2281 A$ = HEX$(AL1)+"H":IF AL1 < 16 THEN A$ = "0" + A$
2282 LPRINT ,A$
2290 LPRINT "Checked Directory Entries : ";:LPRINT USING ("####");CKS2,
2291 A$ = HEX$(CKS2)+"H":IF CKS2 < 16 THEN A$ = "0" + A$
2292 LPRINT ,A$
2300 LPRINT "                EXTENSION : ";:LPRINT USING ("####");CKS1,
2301 A$ = HEX$(CKS1)+"H":IF CKS1 < 16 THEN A$ = "0" + A$
2302 LPRINT ,A$
2310 LPRINT "Offset                    : ";:LPRINT USING ("####");OFF2,
2311 A$ = HEX$(OFF2)+"H":IF OFF2 < 16 THEN A$ = "0" + A$
2312 LPRINT ,A$
2320 LPRINT "                EXTENSION : ";:LPRINT USING ("####");OFF1,
2321 A$ = HEX$(OFF1)+"H":IF OFF1 < 16 THEN A$ = "0" + A$
2322 LPRINT ,A$
2323 LPRINT
2330 LPRINT:LPRINT "BIOS im Speicherbereich EA00H bis EAFFH: "
2331 FOR N = 59904! TO 60159!
2332 IF N/16 <> INT (N/16) THEN 2334
2333 LPRINT:LPRINT HEX$(N);"   ";
2334 A$ = HEX$(PEEK(N)):IF PEEK(N) < 16 THEN A$ = "0" + A$
2335 LPRINT A$;" ";
2336 NEXT N
2337 LPRINT:LPRINT
3000 CALL START
