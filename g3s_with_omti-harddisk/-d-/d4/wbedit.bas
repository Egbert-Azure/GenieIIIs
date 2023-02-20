1 'Wissenserwerbskomponente
2 'Dr. H.-J. Soll        1987
3 'modifiziert fur Tandy TRS80 Mod.1
4 'von Egbert Schroer    1989
5 CLEAR 7000
6 'Diese Version arbeitet mit externen Masken
7 'nur unter NewDos/Gdos
10 MO%=200
20 MF%=50
30 MR%=100
40 MC%=50
50 MQ%=20
60 DIM OA$(MO%),OSTATUS%(MO%),ODIAG%(MO%)
70 DIM FTEXT$(MF%),FA$(2,MF%),FSTATUS%(MF%),FOBJEKT$(MF%)
80 DIM RSTATUS%(MR%),WENN$(5,MR%),WF%(5,MR%),DANN$(MR%),TYP%(MR%)
90 DIM CN$(2,MC%),CS%(MC%)
100 DIM QU$(MQ%)
110 DIM W$(5)
120 CLS
130 PRINT @5,"Was m|chten Sie ....."
140 PRINT @133,"1. Bestehende Wissensbasis bearbeiten"
150 PRINT @197,"2. Neue Wissensbasis erzeugen"
160 PRINT @329,"Bitte w{hlen Sie : ";
170 I$=INKEY$:IF I$="" GOTO 170
180 PRINT I$:I%=VAL(I$)
190 PRINT @581,"Name der Wissensbasis : ",:LINEINPUT WN$
200 ON I% GOTO 230,510
220 PRINT @348," ":PRINT @606,"              ":GOTO 160
230 OPEN "I",1,WN$
240 INPUT #1,OBJEKTE%:INPUT #1,FRAGEN%:INPUT #1,REGELN%
250 INPUT #1,CO%:INPUT #1,NQ%
260 IF OBJEKTE%=0 GOTO 300
270 FOR N%=1 TO OBJEKTE%
280 INPUT #1,OA$(N%):INPUT #1,OSTATUS%(N%):INPUT #1,ODIAG%(N%)
290 NEXT N%
300 IF FRAGEN%=0 GOTO 340
310 FOR N%=1 TO FRAGEN%
320 INPUT #1,FTEXT$(N%):INPUT #1,FA$(1,N%):INPUT #1,FA$(2,N%):INPUT #1,FSTATUS%(N%):INPUT #1,FOBJEKT$(N%)
330 NEXT N%
340 IF REGELN%=0 GOTO 420
350 FOR N%=1 TO REGELN%
360 INPUT #1,RSTATUS%(N%)
370 FOR M%=1 TO 5
380 INPUT #1,WENN$(M%,N%):INPUT #1,WF%(M%,N%)
390 NEXT M%
400 INPUT #1,DANN$(N%):INPUT #1,TYP%(N%)
410 NEXT N%
420 IF CO%=0 GOTO 460
430 FOR N%=1 TO CO%
440 INPUT #1,CN$(1,N%):INPUT #1,CN$(2,N%):INPUT #1,CS%(N%)
450 NEXT N%
460 IF NQ%=0 GOTO 480
470 FOR N%=1 TO NQ%:INPUT #1,QU$(N%):NEXT N%
480 CLOSE #1
490 GOTO 1010
500 'Neue Wissensbasis erzeugen
510 OBJEKTE%=0:FRAGEN%=0:REGELN%=0:CO%=0:QU%=0
520 GOTO 1010
1000 'Wissensbasis bearbeiten
1010 CLS
1020 PRINT @5,"Was m|chten Sie ....."
1030 PRINT @133,"1. Fragen bearbeiten"
1040 PRINT @197,"2. (Diagnose-)Regeln bearbeiten"
1050 PRINT @261,"3. Objekte bearbeiten"
1060 PRINT @325,"4. Sichern und Beenden"
1070 PRINT @584,"Bitte w{hlen Sie : ";
1080 I$=INKEY$: IF I$="" GOTO 1080
1090 PRINT I$:I%=VAL(I$)
1100 ON I% GOTO 2010,5010,8010,10010
1140 PRINT @603," ":GOTO 1070
2000 'Fragen bearbeiten
2010 CLS
2020 PRINT @5,"Was m|chten Sie ....."
2030 PRINT @133,"1. Frage hinzuf}gen"
2040 PRINT @197,"2. Fragen auflisten"
2050 PRINT @261,"3. Fragen l|schen"
2060 PRINT @325,"4. Eingangsfragen festlegen"
2070 PRINT @584,"Bitte w{hlen Sie : ";
2080 I$=INKEY$:IF I$="" GOTO 2080
2090 PRINT I$:I%=VAL(I$)
2100 ON I% GOTO 2210,2610,2810,12010
2140 PRINT @603," ":GOTO 2070
2200 'Frage hinzuf}gen
2210 GOSUB 4010
2220 IF FRAGEN%=MFRAGEN% GOTO 11010
2230 FRAGEN%=FRAGEN%+1
2240 PRINT @83,"";:PRINT CHR$(15);:PRINT FRAGEN%;
2250 ZEILE%=2:K%=20
2255 ZEILE%=ZEILE%+1:IF ZEILE%=9 THEN ZEILE%=10:K%=22
2257 IF ZEILE%>10 THEN 2290
2260 PRINT @ZEILE%*64+K%,"";:PRINT CHR$(14);
2265 A$=INKEY$:IF A$="" THEN 2265
2270 IF A$=CHR$(13) THEN PRINT CHR$(15);:GOTO 2255
2275 IF POS(0)=61 THEN PRINT A$;:PRINT CHR$(15);:GOTO 2255:ELSE PRINT A$;:GOTO 2265
2290 PRINT @910,"Eintrag in Ordnung ? (j/n)";
2300 I$=INKEY$:IF I$="" GOTO 2300
2310 IF (I$<>"j" AND I$<>"J") THEN FRAGEN%=FRAGEN%-1:GOTO 2210
2320 GOSUB 4110:GOTO 1010
2600 'Fragen auflisten
2610 CLS
2620 IF FRAGEN%<>0 GOTO 2670
2630 PRINT @584,"*** keine Fragen in der Wissensbasis ***"
2640 PRINT @652,"*** Bitte eine Taste dr}cken ***"
2650 I$=INKEY$:IF I$="" GOTO 2650
2660 GOTO 1010
2670 PRINT @83,"":GOSUB 4010
2680 FOR N%=1 TO FRAGEN%
2690 PRINT @83,N%;
2700 PRINT @212,LEFT$(FTEXT$(N%),42);:PRINT @212+64,RIGHT$(FTEXT$(N%),42);
2710 PRINT @340,LEFT$(FA$(1,N%),42);:PRINT @340+64,RIGHT$(FA$(1,N%),42);
2720 PRINT @468,LEFT$(FA$(2,N%),42);:PRINT @468+64,RIGHT$(FA$(2,N%),42);
2730 PRINT @662,FOBJEKT$(N%);:PRINT CHR$(15);
2740 PRINT @913,"Zum Abbruch <E> dr}cken"
2750 PRINT @968,"Zur Fortsetzung eine andere Taste dr}cken";
2760 I$=INKEY$:IF I$="" GOTO 2760
2770 IF (I$="E" OR I$="e") GOTO 1010
2780 NEXT N%
2790 GOTO 1010
2800 'Frage l|schen
2810 CLS
2820 PRINT @261,"Geben Sie bitte die Nr. der zu"
2830 PRINT @325,"l|schenden Frage ein  :  ";:INPUT N%
2840 IF N%<=FRAGEN% GOTO 2890
2850 PRINT @584,"***** Fragennummer zu gro~ *****"
2860 PRINT @651,"Bitte eine Taste dr}cken"
2870 I$=INKEY$:IF I$="" GOTO 2870
2880 GOTO 1010
2890 IF NQ%=0 GOTO 2930
2900 FOR M%=1 TO NQ%
2910 IF FOBJEKT$(N%)=QU$(M%) THEN CLS:PRINT @133,"*** Frage ist Eingangsfrage ***":GOTO 2860
2920 NEXT M%
2930 PRINT @83,"":GOSUB 4010
2940 PRINT @83,N%;
2950 PRINT @212,LEFT$(FTEXT$(N%),42);:PRINT @212+64,RIGHT$(FTEXT$(N%),42);
2960 PRINT @340,LEFT$(FA$(1,N%),42);:PRINT @340+64,RIGHT$(FA$(1,N%),42);
2970 PRINT @468,LEFT$(FA$(2,N%),42);:PRINT @468+64,RIGHT$(FA$(2,N%),42);
2980 PRINT @662,FOBJEKT$(N%);:PRINT CHR$(15);
2990 PRINT @910,"Soll diese Frage gel|scht werden (j/n) ";
3000 I$=INKEY$:IF I$="" GOTO 3000
3010 IF (I$<>"j" AND I$<>"J") THEN FRAGEN%=FRAGEN%-1:GOTO 1010
3020 IF N%=FRAGEN% GOTO 3090
3030 FOR M%=N%+1 TO FRAGEN%
3040 FTEXT$(M%-1)=FTEXT$(M%)
3050 FA$(1,M%-1)=FA$(1,M%)
3060 FA$(2,M%-1)=FA$(2,M%)
3070 FOBJEKT$(M%-1)=FOBJEKT$(M%)
3080 NEXT M%
3090 FRAGEN%=FRAGEN%-1
3100 GOTO 1010
4000 'Fragenmaske
4010 CMD"load maske1/dum"
4070 RETURN
4100 'Einlesen aus Fragemaske
4110 PRINT CHR$(15);:I2%=1:I3%=2
4120 FOR I1%=0 TO 2
4130 FOR I%=I2% TO I3%:FOR K%=0 TO 41
4140 A$(I1%)=A$(I1%)+CHR$(PEEK((2+I%)*64+15380+K%)):POKE(2+I%)*64+15380+K%,46:NEXTK%,I%:I2%=I2%+2:I3%=I3%+2:NEXT I1%
4145 FOR K%=0 TO 39:A$(3)=A$(3)+CHR$(PEEK(10*64+15382+K%)):POKE(10*64)+15382+K%,46:NEXTK%
4150 FTEXT$(FRAGEN%)=A$(0)
4160 FA$(1,FRAGEN%)=A$(1)
4170 FA$(2,FRAGEN%)=A$(2)
4180 FOBJEKT$(FRAGEN%)=A$(3)
4185 FOR I1%=0 TO 3:A$(I1%)="":NEXT I1%
4190 RETURN
5000 'Regeln bearbeiten
5010 CLS
5020 PRINT @5,"Was m|chten Sie ....."
5030 PRINT @133,"1. (Diagnose-)Regeln hinzuf}gen"
5040 PRINT @197,"2. (Diagnose-)Regeln auflisten"
5050 PRINT @261,"3. (Diagnose-)Regeln l|schen"
5060 PRINT @584,"Bitte w{hlen Sie : ";
5070 I$=INKEY$:IF I$="" GOTO 5070
5080 PRINT I$:I%=VAL(I$)
5090 ON I% GOTO 5210,5810,6110
5120 PRINT @603," ":GOTO 5060
5200 'Regeln hinzuf}gen
5210 GOSUB 7010
5220 IF REGELN%=MR% GOTO 11010
5230 REGELN%=REGELN%+1
5240 PRINT @18,REGELN%;
5250 PRINT @147,"";:INPUT DIAGNOSE$
5260 DIAGNOSE%=0:IF (LEFT$(DIAGNOSE$,1)="d" OR LEFT$(DIAGNOSE$,1)="D") THEN DIAGNOSE%=1
5270 TYP%(REGELN%)=DIAGNOSE%
5280 PRINT @275,"";:INPUT W$(1):W$=W$(1):GOSUB 5690
5290 PRINT @339,"";:INPUT W$(2)
5300 PRINT @403,"";:INPUT W$(3)
5310 PRINT @467,"";:INPUT W$(4)
5320 PRINT @531,"";:INPUT W$(5)
5330 PRINT @659,"";:INPUT D$
5340 PRINT @910,"Eintrag in Ordnung ? (j/n)";
5350 I$=INKEY$:IF I$="" GOTO 5350
5360 IF (I$<>"j" AND I$<>"J") THEN REGELN%=REGELN%-1:GOTO 5210
5370 FOR N%=1 TO 5
5380 OBJEKT$=W$(N%)
5390 IF W$(N%)<>"- oder -" GOSUB 5610:OTYP%=0:GOSUB 5510
5400 WENN$(N%,REGELN%)=OBJEKT$:WF%(N%,REGELN%)=VO%
5410 NEXT N%
5420 OBJEKT$=D$
5430 OTYP%=DIAGNOSE%:GOSUB 5510
5440 DANN$(REGELN%)=OBJEKT$
5450 GOTO 1010
5500 'Objekte f}r Regeln erzeugen
5510 IF OBJEKT$="" THEN RETURN
5520 IF OBJEKTE%=0 GOTO 5570
5530 FOR NUMMER%=1 TO OBJEKTE%
5540 IF OBJEKT$=OA$(NUMMER%) THEN RETURN
5550 NEXT NUMMER%
5560 IF OBJEKTE%=MOBJEKTE% GOTO 11010
5570 OBJEKTE%=OBJEKTE%+1
5580 OA$(OBJEKTE%)=OBJEKT$:ODIAG%(OBJEKTE%)=OTYP%
5590 RETURN
5600 '"Nicht" bei Regel abtrennen
5610 VO%=1
5620 IF LEN(OBJEKT$)<7 THEN RETURN
5630 U$=LEFT$(OBJEKT$,6):GOSUB 20020:IF U$<>"NICHT" THEN RETURN
5640 OBJEKT$=RIGHT$(OBJEKT$,LEN(OBJEKT$)-6)
5650 IF LEFT$(OBJEKT$,1)=" " THEN OBJEKT$=RIGHT$(OBJEKT$,LEN(OBJEKT$)-1):GOTO 5650
5660 VO%=-1
5670 RETURN
5680 'oder-Regel
5690 IF (LEFT$(W$,1)=" " OR LEFT$(W$,1)="-") THEN W$=RIGHT$(W$,LEN(W$)-1):GOTO 5690
5700 IF (RIGHT$(W$,1)=" " OR RIGHT$(W$,1)="-") THEN W$=LEFT$(W$,LEN(W$)-1):GOTO 5700
5710 U$=W$:GOSUB 20020:IF U$<>"ODER" THEN RETURN
5720 W$(1)="- oder -"
5730 RETURN
5800 'Regeln auflisten
5810 CLS
5820 IF REGELN%<>0 GOTO 5870
5830 PRINT @584,"*** keine Regeln in der Wissensbasis ***"
5840 PRINT @657,"Bitte eine Taste dr}cken"
5850 I$=INKEY$:IF I$="" GOTO 5850
5860 GOTO 1010
5870 FOR N%=1 TO REGELN%
5880 GOSUB 7010
5890 PRINT @18,N%;
5900 DIAGNOSE$="Regel                ":IF TYP%(N%)=1 THEN DIAGNOSE$="Diagnose             "
5910 PRINT @147,DIAGNOSE$;
5920 FOR M%=1 TO 5
5930 W$=WENN$(M%,N%)
5940 IF WF%(M%,N%)=(-1) THEN W$="nicht "+W$
5950 IF W$="" THEN W$=" -"
5960 PRINT @64*M%+211,W$;
5970 NEXT M%
5980 PRINT @659,DANN$(N%);
5990 PRINT @848,"Zum Abbruch <E> dr}cken"
6000 PRINT @904,"Zur Fortsetzung eine andere Taste dr}cken"
6010 I$=INKEY$:IF I$="" GOTO 6010
6020 IF (I$="E" OR I$="e") GOTO 1010
6030 NEXT N%
6040 GOTO 1010
6100 'Regeln l|schen
6110 CLS
6120 PRINT @197,"Geben Sie bitte die Nr. der zu"
6130 PRINT @261,"l|schenden Regel ein : ";:INPUT N%
6140 IF N%<=REGELN% GOTO 6190
6150 PRINT @584,"***** Regelnummer zu gro~ *****"
6160 PRINT @651,"Bitte eine Taste dr}cken"
6170 I$=INKEY$:IF I$="" THEN 6170
6180 GOTO 6110
6190 GOSUB 7010
6200 PRINT @18,N%;
6210 DIAGNOSE$="Regel                ":IF TYP%(N%)=1 THEN DIAGNOSE$="Diagnose             "
6220 PRINT @147,DIAGNOSE$;
6230 FOR M%=1 TO 5
6240 W$=WENN$(M%,N%)
6250 IF WF%(M%,N%)=(-1) THEN W$="nicht "+W$
6260 IF W$="" THEN W$=" -"
6270 PRINT @64*M%+211,W$;
6280 NEXT M%
6290 PRINT @659,DANN$(N%);
6300 PRINT @904,"Soll diese (Diagnose-)Regel gel|scht werden ? (j/n)";
6310 I$=INKEY$:IF I$="" GOTO 6310
6320 IF (I$<>"j" AND I$<>"J") GOTO 1010
6330 PRINT @787,"--- Bitte warten ---"
6340 FOR K%=1 TO 5
6350 OBJEKT$=WENN$(K%,N%):GOSUB 7110
6360 NEXT K%
6370 OBJEKT$=DANN$(N%):GOSUB 7110
6380 IF N%=REGELN% GOTO 6460
6390 FOR M%=N%+1 TO REGELN%
6400 FOR K%=1 TO 5
6410 WENN$(K%,M%-1)=WENN$(K%,M%)
6420 WF%(K%,M%-1)=WF%(K%,M%)
6430 NEXT K%
6440 DANN$(M%-1)=DANN$(M%):TYP%(M%-1)=TYP%(M%)
6450 NEXT M%
6460 REGELN%=REGELN%-1
6470 PRINT @777,"                    "
6480 GOTO 1010
7000 'Regelmaske
7010 CMD"load maske2/dum"
7100 RETURN
7110 'ggf. Objekt l|schen
7120 ANZAHL%=0
7130 FOR M%=1 TO OBJEKTE%
7140 IF OBJEKT$=WENN$(1,M%) THEN ANZAHL%=ANZAHL%+1
7150 IF OBJEKT$=WENN$(2,M%) THEN ANZAHL%=ANZAHL%+1
7160 IF OBJEKT$=WENN$(3,M%) THEN ANZAHL%=ANZAHL%+1
7170 IF OBJEKT$=WENN$(4,M%) THEN ANZAHL%=ANZAHL%+1
7180 IF OBJEKT$=WENN$(5,M%) THEN ANZAHL%=ANZAHL%+1
7190 IF OBJEKT$=DANN$(M%) THEN ANZAHL%=ANZAHL%+1
7200 NEXT M%
7210 IF ANZAHL%>1 THEN RETURN
7220 FOR M%=1 TO OBJEKTE%
7230 IF OA$(M%)=OBJEKT$ GOTO 7260
7240 NEXT M%
7250 RETURN
7260 IF N%=OBJEKTE% GOTO 7320
7270 FOR L%=M%+1 TO OBJEKTE%
7280 OA$(L%-1)=OA$(L%)
7290 OSTATUS%(L%-1)=OSTATUS%(L%)
7300 ODIAG%(L%-1)=ODIAG%(L%)
7310 NEXT L%
7320 OBJEKTE%=OBJEKTE%-1
7330 RETURN
8000 'Objekte anzeigen
8010 CLS
8020 PRINT @5,"Was m|chten Sie ....."
8030 PRINT @133,"1. Objekte auflisten"
8040 PRINT @197,"2. Constraints auflisten"
8050 PRINT @261,"3. Constraints eintragen"
8060 PRINT @325,"4. Constraints l|schen"
8070 PRINT @584,"Bitte w{hlen Sie : ";
8080 I$=INKEY$:IF I$="" GOTO 8080
8090 PRINT I$:I%=VAL(I$)
8100 ON I% GOTO 8210,8610,9010,9410
8140 PRINT @603," ":GOTO 8070
8200 'Objekte auflisten
8210 CLS
8220 IF OBJEKTE%<>0 GOTO 8270
8230 PRINT @584,"*** keine Objekte in der Wissensbasis ***"
8240 PRINT @657,"Bitte eine Taste dr}cken"
8250 I$=INKEY$:IF I$="" GOTO 8250
8260 GOTO 1010
8270 ZEILE%=2
8280 FOR N%=1 TO OBJEKTE%
8290 IF ZEILE%=2 THEN PRINT @0,"Objektnr.  Typ  Objektname"
8300 PRINT @64*ZEILE%+4,N%
8310 I$="R":IF ODIAG%(N%)=1 THEN I$="D"
8320 PRINT @64*ZEILE%+12,I$
8330 PRINT @64*ZEILE%+15,OA$(N%)
8340 ZEILE%=ZEILE%+2
8350 IF ZEILE%<14 GOTO 8400
8360 PRINT @904,"Bitte eine Taste dr}cken     <E>=Ende"
8370 I$=INKEY$:IF I$="" GOTO 8370
8380 IF (I$="E" OR I$="e") GOTO 1010
8390 CLS:ZEILE%=2
8400 NEXT N%
8410 PRINT @840,"******** Ende der Liste ********"
8420 PRINT @908,"Bitte eine Taste dr}cken"
8430 I$=INKEY$:IF I$="" GOTO 8430
8440 GOTO 1010
8600 'Constraints auflisten
8610 CLS
8620 IF CO%<>0 GOTO 8670
8630 PRINT @584,"*** keine Constraints in der Wissensbasis ***"
8640 PRINT @657,"Bitte eine Taste dr}cken"
8650 I$=INKEY$:IF I$="" GOTO 8650
8660 GOTO 1010
8670 ZEILE%=2
8680 FOR N%=1 TO CO%
8690 IF ZEILE%=2 THEN PRINT @0,"Nr.   Faktum 1  bedingt  Faktum 2"
8700 PRINT @64*ZEILE%,N%
8710 I$=CN$(1,N%)+"  ===>  "
8720 IF CS%(N%)=(-1) THEN I$=I$+"nicht  "
8730 I$=I$+CN$(2,N%)
8740 PRINT @64*ZEILE%+6,I$
8750 ZEILE%=ZEILE%+2
8760 IF ZEILE%<14 GOTO 8810
8770 PRINT @904,"Bitte eine Taste dr}cken     <E>=Ende"
8780 I$=INKEY$:IF I$="" GOTO 8780
8790 IF (I$="E" OR I$="e") GOTO 1010
8800 CLS:ZEILE%=2
8810 NEXT N%
8820 PRINT @840,"********* Ende der Liste ********"
8830 PRINT @908,"Bitte eine Taste dr}cken"
8840 I$=INKEY$:IF I$="" GOTO 8840
8850 GOTO 1010
9000 'Constraints eintragen
9010 CLS
9020 PRINT @133,"Faktum 1 :"
9030 PRINT @261,"bewirkt ==>"
9040 PRINT @389,"Faktum 2 :"
9050 CO%=CO%+1:IF CO%>MC% GOTO 11000
9060 PRINT @133+12,"";:INPUT CN$(1,CO%)
9070 PRINT @389+12,"";:INPUT I$
9080 CS%(CO%)=1
9090 IF LEN(I$)<7 GOTO 9140
9100 U$=LEFT$(I$,6):GOSUB 20020:IF U$<>"NICHT " GOTO 9140
9110 I$=RIGHT$(I$,LEN(I$)-6)
9120 IF LEFT$(I$,1)=" " THEN I$=RIGHT$(I$,LEN(I$)-1):GOTO 9120
9130 CS%(CO%)=-1
9140 CN$(2,CO%)=I$
9150 PRINT @648,"Eintrag in Ordnung ?  (j/n)";
9160 I$=INKEY$:IF I$="" GOTO 9160
9170 IF (I$<>"j" AND I$<>"J") THEN CO%=CO%-1:GOTO 9010
9180 GOTO 1010
9400 'Constraints l|schen
9410 CLS
9420 PRINT @261,"";:INPUT"Nr. des zu l|schenden Constraints : ";N%
9430 IF N%<=MC% GOTO 9480
9440 PRINT @584,"***** Constraint-Nr. zu hoch *****"
9450 PRINT @643,"Bitte eine Taste dr}cken"
9460 I$=INKEY$:IF I$="" GOTO 9460
9470 GOTO 9410
9480 I$=CN$(1,N%)+"  ===>   "
9490 IF CS%(N%)=(-1) THEN I$=I$+"nicht  "
9500 I$=I$+CN$(2,N%)
9510 PRINT @325,I$
9520 PRINT @584,"Soll dies gel|scht werden ? (j/n)"
9530 I$=INKEY$:IF I$="" GOTO 9530
9540 PRINT @654,"--- Bitte warten ---"
9550 IF (I$<>"j" AND I$<>"J") GOTO 1010
9560 FOR M%=N%+1 TO CO%
9570 CN$(1,M%-1)=CN$(1,M%)
9580 CN$(1,M%-1)=CN$(2,M%)
9590 CS%(M%-1)=CS%(M%)
9600 NEXT M%
9610 CO%=CO%-1
9620 PRINT @654,"                    "
9630 GOTO 1010
10000 'Sichern/Beenden
10010 OPEN "O",1,WN$
10020 PRINT #1,OBJEKTE%:PRINT #1,FRAGEN%:PRINT #1,REGELN%
10030 PRINT #1,CO%:PRINT #1,NQ%
10040 IF OBJEKTE%=0 GOTO 10080
10050 FOR N%=1 TO OBJEKTE%
10060 PRINT #1,OA$(N%):PRINT #1,OSTATUS%(N%):PRINT #1,ODIAG%(N%)
10070 NEXT N%
10080 IF FRAGEN%=0 GOTO 10120
10090 FOR N%=1 TO FRAGEN%
10100 PRINT #1,FTEXT$(N%):PRINT #1,FA$(1,N%):PRINT #1,FA$(2,N%):PRINT #1,FSTATUS%(N%):PRINT #1,FOBJEKT$(N%)
10110 NEXT N%
10120 IF REGELN%=0 GOTO 10200
10130 FOR N%=1 TO REGELN%
10140 PRINT #1,RSTATUS%(N%)
10150 FOR M%=1 TO 5
10160 PRINT #1,WENN$(M%,N%):PRINT #1,WF%(M%,N%)
10170 NEXT M%
10180 PRINT #1,DANN$(N%):PRINT #1,TYP%(N%)
10190 NEXT N%
10200 IF CO%=0 GOTO 10260
10210 FOR N%=1 TO CO%
10220 PRINT #1,CN$(1,N%)
10230 PRINT #1,CN$(2,N%)
10240 PRINT #1,CS%(N%)
10250 NEXT N%
10260 IF NQ%=0 GOTO 10280
10270 FOR N%=1 TO NQ%:PRINT #1,QU$(N%):NEXT N%
10280 CLOSE #1
10290 CLS:END
11000 'Wissensbasis zu klein
11010 CLS
11020 PRINT @10,"*****  Wissensbasis zu klein  *****"
11030 PRINT @140,"P r o g r a m m a b r u c h  !":PRINT @584,"";
11040 STOP
12000 'Eingangsfragen
12010 CLS
12020 PRINT @5,"Was m|chten Sie ....."
12030 PRINT @133,"1. Eingangsfragen hinzuf}gen"
12040 PRINT @197,"2. Eingangsfragen auflisten"
12050 PRINT @261,"3. Eingangsfragen l|schen"
12060 PRINT @584,"Bitte w{hlen Sie : ";
12070 I$=INKEY$:IF I$="" GOTO 12070
12080 PRINT I$:I%=VAL(I$)
12090 ON I% GOTO 12210,12610,13010
12120 PRINT @603," ":GOTO 12070
12200 'Eingangsfrage hinzuf}gen
12210 CLS
12220 NQ%=NQ%+1:IF NQ%>MQ% GOTO 11010
12230 PRINT @133,"Fragenummer oder Bezugsobjekt-Name :"
12240 PRINT @261,"";:INPUT" ===> ";I$
12250 M%=VAL(I$)
12260 IF M%>FRAGEN% THEN PRINT @325,"*** Fragenumer zu gro~ ***":GOTO 12320
12270 IF M%<>0 THEN QU$(NQ%)=FOBJEKT$(M%):GOTO 12350
12280 FOR M%=1 TO FRAGEN%
12290 IF I$=FOBJEKT$(M%) THEN QU$(NQ%)=I$:GOTO 12350
12300 NEXT M%
12310 PRINT @453,"*** ";I$;" nicht vorhanden ***"
12320 PRINT @653,"Bitte eine Taste dr}cken"
12330 I$=INKEY$:IF I$="" GOTO 12330
12340 NQ%=NQ%-1:GOTO 12210
12350 PRINT @261,FTEXT$(M%)
12360 PRINT @325,FOBJEKT$(M%)
12370 PRINT @584,"Eintrag in Ordnung ? (j/n)";
12380 I$=INKEY$:IF I$="" GOTO 12380
12390 IF (I$<>"j" AND I$<>"J") THEN NQ%=NQ%-1:GOTO 12210
12400 GOTO 1010
12600 'Eingangsfragen auflisten
12610 CLS
12620 IF NQ%<>0 GOTO 12670
12630 PRINT @584,"*** keine Eingangsfragen vorhanden ***"
12640 PRINT @655,"Bitte eine Taste dr}cken"
12650 I$=INKEY$:IF I$="" GOTO 12650
12660 GOTO 1010
12670 ZEILE%=2
12680 FOR N%=1 TO NQ%
12690 IF ZEILE%=2 THEN PRINT @0,"Eingangsfragen :"
12700 PRINT @64*ZEILE%,N%
12710 FOR M%=1 TO FRAGEN%
12720 IF QU$(N%)=FOBJEKT$(M%) THEN I$=FTEXT$(M%):GOTO 12750
12730 NEXT M%
12740 I$="*** Frage nach "+QU$(N%)+" nicht vorhanden ***"
12750 PRINT @64*ZEILE%+2,I$
12760 ZEILE%=ZEILE%+2
12770 IF ZEILE%<14 GOTO 12820
12780 PRINT @908,"Bitte eine Taste dr}cken      <E>=Ende"
12790 I$=INKEY$:IF I$="" GOTO 12790
12800 IF (I$="E" OR I$="e") GOTO 1010
12810 CLS:ZEILE%=2
12820 NEXT N%
12830 PRINT @840,"******** Ende der Liste ********"
12840 PRINT @908,"Bitte eine Taste dr}cken"
12850 I$=INKEY$:IF I$="" GOTO 12850
12860 GOTO 1010
13000 'Eingangsfragen l|schen
13010 CLS
13020 PRINT @261,"Geben Sie bitte die Nr. der zu"
13030 PRINT @325,"";:INPUT"l|schenden Eingangsfrage ein : ";N%
13040 IF N%<=NQ% GOTO 13090
13050 PRINT @584,"***** Fragennummer zu gro~ *****"
13060 PRINT @652,"Bitte eine Taste dr}cken"
13070 I$=INKEY$:IF I$="" GOTO 13070
13080 GOTO 13010
13090 CLS
13100 FOR M%=1 TO FRAGEN%
13110 IF QU$(N%)=FOBJEKT$(M%) GOTO 13170
13120 NEXT M%
13130 PRINT @261,"*** ";QUESTION$(N%);" nicht vorhanden ***"
13140 PRINT @652,"Bitte eine Taste dr}cken"
13150 I$=INKEY$:IF I$="" GOTO 13150
13160 GOTO 1010
13170 PRINT @133,"Frage   : ";FTEXT$(M%)
13180 PRINT @261,"Objekt  : ";FOBJEKT$(M%)
13190 PRINT @584,"Soll diese Frage gel|scht werden ? (j/n)"
13200 I$=INKEY$:IF I$="" GOTO 13200
13210 IF (I$<>"j" AND I$<>"J") THEN NQ%=NQ-1:GOTO 13010
13220 N%=M%
13230 IF N%=NQ% GOTO 13270
13240 FOR M%=N%+1 TO NQ%
13250 QU$(M%-1)=QU$(M%)
13260 NEXT M%
13270 NQ%=NQ%-1
13280 GOTO 1010
20000 'Upper $
20010 '-> U$ ->
20020 UU$=""
20030 IF U$="" THEN RETURN
20040 FOR U%=1 TO LEN(U$)
20050 UT$=MID$(U$,U%,1)
20060 IF (ASC(UT$)>96 AND ASC(UT$)<123) THEN UT$=CHR$(ASC(UT$)-32)
20070 UU$=UU$+UT$
20080 NEXT U%
20090 U$=UU$
20100 RETURN
