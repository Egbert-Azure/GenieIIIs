1 'Expertensystem / Inferenzkomponente
2 'Dr. H.-J. Soll      1987
3 'modifiziert f}r Tandy TRS80 Mod.1
4 'von Egbert Schr|er  1990
10 CLS
15 CLEAR 9000
20 MSTACK%=100
25 MTRACER%=200
30 DIM SRNR%(MSTACK%),PZAEHLER%(MSTACK%)
40 PRINT @581,"Name der Wissensbasis : ";:LINEINPUT WN$
50 OPEN "I",1,WN$
60 INPUT #1,OBJEKTE%:INPUT #1,FRAGEN%:INPUT #1,REGELN%
70 INPUT #1,CO%:INPUT #1,Z2%
80 DIM OA$(OBJEKTE%),OSTATUS%(OBJEKTE%),ODIAG%(OBJEKTE%)
90 DIM FTEXT$(FRAGEN%),FA$(2,FRAGEN%),FSTATUS%(FRAGEN%),FOBJEKT$(FRAGEN%)
100 DIM RSTATUS%(REGELN%),WENN$(5,REGELN%),WF%(5,REGELN%),DANN$(REGELN%),TYP%(REGELN%)
105 DIM CN$(2,CO%),CS%(CO%)
107 DIM TRACER$(MTRACER%)
110 IF OBJEKTE%=0 GOTO 150
120 FOR N%=1 TO OBJEKTE%
130 INPUT#1,OA$(N%):INPUT#1,OSTATUS%(N%):INPUT#1,ODIAG%(N%)
140 NEXT N%
150 IF FRAGEN%=0 GOTO 190
160 FOR N%=1 TO FRAGEN%
170 INPUT #1,FTEXT$(N%):INPUT #1,FA$(1,N%):INPUT #1,FA$(2,N%):INPUT #1,FSTATUS%(N%):INPUT #1,FOBJEKT$(N%)
180 NEXT N%
190 IF REGELN%=0 GOTO 270
200 FOR N%=1 TO REGELN%
210 INPUT #1,RSTATUS%(N%)
220 FOR M%=1 TO 5
230 INPUT #1,WENN$(M%,N%):INPUT #1,WF%(M%,N%)
240 NEXT M%
250 INPUT #1,DANN$(N%):INPUT #1,TYP%(N%)
260 NEXT N%
270 IF CO%=0 GOTO 279
271 FOR N%=1 TO CO%
272 INPUT#1,CN$(1,N%)
273 INPUT#1,CN$(2,N%)
274 INPUT#1,CS%(N%)
275 NEXT N%
279 CLOSE#1
280 N%=0
290 FOR M%=1 TO REGELN%
300 IF TYP%(M%)=1 THEN N%=N%+1
310 NEXT M%
320 CLS
330 PRINT @133,FRAGEN%;" Fragen"
340 PRINT @197,REGELN%;" Regeln"
350 PRINT @261,N%;" Diagnosen"
355 PRINT @325,CO%;" Abh{ngigkeiten"
360 PRINT @657,"Bitte dr}cken Sie eine Taste"
370 I$=INKEY$:IF I$="" GOTO 370
380 PRINT @657,"Bitte etwas Geduld          "
390 PRINT @5,"Die Wissensbasis besteht aus :"
1000 'Status zur}cksetzen
1010 FOR N%=1 TO OBJEKTE%
1020 OSTATUS%(N%)=0
1030 NEXT N%
1040 FOR N%=1 TO FRAGEN%
1050 FSTATUS%(N%)=0
1060 NEXT N%
1070 FOR N%=1 TO REGELN%
1080 RSTATUS%(N%)=0
1090 NEXT N%
1092 FOR N%=1 TO MTRACER%
1093 TRACER$(N%)=""
1094 NEXT N%
1095 TRACER%=1
1100 'alle m|glichen Diagnosen auf Stack ablegen
1110 STACK%=0
1120 FOR N%=1 TO REGELN%
1130 IF TYP%(N%)=1 GOSUB 3220:TRACER$="1="+DANN$(N%):GOSUB 2810:'Stack ablegen
1140 NEXT N%
2000 'eigentliches Infernzprogramm
2010 IF STACK%=0 GOTO 4010:'Diagnosen ausgeben
2020 IF RSTATUS%(SRNR%(STACK%))=0 GOTO 2505
2030 STACK%=STACK%-1
2040 GOTO 2010
2500 'Regeln bearbeiten
2505 PRINT @517,"Regeln werden bearbeitet !"
2510 IF (PZAEHLER%(STACK%)>5 AND WENN$(1,SRNR%(STACK%))<>"- oder -") THEN RSTATUS%(SRNR%(STACK%))=1:NM$=DANN$(SRNR%(STACK%)):TRACER$="2="+NM$+"=1="+STR$(SRNR%(STACK%)):GOSUB 2810:GOSUB 3430:OSTATUS%(N%)=1:GOSUB 5010:GOTO 2030
2520 IF (PZAEHLER%(STACK%)>5 AND WENN$(1,SRNR%(STACK%))="- oder -") THEN RSTATUS%(SRNR%(STACK%))=-1:NM$=DANN$(SRNR%(STACK%)):TRACER$="5="+NM$+"=-1="+STR$(SRNR%(STACK%)):GOSUB 2810:GOSUB 3430:OSTATUS%(N%)=-1:GOTO 2030
2530 NM$=WENN$(PZAEHLER%(STACK%),SRNR%(STACK%))
2540 IF NM$="" THEN PZAEHLER%(STACK%)=6:GOTO 2510
2550 IF NM$="- oder -" THEN PZAEHLER%(STACK%)=PZAEHLER%(STACK%)+1:GOTO 2510
2560 GOSUB 3430
2570 IF OSTATUS%(N%)=0 THEN 3020
2580 IF WENN$(1,SRNR%(STACK%))="- oder -" GOTO 2645
2590 'und-Regel
2600 WF%=WF%(PZAEHLER%(STACK%),SRNR%(STACK%))
2605 IF OSTATUS%(N%)*WF%<>1 THEN RSTATUS%(SRNR%(STACK%))=(-1):NM$=DANN$(SRNR%(STACK%)):TRACER$="2="+NM$+"=-1="+WENN$(PZAEHLER%(STACK%),SRNR%(STACK%))+"="+STR$(WF%):GOSUB 2810:GOSUB 3430:OSTATUS%(N%)=(-1):GOTO 2030
2610 PZAEHLER%(STACK%)=PZAEHLER%(STACK%)+1
2620 GOTO 2510
2630 'oder-Regel
2640 WF%=WF%(PZAEHLER%(STACK%),SRNR%(STACK%))
2645 IF OSTATUS%(N%)*WF%=1THEN RSTATUS%(SRNR%(STACK%))=1:NM$=DANN$(SRNR%(STACK%)):TRACER$="5="+NM$+"=1="+WENN$(PZAEHLER%(STACK%),SRNR%(STACK%))+"="+STR$(WF%):GOSUB 2810:GOSUB 3430:OSTATUS%(N%)=1:GOSUB 5010:GOTO 2030
2650 PZAEHLER%(STACK%)=PZAHLER%(STACK%)+1
2660 GOTO 2510
2800 'Tracer eintragen
2810 IF TRACER%=MTRACER% THEN TRACER$="9=**** ]berlauf ****"
2820 IF TRACER%<=MTRACER% THEN TRACER$(TRACER%)=TRACER$
2830 TRACER%=TRACER%+1
2840 RETURN
3000 'Objektstatus unbekannt
3010 '-> n%  : Nr des unbestimmten Objektes
3020 FOR K%=1 TO REGELN%
3030 IF DANN$(K%)=OA$(N%) THEN M%=N%:N%=K%:GOSUB 3220:N%=M%:GOTO 2020
3040 NEXT K%
3050 GOSUB 4410:'Fragen stellen
3060 OSTATUS%(N%)=I%:IF OSTATUS%(N%)=1 THEN GOSUB 5010:'Constraints testen
3070 GOTO 2580
3200 'Regeln auf stack legen
3210 '-> n% : Nummer der (Diagnose-)Regel
3220 IF STACK%=MSTACK% THEN CLS:PRINT"**** Stack-]berlauf ****":END
3230 STACK%=STACK%+1
3240 SRNR%(STACK%)=N%:PZAEHLER%(STACK%)=1
3250 RETURN
3400 'suchen der Objektnummer
3410 '->NAMEN$ : Name des Objektes
3420 '<-N%     : Nummer des Objektes
3430 FOR N%=1 TO OBJEKTE%
3440 IF OA$(N%)=NM$ THEN RETURN
3450 NEXT N%
3460 CLS
3470 PRINT"**** Objekt ";NM$;" nicht vorhanden ****"
3480 END
4000 'Diagnosen ausgeben
4010 ANZAHL%=0
4020 FOR N%=1 TO REGELN%
4030 IF (TYP%(N%)=1 AND RSTATUS%(N%)=1) THEN ANZAHL%=ANZAHL%+1:DIAG%=N%
4040 NEXT N%
4050 CLS
4060 PRINT @5,"D I A G N O S E N"
4070 IF ANZAHL%=0 THEN PRINT @256,"**** Es konnte keine Diagnose best{tigt werden ****":GOTO 4240
4080 IF ANZAHL%=1 THEN PRINT @256,"Best{tigte Diagnose : ";DANN$(DIAG%): GOTO 4240
4090 PRINT @128,"Best{tigte Diagnosen : "
4100 ZEILE%=10
4110 FOR N%=1 TO REGELN%
4120 IF (TYP%(N%)<>1 OR RSTATUS%(N%)<>1) GOTO 4210
4130 IF ZEILE%<18 THEN PRINT @ZEILE%*64+5,DANN$(N%):ZEILE%=ZEILE%+2:GOTO 4210
4140 PRINT @968,"Zur Fortsetzung eine Taste dr}cken";
4150 I$=INKEY$:IF I$="" THEN 4150
4160 CLS
4170 PRINT @5,"D I A G N O S E N"
4180 PRINT @128,"Fortsetzung best{tigte Diagnosen :"
4190 ZEILE%=10
4200 GOTO 4130
4210 NEXT N%
4220 GOTO 4240
4230 'Programmende
4240 PRINT @968,"Bitte eine Taste dr}cken";
4250 I$=INKEY$:IF I$="" THEN 4250
4260 CLS
4270 PRINT @5,"Was m|chten Sie ....."
4280 PRINT @133,"1. Programm beenden"
4290 PRINT @197,"2. Begr}ndung ausgeben"
4300 PRINT @261,"3. Neuer Durchlauf"
4310 PRINT @584,"Bitte w{hlen Sie : ";
4320 I$=INKEY$:IF I$="" THEN 4320
4330 PRINT I$:I%=VAL(I$)
4340 ON I% GOTO 4350,6010,280
4350 CLS:PRINT @5,"Programm beendet":PRINT:END
4370 GOTO 4270
4380 'Dialog-Komponente: Frage stellen
4390 '-> n% : Nr des unbestimmten Objektes
4400 '<- i% : Antwortcode
4410 FOR L%=1 TO FRAGEN%
4420 IF OA$(N%)=FOBJEKT$(L%) THEN 4520
4430 NEXT L%
4440 'keine Frage vorhanden
4450 CLS
4460 PRINT @128,"*** Es fehlt die Frage f}r '";OA$(N%);"' ***"
4465 PRINT @128+64,"Bitte Antwort eingeben"
4470 PRINT @256+64,"Trifft diese Tatsache zu ?"
4480 PRINT @320+64,"Antwort  (Ja,Nein) : ";:INPUT I$
4490 IF (LEFT$(I$,1)="J" OR LEFT$(I$,1)="j") THEN I%=1:TRACER$="4="+OA$(N%)+"=1=*** Frage fehlt ***= Trifft nicht zu !":GOSUB 2810:CLS:RETURN
4500 I%=(-1):TRACER$="4="+OA$(N%)+"=-1=*** Frage fehlt ***= trifft nicht zu !":GOSUB 2810:CLS:RETURN
4510 'gefundene Frage nun stellen
4520 CLS
4530 PRINT @128,FTEXT$(L%)
4540 PRINT @256,"1 -  ";FA$(1,L%)
4550 PRINT @384,"2 -  ";FA$(2,L%)
4560 PRINT @968,"Welche Antwort ist korrekt ? (1,2) ";
4570 I$=INKEY$:IF I$="" THEN 4570
4580 PRINT I$
4590 IF I$="1" THEN I%=1:FSTATUS%(L%)=I%:TRACER$="4="+FOBJEKT$(L%)+"=1="+FTEXT$(L%)+"="+FA$(1,L%):GOSUB 2810:CLS: RETURN
4600 IF I$="2" THEN I%=-1:FSTATUS%(L%)=I%:TRACER$="4="+FOBJEKT$(L%)+"=-1"+FTEXT$(L%)+"="+FA$(2,L%):GOSUB 2810:CLS:RETURN
4620 FOR I%=1 TO 5:CLS:PRINT @968,"*** Antwort ist falsch ***";:NEXT
4630 GOTO 4520
5000 ' Constraints auswerten
5010 IF  OSTATUS%(N%)<>1 THEN RETURN:' Trifft normalerweise nicht zu
5020 IF CO%=0 THEN RETURN
5030 FOR Z%=1 TO CO%
5040 IF CN$(1,Z%)<>OA$(N%) THEN 5110
5050 FOR ZZ%=1 TO OBJEKTE%
5060 IF CN$(2,Z%)<>OA$(ZZ%) THEN 5100
5070 IF OSTATUS%(ZZ%)<>0 THEN 5110
5080 OSTATUS%(ZZ%)=CS%(Z%)
5090 TRACER$="3="+CN$(2,Z%)+"="+STR$(OSTATUS%(ZZ%))+"="+CN$(1,Z%):GOSUB 2810
5100 NEXT ZZ%
5110 NEXT Z%
5120 RETURN
6000 'Begr}ndungskomponente
6010 CLS
6020 PRINT @5,"B E G R ] N D U N G E N"
6030 PRINT @133,"Folgende Ausgaben sind m|glich:"
6040 PRINT @197+64,"1. Auflistung aller m|glichen Fakten"
6050 PRINT @261+64,"2. Auflistung bestimmter Fakten"
6060 PRINT @325+64,"3. Anzeige einer bestimmten Regel"
6070 PRINT @389+64,"4. Ausdruck aller bestimmter Regeln"
6080 PRINT @453+64,"5. Beenden der Begr}ndungskomponente"
6085 IF TRACER%=MTRACER% THEN PRINT @517+64,"***** Tracer Uberlauf *****"
6090 PRINT @848,"Bitte w{hlen Sie : ";
6100 I$=INKEY$:IF I$="" THEN 6100
6110 PRINT I$:I%=VAL(I$)
6120 ON I% GOTO 6210,6310,7010,10010,4260
6170 GOTO 6090
6200 'Auflistung aller moglichen Fakten
6210 WG%=0:'alle Fakten
6220 GOTO 6340:'Auflistung von Fakten
6300 'Auflistung der bestimmten Fakten
6310 WG%=1:'nur bestimmte Fakten
6320 GOTO 6340:'Auflistung von Fakten
6330 'Auflistung von Fakten
6340 CLS
6350 IF WG%=0 THEN PRINT"Auflistung aller m|glichen Fakten"
6360 IF WG%=1 THEN PRINT"Auflistung der bestimmten Fakten"
6370 ZEILE%=2
6380 FOR M%=1 TO OBJEKTE%
6381 FOR N%=1 TO TRACER%-1
6382 GOSUB 6730:IF (TYP%<>1 AND OA$(M%)=NM$) THEN 6400
6384 NEXT N%
6385 I$="?":GOTO 6420
6400 IF SU%=1 THEN I$="+"
6410 IF SU%=-1 THEN I$="-"
6420 IF WG%=0 THEN PRINT @ZEILE%*64,M%;".":PRINT @ZEILE%*64+6,I$;" ";OA$(M%):ZEILE%=ZEILE%+2
6430 IF WG%=1 AND I$<>"?" THEN PRINT @ZEILE%*64,M%;".":PRINT @ZEILE%*64+6,I$;" ";OA$(M%):ZEILE%=ZEILE%+2
6440 IF ZEILE%<15 THEN 6510
6450 PRINT @968,"Bitte eine Taste dr}cken ! ( E = Abbruch)";
6460 I$=INKEY$:IF I$="" GOTO 6460
6465 IF I$="e" OR I$="E" THEN GOTO 6000
6470 CLS
6480 IF WG%=0 THEN PRINT "Fortsetzung: Alle m|glichen Fakten"
6490 IF WG%=1 THEN PRINT "Fortsetzung: Nur bestimmte Fakten"
6500 ZEILE%=2
6510 NEXT M%
6520 PRINT"------------------------------------------"
6530 PRINT @968,"Ende - bitte eine Taste dr}cken !";
6540 I$=INKEY$:IF I$="" THEN 6540
6550 GOTO 6010
6700 'Tracer aufschl}sseln
6710 ' -> N%
6720 ' <- TYP$,NM$,SU%,N2$
6730 TRACER$=TRACER$(N%)
6740 TYP%=VAL(LEFT$(TRACER$,1))
6750 TRACER$=RIGHT$(TRACER$,LEN(TRACER$)-2):IF TRACER$="" THEN RETURN
6760 FOR K%=1 TO LEN(TRACER$)
6770 IF MID$(TRACER$,K%,1)="=" GOTO 6800
6780 NEXT K%
6790 NM$=TRACER$:RETURN
6800 NM$=LEFT$(TRACER$,K%-1)
6810 TRACER$=RIGHT$(TRACER$,LEN(TRACER$)-K%)
6820 IF LEFT$(TRACER$,1)="1" THEN SU%=1:TRACER$=RIGHT$(TRACER$,LEN(TRACER$)-1):GOTO 6850
6830 IF LEFT$(TRACER$,2)="-1" THEN SU%=-1:TRACER$=RIGHT$(TRACER$,LEN(TRACER$)-2):GOTO 6850
6840 PRINT @968,"**** Fehler in Tracer ****":STOP
6850 IF LEN(TRACER$)=0 THEN RETURN
6860 TRACER$=RIGHT$(TRACER$,LEN(TRACER$)-1)
6880 FOR K%=1 TO LEN(TRACER$)
6890 IF MID$(TRACER$,K%,1)="=" GOTO 6920
6900 NEXT K%
6910 N2$=TRACER$:RETURN
6920 N2$=LEFT$(TRACER$,K%-1)
6930 N3$=RIGHT$(TRACER$,LEN(TRACER$)-K%)
6950 RETURN
7000 'Anzeige einer bestimmten Regel
7010 CLS
7020 PRINT @5,"Anzeige einer bestimmten Regel:"
7030 PRINT @645,"Bitte Nr. oder Namen eingeben : ";:LINEINPUT BN$
7050 IF LEN(BN$)=0 GOTO 7030
7060 FOR N%=1 TO LEN(BN$)
7070 I$=MID$(BN$,N%,1)
7080 IF (ASC(I$)<48 OR ASC(I$)>57) AND I$<>" " GOTO 7130:'Name !
7090 NEXT N%
7100 N%=VAL(BN$)
7110 BN$=OA$(N%)
7120 GOTO 7190
7130 FOR M%=1 TO OBJEKTE%
7140 IF OA$(M%)=BN$ GOTO 7190
7150 NEXT M%
7160 PRINT @197+64,"*** Objekt '";BN$;"' nicht vorhanden ***"
7180 GOTO 7030
7190 FOR N%=1 TO TRACER%-1
7200 GOSUB 6730
7210 IF (NM$=BN$ AND TYP%<>1) GOTO 7275
7220 NEXT N%
7230 CLS:PRINT @197+64,"]ber ";BN$;"liegen keine Informationen vor."
7240 PRINT @968,"Bitte eine Taste dr}cken"
7250 I$=INKEY$:IF INKEY$="" GOTO 7250
7260 GOTO 6010
7270 'Begr}ndung anzeigen
7275 ON TYP% GOTO 7281,7520,8310,7310,7810
7281 PRINT @968,"**** Fehler in Trace ****":STOP
7300 'Frage anzeigen
7310 CLS
7320 PRINT @5,BN$;" trifft ";
7330 IF SU%=(-1) PRINT "nicht ";
7340 PRINT "zu"
7350 PRINT @133,"Dies wurde durch die Frage ermittelt :"
7360 PRINT @197+64,N2$
7370 PRINT @325+64,"Die Antwort war :"
7380 PRINT @389+64,N3$
7390 PRINT @968,"Bitte eine Taste dr}cken"
7400 I$=INKEY$:IF I$="" GOTO 7400
7410 GOTO 6010
7500 'und-Regel anzeigen
7520 IF SU%=-1 GOTO 7550
7530 IF SU%=1 GOTO 7640
7540 'Regel trifft nicht zu
7550 PRINT @133,NM$;" ist nicht er}llt, weil"
7570 I$=N2$
7580 IF VAL(N3$)=(-1) THEN I$="nicht "+I$
7590 PRINT @197+64,I$;"   <=== nicht zutrifft"
7600 PRINT @968,"Bitte eine Taste dr}cken"
7610 I$=INKEY$:IF I$="" GOTO 7610
7620 GOTO 6010
7630 'Regel erf}llt
7640 PRINT @133,NM$;" ist erf}llt weil"
7650 PRINT @197,"alle Pr{missen erf}llt sind :"
7660 N%=VAL(N2$)
7670 FOR M%=1 TO 5
7680 I$=WENN$(M%,N%)
7690 IF I$="" GOTO 7720
7700 IF WF%(M%,N%)=(-1) THEN I$="nicht "+I$
7710 PRINT @261+64*M%,I$
7720 NEXT M%
7730 PRINT @968,"Bitte eine Taste dr}cken"
7740 I$=INKEY$:IF I$="" GOTO 7740
7750 GOTO 6010
7800 'oder-Regel anzeigen
7810 CLS
7820 IF SU%=1 GOTO 7880
7830 IF SU%=-1 GOTO 8110
7870 'Regel trifft zu
7880 PRINT @133,NM$;" ist er}llt, weil"
7890 PRINT @197,"mindestens eine Pr{misse der"
7900 PRINT @261+7,"oder - Regel"
7910 PRINT @325,"erf}llt ist"
7920 I$=N2$
7930 IF VAL(N3$)=(-1) THEN I$="nicht "+I$
7940 PRINT @453,I$;"  <=== trifft zu"
7950 PRINT @968,"Bitte eine Taste dr}cken"
7960 I$=INKEY$: IF I$="" GOTO 7960
7970 GOTO 6010
7980 'Regel unbestimmt
7990 PRINT @968,"Bitte eine Taste dr}cken"
8000 I$=INKEY$:IF I$="" GOTO 8000
8010 GOTO 6010
8020 RETURN
8030 FOR M%=1 TO 5
8040 IF WENN$(M%,N%)<>"" THEN PRINT @197+M%*64,I$
8050 NEXT M%
8060 PRINT @968,"Bitte eine Taste dr}cken"
8070 I$=INKEY$:IF I$="" GOTO 8070
8080 RETURN
8090 'Regel nicht erf}llt
8110 PRINT @133,NM$;" ist nicht erf}llte, weil"
8120 PRINT @197,"keine Pr{misse der oder-Regel"
8130 PRINT @261+20,"----------"
8140 PRINT @325,"erf}llt ist"
8150 N%=VAL(N2$)
8160 FOR M%=2 TO 5
8170 I$=WENN$(M%,N%)
8180 IF WF%(M%,N%)=(-1) THEN I$="nicht "+I$
8190 PRINT @261+M%*64,I$;"  <=== trifft nicht zu"
8200 NEXT M%
8210 PRINT @968,"Bitte eine Taste dr}cken"
8220 I$=INKEY$: IF I$="" GOTO 8220
8230 GOTO 6010
8300 'Constraint
8310 CLS
8320 PRINT @133,BN$;"trifft ";
8330 IF SU%=(-1) THEN PRINT "nicht ";
8340 PRINT "zu"
8350 PRINT @197,"Dies gilt aufgrund des Constraint"
8360 PRINT @261,N2$;" ===> ";
8370 IF SU%=(-1) THEN PRINT "nicht ";
8380 PRINTNM$
8390 PRINT @968,"Bitte eine Taste dr}cken"
8400 I$=INKEY$:IF I$="" GOTO 8400
8410 GOTO 6010
10000 'Begr}ndung ausdrucken
10010 PRINT @968,"**** Druckroutine nicht installiert ****"
10020 GOTO 6010
