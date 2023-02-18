     PRINT
     PRINT  "+------------------------------------+"
     PRINT  "|  CBASIC  Cross Reference  Program  |"
     PRINT  "|                                    |"
     PRINT  "|  after J.Monagan, KBMC 3/83 P.110  |"
     PRINT  "|     complain to R.Schoene, PCC     |"
     PRINT  "+------------------------------------+"
     PRINT

     FF$=CHR$(12)
     C$="-----------": FOR I=1 TO 7: SSS$=SSS$+C$: NEXT
     DIM RWS$(100),PT%(25)
     SC=500: SK=91: SN=SK-26: SA=SN+1-ASC("A")
     DIM VNXT%(SC+SK),V$(SC+SK),FRST%(SC),LST%(SC),RFL%(5*SC),NXT%(5*SC)

	REM	********     reserved words

	DATA ABS,AND,ASC,AS,ATN,CALL,CHAIN,CHR$,CLOSE,COMMON
	DATA CONSOLE,COS,CREATE,CRUN,DATA
	DATA DEF,DELETE,DIM
	DATA ELSE,END,EQ,EXP,FEND,FILE,FOR,FRE
	DATA GE,"GO SUB","GO TO",GOSUB,GOTO
	DATA IF,INPUT,INP,INT,LEFT$,LE,LEN,LET,LINE
	DATA LOG,LPRINTER,LT,MATCH,MID$
	DATA NE,NEXT,NOT,ON,OPEN,OR,OUT
	DATA PEEK,POKE,POS,PRINT,RANDOMIZE,READ,RECL
	DATA REM,REMARK,RENAME,RESTORE,RETURN,RIGHT$,RND
	DATA SGN,SIN,SIZE,SQR,STEP,STOP,STR$,STRING$
	DATA TAB,TAN,THEN,TO
	DATA USING,VAL,WEND,WIDTH,XOR,~

	REM	********     fill array with reserved words

	RW=0
300	READ RW$
	RW=RW+1: RWS$(RW)=RW$
	IF RW$="~" THEN 350
	I=ASC(RW$)-ASC("A")
	IF PT%(I)=0 THEN PT%(I)=RW
	GOTO 300

350	FOR I=0 TO 25
	IF PT%(I)=0 tHEN PT%(I)=RW
	NEXT

	REM	********     get source file name

	F=1
410	PRINT: INPUT" CBASIC SOURCE FILE NAME = "; LINE F$
	IF F$="" THEN STOP
	IF MATCH(".",F$,2)=0 THEN F$=F$+".BAS"
	IF END #F THEN 410
	OPEN F$ AS F

	PRINT: INPUT"INCLUDE '%INCLUDED' PROGRAMS  (Y/N) "; C$
	IF MID$(C$,1,1)="Y" OR MID$(C$,1,1)="y" THEN INC=1 ELSE INC=0
	PRINT: INPUT"DATE = "; D$
	PRG$="'"+F$+"'   -   "+D$

	GOSUB 610
	LPRINTER: PRINT FF$: CONSOLE
	STOP

	REM	********     initialize for cross reference

610	LC=0: PZ=0: V$="": C$="": VC=SK: RC=-1
	FOR I=0 TO SK: VNXT%(I)=-1: NEXT

630	IF END #F THEN 1200
650	IF F=1 THEN LN$=":" ELSE LN$="="

	REM	********     input source liNe

670	READ #F; LINE L$
	LG=LEN(L$): BRNCH=0: LC=LC+1
	LP=0: LN=LC: PRINT STR$(LN); LN$

	REM	********     check for compiler directives

	IF INC=0 OR MATCH("%INCLUDE",L$,1)<>1 THEN 700
	LP=9
690	C$=MID$(L$,LP,1)
	IF C$<"A" AND C$>"" THEN LP=LP+1: GOTO 690
	F$=MID$(L$,LP,14)
	IF F$="" THEN 670

	REM	********     open included source file

	PRINT L$
	IF END #F+1 THEN 670
	OPEN F$ AS F+1: GOTO 630
	
700	IF MID$(L$,1,1)="%" THEN 670

	REM	********     parse line

750	LP=LP+1
	IF LP>LG THEN GOSUB 1010: GOTO 670
	C$=MID$(L$,LP,1)
	IF C$>="A" AND C$<="Z"    THEN 1110
	IF C$>="a" AND C$<="z"    THEN C$=CHR$(ASC(C$)-32): GOTO 1110
	IF C$>="0" AND C$<="9"    THEN 1150
	IF C$ =" " OR  C$=CHR$(9) THEN GOSUB 1010: GOTO 750
	IF C$ ="." AND V$>=""     THEN 1120
	IF C$ ="\"		  THEN GOSUB 1010: GOTO 670
	IF C$<>","		  THEN BRNCH=0

	IF C$<>CHR$(34) THEN 780
	GOSUB 1010: LP=MATCH(C$,L$,LP+1)
	IF LP>0 THEN GOTO 750 ELSE GOTO 670

780	IF C$="$" OR C$="%" THEN 1130
	GOSUB 1010
	GOTO 750

	REM	********     test for command

890	C=ASC(V$): P=PT%(C-ASC("A")): BRNCH=0: RW$=""

900	IF C<ASC(RWS$(P)) THEN RETURN
	IF V$<>RWS$(P) THEN P=P+1: GOTO 900
	RW$=V$
	IF V$="DATA" OR V$="REM" OR V$="REMARK" THEN LP=LG+1: RETURN
	IF V$="GOTO" OR V$="GOSUB" OR V$="THEN" THEN BRNCH=1
	RETURN

	REM	********     end variable

1010	IF V$="" THEN RETURN
	IF ASC(V$)<ASC("A") THEN 1030
	GOSUB 890
	IF RW$>"" THEN V$="": RETURN
	IF C$="(" THEN V$=V$+C$
	C=ASC(V$)+SA: IL=-1: I=C
1020	IF LEFT$(V$+"        ",9)<=LEFT$(V$(I)+"        ",9) THEN 1050
	IL=I: I=VNXT%(I)
	IF I>0 THEN GOTO 1020 ELSE GOTO 1060

1030	IF VAL(V$)<1000 THEN C=INT(VAL(V$)/100) ELSE C=9+INT(VAL(V$)/1000)
	IF C>SN THEN C=SN
	IL=-1: I=C
1040	IF VAL(V$)<=VAL(V$(I)) THEN 1050
	IL=I: I=VNXT%(I)
	IF I>0 THEN GOTO 1040 ELSE GOTO 1060

1050	IF V$<>V$(I) THEN 1060
	J=LST%(I-SK)
	IF RFL%(J)=LN THEN GOTO 1090 ELSE RC=RC+1: NXT%(J)=RC: GOTO 1080
1060	VC=VC+1
	IF IL>=0 THEN VNXT%(IL)=VC
	V$(VC)=V$: VNXT%(VC)=I: RC=RC+1: FRST%(VC-SK)=RC: I=VC
1080	RFL%(RC)=LN: NXT%(RC)=-1: LST%(I-SK)=RC
1090	V$="": RETURN

	REM	********     expand variable

1110	IF VAL(V$)>0 AND VAL(V$)=VAL(V$+MID$(L$,LP,255)) THEN GOSUB 1020
1120	V$=V$+C$: GOTO 750
1130	IF V$<>"" THEN V$=V$+C$
	GOTO 750
1150	IF V$>"" OR BRNCH>0 THEN V$=V$+C$
	GOTO 750

	REM	********     end of source file

1200	CLOSE F
	IF F>1 THEN F=F-1: GOTO 650

	REM	********     print symbol table

	PZ=0: LPRINTER WIDTH 80: GOSUB 1400: SZ=-1
	FOR J=0 TO SK: V=J
1230	V=VNXT%(V)
	IF V<0 THEN 1340
	IF LZ>56 THEN GOSUB 1400: GOTO 1250
	SZ=SZ+1
	IF SZ=3 THEN GOSUB 1410
1250	RZ=0: I=FRST%(V-SK): PRINT V$(V);
1260	IF RZ=0 THEN PRINT TAB(16);
	LN=RFL%(I)
	PRINT USING"     ####"; LN;
	RZ=RZ+1
	IF RZ<6 THEN 1280
	RZ=0: PRINT: LZ=LZ+1
	IF LZ>56 THEN GOSUB 1400

1280	I=NXT%(I)
	IF I>0 THEN 1260
	IF RZ>0 THEN PRINT: LZ=LZ+1
	GOTO 1230
1340 	NEXT J

	PRINT SSS$
	PRINT"LINES:";LC;"    SYMBOLS:";VC-SK;"    REFERENCES:";RC+1
	LZ=LZ+2: RETURN
1400	GOSUB 1520: PRINT"SYMBOL";TAB(20);"REFERENCE LINE": LZ=LZ+1
1410	PRINT SSS$: LZ=LZ+1: SZ=0: RETURN

1520	PRINT FF$
	PZ=PZ+1: PRINT TAB(71);"Page";PZ
	PRINT PRG$: PRINT
	LZ=3: RETURN

	END
