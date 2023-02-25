PROGRAM HIP;

 CONST  QUADRATE=105;LAENGE=36;
        A:ARRAY[1..QUADRATE,1..4]OF BYTE=
         ((1,6,36,31),(1,5,29,25),(2,6,30,26),(7,11,35,31),
          (8,12,36,32),(1,4,22,19),(2,5,23,20),(3,6,24,21),
          (7,10,28,25),(8,11,29,26),(9,12,30,27),(13,16,34,31),
          (14,17,35,32),(15,18,36,33),(1,3,15,13),(2,4,16,14),
          (3,5,17,15),(4,6,18,16),(7,9,21,19),(8,10,22,20),
          (9,11,23,21),(10,12,24,22),(13,15,27,25),(14,16,28,26),
          (15,17,29,27),(16,18,30,28),(19,21,33,31),(20,22,34,32),
          (21,23,35,33),(22,24,36,34),(1,2,8,7),(2,3,9,8),
          (3,4,10,9),(4,5,11,10),(5,6,12,11),(7,8,14,13),
          (8,9,15,14),(9,10,16,15),(10,11,17,16),(11,12,18,17),
          (13,14,20,19),(14,15,21,20),(15,16,22,21),(16,17,23,22),
          (17,18,24,23),(19,20,26,25),(20,21,27,26),(21,22,28,27),
          (22,23,29,28),(23,24,30,29),(25,26,32,31),(26,27,33,32),
          (27,28,34,33),(28,29,35,34),(29,30,36,35),(2,12,35,25),
          (3,18,34,19),(4,24,33,13),(5,30,32,7),(2,11,28,19),
          (3,17,27,13),(4,23,26,7),(3,12,29,20),(4,18,28,14),
          (5,24,27,8),(8,17,34,25),(9,23,33,19),(10,29,32,13),
          (9,18,35,26),(10,24,34,20),(11,30,33,14),(2,10,21,13),
          (3,16,20,7),(3,11,22,14),(4,17,21,8),(4,12,23,15),
          (5,18,22,9),(8,16,27,19),(9,22,26,13),(9,17,28,20),
          (10,23,27,14),(10,18,29,21),(11,24,28,15),(14,22,33,25),
          (15,28,32,19),(15,23,34,26),(16,29,33,20),(16,24,35,27),
          (17,30,34,21),(2,9,14,7),(3,10,15,8),(4,11,16,9),
          (5,12,17,10),(8,15,20,13),(9,16,21,14),(10,17,22,15),
          (11,18,23,16),(14,21,26,19),(15,22,27,20),(16,23,28,21),
          (17,24,29,22),(20,27,32,25),(21,28,33,26),
          (22,29,34,27),(23,30,35,28));

 TYPE AN_DER_REIHE=(MENSCH,COMPUTER);

 VAR FL,S:ARRAY[1..LAENGE]OF INTEGER;
     STAERKE,X,K,L,FELD,I,J:INTEGER;
     SPIELER:AN_DER_REIHE;
     ERFOLG,ANFANG,COMPUTER_VERLIERT,MENSCH_VERLIERT,REMIS:BOOLEAN;
     TASTE:CHAR;

(*$I c5:VIDEO.BIB*)
(*$I c5:window.par*)

 PROCEDURE SPIELFELD;
  VAR SP,ZE,K:INTEGER;
  BEGIN
   CLRSCR;Invers;
   GOTOXY(24,3);WRITE('A  n  t  i  q  u  a  d  r  a  t');
   Normal;SP:=7;ZE:=6;
   GOTOXY(SP,ZE);WRITELN('    1   2   3   4   5   6');
   GOTOXY(SP,ZE+1);WRITELN('  ',SenkrechtL,'DDDBDDDBDDDBDDDBDDDBDDD?');
   FOR K:=1 TO 5 DO
    BEGIN
     GOTOXY(SP,ZE+2*K);WRITELN(K,' 3   3   3   3   3   3   3',K:2);
     GOTOXY(SP,ZE+1+2*K);WRITELN('  CDDDEDDDEDDDEDDDEDDDEDDD4')
    END;
   GOTOXY(SP,ZE+12);WRITELN('6 3   3   3   3   3   3   3 6');
   GOTOXY(SP,ZE+13);WRITELN('  @DDDADDDADDDADDDADDDADDDY');
   GOTOXY(SP,ZE+14);WRITELN('    1   2   3   4   5   6');
  END;

PROCEDURE REGELN;
 VAR SP,ZE:INTEGER;
     TASTE:CHAR;
 BEGIN
  SPIELFELD;SP:=40;ZE:=9;
  GOTOXY(SP,ZE);WRITE('Es werden abwechselnd vom Spieler und');
  GOTOXY(SP,ZE+1);WRITE('Computer Felder besetzt. Wer zuerst');
  GOTOXY(SP,ZE+2);WRITE('4 Felder besitzt, die die Ecken eines');
  GOTOXY(SP,ZE+3);WRITE('Quadrates bilden, der hat verloren !');
  GOTOXY(SP,ZE+4);WRITE('Mein Stein : ');
  Invers;WRITE('O');Normal;
  GOTOXY(SP,ZE+5);WRITE('Dein Stein : ');
  Invers;WRITE('X');Normal;
  GOTOXY(SP,ZE+7);WRITE('Eingabe: ( Zeile / Spalte )');
  GOTOXY(SP+14,ZE+9);WRITE('Viel Spass !');
  GOTOXY(SP+10,ZE+11);Invers;
  WRITE('Druecke die Leertaste !');
  REPEAT READ(KBD,TASTE) UNTIL TASTE=' ';
  Normal;CLRSCR
 END;

PROCEDURE EINGABE;
 VAR SP,ZE:INTEGER;
     TASTE:CHAR;
 BEGIN
  SP:=50;ZE:=9;
  GOTOXY(SP,ZE);WRITE('Deine Eingabe');
  GOTOXY(SP+2,ZE+2);WRITE('(   /   )');
  GOTOXY(SP+4,ZE+2);
  REPEAT READ(KBD,TASTE) UNTIL TASTE IN['1'..'6'];
  WRITE(TASTE);I:=ORD(TASTE)-ORD('0');
  GOTOXY(SP+8,ZE+2);
  REPEAT READ(KBD,TASTE) UNTIL TASTE IN['1'..'6'];
  WRITE(TASTE);J:=ORD(TASTE)-ORD('0');
  X:=(I-1)*6+J
 END;

PROCEDURE FEHLER;
 VAR K:INTEGER;
 BEGIN
  FOR K:=1 TO 2 DO
   BEGIN
   GOTOXY(50,13);
   WRITE('Fehlerhafte Eingabe !');DELAY(2000);
   GOTOXY(50,13);
   WRITE('Bitte wiederholen !  ');DELAY(2000);
  END;
 GOTOXY(50,13);CLREOL
END;

PROCEDURE ZUFALLSZUG;
  VAR ZUG,K,L:INTEGER;
      FERTIG:BOOLEAN;
  BEGIN
   ZUG:=0;
   REPEAT
    ZUG:=SUCC(ZUG);
    REPEAT X:=RANDOM(LAENGE)+1 UNTIL (S[X]=0) AND (FL[X]=0);
    S[X]:=2;FL[X]:=2;K:=0;FERTIG:=FALSE;
    WHILE (K<QUADRATE) AND NOT FERTIG DO
     BEGIN
      K:=SUCC(K);
      IF S[A[K,1]]+S[A[K,2]]+S[A[K,3]]+S[A[K,4]]=8 THEN
       IF ZUG=FELD THEN COMPUTER_VERLIERT:=TRUE
       ELSE BEGIN S[X]:=0;FERTIG:=TRUE END
     END
   UNTIL S[X]=2;
   FL[X]:=1;FOR L:=1 TO LAENGE DO IF FL[L]=2 THEN FL[L]:=0
  END;

 PROCEDURE STRATEGIEZUG;
  VAR ZUG,K,L:INTEGER;
      FERTIG:BOOLEAN;
  BEGIN
   X:=J*6+1-I;ERFOLG:=TRUE;
   IF S[X]<>0 THEN
    BEGIN
     X:=6*I+1-J;
     IF S[X]<>0 THEN
      BEGIN X:=36-6*I+J;IF S[X]<>0 THEN ERFOLG:=FALSE END
    END;
   IF NOT ERFOLG THEN ZUFALLSZUG
   ELSE
    BEGIN
     ZUG:=0;
     REPEAT
      ZUG:=SUCC(ZUG);
      S[X]:=2;FL[X]:=2;K:=0;FERTIG:=FALSE;
      WHILE (K<QUADRATE) AND NOT FERTIG DO
       BEGIN
        K:=SUCC(K);
        IF S[A[K,1]]+S[A[K,2]]+S[A[K,3]]+S[A[K,4]]=8 THEN
         IF ZUG=FELD THEN COMPUTER_VERLIERT:=TRUE
         ELSE BEGIN S[X]:=0;FERTIG:=TRUE END
       END;
      IF S[X]=0 THEN
       REPEAT X:=RANDOM(LAENGE)+1 UNTIL (S[X]=0) AND (FL[X]=0);
     UNTIL S[X]=2;
     FL[X]:=1;FOR L:=1 TO LAENGE DO IF FL[L]=2 THEN FL[L]:=0
    END
  END;

BEGIN{ Hauptprogramm }
 Normal;CLRSCR;
 GOTOXY(10,3);WRITE('Brauchst du die Spielregeln (J/N) ? ');
 REPEAT
  READ(KBD,TASTE);TASTE:=UPCASE(TASTE)
 UNTIL TASTE IN['J','N'];
 WRITE(TASTE);IF TASTE='J' THEN REGELN;
 GOTOXY(10,5);WRITE('Welche Spielstaerke (1/2) ? ');
 REPEAT
  READ(KBD,TASTE)
 UNTIL TASTE IN['1','2'];WRITE(TASTE);
 STAERKE:=ORD(TASTE)-ORD('0');GOTOXY(10,7);
 WRITE('Willst Du den ersten Zug setzen (J/N) ? ');
 REPEAT
  READ(KBD,TASTE);TASTE:=UPCASE(TASTE)
 UNTIL TASTE IN['J','N'];
 IF TASTE='J' THEN SPIELER:=MENSCH ELSE SPIELER:=COMPUTER;
 IF TASTE='J' THEN ANFANG:=TRUE ELSE ANFANG:=FALSE;
 REMIS:=FALSE;COMPUTER_VERLIERT:=FALSE;
 MENSCH_VERLIERT:=FALSE;FELD:=LAENGE;
 FOR K:=1 TO LAENGE DO S[K]:=0;FL:=S;SPIELFELD;
 REPEAT
  IF SPIELER=MENSCH THEN
   BEGIN
    REPEAT EINGABE;IF S[X]<>0 THEN FEHLER UNTIL S[X]=0;
    S[X]:=-2;FL[X]:=1;K:=0;
    GOTOXY(7+4*J,6+2*I);WRITE('X');
    WHILE (K<QUADRATE) AND NOT MENSCH_VERLIERT DO
     BEGIN
      K:=SUCC(K);
      IF S[A[K,1]]+S[A[K,2]]+S[A[K,3]]+S[A[K,4]]=-8 THEN
       MENSCH_VERLIERT:=TRUE
     END
   END
  ELSE
   BEGIN
    IF FELD=LAENGE THEN ZUFALLSZUG
    ELSE IF STAERKE=1 THEN ZUFALLSZUG
         ELSE STRATEGIEZUG;
    I:=X DIV 6+1;J:=X MOD 6;IF J=0 THEN BEGIN J:=6;I:=I-1 END;
    GOTOXY(7+4*J,6+2*I);WRITE('O');
    GOTOXY(50,17);WRITE('Computer setzt auf (',I:2,' /',J:2,' )');
   END;
  FELD:=PRED(FELD);IF FELD=0 THEN REMIS:=TRUE;
  IF SPIELER=MENSCH THEN SPIELER:=COMPUTER ELSE SPIELER:=MENSCH
 UNTIL REMIS OR COMPUTER_VERLIERT OR MENSCH_VERLIERT;
 GOTOXY(11,22);
 IF REMIS THEN
  WRITELN('Das Spiel endet remis ! Welch ein Match !!!');
 IF COMPUTER_VERLIERT THEN
  BEGIN
   WRITE('Ich verliere, denn alle freien Felder');
   WRITELN(' sind Ecken eines Quadrates !')
  END;
 IF MENSCH_VERLIERT THEN
  BEGIN
   WRITELN('Du hast verloren ! Die Felder deines Quadrates :');
   GOTOXY(11,23);
   FOR L:=1 TO 4 DO
    BEGIN
     X:=(A[K,L]-1) DIV 6;I:=X+1;J:=A[K,L]-6*X;
     WRITE('(',I:2,' /',J:2,' )')
    END
  END
END.