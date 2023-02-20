PROGRAM Zinseszinsrechnung;

TYPE  Info   = RECORD
                   spa:byte;
                   fel:byte;
                   max:real;
                   sch:real;
                END;
      Zahlen = SET OF '0'..'9';
VAR   j,b      :integer;
      c        :char;

CONST Maske1:ARRAY[1..3] OF Info = ((spa:6;fel:12;max:1000000000.0;sch:10),
                                    (spa:28;fel:6;max:1000;sch:0.1),
                                    (spa:44;fel:6;max:1000;sch:1)           );
      Pruefmenge:zahlen = ['1'..'3'];
PROCEDURE menue;
       BEGIN
          ClrScr;
          gotoxy(22,2);  writeln ('CHIP - Special Zinseszinsrechnung');
          gotoxy(30,8);  writeln ('1:  Aufzinsen');
          gotoxy(30,10); writeln ('2:  Abzinsen ');
          gotoxy(30,12); writeln ('3:  Annuitaetenberechnung');
          gotoxy(30,14); writeln ('4:  Ende');
          gotoxy(22,19); write('Ihre Wahl ? ');
       END;

PROCEDURE Hilfe;
       BEGIN
         gotoxy(10,17);
         writeln('Bedienung:   ',#27,' ',#26,'   Anwahl eines Parameters');
         gotoxy(10,18);
         writeln('             ',#24,' ',#25,'   Aenderung eines Parameters');
         gotoxy(23,19);writeln('ESC   Ende');
       END; { Hilfe }

PROCEDURE bildaufbauen(Wahl:integer);

 BEGIN
   ClrScr;
   gotoxy(22,2);writeln('CHIP - Special Zinseszinsrechnung');
   gotoxy(29,5);
      CASE Wahl OF
           1: writeln('  A U F Z I N S E N');
           2: writeln('   A B Z I N S E N');
           3: writeln('ANNUITAETENBERECHNUNG');
      END; { case }
  gotoxy(5,8);
  writeln('                    Zinssatz p.a.   Kapitalbindung');
  gotoxy(7,9);
  writeln('   Kapital         in Prozent       in Monaten        Ergebnis');
  gotoxy(10,16);writeln('Bitte Ihre Eingaben...');
 END;

PROCEDURE print_wert(wert:real;s,f:integer);
          BEGIN
            gotoxy(s,12);write(wert:f:2);
          END;

FUNCTION get_wert(spalte,feld:integer):real;
         VAR zahl     :real;
             result,m :integer;
             puffer   :STRING[13];
         BEGIN
             puffer:='';m:=feld-3;
             REPEAT
               gotoxy(spalte+feld,12);
               read(kbd,c);
               CASE c OF
                      '.': IF (length(puffer)<m+1) AND (pos('.',puffer)=0)
                              THEN BEGIN
                                     puffer:=puffer+c;
                                     m:=length(puffer)+2;
                                   END;
                '0'.. '9': IF length(puffer)<m THEN puffer:=puffer+c;
                      #8 : BEGIN
                             IF pos('.',puffer)=length(puffer) THEN m:=feld-3;
                             IF length(puffer)>1
                                THEN delete(puffer,length(puffer),1)
                                ELSE puffer:='';
                           END;
               END; { case }
             gotoxy(spalte,12);writeln(puffer:feld);
             UNTIL (c=#13) AND (puffer<>'');
             val(puffer,zahl,result);
             print_wert(zahl,spalte,feld);
             get_wert:=zahl;
          END; {get_wert}

PROCEDURE Fehler;
        BEGIN
          gotoxy(9,22);writeln('Illegale Bereichsueberschreitung',^G);
          gotoxy(58,12);write('XXXXXXXXX.XX');
          delay(2000);
          gotoxy(9,22);ClrEol;
        END;

PROCEDURE Berechne(Art:integer;Kapital,Zinssatz,Frist:real);

VAR Ergebnis,AFaktor,Zaehler,Nenner:real;
BEGIN
  CASE Art OF
    1: Ergebnis:=Kapital*exp(ln((1+(Zinssatz/100)))*(Frist/12));     { Aufz.}
    2: Ergebnis:=Kapital*1/(exp(ln((1+(Zinssatz/100)))*(Frist/12))); { Abz. }
    3: BEGIN                                        { Annuitaetenberechnung }
          Zinssatz:=Zinssatz/100;
          Nenner := (exp(ln((1+(Zinssatz/12)))*Frist) - 1);
          Zaehler:= (exp(ln((1+(Zinssatz/12)))*Frist)*((1+Zinssatz/12)-1));
          IF (Zaehler<=0) OR (Nenner<=0) THEN BEGIN Fehler; Exit; END;
          AFaktor:= Zaehler/Nenner;
          Ergebnis:=(Kapital*AFaktor);
       END; { 3 }
  END; { case }
  IF Ergebnis > 999999999.99 THEN BEGIN Fehler;Exit; END;
  print_wert(Ergebnis,58,12);
END;{ Berechne }

PROCEDURE ceteris_paribus(rechnung:integer;Akapital,Zins,AFrist:real);

          VAR ch:char; zahl:real; a:integer;
          BEGIN
            zahl:=Akapital;a:=1;
            REPEAT
             WITH Maske1[a] DO BEGIN
              LowVideo; print_wert(zahl,spa,fel); NormVideo;
              read(kbd,ch);
                IF (ch=#27) AND keypressed THEN BEGIN
                read(kbd,ch);
                  CASE ch OF
                       #77 : BEGIN
                                Print_Wert(Zahl,spa,fel);
                                a:=a+1; IF a>3 THEN a:=1;
                                CASE a OF
                                   1: zahl:=Akapital;
                                   2: zahl:=Zins;
                                   3: zahl:=AFrist;
                                END; { case }
                             END;
                       #75 : BEGIN
                                Print_Wert(Zahl,spa,fel);
                                a:=a-1; IF a<1 THEN a:=3;
                                CASE a OF
                                   1: zahl:=Akapital;
                                   2: zahl:=Zins;
                                   3: zahl:=AFrist;
                                END; { case }
                             END;
                       #72 : IF zahl< max-sch  THEN  BEGIN
                                zahl:= zahl+sch;
                               END;
                       #80 : IF zahl-sch >= 0 THEN   BEGIN
                                zahl:= zahl-sch;
                               END;
                  END; { case };
                  IF a=1 THEN Akapital:=zahl;
                  IF a=2 THEN Zins:=zahl;
                  IF a=3 THEN AFrist:=zahl;
                  Berechne(Rechnung,AKapital,Zins,AFrist);
                END; { if }
              END; { with }
            UNTIL (ch=#27) AND NOT keypressed;
          END; { ceteris_paribus }

PROCEDURE Starte(Methode:integer);

          VAR ka,r,n :real;

          BEGIN
          bildaufbauen(Methode);
          ka:=get_wert(6,12);
          r:=get_wert(28,6);
          n:=get_wert(44,6);
          gotoxy(1,16);ClrEol;
          Berechne(Methode,ka,r,n);
          Hilfe;
          ceteris_paribus(Methode,ka,r,n);
          END; { Starte }

BEGIN { Hauptprogramm }
REPEAT
  menue;
  read(kbd,c);
  IF c IN pruefmenge THEN BEGIN val(c,b,j); starte(b); END;
UNTIL c='4';
ClrScr;
END.
