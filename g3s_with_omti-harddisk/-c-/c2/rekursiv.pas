PROGRAM rekursion_demo;

VAR k0,i,kn:real;
    n      :integer;

{$A-}               { Compilerbefehl nur fuer CP/M }

PROCEDURE Aufzinsen(AnfangsKapital,Zinssatz,Frist:real;VAR wert:real);
          VAR Ergebnis:real;
          BEGIN
              Ergebnis:=Anfangskapital*(1+(Zinssatz/100));
              Frist:=Frist-1;
              IF Frist>0 THEN Aufzinsen(Ergebnis,Zinssatz,Frist,wert)
              ELSE wert:=ergebnis;
          END;

{$A+}               { Compilerbefehl nur fuer CP/M }

BEGIN
 write('Startkapital:');readln(k0);
 write('Zinssatz in Prozent:');readln(i);
 REPEAT
  write('Anlagedauer (ganzzahlig):');readln(n);
 UNTIL (n>0);
 Aufzinsen(k0,i,n,kn);
 writeln('Ergebnis: ',kn:12:2);
END.
