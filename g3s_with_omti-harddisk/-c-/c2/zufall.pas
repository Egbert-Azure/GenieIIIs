program Zufall_Test; {Chi-Quadrat und K-S-Test fr Zufallszahlen}


const Bereich = 10;            {Bereich der Zufallszahlen 1..10}
      Anzahl = 100;            {Anzahl der Zufallszahlen}

var Fb : array[1..Bereich] of integer; {beobachtete Meawerte}
    f : text;
    Zufall : 1..Bereich;
    i,x    : integer;
    {fr Chi-Quadrat-Test:}
       Fe,                     {erwarter Wert}
       ChiQu  : real;          {Chi-Quadrat}
    {fr Kolmogorow-Smirnow-Test:}
       tk_Vert_x,              {theoretische kummulative Verteilung von x}
       bk_Vert_x: real;        {beobachtete kummulative Verteilung von x}
       MaxAbw, Abw : real;     {(max.) Abweichung zwischen beob. kummulierten
                                Hufigkeiten und theoret. kumm. Hufigkeiten}
begin
  assign(f,'CON:'); rewrite(f); ClrScr;
  ChiQu:=0; Fe:=Anzahl/Bereich;
  tk_Vert_x:=0;  bk_Vert_x:=0; MaxAbw:=0;
  writeln(f,'Test der random-Funktion'); writeln(f);

  {>>>>>>> Ermittlung der Zufallswerte: <<<<<<<<}
  fillchar(Fb,SizeOf(Fb),0); randomize;
  FOR i:=1 TO Anzahl DO BEGIN
    Zufall:=random(Bereich)+1;
    Fb[Zufall]:=succ(Fb[Zufall]);
  END;

  {>>>>>>> Chi-Quadrat-Test:            <<<<<<<<}
  writeln(f,'Chi-Quadrat-Test fr eine Stichprobe: ');
  writeln(f,'Wert':12,'beobachtet':12,'erwartet':12,
            'Differenz':12,'quadriert':12,'quad./erw.':12);
  for i:=1 to 75 do write(f,'-'); writeln(f);
  for x:=1 to Bereich do begin
    ChiQu:=ChiQu + sqr(Fb[x]-Fe)/Fe;
    writeln(f,x:12,Fb[x]:12,Fe:12:2,Fb[x]-Fe:12:2,
    sqr(Fb[x]-Fe):12:2,sqr(Fb[x]-Fe)/Fe:12:2);
  END;
  writeln(f); writeln(f,'Chi-Quadrat-Wert: ':68,ChiQu:4:2); writeln(f);
  writeln(f,'Kritische CHI-Quadrat-Werte bei 9 Freiheitsgraden:');
  writeln(f,
  '%   99    95    90    80    70    50    30    20    10     5     1');
  writeln(f,
  '   2.0  3.33  4.17  5.38  6.39  8.34  10.8  12.2  14.7  16.9  21.7');
  writeln(f);

  {>>>>>>> Kolmogorov-Smirnov-Test      <<<<<<<<}
  writeln(^G, 'Fr K-S-Test bitte RETURN drcken'); readln; ClrScr;
  write(f,'Kolmogorov-Smirnov-Test fr eine Stichprobe');
  writeln(f,' auf Gleichverteilung'); writeln(f);
  writeln(f,'Wert':10,'erw.':10,'kumm.':10,'beob.':10,'kumm.':10,
            'Abw.':10,'Max-Abw.':10);
  for i:=1 to 75 do write(f,'-'); writeln(f);
  FOR x:=1 TO Bereich DO BEGIN
    tk_Vert_x := x/Bereich;
    bk_Vert_x := bk_Vert_x + Fb[x]/Anzahl;
    Abw:=tk_Vert_x - bk_Vert_x;
    IF MaxAbw < abs(tk_Vert_x - bk_Vert_x)
    THEN MaxAbw := abs(tk_Vert_x - bk_Vert_x);
    writeln(f,x:10, 1/Bereich:10:2, tk_Vert_x:10:2, Fb[x]/Anzahl:10:2,
              bk_vert_x:10:2, Abw:10:2, MaxAbw:10:2);
  END;
  writeln(f);
  writeln(f,'Absolutwert der maximalen Abweichung (D): ',MaxAbw:7:2);
  writeln(f,'Kritischer D-Wert bei Signifikanzniveau von: ');
  writeln(f,'5%: ':20,1.36 / sqrt(Anzahl):7:2,
            '1%: ':20,1.63 / sqrt(Anzahl):7:2); writeln(f);
  writeln(f,'Bei Gleichverteilung liegt berechnetes D unter krit. D-Wert!');
  close(f);
end.