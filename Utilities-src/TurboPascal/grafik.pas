program grafik;

type   biospb=record
                 func    :     byte;
                 areg    :     byte;
                 bcreg   :  integer;
                 dereg   :  integer;
                 hlreg   :  integer;
              end;

type  str4               =        string[4];

var   parameterblock     :           biospb;
      blockzeiger        :          ^biospb;
      dirbios            :             byte;
      uebergebe          :          integer;



Function HEX(N:Integer; B:Integer) : str4;
Const    T : Array[0..15] of Char = '0123456789ABCDEF';
Var      D : Integer; H : str4;
Begin
  H[0]     := Chr(2*B);
  For D    := 2*B DownTo 1 Do
  Begin
    H[D]   := T[N And 15];
    N      := N Shr 4
  End;
  hex := H
End;



begin

writeln('Testprogramm um dir HRG einzuschalten ');
parameterblock.func   :=   50;
parameterblock.areg   :=  255;
parameterblock.bcreg  :=   25;
parameterblock.dereg  :=    0;
parameterblock.hlreg  :=    0;

writeln('Die dem BDOS }bergebenen Parameter lauten wie folgt:');
writeln;
writeln('Funktionsnummer     : ',parameterblock.func);
writeln('Inhalt von  A       : ',parameterblock.areg);
writeln('Inhalt von BC       : ',parameterblock.bcreg);
writeln('Inhalt von DE       : ',parameterblock.dereg);
writeln('Inhalt von HL       : ',parameterblock.hlreg);
writeln;
write('Der Zeiger auf diesen Block hat Adress ');

uebergebe := addr(parameterblock.func);

writeln(hex(uebergebe,2));

writeln('Der Inhalt dieser Adresse ist : ',mem[uebergebe]);
writeln('Der Inhalt der naechsten  ist : ',mem[uebergebe+1]);
writeln('                              : ',mem[uebergebe+2]);
writeln('                              : ',mem[uebergebe+3]);
writeln('                              : ',mem[uebergebe+4]);
writeln('                              : ',mem[uebergebe+5]);
writeln('                              : ',mem[uebergebe+6]);
writeln('                              : ',mem[uebergebe+7]);


dirbios       :=           50;

bdos(dirbios,uebergebe);

writeln('Das wars dann.');
end.
