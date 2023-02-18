program grafik1;


type  str4               =        string[4];

var   nummer             :          integer;
      bc_register        :          integer;
      dummi              :          integer;


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


function bios(nummer,bc_register:integer):integer;
var      bios_parameterblock : record
                                 funktionsnummer,
                                 akku              :  byte;
                                 bc_reg,
                                 de_reg,
                                 hl_reg            : integer;
                               end;
begin
 with bios_parameterblock do
    begin
      funktionsnummer := succ(nummer);
      bc_reg := bc_register;
      akku   :=  255;

    end;

  bios := bdos(50,addr(bios_parameterblock));

end;



begin
write('Der HRG Schirm wird eingeschaltet.');
dummi := bios(30,25);
end.
