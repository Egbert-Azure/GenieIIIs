Function RETUD:Integer;
Begin
  retud := 256 * (Bdos(25)+1) + Bdos(32,$FF)
End;

