Function HEX(N:Integer; B:Integer):str4;
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

