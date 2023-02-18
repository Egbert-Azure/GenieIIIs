Function GET(I:Integer):Integer;
Begin
  GET := Mem[I+1] * 256 + Mem[I]
End;

