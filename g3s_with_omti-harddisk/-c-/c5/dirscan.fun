Function DIRSCAN(S:dirstr):Integer;
{ Search Z3NDIR for directory name matching the string at S }
{ Return Rel 1 Drive/User if found, Zero if not.            }
Var ndir, D, I : Integer;
Begin
  ndir := GETNDR;
  If Mem[ndir] <> 0 Then
  Repeat
    name[0] := #8; { Initially 8 chars long }
    For I := 1 to 8 Do
    name[I] := Chr(Mem[ndir+I+1]);
    I := Pos(' ',name);
    If I <> 0 Then name[0] := Chr(I-1);
    If S = name Then D := Swap(get(ndir)) Else D := 0;
    ndir := ndir+18;
  Until (D <> 0) or (Mem[ndir] = 0);
  dirscan := D;
End;

