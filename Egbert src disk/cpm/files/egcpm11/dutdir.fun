Function DUTDIR(DU:Integer):Boolean;
{ DUTDIR searches Z3NDIR for the DU reference and, if found,   }
{ will return the associated Name and Pass variables and True. }
Var ndir, I : Integer;
Begin
  name[0] := #0;
  pass[0] := #0;
  ndir    := GETNDR;
  Repeat
    If du = Swap(get(ndir)) Then
      Begin
        name[0] := #8; { Initially 8 chars long }
        For I := 1 to 8 Do
        name[I] := Chr(Mem[ndir+I+1]);
        I := Pos(' ',name);
        If I <> 0 Then name[0] := Chr(I-1);
        pass[0] := #8; { Initially 8 chars long }
        For I := 1 to 8 Do
        pass[I] := Chr(Mem[ndir+I+9]);
        I := Pos(' ',pass);
        If I <> 0 Then pass[0] := Chr(I-1);
      End;
    ndir := ndir+18;
  Until (Length(name) <> 0) or (Mem[ndir] = 0);
  dutdir  := Length(name) <> 0;
End;

