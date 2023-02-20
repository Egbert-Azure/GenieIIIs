Procedure PARSE(Var S:str21);
Var P : Integer;
Begin
  dirs[0] := #0;
  P := Pos(':',S);
  If P <> 0 Then
  Begin
    dirs := Copy(S,1,P);
    dirs[0] := Chr(Length(dirs)-1);
    Delete(S,1,P);
  End;
End;

