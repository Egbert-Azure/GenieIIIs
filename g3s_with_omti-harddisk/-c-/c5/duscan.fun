Function DUSCAN(S:dirstr):Integer;
{ Resolve S to Rel 1 Drive/User.  S is any of D:, U: or DU: }
{ If not possible, return Zero.                             }
Const D = 'ABCDEFGHIJKLMNOP';
Var DU, usr, cod : Integer;
Begin
  DU := curdu;  { Initial Drive/User (Rel 1) }
  If Length(S) <> 0 Then
    Begin
      If Pos(s[1],d) <> 0 Then
        Begin
          du := Pos(s[1],d)*256 + (du and 255);
          Delete(s,1,1);
        End;
      If Length(s) <> 0 Then
        Begin
          Val(s,usr,cod);
          If cod <> 0 Then du := 0 Else du := (du And -256) + usr
        End;
    End;
  duscan := DU;
End;

