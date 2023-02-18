Function DNSCAN:Integer;
Var du : Integer;
Begin
  du := dirscan(dirs);
  If du = 0 Then du := duscan(dirs);
  dnscan := du
End;

