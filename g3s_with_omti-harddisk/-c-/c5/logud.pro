Procedure LOGUD(DU:Integer);
Begin
  Bdos(14,Hi(DU)-1); Bdos(32,Lo(DU))
End;

