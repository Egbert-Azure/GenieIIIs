Procedure Z3LOG(fcb:fcbarr);
Begin
  if fcb[0] <> 0 then logud(fcb[0] * 256 + fcb[13])
End;

