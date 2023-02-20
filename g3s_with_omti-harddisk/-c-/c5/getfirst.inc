procedure GetFirst(var M:ByteSet; var Element:byte);
  begin
    Element:=0;
    repeat
      if Element in M then begin M:=M-[Element]; exit end;
      Element:=succ(Element);
    until Element=0
  end; (* GetFirst *)