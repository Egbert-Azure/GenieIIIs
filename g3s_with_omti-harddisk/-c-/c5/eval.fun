Function EVAL(RAD:Integer; NUM:strng):Real;
Const T = '0123456789ABCDEF';
Var   N : Real;
Begin
  n := 0;
  While Length(num) <> 0 do
    Begin
      n := n * rad + Pos(num[1],t)-1;
      Delete(num,1,1)
    End;
  eval := n;
End;

