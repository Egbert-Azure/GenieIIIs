(*$I c5:window.par *)
(*$I c5:window.bib *)
type Zeile = string(.20.);
var z : zeile;
procedure ZufallsZeilen(Var Z:Zeile);
  var i: 1..20;
  begin
    z(.0.):=#20;
    for i:=1 to 20 do z(.i.):=chr(32+random(95));
  end;

begin
  InitWindows;
  OpenWindow(10,5,31,15); (* Fenster 1 *)
  OpenWindow(50,5,71,15); (* Window 2 *)
  repeat
    ChangeWindow(1); ZufallsZeilen(z); write(z);
    ChangeWindow(2); ZufallsZeilen(z);
    gotoxy(1,1); InsLine; gotoxy(1,1); write(z)
  until keypressed;
  ExitWindows;
end.