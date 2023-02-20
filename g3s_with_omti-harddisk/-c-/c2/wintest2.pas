(*$I window.par *)
(*$I window.bib *)
var i: 0..9;
begin
  InitWindows;
  for i:=0 to 9 do OpenWindow(succ(4*i), succ(i), succ(40+4*i), succ(10+i));
  repeat
    readln(i); SelWindow(i)
  until i=0;
  SelWindow(9); While ScreenPtr>0 do CloseWindow;
  ExitWindows
end.