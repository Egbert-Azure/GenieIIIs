program cpmTools;

(*$I c5:CPM-80.BIB   *)
(*$I c5:WINDOW.PAR   *)
(*$I c5:WINDOW7.BIB   *)
(*$I c5:READCHAR.INC *)
(*$I c5:CHOOSE.BIB   *)
(*$I c5:SELECT-1.BIB *)

const Menue : array[1..5] of string[10] =
              ('File','Edit','N.N.','Hilfe','Ende');

var   Wahl : integer;
      weiter : boolean;

procedure UnterMenue(var naechster : integer; var weiter : boolean);
  var SubMenue : array[1..5] of string[10];
      i, SubWahl : integer;
  begin
    for i:=1 to 5 do SubMenue[i]:='Sub '+chr(48+Naechster)+'.'+chr(48+i);
    OpenWindow(2+pred(naechster)*15,3,1+naechster*15,9);
    SubWahl:=1; weiter:=true;
    repeat
      Selekt(4,1,SizeOf(SubMenue[1]),SubMenue,5,SubWahl);
      case SubWahl of
       1..5 : begin
                OpenWindow(30,10,50,15);
                writeln('Gewaehlt wurde : ');
                writeln(SubMenue[SubWahl]);
                readln; CloseWindow
              end;
         -1 : if naechster>1 then naechster:=pred(naechster)
                             else naechster:=4;
         -2 : if naechster<4 then naechster:=succ(naechster)
                             else naechster:=1;
          0 : weiter:=false
      end; (* CASE *)
    until SubWahl<=0;
    CloseWindow
  end; (* UnterMenue *)

begin (* DropDownDemo *)
  Cursor_Off;
  InitWindows; ClrScr; OpenWindow(1,1,80,4); Wahl:=1; weiter:=false;
  repeat
    Choose(1,1,15,SizeOf(Menue[1]),Menue,5,Wahl,weiter);
    if Wahl in [1..4] then UnterMenue(Wahl,weiter)
  until Wahl=5; CloseWindow; ExitWindows;
  Cursor_On
end.  (* cpmTools *)