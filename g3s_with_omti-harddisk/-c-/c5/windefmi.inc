(*****************************************************************************)
(* Definitionen fuer Programm MINITED                                        *)
(*****************************************************************************)
(*         SelWindow - Fensterwahl                                           *)
(*****************************************************************************)
(*  hier muessen die Fenstergroessen definiert werden, die mehrfach genutzt  *)
(*  werden sollen. Maximal koennen 8 Fenster gleichzeitig geoeffnet werden   *)
(*****************************************************************************)

procedure SelWindow(x:byte);
begin
  case x of
    1: OpenWindow(x,2,4,12,50);
    2: OpenWindow(x,1,22,0,79);
    3: (*Frei*);
    4: (*Frei*);
    5: (*Frei*);
    6: (*Frei*);
    7: (*Frei*);
    8: (*Frei*);
  end
end;