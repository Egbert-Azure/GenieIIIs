procedure LiesZeichen(Var c : char); (* zur Tastaturanpassung (Turbo-Special 3)           *)

begin
  read(kbd,c);
     { case c of hier noch Holte Codes definieren
        geht aber auch so !!
        ? : c:=^E; (* Cursor hoch *)
        ? : c:=^X; (* Cursor tief *) 
        ? : c:=^S; (* Cursor links*) 
        ? : c:=^D; (* Cursor rechts *)
        ? : c:=^A; (* Wort links *)
        ? : c:=^F; (* Wort rechts *)
        ? : c:=^T; (* Wort l\schen *) 
        ? : c:=^Y; (* Zeile l\schen *)
        ? : c:=^C; (* Seite tief *)
        ? : c:=^H; (* Zeichen links vom Cursor l\schen *)
        ? : c:=^G; (* Zeichen unter Cursor l\schen *)
     end }
end; (* LiesZeichen *)
