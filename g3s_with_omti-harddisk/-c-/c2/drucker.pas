(*************************************************************************)
(*  DRUCKER.PAS     Drucker an Serieller Schnittstelle      1.1.88 w     *)
(*                                                                       *)
(*     Demoprogramm fuer einen Drucker an serieller Schnittstelle        *)
(*     (Erklaerungen siehe Text)                                         *)
(*************************************************************************)
program Drucker_an_serieller_Schnittstelle;

var f : text;

procedure writeDrucker(c:char);
  begin
    write(f,c);   { Eine gleichzeitige Ausgabe an die Konsole ist hier }
  end;            { nicht moeglich, denn dann wird nur noch das erste  }
                  { Zeichen an den Drucker uebermittelt.               }


begin
  assign(f,'LPT1');
  rewrite(f);
  LstOutPtr:=ofs(writeDrucker);
  writeln(lst,'Das ist eine TestZeile');
end.