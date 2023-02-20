(****************************************************************************)
(*  TED-1.INC   Typdefinition und Basisprozeduren f}r den Turbo-Editor TED  *)
(****************************************************************************)

TYPE  TextListe = ^TextZeile;              (* TextListe ist eine doppelt *)
      TextZeile = RECORD                   (* verkettete Liste           *)
                    Zeile : DynString;
                    Vor, Rueck : TextListe
                  END;


FUNCTION HeapAvail : INTEGER;
  VAR temp : INTEGER;
  BEGIN
    temp:=MaxAvail; IF temp>0 THEN HeapAvail:=temp ELSE HeapAvail:=MaxInt
  END; (* HeapAvail *)

PROCEDURE NeuerText(VAR T : TextListe);    (* Initialisierung *)
  BEGIN
    T:=NIL;
  END; (* NeuerText *)

PROCEDURE LiesText(VAR f : TEXT; VAR T : TextListe);
  VAR l : StatString;
      d : DynString;
      p,q : TextListe;
  BEGIN
    NeuerText(T); reset(f); q:=T;
    WHILE NOT EOF(f) DO BEGIN
      IF HeapAvail<1000 THEN BEGIN close(f); EXIT END;
      ReadLn(f,l); NEW(p);
      WITH p^ DO BEGIN MakeDynString(l,Zeile); Rueck:=q; Vor:=NIL END;
      IF q<>NIL THEN q^.Vor:=p ELSE T:=p;
      q:=p
    END; (* WHILE *)
    close(f)
  END; (* LiesText *)

PROCEDURE SchreibText(VAR f : TEXT; T : TextListe);
  BEGIN
    rewrite(f);
    WHILE T<>NIL DO WITH T^ DO BEGIN WriteLn(f,Zeile^); T:=Vor END;
    close(f);
  END; (* SchreibText *)

PROCEDURE LoescheText(VAR T : TextListe);
  VAR p : TextListe;
  BEGIN
    WHILE T<>NIL DO BEGIN
      p:=T^.Vor; ForgetDynString(T^.Zeile); dispose(T); T:=p
    END (* WHILE *)
  END; (* LoescheText *)