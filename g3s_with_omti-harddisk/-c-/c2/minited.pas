PROGRAM MiniTed;

(*$I c5:READCHAR.INC *)  { Angepasste Tastatureingabe          }
(*$I c5:DYNSTR.BIB   *)  { Dynamische Strings fuer Turbo-Pascal}
(*$I c5:TED-1.INC    *)  { Deklarationen von TED               }
(*$I c5:TED-2.INC    *)  { Der erste Teile von EditText        }
(*$I c5:CTRLQ.INC    *)  { Das Ctrl-Q-Menue                    }
(*$I c5:CTRLK.INC    *)  { Das Ctrl-K-Menue                    }
(*$I c5:TED-3.INC    *)  { Der zweite Teil von EditText        }


(* Hauptproramm *)

VAR T : TextListe;
    f : Text;
    X, Y, Z : INTEGER;
    c : char;
    s : string[080];

BEGIN (* MiniTed *)
  CBreak:=FALSE;
  IF ParamCount=0 THEN
    BEGIN
      ClrScr; Writeln('    MINITED    ');
      Write('  Editfile : '); readln(s);
      assign(f,s);
    end
  else assign(f,ParamStr(1));
  (*$I-*) reset(f); (*$I+*)
  IF IOResult=0 THEN BEGIN close(f); LiesText(f,T) end ELSE NeuerText(T);
  X:=1; Y:=1; Z:=1; ClrScr; EditText(T,X,Y,Z,79,25,TRUE);
  gotoxy(1,1); Write('Speichern ? (J/N) '); clreol;
  repeat read(kbd,c); c:=upcase(c) until c in ['J','N'];
  write(c);
  if c='J' then SchreibText(f,T);
  LoescheText(T); ClrScr
end.  (* MiniTed *)
