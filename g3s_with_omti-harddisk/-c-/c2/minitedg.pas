{ Diese Version von MiniTed ist nur fuer Genie 3s unter Holte   }
{ CP/M 3.0. Herr Holte hat ueber Bios Funktion 30 den Zugriff   }
{ auf Fenstertechnik eingebaut.                                 }
{ Um Verbreitung unter Genie 3s Benutzern wird ausdruecklich    }
{ gebeten.                                                      }
{ Cursor Position muesste noch gespeichert werden, um nach      }
{ restore_screen den Cursor an die "richtige" Position zu       }
{ bringen.                                                      }
{ Copyright: Die urspruengliche Version von MiniTed wurde von
             CHIP Turbo Pascal Special Ausgabe 9 veroeffentlicht.
             Von mir stammt nur die Erweiterung um die Fenster-
             technik. Ansprung des Bios und erste Gehversuche
             mit Holte CP/M und Window stammen von Volker Dose.
             Egbert Schroeer
             Joachimstrasse 18
             4270 Dorsten                                       }



PROGRAM MiniTed;

(*$I c5:CPM-80.BIB   *)  {                                     }
(*$I c5:WINDOW.PAR   *)  { hier Modifikationen fuer Zeichensatz}
(*$I c5:WINDOWG.BIB  *)  { Window fuer Genie 3s                }
(*$I c5:WINDEFMI.INC *)  { Window Definitionen fuer TED        }
(*$I c5:READCHAR.INC *)  { Angepasste Tastatureingabe          }
(*$I c5:DYNSTR.BIB   *)  { Dynamische Strings fuer Turbo-Pascal}
(*$I c5:TED-1.INC    *)  { Deklarationen von TED               }
(*$I c5:TED-2.INC    *)  { Der erste Teile von EditText        }
(*$I c5:CTRLQ.INC    *)  { Das Ctrl-Q-Menue                    }
(*$I c5:CTRLK.INC    *)  { Das Ctrl-K-Menue                    }
(*$I c5:TED-3.INC    *)  { Der zweite Teil von EditText        }

procedure Logo;
  begin
      save_screen(addr(buffer));
      ClrScr; Write  ('    MINITED    fuer Genie 3s');
              Writeln('  Version 1.0 - '#152' ES (Juni 1993) ');
  end;

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
      Logo;
      SelWindow(1);
      ClrScr;
      Write(' Editfile : '); readln(s);
      assign(f,s);
      SelWindow(2);gotoxy(1,1); (* Textbereich oeffnen *)
      ClrScr; Cursor_On;
    end
  else assign(f,ParamStr(1));
  (*$I-*) reset(f); (*$I+*)
  IF IOResult=0 THEN BEGIN close(f); LiesText(f,T) end ELSE NeuerText(T);
  X:=1; Y:=1; Z:=1; ClrScr; EditText(T,X,Y,Z,77,21,TRUE);
  SelWindow(1);
  gotoxy(1,1); Write('Speichern ? (J/N) '); clreol;
  repeat read(kbd,c); c:=upcase(c) until c in ['J','N'];
  write(c);
  if c='J' then SchreibText(f,T);
  LoescheText(T); ClrScr;
  ExitWindow;
  Cursor_On
end.  (* MiniTed *)
