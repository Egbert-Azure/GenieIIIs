program window;

{ Testprogramm um dir WINDOW Funktionen des HOLTE CP/M's
  anzutesten. Es werden mehrere Windows verschiedener Groesse
  aufgemacht und Zeichenketten dort hineingeschrieben.         }

var     anystring          :      string[255];
        buffer             :      array[0..2000] of byte;
        i,j                :      byte;

(* FENSTER.PRO ---------------------------------------------------------------

 Mit Hilfe der folgenden Funktionen ist es moeglich auf dem Genie IIIs
Fenster auf dem Bildschirm zu oeffnen, mit frei waehlbaren Grenzen UNTEN,
OBEN, LINKS und RECHTS. Der urspruengliche Bildschirminhalt kann gerettet
werden und wieder restauriert natuerlich. Das ist moeglich indem mit der
USERF Funktion des BIOS Funktion Nummer 22 (Direkter Bildschirmzugriff)
angesprungen wird. Die Fensterfunktionen werden mit Escapesequenzen
realisiert.

Die Proceduren  Save_Screen(zeiger_buffer : integer) und
                Restore_Screen(zeiger_buffer : integer)

bekommen als Uebergabeparameter den Zeiger auf ein Bufferfeld

var     buffer  : array[1..2000] of byte;

der mit         addr(buffer)           realisiert wird.

Die Procedure Fenster wird mit den Parametern

    fenster_nummer
    top_line
    bottom_line
    left_column
    right_coloumn    (alles bytes)

aufgerufen. (siehe weiter unten)

-----------------------------------------------------------------------------*)


(* Direkter BIOS-Aufruf ueber BDOS-Funktion 50 *)

function UBIOS(fn,pa,pbc,pde,phl:integer):integer;
var biospb : record
               func,a   : byte;
               bc,de,hl : integer;
               end;
    result : integer;
begin
  with biospb do begin
    func:=fn; a:=pa;
    bc:=pbc; de:=pde; hl:=phl;
    end;
  result:=0;
  case fn of
    2,3,7,13..15,17..19,24 : result:=BDOS(50,addr(biospb));
    9,16,20,22,25          : result:=BDOSHL(50,addr(biospb));
    else                     BDOS(50,addr(biospb));
    end;
  ubios:=result;
  end;




procedure Save_Screen(zeiger_buffer : integer);
var   nixwert : integer;
begin
nixwert := ubios(30,0,22,0,zeiger_buffer);
end;

procedure Restore_Screen(zeiger_buffer : integer);
var nixwert : integer;
begin
nixwert := ubios(30,1,22,0,zeiger_buffer);
end;


{ Procedure  FENSTER
   eroeffnet auf dem Bildschirm einen Bereich mit neu definierten
  Bildfenstergroessen. Alle Bildschirmausgaben nach Aufruf
  dieser Funktion beziehen sich nur noch auf die neuen Bildschirm-
  groessen.                                                         }


procedure fenster (windownummer,top_line,bottom_line,left_column,right_column : byte);

const   window_char        =      'F';           { Die uebergebenen Parameter }
        set_top            =      'I';           { muessen mit 32 addiert     }
        set_bottom         =      'J';           { werden, steht so im HOLTE  }
        set_left           =      'K';           { Handbuch CPM3.DOC.         }
        set_right          =      'L';           { Durch die Definition der   }
        escape             =      #27;           { Konstanten innerhalb der   }
                                                 { Prozedur kann sie einfach  }
begin                                            { 'so' mit $I eingebunden    }
write(escape,window_char,chr(windownummer+32));  { werden.                    }
write(escape,set_top,chr(top_line+32));          {                            }
write(escape,set_bottom,chr(bottom_line+32));    {                            }
write(escape,set_left,chr(left_column+32));      {                            }
write(escape,set_right,chr(right_column+32));    {                            }
end;


{ Hauptprogramm   }


begin
save_screen(addr(buffer));

fenster(3,3,12,11,61);  (* Hier soll ein Rahmen gemalt werden, zwecks Uebersicht*)
clrscr;
write(#142);
for i := 1 to 49 do write(#129);
writeln(#141);
for j := 1 to 7 do
   begin
   write(#128);
   for i:=1 to 49 do write(' ');
   writeln(#128);
   end;
write(#140);
for i := 1 to 49 do write(#129);
writeln(#139);

fenster(1,4,10,12,60);
    clrscr;
     writeln('Dies ist Fenster Nummer 1.');
     writeln('Hier koennte jetzt auch irgendeine Systemmeldung');
     writeln('stehen, mit der irgendetwas wichtiges mitgeteilt');
     writeln('werden kann.');
     writeln('Das hier soll nur den gewaehlten Bereich voll machen,');
     writeln('bei der jetzt folgenden Eingabe das scrollen beobachtet');
     writeln('werden kann.');
     writeln('Druecke nach der Eingabe noch einmal <ENTER>.');
     readln(anystring);
     writeln;

  repeat until keypressed;

fenster(0,0,23,0,79);
restore_screen(addr(buffer));


fenster(2,20,23,55,79);
    clrscr;
     write(chr(27),chr(82));    { Bildschirmsequenz fuer INVERS  }
     writeln('Dies ist jetzt ein');
     writeln('zweites zusaetzliches');
     writeln('Fenster !');
     write(chr(27),chr(83));    { Bildschirmsequenz INVERS AUS   }

repeat until keypressed;

fenster(0,0,23,0,79);
restore_screen(addr(buffer));
     write('Und wieder alles ganz normal.');

repeat until keypressed;

end.
