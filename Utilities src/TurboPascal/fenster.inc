
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
