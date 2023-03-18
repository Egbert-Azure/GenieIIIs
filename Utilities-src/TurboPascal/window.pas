program window;

{ Testprogramm um dir WINDOW Funktionen des HOLTE CP/M's
  anzutesten. Es werden mehrere Windows verschiedener Groesse
  aufgemacht und Zeichenketten dort hineingeschrieben.         }

var     anystring          :      string[255];


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

fenster(1,4,10,12,60);
    clrscr;
     writeln('Dies ist Fenster Nummer 1.');
     write  ('Die sind folgendermassen : Oben=4, Unten=10, Links=12');
     writeln(' Rechts=60.');
     write('Das hier soll nur den gewaehlten Bereich voll machen, so das');
     write(' bei der jetzt folgenden Eingabe das scrollen beobachtet werden');
     write(' kann.');
     readln(anystring);
     writeln;

fenster(2,20,23,60,79);
    clrscr;
     write(chr(27),chr(82));    { Bildschirmsequenz fuer INVERS  }
     write('Dies ist jetzt ein  zweites zusaetzliches Fenster !');
     write(chr(27),chr(83));    { Bildschirmsequenz INVERS AUS   }

repeat until keypressed;

fenster(0,0,23,0,79);
     write('Und wieder alles ganz normal.');

repeat until keypressed;

end.
