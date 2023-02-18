
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


