program wintest;

(*$I c5:CPM-80.BIB  *)
(*$I c5:WINDOW.PAR  *)
(*$I c5:WINDOWG.BIB *)
(*$I c5:WINDEF.INC  *)

{ Hauptprogramm   }

begin

save_screen(addr(buffer));

OpenWindow(1,4,10,12,63);
     writeln('Dies ist Zeile 1');
     writeln('Dies ist Zeile 2');
     writeln('Dies ist Zeile 3');
     writeln('Dies ist Zeile 4');
     write('Dies ist Zeile 5');
     readln(anystring);
     repeat until keypressed;

OpenWindow(2,2,8,10,60);
     writeln('Dies ist Fenster Nummer 2');
     readln(anystring);
     repeat until keypressed;

SelWindow(1);
     writeln('Und nun wieder Fenster 1');
     readln(anystring);
     repeat until keypressed;

SelWindow(2);
     writeln('Oh wie schoen. Dies ist wieder');
     writeln('Fenster Nummer 2');
     writeln('Jetzt ist aber Schluss');
     readln(anystring);
     repeat until keypressed;

ExitWindow;
Cursor_On
end.
