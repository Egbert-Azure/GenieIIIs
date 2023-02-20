program   POSITION_DEMO;

(*$I pos.inc *)

const     test: array[1..3] of string[80]=
                ('CHIP Turbo-PASCAL-Heft Nr. 9',
                 'Dies ist ein Pascal-Programm ...',
                 'Turbo-C, Turbo-Basic, Turbopascal');

{ gesucht means -> search
  Gesuchte Zeichenkette means -> search string}

          gesucht: array[1..5] of string[80]=
                   ('TURBO*HEFT','chiP*heFT','T*pascal','pas','ein*programm');

var       i,i1,start: integer;
const     strich='--------------------------------------------------------------------------------'; { 80 mal '-' }

procedure INVERS_COLOR;
begin     lowvideo;  end;

procedure NORMAL_COLOR;
begin     normvideo;  end;

begin     for i:=1 to 5
              do begin
                    clrscr; lowvideo;
                    writeln('Die Testdaten:');
                    write(strich);
                    normvideo;
                    for i1:=1 to 3 do writeln(test[i1]);
                    lowvideo; writeln(strich);
                    gotoxy(1,10); write('Gesuchte Zeichenkette: ');
                    normvideo; writeln(gesucht[i]); lowvideo;
                    gotoxy(1,13); write('Suchergebnisse:');
                    gotoxy(1,14); write(strich);
                    gotoxy(1,20); write(strich);
                    normvideo;
                    for i1:=1 to 3
                        do begin
                              start:=POSITION(gesucht[i],test[i1]);
                              if start>0
                                 then begin
                                         gotoxy(1,13+i1*2); writeln(test[i1]);
                                         INVERS_COLOR;
                                         gotoxy(start,13+i1*2);
                                         write(copy(test[i1],start,poslaenge));
                                         NORMAL_COLOR;
                                      end;
                           end;
                    gotoxy(1,25); write('Bitte Return druecken ! ');
                    read;
                 end;
end.
