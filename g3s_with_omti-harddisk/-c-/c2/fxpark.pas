{ ---------------------------------------------- }
{ Programm FXPARK.PAS                            }
{ Only for use with Genie 3s with OMTI Controller}
{------------------------------------------------}
{ Parkt die Festplatte mit OMTI xxxx Controller }
{ Autor: Volker Dose                            }
{ Original Programm-Name: Park.pas              }
{ ein wenig herumgepfuscht hat Egbert Schr|er   }
{ Die Window Routine sollte man eigentlich als  }
{ Bibliotheks-Modul definieren und einbinden    }
{ Ebenso Invers und Normal und aehnliche        }
{ Nuetzlichkeiten                               }
{ --------------------------------------------- }
{ WINDOWG.BIB am 31.05.93 eingefuegt            }
{ Error Routine verbessert                      }
{ Zur Anpassung an die eigene Festplatte        }
{ Anzahl der Tracks richtig einstellen und neu  }
{ compilieren. E.S. 05.06.93                    }
{ --------------------------------------------- }

program parkhd;

var
        Antwort: char      ;
        cfield:  record
        command:    byte;
        address:    byte;
        sector:     byte;
        track:      byte;
        bcount:     byte;
        termin:     byte
        end;
        cfarray:                array [0..5]   of byte absolute cfield;
        error:                  boolean;
        park_cyl:               integer;
{ ==================================================================== }
(*$I C5:CPM-80.BIB*)
(*$I C5:WINDOW.PAR*)
(*$I C5:WINDOWG.BIB*)
{ ==================================================================== }
procedure wait_controller_ready;
var  status:  byte;
begin
  repeat
    status:=port[$41] and 1;
  until status=1
end;
{ -------------------------------------------------------------------- }
procedure send_command;
var  i:  byte;
begin
  port[$42]:=0;                 { select controller }
  for i:= 0 to 5 do begin       { send command with }
    wait_controller_ready;        { parameters }
    port[$40]:=cfarray[i]
  end
end;
{ -------------------------------------------------------------------- }
procedure read_result;
var  i,x:  byte;
begin
  writeln ('Read Result !');
  for i:=1 to 4 do begin
    wait_controller_ready;
    x:=port[$40];
    write (i:8);
    writeln (x:8)
    {  if i=4 then res[i]:=res[i]+port[$40]  }
    {  else res[i]:=port[$40];  }
    {  if i=3 then   }
    {  if res[i] >= 64 then res[i+1]:=(res[i] div 64)*2  }
    {  else res[i+1]:=0  }
  end
end;
{ -------------------------------------------------------------------- }
function  error_check:  boolean;
var  err:     boolean;
     status:  byte;
begin
  wait_controller_ready;        {  wait until controller is ready  }
  status:=port[$40];            {  read controller status  }
  err:=false;                   {  reset error flag  }
  if status and 2 <> 0 then begin
     cfield.command:=3;         {  in case of error read error info  }
     cfield.address:=0;         {  from controller  }
     cfield.sector:=0;
     cfield.track:=0;
     cfield.bcount:=1;
     cfield.termin:=2;
     send_command;
     read_result;
     err:=true                  {  set error flag  }
  end;
  error_check:=err
end;
{ -------------------------------------------------------------------- }
procedure park_heads  (var err: boolean);
const  park_cyl = 610;          { heads parking position }
begin
   cfield.command:=$0b;
   cfield.address:=0;
   cfield.sector:=((park_cyl div 4) and $c0);
   cfield.track:=park_cyl mod 256;
   cfield.bcount:=1;
   cfield.termin:=$02;
   send_command;
   err:=error_check             { test controller status }
end;
{ -------------------------------------------------------------------- }
procedure  fenster_1;
begin
  save_screen(addr(buffer));
  OpenWindow(1,3,14,11,61);
  ClrScr;

end;
{ -------------------------------------------------------------------- }
procedure Titel;
begin
  ClrScr;
  LowVideo;
  GotoXY(14,2);
  writeln('Parken der Festplatte');
  NormVideo;
  GotoXY(12,4);
  LowVideo;
  writeln('Wollen Sie die Festplatte');
  GotoXY(20,5);
  writeln('parken ?');
  NormVideo;
  GotoXY(10,7);
  write(#153#154#155);                { Zeichen fuer Hand }
  GotoXY(18,7);
  writeln('(J)a, (N)ein');
  writeln;
    write(#151#151#151#151#151#151#151#151);
  write(' '#152' by  Volker Dose / Brodersdorf ');
  writeln(#151#151#151#151#151#151#151#151);
end;
{ -------------------------------------------------------------------- }
procedure fenster_2;
begin
  OpenWindow(2,15,20,11,61);
  ClrScr;
end;
{ -------------------------------------------------------------------- }
procedure Message_1;
begin
  ClrScr;
  write(#153#154#155);                { Zeichen fuer Hand }
  writeln(' Parking Heads on Track # 610 !');
end;
{ -------------------------------------------------------------------- }
procedure Message_2;
begin
  writeln('    Bitte schalten Sie den Rechner aus');
  write('    oder '#153#154#155' ');
  LowVideo;
  write('Taste');
  NormVideo;
  writeln(' fuer Unterbrechen');
  Cursor_Off;
  repeat until keypressed;
  Cursor_On;
end;
{ -------------------------------------------------------------------- }
procedure ErrorMessage;
begin
  write(#153#154#155);                { Zeichen fuer Hand }
  writeln(' Heads incorrectly parked !');
  readln(anystring);
  repeat until keypressed;
end;
{ -------------------------------------------------------------------- }
procedure Taste;
begin
  Cursor_Off;
  read (KBD,Antwort);
  Antwort := upcase(Antwort);
  Cursor_On;
end;
{ -------------------------------------------------------------------- }
label exit;
begin      {Hauptprogramm}
    fenster_1;
    Titel;
    Taste;
    if Antwort = 'J' then begin
      fenster_2;
      Message_1;
      park_heads (error);
      if error then begin
        ErrorMessage;
        goto exit;
      end;
    Message_2
    end;
    exit:
    ExitWindow;
    Cursor_On
end.
