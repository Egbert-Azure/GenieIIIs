{ --------------------------------------------- }
{ Programm FXPARK.PAS                           }
{ Parkt die Festplatte mit OMTI xxxx Controller }
{ Autor: Volker Dose                            }
{ Original Programm-Name: Park.pas              }
{ ein wenig herumgepfuscht hat Egbert Schr|er   }
{ Die Window Routine sollte man eigentlich als  }
{ Bibliotheks-Modul definieren und einbinden    }
{ Ebenso Invers und Normal und aehnliche        }
{ Nuetzlichkeiten                               }
{ --------------------------------------------- }

program parkhd;

var     anystring          :      string[255];
        Antwort: char      ;
        buffer             :      array[0..2000] of byte;
        i,j                :      byte;
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

(* FENSTER.PRO

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

---- *)


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

(*-------------------------------------------------*)

procedure Invers;
begin
  LowVideo;
end;

(*-------------------------------------------------*)

procedure Normal;
begin
  NormVideo;
end;

(*-------------------------------------------------*)

procedure Cursor_On;
begin
  write(#27,#13);
end;

(*-------------------------------------------------*)

procedure Cursor_Off;
begin
  write(#27,#12);
end;

(*-------------------------------------------------*)





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
{ Das Zeichnen des Rahmens ist noch nicht ganz TURBO like }
{ und somit stark Verbesserungswuerdig }
begin
  save_screen(addr(buffer));
{ zunaechst Fenster etwas groesser }
  fenster (1,3,14,11,61);
  ClrScr;
  write(#138);
  for i := 1 to 49 do write(#129);
    writeln(#137);
  for j := 1 to 7 do
    begin
      write(#128);
      for i:= 1 to 49 do write (' ');
      writeln(#128);
    end;
  write(#132);
  for i := 1 to 49 do write(#129);
  writeln(#131);

  write(#128#151#151#151#151#151#151#151#151);
  write(' '#152' by  Volker Dose / Brodersdorf ');
  writeln(#151#151#151#151#151#151#151#151#128);

  write(#136);
  for i := 1 to 49 do write(#129);
  writeln(#135);
{ nun die eigentliche Fenstergroesse festlegen }
  fenster(1,4,10,12,60);
end;
{ -------------------------------------------------------------------- }
procedure Titel;
begin
  ClrScr;
  Invers;
  GotoXY(14,2);
  writeln('Parken der Festplatte');
  Normal;
  GotoXY(12,4);
  Invers;
  writeln('Wollen Sie die Festplatte');
  GotoXY(20,5);
  writeln('Parken ?');
  Normal;
  GotoXY(10,7);
  write(#153#154#155);                { Zeichen fuer Hand }
  GotoXY(18,7);
  write('(J)a, (N)ein');
end;
{ -------------------------------------------------------------------- }
procedure fenster_2;
{ Das Zeichnen des Rahmens ist noch nicht ganz TURBO like }
{ und somit stark Verbesserungswuerdig }
begin
{ Fenster fuer das Outfit wieder groesser }
  fenster (2,15,20,11,61);
  ClrScr;
  write(#138);
  for i := 1 to 49 do write(#129);
    writeln(#137);
  for j := 1 to 3 do
    begin
      write(#128);
      for i:= 1 to 49 do write (' ');
      writeln(#128);
    end;
  write(#136);
  for i := 1 to 49 do write(#129);
    writeln(#135);
{ dann die entgueltige Fenstergroesse festlegen }
  fenster(2,16,18,13,60);
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
  Invers;
  write('Taste');
  Normal;
  writeln(' fuer Unterbrechen');
  cursor_off;
  repeat until keypressed;
  cursor_on;
end;
{ -------------------------------------------------------------------- }
procedure ErrorMessage;
begin
  write(#153#154#155);                { Zeichen fuer Hand }
  writeln(' Heads incorrectly parked !');
end;
{ -------------------------------------------------------------------- }
procedure  Monitor_80;
begin
  fenster(0,0,23,0,79);
  restore_screen(addr(buffer));
end;
{ -------------------------------------------------------------------- }
procedure Taste;
begin
  cursor_off;
  read (KBD,Antwort);
  Antwort := upcase(Antwort);
  cursor_on;
end;
{ -------------------------------------------------------------------- }

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
        writeln;
      end;
    Message_2
    end;
    Monitor_80;
end.
