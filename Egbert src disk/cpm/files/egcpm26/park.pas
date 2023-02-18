program parkhd;
var  cfield:  record
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
begin      {Hauptprogramm}
    writeln ('Parking Heads on Track # 610 !');
    park_heads (error);
    if error then begin
      writeln ('Heads incorrectly parked  !');
      writeln;
    end;
    writeln ('Command completed !')
end.
