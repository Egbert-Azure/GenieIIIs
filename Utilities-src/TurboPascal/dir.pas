{$U-,C-,R-}

{

  *
  *  DIR.PAS
  *
  *  nach Eingabe einer Dateimaske werden passende Dateien angezeigt
  *
  *
  *  (c)  26.2.1991  -  FuzzySoft
  *

}


{$I CPM31.INC  -  Zugriff auf Betriebssystem CP/M 3.1   }


type
  Str80= string[80];


var
  f:     file;
  fName: cpmStr12;
  Dir:   cpmDir;
  FCB:   array[0..39] of byte;
  Drive: integer;


function GetFileName( var fName: cpmStr12 ): boolean;
begin
  writeln;
  write( 'DIR> ' );
  readln( fName );
  while( pos( ' ', fName ) > 0 ) do
    delete( fName, pos( ' ', fName ), 1 );
  GetFileName:= ( fName <> '' );
end;


procedure InitProgram;
begin
  writeln;
  writeln( '-------------------' );
  writeln( 'DIR 1.0' );
  writeln( 'Disketten-Directory' );
  writeln( '-------------------' );
end;


begin
  InitProgram;
  while( GetFileName( fName )) do begin
    ParseFileName( FCB, fName );
    drive:= FCB[0];
    newDirectory( Dir, fName );
    writeDirectory( Dir, 5 );
    writeln;
    writeln( 'Auf der Diskette sind noch ', DiskFreeSpace( Drive ), 'K frei' );
  end;
end.

