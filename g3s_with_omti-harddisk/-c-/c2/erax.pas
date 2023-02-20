{$U-,C-,R-}

{

  *
  *  ERAX.PAS
  *
  *  komfortables Loeschen von Dateien
  *  nach Eingabe einer Dateimaske werden passende Dateien abgefragt
  *
  *
  *  (c)  26.2.1991  -  FuzzySoft
  *

}


{$I c5:CPM31.INC  -  Zugriff auf Betriebssystem CP/M 3.1   }


type
  Str80= string[80];


var
  f:     file;
  fName: cpmStr12;
  Dir:   cpmDir;


function GetFileName( var fName: cpmStr12 ): boolean;
begin
  writeln;
  write( 'ERAX> ' );
  readln( fName );
  while( pos( ' ', fName ) = 1 ) do
    delete( fName, 1, 1 );
  GetFileName:= ( fName <> '' );
end;


function Erlaubnis: boolean;
var
  c: char;
begin
  read( Kbd, c );
  if( c = ^c ) then begin
    writeln( '^C' );
    halt;
  end;
  Erlaubnis:= ( c in ['J','j','Y','y',#13] );
end;


procedure InitProgram;
begin
  writeln;
  writeln( '--------------------' );
  writeln( 'ERAX 1.0' );
  writeln( 'Loeschen von Dateien' );
  writeln( '--------------------' );
end;


begin
  InitProgram;
  while( GetFileName( fName )) do begin
    newDirectory( Dir, fName );
    while( Dir <> nil ) do begin
      fName:= nextFileName( Dir );
      write( ' - ', fName, '':( 15 - length( fName )));
      if( Erlaubnis ) then begin
        assign( f, fName );
        erase( f );
        Write( '#' );
      end;
      writeln;
    end;
  end;
end.

