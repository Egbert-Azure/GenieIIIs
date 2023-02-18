{

  *
  *
  *  SEND.PAS
  *
  *  Dateiuebertragung von Textdateien
  *
  *  Routinen fuer die serielle Uebertragung
  *  unter CP/M 3.1 auf dem Schneider CPC 6128
  *
  *  (c) 22.2.1991  FuzzySoft
  *
  *

}

{$I CPM31.INC  --  Zugriff auf Betriebssystem CP/M 3.1    }
{$I SIO1.INC   --  Steuerung der seriellen Schnittstelle  }


const
  Trenner = #13#10'------------'#13#10;

type
  Str127 = string[127];

var
  f:      text;
  fName:  cpmStr12;
  Dir:    cpmDir;


function GetFName( var fName: cpmStr12 ): boolean;
begin
  Writeln;
  Write( 'SEND> ' );
  Readln( fName );
  while( pos( ' ', fName ) = 1 ) do
    delete( fName, 1, 1 );
  GetFName:= ( fName <> '' );
end;


function OpenFile( var f: text; fName: cpmStr12 ): boolean;
begin
  assign( f, fName );
  {$I-}
  reset( f );
  {$I+}
  OpenFile:= ( ioResult = 0 );
end;


procedure SendFile( var f: text; fName: cpmStr12 );
var
  line: Str127;
begin
  SioInit;
  writeln( Sio, Trenner );
  writeln( Sio, fName );
  writeln( Sio, Trenner );
  while not eof( f ) do begin
    readln( f, line );
    writeln( Sio, line );
  end;
  writeln( Sio, Trenner );
  SioExit;
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
  writeln( '---------------------------------' );
  writeln( 'SEND 1.0' );
  writeln( 'Dateiuebertragung von Textdateien' );
  writeln( '---------------------------------' );
end;


begin
  InitProgram;
  while( GetFName( fName )) do begin
    newDirectory( Dir, fName );
    while( Dir <> nil ) do begin
      fName:= nextFileName( Dir );
      write( ' - ', fName, '':( 15 - length( fName )));
      if( Erlaubnis ) then begin
        write( '#' );
        if( OpenFile( f, fName )) then begin
          SendFile( f, fName );
          close( f );
        end;
      end;
      writeln;
    end;
  end;
  writeln( aux, '@' );
end.


