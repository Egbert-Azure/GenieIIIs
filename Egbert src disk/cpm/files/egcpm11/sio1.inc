{ *
  *
  *  SIO1.INC
  *
  *  hardwareabhaengige Routinen
  *
  *  Routinen fuer die serielle Uebertragung
  *  unter CP/M 3.1 auf dem Schneider CPC 6128
  *
  *  (c) 10.2.1991  FuzzySoft
  *
  *
}

var
  Sio:          text;
  oldUsrOutPtr,
  oldUsrInPtr:  integer;


procedure SioAlarm;
begin
  write( #7 );
end;


function SioInReady: Boolean;
begin
  SioInReady:= ( Bdos( 7 ) = 255 );
end;


function SioIn: char;
begin
  repeat
  until SioInReady;
  SioIn := chr( Bdos( 3 ));
end;


function SioOutReady: Boolean;
begin
  SioOutReady:= ( Bdos( 8 ) = 255 );
end;


procedure SioOut( c: Char );
begin
  repeat
  until SioOutReady;
  Bdos( 4, ord( c ));
end;


procedure SioInit;
var
  c: char;
begin
  oldUsrOutPtr:= UsrOutPtr;
  oldUsrInPtr := UsrInPtr;
  assign( Sio, 'USR:' );
  reset( Sio );
  UsrOutPtr:= addr( SioOut );
  UsrInPtr := addr( SioIn );
  while SioInReady do
    c:= SioIn;
end;


procedure SioExit;
begin
  UsrOutPtr:= oldUsrOutPtr;
  UsrInPtr := oldUsrInPtr;
end;

