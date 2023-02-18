program test;

{

  *
  *
  *  CPM31.INC
  *
  *  Zugriff auf CP/M 3.1 unter Turbo Pascal 3.0
  *
  *  (c) 23.2.1991  -  FuzzySoft Schneider CPC 6128
  *
  *
  *  Die Funktionen und Prozeduren sind entsprechend ihrer Beschreibung
  *  im Buch Digital Research: 'CP/M 3 Programmer's Guide' implementiert
  *
  *
  *    Laufwerkscode             0=Bezugslaufwerk, 1=A, 2=B, ...
  *
  *  f cpmVersion: integer       Versionsnummer zurueckgeben
  *  p ResetDiskSystem           Diskettensystem zuruecksetzen
  *                                  ( Bezugslaufwerk wird A )
  *  f CurrentDisk: integer      Bezugslaufwerk zurueckgeben
  *  p SelectDisk( d )           Bezugslaufwerk d definieren
  *  p ResetDrive( d )           Laufwerk d zuruecksetzen
  *
  *  p ParseFilename( FCB,Input )erzeugt FCB
  *  f firstEntry( FCB, DMA )    ersten zum FCB passenden DirEintrag suchen
  *               : integer          ( Ergebnis in DMA, wenn Function <> 255 )
  *  f nextEntry: integer        naechsten passenden DirEintrag suchen
  *
  *  p newDirectory( dir, m )    erzeugt ein sortiertes Directory zur Maske m
  *  p disposeDirectory( dir )   loescht Directory
  *  p writeDirectory( dir,n )   schreibt Directory mit n Spalten
  *  f nextFileName( dir )       holt naechsten Dateinamen aus Directory
  *               : cpmStr12
  *
  *  f DiskFreeSpace( d )        freier Diskettenplatz
  *              : integer
  *  f LoginVector : Loginstr    gibt alle angeschlossenen Laufwerke
  *                              zurueck



}

type
  loginStr    = string[16];
  cpmStr12    = string[12];
  cpmDir      = ^cpmDirEntry;
  cpmDirEntry = record
                  fName: cpmStr12;
                  next:  cpmDir;
                end;


function cpmVersion: cpmStr12;
var
  i: integer;
begin
  i:= BdosHL( 12 );
  case i of
    $0022: cpmVersion:= 'CP/M 2.2';
    $0031: cpmVersion:= 'CP/M 3.1';
  end;
end;


procedure ResetDiskSystem;
begin
  Bdos( 13 );
end;


function CurrentDisk: integer;
begin
  CurrentDisk:= succ( Bdos( 25 ));
end;


procedure SelectDisk( Drive: integer );
begin
  if ( Drive > 0 ) and ( Drive < 16 ) then begin
    Bdos( 45, 255 );
    Bdos( 14, pred( Drive ));
    Bdos( 45, 1 );
  end;
end;


procedure ResetDrive( Drive: integer );
begin
  if ( Drive >= 0 ) and ( Drive < 16 ) then begin
    if Drive = 0 then
      Drive:= CurrentDisk;
    Bdos( 37, 1 shl pred( Drive ));
  end;
end;


procedure SetDMA( var DMA );
begin
  Bdos( 26, addr( DMA ));
end;


procedure ParseFilename( var FCB, Input );
var
  PFCB : array[0..1] of integer;
begin
  PFCB[0]:= addr( Input ) + 1;
  PFCB[1]:= addr( FCB );
  Bdos( 152, addr( PFCB ));
end;


function firstEntry( var FCB, DMA ): integer;
begin
  SetDMA( DMA );
  firstEntry:= Bdos( 17, addr( FCB ));
end;


function nextEntry: integer;
begin
  nextEntry:= Bdos( 18 );
end;


procedure newDirectory( var Dir: cpmDir; mask: cpmStr12 );
var
  FCB: array[0..39]  of char;
  DMA: array[0..127] of char;
  i, j: integer;
  s: cpmStr12;

  procedure sort( var Dir: cpmDir; s: cpmStr12 );
  var
    p,q: cpmDir;
  begin
    p:= Dir;
    q:= nil;
    while( p <> nil ) do with p^ do
      if( fName < s ) then begin
        q:= p;
        p:= next;
      end
      else
        p:= nil;
    new( p );
    p^.fName:= s;
    if( q = nil ) then begin
      p^.next:= Dir;
      Dir:= p;
    end
    else begin
      p^.next:= q^.next;
      q^.next:= p;
    end;
  end;

begin
  ParseFileName( FCB, mask );
  i:= firstEntry( FCB, DMA );
  Dir:= nil;
  while ( i <> 255 ) do begin
    i:= i shl 5;
    s:= copy( DMA, i+2, 8 ) + '.' + copy( DMA, i+10, 3 );
    sort( Dir, s );
    i:= nextEntry;
  end;
end;


procedure disposeDirectory( var Dir: cpmDir );
var
  d: cpmDir;
begin
  while ( Dir <> nil ) do begin
    d:= Dir;
    Dir:= d^.next;
    dispose( d );
  end;
end;


procedure writeDirectory( Dir: cpmDir; n: integer );
var
  i: integer;
begin
  i:= 0;
  while ( Dir <> nil ) do begin
    if ( i mod n = 0 ) then
      writeln
    else
      write( ' : ' );
    with Dir^ do begin
      write( fName );
      Dir:= next;
    end;
    i:= succ( i );
  end;
  writeln;
end;


function nextFileName( var Dir: cpmDir ): cpmStr12;
var
  d: cpmDir;
begin
  if ( Dir <> nil ) then begin
    d:= Dir;
    with d^ do begin
      while ( pos( ' ', fName ) > 0 ) do
        delete( fName, pos( ' ', fName ), 1 );
      nextFileName:= fName;
      Dir:= next;
    end;
    dispose( d );
  end;
end;


function DiskFreeSpace( Drive: integer ): integer;
var
  n: array[0..2] of byte absolute $80;
  d: integer;
begin
  if ( Drive >= 0 ) and ( Drive < 16 ) then begin
    d:= CurrentDisk;
    ResetDiskSystem;
    if Drive = 0 then
      Drive:= d;
    SelectDisk( Drive );
    Bdos( 46, pred( Drive ));
    DiskFreeSpace:= ( n[2] SHL 13 ) or ( n[1] SHL 5 ) or ( n[0] SHR 3 );
    SelectDisk( d );
  end;
end;

function LoginVector: loginstr;
var  i,j : integer;
var  l   : char;
var  x   : loginstr;
begin
i:= bdoshl(24);
l:= 'A';
x:='';
for j:=1 to 16 do
    begin
    if odd(i) then x:=x+l;
    l:=succ(l);
    i:= i shr 1;
    end;
LoginVector:=x;
end;




begin {Hauptprogramm}
writeln(cpmVersion);
writeln(LoginVector);
end.
