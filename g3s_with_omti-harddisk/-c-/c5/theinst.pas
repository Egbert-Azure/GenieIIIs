(*********************************************)
(*                                           *)
(*                TheInst                    *)
(*                                           *)
(*    universelles Installationsprogramm     *)
(*                                           *)
(*********************************************)

const
	Version= '1.01';
	Stand=	'13.05.89';

{
	(C)opyright 1989
	  Olaf Krumnow
	 Wiesnerring 19c
	D-2050 Hamburg 80
	 (040) 724 95 66


TheInst ist ein universelles Installationsprogramm, mit dem praktisch alle
Programme leicht installiert werden koennen.

Es basiert auf einer Steuerdatei, die alle zur Installation noetigen Informationen
enthaelt.


	  Die moeglichen Datentypen, die installiert
	  werden koennen:
	  
	  wort		(W) = 16bit im Intel-Format (Low, High)
	  wortX		(X) = 16bit im inv. Intel-Format (High, Low)
	  zeichen	(Z) = ASCII-Zeichen
	  einbyte	(E) = 8bit, als ein Wert eingegeben
	  flagbyte	(F) = 8bit, max. 2 Werte (ja/nein)
	  bitbyte	(B) = 8bit, die bitweise veraendert werden
	  janein	(J) = ein bestimmtes Bit in einem Byte
	  satz		(S) = Text bis zu einer angegebenen Laenge			}

{
23.04.89	kleine Optimierungen
16.04.89	String nach TURBO-Pascal-Art hinzugefuegt (SL0/n2)
}

type
	anystr		= string[255];
	filename	= string[14];
	BDOSstring	= record
		len	: byte;
		txt	: anystr
	end;
	
var
	SektorPuffer	: array[0..127] of byte;
	LadeSektor,
	SchreibSektor,
	SektorNummer	: integer;
	SektorOffset,
	LadeOffset,
	SchreibOffset	: byte;
	
	InstFileName,
	WorkFileName,
	NextFileName	: filename;
	InstFile		: text;
	WorkFile		: file;
	
	line			: anystr;
	
	geaendert		: boolean;

const
	DefType			= '.INS';	{ Default-Dateityp fuer Steuerdatei }


function HexByte (n:byte): anystr;	{ gib Byte als 2-stellig Hex aus }
const Hexnum: array[0..$f] of char = '0123456789ABCDEF';
begin
	HexByte := Hexnum[n shr 4]+Hexnum[n and $f]
end;

function HexWord (n:integer): anystr; { gib Wort als 4-stellig Hex aus }
begin
	HexWord := HexByte(hi(n)) + HexByte(lo(n))
end;

procedure Fehler(n:byte);
begin
	write(^g'FEHLER');
	case n of
		2: writeln(' im Zahlenformat.');
		3: writeln(' im Kommando.');
		4: writeln(': Steuerdatei nicht gefunden.');
		5: writeln(': Zu installierende Datei nicht gefunden.');
		6: writeln(': Unerwartetes Ende der Steuerdatei.');
		7: writeln(': Unerwartetes Ende der zu installierenden Datei.')
	end;
	writeln('Programm abgebrochen...');
	halt
end;

procedure WriteSektor;
begin
	if geaendert then begin
{$i-}
		seek(WorkFile,SektorNummer);
{$i+}
		if IOresult<>0 then
			Fehler(7);
		blockwrite(WorkFile,SektorPuffer,1);
	end;
	geaendert := false
end;

procedure LiesSektor(n:integer);
begin
	if n<>SektorNummer then begin
		if geaendert then
			WriteSektor;
{$i-}
		seek(WorkFile,n);
{$i+}
		if IOresult<>0 then
			Fehler(7);
		blockread(WorkFile,SektorPuffer,1);
		SektorNummer := n;
	end
end;

function HoleByte: byte;
begin
	if LadeOffset=$80 then begin
		LadeSektor := succ(LadeSektor);
		LadeOffset := 0;
	end;
	LiesSektor(LadeSektor);
	HoleByte := SektorPuffer[LadeOffset];
	LadeOffset := succ(LadeOffset);
end;

procedure WriteByte(by: byte);
begin
	if SchreibOffset=$80 then begin
		SchreibSektor := succ(SchreibSektor);
		SchreibOffset := 0;
	end;
	LiesSektor(SchreibSektor);
	if by<>SektorPuffer[SchreibOffset] then begin
		SektorPuffer[SchreibOffset] := by;
		geaendert := true;
	end;
	SchreibOffset := succ(SchreibOffset);
end;

procedure InstLine;		{ werte eine Zeile aus und bearbeite sie }
var Wort,
	Antwort: anystr;
	pos:  byte;
	Zahl: real;
	Error: integer;
	mode: char;
	i,j: byte;
	
	word,
	word2	: integer;
	ch		: char;
	bit,
	by		: byte;
	jawert,
	neinwert: integer;
	str		: BDOSstring;
	len0,
	bit7,
	nend	: boolean;
	
	procedure EinWort;	{ extrahiere das naechste Wort aus der Eingabezeile }
	begin
		Wort := '';
		while line[pos] in [#9,' '] do
			pos := succ(pos);
		while not (line[pos] in [#0,#9,' ']) do begin
			Wort := Wort+line[pos];
			pos := succ(pos)
		end;
	end; { EinWort }
	
	function Prompt(s:anystr): anystr;
	var t:anystr;
	begin
		t := '';
		write('(RET = ',s,') --> ');
		readln(t);
		if t='' then
			t := s;
		Prompt := t
	end;
	
	function KurzPrompt(c:anystr): char;
	var ch: char;
	begin
		write('(RET = ');
		if c<' ' then
			write('^',chr(ord(c)+$40),') --> ')
		else
			write(c,') --> ');
		read(kbd,ch);
		if ch<' ' then
			write('^',chr(ord(ch)+$40))
		else
			write(ch);
		writeln;
		KurzPrompt := ch
	end;
	
	procedure ReHex(s:anystr;var zahl:real;var e:integer);
	var wert:integer;
		i:byte;
	begin
		zahl := 0.0; e := 0;
		for i:=1 to length(s) do begin
			if not (s[i] in ['0'..'9','A'..'F']) then begin
				e := i;
				exit
			end;
			wert := ord(s[i])-ord('0');
			if wert>9 then
				wert := wert-7;
			zahl := 16*zahl+wert
		end;
	end;
	
begin { ScanLine }
	line := line+#0;	{ setze Endemarkierung, spart Abfragen }
	pos := 1;
	EinWort; { lies ein Wort }
	ReHex(Wort,Zahl,Error);				{ wandele in REAL um }
	if (Error<>0) or (Zahl<256.0) then
		Fehler(2);
	Zahl := Zahl-256.0;					{ 100H ist das erste Byte im ersten Sektor }
	LadeSektor := trunc(Zahl/128.0);	{ bestimme die Sektornummer }
	SchreibSektor := LadeSektor;
	LadeOffset := trunc(Zahl-LadeSektor*128); { Offset im Sektor }
	SchreibOffset := LadeOffset;
	LiesSektor(LadeSektor);
	
	EinWort;		{ lies das naechste Wort }
	if Wort='' then 
		Fehler(3);
	mode := Wort[1]; delete(Wort,1,1);
	
	case upcase(mode) of			{ verzweige entsprechend Modus }
		'W': { Format W:  ein Wort im Intel-Format }
			begin
				word := HoleByte+256*HoleByte;	{ Original holen }
				repeat
					Antwort := Prompt('$'+HexWord(word)); { Prompt und Zeile einlesen }
					val(Antwort,word2,Error)
				until Error=0;
				WriteByte(lo(word2)); WriteByte(hi(word2));
			end;
		'X': { Format X:  ein Wort im inversen Intel-Format }
			begin
				word := 256*HoleByte+HoleByte; { Original holen }
				repeat
					Antwort := Prompt('$'+HexWord(word)); { Prompt und Zeile einlesen }
					val(Antwort,word,Error)
				until Error=0;
				WriteByte(hi(word)); WriteByte(lo(word));
			end;
		'Z': { Format Z: ein ASCII-Zeichen }
			begin
				by := HoleByte;		{ Original holen }
				ch := KurzPrompt(char(by));
				if mode='Z' then	{ Bei einem Z wird Upcase durchgefuehrt, bei z nicht }
					ch := upcase(ch);
				if ch<>^m then
					WriteByte(byte(ch));
				writeln
			end;
		'E': { Format E: ein Byte-Wert 0..255 }
			begin
				by := HoleByte;
				repeat
					Antwort := Prompt('$'+HexByte(by));
					if Antwort='' then
						Error := 0
					else begin
						val(Antwort,word,Error);
						if abs(word)>255 then
							Error := 1
					end
				until Error=0;
				WriteByte(lo(word))
			end;
		'F': { Format F: ein Byte als Flag (zwei moegliche Werte) als Ja/Nein
				Fn1/n2, wobei n1 Wert fuer JA und n2 Wert fuer NEIN ist }
			begin
				if (length(Wort)=5) and (Wort[3]='/') then begin
					val('$'+copy(Wort,1,2),jawert,Error);	{ werte Kommando aus }
					if Error<>0 then
						Fehler(3);
					val('$'+copy(Wort,4,2),neinwert,Error);
					if Error<>0 then
						Fehler(3);
					by := HoleByte;
					repeat
						if by=jawert then
							ch := KurzPrompt('JA')
						else
						if by=neinwert then
							ch := KurzPrompt('NEIN')
						else
							ch := KurzPrompt('undefiniert');
						ch := upcase(ch);
						if ch in ['J','Y'] then by := jawert
						else if ch='N' then by := neinwert
					until ch in ['J','N','Y',^m];
					WriteByte(by)
				end else
					Fehler(3)
			end;
		'B': { Format B: BitByte; ein Byte, das Bitweise veraendert werden kann }
			begin
				by := HoleByte;
				repeat
					writeln('Bit#   7 6 5 4 3 2 1 0');
					write  ('       ');
					j := $80;
					for i := 0 to 7 do begin
						if (by and j) = j then
							write('1 ')
						else
							write('0 ');
						j := j shr 1
					end;
					writeln('  ==> ',Hexbyte(by));
					write('Bit# ? ');
					read(kbd,ch);
					if ch in ['0'..'7'] then begin
						by := by xor (1 shl (ord(ch)-ord('0')));
					end;
					if ch<>^m then
						write(^m^k^k);  { bringe Cursor wieder auf oberste Position}
				until ch=^m;
				WriteByte(by)
			end;
		'J': { Format J: Ja/Nein in einem Bit kodiert
				Jm/n	Bit m mit n als JA				}
			begin
				if (length(Wort)=3) and (Wort[2]='/') and (Wort[1] in ['0'..'7'])
				and (Wort[3] in ['0','1']) then begin
					bit := ord(Wort[1])-ord('0');
					jawert := ord(Wort[3])-ord('0');
					neinwert := 1-jawert;
					by := HoleByte;
					repeat
						if (by shr bit) and 1=jawert then
							ch := KurzPrompt('JA')
						else
							ch := KurzPrompt('NEIN');
						ch := upcase(ch)
					until ch in ['J','Y','N',^m];
					if ch in ['J','Y'] then
						by := by or (1 shl bit)
					else if ch='N' then
						by := by and ((1 shl bit) xor $ff);
					WriteByte(by)
				end else
					Fehler(3)
			end;
		'S': { Format S: String mit beliebigem Endezeichen und Maximallaenge
				Sn1/n2 mit n1 als Endezeichen, z.B. 00 oder 24 (='$').
				Ist n1 = B7 dann ist das Ende durch ein gesetztes 7.Bit gegeben.
				n2 ist die Maximallaenge inclusive Endezeichen }
			begin
				if (length(Wort)=5) and (Wort[3]='/') then begin
					bit7 := copy(Wort,1,2)='B7';
					len0 := copy(Wort,1,2)='L0';
					nend := copy(Wort,1,2)='NE';
					if not (bit7 or len0 or nend) then begin
						val('$'+copy(Wort,1,2),jawert,Error);
						if Error<>0 then
							Fehler(3);
					end;
					val('$'+copy(Wort,4,2),neinwert,Error);
					if Error<>0 then
						Fehler(3);
					str.len := lo(neinwert);
					str.txt := '';
					if len0 then begin
						str.txt[0] := chr(HoleByte);
						for by := 1 to ord(str.txt[0]) do
							str.txt[by] := chr(HoleByte)
					end else
						if nend then
							for by := 1 to str.len do
								str.txt := str.txt + chr(HoleByte)
						else
							repeat
								by := HoleByte;
								if bit7 or (by<>jawert) then
									str.txt := str.txt + chr(by and $7f);
							until (bit7 and (by>=$80)) or (by=jawert);
					if not (bit7 or nend) then
						str.len := str.len-1;
{					BDOS(10,addr(str));						}
					inline($11/str/$cd/>$f95a);
					if len0 then
						WriteByte(ord(str.txt[0]));
					for i := 1 to length(str.txt)-1 do
						WriteByte(ord(str.txt[i]));
					if bit7 then
						WriteByte(ord(str.txt[length(str.txt)]) or $80)
					else begin
						WriteByte(ord(str.txt[length(str.txt)]));
						if not (len0 or nend) then
							WriteByte(jawert)
					end
				end else
					Fehler(3);
			end;
	else
		Fehler(3)
	end;
end;

begin	{ main }
	
	ClrScr;
	
	writeln(^t'TheInst ',Version,^p' vom ',Stand,'  (C)1989  Olaf Krumnow');
	writeln;
	if ParamCount<>1 then begin	{ teste Kommandozeile }
		writeln;
		write('Aufruf: TheInst instfile');
		halt
	end;
	
	InstFileName := ParamStr(1);	{ lies Dateiname }
	
	repeat
	
		assign(InstFile,InstFileName+DefType);
{$i-}
		reset(InstFile);				{ eroeffne Datei }
{$i+}
		if IOresult<>0 then
			Fehler(4);
		
		SektorNummer := -1;				{ Vars fuer Dateizugriff vorbelegen }
		geaendert := false;
{$i-}
		readln(InstFile,WorkFileName);	{ Namen der zu inst. Datei lesen }
{$i+}
		if (IOresult<>0) or eof(InstFile) then
			Fehler(6);
		
		if WorkFileName = '?' then begin { interaktiv einlesen }
			write('Gib den Namen der zu installierenden Datei ein --> ');
			BufLen := 14;				{ Puffer begrenzen }
			readln(WorkFileName)		{ Dateinamen einlesen }
		end;
		
		assign(WorkFile,WorkFileName);
{$i-}
		reset(WorkFile);				{ oeffne Arbeitsdatei }
{$i+}
		if IOresult<>0 then
			Fehler(5);

{$i-}
		readln(InstFile,NextFileName);	{ Anschluss-Installation ? }
{$i+}
		if (IOresult<>0) or (eof(InstFile)) then
			Fehler(6);
		
		while not eof(InstFile) do begin
			
			line := ' ';
{$i-}
			readln(InstFile,line);		{ Zeile lesen }
{$i+}
			if IOresult<>0 then
				Fehler(6);
			
			if line[1] in ['0'..'9'] then	{ Installationsanweisung? }
				InstLine					{ ja, dann auswerten }
			else
				writeln(line)				{ nein, dann einfach ausgeben }
			
		end;	{ while }
		
		WriteSektor;
		
		close(InstFile);
		close(WorkFile);
		
		InstFileName := NextFileName		{ evtl. Folgedatei bearbeiten }
		
	until InstFileName = '';
	
end.
