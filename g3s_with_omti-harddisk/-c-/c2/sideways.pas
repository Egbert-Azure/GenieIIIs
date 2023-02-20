PROGRAM Sideways;

CONST
 VersionsNo='2.0.0';
 SegCGRom=$F000; OfsCGRom=$FA6E; { ROM Character Generator }
 FontSize=8; Spacing=2; { Zeichenhhe: 8 Pixel & Zwischenraum: 2 Pixel }
 MaxPixInLine:Integer=960;           { doppelte Dichte: 960 Pixel overlapping }
 InitLineHeight:String[3]=#27'3'#24; { setze Zeilenabstand auf 8/72 Zoll }
 HiResGraphik  :String[2]=#27'L';    { 8 Punkt Bitmuster mit doppelter Dichte }

TYPE
  String80=String[80]; String255=String[255];
  T_TextArray=Array[1..60] OF String255;

VAR
  GraphFont:Array[0..255] OF Array[0..7] OF Byte;
  MaxLinesSW,             { max. Zeichen/Zeile = max. Zeilen/Seite "sideways" }
  MaxLineLengthSW,
  LaufendeZeile,
  PixInLine : Integer;
  TextArray:T_TextArray;
  TextLine: String255;
  Datei:Text;
  DatName:String80;
  { Vor dem Start von SIDEWAYS mua GRAFTABL geladen werden!!!!
    Dies initialisiert die folgenden Pointer: }
  SegCGRam:Integer ABSOLUTE $0:$7E; OfsCGRam:Integer ABSOLUTE $0:$7C;



PROCEDURE Message;
BEGIN {Message}
  NormVideo; Writeln(^M^J'Sideways  (Version ',VersionsNo,')');
  LowVideo; Writeln('(c)  Hans Wiederhold 1986'^M^J);
  Writeln('Aufruf: "SIDEWAYS DATEINAME.TYP"'^M^J)
END; {Message}


FUNCTION Exist(Name:String80) : Boolean;
VAR Fil:File;
BEGIN {Exist}
  Assign(Fil,Name); {$I-} Reset(Fil); {$I+}
  IF (IOresult=0) THEN BEGIN
    Close(Fil); Exist:=True END
  ELSE Exist:= False
END; {Exist}


PROCEDURE PrintPage(VAR TxtArr:T_TextArray;MaxLines,MaxLineLength:Byte);
VAR Col,Line,Byt,i,j:Byte;
BEGIN {PrintPage}
  FOR Line:= 1 TO MaxLines DO  { Zeilen auf gleiche Lnge bringen }
    WHILE Length(TxtArr[Line])<MaxLineLength DO TxtArr[Line]:=TxtArr[Line]+' ';
  FOR Col:= 1 TO MaxLineLength DO BEGIN
    Write(LST,HiResGraphik+Chr(PixInLine MOD 256)+Chr(PixInLine DIV 256));
    FOR Line:= MaxLines DOWNTO 1 DO BEGIN
      Byt:= Ord(TxtArr[Line][Col]);
      FOR i:= 1 TO (Spacing*2) DO Write(LST,Chr(0));
      FOR i:= Pred(FontSize) DOWNTO 0 DO
        FOR j:= 1 TO 2 DO Write(LST,Chr(GraphFont[Byt,i])); END; {FOR Line}
    Writeln(LST); END; {FOR Col}
  Write(LST,^L);
  FillChar(TxtArr,SizeOf(TxtArr),#0)
END; {PrintPage}


BEGIN { Sideways - Hauptprogramm }
  IF ParamCount>0 THEN DatName:=ParamStr(1) ELSE BEGIN Message; Halt END;
  IF NOT Exist(DatName) THEN BEGIN
    Message; Writeln('Datei "'+DatName+'" nicht gefunden!'); Halt END;
  Move(Mem[SegCGRom:OfsCGRom],Mem[Seg(GraphFont):Ofs(GraphFont)],$400);
  Move(Mem[SegCGRam:OfsCGRam],Mem[Seg(GraphFont):(Ofs(GraphFont)+$400)],$400);
  Assign(Datei,DatName); Reset(Datei);
  MaxLinesSW:= (MaxPixInLine DIV 2) DIV (FontSize+Spacing);
  PixInLine:= 2*MaxLinesSW*(FontSize+Spacing);
  MaxLineLengthSW:=0; LaufendeZeile:=1;
  FillChar(TextArray,SizeOf(TextArray),#0);
  Write(LST,InitLineHeight);
  WHILE NOT EOF(Datei) DO BEGIN
    Readln(Datei,TextLine);
    TextArray[LaufendeZeile]:=TextLine;
    IF Length(TextLine)>MaxLineLengthSW THEN MaxLineLengthSW:=Length(TextLine);
    IF (LaufendeZeile=MaxLinesSW) OR (EOF(Datei)) THEN BEGIN
      PrintPage(TextArray,MaxLinesSW,MaxLineLengthSW);
      MaxLineLengthSW:=0 END;
    IF LaufendeZeile=MaxLinesSW THEN LaufendeZeile:=1
    ELSE LaufendeZeile:=Succ(LaufendeZeile) END; {WHILE NOT EOF}
  Close(Datei)
END. { Sideways }
