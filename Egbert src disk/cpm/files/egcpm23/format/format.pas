program Disketten_Format_Manager;          (* 280491 Tilmann Reh *)

{$I FORMAT.IN0 } (* Datentyp-Deklarationen und Konstanten        *)

(* Globale und Hauptprogramm-Variablen *)

const sign1             = 'FORMAT-MANAGER V1.01';
      sign2             = 'Tilmann Reh 30.05.91';
      Ende              : boolean = false;
      ResetVector       : integer = 0;

var   Datei             : file;
      AktForm           : format;
      AktDriveChar      : char;
      AktDrive,
      AktSize,AktType   : byte;
      AktNum,AktSecShift: integer;
      OK                : boolean;
      ch                : char;
      IndexField        : array[1..32] of chrntyp;
      SecBuf            : array[0..1023] of byte;
      Result            : ^chrntyp;

{$I FORMAT.IN1 } (* Hardware-Interface, Allgemeine Routinen      *)
{$I FORMAT.IN2 } (* Liste auf Schirm und Drucker, Daten anzeigen *)
{$I FORMAT.IN3 } (* Editieren von Formaten                       *)
{$I FORMAT.IN4 } (* Disketten untersuchen                        *)
{$I FORMAT.IN5 } (* Parameterblock schreiben und setzen          *)
{$I FORMAT.IN6 } (* Diskette formatieren                         *)

(*-------------- HAUPTMENUE ----------------*)

procedure hauptmenue;
begin
  clrscr;
  gotoxy(29,5); write(sign1);
  gotoxy(29,6); write('====================');
  gotoxy(29,7); write(sign2);
  gotoxy(10,9);  write('(1) Liste der m|gl. Formate      (6) Diskette formatieren');
  gotoxy(10,10); write('(2) Gesamtliste drucken          (7) Parameter schreiben');
  gotoxy(10,11); write('(3) Formatdaten anzeigen         (8) Fremdformat bearbeiten');
  gotoxy(10,12); write('(4) Format editieren');
  gotoxy(10,13); write('(5) Diskette untersuchen         (0) Ende');
  gotoxy(28,15); write('Gew}nschte Funktion ? ');
  repeat read(kbd,ch); until ch in ['0'..'9'];
  write(ch);
  case ch of '1' : List;
             '2' : PrintOut;
             '3' : Show;
             '4' : Editieren;
             '5' : GetForm;
             '6' : FormatDisk;
             '7' : WriteParam;
             '8' : SetFormat;
         '9','0' : Ende:=true;
             end;
  RestoreBiosDrive; (* fuer Dateioperationen! *)
  end;

function OpenSuccessful(path:string2):boolean;
begin
  assign(datei,path+'FORMAT.DAT');
  {$I-} reset(datei); {$I+}
  OpenSuccessful:=ioresult=0;
  end;

(*----------------- MAIN -------------------*)

begin
  writeln(^M^J,sign1,'  ',sign2);
  if GetBiosVersion<>$0101 then begin
    writeln('falsche BIOS-Version !');
    halt; end;
  if not OpenSuccessful('') then if not OpenSuccessful('A:') then
    if InputBoolean('Kein Datenfile vorhanden. Neu erzeugen') then begin
      assign(datei,'FORMAT.DAT');
      rewrite(datei);
      end
    else halt;
  SaveBiosDrive; (* falls nicht sofort InputDrive aufgerufen wird... *)
  Result:=Ptr(GetResultAddress);
  if paramcount=2 then SetGetFormatCommandLine;
  repeat hauptmenue until Ende;
  BDOS(37,ResetVector);  (* Reset used drives *)
  end.
