program Universal_Directory;

(* 250292 Tilmann Reh, In der Grossenbach 46, D-57072 Siegen *)

(* For english messages and help screen, compile with D-ENG.IN1  *)
(* instead of D.IN1. For making the 127 character command line   *)
(* available, load CLINE.HEX over the compiled .COM file.        *)
(* This is a german program. All other comments are german.      *)

{$I c5:CLINE127.INC     127 Zeichen Kommandozeile        }
{$I c5:D.IN0            Typdefinitionen, Datenstrukturen }

const VersionMsg     = 'Universal Directory V0.5  TR 931012';

(* Optionen-Defaults: *)

const Display        : DisplayType   = TwoColumn;
      SortOrder      : SortOrderType = ByName;
      ShowAttributes : boolean       = false;
      UpperCase      : boolean       = false;
      ShowFiles      : boolean       = true;
      Horizontal     : boolean       = false;
      Justify        : boolean       = false;
      ShowHeader     : boolean       = true;
      ShowSummary    : boolean       = true;
      ShowSysFiles   : boolean       = false;
      Exclude        : boolean       = false;

(* Globale Variablen *)

const MaxMaskCount   = 10;
      MaskCount      : byte          = 0;
      TooManyMasks   : boolean       = false;
      UsedDrives     : set of 0..15  = [];
      UsedUsers      : array[0..15] of set of 0..15
                     = ([],[],[],[],[],[],[],[],[],[],[],[],[],[],[],[]);
      DisplayLine    : byte          = 0;

var   Mask                        : array[0..MaxMaskCount] of MaskType;
      DirDat                      : ^DirDatArray;
      MaxFileCount,ActFileCount   : integer;
      LoggedDrive,LoggedUser,
      ConsoleWidth,PageLength     : byte;
      PageMode                    : boolean;
      i,j                         : byte;
      r                           : real;
      s                           : string20;

{$i c5:D.IN1  Meldungen, Online-Help             }
{$i c5:D.IN2  Diverse elementare Grundfunktionen }
{$i c5:D.IN3  Optionen und Maskenbearbeitung     }
{$i c5:D.IN4  Dateien suchen und erfassen        }
{$i c5:D.IN5  Dateiliste sortieren               }
{$i c5:D.IN6  Directory nach Optionen anzeigen   }

(*********************************************************************
**                             MAIN                                 **
*********************************************************************)

begin
  if BDOSHL(12)<$31 then begin
    writeln(WrongVersion);
    halt;
    end;
  ConOutPtr:=Addr(OwnConOut);
  LoggedDrive:=BDOS(25);
  LoggedUser:=BDOS(32,$FF);
  GetPageMode;

  for i:=1 to ParamCount do begin
    s:=ParamStr(i);
    if (s[1]='/') or (s[1]='$') or (s[1]='[')
      then GetOptions(s) else AppendMaskList(s);
    end;
  if MaskCount=0 then AppendMaskList('*.*');
  if TooManyMasks then writeln(TooManyMaskMsg);
  if Exclude then begin
    UsedDrives:=[Mask[0].Drive];
    UsedUsers[Mask[0].Drive]:=[Mask[0].User];
    end;

  MaxFileCount:=MaxAvail-1000; GetMem(DirDat,MaxFileCount);
  if MaxFileCount>0 then r:=MaxFileCount else r:=65536.0+MaxFileCount;
  MaxFileCount:=trunc(r/sizeof(DirDatRecord));

  for i:=0 to 15 do if i in UsedDrives then
  for j:=0 to 15 do if j in UsedUsers[i] then begin
    ScanDriveUser(i,j);
    GetFileSizes(i);
    SortNotComplete:=false;
    if SortOrder<>NoSort then QuickSortFileList(0,pred(ActFileCount));
    if SortNotComplete then begin writeln(SorryNoSort); IncDisplayLine; end;
    DisplayDirectory(i,j);
    end;

  BDOS(32,LoggedUser);
  BDOS(14,LoggedDrive);
  end.
