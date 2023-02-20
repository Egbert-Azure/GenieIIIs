(****************************************************************************)
(*            TDB-2  Vers. 1.0 (c) U.K. 21.03.1987                          *)
(****************************************************************************)
(* Folgende Aenderungen gegnueber der urspruenglichen Version:              *)
(* StrLaenge von 255 auf 80                                                 *)
(* EintrGroesse von 2000 auf 1000                                           *)
(****************************************************************************)

program TurboDatenBank_2;

function  Abbruch : boolean; forward;

procedure LiesZeichen(var c : char); forward;

(* Eingebundene Bibliotheks-Module                                      *)
(*$I c5:WINDOW.PAR           Parameter fuer WINDOWS                     *)
(*$I c5:WINDOW7.BIB          Fenstertechnik aus Turbo-Special 3, 7 Bit  *)
(* Bei Bedarf Universalversion WINDOW.BIB (7 + 8 Bit ASCII) einbinden   *)
(*$I c5:MASKE-1.BIB          Mofifizierte Maskeneingabe Turbo-Special 5 *)
(*$I c5:CHOOSE.BIB           Horizontale Menue-Auswahl Turbo-Special 5  *)
(*$I c5:SELECT-1.BIB         Vertikale Menue-Auswahl Turbo-Special 5    *)

const MaxPuffer = 2048;    (* Puffer fuer File-Operationen     *)
      MemConst = 1.0;      (* 16.0 bei 8086/88, 1.0 bei Z80    *)
      MaxIndex = 10;       (* Fuer IndexAuswahl                *)
      StrLaenge = 80;      (* maximale Stringlaenge            *)
      EintrGroesse = 1000; (* maximale Byte-Anzahl pro Eintrag *)

type  anystring = string[StrLaenge];
      Index  = record
                 DateiVerweis : integer;
                 Schluessel : anystring
               end;
      IndexVektor = array[1..MaxIndex] of record Feld, Anzahl : byte end;
      Eintrag = array[1..EintrGroesse] of byte;
      MaskType  = array[1..127] of MaskEintr; (* Kommt von Maske-1.BIB *)
      MaskPtr   = ^MaskType;
      FileNameStr = string[14];
      DatFile = file;
      IndexFile = file;
      AusgabeFormat = record
                        Format : string[60];
                        KopfZeile : string[60];
                        FussZeile : string[60];
                        Zeilen    : integer;
                        ab        : boolean;
                        FormFeed  : boolean;
                        ZielName  : string[14];
                        SelString : string[60]
                      end;

var FilePuffer : array[1..MaxPuffer] of byte;
    PufferZeiger : integer;
    Dat : Eintrag;
    Ind : Index;
    IndexAuswahl : IndexVektor;
    DateiOffen, indiziert, Umleitung, warte : boolean;
    FileGroesse, SatzGroesse, RecZahl, DateiZeiger, Result : integer;
    IndFile : IndexFile;
    DatenFile : DatFile;
    Maske : MaskPtr;
    MenueFenster, ArbeitsFenster, MenueWahl, Erster, i : integer;
    DBName, IndexName : FileNameStr;
    TextFile : Text;
    TempMask : MaskEintr;

const FormMask : array[1..8] of MaskEintr =
                  ((x:1; y:1; m:'Format : '; l:60;sl:60;Art:1),
                   (x:1; y:2; m:'Kopfzeile : ';l:60;sl:60;Art:1),
                   (x:1; y:3; m:'Fusszeile : ';l:60;sl:60;Art:1),
                   (x:1; y:4; m:'Zeilen/Seite :';l:1; sl:3; Art:2),
                   (x:40; y:4; m:'absteigend : ';l:0; sl:4; Art:5),
                   (x:60; y:4; m:'FF : ';l:0;sl:4;Art:5),
                   (x:1; y :5; m:'Ausgabedatei : '; l:14; sl:14; Art:1),
                   (x:1; y:6; m:'Selektion : ';l:60;sl:60;Art:1));

      AusForm : AusgabeFormat =
                  (Format:'1/'; KopfZeile:'Seite #'; FussZeile:''; Zeilen:24;
                   ab:false; FormFeed:false; ZielName:'CON:';SelString:'');

      HauptMenue : array[1..6] of string[13] =
         ('Ausgabe','Bearbeitung','Indizierung','Sonstiges','Dateien','Ende');

(* Low-Level-Routinen zum Index- und Filezugriff                     *)
(*$i c5:tdb-00.inc						     *)	
(*$I c5:TDB-01.INC           Datei-Zugriffe                          *)
{ =================================================================== }
(* Utilities                                                         *)

(*$I c5:TDB-02.INC           Allgemeine Routinen fuer die Datenbank  *)

(* Die TDB-2-Routinen                                                *)

(*$I c5:TDB-06.INC*)         {  Untermenue Datei-Handling }

(* Die FORWARD-deklarierten Prozeduren                               *)

function Abbruch;  (* Liefert TRUE, wenn ^U gedrueckt wurde *)
  var c : char;
  begin
    if keypressed
      then begin read(kbd,c); Abbruch:=c=^U end
      else Abbruch:=false
  end; (* Abbruch *)

(*-------------------------------------------------*)
procedure Cursor_On;
begin
  write(#27,#13);
end;
(*-------------------------------------------------*)
procedure Cursor_Off;
begin
  write(#27,#12);
end;
(*-------------------------------------------------*)

procedure LiesZeichen; (* zur Tastaturanpassung (Turbo-Special 3)           *)
  begin
    (*$U-*)
    if Umleitung then
      if Abbruch then begin Umleitung:=false; c:=^[ end else
      if EOF(TextFile) then c:=^[ else read(TextFile,c)
    else (* Hier Ihre Tastaturanpassung einfuegen *)
      begin
        read(kbd,c);
        if c=#27 then if keypressed then
          begin
            read(kbd,c);
          end
      end (* ELSE *)
  end; (* LiesZeichen *)

(****************************************************************************)
(*                         Das Hauptprogramm                                *)
(****************************************************************************)
begin
 Cursor_Off;
 cbreak:=false; (* Bei Turbo 2.x stattdesen die Compileranweisung C- setzen *)
 indiziert:=false; DateiOffen:=false; Umleitung:=false; DBName:='';
 FileGroesse:=0; MenueWahl:=4; warte:=false; DateiZeiger:=0;
 InitWindows; OpenWindow(1,1,80,4); MenueFenster:=ScreenPtr;
 gotoxy(30,0); write('[ TDB-2 Version 1.0 ]');
 OpenWindow(1,5,80,23); ArbeitsFenster:=ScreenPtr;
 repeat
   ChangeWindow(MenueFenster);
   if not DateiOffen then Erster:=4 else
   if FileGroesse=0  then Erster:=2 else Erster:=1;
   MenueWahl:=succ(MenueWahl-Erster);
   gotoxy(1,1); for i:=1 to pred(Erster) do write('':13);
   choose(1+pred(Erster)*13,1,13,SizeOf(HauptMenue[1]),
          HauptMenue[Erster],7-Erster, MenueWahl,warte);
   MenueWahl:=pred(Erster+MenueWahl); warte:=false;
   case MenueWahl of
     1 : (*Ausgabe*);
     2 : (*Bearbeiten*);
     3 : (*Indizierung*);
     4 : DateiVerwaltung;
     5 : (*Sonstiges*);
     6 : if DateiOffen then begin MenueWahl:=4; warte:=true end
   end
 until MenueWahl=6;
 ExitWindows; ClrScr;
Cursor_On
end. (* TurboDatenbank_2 *)