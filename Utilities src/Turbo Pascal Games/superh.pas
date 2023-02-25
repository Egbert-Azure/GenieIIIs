(****************************************************************************)
(*                         de Luxe-Version von Superhirn                    *)
(****************************************************************************)
(* Version mit 7 Bit ASCII Window.bib                                       *)
(* Aenderung durch Ersetzen von Window7.bib mit Window.bib                  *)
(****************************************************************************)

program SuperBrain_Professional;

procedure LiesZeichen(var c : char); forward;

(*$I c5:CPM-80.BIB     *)
(*$I c5:WINDOW.PAR     *)
(*$I c5:WINDOW7.BIB    *)
(*$I c5:LAUF.BIB       *)
(*$I c5:CHOOSE.BIB     *)
(*$I c5:SELECT-1.BIB   *)

const MaxFarben = 6;
      MaxSteckerlZahl = 5;
      MaxVersuche = 10;
      SteckerlZahl : 1..MaxSteckerlZahl = 5;
      FarbZahl : 1..MaxFarben = 5;
      SchriftZug = 'CHIP - SPECIAL - SUPERBRAIN - PROFESSIONAL';

type  SteckerlFarbe = (rot,pink,blau,gelb,lila,hell,illegal);
      BewertungsSteckerl = (schwarz, weiss);
      Bewertung = array[BewertungsSteckerl] of 0..MaxSteckerlZahl;
      Kombination = array[1..MaxSteckerlZahl] of SteckerlFarbe;

var   MenueFenster, MenschFenster, ComputerFenster : byte;
      M_Zuege, C_Zuege : integer;

const Farben : array[1..MaxFarben] of string[5] =
               ('rot','pink','blau','gelb','lila','wei~');
      Menue : array[1..6] of string[6] =
             ('1','2','3','4','5','Fertig');
      Wahl : integer = 1;
      HauptMenue : array[1..5] of string[10] =
      ('Parameter','Ihr Spiel','Mein Spiel','Game','Ende');
      HauptmenueWahl : integer = 1;

procedure LiesZeichen; (* Mit Ihrer speziellen Tastaturanpassung *)
  begin
    while not KeyPressed do Lauf;
    read(kbd,c);
  end; (* LiesZeichen *)

(* inverse_video und normal_video bitte entsprechend anpassen! *)
procedure inverse_video; begin LowVideo; end;
procedure normal_video;  begin NormVideo end;

procedure Bewerte(Ziel, Test : Kombination; var Wertung : Bewertung);
  var i, j : 1..MaxSteckerlZahl;
  begin
    (* Erst die schwarzen BewertungsSteckerl *)
    Wertung[schwarz]:=0;
    for i:=1 to SteckerlZahl do
      if Ziel[i]=Test[i] then
        begin
          Wertung[schwarz]:=succ(Wertung[schwarz]);
          Ziel[i]:=illegal; Test[i]:=illegal
        end;
    (* Dann die weissen *)
    Wertung[weiss]:=0;
    for i:=1 to SteckerlZahl do
      for j:=1 to SteckerlZahl do
        if (Ziel[i]=Test[j]) and not (Ziel[i]=illegal)
          then begin
                 Wertung[weiss]:=succ(Wertung[weiss]);
                 Ziel[i]:=illegal; Test[j]:=illegal
               end
  end;

procedure ZufallsKombination(VAR K : Kombination);
  var i : 1..MaxSteckerlZahl;
  begin
    for i:=1 to SteckerlZahl do
      K[i]:=SteckerlFarbe(random(FarbZahl)) (* Re-Typing !! *)
  end; (* ZufallsKombination *)

procedure SchreibBewertung(Wertung : Bewertung; Versuch : integer);
  var i : 1..MaxSteckerlZahl;
  begin
    gotoxy(3+SteckerlZahl*6,succ(Versuch)); ClrEol; gotoxy(33,succ(Versuch));
    for i:=1 to Wertung[schwarz] do write('O');
    for i:=1 to Wertung[weiss]   do write('X');
    writeln
  end; (* SchreibBewertung *)

procedure LiesKombination(Versuch : integer; var K : Kombination;
                          Mensch : boolean);
var FarbWahl, i, Versch : integer; temp : byte;

procedure NextWindow(i,m : integer);
  var t : integer;
  begin
    Wahl:=i; Choose(3,succ(Versuch),6,7,Menue,m,Wahl,true);
  end; (* NextWindow *)

function fertig : boolean;
  var i : integer;
  begin
    for i:=1 to SteckerlZahl do
      if Menue[i]=chr(48+i) then begin fertig:=false; exit end;
    fertig:=true
  end; (* fertig *)

begin (* LiesKombination *)
  Wahl:=1; for i:=1 to SteckerlZahl do Menue[i]:=chr(48+i);
  Menue[SteckerlZahl+1]:='Okay'; if Mensch then Versch:=3 else Versch:=43;
  repeat
    if fertig then i:=succ(SteckerlZahl) else i:=SteckerlZahl;
    gotoxy(1,succ(Versuch)); if Mensch then write(Versuch);
    Choose(3,succ(Versuch),6,7,Menue,i,Wahl,false);
    if Wahl<=SteckerlZahl then
      repeat
        FarbWahl:=1;
        temp:=ScreenPtr; ChangeWindow(MaxScreen);
        OpenWindow(Versch+pred(Wahl)*6,Versuch+7,Versch+7+pred(Wahl)*6,
                   Versuch+succ(7+FarbZahl));
        Selekt(2,1,SizeOf(Farben[1]),Farben,FarbZahl,FarbWahl);
        CloseWindow; ChangeWindow(temp);
        case FarbWahl of
          1..MaxFarben : begin
                           Menue[Wahl]:=Farben[FarbWahl];
                           K[Wahl]:=SteckerlFarbe(pred(FarbWahl));
                         end;
          -1   : if Wahl>1 then NextWindow(pred(Wahl),i);
          -2   : if Wahl<SteckerlZahl then NextWindow(succ(Wahl),i);
        end
      until (FarbWahl>=0) or (Wahl=succ(SteckerlZahl))
  until (Wahl=succ(SteckerlZahl)) and fertig;
end; (* LiesKombination *)

procedure SchreibKombination(K : Kombination);
  var i : 1..MaxSteckerlZahl;
  begin
    for i:=1 to SteckerlZahl do
      write(Farben[1+ord(K[i])],'':6-length(Farben[1+ord(K[i])]))
  end; (* SchreibKombination *)

procedure MenschenSpiel(var ZugZahl : integer);
  var i : integer;
      VersuchsBewertung : Bewertung;
      Gesucht, Versuch : Kombination;

procedure LeistungsBewertung(Versuche : integer);
  begin
    writeln;
    case (Versuche*30) DIV (SteckerlZahl*FarbZahl) of
      1..3   : writeln('   Das war ein Glueckstreffer!');
      4..6   : writeln('       Eine tolle Leistung!');
      7..9   : writeln('         Das war sehr gut!');
      10..12 : writeln('   Immerhin haben Sie es geschafft');
      else writeln(' Mehr Denken hat noch nie geschadet!')
    end
  end; (* LeistungsBewertung *)

  begin (* MenschenSpiel *)
    ChangeWindow(MenschFenster); ClrScr; inverse_video; write('  ');
    for i:=1 to SteckerlZahl do write(' ???? '); ClrEol; normal_video;
    ZufallsKombination(Gesucht);
    ZugZahl:=0;
    repeat
      ZugZahl:=succ(ZugZahl);
      LiesKombination(ZugZahl,Versuch,true);
      Bewerte(Gesucht,Versuch,VersuchsBewertung);
      SchreibBewertung(VersuchsBewertung,ZugZahl)
    until (VersuchsBewertung[schwarz]=SteckerlZahl) or (ZugZahl=MaxVersuche);
    if VersuchsBewertung[schwarz]<SteckerlZahl
      then writeln('Das war nix!') else LeistungsBewertung(ZugZahl);
    gotoxy(1,1); inverse_video; write('   '); SchreibKombination(Gesucht);
    normal_video; ChangeWindow(MenueFenster)
  end; (* MenschenSpiel *)

procedure LiesParameter;
  var c : char;
  begin
    OpenWindow(1,5,40,10);
    gotoxy(10,1); write('Die Spielparameter');
    gotoxy(1,3); write('  Anzahl der Steckerl (2..5): ',SteckerlZahl);
    gotoxy(1,4); write('  Anzahl der Farben   (2..6): ',FarbZahl);
    gotoxy(31,3);
    repeat LiesZeichen(c) until c in ['2'..'5',^M];
    if c<>^M then begin write(c); SteckerlZahl:=ord(c)-48 end;
    gotoxy(31,4);
    repeat LiesZeichen(c) until c in ['2'..'6',^M];
    if c<>^M then begin write(c); FarbZahl:=ord(c)-48 end;
    CloseWindow;
  end; (* LiesParameter *)

procedure ComputerSpiel(Var ZugZahl : integer);

label GEFUNDEN;
var E : boolean; i : integer;
    S : array[1..10] of record K : Kombination; B : Bewertung end;
    Gesucht : Kombination;

function passt(K : Kombination; Nr : integer) : boolean;
  var i : integer; BT : Bewertung;
  begin
    for i:= 1 to Nr do
      begin
        Bewerte(K,S[i].K,BT); Lauf; Lauf;
        if BT[schwarz]<>S[i].B[schwarz] then begin passt:=false; exit end else
        if BT[weiss]<>S[i].B[weiss] then begin passt:=false; exit end
      end;
    passt:=true
  end; (* passt *)

procedure NextKombination(VAR K : Kombination; var Ende : boolean);
  var i : 0..MaxSteckerlZahl;
      fertig : boolean;
  begin
    i:=SteckerlZahl; Lauf; Lauf;
    repeat
      fertig:=true;
      K[i]:=succ(K[i]);
      if ord(K[i])=FarbZahl then
        begin K[i]:=rot; fertig:=false; i:=pred(i) end
    until fertig or (i=0);
    Ende:=i=0
  end; (* NextKombination *)

function VersuchErfolgreich(Nr : integer):boolean;

procedure LiesBewertung(var B : Bewertung; Nr : integer);
  LABEL FERTIG;
  var c : char; temp : integer;
  begin
    temp:=ScreenPtr; ChangeWindow(MaxScreen); OpenWindow(43,Nr+8,78,Nr+13);
    writeln('Geben Sie die Bewertung ein'); writeln;
    write('Anzahl der schwarzen Stecker: ');
    repeat LiesZeichen(c) until (c>='0') and (c<=chr(48+SteckerlZahl));
    writeln(c); B[schwarz]:=ord(c)-48; B[weiss]:=0;
    if B[schwarz]=SteckerlZahl then goto FERTIG;
    write('Anzahl der weissen Stecker:   ');
    repeat LiesZeichen(c) until c in ['0'..chr(48+SteckerlZahl-B[schwarz])];
    write(c); B[weiss]:=ord(c)-48;
    if B[schwarz]+B[weiss]=SteckerlZahl then
      if B[weiss]=1 then begin B[schwarz]:=SteckerlZahl; B[weiss]:=0 end;
    FERTIG: CloseWindow; ChangeWindow(temp)
  end; (* LiesBewertung *)

  begin (* VersuchErfolgreich *)
    SchreibKombination(S[Nr].K); LiesBewertung(S[Nr].B,Nr);
    SchreibBewertung(S[Nr].B,Nr);
    VersuchErfolgreich:=S[Nr].B[schwarz]=SteckerlZahl
  end; (* VersuchErfolgreich *)

procedure PruefeBewertung(K : Kombination; Z : integer; var i : integer);
  begin
    i:=0; repeat i:=succ(i) until not passt(K,i);
    gotoxy(4,succ(i)); inverse_video; SchreibKombination(S[i].K); normal_video
  end; (* PruefeBewertung *)

begin (* ComputerSpiel *)
  ChangeWindow(ComputerFenster); ClrScr;
  LiesKombination(0,Gesucht,false);
  gotoxy(1,1); inverse_video; write('   ');
  SchreibKombination(Gesucht); ClrEol; normal_video;
  ZugZahl:=1; gotoxy(1,succ(ZugZahl)); write(ZugZahl,'  ');
  ZufallsKombination(S[ZugZahl].K);
  if VersuchErfolgreich(ZugZahl) then goto GEFUNDEN;
  ZugZahl:=2; gotoxy(1,succ(ZugZahl)); write(ZugZahl,'  ');
  repeat ZufallsKombination(S[ZugZahl].K) until passt(S[ZugZahl].K,1);
  if VersuchErfolgreich(ZugZahl) then goto GEFUNDEN;
  repeat
    ZugZahl:=succ(ZugZahl); gotoxy(1,ZugZahl+1); write(ZugZahl,'  ');
    for i:=1 to SteckerlZahl do S[ZugZahl].K[i]:=rot; E:=false;
    while not passt(S[ZugZahl].K,pred(ZugZahl)) and not E do
     NextKombination(S[ZugZahl].K,E);
    if E then begin
                writeln('Falsche Angaben!');
                PruefeBewertung(Gesucht,pred(ZugZahl),ZugZahl);
                goto GEFUNDEN
              end;
  until VersuchErfolgreich(ZugZahl);
  GEFUNDEN:
  ChangeWindow(MenueFenster)
end; (* ComuterSpiel *)

procedure Game;
  const ErsterWahl : array[1..2] of string[8] = ('Mensch','Computer');
  var AnDerReihe : (Mensch,Computer);
      M_Punkte, C_Punkte : integer;
      M_Ergebnis, C_Ergebnis : integer;
      Erster : integer;
      SpielEnde : boolean;
      MenueWahl : integer;
      PunkteStr : string[80];

  begin
    OpenWindow(50,5,70,9); writeln('Wer soll beginnen?'); Erster:=1;
    repeat selekt(5,2,9,ErsterWahl,2,Erster) until Erster in [0..2];
    CloseWindow;
    case Erster of
      0 : exit;
      1 : AnDerReihe:=Mensch;
      2 : AnDerReihe:=Computer;
    end;
    SpielEnde:=false; M_Punkte:=0; C_Punkte:=0; M_Ergebnis:=0; C_Ergebnis:=0;
    repeat
      PunkteStr:='Aktueller Punktestand: Mensch '+chr(48+M_Punkte)+
                 ' - Computer '+chr(48+C_Punkte)+'!';
      InitLaufSchrift(30,1,PunkteStr,20,1500);
      MenueWahl:=2+ord(AnDerReihe);
      Choose(3,3,15,11,HauptMenue,5,MenueWahl,true);
      case AnDerReihe of
        Mensch   : MenschenSpiel(M_Ergebnis);
        Computer : ComputerSpiel(C_Ergebnis);
      end;
      if succ(ord(AnDerReihe))<>Erster then
        begin
          if M_Ergebnis>C_Ergebnis
            then C_Punkte:=C_Punkte+(M_Ergebnis-C_Ergebnis)
            else M_Punkte:=M_Punkte+(C_Ergebnis-M_Ergebnis);
          SpielEnde:=(M_Punkte>=5) or (C_Punkte>=5);
          ChangeWindow(MenschFenster); ClrScr;
          ChangeWindow(ComputerFenster); ClrScr;
          ChangeWindow(MenueFenster);
        end;
      if AnDerReihe=Mensch then AnDerReihe:=Computer else AnDerReihe:=Mensch
    until SpielEnde;
    if M_Punkte>C_Punkte
      then begin
             ChangeWindow(MenschFenster);
             gotoxy(2,5); write('Gratuliere - Sie haben gewonnen!')
           end
      else begin
             ChangeWindow(ComputerFenster);
             gotoxy(2,5); write('Schade - Sie haben verloren!')
           end;
    ChangeWindow(MenueFenster);
    InitLaufSchrift(30,1,SchriftZug,20,1500)
 end; (* Game *)

begin (* SuperBrain_Professional *)
  Cursor_Off;
  InitWindows; ClrScr; cbreak:=false;
  OpenWindow(1,6,40,24); MenschFenster:=ScreenPtr;
  gotoxy(15,0); write('> MENSCH <');
  OpenWindow(41,6,80,24); ComputerFenster:=ScreenPtr;
  gotoxy(14,0); write('> COMPUTER <');
  OpenWindow(1,1,80,5); MenueFenster:=ScreenPtr;
  InitLaufSchrift(30,1,SchriftZug,20,1500);
  repeat
    Choose(3,3,15,11,HauptMenue,5,HauptMenueWahl,false);
    case HauptMenueWahl of
      1 : LiesParameter;
      2 : MenschenSpiel(M_Zuege);
      3 : ComputerSpiel(C_Zuege);
      4 : Game
    end;
  until HauptMenueWahl=5; while MaxScreen>0 do CloseWindow; ExitWindows;
  Cursor_On
end. (* SuperBrain_Professional *)