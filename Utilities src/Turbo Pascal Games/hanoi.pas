Program TuermeVonHanoi;

(*$i c5:ReadChar.inc           Tastaturanpassung, Heft 3 *)
(*$i c5:window.par					 *)
(*$i c5:Window7.bib            siehe Heft 3              *)
(*$i c5:Auswahl.bib            Men}auswahl, siehe Heft 6 *)
(*$i c5:LiFo.bib               FIFO-Stack aus Heft 6     *)

const MaxHoehe=9;
type EinTurm=array[0..MaxHoehe] of byte;

Var Tuerme : array[1..3] of EinTurm;
    Turmspitze : array[1..3] of byte;
    MFenster,Zuege : Integer;   Stop : boolean;

const HauptMenue : array[1..4] of string[20]=
                ('Anz. Scheiben','Verfahren','L|sung','Ende');
      VerMenue : array[1..3] of string[20]=
                ('Rekursive Verteilung','Backtracking','Mensch');
      Wartezeit : Integer=1000;
      AnzScheiben : Integer=3;
      HWahl : Integer=1;  VWahl : Integer=1;


(* Turbo-Pascal 3.x
function ReadKey : char;
var c : char;
begin read(kbd,c); ReadKey:=c end;
*)

procedure WarteAufTaste;
Var c:char;
Begin
  c:=' ';
  if KeyPressed then c:=ReadKey else delay(Wartezeit);
  Stop:=c=^U;
  case c of
    '+':Wartezeit:=Wartezeit+100; '-':Wartezeit:=Wartezeit-100;
  End;
  if Wartezeit<0 then Wartezeit:=0;
End;

procedure EntferneScheibe(Turm : Integer; zeigen:boolean);
Begin
  if zeigen then Begin
    gotoxy(13+pred(Turm)*26-9,20-Turmspitze[Turm]);
    Write('         H         ');
  End;
  Turmspitze[Turm]:=pred(Turmspitze[Turm]);
End;

procedure ScheibeZumTurm(Scheibe, Turm : Integer; zeigen:boolean);
Begin
  if zeigen then Begin
    gotoxy(13+pred(Turm)*26-Scheibe,19-Turmspitze[Turm]);
    Write(copy('*********H*********',10-Scheibe,2*Scheibe+1));
  End;
  Turmspitze[Turm]:=succ(Turmspitze[Turm]);
  Tuerme[Turm,Turmspitze[Turm]]:=Scheibe;
End;

procedure VonTurmZuTurm(VonTurm, ZuTurm:Integer; zeigen:boolean);
Begin
  ScheibeZumTurm(Tuerme[VonTurm,Turmspitze[VonTurm]],ZuTurm,zeigen);
  EntferneScheibe(VonTurm, zeigen);
  if zeigen then begin
    gotoxy(45,5); write(Zuege:3); WarteAufTaste;
  end
End;

procedure init;
Var i,k:Integer;
Begin
  for i:=1 to 3 do
    for k:=Turmspitze[i] downto 1 do EntferneScheibe(i,true);
  fillchar(Turmspitze,sizeof(Turmspitze),0);
  for i:=AnzScheiben downto 1 do ScheibeZumTurm(i,1,true);
  Stop:=false; Zuege:=0;
End;

procedure Start;
Var i:Integer;
Begin
  InitWindows;
  gotoxy(1,8); fillchar(Turmspitze,sizeof(Turmspitze),0);
  Writeln('A':13,'B':26,'C':26); Writeln;
  for i:=1 to 10 do  Writeln('H':13,'H':26,'H':26);
  gotoxy(1,5); Write('Verfahren: ',VerMenue[VWahl]);
  for i:=1 to 3 do Tuerme[i,0]:=100; (* der 'Boden' der Tuerme *)
  Init; OpenWindow(1,1,80,3); MFenster:=ScreenPtr;
End;

(* $a-  bei CP/M *)
$a-
procedure BackTracking;
(* Das ist ein abschreckendes Beispiel fuer den Einsatz von Backtracking *)
Var ZugListe : LIFO;
    EinZug : Record
               von, zu : Integer;
             End;

Function Loesung(zt,vt:Integer) : boolean;
Var t,i,Scheibe:Integer;

procedure ZugMerken;
begin
  EinZug.von:=t; EinZug.zu:=i; push(ZugListe,EinZug,sizeof(EinZug));
End;

Begin
  t:=1; Loesung:=true;
  while t<>4 do Begin   (* Alle Turmspitzen probieren *)
    Scheibe:=Tuerme[t,Turmspitze[t]]; i:=1;
    while i<>4 do Begin  (* Alle Tuerme probieren *)
      if (zt<>t) and (vt<>i) then
        if Tuerme[i,Turmspitze[i]]>Scheibe then Begin (* Zug versuchen *)
          VonTurmZuTurm(t,i,false);
          if Turmspitze[3]=AnzScheiben then  (* L|sung gefunden *)
            Begin ZugMerken; exit End
            else if Loesung(i,t) then Begin ZugMerken; exit End
              else   (* war nichts, Zug r}ckg{ngig machen *)
                VonTurmZuTurm(i,t,false);
        End;
      i:=succ(i);
    End;  (* while i *)
    t:=succ(t); vt:=0;
  End;
  Loesung:=false;
End;

Begin (* BackTracking *)
  InitLIFO(ZugListe);
  if not Loesung(0,0) then writeln('keine L|sung gefunden');
  Init;   (* nun die Zuege anzeigen *)
  while not (EmptyLIFO(ZugListe) or Stop) do Begin
    pop(ZugListe,EinZug,sizeof(EinZug)); Zuege:=succ(Zuege);
    with EinZug do VonTurmZuTurm(von, zu,true);
  End;
End;

procedure Verteilen(N,A,B,C:Integer);
(* N: H|he des Turms, A: "Quellturm", B: Hilfsturm, C: Zielturm *)
Begin
  Zuege:=succ(Zuege);
  If N=1 Then VonTurmzuTurm(A,C,true)  (* Turm mit nur einer Scheibe *)
  else begin
    Verteilen(N-1,A,C,B);              (* den oberen Turm abbauen *)
    if Stop then exit;
    VonTurmZuTurm(A,C,true);           (* die unterste Scheibe nach rechts*)
    Verteilen(N-1,B,A,C);              (* den oberen Turm rechts aufbauen *)
  end;
end;
(* $a+  bei CP/M *)
$a+
procedure MenschWillAuchMal;
type anystring=string[20];
Var c:char; TW,vT,zT : Integer;

procedure Fragen(txt: anystring; var Turm:Integer);
Begin
  Write(txt); ClrEol;
  repeat c:=ReadKey; c:=upcase(c); until c in ['A'..'C',^U];
  Write(c); Turm:=ord(c)-ord('A')+1;
End;

Begin
  TW:=Wartezeit; Wartezeit:=0;
  repeat
    repeat
      gotoxy(1,23); Fragen('von Turm ',vT);
    until (c=^U) or (Turmspitze[vT]<>0);
    if c<>^U then Fragen('  zum Turm ',zT);
    if c<>^U then
      if Tuerme[zT,Turmspitze[zT]]>Tuerme[vT,Turmspitze[vT]] then Begin
        Zuege:=succ(Zuege); VonTurmZuTurm(vT,zT,true);
      end else Begin
        Write('  ung}ltiger Zug ! '); repeat until KeyPressed;
      End;
  until (Turmspitze[3]=AnzScheiben) or (c=^U);
  Wartezeit:=TW;
End;


Begin
  Start;
  repeat
    SelWindow(MFenster);
    Auswahl(1,1,19,4,sizeof(HauptMenue[1]),Hauptmenue,4,HWahl);
    case HWahl of
      1 : Begin
            OpenWindow(15,2,45,4);
            repeat
              gotoxy(1,1); ClrEol;
              Write('Anzahl der Scheiben ? '); Read(AnzScheiben);
            until AnzScheiben in [1..MaxHoehe];
            CloseWindow; SelWindow(0); Init;
          End;
      2 : Begin
            OpenWindow(15,2,36,6);
            Auswahl(1,1,20,1,sizeof(VerMenue[1]),VerMenue,3,VWahl);
            CloseWindow; SelWindow(0);
            gotoxy(1,5); Write('Verfahren: ',VerMenue[VWahl]); ClrEol
          End;
      3 : Begin
            SelWindow(0); Init; gotoxy(1,23);
            Write('^U -> Abbrechen, +/- -> Wartezeit {ndern');
            gotoxy(35,5); write('Zugnummer:');
            case VWahl of
              1: Verteilen(AnzScheiben,1,2,3);
              2: BackTracking;
              3: MenschWillAuchMal;
            End;
          End;
    End; (* Case *)
  until HWahl=4;
  ExitWindows; ClrScr
End.
