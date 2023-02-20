program Ueberweisungen;

(*$I c5:WINDOW.PAR   *) {Spezielle Anpassung der Windows an Rechner         }
(*$I c5:WINDOW7.BIB  *) {Fenster auf jedem Rechner, siehe Turbo-Heft 3      }
(*$I c5:MASKE-1.BIB  *) {Maskenein- und ausgabe aus Heft 5, vgl. auch 1 u. 3}
(*$I c5:CHOOSE.BIB   *) {Horizontale Menueauswahl, siehe Turbo-Heft 5       }
(*$I c5:SELECT-1.BIB *) {Vertikale Menueauswahl, siehe Turbo-Heft 5         }
(*$I c5:PTRESC.INC   *) {Drucker-Escape-Sequenzen, siehe Turbo-Heft 6       }

type Lieferant = record
                   LNr : integer;
                   LName  : string[40];
                   LKonto : string[10];
                   LBLZ   : string[8];
                   LBank  : string[40]
                 end;
     ListenInhalt = Lieferant;

function Kleiner(Var1,Var2:ListenInhalt) : boolean;
  begin Kleiner:=Var1.LNr<Var2.LNr end;

(*$I LISTE.BIB    *) {Listenoperationen aus Turbo-Heft 2                 }

const KontoA = '1234567123';
      NameA  = 'Test-Firma GmbH & Co., Muenchen';
      BLZA = '70150000';

const Eintraege    = 7;
      EingabeMaske : array[1..Eintraege] of MaskEintr =
          ((x:1;  y:1; m:'Empfaenger: ';                l:40; sl:40; Art:1),
           (x:1;  y:3; m:'Konto-Nr. des Empfaengers: '; l:10; sl:10; Art:1),
           (x:40; y:3; m:'Bankleitzahl: ';              l:8;  sl:8;  Art:1),
           (x:1;  y:4; m:'bei Bank: ';                  l:40; sl:40; Art:1),
           (x:1;  y:6; m:'Betrag: ';                    l:5;  sl:178;Art:3),
           (x:1;  y:8; m:'Verwendungszweck: ';          l:40; sl:40; Art:1),
           (x:1;  y:9; m:'noch Verw.zweck : ';          l:40; sl:40; Art:1));

       HauptMenue : array[1..5] of string[15] =
                     ('Eingeben','Drucken','Konten','Loeschen','Quit');

type   UeWFormular = record
                       Empfaenger       : string[40];
                       KontoE           : string[10];
                       BLZ              : string[8];
                       Bank             : string[40];
                       Betrag           : real;
                       Zweck1, Zweck2   : string[40];
                 end;

        anystring = string[127];

var Wahl  :integer;
    LFile : file of Lieferant;
    LTemp : Lieferant;
    KontenListe, SuchListe : Liste;
    FormularFenster, StatusFenster, WahlFenster, KontenFenster: integer;
    Ueberweisung : UeWFormular;
    f : file of UeWFormular;
    Summe : real;


(****************************************************************************)
(*                    diverse globale Hilfsprozeduren                       *)
(****************************************************************************)

(* Diese Prozedur nur wenn ZEITUHR.INC nicht eingebunden wurde! *)
(*
var Datum : string[10];
procedure DatumLesen ;
  const DatMaske : MaskEintr =
     (x:1; y:1; m:'Bitte Datum eingeben: '; l:10; sl:10; Art:1);
  begin Datum:=''; LiesRecord(Datum,DatMaske,1,80) end;
*)

procedure Pieps; begin write(^G) end;

function LoeschBestaetigung : boolean;
  const LoeschFrage : array[1..2] of string[10] = ('Zurueck','Loeschen');
  var   LoeschWahl : integer;
  begin
    LoeschWahl:=1; OpenWindow(29,7,51,10);
    choose(1,1,10,11,LoeschFrage,2,LoeschWahl,false);
    LoeschBestaetigung:=LoeschWahl=2; CloseWindow
  end;

function WeiterDrucken : boolean;
  const Frage : array[1..2] of string[10] = ('Drucken','Abbruch');
  var   WWahl : integer;
  begin
    WWahl:=1; OpenWindow(29,7,51,10); choose(1,1,10,11,Frage,2,WWahl,false);
    WeiterDrucken:=WWahl=1; CloseWindow
  end;

procedure Status;
  begin
    ChangeWindow(StatusFenster); gotoxy(1,1);
    write('Ueberweisungen gesamt: ',filesize(f):4,
          '  Position:  #',filepos(f):4);
    ChangeWindow(FormularFenster);
  end;

procedure Schreib_letzten;
  begin
    if filesize(f)<>0
    then
      begin
        seek(f,pred(filesize(f))); read(f,Ueberweisung);
        SchreibRecord(Ueberweisung,EingabeMaske,Eintraege);
      end
    else ClrScr
  end;

(****************************************************************************)
(* Eingabe und Editieren der einzelnen Ueberweisungen                       *)
(****************************************************************************)

procedure Eingeben;
  const Auswahl : array[1..4] of String[13] =
                  ('Neueingabe','Editieren','Vorwaerts','Zurueck');
  var   LNummer,Code,gewaehlt : integer;

  procedure Neueingabe;
    begin
      seek(f,filesize(f)); Status;
      fillchar(Ueberweisung,SizeOf(Ueberweisung),0);
      SchreibRecord(Ueberweisung,EingabeMaske,Eintraege);
      LiesRecord(Ueberweisung,EingabeMaske,1,78);
      if Ueberweisung.Empfaenger = '' then Schreib_letzten
      else begin
             val(Ueberweisung.Empfaenger,LNummer,Code);
             if Code=0
             then begin
                    LTemp.LNr:=LNummer;
                    Suchen(KontenListe,LTemp,SuchListe);
                    if SuchListe<>NIL
                    then
                      with Ueberweisung, SuchListe^.Eintrag do
                        begin
                          Empfaenger:=LName;
                          KontoE:=LKonto;
                          BLZ:=LBLZ;
                          Bank:=LBank;
                        end (* with Ueberweisung, .. *)
                    else begin Ueberweisung.Empfaenger:=''; Pieps end;
                  end (* if Code = 0 *)
               else Pieps;
             LiesRecord(Ueberweisung,EingabeMaske,Eintraege,78);
             if Ueberweisung.Empfaenger<>''
             then write(f,Ueberweisung)
             else Schreib_letzten;
           end (* if *)
    end; (* Neueingabe *)


  begin (* Eingeben *)
    OpenWindow(3,5,17,10); WahlFenster:=ScreenPtr; gewaehlt:=1;
    repeat
    ChangeWindow(WahlFenster); selekt(1,1,14,Auswahl,4,gewaehlt);
    case gewaehlt of
      1 : Neueingabe;
      2 : if filesize(f)>0 then
          begin
            seek(f,pred(filepos(f)));
            read(f,Ueberweisung); ChangeWindow(FormularFenster);
            LiesRecord(Ueberweisung,EingabeMaske,Eintraege,78);
            seek(f,pred(filepos(f))); write(f,Ueberweisung);
          end;
      3 : if filepos(f)<filesize(f)
               then begin
                   read(f,Ueberweisung); Status;
                   SchreibRecord(Ueberweisung, EingabeMaske, Eintraege);
                 end;
      4 : if filepos(f)>1
                then begin
                   seek(f,filepos(f)-2); read(f,Ueberweisung); Status;
                   SchreibRecord(Ueberweisung, EingabeMaske, Eintraege);
                 end;
      -2 : Wahl:=succ(Wahl);
      -1 : Wahl:=5;
     end; (* case *)
     Status;
     until gewaehlt < 1;
     CloseWindow;
 end;  (* Eingeben *)

(****************************************************************************)
(* Ausdrucken der ausgefuellten Ueberweisungen                              *)
(****************************************************************************)

procedure Drucken;
  label exit;
  const Auswahl : array[1..3] of string[13]=
                  ('Endlosform.', 'Einzelform.', 'Liste');
        vorschub = #10#13#10#13;
  var c:char; Mark:string[9]; Pfennig: string[2];
      i, gewaehlt :integer;

  procedure Print(Ein : anystring);
    var i:integer; Aus : anystring;
    begin
      Ein:=copy(Ein,1,27);
      Aus:=''; for i:=1 to length(Ein) do Aus:= Aus+Ein[i]+#32;
      write(lst,Aus);
    end;

  begin
    OpenWindow(18,5,32,9); gewaehlt:=1; Summe:=0;
    selekt(1,1,14,Auswahl,3,gewaehlt); reset(f);
    case gewaehlt of
      -1 : begin Wahl:=pred(Wahl); goto exit end;
      -2 : begin Wahl:=succ(Wahl); goto exit end;
       0 : goto exit;
    end; (* case gewaehlt of *)
    if not WeiterDrucken then goto exit;
    while not eof(f) do
      begin
        ChangeWindow(FormularFenster); read(f,Ueberweisung); Status;
        SchreibRecord(Ueberweisung, EingabeMaske, Eintraege);
        with Ueberweisung do
          begin
          case gewaehlt of
              1 : begin
                    PrinterAuf(PageLength+#25);
                    write(lst,'  ',Empfaenger);
                    for i:=1 to 44-length(Empfaenger) do write(lst,' ');
                    writeln(lst,BLZ,CRLF);
                    writeln(lst,'  ',KontoE:10,'       ',Bank,CRLF,CRLF,CRLF);
                    writeln(lst,'  ',Zweck1);
                    write(lst,'  ',Zweck2);
                    for i:=1 to 42-length(Zweck2) do write(lst,' ');
                    writeln(lst,Betrag:10:2);
                    for i:=1 to 5 do write(lst,CRLF);
                    writeln(lst,'  ',KontoA:10,'       ',NameA);
                    printerAuf(FF);
                    (* Alternativ, wenn nicht Seitenlaenge auf 25 fest- *)
                    (* gelegt wurde: for i:=1 to 11 do writeln(lst);    *)
                    PrinterAuf(Reset);
                  end;
              2 : begin
                    if not WeiterDrucken then goto exit;
                    str(int(Betrag):0:0,Mark);
                    str(frac(Betrag)*100:0:0,Pfennig);
                    write(lst,' ':28); write(lst,Datum,Vorschub,Vorschub);
                    Print('  '+Empfaenger); write(lst,Vorschub);
                    Print('   '+KontoE);
                    for i:=1 to 17-length(KontoE) do write(lst,'  ');
                    Print(BLZ); write(lst,Vorschub);
                    Print('  '+Bank); write(lst,Vorschub);
                    for i:=1 to 23-length(Mark) do write(lst,'  ');
                    Print(Mark+' '+Pfennig); write(lst,Vorschub);
                    Print('  '+Zweck1); write(lst,Vorschub);
                    Print('  '+Zweck2); write(lst,Vorschub);
                    Print('  '+NameA); write(lst,Vorschub);
                    Print('   '+KontoA);
                    for i:= 1 to 20-length(KontoA)-length(Mark)
                      do write(lst,'  ');
                    Print(Mark+' '+Pfennig); writeln(lst);
                  end;
              3 : begin
                    write(lst,filepos(f):3,'. ',Empfaenger);
                    for i:=1 to 50-length(Empfaenger) do write(lst,' ');
                    writeln(lst,Betrag:13:2,' DM');
                  end;
          end; (* case *)
          Summe:=Summe+Betrag;
          end; (* with *)
      end; (* while *)
      if gewaehlt=3
        then writeln(lst,CRLF,'Gesamtsumme: ',Summe:55:2,' DM',CRLF,'Datum: ',Datum);
    exit: CloseWindow;
  end;  (* Print *)

(****************************************************************************)
(*                      Verwalten der Kontonummern                          *)
(****************************************************************************)

procedure KontenListeLesen;
  begin
    (*$I-*) reset(LFile); (*$I+*)
    if IOResult=0 then while not eof(LFile) do
                          begin
                            read(LFile,LTemp);
                            Einfuegen(KontenListe,LTemp)
                          end;
     close(LFile)
   end;

procedure KontenListeSchreiben;
  begin
    rewrite(LFile);
      SuchListe:=KontenListe;
      while SuchListe<>NIL do
        begin
          write(LFile,SuchListe^.Eintrag);
          SuchListe:=SuchListe^.Naechster
        end;
     close(LFile)
   end;

procedure KontenVerwaltung;
  const Auswahl : array[1..4] of string[13]=
                  ('Einfuegen','Editieren','Loeschen','Kontenliste');
        KontenMaske : array[1..5] of MaskEintr =
          ((x:1;  y:1; m:'Kreditoren-Nr. : ';l:1;sl:4;Art:2),
           (x:1;  y:3; m:'Lieferant: ';     l:40; sl:40; Art:1),
           (x:1;  y:5; m:'Konto-Nr. des Kreditoren: '; l:10; sl:10; Art:1),
           (x:40; y:5; m:'Bankleitzahl: ';    l:8; sl:8; Art:1),
           (x:1;  y:7; m:'bei Bank: '; l:40;  sl:40;  Art:1));
   var gewaehlt : integer;

  procedure ListeDrucken;
  const DevWahl : array[1..3] of string[13]=
                  ('Drucker','Bildschirm','Abbruch');
  var dev : integer; Device : text;
    begin
      dev := 2;
      OpenWindow(20,7,60,10); choose(1,1,13,14,DevWahl,3,dev,false);
      CloseWindow;
      case dev of
        1 : assign(Device,'lst:');
        2 : begin OpenWindow(1,1,80,25); assign(Device,'con:') end;
        3 : exit;
      end;
      SuchListe:=KontenListe;
      writeln(Device,'Liste der Kontonummern',' ':20,
                     'Stand: ',Datum,CRLF);
      while SuchListe<>NIL do
        with SuchListe^ do
          begin
            with Eintrag do writeln(Device,LNr:3,'. ',LName,CRLF,' ':5,
                                           LKonto: 12,LBLZ:10,'  ',LBank,CRLF);
            SuchListe:=Naechster;
          end; (* with SuchListe *)
      if dev=2
      then begin writeln('Weiter: RETURN'); readln; CloseWindow end;
    end;  (* ListeDrucken *)

  begin (* KontenVerwaltung *)
    OpenWindow(33,5,47,10); WahlFenster:=ScreenPtr; gewaehlt:=1;
    repeat
      ChangeWindow(WahlFenster); selekt(1,1,14,Auswahl,4,gewaehlt);
      ChangeWindow(FormularFenster);
      gotoxy(26,0); write('>   Konten-Verwaltung   <'); ClrScr;
      fillchar(LTemp,sizeof(LTemp),0);
      case gewaehlt of
        1 : begin
              LiesRecord(LTemp,KontenMaske,5,78);
              if LTemp.LNr <> 0
                then begin
                       Suchen(KontenListe,LTemp,SuchListe);
                       if SuchListe=NIL then Einfuegen(KontenListe,LTemp)
                                        else Pieps
                     end; (* if *)
            end;
        2 : begin
              LiesRecord(LTemp,KontenMaske,1,78);
              Suchen(KontenListe,LTemp,SuchListe);
              if SuchListe <> NIL
              then begin
                     LiesRecord(SuchListe^.Eintrag,KontenMaske,5,78);
                     SuchListe^.Eintrag.LNr:=LTemp.LNr;
                     SchreibRecord(SuchListe^.Eintrag,KontenMaske,5);
                   end
              else Pieps;
            end;
        3 : begin
              ClrScr; LiesRecord(LTemp,KontenMaske,1,78);
              Suchen(KontenListe,LTemp,SuchListe);
              if SuchListe<>NIL
              then begin
                     SchreibRecord(SuchListe^.Eintrag,KontenMaske,5);
                     if LoeschBestaetigung
                     then begin
                            Loeschen(KontenListe,SuchListe);
                            ChangeWindow(FormularFenster); ClrScr
                          end
                   end
              else Pieps;
            end;
        4 : ListeDrucken;
       -1 : Wahl:= pred(Wahl);
       -2 : Wahl:= succ(Wahl);
      end; (* case *)
    until gewaehlt < 1;
    ChangeWindow(WahlFenster); CloseWindow;
    ChangeWindow(FormularFenster);
    gotoxy(26,0); write('> UEBERWEISUNGSFORMULAR <'); Schreib_letzten;
  end; (* KontenVerwaltung *)

(****************************************************************************)
(*                      Beginn des Hauptprogramms                           *)
(****************************************************************************)

begin
  ClrScr;
  (* DatumLesen;  nur wenn nicht ZEITUHR.INC eingebunden ist *)
  InitWindows; KontenListe:=NIL;
  fillchar(Ueberweisung,sizeof(Ueberweisung),0);
  OpenWindow(1,1,80,5); StatusFenster:=ScreenPtr;
  OpenWindow(1,12,80,24); FormularFenster:=ScreenPtr;
  gotoxy(26,0); write('> UEBERWEISUNGSFORMULAR <');
  assign(LFile,'KONTEN.UEW');
  KontenListeLesen;
  assign(f,'TEMP.UEW');
  (*$I-*) reset(f); (*$I+*)
  if IOResult<>0 then rewrite(f)
                 else Schreib_letzten;
  Status; Wahl:=1;
  repeat
    ChangeWindow(StatusFenster);
    choose(2,3,15,16,HauptMenue,5,Wahl,false);
    case Wahl of
      1 : Eingeben;
      2 : Drucken;
      3 : KontenVerwaltung;
      4 : if LoeschBestaetigung
            then begin close(f); rewrite(f); ClrScr; Status end;
    end; (* case *)
  until Wahl in [5] ;
  KontenListeSchreiben;
  close(f); while MaxScreen > 0 do CloseWindow; ExitWindows;
end. (* UEBER *)
