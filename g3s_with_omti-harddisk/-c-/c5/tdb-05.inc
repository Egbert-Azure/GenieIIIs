(****************************************************************************)
(*                             Indizierung                                  *)
(****************************************************************************)
overlay
procedure Indizierung;
  var Wahl, Erster : integer;
  const IndexMenue : array[1..4] of string[10] =
                    ('Erzeugen','Laden','Verwerfen','Speichern');

 function IndexVorhanden : boolean;
   begin
     assign(IndFile,IndexName);
     (*$I-*) reset(IndFile) (*$I+*);
     if IOResult=0 then begin IndexVorhanden:=true; close(IndFile) end
                   else IndexVorhanden:=false
   end; (* IndexVorhanden *)

 procedure SichereIndex;
  begin
    Schliesse_Index(IndexName,IndFile,FileGroesse,IndexAuswahl,Result);
    if Result<>0 then Fehler(10);
    indiziert:=false
  end; (* SicherIndex *)

 procedure NeuerIndex;
  var i : integer;

  procedure LiesAuswahl(var IV : IndexVektor);
    label exit;
    var i, t, Sum, Hoehe, Breite : integer;
    begin
      Sum:=0; for i:=1 to RecZahl do if Maske^[i].sl<>0 then Sum:=succ(Sum);
      t:=ScreenPtr; ChangeWindow(MaxScreen);
      if Sum>10 then Hoehe:=10 else Hoehe:=Sum;
      Breite:=20*succ(pred(Sum) DIV 10);
      if Breite>76 then Breite:=76;
      OpenWindow(2,6,3+Breite,7+Hoehe); SchreibLabels(Hoehe,Breite);
      OpenWindow(2,8+Hoehe,40,23); gotoxy(2,0); write('[ Index-Auswahl ]');
      fillchar(IV,SizeOf(IV),0); gotoxy(1,1);
      with TempMask do
      repeat
        Art:=4; l:=0; sl:=3; Sum:=0;
        for i:=1 to 10 do with IV[i] do
          begin
            m:='Feld-Nr.: '; x:=1; y:=WhereY; LiesRecord(Feld,TempMask,1,37);
            if Feld=0 then goto exit;
            m:=', Zeichenzahl: '; x:=14; LiesRecord(Anzahl,TempMask,1,37);
            if Anzahl=0 then with Maske^[Feld] do
              case Art of
                0,5 : Anzahl:=1;
                1   : Anzahl:=l;
                2,4 : Anzahl:=sl;
                3   : Anzahl:=sl SHR 4
              end;
            Sum:=Sum+Anzahl; writeln;
          end;
        if Sum>StrLaenge then begin Fehler(11); ClrScr end
      until Sum<=StrLaenge;
      exit: CloseWindow; CloseWindow; ChangeWindow(t)
    end; (* LiesAuswahl *)

  begin (* NeuerIndex *)
    LiesAuswahl(IndexAuswahl); if IndexAuswahl[1].Feld=0 then exit;
    LiesFileName(IndexName,'IND'); if IndexName='' then exit;
    erzeuge_Index(IndexName,IndFile,0,IndexAuswahl,Result);
    if Result<>0 then begin Fehler(4); exit end;
    for i:=1 to FileGroesse do
      begin
        LiesDat(DatenFile,i,Dat,SatzGroesse,Result);
        if Result<>0 then begin Fehler(2); exit end;
        MachIndex(Dat,i,IndexAuswahl,Ind);
        add_Index(IndFile,Ind,Result);
        if Result<>0 then begin Fehler(4); exit end
      end;
    indiziert:=true;
  end; (* NeuerIndex *)

 procedure LadeIndex;
  var Groesse : integer;
  begin
    LiesFileName(IndexName,'IND'); if IndexName='' then exit;
    oeffne_Index(IndexName,IndFile,Groesse,IndexAuswahl,Result);
    if Result<>0 then begin Fehler(12); exit end;
    if Groesse<>FileGroesse then
      begin
        Fehler(13);
        verwerfe_Index(IndexName,IndFile,FileGroesse,IndexAuswahl,Result);
        exit
      end;
    indiziert:=true;
  end; (* LadeIndex *)

 procedure SkipIndex;
  begin
    verwerfe_Index(IndexName,IndFile,FileGroesse,IndexAuswahl,Result);
    if Result<>0 then Fehler(5) else indiziert:=false
  end; (* SkipIndex *)

begin (* Indizierung *)
  ChangeWindow(MaxScreen); OpenWindow(28,3,40,6);
  if indiziert then erster:=3 else erster:=1; Wahl:=2;
  if not indiziert then if not IndexVorhanden then Wahl:=1;
  selekt(2,1,SizeOf(IndexMenue[1]),IndexMenue[erster],2,Wahl);
    case Wahl of
      1 : if indiziert then SkipIndex else NeuerIndex;
      2 : if indiziert then SichereIndex else LadeIndex
    end;
  CloseWindow; ZeigeFileStatus; warte:=false
end; (* Indizierung *)