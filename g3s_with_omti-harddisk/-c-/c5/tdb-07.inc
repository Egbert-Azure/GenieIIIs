(****************************************************************************)
(*                             Sonstiges                                    *)
(****************************************************************************)
overlay
procedure Sonstiges;
  const SonstigesMenue : array[1..5] of string[20] =
    ('Statistik','Zerlegung','Information','Betriebssystem','Quit');
  VAR SonstigesWahl, Erstes, tempScr : integer;

  procedure SystemInformation;
    type str20 = string[20];
    var Summe, max : integer;

    procedure S_Label(S : str20);
      begin
        LowVideo; write(s); NormVideo
      end; (* S_Label *)

  Procedure WegLaengen(VAR Summe, Max : integer);
    (*A-*)
    Procedure W(B:BinBaum; T:Integer);
      begin
        if b <>NIL then begin
          Summe:=Summe+T;
          if t>Max then Max:=t;
          W(B^.links,succ(t));
          W(B^.rechts,succ(t))
        end (*if*)
      end; (* W *)
      (*$A+*)
    begin (* WegLaengen *)
      Summe:=0; Max:=0; W(IndexBaum,1);
    end; (* WegLaengen *)

    begin
      tempScr:=ScreenPtr; ChangeWindow(MaxScreen);
      OpenWindow(20,6,60,18);
      gotoxy(10,0);
      write('[System-Information]');
      gotoxy(1,1);
      S_Label('Aktive Datei: ');
      if DateiOffen then writeln(DBName) else writeln('keine');
      S_Label('Aktiver Index: ');
      if indiziert then writeln(IndexName) else writeln('keiner');
      if DateiOffen then begin
        S_Label('Dateigr|~e: '); writeln(FileGroesse,' Eintr{ge');
        S_Label('Satzgr|~e: '); writeln(SatzGroesse,' Bytes');
        S_Label('Feldzahl: '); writeln(RecZahl);
        if indiziert and (FileGroesse>0) then begin
          WegLaengen(Summe,Max);
          S_Label('Maximaler Suchweg: '); writeln(Max);
          S_Label('Mittlerer Suchweg: '); writeln(Summe/FileGroesse:5:2);
        end (* if indiziert *)
      end; (* if DateiOffen *)
      gotoxy(1,10); S_Label('Freier Speicher: ');
      writeln(' Bytes');
      gotoxy(5,11); WarteAufESC;
      CloseWindow; ChangeWindow(tempScr)
    end; (* SystemInformation *)

procedure Statistik;  (* Ermittelt absolute und relative H{ufigkeiten *)

const  MaxGruppe = 10;
       Eintraege = 20; (* 2*MaxGruppe *)
       StatMenue : array[1..6] of string[10]=
            ('X-Gruppe','Y-Gruppe','Auswertung','Tabelle','%-Tabelle',
              'Selektion');
        SelStr : string[60] = '';

type   Gruppe = RECORD
                  S : ARRAY[1..Eintraege] OF MaskEintr;
                  Grenzen : ARRAY[1..220] OF BYTE;
                  Max : 0..MaxGruppe;
                  Feld : 1..127;
                  Automatik : boolean;
                  Offset, Faktor : real;
                END;

var    Messung : ARRAY[0..MaxGruppe,0..MaxGruppe] OF integer;
       Gruppe1, Gruppe2 : ^Gruppe;
       StatWahl, tScr, Anzahl, legal : integer;
       G1Fenster, G2Fenster, TabFenster : integer;
       F_X, F_Y : ARRAY[0..MaxGruppe] OF integer;
       FL_X, FL_Y : ARRAY[1..MaxGruppe] OF integer;
       EX,EY,EXY,S2X,S2Y,KorrXY : real;

procedure Selektion;
  var test : boolean;
      fc : integer;
  begin
    OpenWindow(2,4,78,7); writeln('Selektion:');
    with TempMask do begin
      Art:=1; l:=60; sl:=60; x:=1; y:=2; m:='> ';
      LiesRecord(SelStr,TempMask,1,60);
      test:=passt(Dat,SelStr,fc);
      if fc<>0 then begin
        Fehler(7);
        SelStr:=''
      end (* if *)
    end;
    CloseWindow
  end; (* Selektion *)

procedure GruppenMenue(VAR G : Gruppe);
  var i, Nr : integer;
    tr,tv,tb : real; ti : integer;
  begin
    with TempMask DO
      BEGIN x:=1; y:=1; m:='Feld-Nr: '; Art:=2; l:=1; sl:=3 END;
    WITH G DO BEGIN
      Nr:=Feld;
      LiesRecord(Nr,TempMask,1,33);
      if Nr>RecZahl then exit;
      if Nr=0 then begin fillchar(G,SizeOF(G),0); ClrScr; exit end;
      if Maske^[Nr].sl=0 then exit;
      write(' -> ',Maske^[Nr].m); ClrEol;
      IF Nr<>Feld then
        begin fillchar(G,SizeOf(G),0); Feld:=Nr; Faktor:=1.0 end;
      with TempMask DO
        BEGIN
          x:=1; y:=2; m:='Automatische Feldbestimmung: ';
          Art:=5; l:=0; sl:=4
        END;
      LiesRecord(Automatik,TempMask,1,33);
      IF Automatik then
        if Maske^[Feld].Art in [2,3,4]
          then with TempMask do
            begin
              Max:=10; tv:=0.0; tb:=0.0;
              m:='von: '; Art:=3; l:=5; sl:=10 SHL 4 OR 4;
              y:=3; LiesRecord(tv,TempMask,1,33);
              m:='bis: '; y:=4; LiesRecord(tb,TempMask,1,33);
              Offset:=tv; Faktor:=(tb-tv)/10;
              gotoxy(1,3); ClrEol; gotoxy(1,4); ClrEol;
            end
          else Max:=0;
      FOR i:=1 TO 20 DO BEGIN
        S[i]:=Maske^[Feld];
        WITH S[i] DO BEGIN
          if Art=1 then if l>10 then begin l:=10; sl:=10 end;
          IF ODD(i)
            THEN BEGIN m:='von: '; x:=1;
                   if i<19 then m:=' '+chr(48+(i+1)DIV 2)+') '+m
                           else m:='10) '+m;
                 END
            ELSE BEGIN m:='bis: '; x:=21 END;
          y:=4+(i-1) DIV 2; gotoxy(x,y); ClrEol;
          if Automatik then
                     case Art of
                       3 : begin
                             tr:=Offset+Faktor*(i DIV 2);
                             CopyTempRec(i,Grenzen,tr,S)
                           end;
                       2,4 : begin
                               tr:=Offset+Faktor*(i DIV 2);
                               if abs(tr)<=maxint then ti:=round(tr);
                               CopyTempRec(i,Grenzen,ti,S)
                             end;
                     end (* CASE *)
        END; (* WITH *)
      END; (* FOR *)
      IF G.Automatik
        THEN SchreibRecord(G.Grenzen,G.S,20)
        ELSE LiesRecord(G.Grenzen,G.S,20,40)
    END; (* WITH G *)
  end; (* GruppenMenue *)

procedure Auswertung(VAR Anzahl : integer);
  label Weiter;
  var i, j, fc : integer;

  procedure inc(VAR x : integer);
    begin
      x:=succ(x)
    end; (* inc *)

  function Nr(VAR G : Gruppe) : integer;
    var i : integer;
        wert : array[byte] of byte; (* absolute TX; *)
        von,bis : array[0..10] of byte;
        high : integer;

    function InRange(Typ : byte; VAR von,bis,wert):boolean;
      var s_von : string[10] absolute von;
        s_bis : string[10] absolute bis;
        s_wert: string[255] absolute wert;
        i_von : integer absolute von;
        i_bis : integer absolute bis;
        i_wert: integer absolute wert;
        c_von : char absolute von;
        c_bis : char absolute bis;
        c_wert: char absolute wert;
        r_von : real absolute von;
        r_bis : real absolute bis;
        r_wert: real absolute wert;
        b_von : byte absolute von;
        b_bis : byte absolute bis;
        b_wert :byte absolute wert;
      begin
        case Typ of
          0 : InRange:=c_wert in [c_von..c_bis];
          1 : InRange:=(s_wert>=s_von) and (s_wert<=s_bis);
          2 : InRange:= (i_wert>=i_von) and (i_wert<=i_bis);
          3 : InRange:=(r_wert>=r_von) and (r_wert<=r_bis);
        4,5 : InRange:=b_wert in [b_von..b_bis]
        end (* CASE *)
      end; (* InRange *)

    begin (* Nr *)
      with G do begin
        IF Feld=0 THEN BEGIN Nr:=1; EXIT END;
        CopyRecTemp(Feld,Dat,wert,Maske^);
        if S[1].Art=1 then if wert[0]>10 then wert[0]:=10;
        if Automatik then high:=max else high:=MaxGruppe;
        for i:=1 to high do
          begin
            CopyRecTemp(2*i-1,Grenzen,von,S);
            CopyRecTemp(2*i,Grenzen,bis,S);
            if InRange(Maske^[Feld].Art,von,bis,wert)
              then begin Nr:=i; exit end
          end; (* FOR *)
        if Automatik then
          if Max<MaxGruppe
            then begin
                   Max:=succ(max);
                   CopyTempRec(2*Max-1,Grenzen,wert,S);
                   CopyTempRec(2*Max,Grenzen,wert,S);
                   Nr:=Max;
                   exit
                 end;
      end; (* WITH *)
      Nr:=0
    end; (* Nr *)

  begin (* Auswertung *)
    fillchar(Messung,SizeOf(Messung),0);
    Anzahl:=0;
    for i:=1 to FileGroesse do
      begin
        if Abbruch then goto Weiter;
        LiesDat(DatenFile,i,Dat,SatzGroesse,fc);
        if fc<>0 then HALT;
        if passt(Dat,SelStr,fc) then begin
          inc(Messung[Nr(Gruppe1^),Nr(Gruppe2^)]);
          inc(Anzahl)
        end
      end; (* FOR *)
    Weiter:
    with Gruppe1^ do
      if Automatik and (S[1].Art in [0..1])
        then begin
               SelWindow(G1Fenster);
               Automatik:=FALSE;
               gotoxy(30,2); write('NEIN');
               SchreibRecord(Grenzen,S,20)
             end;
    with Gruppe2^ do
      if Automatik and (S[1].Art in [0..1])
        then begin
               SelWindow(G2Fenster);
               Automatik:=FALSE;
               gotoxy(30,2); write('NEIN');
               SchreibRecord(Grenzen,S,20)
             end;
    legal:=0; Anzahl:=0;
    for i:=0 to MaxGruppe do
      begin
        F_X[i]:=0; F_Y[i]:=0;
        IF i>0 THEN BEGIN FL_X[i]:=0; FL_Y[i]:=0 END;
        for j:=0 TO MaxGruppe do
          begin
            Anzahl:=Anzahl+Messung[i,j];
            if i*j>0 then legal:=legal+Messung[i,j];
            F_X[i]:=F_X[i]+Messung[i,j];
            F_Y[i]:=F_Y[i]+Messung[j,i];
            IF j>0 THEN BEGIN
                          FL_X[i]:=FL_X[i]+Messung[i,j];
                          FL_Y[i]:=FL_Y[i]+Messung[j,i];
                        END;
          end
      end;
    EX:=0.0; EY:=0.0; EXY:=0.0;
    FOR i:=1 TO MaxGruppe DO
      begin
        EX:=EX+(Gruppe1^.Offset+Gruppe1^.Faktor*i)*FL_X[i];
        EY:=EY+(Gruppe2^.Offset+Gruppe2^.Faktor*i)*FL_Y[i];
        FOR j:=1 TO MaxGruppe DO
          EXY:=EXY+(Gruppe1^.Offset+Gruppe1^.Faktor*i)*
                   (Gruppe2^.Offset+Gruppe2^.Faktor*j)*Messung[i,j]
      end;
    IF legal>0 THEN BEGIN EX:=EX/legal; EY:=EY/legal; EXY:=EXY/legal END;
    S2X:=0.0; S2Y:=0.0;
    FOR i:=1 TO MaxGruppe DO
      BEGIN
        S2X:=S2X+FL_X[i]*SQR(Gruppe1^.Offset+Gruppe1^.Faktor*i-EX);
        S2Y:=S2Y+FL_Y[i]*SQR(Gruppe2^.Offset+Gruppe2^.Faktor*i-EY);
      END;
    IF legal>0 THEN BEGIN S2X:=S2X/legal; S2Y:=S2Y/legal END;
    IF SQRT(S2X*S2Y)<>0.0 THEN KorrXY:=(EXY-EX*EY)/SQRT(S2X*S2Y)
  end; (* Auswertung *)

procedure Tabelle(Prozent : BOOLEAN; Anzahl : integer);
  var i,j,xm,ym,TabScr : integer;
      leer : boolean;
      c : char;

  procedure print(X : integer);
    begin
      if X=0 then begin write('    '); exit end;
      if Prozent
        then if X=Anzahl then write('100.')
                         else write(100.0*X/Anzahl:4:1)
        else write(X:4)
    end; (* print *)

  procedure Kenndaten;
    begin
      OpenWindow(50,14,79,24);
      GotoXY(2,0); writeln('[ Kenndaten ]');
      writeln('Anzahl der Me~werte: ',legal);
      writeln('Me~g}te: ',100*legal/Anzahl:3:0,'%');
      writeln('E(X) = ',EX:1:2);
      writeln('Varianz(X) = ',S2X:1:2);
      writeln('StandAbw(X)= ',SQRT(S2X):1:2);
      IF Gruppe2^.Feld>0
        THEN BEGIN
               writeln('E(Y) = ',EY:1:2);
               writeln('StandAbw(Y)= ',SQRT(S2Y):1:2);
               writeln('Varianz(Y) = ',S2Y:1:2);
               write('Korr-Koeff = ',KorrXY:1:2)
             END;
      REPEAT UNTIL KeyPressed;
      CloseWindow; SelWindow(TabScr)
    end;

  begin (* Tabelle *)
    IF Anzahl=0 THEN EXIT;
    xm:=10; leer:=true;
    while leer and (xm>0) do
      begin
        for j:=0 to 10 do if Messung[xm,j]>0 then leer:=false;
        if leer then xm:=xm-1
      end;
    ym:=10; leer:=true;
    while leer and (ym>0) do
      begin
        for j:=0 to 10 do if Messung[j,ym]>0 then leer:=false;
        if leer then ym:=ym-1
      end;
    OpenWindow(2,17-xm,23+ym*5,24);
    gotoxy(2,0); write('[ ');
    if Prozent then write('Prozent-');
    writeln('Tabelle ]');
    Write('  X \ Y  '); for i:=1 to ym do write(i:3,' |');
    writeln('Rest|Summe');
    write('--------'); for i:=1 to ym+2 do write('-----'); WriteLn;
    for i:=1 to xm do
      begin
        write(i:6,'  |');
        for j:=1 to ym do begin print(Messung[i,j]); write('|') end;
        print(Messung[i,0]); write('|'); print(F_X[i]); writeln;
      end; (* FOR i *)
    write('  Rest: |');
    for j:=1 to ym do begin print(Messung[0,j]); write('|') end;
    print(Messung[0,0]); write('|'); print(F_X[0]); WriteLn;
    write('--------'); for i:=1 to ym+2 do write('-----'); WriteLn;
    write(' Summe: |');
    for i:=1 to ym do begin print(F_Y[i]); write('|') end;
    print(F_Y[0]); write('|'); print(Anzahl); writeln;
    write('K,X,Y,T '); TabScr:=ScreenPtr;
    repeat
      LiesZeichen(c);
      case upcase(c) of
        'X' : SelWindow(G1Fenster);
        'Y' : SelWindow(G2Fenster);
        'T' : SelWindow(TabScr);
        'K' : Kenndaten;
      end
    until c=^[;
    CloseWindow
  end; (* Tabelle *)

  begin (* Statistik *)
    OpenWindow(1,10,37,24);
    SchreibLabels(12,35);
    OpenWindow(38,3,76,17); G2Fenster:=ScreenPtr;
    gotoxy(2,0); write('[ Y-Gruppe ]');
    OpenWindow(40,5,78,19); G1Fenster:=ScreenPtr;
    gotoxy(2,0); write('[ X-Gruppe ]');
    OpenWindow(2,3,19,10); StatWahl:=1; tScr:=ScreenPtr;
    new(Gruppe1); new(Gruppe2);
    fillchar(Gruppe1^,SizeOf(Gruppe1^),0);
    fillchar(Gruppe2^,SizeOf(Gruppe2^),0);
    fillchar(Messung,SizeOf(Messung),0);
    Anzahl:=0; EX:=0.0; EY:=0.0;
    repeat
      SelWindow(tScr);
      selekt(4,1,SizeOf(StatMenue[1]),StatMenue,6,StatWahl);
      case StatWahl of
        1 : begin
              SelWindow(G1Fenster);
              GruppenMenue(Gruppe1^)
             end;
        2 : begin
              SelWindow(G2Fenster);
              GruppenMenue(Gruppe2^)
            end;
        3 : begin
              OpenWindow(25,7,55,11); writeln;
              if Gruppe1^.Feld=0
                then begin
                       writeln('Keine Gruppendefinition');
                       WarteAufESC
                     end
                else begin
                       write('Datei wird gelesen');
                       Auswertung(Anzahl)
                     end;
              CloseWindow
            end;
        4 : Tabelle(false,Anzahl);
        5 : Tabelle(true,Anzahl);
        6 : Selektion
        ELSE if StatWahl<0 then StatWahl:=1
      end (* CASE *)
    until StatWahl=0;
    dispose(Gruppe1); dispose(Gruppe2);
    CloseWindow; CloseWindow; CloseWindow; CloseWindow
  end; (* Statistik *)

  procedure Zerlegung;
    const SelStr : string[60] = '';
    Var TempDat : DatFile;
        TempName : FileNameStr;
        fc, TempFileSize, TempSatzGroesse, TempRecZahl, i, j : integer;
        test : boolean;
        M : MaskPtr;
    begin
      TempName:='';
      LiesFileName(TempName,'DAT');
      IF (TempName='') or (TempName=DBName) then exit;
      OpenWindow(2,10,78,13); writeln('Selektion:');
      with TempMask do begin
        Art:=1; l:=60; sl:=60; x:=1; y:=2; m:='> ';
        LiesRecord(SelStr,TempMask,1,60)
      end; (* with *)
      test:=passt(Dat,SelStr,fc);
      if fc<>0
        then Fehler(7)
        else begin
               j:=0; M:=NIL;
               NeueDatei(TempName,TempDat,Maske,0,SatzGroesse,RecZahl,fc);
               for i:=1 to FileGroesse do begin
                 LiesDat(DatenFile,i,Dat,SatzGroesse,fc);
                 if passt(Dat,SelStr,fc)
                   then begin
                          j:=succ(j);
                          SchreibDat(TempDat,j,Dat,SatzGroesse,fc)
                        end;
               end;
               SchliesseDatei(TempDat,M,j,SatzGroesse,RecZahl,fc);
             end;
      CloseWindow
    end; (* Zerlegung *)

  procedure Betriebssystem;
    begin
    end; (* Betriebssystem *)

  begin (* Sonstiges *)
    ClrScr;
    SonstigesWahl:=3;
    if DateiOffen AND (FileGroesse>0) THEN Erster:=0 ELSE Erster:=2;
    repeat
      ChangeWindow(MenueFenster);
      choose(1+Erster*15,1,15,SizeOf(SonstigesMenue[1]),
             SonstigesMenue[1+Erster],5-Erster,SonstigesWahl,false);
      case Erster+SonstigesWahl of
        1 : if DateiOffen THEN Statistik else write(^G);
        2 : Zerlegung;
        3 : SystemInformation;
        4 : Betriebssystem
      end (* CASE *)
    until Erster+SonstigesWahl=5
  end; (* Sonstiges *)