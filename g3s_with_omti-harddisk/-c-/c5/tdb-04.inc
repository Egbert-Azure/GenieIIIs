(****************************************************************************)
(*                                 Ausgabe                                  *)
(****************************************************************************)
overlay
procedure Ausgabe;

var  Wahl, t, f, Hoehe, Breite, Block, Sum, i : integer;
     temp : anystring;
     c    : char;

const AusgabeMenue : array[1..5] of string[8] =
                     ('Selektor','Format','Device','Ausgabe','Quit');

procedure AusgabeFestlegen;
  const FormMenue : array[1..3] of string[10] =
                        ('Editieren','Laden','Speichern');
  var FormWahl : integer;
      f : file of AusgabeFormat;
      fn : FileNameStr;
      ft : integer;
  begin
    ChangeWindow(MaxScreen); OpenWindow(18,3,32,7);
    ft:=ScreenPtr; FormWahl:=1; fn:='';
    repeat
      ChangeWindow(ft); selekt(3,1,SizeOf(FormMenue[1]),FormMenue,3,FormWahl);
      case FormWahl of
        1 : begin ChangeWindow(t); LiesRecord(AusForm,FormMask,6,76) end;
        2 : begin LiesFileName(fn,'FOR');
              if fn<>'' then
                begin
                  assign(f,fn); (*$I-*) reset(f) (*$I+*);
                  if IOResult<>0
                    then Fehler(1)
                    else begin
                           read(f,AusForm); ChangeWindow(t);
                           SchreibRecord(AusForm,FormMask,8);
                           close(f)
                         end
                end
            end;
        3 : begin LiesFileName(fn,'FOR');
              if fn<>'' then
                begin
                  assign(f,fn); (*$I-*) rewrite(f) (*$I+*);
                  if IOResult<>0
                    then Fehler(3)
                    else begin write(f,AusForm); close(f) end
                end
            end;
      end (* CASE *)
    until FormWahl<=0; CloseWindow
  end; (* AusgabeFestlegen *)


procedure LiesSelektor;
  var fc : integer; okay : boolean;
  begin
    ChangeWindow(t);
    repeat
      LiesRecord(AusForm.SelString,FormMask[8],1,76);
      okay:=passt(Dat,AusForm.SelString,fc);
      if fc>0 then Fehler(7)
    until fc=0
  end; (* LiesSelektor *)


procedure DeviceBestimmen;
  const DefMenue : array[1..3] of string[10] = ('Bildschirm','Drucker','File');
  var Wahl : integer;
  begin
    ChangeWindow(MaxScreen); OpenWindow(33,3,47,7); Wahl:=1;
    selekt(2,1,11,DefMenue,3,Wahl);
    with AusForm do case Wahl of
                      1 : ZielName:='CON:';
                      2 : Zielname:='LST:';
                      3 : begin
                            ZielName:=''; LiesFileName(ZielName,'TXT');
                            if ZielName='' then ZielName:='CON:'
                          end
                    end; (* CASE *)
    CloseWindow; ChangeWindow(t); SchreibRecord(AusForm,FormMask,7)
  end; (* DeviceBestimmen *)


procedure FileAusgabe;
  label Ende;
  var LA, i, Nr, ZeilenNr, Seiten, w_temp1, w_temp2, Anzahl,
      fc, RandOben, MaxZeilen : integer;
      FF, KonsolenAusgabe, okay, absteigend : boolean;

  procedure CheckNeueSeite;
    begin
      if MaxZeilen>0 then if ZeilenNr+Block>MaxZeilen then
        begin
          if KonsolenAusgabe then begin gotoxy(40,24); WarteAufESC; ClrScr end
          else if FF then write(TextFile,#12)
          else begin
                 w_temp2:=ScreenPtr; ChangeWindow(MaxScreen);
                 OpenWindow(30,10,50,13); write('Seite wechseln!');
                 WarteAufESC; CloseWindow; ChangeWindow(w_temp2)
               end;
          ZeilenNr:=0
        end
    end; (* CheckNeueSeite *)

  procedure CheckKopfZeile;
    var i : integer;
    begin
      if MaxZeilen>0 then if ZeilenNr=0 then with AusForm do
        begin
          Seiten:=Seiten+1;
          for i:=1 to length(KopfZeile) do
            if KopfZeile[i]<>'#' then write(TextFile,KopfZeile[i])
                                 else write(TextFile,Seiten);
          for i:=1 to RandOben do writeln(TextFile);
          ZeilenNr:=RandOben
        end
    end; (* CheckKopfZeile *)

  procedure SatzAusgabe;
    var j, r , letzter, Rest : integer;
        Zeile : anystring;

    procedure NeueZeile;
      begin
        ZeilenNr:=succ(ZeilenNr); writeln(TextFile);
        letzter:=0; r:=0; Zeile:='';
      end; (* NeueZeile *)

    procedure SchreibString;
      begin
        j:=succ(j);
        while (temp[j]<>'''') and (j<LA) do
          begin write(TextFile,temp[j]); j:=succ(j); r:=succ(r) end;
      end; (* SchreibString *)

    procedure SchreibFeld;
      var Nr, Spalten : integer;
          Bund, FuellZeichen : char;

      procedure PrintLinks(var z : anystring; s : integer; c:char);
        var l : integer;
        begin
          l:=ord(z[0]); if s>l then fillchar(z[succ(l)],s-l,c);
          z[0]:=chr(s); write(TextFile,z)
        end; (* PrintLinks *)

      procedure PrintRechts(var z : anystring; s : integer; c:char);
        var l : integer;
        begin
          l:=ord(z[0]);
          if s>l
            then begin move(z[1],z[succ(s-l)],l); fillchar(z[1],s-l,c) end
            else move(z[succ(l-s)],z[1],s);
          z[0]:=chr(s); write(TextFile,z)
        end; (* PrintRechts *)

      procedure LiesZahl(var p : integer);
        begin
          p:=0;
          while (temp[j] in ['0'..'9']) and (j<LA) do
            begin p:=10*p+ord(temp[j])-48; j:=succ(j) end; j:=pred(j)
        end; (* LiesZahl *)

      begin (* SchreibFeld *)
        Bund:=' '; LiesZahl(Nr); j:=succ(j); Spalten:=1;
        if (Nr<0) or (Nr>RecZahl) then begin Fehler(8); okay:=false; exit end;
        case upcase(temp[j]) of
          'L','R' : begin Bund:=upcase(temp[j]); j:=j+1; LiesZahl(Spalten) end
          else j:=pred(j)
        end; (* CASE *)
        if Spalten=0 then begin Fehler(9); okay:=false; exit end;
        if temp[succ(j)]='.' then FuellZeichen:='.' else FuellZeichen:=' ';
        if Nr=0 then Zeile:='' else CopyRecToString(Nr,Dat,Zeile,Maske^);
        if Bund<>' '
          then begin
                 if Bund='L'
                   then PrintLinks(Zeile,Spalten-letzter,FuellZeichen)
                   else PrintRechts(Zeile,Spalten,FuellZeichen);
                 r:=0; letzter:=0; Zeile:=''
               end
          else begin write(TextFile,Zeile); r:=r+length(Zeile) end
      end; (* SchreibFeld *)

    begin (* SatzAusgabe *)
      j:=1; letzter:=0; r:=0; Zeile:=''; Rest:=0;
      while j<LA do
        begin
          if Abbruch then begin okay:=false; exit end;
          case temp[j] of
            ''''   : SchreibString;
            '/'    : NeueZeile;
            '\'    : if Zeile<>'' then NeueZeile;
            '|'    : if Zeile<>'' then NeueZeile else Rest:=succ(Rest);
            '+'    : letzter:=r;
            ','    : write(TextFile,',');
            '0'..'9' : SchreibFeld;
            'A'..'Z' : write(TextFile,chr(ord(temp(.j.))-64));
            (* statt Z auch [ moeglich - erlaubt Umlaute *)
          end; (* CASE *)
          j:=succ(j)
        end; (* WHILE *)
      while Rest>0 do begin writeln(TextFile); Rest:=pred(Rest) end
    end; (* SatzAusgabe *)

  begin (* FileAusgabe *)
    with AusForm do
      begin
        temp:=Format+#0; FF:=FormFeed; KonsolenAusgabe:=ZielName='CON:';
        MaxZeilen:=Zeilen; absteigend:=ab
      end;
    LA:=length(temp); Block:=0; Anzahl:=0; okay:=true;
    if KonsolenAusgabe
      then begin SelWindow(0); ClrScr end
      else begin
             w_temp1:=ScreenPtr; OpenWindow(30,10,50,15);
             writeln('Datei-Ausgabe!');
             writeln; write('Abbruch mit Ctrl-U')
           end;
    for i:=1 to LA do if temp[i] in ['/','\','|'] then Block:=Block+1;
    ZeilenNr:=0; Seiten:=0; RandOben:=2;
    assign(TextFile,AusForm.ZielName); rewrite(TextFile); writeln;
    if indiziert then
      if absteigend then letzter_Index(IndFile,Ind,Result)
                    else erster_Index(IndFile,Ind,Result);
    if Result<>0 then begin Fehler(6); goto Ende end;
    for i:=1 to FileGroesse do
      begin
        if Abbruch then goto Ende;
        if indiziert then Nr:=Ind.DateiVerweis else Nr:=i;
        LiesDat(DatenFile,Nr,Dat,SatzGroesse,Result);
        if passt(Dat,AusForm.SelString,fc) then
          begin
            Anzahl:=succ(Anzahl); CheckNeueSeite; CheckKopfZeile;
            SatzAusgabe; if not okay then goto Ende
          end;
        if indiziert then if i<FileGroesse then
          if absteigend then vorheriger_Index(IndFile,Ind,Result)
                        else naechster_Index(IndFile,Ind,Result);
        if Result<>0 then begin Fehler(6); goto Ende end;
      end; (* FOR *)
    if KonsolenAusgabe
         then begin gotoxy(40,24); write('Fertig! '); WarteAufESC end;
    Ende:
    close(TextFile);
    if KonsolenAusgabe
      then begin SelWindow(t); ZeigeFileStatus end
      else begin CloseWindow; ChangeWindow(w_temp1) end
  end; (* FileAusgabe *)

  begin (* Ausgabe *)
    ChangeWindow(MenueFenster); ClrScr; ChangeWindow(MaxScreen);
    Sum:=0; for i:=1 to RecZahl do if Maske^[i].sl<>0 then Sum:=succ(Sum);
    if Sum<9 then Hoehe:=Sum else Hoehe:=9;
    Breite:=20*(succ(pred(Sum) DIV 9)); if Breite>76 then Breite:=76;
    OpenWindow(2,6,3+Breite,7+Hoehe); SchreibLabels(Hoehe,Breite);
    OpenWindow(2,8+Hoehe,79,15+Hoehe); f:=ScreenPtr; gotoxy(2,0);
    write('[ Ausgabe-Format ]'); SchreibRecord(AusForm,FormMask,8);
    t:=ScreenPtr;
    Wahl:=4;
    repeat
      ChangeWindow(MenueFenster);
      Choose(2,1,15,SizeOf(AusgabeMenue[1]),AusgabeMenue,5,Wahl,false);
      case Wahl of
       1 : LiesSelektor;
       2 : AusgabeFestlegen;
       3 : DeviceBestimmen;
       4 : FileAusgabe
      end
    until Wahl=5; ChangeWindow(MaxScreen); CloseWindow; CloseWindow
  end;  (* Ausgabe *)