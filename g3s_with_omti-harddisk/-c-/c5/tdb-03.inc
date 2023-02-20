(****************************************************************************)
(*                            Bearbeiten                                    *)
(****************************************************************************)
overlay
procedure Bearbeiten;
  var Wahl, Erster, tempWindow : integer;
      veraendert    : boolean;
      temp          : anystring absolute TX;

  const UpdateMenue : array[1..7] of string[11] =
                ('Editieren','Suchen','Loeschen',
                 'Zurueck','Vorwaerts','NeuEingabe','Quit');
        KonstanteMaske : record l: byte; f : Eintrag end = (l:0);
        TempIndex : Index = (DateiVerweis:0; Schluessel:'');

   procedure Neueingabe;
    var FName  : FileNameStr;
        SubMenueWahl, SubMenueFenster : integer;

    const SubMenue : array[1..3] of string[11] =
                ('Naechster','Konstante','File');

    function leer(var rec : Eintrag; laenge : integer) : boolean;
      var i : integer;
      begin
        leer:=true; for i:=1 to laenge do
          if rec[i]<>KonstanteMaske.f[i] then begin leer:=false; exit end
      end; (* leer *)

    procedure anhaengen;
      begin
        SchreibDat(DatenFile,succ(FileGroesse),Dat,SatzGroesse,Result);
        if Result<>0 then begin Fehler(3); exit end;
        FileGroesse:=succ(FileGroesse); DateiZeiger:=FileGroesse;
        if indiziert then
          begin
            MachIndex(Dat,FileGroesse,IndexAuswahl,Ind);
            add_Index(IndFile,Ind,Result);
            if Result<>0 then Fehler(4)
          end
      end; (* anhaengen *)

    begin (* Neueingabe *)
      ChangeWindow(MaxScreen);
      OpenWindow(1+pred(Wahl)*11,3,3+Wahl*11,7); SubMenueWahl:=1;
      SubMenueFenster:=ScreenPtr;
      (* fillchar(KonstanteMaske,SatzGroesse,0); *)
      repeat
        ChangeWindow(SubMenueFenster);
        selekt(2,1,SizeOf(SubMenue[1]),SubMenue,3,SubMenueWahl);
        case SubMenueWahl of
         1 :  begin
                SelWindow(ArbeitsFenster);
                repeat
                  move(KonstanteMaske.f,Dat,SatzGroesse);
                  LiesRecord(Dat,Maske^,RecZahl,78); ZeigeFileStatus;
                  if not leer(Dat,SatzGroesse) then anhaengen
                until leer(Dat,SatzGroesse); veraendert:=true;
                SelWindow(SubMenueFenster)
              end;
         2 : begin
               SelWindow(ArbeitsFenster);
               fillchar(KonstanteMaske,SizeOf(KonstanteMaske),0);
               liesrecord(KonstanteMaske.f,Maske^,RecZahl,78);
               veraendert:=true; SelWindow(SubMenueFenster)
             end;
         3 : begin
               FName:=''; LiesFileName(FName,'TXT');
               if FName<>'' then
                 begin
                    assign(TextFile,FName); (*$I-*) reset(TextFile); (*$I+*)
                    if IOResult<>0
                      then Fehler(1)
                      else begin
                             Umleitung:=true; SelWindow(ArbeitsFenster);
                             while not SeekEOF(TextFile) and Umleitung do
                               begin
                                 move(KonstanteMaske.f,Dat,SatzGroesse);
                                 LiesRecord(Dat,Maske^,RecZahl,78);
                                 veraendert:=true; ZeigeFileStatus;
                                 anhaengen;
                               end;
                             Umleitung:=false; close(TextFile);
                           end
                 end;
               SelWindow(SubMenueFenster)
             end
         end (* CASE *)
       until (SubMenueWahl<=0) or (Result<>0);
       CloseWindow;
    end; (* NeuEingabe *)

    procedure weiter(Richtung : integer);
      begin
        if indiziert
         then case Richtung of
                -1 : vorheriger_Index(IndFile,Ind,Result);
                +1 : naechster_Index(IndFile,Ind,Result)
              end
         else case Richtung of
               -1 : if DateiZeiger>1
                      then DateiZeiger:=pred(DateiZeiger)
                      else Result:=1;
               +1 : if DateiZeiger<FileGroesse
                      then DateiZeiger:=succ(DateiZeiger)
                      else Result:=1
              end;
        if Result<>0 then begin write(^G); Result:=0 end
                     else veraendert:=true;
      end; (* weiter *)

    procedure index_suche;
      label Ende;
      var i,s,t,fc : integer;
      begin
        t:=ScreenPtr; s:=0; (*fillchar(Dat,SatzGroesse,0);  Aenderung 12.4.87*)
        for i:=1 to 10 do if IndexAuswahl[i].Feld>0 then s:=i; i:=1;
        ChangeWindow(MaxScreen); OpenWindow(10,3,60,5+s);
        repeat
          with IndexAuswahl[i] do with TempMask do
            begin
              TempMask:=Maske^[Feld]; x:=1; y:=i; l:=Anzahl; sl:=l; Art:=1;
              temp:=''; LiesRecord(temp,TempMask,1,49);
              if (i=1) and (temp='') then goto Ende;
              if i=1 then fillchar(Dat,SatzGroesse,0); (*Aenderung 12.4.87*)
              CopyStringToRec(Feld,Dat,temp,Maske^,fc);
            end;
          i:=succ(i)
        until (temp='') or (i>s);
        MachIndex(Dat,0,IndexAuswahl,Ind);
        suche_Index(IndFile,Ind,Result); veraendert:=true;
        Ende: CloseWindow; ChangeWindow(t)
      end; (* index_suche *)

    procedure data_suche;
      var Nr, t : integer;
      begin
        t:=ScreenPtr; ChangeWindow(MaxScreen); OpenWindow(10,3,40,6);
        writeln('Nummer des Eintrages:'); Nr:=0;
        with TempMask do begin Art:=2; l:=1; sl:=5; x:=10; y:=2; m:='# ' end;
        LiesRecord(Nr,TempMask,1,29);
        if (Nr>=1) and (Nr<=FileGroesse)
          then begin DateiZeiger:=Nr; veraendert:=true end;
        CloseWindow; ChangeWindow(t)
      end; (* data_suche *)

    procedure editiere;
      label Ende;
      begin
        ChangeWindow(ArbeitsFenster);
        LiesRecord(Dat,Maske^,RecZahl,78);
        if indiziert
          then begin
                 MachIndex(Dat,DateiZeiger,IndexAuswahl,TempIndex);
                 if TempIndex.Schluessel<>Ind.Schluessel then
                   begin
                     Loesche_Index(IndFile,Ind,Result);
                     if Result<>0 then begin Fehler(5); goto Ende end;
                     Ind:=TempIndex; add_Index(IndFile,Ind,Result);
                     if Result<>0 then begin Fehler(4); goto Ende end
                   end
               end;
        Ende: SchreibDat(DatenFile,DateiZeiger,Dat,SatzGroesse,Result);
        ChangeWindow(MenueFenster)
      end; (* editiere *)

    procedure loesche;
      const Frage : array[1..2] of string[10]=('nichts','loeschen');
      var   LoeschWahl, t : integer;
      begin
        t:=ScreenPtr; ChangeWindow(MaxScreen); veraendert:=true;
        OpenWindow(1+pred(Wahl)*11,3,3+Wahl*11,6); LoeschWahl:=1;
        selekt(2,1,11,Frage,2,LoeschWahl); CloseWindow; ChangeWindow(t);
        if LoeschWahl<>2 then exit;
        if DateiZeiger=FileGroesse then
          begin
            FileGroesse:=pred(FileGroesse); DateiZeiger:=FileGroesse;
            if indiziert then
              begin
                loesche_Index(IndFile,Ind,Result);
                suche_Index(IndFile,Ind,Result)
              end;
            exit
          end;
        LiesDat(DatenFile,FileGroesse,Dat,SatzGroesse,Result);
        if Result<>0 then begin Fehler(2); exit end;
        SchreibDat(DatenFile,DateiZeiger,Dat,SatzGroesse,Result);
        FileGroesse:=pred(FileGroesse);
        if Result<>0 then begin Fehler(3); exit end;
        if indiziert then
          begin
            loesche_Index(IndFile,Ind,Result);
            if Result<>0 then begin Fehler(5); exit end;
            MachIndex(Dat,succ(FileGroesse),IndexAuswahl,TempIndex);
            loesche_Index(IndFile,TempIndex,Result);
            if Result<>0 then begin Fehler(5); exit end;
            TempIndex.DateiVerweis:=DateiZeiger;
            add_Index(IndFile,TempIndex,Result); if Result<>0 then Fehler(4);
            suche_Index(IndFile,Ind,Result)
          end
      end;  (* loesche *)

    begin  (* Berabeiten *)
      ChangeWindow(MenueFenster); veraendert:=true;
      Wahl:=1;
      if indiziert
        then begin erster_Index(IndFile,Ind,Result);
               if Result<>0 then Fehler(6)
             end;
      repeat
        if FileGroesse>0
          then begin
                 if DateiZeiger=0 then DateiZeiger:=1;
                 if veraendert then
                   begin
                     if indiziert then DateiZeiger:=Ind.DateiVerweis;
                     LiesDat(DatenFile,DateiZeiger,Dat,SatzGroesse,Result);
                     veraendert:=false; if Result<>0 then exit;
                     ChangeWindow(ArbeitsFenster);
                     SchreibRecord(Dat,Maske^,RecZahl)
                   end;
                 Erster:=1;
               end
          else begin
                 ChangeWindow(ArbeitsFenster);
                 SchreibRecord(KonstanteMaske,Maske^,RecZahl);
                 Wahl:=6; Erster:=6
               end;
        ZeigeFileStatus;
        ChangeWindow(MenueFenster);
        Wahl:=succ(Wahl-Erster);
        gotoxy(1,1); for i:=1 to pred(Erster) do write('':11);
        choose(1+pred(Erster)*11,1,11,SizeOf(UpdateMenue[1]),
               UpdateMenue[Erster],8-Erster, Wahl,false);
        Wahl:=pred(Erster+Wahl);
         case Wahl of
          1 : editiere;
          2 : if indiziert then index_suche else data_suche;
          3 : loesche;
          4 : weiter(-1);
          5 : weiter(+1);
          6 : Neueingabe;
         end (* case *)
       until Wahl=7; gotoxy(1,18); ClrEol; warte:=false
     end; (* Bearbeiten *)