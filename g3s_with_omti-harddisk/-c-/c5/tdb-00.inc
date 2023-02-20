(****************************************************************************)
(*          Index-Verwaltung ueber binaeren Suchbaum mit Rueckzeiger        *)
(****************************************************************************)

type BinBaum = ^Knoten;
     Knoten  = record
                 links, rechts, zurueck : BinBaum;
                 Eintrag : Index
               end;

var  IndexSize : integer;

procedure Einfuegen(var B : BinBaum; E : Index; var okay : boolean);
  (* Nicht-rekursives Einfuegen des Eintrags E in den BinBaum B *)

  procedure neu(var B_neu : BinBaum; Letzter : BinBaum);
    begin if MaxAvail>0 then
    	     if MaxAvail*MemConst<1000 then begin okay:=false; exit end;
      getmem(B_neu,IndexSize+succ(length(E.Schluessel)));
      with B_neu^ do
        begin
          links:=NIL; rechts:=NIL; zurueck:=Letzter;
          move(E,Eintrag,SizeOf(integer)+succ(length(E.Schluessel)))
        end;
    end; (* neu *)

    var p,q : BinBaum; Richtung : (l,r);
    begin (* Einfuegen *)
      p:=B; q:=p; okay:=true;
      while p<>NIL do
        with p^,Eintrag do
          begin q:=p;
            if E.Schluessel<Schluessel then begin Richtung:=l; p:=links end
                                   else begin Richtung:=r; p:=rechts end
          end;
      neu(p,q); if not okay then exit;
      if q<>NIL then if Richtung=l then q^.links:=p else q^.rechts:=p
                else B:=p
    end; (* Einfuegen *)

procedure Suche_Knoten(B : BinBaum; var E : Index; var gefunden : BinBaum);
  var fertig : boolean; p, q : BinBaum;
  begin
    p:=B; q:=B; fertig:=false;
    while not fertig do
      begin
        fertig:=p=NIL;
        if not fertig then with p^,Eintrag do
          begin
            fertig:=E.Schluessel=Schluessel;
            if not fertig then
              begin
                q:=p; if E.Schluessel<Schluessel then p:=links else p:=rechts
              end
          end
      end;
    if p<>NIL then gefunden:=p else
    if q<>NIL then gefunden:=q
    else gefunden:=NIL
  end; (* Suche_Knoten *)

function kleinster_Knoten(B : BinBaum) : BinBaum;
  var k : BinBaum;
  begin
    k:=B; while k^.links<>NIL do k:=k^.links; kleinster_Knoten:=k
  end; (* kleinster_Knoten *)

function groesster_Knoten(B : BinBaum) : BinBaum;
  var k : BinBaum;
  begin
    k:=B; while k^.rechts<>NIL do k:=k^.rechts; groesster_Knoten:=k
  end; (* groesster_Knoten *)

procedure naechster_Knoten(B : BinBaum; var k : BinBaum);
  var fertig : boolean;
  begin
    if B^.rechts<>NIL
      then k:=kleinster_Knoten(B^.rechts)
      else begin
             fertig:=false; k:=B;
             repeat
               B:=k; k:=k^.zurueck;
               if k=NIL then fertig:=true
                        else fertig:=k^.links=B
             until fertig
           end
  end; (* naechster_Knoten *)

procedure vorheriger_Knoten(B : BinBaum; var k : BinBaum);
  var fertig : boolean;
  begin
    if B^.links<>NIL
      then k:=groesster_Knoten(B^.links)
      else begin
             fertig:=false; k:=B;
             repeat
               B:=k; k:=k^.zurueck;
               if k=NIL then fertig:=true
                        else fertig:=k^.rechts=B
             until fertig
           end
  end; (* vorheriger Knoten *)

procedure loesche_Knoten(VAR B,p : BinBaum; var Erfolg : boolean);
  var q,t : BinBaum;

  procedure Loesche(var Z : BinBaum);
    begin
      freemem(Z,IndexSize+succ(length(Z^.Eintrag.Schluessel))); Z:=NIL
    end; (* Loesche *)

  procedure SetzeLetzten(var p : BinBaum; q : BinBaum);
    begin
      if p^.zurueck<>NIL then with p^.zurueck^ do
        if links=p then links:=q else rechts:=q;
      if q<>NIL then q^.zurueck:=p^.zurueck
    end; (* SetzeLetzten *)

  begin (* loesche_Knoten *)
    Erfolg:=true; if p=NIL then begin Erfolg:=false; exit end;
    with p^ do
      begin
        if links<>NIL
          then begin q:=groesster_Knoten(links); t:=q^.links end else
        if rechts<>NIL
          then begin q:=kleinster_Knoten(rechts); t:=q^.rechts end
        else begin q:=p; t:=NIL end
      end;
    if p=q then begin
                  if p=B then Loesche(B)
                         else begin SetzeLetzten(p,NIL); Loesche(p) end
                end
           else begin
                  SetzeLetzten(q,t); SetzeLetzten(p,q); if p=B then B:=q;
                  if p^.links<>NIL then p^.links^.zurueck:=q;
                  if p^.rechts<>NIL then p^.rechts^.zurueck:=q;
                  move(p^,q^,IndexSize-2); Loesche(p)
                end;
  end; (* Loesche_Knoten *)

(****************************************************************************)
(*                High-Level-Routinen zur Indexverarbeitung                 *)
(****************************************************************************)

var IndexBaum : BinBaum;
    CopyPuffer : array[0..262] of byte;

procedure Schliesse_Index(Name : FileNameStr; var f : IndexFile;
              var Anzahl : integer; IV : IndexVektor; var Okay : integer);

  procedure PutIndex(var E : Index; l,r : boolean);
    var Size, Rest : integer;
    begin
      Size:=6+length(E.Schluessel); Rest:=1; CopyPuffer[0]:=Size-6;
      CopyPuffer[1]:=ord(l); CopyPuffer[2]:=ord(r);
      move(E,CopyPuffer[3],SizeOf(integer)+succ(CopyPuffer[0]));
      if PufferZeiger+Size>MaxPuffer
        then begin
               Rest:=MaxPuffer-PufferZeiger;
               move(CopyPuffer,FilePuffer[PufferZeiger],Rest);
               blockwrite(f,FilePuffer,MaxPuffer DIV 128);
               PufferZeiger:=1
             end;
      move(CopyPuffer[pred(Rest)],FilePuffer[PufferZeiger],succ(Size-Rest));
      PufferZeiger:=PufferZeiger+succ(Size-Rest)
    end; (* PutIndex *)

  (*$A-*)
  procedure PutBaum(B : BinBaum);
    begin
      if B<>NIL then with B^ do
        begin
          PutIndex(Eintrag,links=NIL,rechts=NIL);
          PutBaum(links); PutBaum(rechts);
          freemem(B,IndexSize+succ(length(Eintrag.Schluessel))); B:=NIL
        end
      end; (* PutBaum *)
  (*$A+*)

  begin (* Schliesse_Index *)
    assign(f,Name);
    (*$I-*) rewrite(f) (*$I+*); Okay:=IOResult; if Okay<>0 then exit;
    move(IV,FilePuffer,SizeOf(IV)); PufferZeiger:=succ(SizeOf(IV));
    PutBaum(IndexBaum);
    blockwrite(f,FilePuffer,succ(PufferZeiger DIV 128));
    close(f); indiziert:=false;
  end; (* Schliesse_Index *)

procedure Oeffne_Index(Name : FileNameStr; var f : IndexFile;
              var Anzahl : integer; var IV : IndexVektor; var okay : integer);
  var FileRest, ZuLesen : integer;
      TempIndex : Index;

  procedure LiesPuffer(var Rest, Lesen : integer);
    begin
      if Rest>MaxPuffer DIV 128 then Lesen:=MaxPuffer DIV 128
                                    else Lesen:=Rest;
      Rest:=Rest-Lesen;
      blockread(f,FilePuffer,Lesen);
    end; (* LiesPuffer *)

  procedure GetIndex(var E : Index; var l,r : boolean);
    var Size, Rest : integer;
    begin
      Size:=6+FilePuffer[PufferZeiger]; Rest:=1;
      if PufferZeiger+Size>=MaxPuffer
        then begin
               Rest:=MaxPuffer-PufferZeiger;
               move(FilePuffer[PufferZeiger],CopyPuffer,Rest);
               LiesPuffer(FileRest,ZuLesen); PufferZeiger:=1
             end;
      move(FilePuffer[PufferZeiger],CopyPuffer[pred(Rest)],succ(Size-Rest));
      PufferZeiger:=PufferZeiger+succ(Size-Rest);
      l:=boolean(CopyPuffer[1]); r:=boolean(CopyPuffer[2]);
      move(CopyPuffer[3],E,sizeOf(integer)+succ(CopyPuffer[0]));
    end; (* GetIndex *)

  (*$A-*)
  procedure GetBaum(var B : BinBaum; z : BinBaum);
    var l,r : boolean;
    begin
      Anzahl:=succ(Anzahl); GetIndex(TempIndex,l,r);
      getmem(B,IndexSize+succ(length(TempIndex.Schluessel)));
      with B^,Eintrag do
        begin
          move(TempIndex,Eintrag,
               SizeOf(integer)+succ(length(TempIndex.Schluessel)));
          zurueck:=z;
          if not l then GetBaum(links,B) else links:=NIL;
          if not r then GetBaum(rechts,B) else rechts:=NIL;
        end
      end; (* GetBaum *)
  (*$A+*)

  begin (* Oeffne_Index *)
    IndexSize:=3*SizeOf(BinBaum)+SizeOf(integer);
    Anzahl:=0; fillchar(IV,SizeOf(IV),0);
    assign(f,Name); IndexBaum:=NIL;
    (*$I-*) reset(f); Okay:=IOResult;
    if Okay<>0 then exit;
    FileRest:=FileSize(f); LiesPuffer(FileRest,ZuLesen);
    move(FilePuffer,IV,SizeOf(IV)); PufferZeiger:=succ(SizeOf(IV));
    if FileGroesse>0 then GetBaum(IndexBaum,NIL);
    close(f); (*$I+*)
  end; (* Oeffne_Index *)

procedure Erzeuge_Index(Name : FileNameStr; var f : IndexFile;
              Anzahl : integer; IV : IndexVektor; var okay : integer);
  begin
    IndexSize:=3*SizeOf(BinBaum)+SizeOf(integer);
    okay:=0; IndexBaum:=NIL; (*$I-) assign(f,Name) (*$I+*); okay:=IOResult
  end; (* Erzeuge_Index *)

function IndexGleich(var X,Y : Index) : boolean;
  begin
    IndexGleich:=(X.Schluessel=Y.Schluessel) and
                 (X.DateiVerweis=Y.DateiVerweis)
  end; (* IndexGleich *)

procedure erster_Index(var f : IndexFile; var I : Index;
                       var vorhanden : integer);
  var p : BinBaum;
  begin
    p:=kleinster_Knoten(IndexBaum); vorhanden:=ord(p=NIL);
    if vorhanden=0 then I:=p^.Eintrag
  end; (* erster_Index *)

procedure letzter_Index(var f : IndexFile; var I : Index;
                        var vorhanden : integer);
  var p : BinBaum;
  begin
    p:=groesster_Knoten(IndexBaum); vorhanden:=ord(p=NIL);
    if vorhanden=0 then I:=p^.Eintrag
  end; (* letzter_Index *)

procedure vorheriger_Index(var f : IndexFile; var I : Index;
                           var vorhanden : integer);
  var p : BinBaum; g : boolean;
  begin
    vorhanden:=1; Suche_Knoten(IndexBaum,I,p); if p=NIL then exit;
    while not IndexGleich(p^.Eintrag,I) do
      begin naechster_Knoten(p,p); if p=NIL then exit end;
    vorheriger_Knoten(p,p); if p=NIL then exit;
    vorhanden:=0; I:=p^.Eintrag
  end; (* vorheriger_Index *)

procedure naechster_Index(var f : IndexFile; var I : Index;
                          var vorhanden : integer);
  var p : BinBaum;
  begin
    vorhanden:=1; Suche_Knoten(IndexBaum,I,p); if p=NIL then exit;
    while not IndexGleich(p^.Eintrag,I) do
      begin naechster_Knoten(p,p); if p=NIL then exit end;
    naechster_Knoten(p,p); if p=NIL then exit;
    vorhanden:=0; I:=p^.Eintrag
  end;  (* naechster_Index *)

procedure add_Index(var f : IndexFile; var I : Index; var okay : integer);
  var g : boolean;
  begin
    EinFuegen(IndexBaum,I,g); okay:=ord(not(g))
  end; (* add_Index *)

procedure suche_Index(VAR f : IndexFile; var I : index; var okay : integer);
  var p,q : BinBaum;
  begin
    Suche_Knoten(IndexBaum,I,p);
    if p=NIL then begin okay:=1; I.Dateiverweis:=0; exit end;
    okay:=ord(I.Schluessel<>p^.Eintrag.Schluessel);
    if I.Schluessel>p^.Eintrag.Schluessel
      then begin naechster_Knoten(p,q); if q<>NIL then p:=q end;
    I:=p^.Eintrag
  end; (* suche_Index *)

procedure loesche_Index(var f : IndexFile; var I : Index; var Result : integer);
  var okay : boolean; p : BinBaum;
  begin
    Result:=1; Suche_Knoten(IndexBaum,I,p); if p=NIL then exit;
    while not IndexGleich(p^.Eintrag,I) do
      begin naechster_Knoten(p,p); if p=NIL then exit end;
    loesche_Knoten(IndexBaum,p,okay); if not okay then exit;
    Result:=0;
  end; (* loesche_Index *)

procedure verwerfe_Index(var Name : FIleNameStr; var f : IndexFile;
                    Anzahl : integer; IV : IndexVektor; var Result : integer);
  (*$A-*)
  procedure Del(var B : BinBaum);
    begin
      if B<>NIL then
        begin
          Del(B^.links); Del(B^.rechts);
          freemem(B,IndexSize+succ(length(B^.Eintrag.Schluessel))); B:=NIL
        end
    end; (* Del *)
  (*$A+*)
  begin (* verwerfe_Index *)
    Del(IndexBaum); Result:=0
  end; (* verwerfe_Index *)