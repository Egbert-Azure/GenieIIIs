(****************************************************************************)
(*                               Utilities                                  *)
(****************************************************************************)

type DefaultExtension = string[3];
var  TX, TY : anystring;   (* Gemeinsam benutzter Speicherplatz *)

 procedure MachGross(var X);  (* Uebersetzt einen String in Grossbuchstaben *)
  var TS : anystring absolute X;
      i  : integer;
    begin
      for i:=1 to length(TS) do TS[i]:=upcase(TS[i])
    end; (* MachGross *)

 procedure ZeigeFileStatus;   (* Zeigt Filenamen, -groesse etc. in der
                                letzten Zeile an *)
  label exit;
  var temp : integer;
  begin
    temp:=ScreenPtr; ChangeWindow(0);
    gotoxy(1,24); if not DateiOffen then goto exit;
    LowVideo; write('Datei: '); normvideo; write(DBName);
    LowVideo; write(' Eintraege: '); normvideo; write(FileGroesse);
    if DateiZeiger>0 then
       begin lowvideo; write(' #'); normvideo; write(DateiZeiger) end;
    if indiziert then write(' INDEX');
    lowvideo; write(' Speicher: '); normvideo; write(MemConst*MemAvail:1:0);
    exit: ClrEol; ChangeWindow(temp)
  end;  (* ZeigeFileStatus *)

 procedure LiesFileName(var fn : FileNameStr; ext : DefaultExtension);
  var temp : integer;
  begin
    temp:=ScreenPtr; OpenWindow(10,10,70,13);
    write('Name der Datei (.',ext,'):');
    with TempMask do
      begin
        x:=5; y:=2; l:=pred(SizeOf(FileNameStr)); m:=''; sl:=l; Art:=1;
        LiesRecord(fn,TempMask,1,59)
      end;
    MachGross(fn);
    if fn<>'' then if pos('.',fn)=0 then fn:=fn+'.'+ext;
    CloseWindow; ChangeWindow(temp)
  end; (* LiesFileName *)

 procedure SchreibLabels(Hoehe, Breite : integer);
  var i,ml,b,n : integer;
  begin
    n:=0; for i:=1 to RecZahl do if Maske^[i].sl<>0 then n:=succ(n);
    gotoxy(2,0); write('[ Die Labels ]');
    b:=Breite DIV succ(pred(n) DIV Hoehe)-5; n:=0;
    for i:=0 to pred(RecZahl) do
      begin
        gotoxy(1+(b+4)*(n div Hoehe),succ(n mod Hoehe));
        with Maske^[succ(i)] do if sl<>0 then
          begin
            ml:=length(m); while m[ml] in [' ',':','.'] do ml:=pred(ml);
            if ml>b then ml:=b; LowVideo; write(succ(i):2,'> ');
            NormVideo; write(copy(m,1,ml)); n:=succ(n)
          end
      end
  end; (* SchreibLabels *)

 procedure WarteAufESC;
  var c : char;
  begin
    write('Bitte <ESC> druecken! ');
    repeat LiesZeichen(c) until c=#27
  end; (* WarteAufESC *)

 procedure Fehler(code : integer);
  var t : integer;
  begin
    t:=ScreenPtr; ChangeWindow(MaxScreen); OpenWindow(20,11,60,15);
    writeln('Fehler ',code);
    case code of
     1 : writeln('Datei nicht gefunden');
     2 : writeln('Lesefehler');
     3 : writeln('Schreibfehler');
     4 : writeln('Fehler bei Indexerstellung');
     5 : writeln('Fehler bei Indexloeschung');
     6 : writeln('Index nicht vorhanden');
     7 : writeln('Syntax-Fehler (Selektieren)');
     8 : writeln('Illegales Datenfeld (Format)');
     9 : writeln('Synatx-Fehler (Format)');
     10: writeln('Fehler beim Dateischliessen');
     11: writeln('Indexschluessel zu gross');
     12: writeln('Fehler beim Indexladen');
     13: writeln('Index passt nicht zur Datei');
     14: writeln('Kein Datenfeld')
    end; (* CASE *)
    WarteAufESC; CloseWindow; ChangeWindow(t)
  end;

 function passt(var Dat, SelStr; var fc : integer):boolean;
  const Ziffern : SET OF '0'..'9' = ['0'..'9'];
        EndeZeichen : SET OF CHAR = [''''];

  VAR S : string[127] ABSOLUTE SelStr;
      L, P : INTEGER;

   procedure inc;
    begin
      P:=succ(P); if P>L THEN fc:=2;
    end (* inc *);

   procedure Skip;
    begin
      while (fc=0) AND (S[P]=' ') do inc
    end; (* Skip *)

   (*$A-*)
   function Ausdruck : boolean; var temp : boolean;
     function Term : boolean; var temp : boolean;
       function Faktor : boolean; VAR vz : boolean;
         function Atom : boolean;
          var Nr : integer; o, e : char; ry : real;
          begin
            Nr:=0; Skip; if (fc>0) OR (P=L) then exit;
            while S[P] in Ziffern do begin Nr:=10*Nr+ord(S[P])-48; inc end;
            if (Nr=0) or (Nr>RecZahl) then begin fc:=1; exit end else Skip;
            if fc>0 then exit;
            o:=S[P]; inc; Skip; if fc>0 then exit;
            if S[P]='''' then begin EndeZeichen:=['''',#0]; INC END
                         else EndeZeichen:=[#0,' ','(',')','&','~','\'];
            TY:='';
            while (fc=0) AND NOT(S[P] IN EndeZeichen) do
              begin TY:=TY+S[P]; inc end;
            IF S[P]='''' THEN inc;
            CopyRecToString(Nr,Dat,TX,Maske^);
            if Maske^[Nr].Art in [2..4]
              then begin
                     CopyStringToRec(1,ry,TY,Maske^[Nr],fc);
                     if fc>0 then exit;
                     CopyRecToString(1,ry,TY,Maske^[Nr])
                   end;
            case o of
              '>' : Atom:=TX>TY;
              '<' : Atom:=TX<TY;
              '=' : Atom:=TX=TY;
              '|' : Atom:=pos(TY,TX)>0
              else fc:=1
            end
          end; (* Atom *)

        begin (* Faktor *)
          Skip; if (fc>0) OR (P=L) then exit;
          if S[P]='~' THEN BEGIN vz:=TRUE; inc END ELSE vz:=FALSE;
          if S[P]='(' THEN BEGIN
                            inc;
                            if vz THEN Faktor:=NOT(Ausdruck)
                                  ELSE Faktor:=Ausdruck;
                            Skip;
                            IF S[P]<>')' THEN fc:=2 ELSE inc
                           END
                      ELSE if vz THEN Faktor:=NOT(Atom)
                                 ELSE Faktor:=Atom
        end (* Faktor *);

      begin (* Term *)
        if fc>0 then exit;
        temp:=faktor; Skip;
        if (fc>0) OR (P=L) THEN BEGIN Term:=temp; EXIT END;
        while (fc=0) and (S[P]='&') do begin
          inc; temp:=temp AND faktor; Skip
        end;
        Term:=temp
      end (* Term *);

    begin (* Ausdruck *)
      if fc>0 then exit;
      temp:=Term; Skip;
      if (fc>0) OR (P=L) then begin Ausdruck:=temp; exit end;
      while (fc=0) and (S[P]='\') do begin
        inc; temp:=temp OR Term; Skip end;
      Ausdruck:=temp
    end (* Ausdruck *);
  (*$A+*)
  begin (* passt *)
    fc:=0; if S='' then begin passt:=true; exit end;
    S:=S+#0; L:=ord(S[0]);
    P:=1; passt:=Ausdruck; S[0]:=chr(L-1);
  end; (* passt *)

 procedure MachIndex(var Dat; Nr : integer; var Auswahl : IndexVektor;
                    var Ind : Index);
  var i,j,Sum : integer; temp : anystring absolute TX;
  begin
    with Ind do
      begin
        fillchar(Schluessel,SizeOf(Schluessel),32); Sum:=0;
        for i:=1 to 10 do with Auswahl[i] do
          if Feld<>0 then
            begin
              CopyRecToString(Feld,Dat,temp,Maske^);
              if Maske^[Feld].Art in [2,3] then
                if pos('-',temp)>0 then
                  for j:=1 to ord(temp[0]) do temp[j]:=chr(48-ord(temp[j]));
              move(temp[1],Schluessel[succ(Sum)],length(temp));
              if Auswahl[succ(i)].Feld=0
                          then Sum:=Sum+length(temp)
                          else Sum:=Sum+Anzahl
            end;
        Schluessel[0]:=chr(Sum); DateiVerweis:=Nr; MachGross(Schluessel)
      end
  end; (* MachIndex *)