(****************************************************************************)
(*                            Dateiverwaltung                               *)
(****************************************************************************)
overlay
procedure DateiVerwaltung;
  var Erst, Anz, Wahl, SubMenueFenster : integer;
  const DateiMenue : array[1..3] of string[10] =
                       ('Erzeugen','Oeffnen','Schliessen');

(****************************************************************************)

(*                        Maskengenerator fuer TDB                          *)

(****************************************************************************)

 procedure MaskenGenerator;
  const MaxZeile = 18;
        MaxSpalte= 78;
        MaskMenue : array[1..4] of string[10] =
                    ('Editieren','Generieren','Speichern','Laden');
  type Screen       = array[1..MaxZeile,1..MaxSpalte] of char;
       Zeile        = string[MaxSpalte];

   var Bildschirm : Screen (* absolute FilePuffer *);
       ScreenName : FileNameStr;
       ScreenFile : File of Screen;
       Wahl, t    : integer;

   procedure SchreibBildschirm;
      var i,j : integer;
      begin
        gotoxy(1,1);
        for i:=1 to MaxZeile do
          for j:=1 to MaxSpalte-ord(i=MaxZeile) do
            write(Bildschirm[i,j])
      end;

   procedure LiesBildschirm(Zeile, Spalte : integer);

     const insert : boolean = false;

     var   c         : char;
           i,z       : integer;
           okay      : boolean;
           temp      : integer;

     procedure LoeschZeichen;
       begin
         for i:=Spalte+1 to MaxSpalte do
           begin
             Bildschirm[Zeile,i-1]:=Bildschirm[Zeile,i];
             gotoxy(i-1,Zeile); write(Bildschirm[Zeile,i-1])
           end;
         write(' '); Bildschirm[Zeile,MaxSpalte]:=' '
       end; (* LoeschZeichen *)

     procedure Einfuegen(x:char);
       var i : integer;
       begin
         if x=' ' then x:='_' else if x=#0  then x:=' ';
         for i:=pred(MaxSpalte) downto Spalte do
              Bildschirm[Zeile,succ(i)]:=Bildschirm[Zeile,i];
         Bildschirm[Zeile,Spalte]:=x;
         for i:=Spalte to MaxSpalte do write(BildSchirm[Zeile,i])
       end; (* Einfuegen *)

     procedure erhoehe(x:integer);
       begin
         if Spalte+x>MaxSpalte then
           begin if Zeile<MaxZeile-1 then Zeile:=succ(Zeile) else Zeile:=1;
               Spalte:=1 end else
         if Spalte+x<1  then
           begin if Zeile>1 then Zeile:=pred(Zeile) else Zeile:=MaxZeile-1;
               Spalte:=MaxSpalte end
         else Spalte:=Spalte+x
       end; (* erhoehe *)

     procedure SchreibModus(ins:boolean);
       begin
         t:=ScreenPtr; ChangeWindow(0); gotoxy(20,24);
         if ins then write('EINFUEGEN     ') else write('UEBERSCHREIBEN');
         ChangeWindow(t);
       end; (* SchreibModus *)

     procedure ZeileLoeschen;
       var z, s : integer;
       begin
         for z:=Zeile+1 to MaxZeile-1 do
           for s:=1 to MaxSpalte do Bildschirm(.z-1,s.):=Bildschirm(.z,s.);
         for s:=1 to MaxSpalte do Bildschirm(.MaxZeile,s.):=' ';
         SchreibBildschirm
       end; (* ZeileLoeschen *)

     procedure ZeileEinfuegen;
       var z, s : integer;
       begin
         for z:=MaxZeile-1 downto zeile+1 do
           for s:=1 to MaxSpalte do Bildschirm(.z,s.):=Bildschirm(.z-1,s.);
         for s:=1 to MaxSpalte do Bildschirm(.Zeile,s.):=' ';
         SchreibBildschirm
       end; (* ZeileEinfuegen *)

     begin  (* LiesBildschirm *)
       temp:=ScreenPtr; SelWindow(ArbeitsFenster);(*SelWindow*)
       ChangeWindow(0); gotoxy(1,24); write('Masken-Editor');
       gotoxy(40,24); write('Zeile/Spalte : ');
       SchreibModus(insert);
         repeat
           ChangeWindow(0); gotoxy(55,24); ClrEol; write(Zeile,'/',Spalte);
           ChangeWindow(ArbeitsFenster); gotoxy(Spalte,Zeile);
           LiesZeichen(c);
           case c of
             ^E : if Zeile>1 then Zeile:=pred(Zeile);
             ^X : if Zeile<MaxZeile-1 then Zeile:=succ(Zeile);
             ^S : erhoehe(-1);
             ^D : erhoehe(1);
             ^M : erhoehe(MaxSpalte);
             ^I : erhoehe(8-Spalte mod 8);
             ^G : LoeschZeichen;
             ^H : if Spalte>1 then
                    begin Spalte:=pred(Spalte);LoeschZeichen end;
             ^T : for i:=Spalte to MaxSpalte do
                    begin Bildschirm[Zeile,i]:=' '; write(' ') end;
             ^Y : ZeileLoeschen;
             ^A : Spalte:=1;
             ^V : begin insert:=not insert; SchreibModus(insert) end;
             ^O : Einfuegen(#0);
             ^N : ZeileEinfuegen;
             ^L : begin
                   fillchar(Bildschirm,SizeOf(Bildschirm),0);
                   Spalte:=1; Zeile:=1; ClrScr
                  end;
      #32..#255 : begin (* von #255 auf #127 geaendert *)
                    if insert then Einfuegen(c)
                              else begin
                                     if c=' ' then c:='_'; write(c);
                                     Bildschirm[Zeile,Spalte]:=c
                                   end;
                    erhoehe(1);
                  end
             end; (* case *)
         until (c=^[) or (c=^U); result:=ord(c=^U);
         ChangeWindow(0); gotoxy(1,24); ClrEol;
         ChangeWindow(ArbeitsFenster); SelWindow(temp)
                                       (*SelWindow*)
     end; (* LiesBildschirm *)

   procedure GenMaske(PASS : integer; var SatzGroesse : integer);
     var  i,j,k,l_temp    : integer;

     procedure error(x:byte);
       var t : integer;
       const FehlerMeldung : array[1..3] of string[16]=
              ('Kein Label','String zu lang','String zu kurz');
       begin
          t:=ScreenPtr; ChangeWindow(MaxScreen); OpenWindow(26,11,54,14);
          writeln('Fehler : ',FehlerMeldung[x]); WarteAufESC; CloseWindow;
          ChangeWindow(t); LiesBildschirm(i,j); RecZahl:=0; i:=1; j:=1
       end; (* error *)

     procedure erhoehe;
       begin
         if j<MaxSpalte then j:=succ(j)
                 else if i<MaxZeile then begin i:=succ(i); j:=1 end
       end; (* erhoehe *)

     begin (* GenMaske *)
       i:=1; j:=1; RecZahl:=0;
       repeat
         if Result<>0 then exit;
         if Bildschirm[i,j]=' ' then erhoehe else
           begin
             RecZahl:=succ(RecZahl);
             with TempMask do
               begin
                 m:=''; x:=j; y:=i; l_temp:=0;
                 while (not (BildSchirm[i,j] in [' ','#'])) and (j<MaxSpalte)
                   do begin m:=m+Bildschirm[i,j]; j:=succ(j) end;
                 if Bildschirm[i,j]<>'#'
                   then begin l:=0; sl:=0; Art:=0 end
                   else
                     begin
                       while Bildschirm[i,j]='#' do
                          begin erhoehe; l_temp:=succ(l_temp) end;
                      if l_temp=1 then error(3) else   (* hier stand 0 *)
                      if l_temp>127 then error(2) else (* von 255 auf 127 *)
                      if m='' then error(1)
                      else case upcase(Bildschirm[i,j]) of
                                'C' : begin Art:=0; l:=0; end;
                                'B' : begin Art:=4; l:=0 end;
                                'I' : begin Art:=2; l:=1 end;
                                'J' : begin Art:=5; l:=0 end;
                                'R' : begin
                                        Art:=3; l:=5; l_temp:=l_temp SHL 4
                                      end;
                                '.' : begin erhoehe; k:=0; l:=5; Art:=3;
                                         while Bildschirm[i,j]='#' do
                                           begin erhoehe; k:=succ(k) end;
                                         l_temp:=succ(l_temp+k) SHL 4 or k
                                      end;
                                else begin Art:=1; l:=l_temp end
                           end (* CASE *);
                           sl:=l_temp;
                           if BildSchirm[i,j]<>' ' then erhoehe;
                     end;
                 for k:=1 to ord(m[0]) do if m[k]='_' then m[k]:=' ';
                 IF PASS=2 then Maske^[RecZahl]:=TempMask
                           else SatzGroesse:=SatzGroesse+succ(l)
               end
           end
       until (i>=MaxZeile) and (j>=MaxSpalte)
     end; (* GenMaske *)

  procedure MachFile;
    var i,t : integer;
    begin
      SatzGroesse:=0; Result:=0;
      GenMaske(1,SatzGroesse); if Result<>0 then exit;
      if RecZahl=0 then begin Fehler(14); exit end;
      getmem(Maske,RecZahl*SizeOf(MaskEintr));
      GenMaske(2,SatzGroesse); DBName:=''; LiesFileName(DBName,'DAT');
      if DBName='' then exit;
      NeueDatei(DBName,DatenFile,Maske,0,SatzGroesse,RecZahl,Result);
      if Result=0 then
        SchliesseDatei(DatenFile,Maske,0,SatzGroesse,RecZahl,Result);
    end; (* MachFile *)

 procedure LiesMaske;
  var t : integer;
  begin
    LiesFileName(ScreenName,'MAS'); if ScreenName='' then exit;
    assign(ScreenFile,ScreenName);
    (*$I-*) reset(ScreenFile) (*$I+*);
    if IOResult=0 then
       begin
         t:=ScreenPtr; SelWindow(ArbeitsFenster);
                       (* SelWindow *)
         read(ScreenFile,Bildschirm); SchreibBildschirm;
         SelWindow(t); close(ScreenFile)
         (* SelWindow *)
       end else Fehler(1)
  end; (* LiesMaske *)

procedure SchreibMaske;
  begin
    LiesFileName(ScreenName,'MAS'); if ScreenName='' then exit;
    assign(ScreenFile,ScreenName);
    rewrite(ScreenFile); write(ScreenFile,Bildschirm); close(ScreenFile)
  end; (* SchreibMaske *)

begin
  t:=ScreenPtr; fillchar(Bildschirm,SizeOf(Bildschirm),' '); Wahl:=1;
  ChangeWindow(MaxScreen); ScreenName:=''; RecZahl:=0;
  OpenWindow(1+pred(MenueWahl)*13,5,2+MenueWahl*13,10);
    repeat
     selekt(2,1,SizeOf(MaskMenue[1]),MaskMenue,4,Wahl);
     case Wahl of
       1 : LiesBildschirm(1,1);
       2 : MachFile;
       3 : SchreibMaske;
       4 : LiesMaske
     end
  until Wahl<=0;
  SelWindow(ArbeitsFenster); ClrScr; CloseWindow
  (* SelWindow *)
end; (* MaskenGenerator *)

(****************************************************************************)
(*                          Oeffnen einer Datei                             *)
(****************************************************************************)
procedure DateiOeffnen;
  begin
    LiesFileName(DBName,'DAT'); if DBName='' then exit;
    OeffneDatei(DBName,DatenFile,Maske,FileGroesse,SatzGroesse,RecZahl,Result);
    DateiOffen:=Result=0; gotoxy(1,1); ClrEol;
    if not DateiOffen
      then Fehler(1)
      else begin fillchar(Dat,SatzGroesse,0); SelWindow(ArbeitsFenster);
             IndexName:=copy(DBName,1,pos('.',DBName)-1)+'.IND';
             SchreibRecord(Dat,Maske^,RecZahl); DateiZeiger:=0;
             SelWindow(SubMenueFenster); MenueWahl:=3; warte:=true
           end
  end; (* DateiOeffnen *)

(****************************************************************************)
(*                          Schliessen einer Datei                          *)
(****************************************************************************)
procedure DateiSchliessen;
  begin
    SchliesseDatei(DatenFile,Maske,FileGroesse,SatzGroesse,RecZahl,Result);
    DateiOffen:=false; if Result<>0 then Fehler(3); DateiZeiger:=0;
    (* hier stand SelWindow *)
    ChangeWindow(ArbeitsFenster); ClrScr; ChangeWindow(SubMenueFenster)
  end; (* DateiSchliessen *)

begin (* DateiVerwaltung *)
  if DateiOffen and indiziert then begin MenueWahl:=3; warte:=true; exit end;
  ChangeWindow(MaxScreen); OpenWindow(2+pred(MenueWahl)*13,3,1+MenueWahl*13,6);
  SubMenueFenster:=ScreenPtr; if DateiOffen then Wahl:=1 else Wahl:=2;
  repeat
    if not DateiOffen then begin Erst:=1; Anz:=2 end
                      else begin Erst:=3; Anz:=1 end;
    ClrScr; selekt(2,1,SizeOf(DateiMenue[1]),DateiMenue[Erst],Anz,Wahl);
    if Wahl>0 then
      case pred(Wahl+Erst) of
       1 : MaskenGenerator;
       2 : DateiOeffnen;
       3 : DateiSchliessen
      end
  until (pred(Wahl+Erst)=3) or DateiOffen or (Wahl<=0);
  CloseWindow; ZeigeFileStatus
end; (* DateiVerwaltung *)