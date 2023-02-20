(*--------------------------------------------------------------------*)
(*                          Wator.pas                       14.7.1988 *)
(*-----------------------===============------------------------------*)
(* Simulation des oekologischen Systems des wasserbedeckten, torus-   *)
(* foermigen (Ring-) Planeten Wator mit seinen 1080 einzelnen Lebens- *)
(* raeumen (Zellen), in denen entweder kein Lebewesen, ein Fisch oder *)
(* ein Hai lebt. Bei ungeeigneter Wahl der oekologischen Parameter    *)
(* sterben die Haie oder sogar die Haie und die Fische aus. R.Franzen *)
(*--------------------------------------------------------------------*)

(* Zellulaerer Automat: Bitbedeutung einer Zelle:                     *)

(* 11xxxxxx xxxxxxxx : Ein Hai   lebt in der Zelle                    *)
(* 10xxxxxx xxxxxxxx : Ein Fisch lebt in der Zelle                    *)
(* 00xxxxxx xxxxxxxx : Die Zelle ist leer                             *)
(* xx111111 xxxxxxxx : max.  63 Chrononen, bis zum Hungertot          *)
(* xxxxxxxx 11111111 : max. 255 Chrononen, bis Nachwuchs folgt        *)

const  hai    = $c000;
       fisch  = $8000;
       leer   = $0000;

       haibild   = #$9D;
       fischbild = #$8C;

       anzf   : Integer = 200; (* Anzahl der Fische   *)
       brutf  : Integer =   2; (* Brutzeit der Fische *)
       anzh   : Integer =  20; (* Anzahl der Haie     *)
       bruth  : Integer =   3; (* Brutzeit der Haie   *)
       essh   : Integer =   2; (* Esszeit der Haie    *)

Type   TypeFeld   = Array[-60..1139] of Integer;

var    alt, neu   : TypeFeld;

       C          : Integer;  (* Chronoen (=Anzahl der Zeiteiheiten)  *)

       k          : Char;
       key        : Boolean;

(*--------------------------------------------------------------------*)

procedure Rahmen;

var i:Integer;

begin
  clrscr;
  gotoxy(8, 3); write (#$8A); for i:=1 to 63 do write(#$81); write (#$89);
  gotoxy(8,24); write (#$88); for i:=1 to 63 do write(#$81); write (#$87);
  for i:=4 to 23 do begin
      gotoxy( 8,i); write(#$80);
      gotoxy(72,i); write(#$80);
  end;
end;

procedure ausgabe(var alt:TypeFeld);

const fa:integer = 0;      (* Alter Fischbestand *)
      ha:integer = 0;      (* Alter Haibestand   *)

var  x,y  : integer;        (* Bildschirmkoordinaten *)
     i    : integer;        (* Array Index *)
     f,h  : integer;        (* Neuer Fisch- Haibestand *)

begin
  f:=0; h:=0;
  i:=0;
  key:=false;
  for y:=5 to 22 do begin
    gotoxy(10,y);
    for x:=1 to 60 do
      begin
        if keypressed then begin
            key:=true;
            read(kbd,k);
            NormVideo;
            exit
        end;
        case alt[i] and hai of
          fisch : begin
                    NormVideo;
                    write(fischbild);
                    f:=succ(f)
                  end;
          hai   : begin
                    NormVideo;
                    write(haibild);
                    h:=succ(h)
                  end;
          else write(' ');
        end;
      i:=succ(i);
      end;
    end;
  NormVideo;
  gotoxy(14,2); write('Chrononen:',C);
  gotoxy(32,2); write('Fische:',f,'(',fa,')  ');
  gotoxy(55,2); write('Haie:',h,'(',ha,')  ');
  fa:=f; ha:=h
end;

(*--------------------------------------------------------------------*)

procedure Neustart;

var i : integer; (* Array Index *)
    z : integer; (* Zaehler     *)

begin
  clrscr;
  writeln('Wator - Simulation eines einfachen oekologischen Systems');
  writeln('========================================================');

  writeln(#10,'Biologische Parameter eingeben:');
  fillchar(alt[-60],2400,0);
  write(#10,anzf:3, ' Anzahl   Fische ? '); readln(anzf);
  write(#10,brutf:3,' Brutzeit Fische ? '); readln(brutf);
  writeln;
  write(#10,anzh:3, ' Anzahl   Haie   ? '); readln(anzh);
  write(#10,bruth:3,' Brutzeit Haie   ? '); readln(bruth);
  write(#10,essh:3, ' Esszeit  Haie   ? '); readln(essh);
  C:=0;
  fillchar(alt[-60],2400,0);
  clrscr;
  Rahmen;
  if anzh>1080 then anzh:=1080;
  for z:=1 to anzh do
    begin
      NormVideo;
      repeat i:=random(1080) until alt[i]=0;
      gotoxy(10+i mod 60,5+i div 60);
      write(haibild);
      alt[i]:=hai or swap(succ(random(essh))) or random(bruth);
    end;
  if anzf>1080-anzh then anzf:=1080-anzh;
  for z:=1 to anzf do
    begin
      NormVideo;
      repeat i:=random(1080) until alt[i]=0;
      gotoxy(10+i mod 60,5+i div 60);
      write(fischbild);
      alt[i]:=fisch or random(brutf);
    end;
  k:=#0;
end;

(*--------------------------------------------------------------------*)

procedure   sim  (alt:TypeFeld; var neu:TypeFeld);

var      i: integer;  (* Array Laufindex           *)
         j: integer;  (* Aktuelle Zellenbesetzung  *)
         m: integer;  (* Map, gibt freie Plaetze an*)
         a: integer;  (* Anzahl der freien Plaetze *)

label    nexthai,nextfisch;

  procedure TestAltFisch(var m,a: integer);
  begin
    m:=0; a:=0;
    if (alt[succ(i)] and hai)=fisch then begin m:=1; a:=1 end;
    if (alt[  i-60 ] and hai)=fisch then begin m:=m or 2; a:=succ(a) end;
    if (alt[pred(i)] and hai)=fisch then begin m:=m or 4; a:=succ(a) end;
    if (alt[  i+60 ] and hai)=fisch then begin m:=m or 8; a:=succ(a) end
  end;

  procedure TestNeuLeer(var m,a: integer);
  begin
    m:=0; a:=0;
    if neu[succ(i)]=0 then begin m:=1; a:=1 end;
    if neu[ i-60  ]=0 then begin m:=m or 2; a:=succ(a) end;
    if neu[pred(i)]=0 then begin m:=m or 4; a:=succ(a) end;
    if neu[ i+60  ]=0 then begin m:=m or 8; a:=succ(a) end
  end;

  procedure nummer(var m,a:integer);
  (* gibt in a den zufaellig gewaehlten Ort zurueck *)
  var n:integer; (* Nummer 1,2,3 od. 4 der neuen Zelle *)
      z:integer; (* Zaehler *)
    procedure nextpos;     (* sucht Ort in Map m *)
    begin
      while not odd(m) do
        begin
          m:=m shr 1; n:=succ(n)
        end;
    end; (* nextpos *)
  begin (* nummer *)
    a:=random(a);          (* Zufallsauswahl *)
    z:=0;
    n:=1;                  (* 1=rechts,2=oben,3=links,4=unten *)
    nextpos;
    while z<a do
      begin
        m:=m shr 1;
        n:=succ(n);
        nextpos;
        z:=succ(z)
      end;
    a:=n
  end; (* nummer *)

  procedure settier(tier:integer; var a:integer);
  begin
    case a of
     1 : neu[succ(i)]:=tier;
     2 : neu[i-60]:=tier;
     3 : neu[pred(i)]:=tier;
     4 : neu[i+60]:=tier
    end
  end; (* settier *)

  procedure LoeschAltFisch(var a:integer);
  begin
    case a of
     1 : alt[succ(i)]:=leer;
     2 : alt[ i-60  ]:=leer;
     3 : alt[pred(i)]:=leer;
     4 : alt[ i+60  ]:=leer
    end
  end; (* LoeschAltFisch *)

begin (* sim *)
  move(alt[1020],alt[-60],120);
  fillchar(neu,2400,0);
                 (* ---------- Hai - Zug ------------- *)
  for i:=0 to 1079 do begin
    if (alt[i] and hai)=hai then
      begin
        if keypressed then begin key:=true; read(kbd,k); exit end;
        j:=alt[i];
        TestAltFisch(m,a);         (* m=0000..1111; a=0..4 *)
        if (hi(j) and 63)=0 then
          if a=0 then goto nexthai;                 (* verhungert *)
        if (j and 255)=0 then
          begin                                     (* Nachwuchs Hai *)
            if neu[i]=0
              then neu[i]:=hai or swap(essh) or bruth
              else
                begin
                  TestNeuLeer(m,a);
                  if a>0 then
                    begin
                      nummer(m,a);
                      settier(hai or swap(essh) or bruth,a);
                    end;
                  TestAltFisch(m,a);
                end;
            j:=(j and $ff00) or bruth (* Brutrate neu *)
          end;
        if a>0 then
          begin                       (* Fisch ist da *)
            nummer(m,a);              (* a=1..4 neue Pos *)
            settier(hai or swap(essh)or pred(j and 255),a);
            LoeschAltFisch(a);
            goto nexthai
          end;
        TestNeuLeer(m,a);             (* m=0000..1111; a=0..4 *)
        j:=hai or
           swap(pred(hi(j)and 63)) or (* Hunger verstaerken             *)
           pred(j and 255);           (* Nachwuchs-Wartezeit verkuerzen *)
      if a=0 then
        begin                         (* weder fisch noch leer *)
          if neu[i]=0 then neu[i]:=j; (* bleibt am selben Ort  *)
        end
      else                            (* leerfelder vorhanden *)
        begin
          nummer(m,a);                (* a=1..4 neue Pos *)
          settier(j,a);
        end;
    end;(* if (alt[i].. *)
    nexthai:
      if i=540 then                   (* Torus: von Oben unten an *)
        begin
          move(alt,alt[1020],240);
          move(neu,neu[1020],240)
        end;
  end; (* for i *)
  move(alt[1020],alt,240);            (* Torus: von Unten oben an *)
  move(neu[1020],neu,240);
  for i:=0 to 1079 do begin
    if (alt[i] and hai)=fisch then
      begin
        if keypressed then begin key:=true; read(kbd,k); exit end;
        j:=alt[i];
        if (j and 255)=0 then
          begin
            j:=fisch or brutf;
            if neu[i]=leer then neu[i]:=j
            else
              begin
                TestNeuLeer(m,a);
                if a>0 then
                  begin
                    nummer(m,a);
                    settier(j,a)
                  end
                else goto nextfisch;
              end;
          end; (* if (j *)
        TestNeuLeer(m,a);          (* m=0000..1111; a=0..4 *)
        j:=fisch or pred(j and 255);
        if a>0 then
          begin
            nummer(m,a);
            settier(j,a)
          end
        else
          if neu[i]=0 then neu[i]:=j;
      end; (* if (alt[i].. *)
    nextfisch:
    if i=480 then
      begin
        move(alt,alt[1020],240);
        move(neu,neu[1020],240)
      end;
  end; (* for i *)
  move(neu[1020],neu,240);
end;(* sim *)

procedure simuliere;
begin
  Rahmen;
  ausgabe(alt);
  key:=false;
  repeat
    sim(alt,neu);
    if key then exit;
    alt:=neu;
    C:=succ(C);
    ausgabe(alt);
  until key;
end;

(*--------------------------------------------------------------------*)

begin (* main *)
  Neustart;
  repeat
    clrscr;
    LowVideo;
    writeln('  Wator Simulation  ',#10,#10);
    writeln('N Neustart');
    writeln('< Ende');
    writeln('A Aktuelle Population zeigen');
    writeln('S Simulation');
    NormVideo;
    read(kbd,k);
    k:=upcase(k);
    repeat
      case k of
       'N' : Neustart;
       'A' : begin Rahmen; ausgabe(alt); read(kbd,k) end;
       'S' : simuliere;
      end;
    until pos(k,'NAS')=0;
  until k='<';
end.