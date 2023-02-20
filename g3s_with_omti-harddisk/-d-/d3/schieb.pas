(*-------------------------------------------------*)
(*              Schieb.pas                         *)
(* Bringen Sie die Buchstaben auf dem 6*4 Felder   *)
(* grossen Spielbrett in die vorgegebene           *)
(* Reihenfolge.                                    *)
(* System: CP/M plus     / aus Pascal 3'88         *)
(***************************************************)

program schiebung;

type string24 = string[24];

var ist,                  (* das Spielbrett*)
    soll : string24;      (* das Ergebnis  *)
    zug  : integer;

(*-------------------------------------------------*)

procedure invers;
begin
  LowVideo;
end;

(*-------------------------------------------------*)

procedure normal;
begin
  NormVideo;
end;

(*-------------------------------------------------*)

procedure clearscreen;
begin
  ClrScr;
end;

(*-------------------------------------------------*)

procedure posxy (x, y: integer);
begin
  gotoxy (x,y);
end;

(*-------------------------------------------------*)

function my_random (max: integer): integer;
begin
  my_random := random(max);
end;

(*-------------------------------------------------*)

procedure titelbild;

begin
  clearscreen;
  posxy (35,1);
  invers; write (' Schiebung '); normal;
  posxy (11,22); invers;
  write (' Bewegung mit den Pfeil-Tasten',
         ' --- Schiebung mit Leer-Taste ');
  posxy (33,24);
  invers; write (' Ende mit "Q" '); normal;
end;

(*-------------------------------------------------*)
(*   liefert die Strings 'Soll' und 'Ist'          *)

procedure Zufalls_String (var Zufall: String24);
var  Original : String24;
     Laenge,
     Nummer   : integer;
begin
  Original := 'ABCDEFGHIJKLMNOPQRSTUVW';
  Zufall   := '';
  repeat
    Laenge := length (Original);
    Nummer := my_random (Laenge) + 1;
    Zufall := Zufall + Original [Nummer];
    Delete (Original, Nummer, 1);
  until Laenge = 1;
  Zufall := ' ' + Zufall
end;

(*-------------------------------------------------*)
(*   Das Spielfeld mit dem 'Ist'-String            *)

procedure zeige_ist;

	(* Spielfeld-Koordinaten links oben *)
const s_offset = 7;
      z_offset = 7;
var i, j,
    Zaehler,
    Zeile, Spalte : integer;
    
begin
  Zaehler := 0;
  for i := 0 to 3 do begin
    Zeile := 3 * i +z_offset;
    for j := 0 to 5 do begin
      Spalte := 3 * j + s_offset - 1;

    (* erweiterter Zeichensatz schreibt Kaestchen *)
    (* um die einzelnen Buchstaben                *)
    (*                                            *)

      posxy (Spalte, Pred(Zeile));
      write (#138#129#137);
      posxy (Spalte, Zeile);
      write (#128#32#128);
      posxy (Spalte, Succ(Zeile));
      write (#136#129#135);
      		(* Zaehler := 6*I+1+J: *)
      Zaehler := Succ(Zaehler);
      posxy (Succ(Spalte), Zeile);
      write (Ist [Zaehler]);
    end;
  end;
end;

(*-------------------------------------------------*)

procedure zeige_soll;

const s_offset = 60;
      z_offset = 14;

var   i, j,
      Zaehler,
      Zeile, Spalte : integer;

begin
  posxy (s_offset + 5, z_offset - 2);
  invers; write (' Soll : '); normal;
  for i := 0 to 3 do
    for j := 0 to 5 do begin
      Zaehler := 6 * i + 1 +j;
      posxy (j*3 + s_offset, 2*i + z_offset);
      write (Soll [Zaehler]);
    end;
end;

(*-------------------------------------------------*)

procedure bewegung;

const s_offset = 7;
      z_offset = 7;
      leerfeld = 32;

var   taste    : char;
      x_Alt, y_Alt,
      x_Neu, y_Neu,
      Neu_Zaehler,
      Zaehler  : integer;
      Delta    : 0..1;
      links, rechts, oben, unten : boolean;

(*-------------------------------------------------*)
(*    Eingabe mit Cursortasten oder entsprechenden *)
(*    Control-Codes                                *)

procedure hol_zeichen (var Eingabe : char);
		(* liefert nur die CTRL-codes *)
		(* fuer CP/M                  *)
		
begin
  read (KBD, Eingabe);
end;

(*-------------------------------------------------*)

procedure spielstand;

var schleife,
    korrekt : integer;
begin
  zug := succ(zug);
  posxy (60,4); write ('Zuege  :  ');
  invers; write (zug:5, ' '); normal;
  korrekt := 0;
  for schleife := 1 to length(ist) do
    if ist [schleife] = soll [schleife] then
      korrekt := succ(korrekt);
  posxy (60,7); write ('Korrekt : ');
  invers; write (korrekt:5, ' '); normal;
  if korrekt = length (ist) then begin
    posxy (31,24);
    invers;
    write(' Geschafft in ',zug,' Zuegen !');
    normal;
    halt;
  end;
end;

(*-------------------------------------------------*)

procedure schreibebuchstabe (x,y : integer);

begin
  x := 3*x+s_offset;
  y := 3*y+z_offset;
  posxy (x,y); write (ist [zaehler]);
end;

(*-------------------------------------------------*)

procedure zeigerwechsel;

begin
  schreibebuchstabe (x_alt, y_alt);
  zaehler := x_neu + 1 + 6 * y_neu;
  invers; schreibebuchstabe (x_neu, y_neu);
  normal;
end;

(*-------------------------------------------------*)

begin (* Bewegung *)
Zug := -1;
x_alt := 0; y_alt := 0;
x_neu := 0; y_neu := 0;
zaehler := 6 * y_alt + 1 + x_alt;
invers; schreibebuchstabe (x_alt, y_alt);
normal;
spielstand;
repeat
  x_alt := x_neu; y_alt := y_neu;
  hol_zeichen (Taste);
  
  case ord(Taste) of
  
  4 : begin	(* Pfeil rechts *)
        delta := ord ((x_alt + 1 <= 5));
        x_neu := x_alt + delta;
        zeigerwechsel;
      end;
 19 : begin	(* Pfeil links *)
        delta := ord ((x_alt - 1 >= 0));
        x_neu := x_alt - delta;
        zeigerwechsel;
      end;
  5 : begin	(* Pfeil hoch *)
        delta := ord ((y_alt - 1 >= 0));
        y_neu := y_alt - delta;
        zeigerwechsel;
      end;
 24 : begin	(* Pfeil runter *)
        delta := ord ((y_alt + 1 <= 3));
        y_neu := y_alt + delta;
        zeigerwechsel;
      end;
 32 : begin
        zaehler := x_neu + 1 + 6 * y_neu;
        neu_zaehler := zaehler;
        (* nur wenn der Zeiger nicht im *)
        (* leeren Feld steht Kontrolle der *)
        (* zulaessigen Bewegungsrichtungen *)
        if ord (ist[zaehler]) <> leerfeld then
        begin
          oben := (zaehler - 6 > 0);
          unten := (zaehler + 6 <= 24);
          links := (pred(zaehler) mod 6<>0);
          rechts := (zaehler mod 6<>0);
          if (oben and (ord(ist[zaehler - 6])
             = leerfeld)) then
          begin
            y_neu := pred(y_alt);
            neu_zaehler := zaehler - 6;
          end
          else
          if (unten and (ord(ist[zaehler+6])
             = leerfeld)) then
          begin
            y_neu := succ(y_alt);
            neu_zaehler := zaehler + 6;
          end
          else
          if (links and (ord(ist[zaehler-1])
             = leerfeld)) then
          begin
            x_neu := pred(x_neu);
            neu_zaehler := pred(zaehler);
          end
          else
          if (rechts and (ord(ist[zaehler+1])
             = leerfeld)) then
          begin
            x_neu := succ(x_neu);
            neu_zaehler := succ(zaehler);
          end;
          if neu_zaehler <> zaehler then begin
            ist [neu_zaehler] := ist [zaehler];
            ist [zaehler] := ' ';
            schreibebuchstabe (x_alt, y_alt);
            zaehler := neu_zaehler;
            invers;
            schreibebuchstabe (x_neu, y_neu);
            normal;
            spielstand;
          end;
       end;
    end; (* CASE ' ' *)
  end; (* CASE *)
 until taste in ['Q','q'];
end;

(*-------------------------------------------------*)

begin
  titelbild;
  Zufalls_String (Ist);
  Zufalls_String (Soll);
  zeige_ist;
  zeige_soll;
  bewegung;
end.