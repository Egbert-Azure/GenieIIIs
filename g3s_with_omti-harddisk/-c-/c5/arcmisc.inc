(* ARCMISC.INC TR 120192 *)

const rmask : array[0..16] of integer
            = ($0,$1,$3,$7,$F,$1F,$3F,$7F,$FF,
               $1FF,$3FF,$7FF,$FFF,$1FFF,$3FFF,$7FFF,$FFFF);

(* Basisoperationen mit 32-Bit-Variablen (Longword) *)

procedure inc_long(var lw:longword);
begin
  inline($2A/LW/$06/$03/$34/$C0/$23/$10/$FB/$34);
  end;

procedure dec_long(var lw:longword);
begin
  inline($2A/LW/$06/$03/$35/$7E/$3C/$C0/$23/$10/$F9/$35);
  end;

procedure add_long(var lw1,lw2:longword);
begin
  inline($2A/LW1/$ED/$5B/LW2/$B7/$06/$04/$1A/$8E/$77/$23/$13/$10/$F9);
  end;

procedure add_long_int(var lw:longword; n:integer);
begin                  (* nur fuer positive N ! *)
  inline($2A/LW/$ED/$5B/N/$7E/$83/$77/$23/$7E/$8A/$77/$D0/$23/$34/
  $C0/$23/$34);
  end;

(* Produkt aus Langwort und Byte zu Langwort addieren *)

procedure add_long_mult(var sum,lw:longword; m:byte);
begin
  inline($DD/$2A/SUM/$2A/LW/$5E/$23/$56/$23/$D5/$5E/$23/$56/$E1/$3A/M/
  $CB/$3F/$30/$1E/$F5/$DD/$7E/$00/$85/$DD/$77/$00/$DD/$7E/$01/$8C/
  $DD/$77/$01/$DD/$7E/$02/$8B/$DD/$77/$02/$DD/$7E/$03/$8A/$DD/$77/$03/
  $F1/$C8/$CB/$25/$CB/$14/$CB/$13/$CB/$12/$18/$D3);
  end;

(* Langwort um 0..+-31 Bit schieben (pos=links, neg=rechts) *)

procedure shift_long(var lw:longword; n:integer);
begin
  inline($DD/$2A/LW/$DD/$6E/$00/$DD/$66/$01/$DD/$5E/$02/$DD/$56/$03/
  $ED/$4B/N/$78/$B1/$C8/$CB/$78/$20/$16/$04/$05/$20/$29/$79/$E6/$E0/
  $20/$24/$41/$CB/$25/$CB/$14/$CB/$13/$CB/$12/$10/$F6/$18/$1D/$04/
  $20/$14/$79/$ED/$44/$47/$E6/$E0/$20/$0C/$CB/$3A/$CB/$1B/$CB/$1C/
  $CB/$1D/$10/$F6/$18/$06/$11/$00/$00/$21/$00/$00/$DD/$72/$03/$DD/$73/$02/
  $DD/$74/$01/$DD/$75/$00);
  end;

(* Langwort in REAL bzw. Integer wandeln *)

function long_to_real(var lw:longword):real;
begin
  inline($2A/LW/$5E/$23/$56/$23/$4E/$23/$46/$21/$00/$00/$78/$B1/$B2/
  $B3/$C8/$2E/$A0/$CB/$78/$20/$0B/$CB/$23/$CB/$12/$CB/$11/$CB/$10/
  $2D/$18/$F1/$CB/$B8/$C9);
  end;

function long_to_integer(var lw:longword):integer;
begin
  inline($2A/LW/$5E/$23/$56/$CB/$7A/$20/$08/$23/$7E/$23/$B6/$20/$02/
  $EB/$C9/$21/$FF/$7F/$C9);
  end;

(* String von C- nach Pascal-Konvention wandeln *)

function pstring(st:filenam):filenam;
begin
  inline($21/ST/$06/$FF/$23/$04/$7E/$B7/$20/$FA/$78/$32/ST);
  pstring:=st;
  end;

(* Dateiname von String- in Array-Form wandeln *)

procedure astring(var st:filenam);
var help : filenam;
begin
  inline($21/HELP/$06/$0B/$70/$23/$36/$20/$10/$FB/$2A/ST/$7E/$B7/$28/$37/
  $47/$EB/$21/HELP/$23/$0E/$01/$13/$1A/$FE/$2E/$28/$11/$FE/$2A/$28/$16/
  $FE/$61/$38/$02/$E6/$5F/$77/$23/$0C/$10/$EB/$18/$18/$79/$FE/$09/
  $30/$F7/$23/$0C/$18/$F7/$36/$3F/$23/$0C/$79/$FE/$0C/$30/$EA/$FE/$09/
  $28/$E6/$18/$F1/$21/HELP/$ED/$5B/ST/$01/$0C/$00/$ED/$B0);
  end;

(* Dateinamen und Maske (beide String) auf Uebereinstimmung pruefen *)

function match(s,mask:filenam):boolean;
begin
  astring(s); astring(mask);
  inline($21/S/$11/MASK/$06/$0B/$23/$13/$1A/$FE/$3F/$28/$03/$BE/$20/$02/
  $10/$F4/$21/$00/$00/$C0/$2C/$C9);
  end;

(* Extension an Dateinamen anfuegen *)

procedure makefnam(nam:filenam; ext:str4; var erg:filenam);
var col : byte;
begin
  col:=pos('.',nam);
  if col>0 then nam[0]:=chr(pred(col));
  erg:=nam+ext;
  end;

(* Integer-Wert hexadezimal ausgeben *)

procedure writehex(n:integer);
begin
  inline($CD/$04AF);
  end;

(* Umrechnungen fuer verschiedene Datum- und Uhrzeitformate *)

var cpmdat  : array[0..4] of byte;   (* CP/M-Plus Format *)
    datum   : array[0..2] of byte;   (* Tag,Monat,Jahr *)
    uhrzeit : array[0..2] of byte;   (* Stunde,Minute,Sekunde *)

(* CP/M-Plus Format (jul. Datum / BCD) in Binaer-Arrays 2x 3 Byte *)

procedure cpm_date(var inarray,datout,timout);
begin
  inline($DD/$2A/INARRAY/$FD/$2A/DATOUT/$DD/$6E/$00/$DD/$66/$01/$11/$6D/$01/
  $06/$22/$3E/$4E/$CB/$00/$ED/$52/$38/$05/$28/$03/$3C/$18/$F5/$CB/$08/
  $ED/$5A/$FD/$77/$02/$CB/$78/$11/*+58/$28/$03/$11/*+65/$3E/$01/$06/$00/
  $EB/$4E/$23/$EB/$B7/$ED/$42/$38/$05/$28/$03/$3C/$18/$F2/$09/$FD/$77/$01/
  $FD/$75/$00/$2A/TIMOUT/$06/$03/$DD/$56/$02/$7A/$E6/$0F/$5F/$7A/$93/
  $0F/$0F/$82/$83/$1F/$77/$DD/$23/$23/$10/$EC/$C9/$1F/$1C/$1F/$1E/
  $1F/$1E/$1F/$1F/$1E/$1F/$1E/$1F/$1F/$1D/$1F/$1E/$1F/$1E/$1F/$1F/
  $1E/$1F/$1E/$1F);
  end;

(* Binaeres Array 3 Byte in MS-DOS Format Datum (Integer) *)

function calc_date(var dat):integer;
begin
  inline($ED/$5B/DAT/$13/$13/$1A/$D6/$50/$30/$01/$AF/$67/$1B/$1A/$E6/$0F/
  $07/$07/$07/$07/$6F/$29/$1B/$1A/$E6/$1F/$B5/$6F/$C9);
  end;

(* Binaeres Array 3 Byte in MS-DOS Format Uhrzeit (Integer) *)

function calc_time(var zeit):integer;
begin
  inline($ED/$5B/ZEIT/$1A/$67/$13/$1A/$E6/$3F/$07/$07/$6F/$29/$29/
  $29/$13/$1A/$0F/$E6/$1F/$B5/$6F/$C9);
  end;

(* MS-DOS Format Datum und Uhrzeit in BinArray 6 Byte *)

procedure dos_date(date,time:integer; var outarray);
begin
  inline($DD/$2A/OUTARRAY/$2A/DATE/$7D/$E6/$1F/$DD/$77/$00/$06/$05/
  $CD/*+54/$7D/$E6/$0F/$DD/$77/$01/$06/$04/$CD/*+43/$7D/$E6/$7F/$C6/$50/
  $DD/$77/$02/$2A/TIME/$7D/$E6/$1F/$87/$DD/$77/$05/$06/$05/$CD/*+20/
  $7D/$E6/$3F/$DD/$77/$04/$06/$06/$CD/*+9/$7D/$E6/$1F/$DD/$77/$03/
  $C9/$CB/$3C/$CB/$1D/$10/$FA/$C9);
  end;

(* BinArray 6 Byte in CP/M-Plus Datum-Integer und BCD-Array 3 Byte *)

procedure calc_dt_cpm(var InOutArray);
begin
  inline($DD/$2A/INOUTARRAY/$DD/$7E/$02/$21/$00/$00/$11/$6D/$01/$06/$22/
  $D6/$4E/$28/$07/$CB/$00/$ED/$5A/$3D/$18/$F7/$DD/$7E/$01/$CB/$78/
  $11/*+60/$28/$03/$11/*+67/$06/$00/$EB/$4E/$23/$EB/$09/$3D/$20/$F8/
  $ED/$42/$DD/$4E/$00/$09/$DD/$75/$00/$DD/$74/$01/$06/$03/$DD/$7E/$03/
  $0E/$FF/$0C/$D6/$0A/$30/$FB/$C6/$0A/$CB/$21/$CB/$21/$CB/$21/$CB/$21/
  $B1/$DD/$77/$02/$DD/$23/$10/$E4/$C9/$1F/$1C/$1F/$1E/$1F/$1E/$1F/$1F/
  $1E/$1F/$1E/$1F/$1F/$1D/$1F/$1E/$1F/$1E/$1F/$1F/$1E/$1F/$1E/$1F);
  end;

(* Datum und Uhrzeit ermitteln und in ARCDATE/ARCTIME ablegen *)

procedure get_global_datetime;
var eingabe : filenam;

  procedure datestr(var output; default:byte);  (* ASCII --> BinArray *)
  begin
    inline($AF/$11/EINGABE/$13/$2A/EINGABE/$67/$19/$77/$DD/$2A/OUTPUT/
    $DD/$77/$00/$DD/$77/$01/$DD/$77/$02/$EB/$1E/$03/$0E/$00/$7E/$23/
    $B7/$28/$19/$D6/$30/$FE/$0A/$30/$0A/$47/$79/$87/$87/$81/$87/$80/
    $4F/$18/$EB/$DD/$71/$00/$DD/$23/$1D/$20/$E1/$C9/$DD/$71/$00/$2A/OUTPUT/
    $7E/$B7/$C8/$ED/$4B/DEFAULT/$06/$03/$7E/$B7/$20/$01/$71/$23/$10/$F8);
    end;

begin
  if time then begin                            (* CP/M 3 *)
    cpmdat[4]:=bdos(105,addr(cpmdat));
    cpm_date(cpmdat,datum,uhrzeit);
    end
  else begin                                    (* CP/M 2 oder Switch T *)
    write('Enter date (dd.mm.yy) : '); readln(eingabe);
    datestr(datum,1);
    if length(eingabe)>0 then begin
      write('Enter time (hh:mm:ss) : '); readln(eingabe);
      end;
    datestr(uhrzeit,0);
    end;
  arcdate:=calc_date(datum);
  arctime:=calc_time(uhrzeit);
  end;

(* Parameter aus CP/M-3 SCB holen *)

function GetSCB(index:byte):integer;
const SCBPB : array[0..1] of byte = (0,0);
begin
  SCBPB[0]:=index;
  GetSCB:=BDOSHL(49,addr(SCBPB));
  end;

(* Meldung ueber unbearbeitete Parameter *)

procedure note_notfound;
begin
  if note then for gi:=1 to pcount do
    if not did[gi] then writeln('File not found: ',arg[gi]);
  end;
