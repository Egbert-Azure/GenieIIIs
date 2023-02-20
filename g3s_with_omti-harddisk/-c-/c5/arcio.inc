(* ARCIO.INC  TR 311089 *)

(* Header aus ARC-File lesen und schreiben *)

const first_io : boolean = true;

function readhdr(var hdr:headtype; var f:binfile):boolean;
var i : byte;
begin
  readhdr:=false; fillchar(hdr,sizeof(hdr),0);
  if f.feof then exit;
  if fgetc(f)=$1A then hdr.vers:=fgetc(f) else begin
    if warn then writeln(^M^J'An entry in ',arcname,' has a bad header.');
    i:=0; hdr.vers:=$FF;
    repeat
      i:=succ(i);
      if fgetc(f)=$1A then begin hdr.vers:=fgetc(f); i:=succ(i); end;
    until f.feof or (hdr.vers<=9);
    if f.feof then if first_io then begin
      writeln(arcname,' is not an archive.'); halt; end
      else exit;
    if warn then writeln(i,' Bytes skipped.');
    end;
  if hdr.vers=0 then exit;
  if f.feof then begin
    writeln('Invalid header in archive ',arcname); halt; end;
  if hdr.vers=1 then begin
    writeln('Sorry: old style not implemented.'); halt; end;
  for i:=2 to 28 do hdr.arr[i]:=fgetc(f);
  if hdr.vers>9 then begin
    writeln('I dont know how to handle file ',pstring(hdr.name),' in archive ',
            arcname,^M^J'I think you need a newer version of ARC.');
    halt; end;
  first_io:=false; readhdr:=true;
  end;

procedure writehdr(var hdr:headtype; var f:binfile);
var i : byte;
begin
  fputc(f,$1A);
  fputc(f,hdr.vers);
  if hdr.vers>0 then for i:=2 to 28 do fputc(f,hdr.arr[i]);
  end;

(* Verschluesseln bzw. Entschluesseln von Dateien *)

var crypt_point : byte;

procedure init_crypt;
begin
  crypt_point:=1;
  end;

function crypt(cod:byte):byte;
begin
  inline($3A/PASS/$B7/$2A/COD/$26/$00/$C8/$47/$7D/$ED/$5B/CRYPT_POINT/
  $16/$00/$21/PASS/$19/$AE/$6F/$26/$00/$E5/$7B/$B8/$30/$06/$3C/$32/CRYPT_POINT/
  $E1/$C9/$3E/$01/$32/CRYPT_POINT/$E1/$C9);
  end;

(* Datenblock von einer Datei zur anderen kopieren, *)
(* dabei u.U. Ver/Entschluesseln der Daten.         *)
(* CRYPT_POINT wird dabei nicht initialisiert!      *)

procedure filecopy(var source,dest:binfile; size:longword; cryptflag:boolean);
type buffer            = array[0..maxint] of byte;
var  bufptr            : ^buffer;
     bufsiz,pages,cp,i : integer;
begin
  bufsiz:=maxavail and $7F00;
  getmem(bufptr,bufsiz);
  if cryptflag then for i:=1 to size.b0 do fputc(dest,crypt(fgetc(source)))
  else for i:=1 to size.b0 do fputc(dest,fgetc(source));
  pages:=size.mid;
  while pages>0 do begin
    if pages>=hi(bufsiz) then cp:=bufsiz else cp:=pages shl 8;
    for i:=0 to pred(cp) do bufptr^[i]:=fgetc(source);
    if cryptflag then for i:=0 to pred(cp) do fputc(dest,crypt(bufptr^[i]))
    else for i:=0 to pred(cp) do fputc(dest,bufptr^[i]);
    pages:=pages-hi(cp);
    end;
  freemem(bufptr,bufsiz);
  end;

(* Direktes BinaerFile-I/O mit CRC-Berechnung. *)

var crcval   : integer;

procedure putc_dir(var f:binfile; c:byte);
begin
  addcrc(crcval,c);
  fputc(f,c);
  end;

function getc_dir(var f:binfile):byte;
var c : byte;
begin
  if f.feof then getc_dir:=0
  else begin
    c:=fgetc(f);
    addcrc(crcval,c);
    getc_dir:=c;
    end;
  end;

(* Zeichen aus Datei holen und Zaehler SIZ dekrementieren *)

var siz : longword;

function getc_decsiz(var f:binfile):byte;
begin
  getc_decsiz:=fgetc(f); dec_long(siz);
  end;

(* NCR-Packing / Unpacking. GETC_NCR liest Zeichen aus Datei und  *)
(* liefert komprimiertes (gepacktes) Ergebnisbyte.                *)
(* Flag NCREND wird nach Uebergabe des letzten NCR-Bytes gesetzt. *)
(* Zaehler NCRLEN wird bei jedem GETC_NCR-Aufruf inkrementiert.   *)
(* PUTC_NCR expandiert uebergebenes Byte in Ausgabedatei.         *)
(* GETC: Zeichenzaehler VALCOUNT wird erhoeht.                    *)

var ncrstate,ncrlast : byte;
    ncrlen           : longword;
    ncrend           : boolean;
    valcount         : array[0..256] of longword;  (* SPEOF! *)

procedure init_ncr;
begin
  ncrstate:=0; ncrlen:=long_null;
  fillchar(valcount,sizeof(valcount),0);
  end;

function getc_ncr(var f:binfile):integer;
const cc  : byte = 0;
      rep : byte = 0;
begin
  inc_long(ncrlen);
  inline($ED/$5B/F/$DD/$21/$08/$01/$DD/$19/$3A/NCRSTATE/$3D/$28/$4E/
  $3D/$28/$1A/$3D/$28/$2D/$3E/$01/$32/NCRSTATE/$DD/$E5/$D5/$CD/GETC_DIR/
  $DD/$E1/$7D/$32/NCRLAST/$FE/$90/$28/$22/$18/$0E/$3E/$01/$32/NCRSTATE/
  $2A/CC/$26/$00/$7D/$32/NCRLAST/$DD/$7E/$00/$32/NCREND/$18/$0E/$3E/$02/
  $32/NCRSTATE/$2A/REP/$26/$00/$AF/$32/NCREND/$E5/$29/$29/$11/VALCOUNT/
  $19/$E5/$CD/INC_LONG/$E1/$C9/$3A/NCRLAST/$FE/$90/$20/$09/$AF/$32/NCRSTATE/
  $21/$00/$00/$18/$CD/$06/$00/$04/$DD/$E5/$C5/$2A/F/$E5/$CD/GETC_DIR/
  $C1/$DD/$E1/$3A/NCRLAST/$BD/$20/$0A/$78/$3C/$28/$06/$DD/$CB/$00/$46/
  $28/$E2/$7D/$32/CC/$78/$32/REP/$3D/$28/$0D/$3D/$28/$10/$3E/$03/$32/NCRSTATE/
  $21/$90/$00/$18/$A7/$7D/$32/NCRLAST/$18/$8F/$3E/$02/$32/NCRSTATE/
  $2A/NCRLAST/$26/$00/$18/$95);
  end;

procedure putc_ncr(var f:binfile; c:byte);
var i : byte;
begin
  case ncrstate of
    1 : begin
          if c>0 then for i:=1 to pred(c) do putc_dir(f,ncrlast)
            else putc_dir(f,$90);
          ncrstate:=0;
          end;
{0} else if c=$90 then ncrstate:=1 else begin ncrlast:=c; putc_dir(f,ncrlast) end;
    end;
  end;
