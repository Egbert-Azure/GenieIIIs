(* ARCLZW.INC  TR 940111 *)

const hsize      = 5003;
      firstfree  = 257;
      clearcode  = 256;
      initbits   = 9;
      maxbits    = 12;
      maxmax     = 4096;    (* = 1 shl maxbits *)
      check_gap  = 10000.0;

type  entry      = record
                     case byte of
                       0 : (neucod:byte; lastcod:integer; buf:byte);
                       1 : (dum,lzwcod : integer);
                     end;
      lzwtab     = array[0..hsize] of entry;

var   buf                      : array[0..maxbits] of byte;
      tab                      : ^lzwtab;
      scod,lzwlen              : longword;
      bytpos,codzahl           : byte;
      actbits,nextfree,maxcode : integer;
      ratio,nextcheck          : real;

(* Codebreite (9 bis 12 Bit) neu einstellen *)

procedure setbits(n:byte);
begin
  actbits:=n;
  if n=maxbits then maxcode:=maxmax else maxcode:=rmask[n];
  bitpos:=0; bytpos:=0; codzahl:=0;
  end;

(* Einzelnen 9/12-Bit-Code an LZW-Datei ausgeben *)

procedure putcode(var f:binfile; code:integer);

  procedure boost(n:byte);
  var i : byte;
  begin
    for i:=0 to pred(n) do fputc(f,buf[i]);
    add_long_int(lzwlen,n);
    bitpos:=0; bytpos:=0;
    end;

  procedure setbits_put(n:byte);
  begin
    if bytpos>0 then boost(actbits);
    setbits(n);
    end;

begin
  if code>=0 then begin
    inline($2A/CODE/$1E/$00/$3A/BITPOS/$B7/$28/$06/$47/$29/$CB/$13/$10/$FB/
    $4F/$06/$00/$DD/$21/RMASK/$DD/$09/$DD/$09/$DD/$7E/$00/$ED/$4B/BYTPOS/
    $06/$00/$DD/$21/BUF/$DD/$09/$DD/$A6/$00/$B5/$DD/$77/$00/$DD/$74/$01/
    $2A/ACTBITS/$3A/BITPOS/$85/$D6/$08/$0C/$FE/$08/$38/$06/$D6/$08/$0C/
    $DD/$73/$02/$32/BITPOS/$79/$32/BYTPOS);
    if bytpos>=actbits then boost(actbits);
    if code=clearcode then setbits_put(initbits)
    else if nextfree>maxcode then setbits_put(succ(actbits));
    end
  else boost(bytpos+byte(bitpos>0));
  end;

(* gesamte Datei komprimierend in Archiv uebernehmen *)

procedure crunch_lzw(var source,dest:binfile);
var inbytes         : longword;
    last,neu,i,disp : integer;
    fnd             : boolean;

  procedure checkratio;
  var rat : real;
  begin
    nextcheck:=nextcheck+check_gap;
    rat:=long_to_real(inbytes)/long_to_real(lzwlen);
    if ratio<rat then ratio:=rat else begin
      ratio:=0.0;
      fillchar(tab^,sizeof(lzwtab),$FF);
      nextfree:=firstfree;
      putcode(dest,clearcode);
      end;
    end;

begin
  lzwlen:=long_eins;
  if source.feof then exit;
  ratio:=0.0; nextcheck:=check_gap;
  setbits(initbits);
  nextfree:=firstfree;
  new(tab); fillchar(tab^,sizeof(lzwtab),$FF);
  fputc(dest,maxbits);
  last:=getc_ncr(source); inbytes:=long_eins;
  while not ncrend do begin
    neu:=getc_ncr(source);
    inc_long(inbytes);
    inline($2A/NEU/$26/$00/$29/$29/$29/$29/$ED/$5B/LAST/$7C/$AA/$67/
    $7D/$AB/$6F/$22/I/$B4/$21/$01/$00/$28/$09/$21/$8B/$13/$ED/$5B/I/
    $ED/$52/$22/DISP/$2A/I/$29/$29/$ED/$5B/TAB/$19/$E5/$3A/NEU/$BE/$20/$2B/
    $ED/$4B/LAST/$23/$79/$BE/$20/$22/$23/$7E/$E6/$0F/$B8/$20/$1B/$7E/
    $23/$66/$CB/$3C/$1F/$CB/$3C/$1F/$CB/$3C/$1F/$CB/$3C/$1F/$6F/$22/LAST/
    $E1/$3E/$01/$32/FND/$18/$24/$AF/$32/FND/$E1/$7E/$23/$A6/$23/$A6/
    $23/$A6/$3C/$28/$15/$2A/I/$ED/$5B/DISP/$B7/$ED/$52/$30/$04/$11/$8B/$13/
    $19/$22/I/$18/$A0);
    if not fnd then begin
      putcode(dest,last);
      if nextfree<maxmax then begin
        tab^[i].neucod:=neu;
        tab^[i].lastcod:=last;
        tab^[i].lzwcod:=tab^[i].lzwcod and $F + (nextfree shl 4);
        nextfree:=succ(nextfree);
        end
      else if long_to_real(inbytes)>=nextcheck then checkratio;
      last:=neu;
      end;
    end;
  putcode(dest,last);
  putcode(dest,-1);
  dispose(tab);
  end;

(* Einzelnen 9/12-Bit-Code aus LZW-Datei holen *)

function getcode(var f:binfile):integer;
begin
  if codzahl=0 then begin
    bytpos:=0; bitpos:=0;
    while (siz.hi>=0) and (codzahl<actbits) do begin
      buf[codzahl]:=crypt(getc_decsiz(f));
      codzahl:=succ(codzahl);
      end;
    if codzahl<2 then begin getcode:=-1; exit; end;
    codzahl:=(codzahl shl 3) div actbits;
    end;
  scod.b0:=buf[bytpos];
  bytpos:=succ(bytpos);
  scod.b1:=buf[bytpos];
  scod.b2:=buf[succ(bytpos)];
  shift_long(scod,-bitpos);
  getcode:=scod.lo and rmask[actbits];
  bitpos:=bitpos+actbits-8;
  if bitpos>7 then begin bitpos:=bitpos-8; bytpos:=succ(bytpos); end;
  codzahl:=pred(codzahl);
  end;

(* komplette LZW-Datei dekomprimieren *)

procedure uncrunch_lzw(var source,dest:binfile);
label StartAgain;
var code,last,sp,c : integer;

  procedure put_tab(x:byte);
  begin
    putc_ncr(dest,x);
    if nextfree<maxmax then begin
      tab^[nextfree].lastcod:=last;
      tab^[nextfree].neucod:=x;
      nextfree:=succ(nextfree);
      end;
    end;

begin
  code:=crypt(getc_decsiz(source));
  if code<>maxbits then begin
    writeln('File packed with ',code,' bits, I can only handle ',maxbits);
    halt; end;
  new(tab);
StartAgain:
  setbits(initbits); nextfree:=firstfree;
  last:=getcode(source);
  if last=-1 then begin dispose(tab); exit; end;
  putc_ncr(dest,last);
  repeat
    if nextfree>maxcode then setbits(succ(actbits));
    code:=getcode(source);
    if code>=0 then begin
      if code=clearcode then goto StartAgain
      else begin
        if code<firstfree then put_tab(code) else begin
          sp:=0;
          if code<nextfree then c:=code else begin
            tab^[sp].buf:=c;     (* letzter Anfangscode! *)
            sp:=succ(sp);
            c:=last;
            end;
          while c>=firstfree do begin
            tab^[sp].buf:=tab^[c].neucod;
            c:=tab^[c].lastcod;
            sp:=succ(sp);
            end;
          put_tab(c);
          while sp>0 do begin
            sp:=pred(sp);
            putc_ncr(dest,tab^[sp].buf);
            end;
          end;
        last:=code;
        end;
      end;
  until code<0;
  dispose(tab);
  end;
