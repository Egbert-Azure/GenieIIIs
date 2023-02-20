(* ARCSQU.INC  TR 071189 *)

const listsexist  : boolean = false;
      speof       = 256;
      notspeof    = $FEFF;
      numvals     = 257;
      maxnumnodes = 513;
      nochild     = -1;

type  listtyp     = array[0..maxnumnodes] of integer;
      listptr     = ^listtyp;

var   weight,lchild,rchild,tdepth,
      codlen,scode,idxtree         : listptr;
      numnodes,treehead,tcode      : integer;
      tables_ok                    : boolean;
      squlen                       : longword;
      bitpos                       : byte;     (* LZW too *)

(* Compare 2 (sub)trees after weight and depth.           *)
(* Result is true when first tree is greater than second. *)

function cmptrees(x,y:integer):boolean;
begin
  inline($ED/$5B/WEIGHT/$2A/X/$29/$19/$4E/$23/$46/$2A/Y/$29/$19/$7E/
  $23/$66/$6F/$ED/$42/$21/$01/$00/$D8/$2E/$00/$C0/$ED/$5B/TDEPTH/$2A/X/
  $29/$19/$4E/$23/$46/$2A/Y/$29/$19/$7E/$23/$66/$6F/$ED/$42/$21/$01/$00/
  $D8/$2E/$00/$C9);
  end;

(* HeapSort the IDXTREE in ascending order of weight and depth *)

{$a-}
procedure adjust(top,bottom:integer);
var k,temp : integer;
begin
  k:=succ(top shl 1);
  temp:=idxtree^[top];
  if k<=bottom then begin
    if (k<bottom) and cmptrees(idxtree^[k],idxtree^[succ(k)]) then k:=succ(k);
    if cmptrees(temp,idxtree^[k]) then begin
      idxtree^[top]:=idxtree^[k];
      idxtree^[k]:=temp;
      adjust(k,bottom);
      end;
    end;
  end;
{$a+}

(* Build a Huffman Tree in IDXTREE using weight and depth. *)
(* Result: Tree stored in upper half of RCHILD/LCHILD, TREEHEAD is top *)

procedure bld_tree(len:integer);
var lch,rch : integer;

  function max(a,b:byte):byte;
  begin
    if a>b then max:=a else max:=b;
    end;

begin
  treehead:=speof;
  for lch:=pred(len shr 1) downto 1 do adjust(lch,pred(len));
  while len>1 do begin
    adjust(0,pred(len));
    lch:=idxtree^[0]; rch:=idxtree^[1];
    len:=pred(len); idxtree^[1]:=idxtree^[len];
    adjust(1,pred(len));
    treehead:=succ(treehead); idxtree^[0]:=treehead;
    lchild^[treehead]:=lch; rchild^[treehead]:=rch;
    weight^[treehead]:=weight^[lch]+weight^[rch];
    tdepth^[treehead]:=succ(max(tdepth^[lch],tdepth^[rch]));
    end;
  end;

(* Build encoding table (SCODE,CODLEN) out of the tree *)

{$a-}
procedure buildenc(level,root:integer);
begin
  tcode:=tcode and rmask[level];
  if root<=speof then begin
    codlen^[root]:=level; scode^[root]:=tcode;
    if level>16 then tables_ok:=false;
    end
  else begin
    buildenc(succ(level),lchild^[root]);
    tcode:=tcode or succ(rmask[level]);
    buildenc(succ(level),rchild^[root]);
    end;
  end;
{$a+}

(* Predict size of squeezed file (into global var SQULEN) *)

procedure predict_squ;
var listlen,ceiling,divi,dum : integer;
    w                        : ^integer absolute dum;
begin
  valcount[speof]:=long_eins;
  new(scode); new(codlen); new(lchild); new(rchild);
  new(weight); new(tdepth); new(idxtree); listsexist:=true;
  for gi:=0 to speof do weight^[gi]:=long_to_integer(valcount[gi]);
  ceiling:=maxint;
  repeat
    squlen:=long_null;
    for gi:=0 to speof do add_long_int(squlen,weight^[gi]);
    divi:=succ(trunc(long_to_real(squlen)/ceiling));
    ceiling:=ceiling shr 1; listlen:=0;
    for gi:=0 to speof do begin
      dum:=addr(weight^[gi]);
      if w^<>0 then begin
        if divi>1 then if w^<=divi then w^:=1 else w^:=w^ div divi;
        tdepth^[gi]:=0;
        idxtree^[listlen]:=gi; listlen:=succ(listlen);
        end;
      end;
    bld_tree(listlen);
    tables_ok:=true; buildenc(0,treehead);
  until tables_ok;
  dispose(idxtree); dispose(tdepth); dispose(weight);
  squlen:=long_null;
  for gi:=0 to speof do
    add_long_mult(squlen,valcount[gi],codlen^[gi]);
  add_long_int(squlen,23); shift_long(squlen,-3);
  if treehead>speof then numnodes:=treehead-speof else numnodes:=0;
  add_long_int(squlen,numnodes shl 2);
  end;

(* SQUEEZE entire file using heaped tables CODE and CODELEN *)

procedure squeeze(var source,dest:binfile);
var i : integer;

  procedure putint(i:integer);
  begin
    fputc(dest,crypt(lo(i)));
    fputc(dest,crypt(hi(i)));
    end;

  procedure putnode(n:integer);
  begin
    if n<=speof then n:=not n else n:=treehead-n;
    putint(n);
    end;

  procedure outcode(cod,len:integer);
    procedure putcod;
    begin
      fputc(dest,crypt(tcode));
      end;
  begin
    inline($3A/LEN/$B7/$C8/$47/$21/TCODE/$ED/$5B/COD/$3A/BITPOS/$CB/$3A/
    $CB/$1B/$CB/$1E/$3C/$E6/$07/$20/$0B/$F5/$C5/$D5/$E5/$CD/PUTCOD/$E1/
    $D1/$C1/$F1/$10/$E8/$32/BITPOS);
    end;

begin  { squeeze }
  putint(numnodes);
  for i:=treehead downto numvals do begin
    putnode(lchild^[i]); putnode(rchild^[i]); end;
  init_ncr; ncrend:=source.feof;
  bitpos:=0;
  while not ncrend do begin
    i:=getc_ncr(source);
    outcode(scode^[i],codlen^[i]);
    end;
  outcode(scode^[speof],codlen^[speof]);
  outcode(0,7);   (* write TCODE if necessary *)
  end;

(* Nach Kompression Listen loeschen *)

procedure deinit_squ;
begin
  if listsexist then begin
    dispose(rchild); dispose(lchild); dispose(codlen); dispose(scode);
    listsexist:=false;
    end;
  end;

(* Gesamte Datei dekomprimieren *)

procedure unsqueeze(var source,dest:binfile);
type nodetyp = array[0..speof,0..1] of integer;
var  node    : ^nodetyp;
     i,curin : integer;

  procedure getint(var int:integer);
  begin
    int:=crypt(getc_decsiz(source)) + crypt(getc_decsiz(source)) shl 8;
    end;

begin
  new(node);
  getint(curin);
  if curin=0 then begin
    node^[0,0]:=notspeof;
    node^[0,1]:=notspeof;
    end
  else for i:=0 to pred(curin) do begin
    getint(node^[i,0]);
    getint(node^[i,1]);
    end;
  inline($DD/$21/SIZ/$AF/$DD/$CB/$03/$7E/$20/$5F/$11/$00/$00/$B7/$20/$15/
  $F5/$D5/$DD/$E5/$ED/$5B/SOURCE/$D5/$CD/GETC_DECSIZ/$E5/$CD/CRYPT/
  $4D/$DD/$E1/$D1/$F1/$2A/NODE/$CB/$39/$30/$02/$23/$23/$19/$19/$19/
  $19/$5E/$23/$56/$3C/$E6/$07/$CB/$7A/$28/$D1/$21/NOTSPEOF/$ED/$52/
  $28/$17/$F5/$C5/$DD/$E5/$2A/DEST/$E5/$7B/$2F/$5F/$16/$00/$D5/$CD/PUTC_NCR/
  $DD/$E1/$C1/$F1/$18/$AA/$DD/$CB/$03/$7E/$20/$09/$2A/SOURCE/$E5/$CD/
  GETC_DECSIZ/$18/$F1);
  dispose(node);
  end;
