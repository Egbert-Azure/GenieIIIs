(* ARCDIR.INC  TR 031189 *)

type fcbtype    = record
                    nam   : string[11];  (* nam[0] == drv *)
                    ext   : byte;
                    fil   : array[13..27] of byte;
                    date  : integer;     (* File Update Stamp *)
                    h,m,s : byte;        (* BDOS Function 102 *)
                    fil2  : array[33..39] of byte;
                    end;
     resulttype = record
                    usr : byte;
                    nam : array[1..11] of char;
                    fil : array[1..20] of byte;
                    end;

var  dma        : array[0..3] of resulttype;
     dirfcb     : fcbtype;

(* Filenamen aus DMA-Array holen und in String-Form wandeln *)

function get_dir(n:byte):filenam;
var fnam : filenam;
    i    : byte;
begin
  if n>3 then get_dir:='' else begin
    fnam:=dma[n].nam;
    insert('.',fnam,9);
    for i:=12 downto 1 do begin
      fnam[i]:=chr(ord(fnam[i]) and $7F);
      if fnam[i]=' ' then delete(fnam,i,1);
      end;
    get_dir:=fnam;
    end;
  end;

(* Globalen FCB "DIRFCB" aufbauen *)

procedure build_fcb(mask:filenam);
var col,drv : byte;
begin
  col:=pos(':',mask);
  if col=0 then drv:=0 else begin
    drv:=ord(mask[1])-64;
    delete(mask,1,col);
    end;
  astring(mask); dirfcb.nam:=mask;
  dirfcb.nam[0]:=chr(drv); dirfcb.ext:=0;
  end;

(* erstes File zu mitgelieferter Maske suchen *)

function first_dir(var mask:filenam):filenam;
begin
  build_fcb(mask);
  bdos(26,addr(dma));
  first_dir:=get_dir(bdos(17,addr(dirfcb)));
  end;

function next_dir:filenam;
begin
  next_dir:=get_dir(bdos(18));
  end;
