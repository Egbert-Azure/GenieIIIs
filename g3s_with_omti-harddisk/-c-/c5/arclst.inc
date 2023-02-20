(* ARCLST.INC TR 940111 *)

(* Ausgabe aller enthaltenen Dateien *)

procedure lstarc;
var  n           : byte;
     dsksiz,tdsk,
     tnum,crcsum : integer;
     tlen,tsize  : longword;
     rlen,rsiz   : real;

  (* Angaben zu einzelnem Eintrag in UNARC-Format ausgeben *)

  procedure lstfile(var hdr:headtype);
  const mon : array[1..12,1..3] of char
            = ('Jan','Feb','Mar','Apr','May','Jun',
               'Jul','Aug','Sep','Oct','Nov','Dec');
  var   sav,i   : byte;
        dt      : array[0..5] of byte;
        nam     : filenam;
  begin
    with hdr do begin
      nam:=name; nam[0]:=chr(pred(pos(#0,nam)));
      sav:=pos('.',nam);
      if sav>0 then for i:=sav to 8 do insert(' ',nam,i);
      nam:=copy(nam+'           ',1,12);
      dos_date(date,time,dt);
      rlen:=long_to_real(length);
      rsiz:=long_to_real(size);
      if rlen=0.0 then dsksiz:=0
        else dsksiz:=succ(trunc((rlen-1.0)/2048.0)) shl 1;
      write(nam,rlen:9:0,dsksiz:5,'k  ');
      case vers of
        1       : write('   --   ');
        2       : write(' Stored ');
        3       : write(' Packed ');
        4       : write('Squeezed');
        5,6,7,8 : write('Crunched');
        9       : write('Squashed');
        else      write(' Unknown');
        end;
      if rlen=0.0 then sav:=0 else sav:=100-round(100.0*rsiz/rlen);
      write(vers:3,rsiz:9:0,sav:4,'%');
      if date=0 then write('':11)
        else write(dt[0]:4,' ',mon[dt[1]],dt[2]:3);
      if time=0 then write('':8)
        else write(dt[3]:3,':',dt[4] div 10,dt[4] mod 10,'  ');
      writehex(crc); writeln;
      tnum:=succ(tnum); add_long(tlen,length);
      tdsk:=tdsk+dsksiz; add_long(tsize,size); crcsum:=crcsum+crc;
      end;
    end;

begin  { lstarc }
  if openarc(false) then begin
    tlen:=long_null; tsize:=long_null; tnum:=0; crcsum:=0; tdsk:=0;
    for n:=1 to pcount do did[n]:=false;
    writeln(^M^J'Name           Length  Disk   Method  Ver  Stored Saved',
            '    Date    Time  CRC'^M^J'============  =======  ====  ========',
            ' === ======= ===== ========= =====  ====');
    if pcount=0 then while readhdr(header,oldarc) do begin
      lstfile(header);
      fskip(oldarc,header.size);
      end
    else while readhdr(header,oldarc) do begin
      found:=false; n:=1;
      repeat
        if match(pstring(header.name),arg[n]) then begin
          found:=true; did[n]:=true; end;
        n:=succ(n);
      until found or (n>pcount);
      if found then lstfile(header);
      fskip(oldarc,header.size);
      end;
    if tnum>0 then begin
      rlen:=long_to_real(tlen);
      rsiz:=long_to_real(tsize);
      write('        ====  =======  ====               =======  ===',
            '                   ====',^M^J,
            'Total',tnum:7,rlen:9:0,tdsk:5,'k',rsiz:22:0);
      if rlen>0.0 then write(100-round(100.0*rsiz/rlen):4) else write('   0');
      write('%','':19);
      writehex(crcsum); writeln;
      end;
    note_notfound;
    closearc;
    end
  else writeln(empty);
  end;
