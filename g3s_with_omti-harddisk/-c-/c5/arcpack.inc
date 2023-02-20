(* ARCPACK.INC  TR 120192 *)

(* Datei aus ARC-File extrahieren und expandieren *)

procedure unpack(var source,dest:binfile; var hdr:headtype);
begin
  crcval:=0; siz:=hdr.size; dec_long(siz);
  init_ncr; init_crypt;
  case hdr.vers of
    1,2   : while siz.hi>=0 do putc_dir(dest,crypt(getc_decsiz(source)));
    3     : while siz.hi>=0 do putc_ncr(dest,crypt(getc_decsiz(source)));
    4     : unsqueeze(source,dest);
    5,6,7,9 : begin
              writeln('Sorry, style ',hdr.vers,' not (yet?) implemented!');
              fskip(source,hdr.size);
              exit; end;
    8     : uncrunch_lzw(source,dest);
    else    begin
              if warn then begin
                writeln('I dont know how to unpack file ',pstring(hdr.name));
                writeln('I think you need a newer version of ARC');
                end;
              fskip(source,hdr.size);
              exit; end;
    end;
  if (crcval<>hdr.crc) then if warn then
    writeln('WARNING: File ',pstring(hdr.name),' fails CRC check!');
  end;

(* Datei u.U. komprimiert in Archiv einspeichern *)

procedure pack(var source,dest:binfile; var hdr:headtype);
var  stdlen                  : longword;
     crn_nam                 : filenam;
     crn_file                : binfile;
     r_std,r_ncr,r_squ,r_lzw : real;
     dum                     : byte;
begin
  stdlen:=source.fsize; hdr.length:=stdlen; crcval:=0;
  if note then if kludge and not force then
    write(' analyzing..., ') else write(' ............, ');
  if not (kludge or force) then begin
    ncrlen:=stdlen; squlen:=stdlen; lzwlen:=stdlen;    (* STORE *)
    while not source.feof do dum:=getc_dir(source);     (* CRC! *)
    end
  else begin
    init_ncr;
    makefnam(arcname,'.CRN',crn_nam);
    crn_nam:=char(TempDrv+$40)+':'+crn_nam;
    fopen_write(crn_nam,crn_file);
    crunch_lzw(source,crn_file);
    fclose(crn_file);
    if force then begin                        (* nur Crunch *)
      ncrlen:=lzwlen; inc_long(ncrlen); squlen:=ncrlen; stdlen:=ncrlen;
      end
    else begin    (* weitere Kompressionen (Squeeze, Squash) *)
      predict_squ;
      end;
    end;
  r_std:=long_to_real(stdlen);
  r_ncr:=long_to_real(ncrlen);
  r_squ:=long_to_real(squlen);
  r_lzw:=long_to_real(lzwlen);
  freset(source); init_crypt; hdr.crc:=crcval;
  if (r_std<=r_ncr) and (r_std<=r_lzw) and (r_std<=r_squ) then begin
    if note then write('storing..., ');
    hdr.vers:=2; hdr.size:=stdlen;
    writehdr(hdr,dest);
    filecopy(source,dest,stdlen,true);
    end
  else if (r_ncr<=r_lzw) and (r_ncr<=r_squ) then begin
    if note then write('packing..., ');
    hdr.vers:=3; hdr.size:=ncrlen;
    writehdr(hdr,dest);
    init_ncr; ncrend:=source.feof;
    while not ncrend do fputc(dest,crypt(getc_ncr(source)));
    end
  else if r_squ<r_lzw then begin
    if note then write('squeezing..., ');
    hdr.vers:=4; hdr.size:=squlen;
    writehdr(hdr,dest);
    squeeze(source,dest);
    end
  else begin
    if note then write('crunching..., ');
    hdr.vers:=8; hdr.size:=lzwlen;
    writehdr(hdr,dest);
    bdummy:=fopen_read(crn_nam,crn_file);
    filecopy(crn_file,dest,lzwlen,true);
    end;
  deinit_squ;
  if kludge or force then begin close(crn_file.id); erase(crn_file.id); end;
  if note then writeln('done.');
  end;
