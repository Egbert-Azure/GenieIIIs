(* ARCFUNC.INC TR 940111 *)

(* Folgende Prozeduren/Funktionen nur fuer Lese-Dateien! *)

(* Datei auf Anfang ruecksetzen *)

procedure freset(var f:binfile);
begin
  with f do begin
    seek(id,0); actpos:=long_null;
    feof:=filesize(id)=0;
    end;
  end;

(* Binaerfile eroeffnen *)

function fopen_read(name:filenam; var f:binfile):boolean;
var last : integer;
begin
  with f do begin
    assign(id,name);
    {$I-} reset(id); {$I+}
    fopen_read:=false;
    if ioresult=0 then begin
      fopen_read:=true;
      if filesize(id)>0 then begin
        last:=pred(filesize(id));
        fsize.hi:=0; fsize.lo:=last; shift_long(fsize,7);
        seek(id,last); blockread(id,buff,1); last:=127;
        while (buff[last]=$1A) and (last>0) do last:=pred(last);
        fsize.b0:=fsize.b0 + last; inc_long(fsize);
        end
      else begin fsize:=long_null; feof:=true; end;
      freset(f);
      end;
    end;
  end;

(* Einzelnes Zeichen aus Binaerfile holen (BYTE). *)
(* nach Lesen des letzten Zeichens ist FEOF=TRUE. *)
(* weiteres Lesen ergibt nur noch 00.             *)

function fgetc(var f:binfile):byte;
begin
  inline($ED/$5B/F/$21/$09/$01/$19/$CB/$46/$21/$00/$00/$C0/$21/$04/$01/
  $19/$7E/$B7/$20/$19/$21/GI/$22/$E6/$00/$21/$09/$01/$19/$01/$02/$00/
  $3E/$21/$CD/$FD/$1A/$CD/$1B/$20/$ED/$5B/F/$21/$04/$01/$19/$E5/$6E/
  $26/$00/$19/$6E/$26/$00/$E3/$34/$20/$06/$23/$34/$20/$02/$23/$34/
  $21/$04/$01/$19/$44/$4D/$21/$00/$01/$19/$0A/$96/$03/$23/$0A/$9E/
  $03/$23/$0A/$9E/$3F/$3E/$00/$17/$21/$08/$01/$19/$77/$E1/$C9);
  end;

(* Block in Datei uebergehen *)

procedure fskip(var f:binfile; offset:longword);
begin
  with f do begin
    add_long(actpos,offset);
    seek(id,actpos.mid shl 1);
    if actpos.b0>0 then blockread(id,buff,2,gi);
    inline($ED/$5B/F/$21/$04/$01/$19/$44/$4D/$21/$00/$01/$19/$0A/$96/
    $03/$23/$0A/$9E/$03/$23/$0A/$9E/$3F/$3E/$00/$17/$21/$08/$01/$19/$77);
    end;
  end;

(* Folgende Prozeduren/Funktionen nur fuer Schreib-Dateien! *)

(* Binaerfile eroeffnen *)

procedure fopen_write(name:filenam; var f:binfile);
begin
  with f do begin
    assign(id,name); rewrite(id); actpos:=long_null;
    end;
  end;

(* Einzelnes Zeichen in Binaerdatei schreiben. *)

procedure fputc(var f:binfile; c:byte);
begin
  inline($ED/$5B/F/$21/$04/$01/$19/$E5/$6E/$26/$00/$19/$3A/C/$77/$E1/
  $34/$C0/$23/$34/$20/$02/$23/$34);
  blockwrite(f.id,f.buff,2);
  end;

(* Binaerfile schliessen. Evtl. noch Puffer speichern *)

procedure fclose(var f:binfile);
var sav : byte;
begin
  with f do begin
    sav:=actpos.b0;
    if sav>0 then begin
      fillchar(buff[sav],256-sav,$1A);
      if sav>128 then sav:=2 else sav:=1;
      blockwrite(id,buff,sav);
      end;
    close(id);
    end;
  end;

(* CRC-Bearbeitung *)

procedure addcrc(var crc:integer; cod:byte);
const crctab : array[0..255] of integer =
   ($0000,$C0C1,$C181,$0140,$C301,$03C0,$0280,$C241,$C601,$06C0,$0780,$C741,
    $0500,$C5C1,$C481,$0440,$CC01,$0CC0,$0D80,$CD41,$0F00,$CFC1,$CE81,$0E40,
    $0A00,$CAC1,$CB81,$0B40,$C901,$09C0,$0880,$C841,$D801,$18C0,$1980,$D941,
    $1B00,$DBC1,$DA81,$1A40,$1E00,$DEC1,$DF81,$1F40,$DD01,$1DC0,$1C80,$DC41,
    $1400,$D4C1,$D581,$1540,$D701,$17C0,$1680,$D641,$D201,$12C0,$1380,$D341,
    $1100,$D1C1,$D081,$1040,$F001,$30C0,$3180,$F141,$3300,$F3C1,$F281,$3240,
    $3600,$F6C1,$F781,$3740,$F501,$35C0,$3480,$F441,$3C00,$FCC1,$FD81,$3D40,
    $FF01,$3FC0,$3E80,$FE41,$FA01,$3AC0,$3B80,$FB41,$3900,$F9C1,$F881,$3840,
    $2800,$E8C1,$E981,$2940,$EB01,$2BC0,$2A80,$EA41,$EE01,$2EC0,$2F80,$EF41,
    $2D00,$EDC1,$EC81,$2C40,$E401,$24C0,$2580,$E541,$2700,$E7C1,$E681,$2640,
    $2200,$E2C1,$E381,$2340,$E101,$21C0,$2080,$E041,$A001,$60C0,$6180,$A141,
    $6300,$A3C1,$A281,$6240,$6600,$A6C1,$A781,$6740,$A501,$65C0,$6480,$A441,
    $6C00,$ACC1,$AD81,$6D40,$AF01,$6FC0,$6E80,$AE41,$AA01,$6AC0,$6B80,$AB41,
    $6900,$A9C1,$A881,$6840,$7800,$B8C1,$B981,$7940,$BB01,$7BC0,$7A80,$BA41,
    $BE01,$7EC0,$7F80,$BF41,$7D00,$BDC1,$BC81,$7C40,$B401,$74C0,$7580,$B541,
    $7700,$B7C1,$B681,$7640,$7200,$B2C1,$B381,$7340,$B101,$71C0,$7080,$B041,
    $5000,$90C1,$9181,$5140,$9301,$53C0,$5280,$9241,$9601,$56C0,$5780,$9741,
    $5500,$95C1,$9481,$5440,$9C01,$5CC0,$5D80,$9D41,$5F00,$9FC1,$9E81,$5E40,
    $5A00,$9AC1,$9B81,$5B40,$9901,$59C0,$5880,$9841,$8801,$48C0,$4980,$8941,
    $4B00,$8BC1,$8A81,$4A40,$4E00,$8EC1,$8F81,$4F40,$8D01,$4DC0,$4C80,$8C41,
    $4400,$84C1,$8581,$4540,$8701,$47C0,$4680,$8641,$8201,$42C0,$4380,$8341,
    $4100,$81C1,$8081,$4040);
begin
  inline($2A/CRC/$5E/$23/$56/$3A/COD/$AB/$4F/$06/$00/$DD/$21/CRCTAB/
  $DD/$09/$DD/$09/$DD/$7E/$01/$77/$2B/$DD/$7E/$00/$AA/$77);
  end;

(* ARC-File(s) oeffnen bzw. schliessen *)

var oldexist,newflag : boolean;

function openarc(flag:boolean):boolean;
begin
  newflag:=flag;
  oldexist:=fopen_read(arcpath+arcname,oldarc);
  if newflag then begin
    writeln('Creating new archive ...');
    fopen_write(arcpath+newname,newarc);
    end;
  openarc:=oldexist;
  end;

procedure closearc;

  procedure filerename(var f:binfile; var nam:filenam);
  var h : file;
  begin
    assign(h,arcpath+nam);
    {$I-} reset(h); {$I+}
    if ioresult=0 then begin close(h); erase(h); end;
    rename(f.id,nam);
    end;

begin
  if newflag then begin
    fclose(newarc);
    if oldexist then begin
      close(oldarc.id);
      if keepbak then filerename(oldarc,bakname) else erase(oldarc.id);
      end;
    filerename(newarc,arcname);
    end;
  end;

