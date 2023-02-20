program MS_DOS_Reformatter;

(* MSFORM.PAS vom 3.9.87, Tilmann Reh *)

const bufgr = 2047;      (* max. Puffer fuer Boot/FAT/Dir *)
      titel = ^M^J'MS-DOS-Reformatter V1.2 vom 3.9.87';

var  buffer                       : array[0..bufgr] of byte;
     psize,secptrk,lsecptrk,heads,
     tracks,sektoren,reservsec,
     eintraege,fatsecs,ident      : integer;
     secpcl,fatzahl,medium,
     msdrive,logdrive,i           : byte;
     eingabe                      : string[14];

(***********************************************************
**                 U T I L I T I E S                      **
***********************************************************)

(* Direkter BIOS-Aufruf ueber BDOS-Funktion 50 *)

function UBIOS(fn,pa,pbc,pde,phl:integer):integer;
var biospb : record
               func,a   : byte;
               bc,de,hl : integer;
               end;
    result : integer;
begin
  with biospb do begin
    func:=fn; a:=pa;
    bc:=pbc; de:=pde; hl:=phl;
    end;
  result:=0;
  case fn of
    2,3,7,13..15,17..19,24 : result:=BDOS(50,addr(biospb));
    9,16,20,22,25          : result:=BDOSHL(50,addr(biospb));
    else                     BDOS(50,addr(biospb));
    end;
  ubios:=result;
  end;

(* RWSECTOR liest/schreibt phys. Sektor auf MS-Drive *)
(* Absolute Sektorangabe, Sektoren ab 1 gezaehlt !!  *)

procedure rwsector(abssec:integer; wflag:boolean;
                                               buf:integer);
var trk,sec,k : integer;
    dph       : ^integer absolute k;
begin
  abssec:=pred(abssec);                 (* zaehlt ab 0 *)
  trk:=abssec div lsecptrk;
  sec:=abssec mod lsecptrk;
  k:=ubios(9,0,msdrive,1,0);            (* SELDSK *)
  sec:=ubios(16,0,sec,dph^,0);          (* SECTRN *)
  k:=ubios(23,0,1,0,0);                 (* MULTIO *)
  k:=ubios(10,0,trk,0,0);               (* SETTRK *)
  k:=ubios(11,0,sec,0,0);               (* SETSEC *)
  k:=ubios(12,0,buf,0,0);               (* SETDMA *)
  k:=ubios(28,1,0,0,0);                 (* SETBNK *)
  if wflag then k:=ubios(14,0,0,0,0)    (* WRITE  *)
    else k:=ubios(13,0,0,0,0);          (* READ   *)
  end;

(* MSLOGIN schaltet BDOS und BIOS 'kalt' auf MS-Drive um. *)
(* Physikalische Sektorgroesse (CP/M) wird gelesen.       *)

procedure mslogin;
type dpb = record
             spt         : integer;
             bsh,blm,exm : byte;
             dsm,drm     : integer;
             al0,al1     : byte;
             cks,off     : integer;
             psh,phm     : byte;
             end;
var  k   : integer;
     ptr : ^dpb absolute k;
begin
  k:=ubios(9,0,msdrive,0,0);     (* SELDSK kalt         *)
  bdos(14,msdrive);              (* Select Drive        *)
  k:=bdoshl(31);                 (* Get DPB             *)
  psize:=128 shl ptr^.psh;       (* Byte/Sektor         *)
  end;

(* RELOG schaltet 'warm' auf angegebenes Drive um. *)

procedure relog(drive:byte);
var k : integer;
begin
  k:=ubios(9,0,drive,1,0);       (* SELDSK *)
  bdos(14,drive);                (* Select Drive *)
  end;

(* FAT's auf Diskette schreiben *)

procedure writefat;
var i,j,sec : integer;
begin
  for i:=1 to fatzahl do begin
    sec:=reservsec+pred(i)*fatsecs;
    for j:=1 to fatsecs do
      rwsector(sec+j,true,addr(buffer[psize*pred(j)]));
    end;
  end;

(* Verwaltungsdaten fuer IBM-Disketten berechnen *)

procedure calculate_ibm;
begin
  reservsec:=1; fatzahl:=2; tracks:=40;
  case ident of
    160 : begin
            heads:=1; secpcl:=1; secptrk:=8;
            fatsecs:=1; eintraege:=64;
            end;
    180 : begin
            heads:=1; secpcl:=1; secptrk:=9;
            fatsecs:=2; eintraege:=64;
            end;
    320 : begin
            heads:=2; secpcl:=2; secptrk:=8;
            fatsecs:=1; eintraege:=112;
            end;
    360 : begin
            heads:=2; secpcl:=2; secptrk:=9;
            fatsecs:=2; eintraege:=112;
            end;
    end;
  end;

(* Format-Daten manuell eingeben *)

procedure input_data;
begin
  write('Seitenzahl       : '); readln(heads);
  write('Spurzahl         : '); readln(tracks);
  write('Sektoren/Spur    : '); readln(secptrk);
  write('Boot-Sektoren    : '); readln(reservsec);
  write('Anzahl FATs      : '); readln(fatzahl);
  write('Sektoren/FAT     : '); readln(fatsecs);
  write('DIR-Eintr{ge     : '); readln(eintraege);
  write('Sektoren/Cluster : '); readln(secpcl);
  end;

(* RE-FORMATTING : Boot-Record/FAT's/Directory schreiben *)

procedure reformat;
const oem : array[1..8] of char = 'REH V1.2';
var   i,s : integer;
begin
  sektoren:=tracks*secptrk*heads;
  lsecptrk:=secptrk*heads;
  medium:=$7C+byte(heads=2)
          +byte(secptrk=8) shl 1
          +byte(tracks<>80) shl 7;
  rwsector(1,false,addr(buffer));    (* Boot-Record lesen *)
  fillchar(buffer,32,0);
  move(oem,buffer[3],8);
  buffer[$0B]:=lo(psize); buffer[$0C]:=hi(psize);
  buffer[$0D]:=secpcl;
  buffer[$0E]:=reservsec; buffer[$0F]:=0;
  buffer[$10]:=fatzahl;
  buffer[$11]:=lo(eintraege); buffer[$12]:=hi(eintraege);
  buffer[$13]:=lo(sektoren); buffer[$14]:=hi(sektoren);
  buffer[$15]:=medium;
  buffer[$16]:=fatsecs; buffer[$17]:=0;
  buffer[$18]:=secptrk; buffer[$19]:=0;
  buffer[$1A]:=heads; buffer[$1B]:=0;
  rwsector(1,true,addr(buffer));           (* Boot-Record *)
  fillchar(buffer,succ(bufgr),0);
  buffer[0]:=medium; buffer[1]:=$FF; buffer[2]:=$FF;
  writefat;
  fillchar(buffer,1024,$F6);                     (* FAT's *)
  for i:=0 to 32 do buffer[i shl 5]:=0;
  s:=reservsec+fatzahl*fatsecs;
  for i:=succ(s) to s+eintraege shl 5 div psize do
    rwsector(i,true,addr(buffer));           (* Directory *)
  end;

(***********************************************************
**             H A U P T P R O G R A M M                  **
***********************************************************)

begin
  logdrive:=BDOS(25);
  writeln(titel);
    writeln(^M^J'Laufwerk mit MS-DOS-Diskette :  P:');
    msdrive:=ord(upcase('P'))-$41;
  write('Hinweis: phys. Disk-Parameter m}ssen korrekt ',
                                    'gesetzt sein !'^M^J^J);
  repeat
    mslogin;
    ident:=0;
    if psize=512 then begin
      write('Kapazit{t in KB (nur IBM) : ');
      readln(ident);
      end;
    case ident of
      160,180,320,360 : calculate_ibm;
      else              input_data;
      end;
    repeat
      reformat;
      write('Weitere Diskette gleiche Daten (J/N) ? ');
      readln(eingabe);
    until (length(eingabe)=0) or (upcase(eingabe[1])='N');
    write('Weitere Diskette andere Daten (J/N) ? ');
    readln(eingabe);
  until (length(eingabe)=0) or (upcase(eingabe[1])='N');
  relog(logdrive);
  end.


