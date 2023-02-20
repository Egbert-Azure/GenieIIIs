program arc;    { ARCCPM Version 2.2 }

(* TR 120192 *)

{$I CLINE127.INC }    (* 127-Character Commandline Patch *)
{$I ARCDEF.INC }      (* ARCCPM Variable Type Definition *)

(* Vordefinierte Variablen des Hauptprogramms: Default-Einstellung *)

const title    = 'ARC for CP/M, V2.2  TR 940111';
      kludge   : boolean    = true;   (* Komprimieren versuchen   *)
      force    : boolean    = false;  (* nicht immer crunchen     *)
      warn     : boolean    = true;   (* Fehlermeldungen anzeigen *)
      note     : boolean    = true;   (* Hinweise anzeigen        *)
      keepbak  : boolean    = false;  (* Kein ARC-Backup anlegen  *)
      pass     : string[10] = '';     (* Kein Passwortschutz      *)
      cmd      : char       = #0;     (* noch kein Befehl gueltig *)
      CreateStamp : boolean = false;  (* Auspacken mit Date&Time  *)
      empty    = 'Archive empty - nothing done!';

(* Globale Variablen des Hauptprogramms *)

var   arg                            : arglist;
      time,cpm3                      : boolean;
      cmdstr,arcname,newname,bakname : filenam;
      arcpath                        : str4;
      newarc,oldarc                  : binfile;
      pcount,TempDrv                 : byte;
      gi,arcdate,arctime,
      SCBBase,TimeJumpAdr            : integer;

(* Globale Variablen fuer Unterprogramme gemeinsam *)

      orgfile                        : binfile;
      header                         : headtype;
      did                            : flaglist;
      found,bdummy                   : boolean;

{$I ARCMISC.INC }    (* Grundfunktionen, Mathe   *)
{$I ARCFUNC.INC }    (* Binaer-File-I/O          *)
{$I ARCDIR.INC }     (* Directory-Funktionen     *)
{$I ARCIO.INC }      (* ARC-File-I/O             *)
{$I ARCSQU.INC }     (* Un/Squeezing             *)
{$I ARCLZW.INC }     (* Un/Crunching (LZW)       *)
{$I ARCPACK.INC }    (* Komprimieren/Expandieren *)
{$I ARCLST.INC }     (* ARC-Inhalt zeigen        *)
{$I ARCEXT.INC }     (* Dateien extrahieren      *)
{$I ARCDEL.INC }     (* Dateien loeschen         *)
{$I ARCADD.INC }     (* Dateien einfuegen        *)

procedure showhelp;
begin
  write(
'          ARCCPM - an ARCHIVE Utility for CP/M-80 (V 2.2 & 3.0)'^M^J,
'          -----------------------------------------------------'^M^J,
'              Version 2.2  - Turbo Pascal 3.00A -  Jan.1994'^M^J^J,
'              Public domain by      Reimer Mellin, Muenchen'^M^J,
'              Enhanced Rewriting by     Tilmann Reh, Siegen'^M^J^J,
'              > NOTE: FOR PRIVATE NON-COMMERCIAL USE ONLY <'^M^J^J,
'                               U S A G E :'^M^J^J,
'            ARC adelmvx (bknstw) (g<passw>) <arch> (<fname>..)'^M^J^J,
'           a   = add files to archive   b   = keep backup of archive'^M^J,
'           d   = delete from archive    k   = force crunching'^M^J,
'           x,e = extract from archive   n   = suppress notes'^M^J,
'           l,v = list files in archive  s   = store only'^M^J,
'           m   = move files to archive  t   = set date and time'^M^J,
'           g   = Encrypt/decrypt entry  w   = suppress warnings'^M^J^J,
'           ( Refer to ARCCPM.DOC for complete documentation ... )'^M^J);
  end;

(*----- MAIN -----*)

begin
  if paramcount<2 then showhelp else begin
    writeln(^M^J,title,^M^J);
    TempDrv:=succ(BDOS(25));
    cpm3:=(bdoshl(12)>=$30);
    if cpm3 then begin
      gi:=lo(GetSCB($50));
      if gi>0 then TempDrv:=gi;
      SCBBase:=GetSCB($3A);
      TimeJumpAdr:=SCBBase+$B2;
      CreateStamp:=mem[TimeJumpAdr]=$C3;
      end;
    cmdstr:=paramstr(1); arcname:=paramstr(2);
    pcount:=pred(pred(paramcount));
    for gi:=1 to pcount do arg[gi]:=paramstr(succ(succ(gi)));
    gi:=pos(':',arcname);
    if gi=0 then arcpath:='' else begin
      arcpath:=copy(arcname,1,gi); delete(arcname,1,gi); end;
    makefnam(arcname,'.ARC',arcname);
    makefnam(arcname,'.$$$',newname);   (* ARCPATH erst bei OPENARC *)
    makefnam(arcname,'.BAK',bakname);
    time:=cpm3;
    gi:=1; repeat
      case cmdstr[gi] of
        'A','D','E','L','M','V','X' : begin
               if cmd<>#0 then begin
                 writeln('Cannot mix ',cmd,' and ',cmdstr[gi]);
                 halt; end;
               cmd:=cmdstr[gi];
               end;
        'B': keepbak:=true;
        'W': warn:=false;
        'N': note:=false;
        'S': kludge:=false;
        'K': force:=true;
        'T': time:=false;
        'G': begin
               pass:=copy(cmdstr,succ(gi),gi+21);
               gi:=length(cmdstr);
               end;
        '-','/':;
        else begin
          writeln(cmdstr[gi],' is an unknown command!');
          halt; end;
        end;
      gi:=succ(gi);
    until gi>length(cmdstr);
    if cmd=#0 then begin writeln('I have nothing to do ...'); halt; end;
    get_global_datetime;
    case cmd of
      'A','M' : addarc;
      'D'     : delarc;
      'E','X' : extarc;
      'V','L' : lstarc;
      end;
    end;
  end.
