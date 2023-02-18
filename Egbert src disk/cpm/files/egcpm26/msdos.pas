program MS_DOS_Emulation;

const bufgr    = 32767;                     (* Datenpuffer *)
      fatgr    = 2047;                      (* FAT-Puffer  *)
      titel    = ^M^J'MS-DOS-Emulator V1.2 vom 26.8.87'^M^J;
      befehle1 = 'Befehle: Change, Dir, Erase, Help, New,'^M^J;
      befehle2 = '         Protocol, Quit, Read, Write';
      nofile   = 'No File';
      copying  = '- Copying :';
      diskfull = 'Error : Disk Full';

type tbuff     = array[0..1023] of byte;    (* Sektorpuffer *)
     string14  = string[14];
     anystring = string[255];

(* MS-DOS-Verwaltungsbezogene Variablen *)

var  fatbuf                                 : array[0..fatgr] of byte;
     datbuf                                 : array[0..bufgr] of byte;
     dirbuf                                 : tbuff;
     dirsec,diroff,bufcl                    : integer;

(* MS-DOS-Formatbezogene Variablen *)

     psize,clsize,dirstart,datstart,
     reservsec,eintraege,sektoren,fatsecs,
     secptrk,heads,maxclnum                 : integer;
     secpcl,fatzahl,medium                  : byte;

(* MS-DOS-Dateibezogene Variablen *)

     msname                                 : string14;
     startgruppe,datum,zeit                 : integer;
     datlength                              : real;

(* CP/M-Dateibezogene Variablen *)

     cpmname                                : string14;
     datei                                  : file;
     cpmlength                              : integer;

(* Variablen des Hauptprogramms und zur allgemeinen Verwaltung *)

     msdrive,cpmdrive,logdrive,i            : byte;
     befehl                                 : anystring;
     cmd                                    : char;
     suchname                               : string14;
     gefunden,exit,print                    : boolean;

{$I MSDOS.IN0 }  (* Utilities                 *)
{$I MSDOS.IN1 }  (* MS-DOS Diskette lesen     *)
{$I MSDOS.IN2 }  (* MS-DOS Diskette schreiben *)
{$I MSDOS.IN3 }  (* MS-DOS Erase und Rename   *)

procedure conout(ch:char); external $339;
procedure lstout(ch:char); external $32F;

procedure co(ch:char);
begin
  conout(ch);
  lstout(ch);
  end;

(*********************************************************************
**                        H E L P                                   **
*********************************************************************)

procedure help;

  procedure waittaste;
  var ch : char;
  begin
    write('Beliebige Taste bet{tigen ');
    read(kbd,ch);
    write(^M,'':30);
    end;

begin
  writeln(titel,befehle1,befehle2);
  write(
^M^J'Befehle bestehen generell aus einem Buchstaben. Hinter dem Befehls-',
^M^J'buchstaben kann sofort ein Dateiname folgen, welcher aber auch durch',
^M^J'ein oder mehrere Leerzeichen vom Befehl getrennt sein oder ganz fehlen',
^M^J'darf. Es existieren folgende Befehle :'^M^J,
^M^J'C : CHANGENAME, besser bekannt als RENAME. Es werden zwei Dateinamen',
^M^J'    eingegeben, wovon der erste der Alte und der zweite der Neue ist.',
^M^J'    Fehlende Dateinamen werden explizit abgefragt. Wildcards sind in',
^M^J'    keinem der beiden Namen erlaubt.'^M^J,
^M^J'D : DIRECTORY, gibt die Directory der MS-DOS-Diskette aus. Wildcards',
^M^J'    sind erlaubt, au~erdem kann durch Eingabe eines ''F'' unmittelbar',
^M^J'    nach dem Befehl die Option ''FULL'' gesetzt werden, welche f}r alle',
^M^J'    Files die Dateil{nge sowie Datum- und Zeiteintrag ausgibt. Wird',
^M^J'    kein Dateiname angegeben, so werden alle Files angezeigt.'^M^J,
^M^J'E : ERASE, l|scht das angegebene File. Wildcards sind erlaubt.'^M^J^J);
  waittaste;
  write(
  ^M'H : HELP, augenblicklich aktiver Programmteil.'^M^J,
^M^J'N : NEW, loggt neue (andere) MS-DOS-Diskette ein (FAT wird gelesen und',
^M^J'    logische Parameter werden gesetzt).'^M^J,
^M^J'P : PROTOCOL, schaltet Printer-Echo an bzw. aus.'^M^J,
^M^J'Q : QUIT, Ausgang zum Betriebssystem CP/M.'^M^J,
^M^J'R : READ, Datei(en) von MS-DOS lesen und nach CP/M schreiben. Wird kein',
^M^J'    Dateiname angegeben, werden Quell- und Zielname explizit erfragt,',
^M^J'    wobei diese verschieden sein d}rfen. Ansonsten wird die angegebene',
^M^J'    Datei unter gleichem Namen kopiert. Werden Wildcards benutzt, so',
^M^J'    wird der Dateiname grunds{tzlich von MS-DOS }bernommen.'^M^J,
^M^J'W : WRITE, Datei(en) von CP/M lesen und nach MS-DOS schreiben. Dateien',
^M^J'    werden genau wie bei READ angegeben, bei Wildcards wird immer der',
^M^J'    CP/M-Name }bernommen.'^M^J,
^M^J'Bei den Kopierbefehlen ist zu beachten, da~ unter CP/M die Dateil{ngen',
^M^J'nur Vielfache von 128 sein k|nnen, deshalb werden die Dateien in der',
^M^J'Regel etwas l{nger beim Kopieren, au~erdem m}ssen u.U. Dateiende-Zeichen',
^M^J'angef}gt werden.'^M^J^J);
  end;

(*********************************************************************
**               H A U P T P R O G R A M M                          **
*********************************************************************)

begin
  print:=false;
  writeln(titel,befehle1,befehle2);
    writeln(^M^J'Laufwerk mit MS-DOS-Diskette : P ');
    msdrive:=15;
  repeat
    write('Laufwerk mit CP/M - Diskette : ');
    readln(befehl);
    cpmdrive:=ord(upcase(befehl[1]))-$41;
  until (cpmdrive in [0..7]) and (cpmdrive<>msdrive);
  writeln('Hinweis: phys. Disk-Parameter m}ssen korrekt gesetzt sein !'^M^J);
  logdrive:=BDOS(25);
  mslogin;
  repeat
    repeat
      write('MSDOS>');
      readln(befehl);
    until length(befehl)>0;
    for i:=1 to length(befehl) do befehl[i]:=upcase(befehl[i]);
    cmd:=befehl[1];
    exit:=cmd='Q';
    delete(befehl,1,1);
    while (length(befehl)>0) and (befehl[1]=' ') do delete(befehl,1,1);
    case cmd of
      'C' : rename;
      'D' : directory;
      'E' : erase;
      'H' : help;
      'N' : mslogin;
      'P' : begin
              print:=not print;
              if print then conoutptr:=addr(co)
              else conoutptr:=addr(conout);
              end;
      'Q' : ;
      'R' : readfile;
      'W' : writefile;
      else writeln(^G,cmd,'?');
      end;
  until exit;
  relog(logdrive);
  end.
