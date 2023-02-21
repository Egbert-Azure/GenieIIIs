# alias.cmd unter Z3Plus

> Note: first draft; needs to be adjusted to support 2k dates

## Purpose of alias.cmd

``` consol
  SAMPLE ALIAS.CMD FILE FOR USE WITH Z3PLUS
  -----------------------------------------
                          Version 07/24/92
last modification: 11/01/92
                   11/08/92
                   01/30/93
                   03/02/93
     10/10/93
     10/16/93 
     10/30/94
     11/20/94
   
 ; This alias provides a response to the DIR command when there is no
 ; resident DIR or transient DIR.COM.

DDIR            tdir [date]                
DIR  sdz $*

 ; This alias is used with the syntax MD DU:DIR PW or MD DIR PW to assign
 ; the named directory DIR to the designated (or current) drive/user.  The
 ; second token is optional; if present, it will assign a password as well.

MD   editndr $td1$tu1:$tn1 $2 \x;savendr default.ndr;lput root:z3plus default.ndr;del default.ndr

 ; This alias will allow you to examine a file.  It loads the file into the
 ; TPA and then uses the RCP peek command to examine it.  It will load a file
 ; from the current directory even if that directory is not on the path.

LOOK  get 100 $td1$tu1:$tf1;p 100

 ; This alias removes Z3PLUS and restores the operation of CP/M.

OFF=CPM=EXIT=BYE z3plus off

 ; This alias provides a shorthand for the CRUNCH command.

CR,UNCH  crunch $*

 ; The following pair of aliases automates a recursive invocation of some
 ; other command line.  It is invoked as RECURSE COMMANDLINE.

REC,URSE if nu $1;echo;echo %<  s%>yntax: %<$0 cmdname [parameters];echo;else; recurse2 $*;fi
RECURSE2 fi;$*;if in r%>un %<"$1" %>again? ; $0 $*

 ; The following pair of aliases automates the repeated invocation of a
 ; command that takes a single argument.  It is invoked as
 ; REPEAT COMMAND ARG1 ARG2 ... ARGN.  The command COMMAND is executed
 ; in sequence, first with ARG1, then with ARG2, and so on.

REP,EAT  if nu $2;echo;echo %<  s%>yntax: %<$0 cmdname arg1 arg2 ...;echo;else; repeat2 $*;fi
REPEAT2  fi;$1 $2;if ~nu $3; repeat2 $1 $-2

; Das ist ein ALIAS um dasselbe Programm (USERLIST) mit anderen Daten zu
; verwenden. An 100 HEX geladen - ge{ndert und ausgef}hrt! 21E8=L{nge! 

ZVERTEIL  get 100 userlist.com;poke 21e8 08;poke 21e9 "ZVERTEIL";go $1

;Insert this into your ALIAS.CMD (or LSH.CMD for that matter!) - Rick Charnes

gst  cls;box 0 3 10 19 60;go 0 5 15 15 50;go 0 7 20 11 40;gxymsg 10 30 gst does exactly;go 12 32 what i want!

;aliasins.cmd
  
compare cls;echo  =,< u%>se %<b%>comp (y/n)? ;if input;bcomp $1 $2;else;vcomp $1 $2;fi
copyinto cls;echo =,  c%>opying%< < $2 > %>to the new filename called%< < $1 >;cp $1=$2
prtdrv dos:vid prtdrvr;/selpr
pr.int=list cls;if nu $1;print //;gtv par choose parameters for this print job;gtv ft Font size - 10, 12, or 17cpi? ;rsv ok%ft;rsv print $f2 %par;ok10;else;/print2 $1;fi
print2 $z zif;cls;print //;gtv par choose parameters for this print job;gtv ft Font size - 10, 12, or 17cpi? ;rsv ok%ft;rsv print $1 %par;ok10

; Das ist ein ALIAS um DOSlern und New-/Gdos-lern das Leben leichter zu
; machen. CLUB 80 INFO Nr.33 Seite 45-46 von Alexander Schmid.

ERA,SE=DEL,ETE=KILL               ERA $*

; Das ist ein ALIAS um COM Dateien aus der Library COMMAND auszuf}hren
; Idee: Alexander Schmidt und Z3PLUS Manual

VLU            lx -dos:command vlu $1
FF             lx -dos:command ff $1
BACKUP         lx -dos:command backup $1
TREE  lx -dos:command umap $1 /U
FREE            lx -dos:command umap $1 /VS
PC,COPY  lx -dos:command pccopy
TED             lx -dos:command minited $1
UNZIP           lx -dos:command unzip15 $*
decode  lx -dos:command uudecode $*
encode  lx -dos:command uuencode $*
zfind  lx -dos:command zfind15 $*
pccpm  lx -dos:command pc2cpm $*
cpsel           lx -dos:command cpsel

;ALIAS um Adressen automatisch in Briefkopf einzulesen
;Idee: Reinhard Kirsch

adress cls;del temp.let;zfind m0:adress.dat/B >temp.dat;zex zdebrf;go zde m0:temp.let;

;HELP Men}

HELP          zhelp:zhelp
HELPCPM       zhelp:;help;dos:

; Ab hier stehen ALIAS, mit denen der HI-TECH-C Compiler, der in C9:HITECH
; liegt, einfacher erreicht werden kann. Die Sources der C-Programme sollen
; in einem anderen Userbereich zu liegen kommen, n[mlich in C11:CSOUR
; Vor der LIBC.LIB wird die HOLTE.LIB durchsucht. Durch den Parameter -lz
; wird die LIBZ.LIB mit eingebunden !

CN      z3plus small;C9:C -i9:c: $1.c 9:c:libc.lib  $-1 -lz;z3plus
CC     z3plus small;C9:C -i9:c: $1.c 9:c:holte.lib $-1 -lz;z3plus
CCF z3plus small;C9:C -i9:c: $1.c 9:c:holte.lib $-1 -lf -lz;z3plus

; Suche nach Ausdruck

grep c9:grep $*

; Start C-PreCompiler zum Syntax-check

pre     hitech:pcc $*

; wobei cc einen normalen Compilerlauf initiert und ccf f}r Programm mit
; Fliesskommazahlen gedacht ist.
;
; Memory display aliases -- these aliases use the peek command to show the
; contents of various system modules

PBIOS=BIOS  p $ab
PCCP=CCP=PCPR=CPR p $ac
PDOS=DOS  p $ad
PENV=ENV  p $ae
PFCP=FCP  p $af
PMCL=MCL  p $al
PMSG=MSG  p $am $+m004f
PNDR=NDR  p $an
PPATH   p $ap $+p000f
PRCP=RCP  p $ar
PSHL=PSHELL=SHL=SHELL p $as $+s007f
PXFCB=XFCB=PFCB=FCB p $ax $+x0023
```
