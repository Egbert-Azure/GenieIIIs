BACK,UP         BACKUP               
;;
INPUTERA	$"^j^mEnter files to erase as a filelist:  "IF ~NU $'L1;ERASE $'L1;SP;ELSE;ECHO ^M^J N%>OTHING ENTERED!;FI
INPUT		$Z$"^m^jEnter penultimate command ^[) ('EXIT' to exit ZEX completely):  ^[("$ZIF EQ $'E1 EXIT;ECHO ^M^JR%>ETURNING TO %<ZCPR3...;RS %CURDU;POKE $+M0008 0;ELSE;$'L1;FI
INPUTRET	$Z$"^m^jEnter final command ^[)('EXIT' to exit ZEX completely):  ^[("$ZFI;IF EQ $'E1 EXIT;ECHO ^M^JA%>BORTING %<ZEX %>SCRIPT...;RS %CURDU;POKE $+M0008 0;ELSE;$'L1;RS E R%>ETURNING TO %<ZEX %>SCRIPT AT LETTER%< %$RF05...;HOLD 2;FI

D,IR		SDZ $*

THIS ALIAS PROVIDES A DIRECTORY-DISPLAY COMMAND IN WHICH THE FILE
 ; SPECIFICATION IS AUTOMATICALLY WILDCARDED.  IT SAVES THE TROUBLE OF
 ; HAVING TO TYPE ASTERISKS MANUALLY.

D!=SD/	        SDZ $TD1$TU1:$TN1*.$TT1* $-1

 ; THIS ALIAS IS USED WITH THE SYNTAX NAME DU:DIR PW OR NAME DIR PW TO ASSIGN
 ; THE NAMED DIRECTORY DIR TO THE DESIGNATED (OR CURRENT) DRIVE/USER.  THE
 ; SECOND TOKEN IS OPTIONAL; IF PRESENT, IT WILL ASSIGN A PASSWORD AS WELL.

FOR             format $1;initdir $1;set $1[update=on,access=on] 



NAME=SETNAME	EDITNDR $TD1$TU1:$TN1 $2 \X



 ; this will format a disc on a Computer with CP/M PLUS and prepare DS

CREATE=MAKEDISK	format $1;zex initime $1;set $1[name=$"DISKNAME> "$'L1,create,update]


make=initdir $1;set $1[name=$"DISKNAME> "$'L1,create,update]

 ; THIS ALIAS CAN HELP TO TEMPORARILY CHANGE A DU: NAME

RENDIR		NAME10 $1


 ; THIS ALIAS WILL SAVE THE CURRENTLY DEFINED NAMED DIRECTORIES IN THE FILE
 ; A0:Z3PLUS.NDR.  IF A COMMAND TAIL BEGINNING WITH 'L' (FOR LIBRARY) IS
 ; INCLUDED, THE FILE WILL BE PUT INTO Z3PLUS.LBR SO THAT IT WILL AUTOMATICALLY
 ; BE LOADED AT Z-SYSTEM COLD BOOT AND THE INDIVIDUAL NDR FILE WILL BE ERASED.

PUTNDR		SAVENDR C0:DEFAULT;IF EQ $1 L*;LPUT C0:DEFAULT C0:DEFAULT.NDR;ERA C0:DEFAULT.NDR;FI

 ; THIS ALIAS WILL ALLOW YOU TO EXAMINE A FILE.  IT LOADS THE FILE INTO THE
 ; TPA AND THEN USES THE RCP PEEK COMMAND TO EXAMINE IT.  IT WILL LOAD A FILE
 ; FROM THE CURRENT DIRECTORY EVEN IF THAT DIRECTORY IS NOT ON THE PATH.

LOOK		GET 100 $TD1$TU1:$TF1;P 100


 ; THIS IS TO QUIT Z3PLUS

CPM=OFF=EXIT		Z3PLUS OFF

 ; THIS ALIAS PROVIDES A SHORTHAND FOR THE CRUNCH COMMAND.

CR,UNCH		crunch $*
UNCR,UNCH       uncrunch $*
ZF,ILER         zfiler $* 
ZP,ATCH         zpatch $* 
QLIST           ql $*


 ; F}r das Programm MSDOS um IBM Disketten lesen zu k|nnen, muss das
 ; Z3PLUS SMALL Modell eingeschaltet werden, sonst zuwenig Speicher.
 
MSDOS		fremd ibm-pc;z3plus small;c0:mstocpm;z3plus

 

 ; THE FOLLOWING PAIR OF ALIASES AUTOMATES A RECURSIVE INVOCATION OF SOME
 ; OTHER COMMAND LINE.  IT IS INVOKED AS RECURSE COMMANDLINE.

REC,URSE	IF NU $1;ECHO;ECHO %<  S%>YNTAX: %<$0 CMDNAME [PARAMETERS];ECHO;ELSE; RECURSE2 $*;FI
RECURSE2	FI;$*;IF IN R%>UN %<"$1" %>AGAIN? ; $0 $*


 ; THE FOLLOWING PAIR OF ALIASES AUTOMATES THE REPEATED INVOCATION OF A
 ; COMMAND THAT TAKES A SINGLE ARGUMENT.  IT IS INVOKED AS
 ; REPEAT COMMAND ARG1 ARG2 ... ARGN.  THE COMMAND COMMAND IS EXECUTED
 ; IN SEQUENCE, FIRST WITH ARG1, THEN WITH ARG2, AND SO ON.

REP,EAT		IF NU $2;ECHO;ECHO %<  S%>YNTAX: %<$0 CMDNAME ARG1 ARG2 ...;ECHO;ELSE; REPEAT2 $*;FI
REPEAT2		FI;$1 $2;IF ~NU $3; REPEAT2 $1 $-2
 
 ; Der ALIAS RAMINIT soll einige Programme auf die RAMDisk kopieren, 
 ; weil ich die wichtig finde.
 
INI,TRAM        pip m:=pip.com;pip m:=fremd.com;pip m:=zfiler.com;pip m:=xd.com;pip m:=lsh.com;pip m:=zde.com;pip m:=ql.com;pip m:=zpatch.com;pip m:=dir.com

 ; Die folgenden ALIASes geben einige Contolcodes aus. Der Zugriff wird
 ; entschieden vereinfacht.
 ; STATOFF = Statuszeile aus
 ; WRAPON  = Wrap around modus on
 ; WRAPOFF = Wrap around modus off
 ; WSKBD   = Word Star Keyboard
 ; NKBD    = Normal Keyboard

STATOFF         echo ^[D
WRAPON          echo ^[X
WRAPOFF         echo ^[Y
WSKBD           echo ^[O
NKBD            echo ^[N

 ; Der ALIAS EH initialisiert den Errorhandler bzw. die Eingabeshell
 
EH                era m:lsh.cmd;m:lsh m:lsh.cmd

 ; Mit HOLERAUS werden Files aus einer .LBR-Datei herausgeholt, entcruncht
 ; und die gecrunchten Files werden geloescht.
 
HOLERAUS       lget $1 *.*;uncrunch *.*;era *.?z?

   
 ; Diese ALIASe erleichtern den Zugang zu den Named Directorys
 
COM,:=COMM,:=COM,MANDS:=A0,:              a0:
WOR,:=WORK,:=B0,:                         b0: 
K[M,:=K[MP,:=K[M,PFF:=P0,:                p0:
BASE,:=ROOT,:                             c0:
ASM,:=SLR,:                               c2:
TURBO:=PASCAL:                            c3:
ZMP:=MEX:=MOD:=MODE:=MODE,M:              c4:
UTI,ILITY:                                c1:
SOUR,CES:				  c5:
CAT,ALOG:                                c14: 
NEU,ES:                                  c13:   
MIX,ED:                                  c10:
PROF,:                                    o0:


      ; Dieser Alias ruft VLU auf, wenn ein LBR-Filename eingegeben wird
 
>LBR      vlu $tn0 

 ; Memory Display Aliases
 
PBIOS=BIOS                  p $ab
PCCP=CCP=CPR                p $ac
PDOS=DOS                    p $ad
PENV=ENV                    p $ae
PFCP=FCP                    p $af
PIOP=IOP                    p $ai
PMCL=MCL                    p $al
PMSG=MSG                    p $am
PNDR=NDR                    p $an
PPATH                       p $ap
PRCP=RCP                    p $ar
PSHL=PSHELL=SHELL           p $as
PXFCB=PFCB=FCB              p $ax
PTCAP                       p f000

; Die folgenden Files befinden sich auf C6:Z3COM> und muessen so nicht
; im Suchpfad mit angegeben werden.


@10=COMP=IMP2Z44=ABORT=CPSET=IMP2Z45=ACOPY=CRUNCH=IOBUG=ACREATE           c6:$!
DCREATE=LAP=ADIR=DIFF=LINKPRL=ARK11=EXAMREL=MAKE27=ARK11ZS                c6:$!
EXL=MCOPY=BALIAS=BCINS=EXTEND=MEX+2ZE=BCOMP=FINDERR=MEX+2ZS               c6:$!
BEEP=FOR=MEX2ZE=GETVAR=MEX2ZS=CD39=GOTO=MKDIR32=CLRCST=HELPC              c6:$!
MKLINE=CLRRSX=HELPPR10=MU3=CMD=HOLDZ=NAME=HP11=NEXT11=COMMEN20            c6:$!
HPRSX=PAGE=PAUSE=SHSET22=TRIM=PIPE18=SHVAR=TY3MTEST=PPIP=SLOWDOWN         c6:$!
TY4MTEST=PROTECT=SNAP=TYPELZ22=PUSH12=SNAPRCP=TYPEQZ17=QLUX=SPEEDUP       c6:$!
QLUXBBS=SUB34=RCP-COM=T4GEN01=UNSQ=REG=T4N41=UNZIP099=RENAMEZ=TALIAS      c6:$!
VCED18=RESOLVE=TCCHECK=VCINST20=RLIB12=TCMAKE=VCOMP=SETFILE=VFILER        c6:$!
SHDEFINE=TESTERR=VID=SHELLINI=TEX13=VREN=SHFILE11=TEXT2DB=WHEEL=SHOW14    c6:$!
TEXT2DB=XECHO=SHOWSHST=TPA=Z-GOLF=Z33FILLD=ZLUXKMD=ZPATCH13=Z33VEH11      c6:$!
ZCNFG14=ZPUZZLE=Z33VER10=ZCNFG14=ZRDPUB=ZCOPY=ZXLATE14=Z3LOCATE           c6:$!
ZDDTZ=ZZAP21=ZGREP                                                        c6:$!

;Insert this into your ALIAS.CMD (or LSH.CMD for that matter!) - Rick Charnes
GST  cls;box 0 3 10 19 60;go 0 5 15 15 50;go 0 7 20 11 40;gxymsg 10 30 gst does exactly;go 12 32 what i want!


_:=__:=___:=____:=_____:=______:=_______:=________: if whl;echo d%>irectory %<$0%> is not an allowed directory. ^m^j%<t%>he valid directories are:^m^j;pwd;fi;echo

