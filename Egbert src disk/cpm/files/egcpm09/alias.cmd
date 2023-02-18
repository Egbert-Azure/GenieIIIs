

INPUTERA	$"^j^mEnter files to erase as a filelist:  "IF ~NU $'L1;ERASE $'L1;SP;ELSE;ECHO ^M^J N%>OTHING ENTERED!;FI
INPUT		$Z$"^m^jEnter penultimate command ^[) ('EXIT' to exit ZEX completely):  ^[("$ZIF EQ $'E1 EXIT;ECHO ^M^JR%>ETURNING TO %<ZCPR3...;RS %CURDU;POKE $+M0008 0;ELSE;$'L1;FI
INPUTRET	$Z$"^m^jEnter final command ^[)('EXIT' to exit ZEX completely):  ^[("$ZFI;IF EQ $'E1 EXIT;ECHO ^M^JA%>BORTING %<ZEX %>SCRIPT...;RS %CURDU;POKE $+M0008 0;ELSE;$'L1;RS E R%>ETURNING TO %<ZEX %>SCRIPT AT LETTER%< %$RF05...;HOLD 2;FI

D,IR		SDZ $*

 ; THIS ALIAS PROVIDES A DIRECTORY-DISPLAY COMMAND IN WHICH THE FILE
 ; SPECIFICATION IS AUTOMATICALLY WILDCARDED.  IT SAVES THE TROUBLE OF
 ; HAVING TO TYPE ASTERISKS MANUALLY.

D!=SD/	        SDZ $TD1$TU1:$TN1*.$TT1* $-1

 ; THIS ALIAS IS USED WITH THE SYNTAX NAME DU:DIR PW OR NAME DIR PW TO ASSIGN
 ; THE NAMED DIRECTORY DIR TO THE DESIGNATED (OR CURRENT) DRIVE/USER.  THE
 ; SECOND TOKEN IS OPTIONAL; IF PRESENT, IT WILL ASSIGN A PASSWORD AS WELL.


NAME=SETNAME	EDITNDR $TD1$TU1:$TN1 $2 \X


 ; THIS ALIAS CAN HELP TO TEMPORARILY CHANGE A DU: NAME

RENDIR		NAME10 $1


 ; THIS ALIAS WILL SAVE THE CURRENTLY DEFINED NAMED DIRECTORIES IN THE FILE
 ; C0:Z3PLUS.NDR.  IF A COMMAND TAIL BEGINNING WITH 'L' (FOR LIBRARY) IS
 ; INCLUDED, THE FILE WILL BE PUT INTO Z3PLUS.LBR SO THAT IT WILL AUTOMATICALLY
 ; BE LOADED AT Z-SYSTEM COLD BOOT AND THE INDIVIDUAL NDR FILE WILL BE ERASED.

PUTNDR		SAVENDR c0:DEFAULT;IF EQ $1 L*;LPUT c0:z3plus c0:DEFAULT.NDR;ERA c0:DEFAULT.NDR;FI

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
 
MSDOS		fremd ibm-pc;z3plus small;c1:mstocpm;z3plus

 ; Mit Hilfe dieses ALIAS kann durch die Eingabe des jeweiligen Buchstabens
 ; der Hilfsbildschirm des jeweiligen aufgerufen werden.
 
A=B=C=D=E=F=G=H=I=J=K=L=M=N=O=P=Q=R=S=T=U=V=W=X=Y=Z  lbrhlp -c15:$0 $0
  	


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
 
 ; Ab hier stehen ALIASE, mit denen der HI-TECH-C Compiler, der in C9:HI-TEC
 ; liegt, einfacher erreicht werden kann. Die Sources der C-Programme sollen 
 ; in einem anderen Userbereich zu liegen kommen, n[mlich in C11:CSOUR

CC    	C9:C -v -i9:c: $1.c
CCF	C9:C -v -i9:c: $1.c -lf

 ; wobei CC einen normalen Comiplierlauf initiert und CCf f]r Programme
 ; mit Fliesskommazahlen gedacht ist.
 



 ; Der ALIAS RAMINIT soll einige Programme auf die RAMDisk kopieren, 
 ; weil ich die wichtig finde.
 
INI,TRAM     zex initram


 ; MAKEDISK formatiert und initialisiert eine Diskette

MAKED,ISK 	zex makedisk $1


 ; MOVE kopiert ein oder mehrere Files, und l\scht dann die Quellfiles
 
MOVE	pipe $1 $2;if input ^AE%>RASE %<F%>ILE?%<^B ;era $1;fi
 


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
 
EH                era m15:lsh.cmd;m15:lsh m15:lsh.cmd

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
C-SO,URCES:=CSOU,RCES:			 c11:
       		
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



;Insert this into your ALIAS.CMD (or LSH.CMD for that matter!) - Rick Charnes

GST  cls;box 0 3 10 19 60;go 0 5 15 15 50;go 0 7 20 11 40;gxymsg 10 30 gst does exactly;go 12 32 what i want!


_:=__:=___:=____:=_____:=______:=_______:=________: if whl;echo d%>irectory %<$0%> is not an allowed directory. ^m^j%<t%>he valid directories are:^m^j;pwd;fi;echo

