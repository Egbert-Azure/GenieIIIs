COPYD,ISK         -r copyd
DIRSORT=CLEANDIR  -r cleandir $*
DR                -r dr
FILEMAP=FILES     -r filemap $*
FIND,E            -r find $*
FNDBD=FINDBAD=BAD -r fndbd $*
FORMAT,IERE       -r format $*
FRE,E             -r free $*
INITDIR           -r initdir $*
MLOAD             -r mload $*
LZH               -r lzh $*
SETCOM=SETSIO=SIO -r setcom $* 
TELL              -r tell
TOUCH             -r touch $*
UNFREEZE          -r unfreeze
UNLOAD=COMHEX     -r unload $* 
ZS                -r zs $*

GEN2S     -r -form gen2s
GEN3S     -r -form gen3s
MOD4      -r -form mod4
PROF4     -r -form prof4

ADD-LF    -c add-lf $* 
ARC       -c arc $* 
ASCII     -c ascii $* 
CRCTEST   -c crctest $*
DPAR      -c dpar
DRUCKER   -c drucker
INPPDR    -c inppdr $*
MARK,RES  -c markres $*
MEMO      -c memo
OPTIM     -c optim $*
REPL,ACE  -c replace $*
TAB       -c tab $*
TABEXP    -c tabexp $*
TILG,E    -c tilg
TRANS,FER -c transfer $*
WS4TO,CPM -c ws4tocpm

BDOSINFO  rsm bdosinfo
EDLIN     rsm edlin
FREEZE    rsm freeze
GRAFTREI  rsm graftrei
PRN       rsm prn

CHECK     zex e:check

RAM,:=RAMD,:=RAM,DISK:=M0,: ramd:
DISI,:=E0,:=UTIL,:          util:
ROOT,:=A0,:=BASE,:          base: 
FOREI,:=FOREIGN,:=C0,:      foreign:

COP,Y=KOPI,ERE=AC       ac $*
COMP,ARE=DIFF           diff $*
CRUN,CH                 cr23d3 $*
DEB,UG=DDTZ             ddtz $*
DIR,ECTORY=INH,ALT=I    i $*
DU,3                    du
ED,IT=ZDE               zde $*
ERA,SE=KIL,L=DEL,ETE    era $*
FILER=MAKE=MOVE         filer $*
SUCH,E=FF=WO            ff $*
HILF,E=H                h
LD,IR                   i $1 /l
LOOK                    get 100 $td1$tu1:$tf1;p 100
MU,3                    mu
NULU                    nulu $*
PRINT=DRUCK,E=LIST,E    list $*
RENA,ME                 rena $*
TYP                     type $1 p
UNA,RC=UNAR             unarc $*
UNCRUN,CH=UNC,R         uncr $*
UNERA,SE                filer $1 u
UNL,ZH=UNLZ             unlzh $*
VF,ILER                 vf $*
VLU                     vlu $*
WHL=WHE,EL              wheel $*
ZEIG,E=TYPE             type $*
ZF,ILER                 zf $*

PBIOS=BIOS              p $ab
PCCP=CCP=PCPR=CPR       p $ac
PDOS=DOS=BDOS           p $ad
PENV=ENV                p $ae
PFCP=FCP                p $af
PIOP=IOP                p $ai
PMCL=MCL                p $al
PMSG=MSG                p $am $+m004f
PNDR=NDR                p $an
PPATH                   p $ap $+p000f
PRCP=RCP                p $ar
PSHL=PSHELL=SHL=SHELL   p $as $+s007f
STACK                   p $+b1000 $+b10ff

_:=__:=___:=____:=_____:=______:=_______:=________: if whl;echo d%>irectory %<$0%> is not an allowed directory. ^m^j%<t%>he valid directories are:^m^j;pwd;fi;echo

