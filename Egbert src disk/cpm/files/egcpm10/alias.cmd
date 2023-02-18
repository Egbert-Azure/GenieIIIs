
		SAMPLE ALIAS.CMD FILE FOR USE WITH Z3PLUS
		-----------------------------------------

 ; This alias provides a response to the DIR command when there is no
 ; resident DIR or transient DIR.COM.

DIR		sdz $*

 ; This alias provides a directory-display command in which the file
 ; specification is automatically wildcarded.  It saves the trouble of
 ; having to type asterisks manually.

D=SD/		sdz $td1$tu1:$tn1*.$tt1* $-1

 ; This alias is used with the syntax NAME DU:DIR PW or NAME DIR PW to assign
 ; the named directory DIR to the designated (or current) drive/user.  The
 ; second token is optional; if present, it will assign a password as well.

NAME=SETNAME	editndr $td1$tu1:$tn1 $2 \x

 ; This alias will save the currently defined named directories in the file
 ; A0:DEFAULT.NDR.  If a command tail beginning with 'L' (for library) is
 ; included, the file will be put into Z3PLUS.LBR so that it will automatically
 ; be loaded at Z-System cold boot and the individual NDR file will be erased.

PUTNDR		savendr a0:default;if eq $1 l*;lput a0:z3plus a0:default.ndr;era a0:default.ndr;fi

 ; This alias will allow you to examine a file.  It loads the file into the
 ; TPA and then uses the RCP peek command to examine it.  It will load a file
 ; from the current directory even if that directory is not on the path.

LOOK		get 100 $td1$tu1:$tf1;p 100

 ; This alias removes Z3PLUS and restores the operation of CP/M.

OFF=CPM=EXIT	z3plus off

 ; This alias provides a shorthand for the CRUNCH command.

CR,UNCH		crunch $*

 ; The following pair of aliases automates a recursive invocation of some
 ; other command line.  It is invoked as RECURSE COMMANDLINE.

REC,URSE	if nu $1;echo;echo %<  s%>yntax: %<$0 cmdname [parameters];echo;else; recurse2 $*;fi
RECURSE2	fi;$*;if in r%>un %<"$1" %>again? ; $0 $*

 ; The following pair of aliases automates the repeated invocation of a
 ; command that takes a single argument.  It is invoked as
 ; REPEAT COMMAND ARG1 ARG2 ... ARGN.  The command COMMAND is executed
 ; in sequence, first with ARG1, then with ARG2, and so on.

REP,EAT		if nu $2;echo;echo %<  s%>yntax: %<$0 cmdname arg1 arg2 ...;echo;else; repeat2 $*;fi
REPEAT2		fi;$1 $2;if ~nu $3; repeat2 $1 $-2

 ; Das ist ein ALIAS um dasselbe Programm (USERLIST) mit anderen Daten zu
 ; verwenden. An 100 HEX geladen - ge{ndert und ausgef}hrt! 21E8=L{nge! 

ZVERTEIL	get 100 userlist.com;poke 21e8 08;poke 21e9 "ZVERTEIL";go $1
 ; Memory display aliases -- these aliases use the peek command to show the
 ; contents of various system modules


PBIOS=BIOS		p $ab
PCCP=CCP=PCPR=CPR	p $ac
PDOS=DOS		p $ad
PENV=ENV		p $ae
PFCP=FCP		p $af
PMCL=MCL		p $al
PMSG=MSG		p $am $+m004f
PNDR=NDR		p $an
PPATH			p $ap $+p000f
PRCP=RCP		p $ar
PSHL=PSHELL=SHL=SHELL	p $as $+s007f
PXFCB=XFCB=PFCB=FCB	p $ax $+x0023

  ;Dies ist ein Alias um eine Diskette zu formatiern. Danach wird
  ;eine Bootspur aufgebracht und das Inhaltsverzeichnis eingerichtet.
  ;Nun wird noch CPM3.SYS,CONFIG.SYS,CCP.COM und PIP.COM auf die
  ;neue Diskette aufgebracht. Man erh{lt eine bootf{hige System-
  ;didddskette.

NDF      FORMAT B:;ID B:S80 DSDD;INITDIR B:;y;SET B:[UPDATE=ON];SET B:[ACCESS=ON];pip b:=a:config.sys[r];pip;b:=a:cpm3.sys[r];b:=a:ccp.com[r];b:=a:pip.com[r];
