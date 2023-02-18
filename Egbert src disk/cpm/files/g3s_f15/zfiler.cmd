0 ! $"Enter ZFILER macro script: "
A ! IF eq $t ARC ;$d$u:;$d$u:;zex pkxarc $f;else;echo N%ot an .ARC f%ile !;fi 
B ! IF ~eq $t com and ~eq $t ar?;$d$u:;BISHOW $f;else;echo ^A f%>ile %<$f%> is not viewable! ^B;fi
C ! echo Not yet implemented!
D ! if eq $t lbr;$d$u:;ldir $f;else;$d$u:;SDZ $f $l;fi
E ! $d$u:;get 100 a:zde.com;poke 392 ff;go $f
F ! $D$u:;DA2
H ! ZHELP $F  
K ! $d$u:;$!crunch $f $"Destination directory: "$"Comment in []? ";$h:
L ! $d$u:;F0:LOADND $"Login and update which Drive/User? "
M ! d0:;MENU;$HB
N ! $d$u:;NSWEEP;$h:
Q ! $d$u:;f0:SQ $f;$h:
R ! IF ~eq $t com and ~eq $t ar? and ~eq ?z?;$d$u:;view $f;else;echo ^A f%>ile %<$f%> is not viewable! ^B;fi
T ! $d$u:;v $p
S ! if eq $t ?z? ;echo  PLEASE - N%>o %<crunched%>- files;else;$d$u:;SFCPC $f;fi
U ! if eq $t ?z?;$d$u:;uncr $f;$h:;else;If eq $t ?y?;$d$u:;UCRLZH11 $f;FI
X ! if ~eq $t com;echo n%>ot a %<com%> file;else;$d$u:;:$n $" Command Tail: ";$h:;fi
V ! $d$u:;vlu;$h:
W ! $d$u:;get 100 ws+.com;poke 392 ff;go $f
Z ! $d$u:;$" Command to perform on file: " $f $" Tail: ";$h:
#
	SAMPLE ZFILER COMMAND MACROS FOR USE WITH NZCOM AND Z3PLUS

macros:
        A.  ARC-viewing/extracting         N.  NSWEEP Multi-User op.  
        B.  Bidirect. textview even LBR    O. 
        C.  Copy from  Filelist            P.  
        D.  Display  LBR/ARC/ARK           Q.  Squeeze file
        E.  Edit file pointed to           R.  Read with cntrl chars 
        F.  Filespace occupied             S.  Show large ASCII File  
        G.                                 T.  Type it by V (.?Y? too)
        H.  HELP-DATABASE for ZCPR         U.  Uncrunch standard+.?Y? 
        I.                                 V.  VLU Library utility
        J.                                 W.  edit with Wordstar
        K.  "K"runch the file              X.  eXecute the file
        L.  Log in and update D/U          Y.  
        M.  ===> Menu-System <====         Z.  perform command on file
       
                  =======  0.  on-line macro =======

               ZFILER parameters for use with macro '0' 
$!     ZEX 'GO'		$P  DU:FN.FT	$D  DRIVE $'..'  PROMPT  	$N  FN
$".."  PROMPT  		$F  FN.FT	$U  USER    $H  HOME DU		$T  FT
