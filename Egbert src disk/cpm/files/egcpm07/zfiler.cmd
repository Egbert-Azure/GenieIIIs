0 ! $"Enter ZFILER macro script: "
A ! $d$u:;zex pkxarc $f
B ! IF ~eq $t com and ~eq $t ar?;$d$u:;BISHOW $f;else;echo ^A f%>ile %<$f%> is not viewable! ^B;fi
C ! qlist $d$u:$f
D ! if eq $t lbr;$d$u:;ldir $f;else;$d$u:;SDZ $f $l;fi
E ! $d$u:;ZDE $f
G ! ZPATCH $d$u:$f
H ! HELP $P
K ! $d$u:;$!crunch $f $"Destination directory: "$"Comment in []? ";$h:
L ! $d$u:;F0:LOADND $"Login and update which Drive/User? "
M ! d0:;MENU;$HB
N ! $d$u:;Nulu $P:
P ! GET 100 $d$u:$f;p 100;p 180
Q ! $d$u:;f0:SQ $f;$h:
R ! $D$U:;F0:EXL $P
T ! $!f5:v $p
S ! if eq $t ?z? ;echo  PLEASE - N%>o %<crunched%>- files;else;$d$u:;SFCPC $f;fi
U ! if eq $t ?z?;$d$u:;uncunch $f;$h:;else;If eq $t ?y?;$d$u:;UCRLZH $f;FI
X ! if ~eq $t com;echo n%>ot a %<com%> file;else;$d$u:;:$n $" Command Tail: ";$h:;fi
V ! $d$u:;vlu;$h:
Z ! $d$u:;$" Command to perform on file: " $f $" Tail: ";$h:
#
	SAMPLE ZFILER COMMAND MACROS FOR USE WITH NZCOM AND Z3PLUS

macros:
        A.  ARC-viewing/extracting         N.  NSWEEP Multi-User op.
        B.  Bidirect. textview even LBR    O. 
        C.  QLIST even Crunched            P.  Look File
        D.  Display  LBR/ARC/ARK           Q.  Squeeze file
        E.  Edit file pointed to           R.  Read with cntrl chars
        F.                                 S.  Show large ASCII File
        G.  ZPATCH fname                   T.  Type it by V (.?Y? too)
        H.  HELP-DATABASE for ZCPR         U.  Uncrunch standard+.?Y?
        I.                                 V.  VLU Library utility
        J.                                 W. 
        K.  "K"runch the file              X.  eXecute the file
        L.  Log in and update D/U          Y. 
        M.  ===> Menu-System <====         Z.  perform command on file
       
                  =======  0.  on-line macro =======

               ZFILER parameters for use with macro '0' 
$!     ZEX 'GO'		$P  DU:FN.FT	$D  DRIVE $'..'  PROMPT  	$N  FN
$".."  PROMPT  		$F  FN.FT	$U  USER    $H  HOME DU		$T  FT
