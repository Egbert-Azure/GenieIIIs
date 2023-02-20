!
!     ################################################################
!     #    Zfiler Extended CMD File for single point general and     #
!     #     specialized operations of Z-System Micro Computers       #
!     #                 using  Zfiler vers. 1.0h                     #
!     #                                                              #
!     #              Copyright (C) 1988 by John T. Brown             #
!     #                     All Rights Reserved                      #
!     #                                                              #
!     #       May be used freely by the Z-System User Community      #
!     #                                                              #
!     ################################################################
!     #    Nur noch der Rumpf zu diesem System stammt von John       #
!     #    Anstatt VMENU sollte man wahrscheinlich besser MENU42     #
!     #    nehmen. VMENU gibt z.B. unter CP/M+ falsche Angaben       #
!     #    zum verbleibenden Disk-Platz                              #
!     #    (c) 1994 by Egbert Schroeer                               #
!     ################################################################ 
!
!     reminder --> $d=current disk 
!              --> $u=current user
!              --> $f=current file 
!              --> $p=current drive/user file
!              --> $n=filename only
!              --> $t=filetype only
!              --> $h:=home du:
!              --> $r:=home dir:
!              --> $(char) insert character
!
1 cls;if eq $t com;or eq $t lbr;pause /p2 c%>annot edit < %<$f%> >;else;zde $p /n ;fi
2 $d$u:;cls;if $t=com;pause  /p2  <  $f  >%> cannot be viewed;else;if $t=lbr;ldir $n;gtv fv file to view;rsv v $n %fv;else;v $p;fi;fi
3 $d$u:;cls;if compr $f;uncr $f /q;else;echo c%>runch < %<$f%>  >;if input ;$!crunch $f $"Destination directory: " $"Comment in []? ";else;sq $f;fi;fi;era $f i
4 $d$u:;cls;vren $f
5 $d$u:;/compare $" [du:ufn] to compare " $p
6 $d$u:;cls;if ne $t com;pause /p2 < $f >%> is not a  executable  command file you are trying to install;else;zcnfg $p;fi
7 $d$u:;cls;if ne $t lbr;pause /p2 < $f >%> is not a library!;else;echo O%>pen < $f > (y)%>...for %<vlu%>  or  (n)...for %<nulu%>;if input;vlu $n;else;nulu -o $n -h;fi;fi
8 $d$u:;cls;/print $f
9 $d$u:;/copyinto $" Enter new filename --> " $p
0 cls;echo  < u%>se zpatch (y/n)?  ;if input;$d$u:;zp $p;else;patch;fi
A cls;$" [d/u] or [dir] ":;dos:vmenu *.* asm.vmn
B cls;dos:vmenu *.* menu.vmn
C cls;dos:vmenu *.* term3.vmn
D cls;$" [d/u] or [dir] ":dos:vmenu *.* discat.vmn
E cls;$" [d/u] or [dir] ":;dos:vmenu *.* ws.vmn
F ! cls;ff $" File(s) to search for "
G $d$u:;if ne $t z80;cls;pause /p2 n%>ot a z80 file;else;cls;asm:zas $n;pause /p5               E%>rrors,               cancel               link?;asm:l80 /p:100,$n,c4:vlib/s,c4:z3lib/s,c4:z3libs/s,c4:syslib/s,$n/n,/u,/e;fi
H $d$u:;if ne $t asm;cls;pause /p2 n%>ot an asm file;else;cls;asm:mac $n  $$-s pz;pause /p5 e%>rrors, cancel link?;c4:mload $n;era $n.bak i;era $n.hex;fi
I $d$u:;if ne $t mac;pause  /p2 not  a  mac;else;cls;asm:m80  =$n;pause  /p5 e%>rrors,  cancel link?;asm:l80 /p:100,$n,c4:vlib/s,c4:z3lib/s,c4:z3libs/s,c4:syslib/s,$n/n,/u,/e;era $n.rel;fi
J ! $d$u:;if ne $t mac;or $t asm;pause  /p3  c%>annot  convert   %<<   $f >;else;cls;asm:xiz $p;era $f i
K $d$u:;if ne $t z80;cls;pause /p2 n%>ot a z80 file;else;cls;asm:zas $n h;pause /p5 e%>rrors, cancel link?;asm:mload $n;era $n.bak i;era $n.hex;fi
L $d$u:;if ne $t asm;cls;pause /p2 n%>ot an asm file;else;cls;asm:rmac $n  $$-s pz;pause /p6 E%>rrors, cancel link?;asm:mload $n;era $n.bak i;era $n.hex;fi
M $d$u:;if in r%>un ddt with %<< $f >;cls;asm:ddt $f;else;cls;asm:ddt;fi
N cls;dos:;dos:notepad;cls;$h:
O $d$u:;if in r%>un sid with %<< $f >;cls;asm:sid $f;else;cls;asm:sid;fi
P cls;echo c%>urrent printers:;/cpsel dp;gtv pr select (0-3);rsv cpsel  p%pr;echo   r%>econfigure selected printer? ;if input;dos:;.oki;/cpsel dp;/prtset;gtv cl  set params -> prtset ;rsv prtset %cl;else;$h:;fi
Q ! $d$u:;$" Command to perform on file: " $f $" Tail: ";$h:
R ! $d$u:;cls;if ne $t lbr;pause /p2 < $f >%> is not a  library!;else;lput  $p $"Filename.ext to insert into library " ;fi
S $d$u:;cls;echo p%>lace new alias in alias directory;if input;gtv ali Enter alias name ;rsv salias %ali;else;gtv ali Enter alias name  ;rsv  salias %ali;$h:fi;fi
T $d$u:;cls;box 0 7 20 5 45;gxymsg 9 22 Use Current File < $f >;if input;pascal:turbo $f;else;pascal:turbo;fi;$h:
U !$d:;cls;unerase  *.* l;echo u%>nerase file(s) ;if input ;gtv une which file(s);rsv  unerase  %une z;echo f%>ile(s) found have been unerased to user $u;else;echo no f%>iles %<u%>nerased;fi
V dbase:;dbase asv
W ! cls;/tree $d:
X $d$u:;/prtdrv $p
Y $d$u:;cls;if $t=cmd;dbase $f;else;pause /p2 < $f%> > is not a dbase  command file !;fi
Z mu3
#              ---===  ZFILER  EXTENDED  OPERATIONS  MENU  ===---
                    File Operations                             VMenus          
                                                                                
    1 - Edit                   Install Z3 File -6               A- Assembly     
    2 - View                 Open Library File -7               B- General      
    3 - Compress/Uncompress         Print File -8               C- Term3        
    4 - Modify File           Copy to New File -9               D- Discat       
    5 - Compare                         Zpatch -0               E- Editing      
                                                                                
                    Specialized Operations                           Misc       
                                                                                
    G - Assemble/ZLink -> .Z80's <-   Assemble/MLoad - K         F - Find File  
    H - Assemble w/Mac -> .ASM's <-  Assemble w/Rmac - L         N - Notes      
    I - Assemble w/M80 -> .MAC's             Use DDt - M         Q - DOS Com.   
    J - Convert Source to .Z80 Code          Use Sid - O         S - sAlias     
    P - Configure Printer    Print w/Special Printer - X         T - Turbo      
    R - Insert named file into Current Library                   U - Unerase    
    W - View named Directories on current HD                     V - ASV-Verw.  
    Y - Run Dbase command file                                   Z - Mu3        
                                                                                
                                                                                 
