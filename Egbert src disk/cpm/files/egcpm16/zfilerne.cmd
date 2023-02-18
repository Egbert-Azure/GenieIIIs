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
1cls;a15:if $t=com;or $t=lbr;pause /p2 c%>annot edit < %<$f%> >;else;poke f220 0;ws $p $"N <CR> for Non-doc mode ";poke f220 4;fi
2!$d$u:;cls;a15:if $t=com;pause /p2 < $f >%> cannot be viewed;else;if $t=lbr;ldir $n;gtv fv file to view;rsv zlt $n %fv;else;zlt $p;fi;fi
3$d$u:;cls;if compr $f;uncr $f /q;else;if input c%>runch < %<$f%> > (y/n)? ;crunch $f /q;else;a9:sq $f;fi;fi;era $f i
4$d$u:;cls;a15:if in d%>isplay attributes of%< $f %>(y/n)? ;dfa $f;gtv sd set which attribute?;rsv sfa $f /%sd;else;gtv sd set which attribute?;rsv sfa $f /%sd;fi
5!$d$u:;/compare $" [du:ufn] to compare " $p
6$d$u:;cls;a15:if ne $t com;pause /p2 < $f >%> is not a executable command file you are trying to install;else;z3ins $p;fi
7$d$u:;cls;a15:if ne $t lbr;pause /p2 < $f >%> is not a library!;else;a15:if input (y)%>...for %<v%>lu or (n)...for nulu ;a15:vlu $n;else;a9:nulu -o $n -h;fi;fi
8$d$u:;cls;/print $f
9$d$u:;/copyinto $" Enter new filename--> " $p
0cls;if input < u%>se zpatch (y/n)? ;$d$u:;zpatch $p;else;patch;fi
Acls;$" [d/u] or [dir] ":;a15:vmenu *.* asm.vmn
Bcls;a15:vmenu *.* menu.vmn
Ccls;b6:;a15:vmenu *.* term3.vmn
Dcls;b9:;a15:vmenu *.* discat.vmn
Ecls;$" [d/u] or [dir] ":;a15:vmenu *.* ws.vmn
F!cls;findf $" File(s) to search for "
G$d$u:;if ne $t z80;cls;pause /p2 n%>ot a z80 file;else;cls;a11:zas $n;pause /p5 e%>rrors, cancel link?;a11:zlink $n,a:vlib/,a:z3lib/,a:z33lib/,a:syslib/;era $n.* i;fi
H$d$u:;if ne $t asm;cls;pause /p2 n%>ot an asm file;else;cls;a11:mac $n $$-s pz;pause /p5 e%>rrors, cancel link?;a11:mload $n;era $n.bak i;era $n.hex;fi
I$d$u:;if ne $t mac;pause /p2 not a mac;else;cls;a11:m80 =$n;pause /p5 e%>rrors, cancel link?;a11:l80 /p:100,$n,a:vlib/s,a:z3lib/s,a:z33lib/s,a:syslib/s,$n/n,/u,/e;era $n.*;fi
J!$d$u:;if ne $t mac;or $t asm;pause /p3 c%>annot convert %<< $f >;else;cls;a11:xiz $p;era $f i
K$d$u:;if ne $t z80;cls;pause /p2 n%>ot a z80 file;else;cls;a11:zas $n h;pause /p5 e%>rrors, cancel link?;a11:mload $n;era $n.bak i;era $n.hex;fi
L$d$u:;if ne $t asm;cls;pause /p2 n%>ot an asm file;else;cls;a11:rmac $n $$-s pz;pause /p6 E%>rrors, cancel link?;a11:mload $n;era $n.bak i;era $n.hex;fi
M$d$u:;if in r%>un ddt with %<< $f >;cls;a11:ddt $f;else;cls;a11:ddt;fi
Ncls;b4:;b4:notepad;cls;$h:
O$d$u:;if in r%>un sid with %<< $f >;cls;a11:sid $f;else;cls;a11:sid;fi
Pcls;echo c%>urrent printers:;/cpsel dp;gtv pr select (0-3);rsv cpsel p%pr;if input r%>econfigure selected printer? ;b3:;.oki;/cpsel dp;/prtset;gtv cl set params -> prtset ;rsv prtset %cl;else;$h:;fi
Qa9:end
R!$d$u:;cls;if ne $t lbr;pause /p2 < $f >%> is not a library!;else;lput $p $"Filename.ext to insert into library " ;fi
S$d$u:;cls;if input p%>lace new alias in alias directory (y/n)?;a12:;gtv ali Enter alias name ;rsv salias %ali;else;gtv ali Enter alias name ;rsv salias %ali;$h:fi;fi
Tcls;a15:if input u%>se %<t%>urbo %<p%>ascal...(y) or %<m%>odula...(n) ;b1:;turbo;else;a13:;m2;fi;$h:
U!$d:;cls;unerase *.* l;a15:if input u%>nerase file(s) (y/n)? ;gtv une which file(s);rsv unerase %une z;echo f%>ile(s) found have been unerased to user 0;else;echo no f%>iles %<u%>nerased;fi
Va15:vcomp $p $" [d/u] of text file to compare ":$" filename.ext " /
W!cls;findf26 c:*.*;$h:
X$d$u:;/prtdrv $p
Y$d$u:;cls;if $t=cmd;dbase $f;else;pause /p2 < $f%> > is not a dbase command file!;fi
Zmu3
#              ---=== ZFILER  EXTENDED  OPERATIONS  MENU ===---

  *----------->>> File Operations <<<----------*   *---->>> VMenus <<<----*
  | 1 - Edit               Install Z3 File - 6 |   |    A - Assembly      |
  | 2 - View             Open Library File - 7 |   |    B - General       |
  | 3 - Compress/Uncompress     Print File - 8 |   |    C - Term3         |
  | 4 - Set Attribute     Copy to New File - 9 |   |    D - Discat        |
  | 5 - Compare                     Zpatch - 0 |   |    E - Editing       |
  *--------------------------------------------*   *----------------------*

  *------------>>> Specialized Operations <<<------------*   *->> Misc <<-*
  | G - Assemble/ZLink -> .Z80's <-   Assemble/MLoad - K |   | F - Findf  |
  | H - Assemble w/Mac -> .ASM's <-  Assemble w/Rmac - L |   | N - Notes  |
  | I - Assemble w/M80 -> .MAC's             Use DDt - M |   | Q - Quit   |
  | J - Convert Source to .Z80 Code          Use Sid - O |   | S - sAlias |
  | P - Configure Printer    Print w/Special Printer - X |   | T - Turbo  |
  | R - Insert named file into Current Library           |   | U - Unerase|
  | W - View File Directories in "C" drive               |   | V - Vcomp  |
  | Y - Run Dbase command file                           |   | Z - Mu3    |
  *------------------------------------------------------*   *------------*

