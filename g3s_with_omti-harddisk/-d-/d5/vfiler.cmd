  TITLE:  C Language CMD Macro File
  %d%u:;get 100 ws.com;poke 392 ff;go %f
1 %d%u:;get 100 nw.com;poke 74e ff;poke 787 ff;go %f
2 if ex %d%u:%n.c;%d%u:;era %n.bak;pcc %f;c %n;as %n;era %n.asm;else;sak /p3 not c file type;fi
3 if ex %d%u:%n.c;%d%u:;era %n.bak;pcc %f;cc %f;clink %n -ns;era %n.crl;else;sak /p3 not c file type;fi
4 if ex %d%u:%n.c;%d%u:;era %n.bak;pcc %f;czii -t %f;as %n.asm;ln %n.o z80math.lib z80libc.lib;era %n.o;else;sak /p3 not c file type;fi
5 %d%u:;c %n;as %n;BASE:lzed %$
6 xdir %d%u:*.* ogoh'Options (u=all areas, p=to printer, d=disk, CR=default): '
7 MAIL:;ROOT:menu t3.mnu
8 ROOT:vtype %$
9 if ex $d%u:%n.lbr;%d%u:;nulu -o %n -f;else;ROOT:sak /p3 file is not a library;fi
0 %d%u:;BASE:lzed %$
#

                    >>> C LANGUAGE PROGRAM DEVELOPMENT <<<

      ==================================================================
       1 - EDIT Pointer File (non-document mode)          (Newword) - 1

           Compile/Assemble/Link/Load Using:
       2 - Software Toolworks C/80 v3.0                      (C/AS) - 2
       3 - BD Systems BDS-C v1.5a                        (CC/CLINK) - 3
       4 - Manx Aztec CII v1.05c                       (CZII/AS/LN) - 4

       5 - C Language Program Development         (C/80 using LZED) - 5

       6 - Directory of Files on Current Disk                (XDIR) - 6
       7 - Telecommunicate via Modem (from MAIL directory)  (Term3) - 7
       8 - View Pointer File with Forward/Backward Scroll   (VTYPE) - 8
       9 - Enter Pointer File Library (LBR)                  (NULU) - 9
       0 - EDIT Pointer File using LZED                      (LZED) - 0
      ==================================================================

r File using LZED                      (LZED) - 0
      