{ Turbo Pascal is a Structured language.  The use of Structure
  is not as debilitating as you might think.  It helps the
  programmer to organize his thoughts and, more importantly,
  helps the Compiler find errors BEFORE you run the program. }

(* User-Defined Global Types *)

Type
  STR4    = String[4];              { Used for HEX conversions     }
  STRNG   = String[80];             { General purpose input string }
  FCBARR  = Array[0..15] of Byte;   { Any File Control Block       }
  SECTOR  = Array[0..127] of Byte;  { Standard CP/M Unit Record    }

{ The following Record Structures define the Z3 Environment of
  any NZ-System.  We don't know the addresses yet.  Later. }

  ENVREC =
    Record
      env     : Byte;
      cbios   : Integer;
      z3id    : Array[1..5] of Char;
      envtyp  : Byte;
      expath  : Integer;
      expaths : Byte;
      rcp     : Integer;
      rcps    : Byte;
      iop     : Integer;
      iops    : Byte;
      fcp     : Integer;
      fcps    : Byte;
      z3ndir  : Integer;
      z3ndirs : Byte;
      z3cl    : Integer;
      z3cls   : Byte;
      z3env   : Integer;
      z3envs  : Byte;
      shstk   : Integer;
      shstks  : Byte;
      shsize  : Byte;
      z3msg   : Integer;
      extfcb  : Integer;
      extstk  : Integer;
      quiet   : Byte;
      z3whl   : Integer;
      speed   : Byte;
      maxdsk  : Byte;
      maxusr  : Byte;
      duok    : Byte;
      crt     : Byte;
      prt     : Byte;
      cols    : Byte;
      rows    : Byte;
      lins    : Byte;
      drvec   : Integer;
      spar1   : Byte;
      pcol    : Byte;
      prow    : Byte;
      plin    : Byte;
      form    : Byte;
      spar2   : Byte;
      spar3   : Byte;
      spar4   : Byte;
      spar5   : Byte;
      ccp     : Integer;
      ccps    : Byte;
      dos     : Integer;
      doss    : Byte;
      bio     : Integer;
      shvar   : Array[1..11] of Char;
      file1   : Array[1..11] of Char;
      file2   : Array[1..11] of Char;
      file3   : Array[1..11] of Char;
      file4   : Array[1..11] of Char;
      public  : Integer;
    End;

{ This describes the Z3MSG structure. }

  MSGREC =
    Record
      erflg   : Byte;
      iflev   : Byte;
      ifsts   : Byte;
      cmdst   : Byte;
      eradr   : Integer;
      prger   : Byte;
      zexmsg  : Byte;
      zexrun  : Byte;
      zexnxt  : Integer;
      zex1st  : Integer;
      shctl   : Byte;
      scrat   : Integer;
      ercmd   : Array[1..32] of Char;
      regis   : Array[0..31] of Byte;
      exfcb   : FCB;
      expat   : Array[1..5] of Integer;
      expt    : Byte; { expat terminator, always 0 }
      whl     : Byte;
    End;

{ This describes the Z3CL structure for 'standard' Z-Systems. }

  MCLREC =
    Record
      nxtchr  : Integer;
      mclmax  : Byte;
      mcl     : String[203];
    End;

