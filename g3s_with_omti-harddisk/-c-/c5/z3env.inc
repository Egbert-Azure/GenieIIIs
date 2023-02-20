(*
 * Z3Env.INC - Herbert Oppmann 8/92
 * See ZCPR documentation.
 * The definition of the structure of the named directory buffer is
 * required by NamedDir.INC. The other structures have been added to
 * give you an example of how to complete the data structure.
 *)
TYPE
  PathTyp = ARRAY[0..99] OF RECORD drive, user : BYTE END;

  NdrTyp  = ARRAY[0..99] OF RECORD
    drive : BYTE;
    user  : BYTE;
    name  : ARRAY[1..8] OF CHAR;
    pwd   : ARRAY[1..8] OF CHAR;
    END;

  MclTyp  = RECORD
    nxtchr : INTEGER; {^CHAR}
    bufsiz : BYTE;
    chrcnt : BYTE;
    cmdlin : ARRAY[1..255] OF CHAR;
    END;

  EnvTyp  = RECORD
    jump  : ARRAY[1..3] OF BYTE;
    z3id  : ARRAY[1..5] OF CHAR;
    typ   : BYTE;
    path  : ^PathTyp;
    paths : BYTE;
    rcp   : INTEGER; {^RcpTyp}
    rcps  : BYTE;
    iop   : INTEGER; {^IopTyp}
    iops  : BYTE;
    fcp   : INTEGER; {^FcpTyp}
    fcps  : BYTE;
    ndr   : ^NdrTyp;
    ndrs  : BYTE;
    mcl   : ^MclTyp;
    mcls  : BYTE;
    {usw.}
    END;

VAR
  Env : ^EnvTyp ABSOLUTE $0109;
