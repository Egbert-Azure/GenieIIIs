(*
 * Chain - replaces former CHAINDU
 * Herbert Oppmann 1/93
 * needs NewChain.INC, Z3Env.INC, NamedDir.INC
 *)

{at the beginning, for installation purposes}
{to facilitate installation with a debugger there are labels before each item}
CONST
  (*
   * search path (as in XRUN)
   * First byte is drive (1-16 = A-P), second byte is user (0-31)
   * ORD('$') =24h =36d for current drive/user
   * an entry with drive = 0 terminates the list if there are less than 5 entries
   *)
  IPath_Magic : ARRAY[0..8] OF CHAR = '[INTPATH>';
  IPath : ARRAY[0..4] OF ARRAY[0..1] OF BYTE =
    ((0,0), (0,0), (0,0), (0,0), (0,0));

  (*
   * filename of CHN-library (extension .LBR is needed)
   * leave empty if you don't want library search
   *)
  ChainLib_Magic : ARRAY[0..9] OF CHAR = '[CHAINLIB>';
  ChainLib : STRING[22] = '';

  (*
   * filename of extended command processor (ECP) that is to be called
   * if CHAIN fails (extension .COM is needed)
   * leave empty if you don't want another ECP to be started
   *)
  CmdProz_Magic : ARRAY[0..8] OF CHAR = '[CMDPROC>';
  CmdProz  : STRING[22] = '';

{$I NewChain.INC}
{$I Z3Env.INC}
{$I NamedDir.INC}

CONST Dollar = $24;
TYPE  PName = ARRAY[0..10] OF CHAR;

VAR
  f : FILE;
  FName : FNameT;
  CmpName : PName;
  i, j : INTEGER;


PROCEDURE Hallo; {no return}
BEGIN
  WriteLn('Comfortable Chain (C) 8/92 Herbert Oppmann');
  Write('Syntax: CHAIN <DU:/DIR:>name <parameter ...>');
  Halt;
END; {Hallo}


{search CHN at the given drive/user}
PROCEDURE TryDU;
VAR
  fib : {this is just for having easy access to the parsed filename}
   RECORD
    dummy : ARRAY[0..12] OF BYTE;
    fn : PName;
   END ABSOLUTE f;
BEGIN
  Assign(f, Dir2Du(FName));
  CmpName := fib.fn; {save the parsed name for later use}
  Chain(f);
  {we only come here if something went wrong}
  IF (IORes <>1) THEN ChkIORes;
  {handle only 'File does not exist', report other errors}
END; {TryDU}


{search CHN along internal path}
PROCEDURE TryInternalPath;
VAR
  d, u : INTEGER;
  TmpFN : FNameT;
BEGIN
  FOR i := 0 TO 5 DO
    BEGIN
    d := IPath[i][0];
    if (d =0) THEN Exit;
    u := IPath[i][1];
    IF (d =Dollar) THEN TmpFN := '' ELSE TmpFN := Chr(d+64);
    IF (u <>Dollar) THEN TmpFN := TmpFN+Chr((u DIV 10)+48)+Chr((u MOD 10)+48);
    IF (TmpFN ='') THEN TmpFN := FName ELSE TmpFN := TmpFN+':'+FName;
    Assign(f, TmpFN);
    Chain(f);
    IF (IORes <>1) THEN ChkIORes;
    END; {FOR}
END; {TryInternalPath}


{try the library}
PROCEDURE TryLibrary;
TYPE
  DirEntry =
   RECORD
    Status : BYTE;
    Name   : PName;
    Index, Len, Crc,
    CreateD, ChangeD, CreateT,
    ChangeT : INTEGER;
    PadCnt : BYTE;
    Filler : ARRAY[27..31] OF BYTE
   END;
VAR
  dir : ARRAY[0..3] OF DirEntry;
  dirlen : INTEGER;
BEGIN
  IF (ChainLib ='') THEN Exit;
  IF (Pos('.',ChainLib) =0) THEN ChainLib := ChainLib+'.LBR';
  IORes := 0;
  Assign(f, Dir2Du(ChainLib));
  {$I-}
  Reset(f);
  {$I+}
  IF (IORes =1) THEN
    BEGIN
    IORes := 0;
    WriteLn('Chain: Library ',ChainLib,' not found.');
    Exit;
    END;
  IF (IORes <>0) THEN ChkIORes;
  BlockRead(f,dir,1);
  WITH dir[0] DO
    BEGIN
    IF (Status <>0) OR (Name <>'           ') OR (Index <>0) THEN
      BEGIN
      Write('Chain: ',ChainLib,' is no library.');
      Exit;
      END;
    DirLen := Len;
    END;
  i := 1;
  WHILE (DirLen >0) DO
    BEGIN
    WITH dir[i] DO
      IF (Status =0) THEN
        IF Name=CmpName THEN {no return}
          BEGIN
          Seek(f, Index);
          NCLib := Len;
          Chain(f);
          ChkIORes;
          END; {IF, IF, WITH}
    i := Succ(i);
    IF (i >3) THEN
      BEGIN
      i := 0;
      DirLen := Pred(DirLen);
      IF (DirLen >0) THEN BlockRead(f,dir,1);
      END;
    END; {WHILE}
END; {TryLibrary}


PROCEDURE TryECP;
BEGIN
  IF (CmdProz ='') THEN Exit;
  NCCmdLine := ParamStr(1)+' '+NCCmdLine;
  IF (Pos('.',CmdProz) =0) THEN CmdProz := CmdProz+'.COM';
  Assign(f, Dir2Du(CmdProz));
  Execute(f);
  IF (IORes =1) THEN
    BEGIN
    IORes := 0;
    WriteLn('Chain: ECP ',CmdProz,' not found.');
    Exit;
    END;
  ChkIORes;
END; {TryECP}


BEGIN {Chain}
{is there something to do?}
  i := ParamCount;
  IF (i =0) THEN Hallo;
{prepare name of the CHN-file}
  FName := ParamStr(1);
  IF (Pos('.',FName) =0) THEN FName := FName+'.CHN';
{build command line}
  NCCmdLine := '';
  j := 2;
  WHILE (j <=i) DO
    BEGIN
    NCCmdLine := NCCmdLine+' '+ParamStr(j);
    j := Succ(j);
    END;
  TryDU;             {given drive/user}
  {if a DU:/DIR: was given we don't search}
  i := Pos(':',FName);
  IF (i =0) THEN
    BEGIN
    TryInternalPath; {internal path}
    TryLibrary;      {Library. TryDU must be called first because of CmpName}
    TryECP;          {other command processor}
    END; {IF}
  IORes := 0;
  WriteLn('Chain: ',FName,' not found.');
END. {Chain}
