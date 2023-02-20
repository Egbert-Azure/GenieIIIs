(****************************************************************************)
(*     Datei-Operationen                                                    *)
(*                    Oeffnen einer bestehenden Datei                       *)
(****************************************************************************)

 
procedure OeffneDatei(Name : FileNamestr; var f : DatFile; var Maske : MaskPtr;
                      var FileGroesse, SatzGroesse, RecZahl, okay : integer);
  var i,p,HeapSize : integer;
  begin
    (*$I-*) assign(f,Name); reset(f); okay:=IOResult; if okay>0 then exit;
    blockread(f,FilePuffer,16); okay:=IOResult; if okay>0 then exit;
    move(FilePuffer[1],FileGroesse,2);
    move(FilePuffer[3],SatzGroesse,2);
    move(FilePuffer[5],RecZahl,2); HeapSize:=RecZahl*SizeOf(MaskEintr);
    if MaxAvail>0 then
      if MaxAvail*MemConst<HeapSize then begin okay:=-1; exit end;
    getmem(Maske,HeapSize); p:=7;
    for i:=1 to RecZahl do with Maske^[i] do
      begin
        move(FilePuffer[p],x,1); p:=p+1;
        move(FilePuffer[p],y,1); p:=p+1;
        move(FilePuffer[p],m,succ(FilePuffer[p])); p:=p+succ(length(m));
        move(FilePuffer[p],l,1); p:=p+1;
        move(FilePuffer[p],sl,1); p:=p+1;
        move(FilePuffer[p],Art,1); p:=p+1
      end;
    (*$I+*)
  end; (* OeffneDatei *)

(****************************************************************************)
(*                      Schliessen einer Datei                              *)
(****************************************************************************)

 procedure SchliesseDatei(var f : DatFile;
             var Maske : MaskPtr; FileGroesse, SatzGroesse, RecZahl : integer;
             var okay : integer);
  begin
    (*$I-*) seek(f,0); okay:=IOresult; if okay>0 then exit;
    blockread(f,FilePuffer,1); okay:=IOresult; if okay>0 then exit;
    move(FileGroesse,FilePuffer[1],2);
    seek(f,0); blockwrite(f,FilePuffer,1); okay:=IOresult; if okay>0 then exit;
    close(f) (*$I+*); okay:=IOResult; if okay>0 then exit;
    if Maske=NIL then begin write(^G'NIL!'); readln end
                 else freemem(Maske,RecZahl*SizeOf(MaskEintr));
    Maske:=NIL
  end; (* SchliesseDatei *)

(****************************************************************************)
(*                     Erzeugen einer neuen Datei                           *)
(*                Datei ist nach dem Erzeugen geoeffnet                     *)
(****************************************************************************)

 
procedure NeueDatei(Name : FileNameStr; var f : DatFile; Maske : MaskPtr;
                    FileGroesse, SatzGroesse, RecZahl : integer;
                    var okay : integer);
  var i,p : integer;
  begin
    (*$I-*) assign(f,Name); rewrite(f); okay:=IOResult; if okay>0 then exit;
    move(FileGroesse,FilePuffer[1],2);
    move(SatzGroesse,FilePuffer[3],2);
    move(RecZahl,FilePuffer[5],2); p:=7;
    for i:=1 to RecZahl do with Maske^[i] do
      begin
        move(x,FilePuffer[p],1); p:=p+1;
        move(y,FilePuffer[p],1); p:=p+1;
        move(m,FilePuffer[p],succ(length(m))); p:=p+succ(length(m));
        move(l,FilePuffer[p],1); p:=p+1;
        move(sl,FilePuffer[p],1); p:=p+1;
        move(Art,FilePuffer[p],1); p:=p+1
      end;
    blockwrite(f,FilePuffer,16); okay:=IOResult; if okay>0 then exit;
    (*$I+*)
  end; (* NeueDatei *)

(****************************************************************************)
(*                         Ausgabe eines Eintrages                          *)
(****************************************************************************)

 procedure SchreibDat(var f : DatFile; Nr : integer;
                     Var Dat; Size : integer; var okay : integer);
  var PufferPos, Block, BlockAnzahl, ZuLesen : integer;
      BytePos : real;
  begin
    BytePos:=2048.0+1.0*Size*pred(Nr); Block:=trunc(BytePos/128);
    PufferPos:=succ(trunc(BytePos-128.0*Block));
    BlockAnzahl:=succ(pred(Size+PufferPos) SHR 7);
    ZuLesen:=FileSize(f)-Block;
    if ZuLesen>=BlockAnzahl then ZuLesen:=BlockAnzahl;
    (*$I-*) seek(f,Block); okay:=IOResult; if okay>0 then exit;
    BlockRead(f,FilePuffer,ZuLesen); okay:=IOResult; if okay>0 then exit;
    move(Dat,FilePuffer[PufferPos],Size);
    seek(f,Block); okay:=IOResult; if okay>0 then exit;
    BlockWrite(f,FilePuffer,BlockAnzahl); okay:=IOResult; (*$I+*)
  end; (* SchreibDat *)

(****************************************************************************)
(*                        Eingabe eines Eintrages                           *)
(****************************************************************************)

 procedure LiesDat(var f : DatFile; Nr : integer;
                  Var Dat; Size : integer; var okay : integer);
  var PufferPos, Block, BlockAnzahl : integer;
      BytePos : real;
  begin
    BytePos:=2048.0+1.0*Size*pred(Nr); Block:=trunc(BytePos/128);
    PufferPos:=succ(trunc(BytePos-128.0*Block));
    BlockAnzahl:=((Size+PufferPos) SHR 7)+ord(((Size+PufferPos) and 127)>0);
    (*$I-*) seek(f,Block); okay:=IOResult; if okay>0 then exit;
    BlockRead(f,FilePuffer,BlockAnzahl); okay:=IOResult;
    move(FilePuffer[PufferPos],Dat,Size); (*$I+*)
  end; (* LiesDat *)