(****************************************************************************)
(* Includefile QFILE.INC                                                    *)
(*                                                                          *)
(* Auswahl einer einzelnen Datei aus dem Directory                          *)
(* Benoetigt folgende Bibliotheksmodule CPM-80.BIB , QSORT.BIB              *)
(* SELECT-2.BIB )                                                           *)
(* t DateiString = string[14]   wird von QFILE.INC zu Verfuegung gestellt   *)
(* p DirectoryLesen(WC : DateiString; VAR Dir: Directory)                   *)
(*       liest in die Directory-Variable das in WC spezifizierte Directory  *)
(* p QueryFile (NettoZeilen:integer; DateiMaske:DateiString;                *)
(*              sortiert, NurEine:boolean; VAR Dir:Directory                *)
(* Dabei ist: NettoZeilen    Anzahl der Bildschrim-Zeilen,siehe SELEKT-2.BIB*)
(*            sortiert       false: Auswahl in der Reihenfolge von Directory*)
(*                           true : Directory-Feld wird vorher sortiert     *)
(*            NurEine        false: mehrere Dateien koennen gewaehlt werden *)
(*                           true : nue eine Datei kann gewaehlt werden     *)
(*            Dir            Dir.Flags enth. Nr. der gewaehlten Files       *)
(****************************************************************************)

type   DateiString = string[14];
       Directory = record
                     Drive : byte; Size : integer;
                     Name : array[byte] of DateiString;
                     Flags : set of byte
                   end;

function Kleiner(var Str1, Str2 : DateiString):boolean; {fuer Sortiere}
  begin Kleiner:=Str1 < Str2 end;

procedure DirectoryLesen(WC : DateiString; var Dir : Directory);
  var  FCB           : string[39];
       DMA_Puffer    : ARRAY[0..127] OF char;
       LW, Eintrag,b : byte;
       LWChar        : char;

  procedure WildCard(var WildString : DateiString; var LW:byte);
  {macht aus Wildcard einen gueltigen FCB}
    var i: integer;
    begin
      if pos(':',WildString)=2
        then begin
               LW:=ord(upcase(WildString[1]))-64;
               delete(WildString,1,2)
             end
        else LW:=0;
      wildString:=wildString+'..';
      for i:=1 to 11 do
        begin
          if i=9 then begin
                        if WildString[i]='*' then delete(WildString,i,1);
                        delete(WildString,i,1)
                      end;
          case WildString[i] of
            '*' : insert('?',WildString,i);
            '.' : insert(' ',WildString,i)
          end; (* case *)
        end; (* for *)
      WildString:=copy(WildString,1,11)
    end; (* WildCard *)

  begin (* DirectoryLesen *)
    if WC='' then WC:='*.*'
             else for b:=1 to length(WC) do WC[b]:=upcase(WC[b]);
    WildCard(WC,LW);
    with Dir do
      begin
        Size:=0; Drive:=LW;
        fillchar(FCB,40,0); FCB:=WC;
        FCB[0]:=chr(LW);
        Eintrag:=ErsterEintrag(FCB,DMA_Puffer);
        while Eintrag<>255 do
          begin
            Name[Size]:=copy(DMA_Puffer,Eintrag*32+2,8)+'.'+
                        copy(DMA_Puffer,Eintrag*32+10,3);
            Eintrag:=NaechsterEintrag;
            Size:=succ(Size)
          end; (* while *)
      end; (* with *)
  end; (* DirectoryLesen *)

procedure QueryFile(NettoZeilen:integer; sortiert, NurEine: boolean;
                    var Dir : Directory);
  begin
    with Dir do begin
      Flags:=[];
      if Size=0  then begin exit end;
      if Sortiert then
           sortiere(Name,SizeOf(Name),0,255,Addr(Kleiner),0,pred(Size));
        Selekt2(NettoZeilen,Name,sizeOf(Name[1]),Size,Flags,NurEine);
    end;
  end; (* QueryFile *)
