program CONVERT;                   (* Herbert zur Nedden *)
(* Umsetzung von Turbo-Pascal-Sources CP/M <-> DoMessDos *)

(* Veraendert mit Hilfe von Filz.Pas derart,
   dass Wildcards im Dateinamen moeglich sind (Holger Goebel 20.03.91*)

(* CONVERT beachtet in einfache ' eingeschlossene Strings   *)
(* wegen unterschiedlicher Codes fuer Umlaute und Klammern  *)
(* unter DoMessDos                                          *)
(* (beachte, Strings gehen nicht ueber Zeilenende hinaus!)  *)
(* Kommentare werden wie normaler Programm-Source behandelt *)

(*$I defconv.pas*)

FUNCTION SpaceWeg(FileName: DateiName): DateiName;
         (* Loesche Leerzeichen aus Dateinamen *)
Var x,y: Integer;
begin
  while (Pos(' ',FileName) <> 0 ) do
  begin
    x:=Pos(' ',FileName);
    y:= Length(FileName);
    FileName := Concat(Copy(FileName,1,Pred(x)),Copy(FileName,Succ(x),y-x));
  End;
  SpaceWeg:=FileName;
End;  (* SpaceWeg *)


FUNCTION UpCaseStr(Line: AnyStr): AnyStr;
         (* Sting in Grossbuchstaben umwandeln *)
var i:     byte;
    Satz:  AnyStr;
begin
  Satz:=Line;
  for i:=1 to length(Line) do Satz[i]:=Upcase(Satz[i]);
  UpCaseStr:=Satz
end;  (* UpCaseSt)r *)


FUNCTION RichtigesFormat(FileName: DateiName): DateiName;
         (* Umsetzen der Datei-Angabe mit Wildcards in das fuer die
            BDOS-Suchfunktion erforderliche Format                  *)
Var s1:   String[8]; 
    s2:   String[3];
    I,J:  Integer;
    done: Boolean;
Begin
  FileName:=Copy(FileName+'           ',1,12);
  If FileName='            ' then
     FileName:='*.*         ';
  s1:=''; s2:='';
  I:=1; Done:=False;
  Repeat
    Case FileName[i] of
    '.':  begin
          Done:=True;
          end;
    '*':  begin
          s1:=copy(s1+'????????',1,8);
          Done:=True;
          End;
    Else  s1:=s1+FileName[i];
    End;
    I:=I+1;
  Until ((Done) or (I>8));

  If FileName[i]='.' then i:=i+1;

  J:=I+3; If J>12 then J:=12; Done:=False;
  If I<13 Then
    Repeat
      Case FileName[i] of
      '*':  begin
            s2:=copy(s2+'???',1,3);
            Done:=True;
            End;
      Else  s2:=s2+FileName[i];
      End;
      I:=I+1;
    Until ((Done) or (I>J));

  s1:=Copy(s1+'        ',1,8);
  s2:=Copy(s2+'   ',1,3);
  RichtigesFormat:=s1+'.'+s2;
End;  (* RichtigesFormat *)


PROCEDURE GetDriveUser(var FileName: DateiName; var Laufwerk: DriveUser);
          (* Bestimme Laufwerk und User *)
var i,j: integer;
begin
  Laufwerk.Drive:=chr(65+(Mem[4] and $f));  (* Aktuelles Laufwerk *)
  Laufwerk.User:=Mem[4] shr 4;              (* Autueller User     *)
  If Pos(':',FileName)>0 then
  Begin
    If FileName[1] in ['A'..'P'] then
    begin
      Laufwerk.Drive:=FileName[1];
      i:=2;
    end
    else i:=1;
    If FileName[i] in ['0'..'9'] then
    begin
      j:=ord(FileName[i])-48;
      If FileName[i+1] in ['0'..'9'] then
        j:=10*j+ord(FileName[i+1])-48;
      If j<16 then
        Laufwerk.User:=j;
      end;
    Delete(FileName,1,Pos(':',FileName));
  end;
  i:=ord(Laufwerk.Drive)-65;
  BDOS(13);
  BDOS(32,Laufwerk.User);
  BDOS(14,i);
  BDOS(37,1 shl I);
End;  (* GetDriveUser *)


PROCEDURE DirLesen(Var Disk: Direktory; Dateien: DateiName);
          (* Direktory einlesen *)
Var Offset,I: Integer;
    Buffer:   String [128];
    FCB:      String[13] absolute $005b;
Begin
  FCB := concat(#0,copy(Dateien,1,8),copy(Dateien,10,3),#0);
  BDOS (26,Succ(Addr(Buffer)));       (* DMA-Addr *)
  Offset := BDOS(17,Succ(Addr(FCB))); (* Search first *)
  Mem[Addr(Buffer)] :=128;
  Disk.Anzahl := 0;
  While Offset < 255 do
  Begin
    Disk.Anzahl := Succ(Disk.Anzahl);
    Disk.Eintrag[Disk.Anzahl].Filler := 0; (* fuer Sort *)
    Disk.Eintrag[Disk.Anzahl].Datei :=
                Concat(Copy(Buffer,Succ(Succ(Offset shl 5)),8),
                '.',Copy(Buffer,10+Offset shl 5,3));
    Offset := BDOS (18);              (* Search Next *)
  End;   (* While *)
End;    (* DirLesen *)


PROCEDURE SortDir(Var Dir: Direktory);
          (* Direktory sortieren *)
Var Stradr,Endadr:Integer;
Begin
  Stradr := Addr(Dir.Eintrag[1]);
  Endadr := Addr(Dir.Eintrag[Dir.Anzahl])+15;
  InLine (
                        (*ENDADR   EXT                                 *)
                        (*STRADR   EXT                                 *)
$CD/*+$0005             (*         CALL START                          *)
/$C3/*+$00E6            (*         JP   ENDE                           *)
/$2A/ENDADR             (*START    LD   HL,(ENDADR)                    *)
/$22/*+$00DA            (*         LD   (EN1),HL                       *)
/$2B                    (*         DEC  HL                             *)
/$AF                    (*         XOR  A                              *)
/$2B                    (*EN2SU    DEC  HL                             *)
/$BE                    (*         CP   (HL)                           *)
/$20/$FC                (*         JR   NZ,EN2SU                       *)
/$23                    (*         INC  HL                             *)
/$22/*+$00D2            (*NWORT    LD   (EN2),HL                       *)
/$AF                    (*         XOR  A                              *)
/$E5                    (*         PUSH HL                             *)
/$ED/$5B/STRADR         (*         LD   DE,(STRADR)                    *)
/$B7                    (*         OR   A                              *)
/$ED/$52                (*         SBC  HL,DE                          *)
/$E1                    (*         POP  HL                             *)
/$C8                    (*         RET  Z                              *)
/$2B                    (*         DEC  HL                             *)
/$2B                    (*EN3SU    DEC  HL                             *)
/$BE                    (*         CP   (HL)                           *)
/$20/$FC                (*         JR   NZ,EN3SU                       *)
/$23                    (*         INC  HL                             *)
/$22/*+$00C0            (*         LD   (EN3),HL                       *)
/$2A/*+$00BB            (*         LD   HL,(EN2)                       *)
/$ED/$5B/*+$00B9        (*         LD   DE,(EN3)                       *)
/$7E                    (*VERGL    LD   A,(HL)                         *)
/$E6/$5F                (*         AND  5FH                            *)
/$47                    (*         LD   B,A                            *)
/$1A                    (*         LD   A,(DE)                         *)
/$E6/$5F                (*         AND  5FH                            *)
/$B8                    (*         CP   B                              *)
/$28/$0D                (*         JR   Z,GLEICH                       *)
/$30/$1B                (*VERGL2   JR   NC,TAUSCH                      *)
/$2A/*+$00A8            (*NWOR     LD   HL,(EN2)                       *)
/$22/*+$00A3            (*         LD   (EN1),HL                       *)
/$2A/*+$00A4            (*         LD   HL,(EN3)                       *)
/$18/$CB                (*         JR   NWORT                          *)
/$FE/$00                (*GLEICH   CP   0H                             *)
/$28/$04                (*         JR   Z,WEITER                       *)
/$23                    (*WEIT     INC  HL                             *)
/$13                    (*         INC  DE                             *)
/$18/$E1                (*         JR   VERGL                          *)
/$1A                    (*WEITER   LD   A,(DE)                         *)
/$FE/$00                (*         CP   0H                             *)
/$28/$E8                (*         JR   Z,NWOR                         *)
/$BE                    (*         CP   (HL)                           *)
/$28/$F4                (*         JR   Z,WEIT                         *)
/$2A/*+$008D            (*TAUSCH   LD   HL,(EN2)                       *)
/$ED/$4B/*+$008B        (*         LD   BC,(EN3)                       *)
/$CD/*+$007C            (*         CALL ADR1                           *)
/$C5                    (*         PUSH BC                             *)
/$ED/$5B/ENDADR         (*         LD   DE,(ENDADR)                    *)
/$ED/$B0                (*         LDIR                                *)
/$2A/*+$007E            (*EN1P     LD   HL,(EN3)                       *)
/$ED/$5B/*+$0076        (*         LD   DE,(EN1)                       *)
/$7E                    (*VERGL1   LD   A,(HL)                         *)
/$E6/$5F                (*         AND  5FH                            *)
/$47                    (*         LD   B,A                            *)
/$1A                    (*         LD   A,(DE)                         *)
/$E6/$5F                (*         AND  5FH                            *)
/$B8                    (*         CP   B                              *)
/$28/$1A                (*         JR   Z,GLEI2                        *)
/$30/$28                (*         JR   NC,WORTGE                      *)
/$2A/ENDADR             (*NWORT2   LD   HL,(ENDADR)                    *)
/$ED/$5B/*+$0063        (*         LD   DE,(EN1)                       *)
/$B7                    (*         OR   A                              *)
/$ED/$52                (*         SBC  HL,DE                          *)
/$28/$32                (*         JR   Z,ALLES                        *)
/$E5                    (*         PUSH HL                             *)
/$C1                    (*         POP  BC                             *)
/$AF                    (*         XOR  A                              *)
/$D5                    (*         PUSH DE                             *)
/$E1                    (*         POP  HL                             *)
/$ED/$B1                (*         CPIR                                *)
/$22/*+$0054            (*         LD   (EN1),HL                       *)
/$18/$D5                (*         JR   EN1P                           *)
/$FE/$00                (*GLEI2    CP   0                              *)
/$28/$04                (*         JR   Z,WEIT2                        *)
/$23                    (*WEI2     INC  HL                             *)
/$13                    (*         INC  DE                             *)
/$18/$D4                (*         JR   VERGL1                         *)
/$1A                    (*WEIT2    LD   A,(DE)                         *)
/$FE/$00                (*         CP   0                              *)
/$28/$DB                (*         JR   Z,NWORT2                       *)
/$BE                    (*         CP   (HL)                           *)
/$28/$F4                (*         JR   Z,WEI2                         *)
/$2A/*+$003F            (*WORTGE   LD   HL,(EN1)                       *)
/$ED/$4B/*+$003D        (*         LD   BC,(EN2)                       *)
/$CD/*+$0030            (*         CALL ADR1                           *)
/$ED/$5B/*+$0038        (*         LD   DE,(EN3)                       *)
/$ED/$B0                (*         LDIR                                *)
/$C1                    (*         POP  BC                             *)
/$2A/ENDADR             (*         LD   HL,(ENDADR)                    *)
/$18/$10                (*         JR   NVEKT                          *)
/$2A/ENDADR             (*ALLES    LD   HL,(ENDADR)                    *)
/$C1                    (*         POP  BC                             *)
/$09                    (*         ADD  HL,BC                          *)
/$ED/$4B/*+$0025        (*         LD   BC,(EN2)                       *)
/$CD/*+$0018            (*         CALL ADR1                           *)
/$ED/$5B/*+$0020        (*         LD   DE,(EN3)                       *)
/$ED/$B0                (*NVEKT    LDIR                                *)
/$2A/*+$001B            (*         LD   HL,(EN3)                       *)
/$E5                    (*         PUSH HL                             *)
/$AF                    (*         XOR  A                              *)
/$BE                    (*NVEKTS   CP   (HL)                           *)
/$23                    (*         INC  HL                             *)
/$20/$FC                (*         JR   NZ,NVEKTS                      *)
/$22/*+$000E            (*         LD   (EN1),HL                       *)
/$E1                    (*         POP  HL                             *)
/$C3/*+$FF39            (*         JP   NWORT                          *)
/$C5                    (*ADR1     PUSH BC                             *)
/$B7                    (*         OR   A                              *)
/$ED/$42                (*         SBC  HL,BC                          *)
/$E5                    (*         PUSH HL                             *)
/$C1                    (*         POP  BC                             *)
/$E1                    (*         POP  HL                             *)
/$C9                    (*         RET                                 *)
/$00/$00                (*EN1      DW   0                              *)
/$00/$00                (*EN2      DW   0                              *)
/$00/$00                (*EN3      DW   0                              *)
                        (*ENDE     END                                 *)
                                                                        );
End;  (* SortDir *)

procedure DostoCPM;
Var i: Integer;
begin
   if length(zeile)>0 then
      for i:=1 to length(zeile) do
          case zeile(.i.) of
               d_kl_ae,d_gkl_a : zeile(.i.):=c_kl_ae;
               d_gr_ae,d_ekl_a : zeile(.i.):=c_gr_ae;
               d_kl_oe         : zeile(.i.):=c_kl_oe;
               d_gr_oe         : zeile(.i.):=c_gr_oe;
               d_kl_ue,d_gkl_z : zeile(.i.):=c_kl_ue;
               d_gr_ue,d_ekl_z : zeile(.i.):=c_gr_ue;
               d_eszet         : zeile(.i.):=c_eszet;
          end;
end;

procedure CPMtoDOS;
Var i:Integer;
begin
   strg:=false;
   if length(zeile)>0 then
      for i:=1 to length(zeile) do
          case zeile(.i.) of
               ''''    : strg:=(not strg);
               c_kl_ae :  if strg then
                             zeile(.i.):=d_kl_ae
                          else
                             zeile(.i.):=d_gkl_a;
               c_gr_ae :  if strg then
                             zeile(.i.):=d_gr_ae
                          else
                             zeile(.i.):=d_ekl_a;
               c_kl_oe :  zeile(.i.):=d_kl_oe;
               c_gr_oe :  zeile(.i.):=d_gr_oe;
               c_kl_ue :  if strg then
                             zeile(.i.):=d_kl_ue
                          else
                             zeile(.i.):=d_gkl_z;
               c_gr_ue :  if strg then
                             zeile(.i.):=d_gr_ue
                          else
                             zeile(.i.):=d_ekl_z;
               c_eszet :  zeile(.i.):=d_eszet;
          end;
end;

procedure Abort(p: AnyStr);
begin
   writeln('Fehler ',p); writeln;
   writeln('Aufruf: ''CONVERT Datei /CD'': CP/M -> DoMessDos');
   writeln('  bzw.: ''CONVERT Datei /DC'': DoMessDos -> CP/M');
   writeln;
   halt;
end;


PROCEDURE Konvertiere(Var FileName: DateiName);
Var i,j: Integer;
begin

   assign(old,FileName);
   (*$i-*) reset(old); (*$i+*)
   if IOResult<>0 then
      Abort('Datei '+FileName+' nicht gefunden');

   assign(new,'CONVERT.$$$');
   rewrite(new);

   j:=0;
   while (not eof(old)) do
      begin
      readln(old,zeile);
      j:=j+1;
      write(^M,j);
      if DtC then
         DOStoCPM
      else
         CPMtoDOS;
      writeln(new,zeile);
   end;

   close(old);
   close(new);
   erase(old);
   rename(new,FileName);
end;

BEGIN  (* Convert *)
   ClrScr;
   WriteLn(^U,'Dateien-CONVERTer  von Herbert zur Nedden',^R);
   WriteLn; WriteLn;
   If not (ParamCount in [1..2]) then
   begin
     WriteLn(#27,'ADer Aufruf von CONVERT ist wie folgt:'); WriteLn;
     WriteLn('CONVERT  [[Laufwerk][User]:][DateiMaske] /DC oder /CD');
     WriteLn; WriteLn('Parameter in [] sind optional',#27,'S'); WriteLn;
     WriteLn('Laufwerk:     A -  P');
     WriteLn('User:         0 - 16');
     WriteLn('DateiMaske:   *.PAS, *.*, oder TEST?.INC, u.s.w.');
     halt;
   end;

   If ParamCount = 1 then i := 1 Else i := 2;
   if ParamStr(i) = '/DC' then
      DtC:=true
   else
      if ParamStr(i) = '/CD' then
         DtC:=false
      else
         Abort('2. Parameter falsch');

   if ParamCount=1 then
     FileName:='*.PAS'
   else
     FileName:=UpCaseStr(SpaceWeg(ParamStr(1)));
   GetDriveUser(FileName,Laufwerk);
   Dateien:=RichtigesFormat(FileName);
   assign(out,'CON:');
   rewrite(out);

   WriteLn(out,'Konvertiere die Dateien ',Laufwerk.Drive,Laufwerk.User,
               ':',Dateien);
   WriteLn(out);
   DirLesen(Dir,Dateien);

   if Dir.Anzahl > 0 then begin
     SortDir(Dir);
     writeln;
     write('CONVERTiere Turbo-Pascal-Source ');
     if DtC then
      writeln(' von DoMessDos nach CP/M')
     else
      writeln(' von CP/M nach DoMessDos');
     For I:=1 to Dir.Anzahl Do
     Begin
       FileName:=Dir.Eintrag[I].Datei;
       FileName:=SpaceWeg(FileName);
       Writeln; WriteLn; WriteLn(FileName);
       Konvertiere(FileName);
     end; end
   else
     WriteLn(out,'Keine passenden Dateien gefunden');

   WriteLn(out); WriteLn('Fertig mit Dateien-CONVERTer');
   close(out);
END.  (* Convert *)

enden Dateien gefunden');

   WriteLn(out); WriteLn('Fertig mit Dateien-CONVERTer');
 