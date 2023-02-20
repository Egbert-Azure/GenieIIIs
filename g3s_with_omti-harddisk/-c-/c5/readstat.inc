(*************************************************************************)
(*  READSTAT.INC        LiesZeichen mit SonderTasten          11.1.88 w  *)
(*      ermittelt per Interrupt ---> 16h <--- die Pos. und den Shift-St. *)
(*     t  Sonder_Tasten = (F1....sF10,plus,minus..keine); siehe unten    *)
(*     t  Tasten_Status = (Ins..Shift);  siehe unten                     *)
(*     v  Taste : Sonder_Tasten                                          *)
(*     v  T_Status : Tasten_Status;                                      *)
(*     p  LiesZeichen(var c : char);  Liest Zeichen per Interrupt 16h    *)
(*                                    sowie den Tasten-Status der an-    *)
(*                                    schliessend abgefragt werden kann. *)
(*     (p) HoleStatus(ST : byte);     Uebersetzt das StatusByte          *)
(*************************************************************************)
type Sonder_Tasten = (F1,F2,F3,F4,F5,F6,F7,F8,F9,F10,
                      sF1,sF2,sF3,sF4,sF5,sF6,sF7,sF8,sF9,sF10,
                      plus,minus,
                      Enter,CtrlEnter,ShiftEnter,
                      Help,Esc,keine);
     Tasten_Status = set of (Ins,Caps,Num,Scroll,Alt,Ctrl,Links,Rechts,Shift);

var  Taste : Sonder_Tasten;
     T_Status : Tasten_Status;

procedure LiesZeichen(var c : char);
  type Regs_ = record case integer of
                 1: (ax,bx,cx,dx,bp,si,di,ds,es,flags : integer);
                 2: (al,ah,bl,bh,cl,ch,dl,dh : byte);
               end;
  var Regs     : Regs_;
      Pos,Stat : byte;

  procedure HoleStatus(ST : byte);
    begin
      T_Status:=[];
      if ST and  1 =  1 then T_Status:=T_Status+[Rechts];
      if ST and  2 =  2 then T_Status:=T_Status+[Links];
      if ST and  4 =  4 then T_Status:=T_Status+[Ctrl];
      if ST and  8 =  8 then T_Status:=T_Status+[Alt];
      if ST and 16 = 16 then T_Status:=T_Status+[Scroll];
      if ST and 32 = 32 then T_Status:=T_Status+[Num];
      if ST and 64 = 64 then T_Status:=T_Status+[Caps];
      if ST and 128=128 then T_Status:=T_Status+[Ins];
      if (Links in T_Status) or (Rechts in T_Status) then
        T_Status:=T_Status+[Shift];
    end;

{$u-}
begin {Lieszeichen}
  Taste:=keine;
  with Regs do begin
    ah:=0; intr($16,Regs); {Warte auf Taste}
    c:=chr(al); Pos:=ah;   {al = ASCII-Code, ah = TastenPosition}
    ah:=2; intr($16,Regs); {Hole Shift Status}
    Stat:=al;              {Bits in al : 7   6    5   4    3   2    1    0}
                           {             Ins Caps Num ScrL Alt Ctrl lkSh rtSh}
  end;
  HoleStatus(Stat);        {Stat uebersetzen in Menge T_Status}
  case c of
    #0   : case chr(Pos) of
             #72 : c:=^E; {Pfeil auf}
             #80 : c:=^X; {Pfeil ab}
             #75 : c:=^S; {Pfeil lk}
             #77 : c:=^D; {Pfeil rt}

             #81 : c:=^C; {PgDn}
             #71 : c:=^A; {Home}
             #73 : c:=^R; {PgUp}
             #82 : c:=^V; {Ins}
             #83 : c:=^H; {Del}
             #79 : c:=^U; {End}
             #15 : c:=^O; {BackTab}
            #115 : c:=^F; {CTRL Pfeil rt}
            #116 : c:=^A; {CTRL Pfeil lk}

            ';': begin c:=^[; Taste:=F1 end;
            '<': begin c:=^[; Taste:=F2 end;
            '=': begin c:=^[; Taste:=F3 end;
            '>': begin c:=^[; Taste:=F4 end;
            '?': begin c:=^[; Taste:=F5 end;
            '@': begin c:=^[; Taste:=F6 end;
            'A': begin c:=^[; Taste:=F7 end;
            'B': begin c:=^[; Taste:=F8 end;
            'C': begin c:=^[; Taste:=F9 end;
            'D': begin c:=^[; Taste:=F10 end;

            'T': begin c:=^[; Taste:=sF1 end;
            'U': begin c:=^[; Taste:=sF2 end;
            'V': begin c:=^[; Taste:=sF3 end;
            'W': begin c:=^[; Taste:=sF4 end;
            'X': begin c:=^[; Taste:=sF5 end;
            'Y': begin c:=^[; Taste:=sF6 end;
            'Z': begin c:=^[; Taste:=sF7 end;
            '[': begin c:=^[; Taste:=sF8 end;
            '\': begin c:=^[; Taste:=sF9 end;
            ']': begin c:=^[; Taste:=sF10 end;
           end; {case chr(Pos) of}

    ^M  :  if Shift in T_Status then
             begin
               Taste:=ShiftEnter; c:=^[
             end
           else
             Taste:=Enter;
    #8  :  c:=^G; {BackSpace}
    #9  :  c:=^I; {TAB}
    #10 :  Taste:=CtrlEnter;
    #43 :  Taste:=plus;
    #45 :  Taste:=minus;
    '?' :  Taste:=help;
    #27 :  Taste:=Esc;
  end; {case c of}
  {$u+}
end; {LiesZeichen}