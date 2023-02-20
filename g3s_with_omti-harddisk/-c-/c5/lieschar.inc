(*--- Modul: LIESCHAR.INC ----------------------- Stand: 20.10.1987 ---*)
(*                                                                     *)
(*    Tastaturabfrage und konvertieren in WordStar-Kompatible          *)
(*    Controlsequenzen.                                                *)
(*                                                                     *)
(*      p Translate   : char  -->  Cursoranpassung                     *)
(*      p LiesZeichen : char  -->  Liest Zeichen von der Tastatur      *)
(*                                                                     *)
(*    Anmerkung:                                                       *)
(*    - EscFlag ist eine globale boolsche Variable die beim druecken   *)
(*      der ESC-Taste auf true gesetzt wird (sonst false).             *)
(*    - In ZeichenPuffer kann ein Mehrtastenbefehl untergebracht werden*)
(*---------------------------------------------------------------------*)
const
        ScanCode = #27;          (* Bei Turbo-Pascal 4.0 ScanCode = #0 *)
        EscFlag : boolean = false;
        ZeichenPuffer : String[255] = '';

function ReadKey : char;         { Nur bei Turbo-Pascal 3.0 }
  var C : char;
  begin
    read(kbd,C); ReadKey:=C
  end; (* ReadKey *)

procedure Translate(var C : char);
  type _String = String[255];

  procedure MehrTastenBefehl(Befehl : _String);
    begin
      C:=Befehl[1]; ZeichenPuffer:=copy(Befehl,2,pred(length(Befehl)))
    end;

  begin
    if (C=ScanCode) and Keypressed then
      begin
        C:=ReadKey;
        case C of
          #72 : C:=^E;
          #80 : C:=^X;
          #75 : C:=^S;
          #77 : C:=^D;
          #73 : C:=^R;
          #81 : C:=^C;
         #115 : C:=^A;
         #116 : C:=^F;
          #71 : MehrTastenBefehl(^Q^S);
          #79 : MehrTastenBefehl(^Q^D);
         #119 : MehrTastenBefehl(^Q^R);
         #117 : MehrTastenBefehl(^Q^C);
         #132 : C:=^W;
         #118 : C:=^Z;
          #82 : C:=^V;
          #08 : C:=^H;
          #83 : C:=^G;

(* F1  *) #59 : MehrTastenBefehl(^K^Q);
(* F2  *) #60 : MehrTastenBefehl(^Q^F);
(* F3  *) #61 : MehrTastenBefehl(^Q^A);
(* F4  *) #62 : MehrTastenBefehl(^K^V);
(* F5  *) #63 : MehrTastenBefehl(^K^C);
(* F6  *) #64 : MehrTastenBefehl(^K^T);
(* F7  *) #65 : MehrTastenBefehl(^K^B);
(* F8  *) #66 : MehrTastenBefehl(^K^K);
(* F9  *) #67 : MehrTastenBefehl(^K^L);
(* F10 *) #68 : MehrTastenBefehl(^K^D);

(*Alt_X*) #45 : MehrTastenBefehl(^K^X);
(*Alt_m*) #50 : MehrTastenBefehl('F:MIST.'#13);
(*Alt_c*) #46 : MehrTastenBefehl('Continue');
(*Alt_s*) #31 : MehrTastenBefehl('Subroutine ');
(*Alt_w*) #17 : MehrTastenBefehl('Write ()'^S);
(*Alt_r*) #19 : MehrTastenBefehl('Return');
(*Alt_e*) #18 : MehrTastenBefehl('End');
(*Alt_f*) #33 : MehrTastenBefehl('Format ()'^S);
       end;
     end;
 end; (* Translate *)

procedure LiesZeichen(var C : char);
  begin
    if length(ZeichenPuffer)>0
      then begin C:=ZeichenPuffer[1]; Delete(ZeichenPuffer,1,1) end
      else begin C:=ReadKey; Translate(C) end;
    if (C=#27) or (C=^U) then EscFlag:=true else EscFlag:=false;
  end; (* LiesZeichen *)
