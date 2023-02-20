(* INCLUDE-File GRAFIK.INC
   in das Programm einzufuegen mit

   ($I GRAFIK.INC)

  Es folgen einige Unterroutinen, mit denen die HRG des
  Genie IIIs angesprochen werden kann. Es sind folgende
  Funktionen, bzw Proceduren implementiert:

 Procedure  Gra_On (seite)
 Procedure  Gra_Off(seite)
 Procedure  Gra_Cls(farbe)

 Procedure  Set_Point(xKoor,yKoor,seite)
 Procedure  Res_Point(xKoor,yKoor,seite)
 Function   Ask_Point(xKoor,yKoor,seite)
 Procedure  Hrg_Line(xStart,yStart,xEnd,yEnd,farbe,seite)

 Procedure  Set_Point_Normiert(xkoor,ykoor,seite)
 Procedure  Res_Point_Normiert(xkoor,ykoor,seite)
 Procedure  Hrg_Line_Normiert(xStart,yStart,xEnd,yEnd,farbe,seite)
 Procedure  Hrg_Circle(xKoor,yKoor,radius,farbe,seite)

  Bei den nicht normierten Proceduren gilt

 xKoor, xStart und xEnd  :  INTEGER im Bereich von 0-639
 yKoor, yStart und yEnd  :  INTEGER im Bereich von 0-275 (25 Zeilen * 11 Scanlinien)
 seite                   :  BYTE im Bereich 0-1
 farbe                   :  Byte 0 = schwarz   1 = weiss

  Bei den normierten Proceduren gilt abweichend

 yKoor, yStart und yEnd  INTEGER im Bereich von 0-449


ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG !
!                                                                             !
!        Es wird nicht ueberprueft, ob die uebergebenen Parameter             !
!       im oben angegebenen Breich liegen. Eventuell kann der                 !
!       Rechner bei einigen Proceduren abstuerzen.                            !
!                                                                             !
ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG ! ACHTUNG !





*)


(* Direkter BIOS-Aufruf ueber BDOS-Funktion 50 *)
(* geklaut aus CPM-IBM.PAS aus der c't         *)

function UBIOS(fn,pa,pbc,pde,phl:integer):integer;
var biospb : record
               func,a   : byte;
               bc,de,hl : integer;
               end;
    result : integer;
begin
  with biospb do begin
    func:=fn; a:=pa;
    bc:=pbc; de:=pde; hl:=phl;
    end;

      result:=BDOS(50,addr(biospb));

    ubios:=result;
  end;


(* Schaltet eine Grafikseite ein --------------------------------------------*)
Procedure Gra_On (seite : byte);
var  sys0,sys1   :    byte;
begin
  sys0 := port[249];
   if (seite = 0) then   port[249] := sys0 and 239
                  else   port[249] := sys0 or   16;
  sys1 := port[250];
  port[250] := sys1 or 2;
end;
(*--------------------------------------------------------------------------*)

(* Schaltet eine Grafikseite aus -------------------------------------------*)
Procedure Gra_Off (seite : byte);
var  nixwert   :    integer;
begin
nixwert:=ubios(30,0,(seite*256)+25,0,0);
end;
(*--------------------------------------------------------------------------*)

(* Fuellt die Grafikseite mit FARBE ----------------------------------------*)
procedure Gra_Cls(seite,farbe : byte);
var  nixwert   :    integer;
begin
nixwert := UBIOS(30,farbe,(seite*256)+26,0,0);
end;
(*-------------------------------------------------------------------------*)

(* Setzt einen nicht normierten Punkt auf SEITE ---------------------------*)
Procedure Set_Point(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := ubios(30,1,(seite*256)+27,xkoor,ykoor);
end;
(*-------------------------------------------------------------------------*)

(* Loescht einen nicht normierten Punkt auf SEITE -------------------------*)
Procedure Res_Point(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := UBIOS(30,0,(seite*256)+27,xkoor,ykoor);
end;
(*-------------------------------------------------------------------------*)

(* Gibt 1 zurueck wenn der nicht normierte Punkt gesetzt ist --------------*)
Function Ask_Point(xkoor,ykoor : integer;seite : byte) :  integer;
begin
ask_point := UBIOS(30,0,(seite*256)+28,xkoor,ykoor);
end;
(*-------------------------------------------------------------------------*)

(* Zieht eine Linie auf SEITE mit FARBE -----------------------------------*)
Procedure Hrg_Line(xstart,ystart,xend,yend : integer; farbe,seite : byte);
var  parablock : array[0..4] of integer;
     nixwert,z :                integer;

begin
parablock[1] := xstart;
parablock[2] := ystart;
parablock[3] := xend;
parablock[4] := yend;
z := addr(parablock[1]);
nixwert := UBIOS(30,farbe,(seite*256)+29,0,z);
end;
(*-------------------------------------------------------------------------*)

(* Setzt normierten Punkt auf SEITE ---------------------------------------*)
Procedure Set_Point_Normiert(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := ubios(30,17,(seite*256)+27,xkoor,ykoor);
end;
(*-------------------------------------------------------------------------*)

(* Loescht normierten Punkt auf SEITE -------------------------------------*)
Procedure Res_Point_Normiert(xkoor,ykoor : integer; seite : byte);
var  nixwert   :    integer;
begin
nixwert := UBIOS(30,16,(seite*256)+27,xkoor,ykoor);
end;
(*-------------------------------------------------------------------------*)

(* Zeichnet normierte Linie mit FARBE auf SEITE ---------------------------*)
Procedure Hrg_Line_Normiert(xstart,ystart,xend,yend : integer; farbe,seite :byte);
var  parablock : array[0..4] of integer;
     nixwert,z :                integer;
begin
parablock[1] := xstart;
parablock[2] := ystart;
parablock[3] := xend;
parablock[4] := yend;
z := addr(parablock[1]);
nixwert := UBIOS(30,16+farbe,(seite*256)+29,0,z);
end;
(*-------------------------------------------------------------------------*)

(* Zeichnet einen Kreis auf SEITE mit FARBE -------------------------------*)
Procedure Hrg_Circle(xkoor,ykoor,radius : integer; farbe,seite : byte);
var parablock : array[1..3] of integer;
    nixwert,z :                integer;
begin
parablock[1] := xkoor;
parablock[2] := ykoor;
parablock[3] := radius;
z := addr(parablock[1]);
nixwert := UBIOS(30,16+farbe,(seite*256)+30,0,z);
end;
(*-------------------------------------------------------------------------*)
