(* ------------------------------------- *)
(* Only for Genie 3s with Holte CP/M 3.0 *)
(* ------------------------------------- *)

(* Author: Volker Dose
           Dorfstrasse 10
           W-2304 Brodersdorf
           Germany *)
            
program apfel;
(*
 Procedure  Gra_On (seite)
 Procedure  Gra_Off(seite)
 Procedure  Gra_Cls(seite,farbe)

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
 yKoor, yStart und yEnd  :  INTEGER im Bereich von 0-375 (25 Zeilen * 15 Scanlinien)
 seite                   :  BYTE im Bereich 0-1
 farbe                   :  Byte 0 = schwarz   1 = weiss

  Bei den normierten Proceduren gilt abweichend

 yKoor, yStart und yEnd  INTEGER im Bereich von 0-449


*)
{$I c5:grafik.inc}

const
     xmin   = -2.0;
     xmax   = +0.5;
     ymin   = -1.25;
     ymax   = +1.25;
     kmax   = 300;
     ak     = 2;
     rmax   = 30;

     xhrg   = 639;
     yhrg   = 374;

var
    ib,a,b,i,j  :  byte;
    xn,yn,xr,k  :  integer;
    xi,yi,dx,dy,p,q,x,xalt,y   :  real;


(*--------------------------------Hauptprogramm----------------------------*)

begin
clrscr;
gra_cls(0,0);               (*------Loesche HRG Seite 0-----*)
gra_on(0);

dx :=  (xmax - xmin) / xhrg;
dy :=  (ymax - ymin) / yhrg;

for  xn := 0 to xhrg -1 do
     begin
       xi := xmin + dx*xn;
       for  yn  :=  0 to yhrg-1 do
         begin
         yi :=ymin + dy*yn;
         k := 0;
         x := 0;
         y := 0;
         repeat
            xalt  :=  x;
            x := x*x  -  y*y  +xi;
            y := 2 * xalt * y + yi;
            k := k+1;
         until  ( x*x + y*y > rmax)  or  ( k=kmax);
         if  k = kmax  then k := 0;
         if (k mod ak ) = 1 then set_point (xn,yn,0);
         end;
      end;

end.
