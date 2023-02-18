program GraCls;
(*
 Procedure  Gra_On (seite)
 Procedure  Gra_Off(seite)
 Procedure  Gra_Cls(farbe,seite)

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
{$I grafik.inc}
begin
gra_cls(0,0);
gra_cls(1,0);
end.
