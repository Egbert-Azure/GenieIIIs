# grafik.inc #

This is a Pascal include file for using the graphics routines of Genie III. The following functions and procedures are implemented:

``` console
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
```

- Gra_On: Turns on a graphics page.
- Gra_Off: Turns off a graphics page.
- Gra_Cls: Fills the graphics page with a given color.
- Set_Point: Sets a point on a graphics page.
- Res_Point: Deletes a point on a graphics page.
- Ask_Point: Checks if a point is set on a graphics page.
- Hrg_Line: Draws a line on a graphics page.
- Set_Point_Normiert: Sets a normalized point on a graphics page.
- Res_Point_Normiert: Deletes a normalized point on a graphics page.
- Hrg_Line_Normiert: Draws a normalized line on a graphics page.
- Hrg_Circle: Draws a circle on a graphics page.

The non-normalized procedures work with the following parameter ranges:

``` console
xKoor, xStart, xEnd: INTEGER in the range 0-639
yKoor, yStart, yEnd: INTEGER in the range 0-275 (25 lines * 11 scan lines)
seite: BYTE in the range 0-1
farbe: Byte 0 = black 1 = white
```

The normalized procedures work with the following parameter ranges:

``` console
yKoor, yStart, yEnd: INTEGER in the range 0-449
```

Attention: There is no check for whether the passed parameters lie within the specified range. The computer may crash with some procedures if the parameters are outside the valid range.
