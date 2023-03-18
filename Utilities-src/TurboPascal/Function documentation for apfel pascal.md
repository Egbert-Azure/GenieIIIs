# Function documentation for the Turbo Pascal program "apfel.pas": #

Holte CP/M 3 - Genie IIIs

## Program: apfel ##

Description:
This program generates a Mandelbrot set image and displays it using the Turbo Pascal Graphics library. The generated image is displayed on page 0 of the graphics window.

Procedures:
- Gra_On(seite: byte): Turns on graphics on the specified page. Page 0 is used in this program.
- Gra_Off(seite: byte): Turns off graphics on the specified page.
- Gra_Cls(seite: byte, farbe: byte): Clears the graphics window on the specified page with the specified color.
- Set_Point(xKoor, yKoor, seite: integer): Sets a pixel on the specified page at the specified coordinates with the current color.
- Res_Point(xKoor, yKoor, seite: integer): Resets a pixel on the specified page at the specified coordinates to the background color.
- Ask_Point(xKoor, yKoor, seite: integer): Returns the color of a pixel on the specified page at the specified coordinates.
- Hrg_Line(xStart, yStart, xEnd, yEnd, farbe, seite: integer): Draws a horizontal line on the specified page between the specified coordinates with the specified color.

Normalized versions of the above procedures are also provided:
- Set_Point_Normiert(xKoor, yKoor, seite: integer): Sets a pixel on the specified page at the specified normalized coordinates with the current color.
- Res_Point_Normiert(xKoor, yKoor, seite: integer): Resets a pixel on the specified page at the specified normalized coordinates to the background color.
- Hrg_Line_Normiert(xStart, yStart, xEnd, yEnd, farbe, seite: integer): Draws a horizontal line on the specified page between the specified normalized coordinates with the specified color.

Parameters:
- xKoor: The x-coordinate of the pixel or line segment to be drawn.
- yKoor: The y-coordinate of the pixel or line segment to be drawn.
- seite: The page of the graphics window to draw on.
- farbe: The color of the pixel or line segment. 0 represents black, and 1 represents white.
- xStart, xEnd: The x-coordinates of the start and end points of the line segment to be drawn.
- yStart, yEnd: The y-coordinates of the start and end points of the line segment to be drawn.

Constants:
- xmin: The minimum value of the x-coordinate of the Mandelbrot set.
- xmax: The maximum value of the x-coordinate of the Mandelbrot set.
- ymin: The minimum value of the y-coordinate of the Mandelbrot set.
- ymax: The maximum value of the y-coordinate of the Mandelbrot set.
- kmax: The maximum number of iterations allowed for each point in the Mandelbrot set.
- ak: The modulus of the iteration number at which to draw a pixel. A value of 2 is used in this program.
- rmax: The maximum radius allowed for each point in the Mandelbrot set.
- xhrg: The width of the graphics window in pixels.
- yhrg: The height of the graphics window in pixels.

Variables:
- ib, a, b, i, j: Temporary byte variables used in the program.
- xn, yn, xr, k: Integer variables used in the program to represent the coordinates and iteration counts of points in the Mandelbrot set.
- xi, yi, dx, dy, p, q, x, xalt, y: Real variables used in the program to represent the coordinates and
