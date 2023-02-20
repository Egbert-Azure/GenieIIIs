/*  WORLD.C -> Die Funktionen  set_mode(), set_world() und      	*
 *             SX(), SY().  (Versionen f}r Hitech C + Genie IIIs)       */

#include <grafik.h>

/*
YMAX	 449	Genie IIIs xmax=639,ymax=449 
XMAX     639
*/

int  maxxpixel, maxypixel,
     getmaxx  , getmaxy  ;    /* Maximale Bildschirmkoordinaten.*/

struct world  { double xmin, xmax,       /* Die Grenzen des     */
                       ymin, ymax;       /* Weltfensters.       */
              } w;

int  set_mode( void)                     /* Videomodus setzen.  */
{
     getmaxx = 639;
     getmaxy = 449;	
     maxxpixel = getmaxx;  maxypixel = getmaxy;

     w.xmin =  0.0;   w.xmax = maxxpixel;   /* Initialisierung  */
     w.ymin =  0.0;   w.ymax = maxypixel;   /* des Weltfensters.*/

}

int  set_world( double xmin, double xmax,    /* Das Weltfenster */
                double ymin, double ymax )   /* neu setzen.     */
{
     if( xmax < xmin || ymax < ymin)  return 0;

     w.xmin  = xmin;    w.xmax = xmax;
     w.ymin  = ymin;    w.ymax = ymax;
     return 1;
}

int  SX( double x)                 /* Die Bildschirm-Koordinate */
{                                  /* zur Welt-Koordinate x     */
     int sx;

     sx = (int)((double)maxxpixel * (x - w.xmin)/(w.xmax - w.xmin));

     return   sx < 0 ? 0 : (sx > maxxpixel ? maxxpixel : sx);
}

int  SY( double y)                 /* Die Bildschirm-Koordinate */
{                                  /* zur Welt-Koordinate y     */
     int sy;

     sy = (int)((double)maxypixel * (w.ymax - y)/(w.ymax - w.ymin));

     return   sy < 0 ? 0 : (sy > maxypixel ? maxypixel : sy);
}
