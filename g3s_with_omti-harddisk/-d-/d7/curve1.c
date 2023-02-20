/* CURVE1.C --> Zeichnet den Graph einer ged{mpften Schwingung. *
 *              (Version f}r Hitech C + Genie IIIs)             *
 *                                                              *
 *              Module: CURVE1.C  WORLD.C                       */

#include <bios3.h>
#include <math.h>
#include "world.h"    /* F}r set_mode(), set_worl(), SX(), SY().*/

#define  PI 3.141593
#define  fkt1(x) exp(-0.2*x)                        /* D{mpfung */
#define  fkt(x)  (fkt1(x) * cos(x))     /* ged{mpfte Schwingung.*/

main(void)
{
   double x, y;                       /* Welt-Koordinaten,      */
   int  sx1, sx2, sy1, sy2;           /* Bildschirm-Koordinaten.*/
   z3vinit();
   cls();
   openpl();
      	
/*
   if( !set_mode())
      printf("Fehler: %s\n", grapherrormsg( graphresult()) ),
      exit(1);
*/

   set_world( -1.0, 15.0, -1.2, 1.2);       /* Welt-Ausschnitt */

   line( SX(-0.9), SY(0.0), SX(14.5), SY(0.0));      /* x-Achse */

   sy1 = SY(0.02); sy2 = SY(-0.02);             /* Unterteilung */
   for( x = -PI/4; x < 14.5; x += PI/4)
      sx1 = SX(x), line(sx1, sy1, sx1, sy2);

   line( SX(0.0), SY(-1.1), SX(0.0), SY(1.1));       /* y-Achse */

   sx1 = SX(0.1); sx2 = SX(-0.1);               /* Unterteilung */
   for( y = -1.0; y < 1.05; y += 0.1)
      line(sx1, SY(y), sx2, SY(y));

   move( SX(0.0), SY(fkt(0.0)) );
   
   x = 0.0;
   while( x < 4.5*PI+0.05 )
   {
      x += PI/16;   /* Der Graph   */
                                                /* fkt(x) von  */
      cont ( SX(x), SY(fkt(x)));               /* 0 bis 4.5PI */
      point( SX(x), SY(fkt1(x)));
      point( SX(x), SY(-fkt1(x)));
   }
   getch();                           /* Auf Tastendruck warten */
   closepl();                            /* Wieder Textmodus */
   return 0;
}
