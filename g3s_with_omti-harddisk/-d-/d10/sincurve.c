/*   SINCURVE.C   -->   Ausgabe der sinus-Kurve                 */

#include <stdio.h>
#include <math.h>

#define  PI        3.1415926536
#define  U_GRENZE  0                           /* Untergrenze   */
#define  O_GRENZE  (2 * PI)                    /* Obergrenze    */
#define  PKT       64                /* Anzahl Punkte der Kurve */
#define  SCHRITT   ((O_GRENZE - U_GRENZE) / PKT)
#define  xK        15                /* Zeile  fÅr x-Koordinate */
#define  yK        10                /* Spalte fÅr y-Koordinate */

#define  CLS          printf("\033[2J")
#define  LOCATE(z,s)  printf("\033[%d;%dH", z, s)

main()
{
     int  zeile, spalte, anfsp, endsp;

     CLS;
     LOCATE(2,25); printf("-------  Die Sinus-Funktion  -------");


     /*    ---  Koordinaten-Kreuz zeichnen: ---    */

     LOCATE(xK,1);                                   /* x-Achse */
     for( spalte = 1 ; spalte <78  ; ++spalte)
       (spalte - yK) % 8  ?  putchar('ƒ') : putchar('≈');
     putchar('\020');                                /* Spitze  */
     LOCATE(xK-1, yK+64);  printf("2„   x");

     for( zeile = 6 ; zeile < 25 ; ++zeile)          /* y-Achse */
       LOCATE(zeile, yK),  putchar('≈');
     LOCATE( 5, yK);  printf("\036 sin(x)");         /* Spitze  */

     LOCATE( xK-8, yK+1);  printf(" 1");
     LOCATE( xK+8, yK+1);  printf(" -1");


     /*     ---  sinus-Funktion ausgeben:  ---     */

     anfsp = yK;    endsp = anfsp + PKT;

     for( spalte = anfsp ;  spalte <= endsp  ;  ++spalte)
     {
        zeile = xK - 8 * sin( (spalte-yK) * SCHRITT)  + 0.5;

        LOCATE( zeile, spalte);  putchar('*');
     }

     LOCATE(25,1);                    /* Cursor in letzte Zeile */
}
