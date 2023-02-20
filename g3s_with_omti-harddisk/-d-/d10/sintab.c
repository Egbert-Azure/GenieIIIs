/*  SINTAB.C  -->  Ausgabe einer Tabelle f}r  sin             */

#include <stdio.h>
#include <math.h>

#define  PI        3.1415926536
#define  U_GRENZE  0                         /* Untergrenze   */
#define  O_GRENZE  (2 * PI)                  /* Obergrenze    */
#define  SCHRITT   (PI / 8)                  /* Schrittbreite */

main()
{
     double x;

     printf("\n\n      "
            "*******  Tabelle f}r die Sinus-Funktion  *******");

     printf("\n\n\n%20s   %20s", "x" , "sin(x)" );
     printf("\n            ----------------------------------\n");

     for( x = U_GRENZE ;  x < O_GRENZE + SCHRITT/2 ;  x += SCHRITT)
        printf("   %20f %20f\n", x, sin(x) );

     printf("\n");
}
