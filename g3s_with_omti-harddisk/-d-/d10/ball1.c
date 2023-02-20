
/*   BALL1.C  -->    Simuliert einen springenden Ball.           */

#include <stdio.h>

#define  DELAY        30000L          /* Verz|gerung der Ausgabe */
                    /* Cursor in Zeile z, Spalte s positionieren */

main()
{
     int  x = 2,  y = 3,  dx = 1,  geschw = 0;
     long warten;

     cls();
     gotoxy(1,25);  printf("****   SPRINGENDER BALL   ****");
     gotoxy( 1,25);
     printf("----------------------------------------"
            "---------------------------------------");

     while(1)                    /* Ball "immer" springen lassen */
     {
         gotoxy( x,y);   printf("o");           /* Ball anzeigen */

         for( warten = 0 ; warten < DELAY ; ++warten)
           ;
         if(x == 1  ||  x == 79)  dx =  -dx;   /* An einer Wand? */
         if(y == 24)                           /* Am Boden?      */
         {
             geschw = -geschw;
             if(geschw == 0)  geschw = -7;     /* neu ansto~en   */
         }
         geschw += 1;                      /* Beschleunigung = 1 */

         gotoxy( x,y); printf(" ");           /* Anzeige l|schen */

         y += geschw;  x += dx;               /* Neue Position   */
     }
}
