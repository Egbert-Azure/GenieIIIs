/*  BALL2.C   -->    Simuliert einen springenden Ball            *
 *                   (ohne Cursor-Anzeige)                       */

#include <stdio.h>

#define  DELAY        30000L
#define  CLS          printf("\033[2J")
#define  LOCATE(z,s)  printf("\033[%d;%dH", z, s)
#define  BB           printf("\033[30;40m")       /* Black-Black */
#define  WB           printf("\033[37m")          /* White-Black */

main()
{
     int  x, y, dx = 1, geschw = 0;
     long warten;

     BB;  CLS;  WB;
     LOCATE(1, 25);  printf("****  SPRINGENDER BALL  ****");

     for( y = 3 ; y < 25 ;  ++y)                  /* linke Wand  */
        LOCATE(y,1), putchar('!');
     LOCATE(y,1); putchar('+');

     LOCATE(25, 2);                                     /* Boden */
     printf("---------------------------------------"
            "---------------------------------------");

     LOCATE(25,79);  putchar('+');
     for( y = 24 ; y > 2 ;  --y)                  /* rechte Wand */
       LOCATE(y,79),  putchar('!');

     x = 3; y = 3;                     /* Einwuf in diesem Punkt */
     while( !kbhit() )           /* solange keine Taste gedr}ckt */
     {
         LOCATE( y,x);  WB; putchar('o'); BB;   /* Ball anzeigen */

         for( warten = 0 ; warten < DELAY ; ++warten)
           ;
         if(x == 2  ||  x == 78)  dx =  -dx;   /* An einer Wand? */
         if(y == 24)                           /* Am Boden?      */
         {
             geschw = -geschw;
             if(geschw == 0)  geschw = -7;     /* neu ansto~en   */
         }
         geschw += 1;                      /* Beschleunigung = 1 */

         LOCATE(y,x); putchar(' ');           /* Anzeige l|schen */

         y += geschw;  x += dx;               /* Neue Position   */
     }
     getch();                   /* Eingegebenes Zeichen auslesen */
     WB; CLS;            /* Wei~ auf Schwarz, Bildschirm l|schen */
}
