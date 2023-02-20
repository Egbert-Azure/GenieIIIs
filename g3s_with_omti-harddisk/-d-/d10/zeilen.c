/*   ZEILEN.C   -->  Filter zur Nummerierung von Zeilen         */

#include  <stdio.h>

main()
{
     int c;
     int neuezeile = 1;              /*  Flag f}r Zeilenbeginn   */
     int nummer = 1;                 /* Z{hler f}r Zeilennummern */

     while( (c = getchar()) != EOF)
     {
         if(neuezeile == 1)
         {
              printf("%3d  ", nummer++);
              neuezeile = 0;
         }
         putchar(c);

         if(c == '\n')
              neuezeile = 1;
     }
}
