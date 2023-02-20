/*  TAB8.C  -->  Ein Filter, der alle Tabulator-Zeichen durch    *
 *               die richtige Anzahl von Blanks ersetzt.         */

#include <stdio.h>
#define   TAB_BR    8

main()
{
    int  c,  spalte = 0;                /* Spaltenz„hler = 0     */

    while( (c = getchar()) != EOF)      /* Zeichen einzeln lesen */
       switch(c)
       {
          case '\n':  putchar(c);
                      spalte = 0;             /* in neuer Zeile  */
                      break;

          case '\t':  do                      /* Blanks ausgeben */
                        putchar(' ');
                      while( ++spalte % TAB_BR != 0 );
                      break;

          default:    putchar(c);
                      ++spalte;
       }
}
