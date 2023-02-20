/*   1MAL1.C  -->  Das Kleine Einmaleins tabellarisch ausgeben. */

#include <stdio.h>

main()
{
     int  faktor1, faktor2;

     printf("\n\n            ");
     printf(" *******   DAS KLEINE EIN-MAL-EINS   *******");
     printf("\n\n\n\n");

     /*  --- Die 1. Zeile und 2. Zeile der Tabelle ausgeben --- */

     printf("\n        ");                          /* 1. Zeile */
     for( faktor2 = 1 ; faktor2 <= 10 ; ++faktor2 )
       printf("%5d", faktor2);

     printf("\n         ");                        /* 2. Zeile */
     printf("컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴컴�");

     /*  -----  Die weiteren Zeilen der Tabelle ausgeben  ----- */

     for( faktor1 = 1 ; faktor1 <= 10 ; ++faktor1 )
     {
	printf("\n %5d �", faktor1);
	for( faktor2 = 1 ; faktor2 <= 10 ; ++faktor2 )
	    printf("%5d", faktor1 * faktor2);
     }
     printf("\n\n\n\n\n");               /* Tabelle hochschieben */
}

