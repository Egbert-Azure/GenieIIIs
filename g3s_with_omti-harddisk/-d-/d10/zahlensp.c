/*  ZAHLENSP.C  -->  Ein Zahlenspiel gegen den Computer        */

#include <stdio.h>
#include <stdlib.h>          /* Prototypen von srand(), rand() */
#include <time.h>            /* Prototyp von time() */

main()
{
     int  zahl, versuch, gefunden, count, jn = 'j';
     long sek;

     time( &sek);                    /* Zeit in Sekunden lesen */
     srand( sek);           /* Zufallsgenerator initialisieren */

     printf("\n\n\n");
     printf("           *******    EIN  ZAHLENSPIEL    *******");
     printf("\n\n\nDie Spielregeln: \n");

     while( jn == 'j')
     {
	printf("\nIch merke mir eine Zahl zwischen 1 und 15");
	printf("\nSie haben drei Versuche die Zahl zu raten!\n");

	zahl = (rand() % 15) + 1;

	gefunden = count = 0;
	while( !gefunden  && count < 3 )
	{
	    fflush( stdin);            /* Eigabepuffer l|schen */
	    printf("\n%d. Versuch:   ", ++count);
	    scanf("%d", &versuch);
	    if( versuch < zahl)      printf("zu klein!\n");
	    else if( versuch > zahl) printf("zu gro~!\n");
	    else                     gefunden = 1;
	}

	if( !gefunden)
	    printf("\nIch habe gewonnen! Die gesuchte Zahl: %d\n",
							     zahl);
	else
	    printf("\nBravo! Sie haben gewonnen!\n");

	printf("Wiederholen --> <j>    Beenden --> <n>\n");
	do
	   scanf("%c", &jn);
	while( jn != 'j' &&  jn != 'n');
     }
}
