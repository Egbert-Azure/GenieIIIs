/* ZUFALL.C  --> Gibt 20 Zufallszahlen aus im Bereich 1-100 aus. */

#include <stdio.h>
#include <stdlib.h>         /* Prototypen von srand() und rand() */

main()
{
   unsigned int  i, seed;

   printf("\nBitte geben Sie eine Zahl zwischen 1 und 65535 ein: ");

   scanf("%u", &seed);      /* Eine Zahl von der Tastatur lesen */
   srand( seed);         /* Den Zufallsgenerator initialisieren */

   printf("\n\n           *******   ZUFALLSZAHLEN   *******\n");

   for( i = 1 ; i <= 20 ; ++i)
      printf("\n%20d. Zufallszahl = %3d", i, rand() % 100 + 1);

   printf("\n");
}

