/*  SCHNITT.C  ->  Berechnet den Durchschnitt ganzer Zahlen.  */

#include <stdio.h>

main()
{
     int  x, anzahl = 0;      float  summe = 0;

     printf("\nBitte geben Sie ganze Zahlen ein\n"
            "(Abbruch mit beliebigem Buchstaben):\n");

     while( scanf("%d", &x) > 0)
     {
          summe += x;    ++anzahl;
     }
     printf("\nDer Durchschnitt der eingegebenen Zahlen ist:  "
            "%.2f\n", summe/anzahl);
}
