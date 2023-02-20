/*  DMDO.C  --->  Programm zur Umrechnung von DM in US-$     */

# include  <stdio.h>

main()
{
     long  dm, step, lower, upper, maxdm;
     float kurs;

     printf("\n\n  * * * KURSTABELLE  DM - US$  * * * \n\n");

     printf("\nBitte den Kurs  DM - US$  eingeben:   ");
     scanf("%f",&kurs);

     printf("\nBitte die Obergrenze der Tabelle eingeben:   ");
     scanf("%ld",&maxdm);

     /* ---  Ausgabe der Tabelle :  ---  */

     printf("\n\n%12s %20s\t\tKurs:  %.2f\n\n", "DM","US-$",kurs);

     for( lower=step=1; lower <= maxdm; step *= 10, lower = 2*step)

        for( dm = lower, upper = step*10;
                          dm <= upper && dm <= maxdm;  dm += step)

           printf("%12ld %20.2f\n", dm , dm/kurs);
}
