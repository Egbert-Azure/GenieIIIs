/*************************************************************
	Das Programm PFLANZE in 'C'

**************************************************************/

#include  <stdio.h>
#include  <grafik.h>
#include  <math.h>

/**************************************************************
Definierte Konstanten fuer die Darstellung der 'Pflanze'.
Diese Werte sind fuer das Aussehen der Figur verantwortlich.
Sinnvolle Werte liegen etwa zwischen -3.0 und +3.0 fuer a,b
und c.Die Konstants 'bunt' dient dazu, erst nach dem Zeichnen
einer groesseren Menge von Punkten die Farbe zu wechseln.
Versuchen solltest du mal b=0.01 und b=0.005. Dargestelt
werden Schnitte von Pflanzenstielen.
**************************************************************/


/* extern void male_bild(double a, double b, double c);

*/

extern char getch();

char  	      code;
double       a,b,c;
char    string[20];
char      *pointer;

void main()
{

/* Vordefinitionen von a, b und c					*/
a = -2.0;	b = 1.0;	c = -2.0;
pointer = string;

do	{
	z3vinit();
	cls();					/* Clear Screen		*/

	printf("**********************    Pflanze   ****************************\n\n");
	printf("\nEs muessen 3 Fliesskommazahlen eingegeben werden");
	printf("\n <ENTER> belaesst den alten Wert.\n\n");

	printf("   Erste Zahl  (a)  :%lf: ",a);
	gets(pointer);
	if ( (*pointer) )	a = atof ( pointer);

	printf("   Zweite Zahl (b)  :%lf: ",b);
	gets(pointer);
	if ( (*pointer) )	b = atof ( pointer);

	printf("   Dritte Zahl (c)  :%lf: ",c);
	gets(pointer);
	if ( (*pointer) )	c = atof ( pointer);
 

	cls();						/* Clear Screen		*/
	openpl();
	erase();	 

	while(!kbhit())	{
	   x_coord = (int)(x0*xzoom) + x_mitte;
	   y_coord = (int)(y0*y_zoom)+y_mitte;
	   point(x_coord,y_coord);
	    
/*
	male_bild(a,b,c);				/* externe Funktion	*/
*/
	getch();				

	printf("Noch ein Bild mit anderen Parametern ? ");
	code = getch();
	putchar(code);

	}

	while (code != 'n');
	closepl();
}
