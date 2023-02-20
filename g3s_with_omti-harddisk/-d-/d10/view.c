/*   VIEW.C  -->   Gibt jeweils 20 Zeilen einer Textdatei am     *
 *                 Bildschirm aus.                               *
 *                 Aufruf:  view  [dateiname]                    *
 *                                                               *
 *                 Module:  VIEW.C, V-UTIL.C                     */

#include <stdio.h>
#include <conio.h>
#include <string.h>
#include "v-util.c"

#define  ESC   27

	       /* Die Prototypen der Funktionen aus V_UTIL.C :  */
extern void v_exit(int nr);
extern int  getlinenr(void), init_linep(void);
extern int  display(int start_line);

FILE *fp;    char datei[21];   int nlines;

main(int argc, char **argv)
{
     int lnr = 0;
     z3vinit();
     cls();
     switch( argc )
     {
        case  2:  strcpy(datei, argv[1]);
                  break;
        case  1:  fprintf(stderr,"Dateiname:  ");
                  scanf("%20s", datei);
                  break;
     }
     if( (fp = fopen(datei, "rb")) == NULL )
        v_exit(2);

     if( (nlines = init_linep()) <= 0 )
	v_exit(nlines+4);                 /* nlines = 0 oder -1   */

     display(lnr);                        /* erste Seite anzeigen.*/
     while(1)
	switch( getch())                  /* Kommando einlesen    */
	{                                 /* und ausf}hren        */
           case  ESC:  v_exit(0);

	   case '\r':  if(lnr < nlines+18)  /* Bildschirmseiten   */
			 lnr += 18;         /* 2 (= 20-18) Zeilen */
		       display(lnr);        /* }berlappen lassen. */
                       break;

	   case  ' ':  lnr = getlinenr() - 1;  /* neue Zeilen-Nr. */
                       display(lnr);
                       break;

        }
}
