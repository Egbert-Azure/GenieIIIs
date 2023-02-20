/*    V_UTIL.C   -->   Die von VIEW.C verwendeten Funktionen.     */

#include <stdio.h>
#include <stdlib.h>
#define  ESC   27

extern FILE *fp;
extern char datei[21];
extern int  nlines;
static long linep[1000];            /* f}r die Zeilen-Positionen  */

void v_exit(int nr)            /* Beendigung mit (Fehler-)Meldung */
{
     static char *exit_msg[] = {
                 "\t\t\t GOODBY\n",
		 "Benutzung:  view  [dateiname]\n",
                 "Falscher Dateiname (Fehler beim Er|ffnen).\n",
                 "Datei zu lang.\n",
                 "Datei ist leer.\n",
                 ""    };

     if( nr < 0  ||  nr > 4)   nr = 5;
     cls();  fputs(exit_msg[nr], stderr);  exit(nr);
}

int getlinenr(void)          /* Einlesen einer neuen Zeilennummer */
{
     int lnr = 0;  char input[10];

     while( lnr <= 0 || lnr > nlines)
     {
          fflush(stdin);
          gotoxy(24,0);   printf("Zeilennummer:            ");
          gotoxy(24,15);  lnr = atoi( fgets(input,10,stdin));
     }
     return( lnr);
}

int  init_linep(void)       /* Initialisierung von line_p[]       */
{                           /* Return-Wert: Anzahl Zeilen oder -1 */
     int i = 0; char line[81];

     do
        linep[i] = ftell(fp);
     while( fgets(line, 81, fp) != NULL  &&  ++i < 1000);

     return( feof(fp) ? i : -1);        /*  -1 = zuviele Zeilen  */
}                                       /*   0 = leere Datei     */


static char status[] =
"Datei: %-20s Dateil{nge: %u Zeilen     erste Zeile: %-5u\n";
static char border[] =
"------------------------------------------------------------------------------\n";
static char commands[] =
"<Return> = n{chste Seite    <Space> = neue Zeilennummer    <Esc> = Ende";

int display(int linenr)            /* Ausgabe einer Bildschirmseite */
{
     int count = 0;  char line[81];

     if(linenr >= nlines)  return(0);
     cls();
     printf(status, datei, nlines, linenr+1); printf(border);

     fseek(fp, linep[linenr], 0);
     while( count++ < 20  &&  fgets(line, 81, fp) != NULL)
        fputs(line, stdout);

     gotoxy(22,0);  printf(border);
     gotoxy(24,0);  printf(commands);  gotoxy(23,0);

     return(1);
}
