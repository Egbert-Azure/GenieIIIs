/* R_FILE.C  -->  Liest eine Textdatei und gibt jeweils 22      *
 *                Zeilen auf die Standardausgabe aus.           *
 *                Aufruf:  r_file  dateiname                    */

#include <stdio.h>

char weiter[] = "\n\n\t\t  ----  weiter mit beliebiger Taste  ----";

main(int argc, char **argv)
{
     FILE *fp;    int  count;    char zeile[81];
     z3vinit();	
     if( argc != 2 )
        fprintf(stderr,"Benutzung:  r-file  dateiname\n");
     else if( (fp = fopen(argv[1], "r")) == NULL )
        fprintf(stderr,"Fehler beim \ffnen der Datei %s\n", argv[1]);
     else
     {
        count = 0;  cls();
        while( fgets(zeile, 81, fp) != NULL)   /* Datei auf die   */
        {                                      /* Standardausgabe */
            fputs(zeile, stdout);              /* kopieren.       */
            if( ++count == 22)
            {
                fprintf(stderr, weiter),  getch();
                count = 0; cls();
            }
        }
        if( !feof(fp))
            fprintf(stderr,"Fehler beim Lesen\n");
     }
     return 0;
}
