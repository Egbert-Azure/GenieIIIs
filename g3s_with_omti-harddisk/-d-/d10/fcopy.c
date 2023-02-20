/* FCOPY.C --> Kopiert Dateien:                                 *
 *             Sind zwei Dateien in der Kommandozeile angegeben,*
 *             wird die erste auf die zweite kopiert.           *
 *             Ist nur eine Datei angegeben, so wird diese auf  *
 *             stdout kopiert.                                  *
 *             Ist keine Datei angegeben, so wird stdin auf     *
 *             stdout kopiert.                                  */

#include <stdio.h>
#include <stdlib.h>

char use[] = "Benutzung:  fcopy  [ Quelle  [ Ziel] ]\n";

main(int argc, char **argv)
{
     int  c;
     FILE *infp, *outfp;

     switch( argc)
     {
        case 1:   infp = stdin;  outfp = stdout;
                  break;
        case 2:
                  infp = fopen( argv[1], "r");  outfp = stdout;
                  break;
        case 3:   if( (infp = fopen( argv[1], "r")) != NULL)
                     outfp = fopen( argv[2], "w");
                  break;
        default:  fprintf(stderr, use);   exit(1);
     }

     if( infp == NULL)
     {
        fprintf(stderr, "Fehler beim |ffnen von %s\n", argv[1]);
        exit(2);
     }

     if( outfp == NULL)
     {
        fprintf(stderr, "Fehler beim |ffnen von %s\n", argv[2]);
        exit(2);
     }

     while( (c = getc(infp)) != EOF)               /* kopieren  */
        putc(c, outfp);

     exit(0);
}
