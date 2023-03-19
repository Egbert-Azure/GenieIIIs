/*  T_FSEEK.C --> Gibt die Zeilen im letzten 512-Byte-Block     *
 *                einer Textdatei aus, sowie die Anzahl der     *
 *                Zeichen in der Datei (ohne ^Z).               *
 *                Aufruf:  t_fseek  dateiname                   */

#include  <stdio.h>
#include  <stdlib.h>

#define   Ctrl_Z   26

main(int argc, char **argv)
{
    int  c, flag = 0;   long filesize;   FILE *fp;

    if( argc != 2 )
       fprintf(stderr,"Benutzung:  t_fseek  dateiname\n");

    else if( (fp = fopen(argv[1], "rb")) == NULL )
       fprintf(stderr,"Fehler beim \ffnen von %s\n", argv[1]);
    else
    {
       fseek(fp, 0L, 2);           /* an das Ende der Datei gehen */
                                   /* und dann 512 Bytes zur}ck.  */
       if( ftell(fp) > 512L )
	   fseek(fp, -512L, 2);
       else
	   rewind(fp);

       while( (c = getc(fp)) != EOF  &&  c != Ctrl_Z)
          if(flag)                    /* Ausgabe erst mit Beginn  */
             putchar(c);              /* einer neuen Zeile        */
          else if(c == '\n')
             flag = 1;

       filesize = ftell(fp);
       if( c == Ctrl_Z )  --filesize;       /* Dateil{nge ohne ^Z */

       printf("\n\nAnzahl der Zeichen in der Datei: %ld", filesize);
       exit(0);
    }
    exit(1);
}
