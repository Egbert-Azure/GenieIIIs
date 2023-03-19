/* CHECKPRN.C  --> Dieses Programm }berpr}ft den Druckerstatus.  */

/*  "far" mu~ dem Compiler als Schl}sselwort bekannt sein.       *
 *  Gegebenenfalls ist die entsprechende Option (bzw. Schalter)  *
 *  zu {ndern.                                                   */

#include <stdio.h>
#include <conio.h>
#include <cpm.h>
#define  LPT1_PORT  0x82    /* Portadresse der ersten prallelen  */
                            /* Scnittstelle. (0x3BC bei PCs mit  */
                            /* Adapter f}r Momochrom-Bildschirm.)*/

struct prn_status {  unsigned bit012 : 3;   /* Nicht verwendet.  */
		     unsigned error  : 1;   /* 0= I/O-Fehler.    */
                     unsigned select : 1;   /* 1= Drucker online.*/
                     unsigned paper  : 1;   /* 1= kein Papier.   */
                     unsigned ack    : 1;   /* Quittungssignal.  */
                     unsigned busy   : 1;   /* 1= Drucker bereit.*/
                  };

void get_status( char *status_ptr);

int main( )
{
    struct prn_status LPT1_status;

    do
    {
      get_status( (char *)&LPT1_status);

      printf("\nStatusbyte(HEX):   %02X\n", *(unsigned char *)&LPT1_status);

      if( LPT1_status.busy  &&  LPT1_status.select )
      {
	 printf("\nDrucker bereit!\n");
	 break;
      }
      else if( LPT1_status.paper )
         printf("\nKein Papier im Drucker!\n");
      else if( !LPT1_status.select )
         printf("\nDrucker nicht online!\n");
      else
         printf("\nDrucker nicht bereit!\n");

      printf("\nBitte Fehler am Drucker beseitigen.\n"
             "Esc -> Abbruch, andere Taste -> Status pr}fen.\n");

    }while( getch() != 27);

    return 0;
}

void get_status( char *status_ptr)
{
    *status_ptr = inp(LPT1_PORT+1) & 0xF;   /* Statusport lesen.*/
}