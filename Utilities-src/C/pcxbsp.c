/* pcxbsp.c                           */
/* Function: sample program for pcx.c */

#include <stdio.h>
#include <conio.h>
#include <grafik.h>
#include "pcx.h"

void main()
{  file *f;
   PcxKopf *k;
   byte *s;
   if((f = fopen("worf.pcx","rb"))==NULL)
   {printf("Datei WORF.PCX nicht gefunden"
           "\n"); return;
   }
   k = PcxLeseKopf(f);
   PcxAusgabe(k, f);
   getch();
}           
