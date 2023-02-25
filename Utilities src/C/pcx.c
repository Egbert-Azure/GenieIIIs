/* ------------------------------------------------------------------- */
/* pcx.c  -       Read & Display a *.PCX graphics file in Microsoft's  */
/* Quick C        (VGA/EGA/CGA/Herc).                                  */
/* ------------------------------------------------------------------- */
/* Function : Display a PCX-File                                       */
/* --------------------------------------------------------------------*/
/* Original by                      :  A. Uk       04-  -91            */
/* Genie IIIs                       :  E. Schroeer 08-08-93            */
/* laueft noch nicht -> Routinen bei Ausgabe noch fuer MSDOS PC        */
/* ------------------------------------------------------------------- */

#include <stdio.h>
#include <stdlib.h>

/* #include <memory.h> */

#include <conio.h>
#include "pcx.h"

/* PcxLeseKopf: read's a header from PCX-File             */
/*   f file opened binary                                 */
/*   reflect pointer to header or Zero if error encounted */

PcxKopf *PcxLeseKopf (FILE *f)
{  PcxKopf *k;

   if((k=malloc(sizeof(PcxKopf)))== NULL)
     return(NULL);
   if(fread(k,1,sizeof(PcxKopf),f)!=
      sizeof (PcxKopf))
     return(NULL);
   return(k);
}
/*   PcxLeseZeile: reads a line from PCX-File,  */
/*   decompress line and store                  */
/*   k pointer to header                        */
/*   f PCX-File opened binary                   */
/*   s memory block                             */
/*   reflect 0 or 1 if error                    */

BYTE PcxLeseZeile(PcxKopf *k,FILE *f,
                  BYTE *s)
{ register i;
  BYTE b;
  size_t l;

  for(i=0;i < k->ebene * k->bz;)
  { if((b = getc(f)) == EOF) return (1);
    if ((b & 0x00C0) != 0x00C0) s[i++]=b;
    else
    { l = b & 0x003F;
      b = getc(f);
      memset(s+i,b,l);
      i += l;
     }
   }
   return (0);
}

/* PcxResZeile: reserved memory for one line             */
/*   k pointer to header                                 */
/*   returns memory block or NULL if no memory available */

BYTE *PcxResZeile(PcxKopf *k)
{ return(malloc(k->ebene * k->bz));
}

/* PcxAusgabe: display of PCX-File */
/*   k pointer to header           */
/*   f PCX-File opened binary      */

void PcxAusgabe(PcxKopf *k,FILE *f)
{ BYTE *s;
  register i,j,z;
  BYTE far * vid = (BYTE far *)0xA0000000;
  int v = 80; /* only for 640x... */

  if((s=PcxResZeile(k)) == NULL) return;
  for(i=0;i <= (k->y2 - k->y1);i++)
  { PcxLeseZeile(k,f,s);
    for(z=0;z < k->ebene;z++)
     for(j=0;j < k->bz;j++)
     
/* hier wird direkt in EGA/VGA Speicher geschrieben */
/* aendern auf Genie IIIs                           */

     { outp (0x3C4, 2);
       outp (0x3C5, 1 << z);
       vid[i * V+j] =
       s[j + k->bz * z];
      }
    }
}