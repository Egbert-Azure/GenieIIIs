/******************************************************************************
*  A L P H A P 3  *  C P M S Y S 5  *  T h o m a s   H o l t e  * 8 4 1 0 1 8 *
*******************************************************************************
*									      *
*              U T I L I T Y   F O R   R E A D / W R I T E   O F              *
*              =================================================              *
*									      *
*	     a l p h a T r o n i c   P 3 / P 4 - D I S K E T T E S            *
*	     =====================================================    	      *
*									      *
*									      *
*  Thomas Holte			                         	 Version 1.0  *
*									      *
******************************************************************************/
  
#include <stdio.h>

#define DCTBASE 0x13D1              /* base of drive control blocks */

xmain ()
{
  extern iolength, readf (), writef ();

  /* disk parameter block */
  static struct dpb {
                      int  SPT;
                      char BSH, BLM, EXM;
                      int  DSM, DRM;
                      char AL0, AL1;
                      int  CKS, OFF;
                    } DPB = {80, 4, 15, 0, 394, 127, 0xC0, 0, 32, 1};

  /* extended disk parameter header */
  struct {
           char (*write) ();           /* addr of sector WRITE */
           char (*read) ();            /* addr of sector READ  */
           char blksiz;                /* CP/M sectors/block   */
           int  cpmspt;                /* CP/M sectors/track   */
           char secmsk;                /* sector mask          */
           char secshf;                /* log2(hstblk)         */
           char type;                  /* drive type           */
           char unit;                  /* physical unit number */
           int  base;                  /* base track           */
           char *XLT;                  /* translate vector     */
           int  scratch[3];            /* scratch area         */
           char *DIRBUF;               /* directory buffer     */
           struct dpb *DPB;            /* disk parameter block */
           char *CSV;                  /* check vector         */
           char *ALV;                  /* alloc vector         */
         } XDPH, *XDPHp;

  /* drive control block */
  static char dct[6] = {0x78, 0xC0, 1, 10, 80};

  /* freemem administration variables */
  unsigned *freespc = 0xFFFC;
  char    **freemem = 0xFFFA;

  int i;


  /* sufficient memory ? */
  if (*freespc - 30 < iolength)
  {
    puts  ("\nOUT OF MEMORY\n");
    abort (1);
  }

  /* load new driver */
  movmem (readf, *freemem, iolength);

  /* search for first floppy disk */
  for (i = 0;;) if (!(XDPHp = bios(SELDSK, i++, 0) - 13)->type) break;

  /* select second floppy disk */
  XDPHp = bios(SELDSK, i, 0) - 13;


  /* save disk parameter header */
  movmem (XDPHp, &XDPH, sizeof XDPH);

  /* alter disk parameter header */
  XDPH.write  = *freemem + ((char *)writef - (char *)readf);
  XDPH.read   = *freemem;
  XDPH.blksiz = 16;
  XDPH.cpmspt = 80;
  XDPH.secmsk =  7;
  XDPH.secshf =  3;
  XDPH.XLT    =  0;

  /* load disk parameter header */
  movmem (&XDPH, XDPHp, sizeof XDPH);


  /* load disk parameter block */
  movmem (&DPB, XDPH.DPB, sizeof(struct dpb));

  /* load physical disk parameters */
  system (XMOVE, 1, sizeof dct, dct, DCTBASE + sizeof dct);


  /* adjust freemem pointers */
  *freespc -= iolength;
  *freemem += iolength;

  /* output message */
  puts ("\nDrive 0: Genie III         format");
  puts ("\nDrive 1: alphaTronic P3/P4 format\n\n");
  puts ("Press <RESET> to return to normal Genie III mode\n");
 
  /* reset converted drive */
  bdos (37, 1 << i);
}
