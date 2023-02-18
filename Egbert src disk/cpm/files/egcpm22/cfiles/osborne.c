/******************************************************************************
*  O S B O R N E  *  C P M S Y S 6  *  T h o m a s   H o l t e  * 8 4 0 9 0 1 *
*******************************************************************************
*									      *
*   U T I L I T Y   F O R   R E A D / W R I T E   O F   O S B O R N E   I -   *
*   =======================================================================   *
*									      *
*	                        D I S K E T T E S            		      *
*	                        =================                     	      *
*									      *
*									      *
*  Thomas Holte			                         	 Version 1.0  *
*									      *
******************************************************************************/
  
#include <stdio.h>

#define DCTBASE 0x13D1              /* base of drive control blocks */

xmain ()
{
  /* sector translation vector */
  static char XLT[] = {0, 1, 4, 5,  8,  9, 12, 13, 16, 17,
		       2, 3, 6, 7, 10, 11, 14, 15, 18, 19};

  /* disk parameter block */
  static struct dpb {
                      int  SPT;
                      char BSH, BLM, EXM;
                      int  DSM, DRM;
                      char AL0, AL1;
                      int  CKS, OFF;
                    } DPB = {20, 4, 15, 1, 45, 63, 0x80, 0, 16, 3};

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
  static char dct[6] = {0x0C, 0x40, 1, 10, 40};

  /* freemem administration variables */
  unsigned *freespc = 0xFFFC;
  char    **freemem = 0xFFFA;

  int i;


  /* sufficient memory ? */
  if (*freespc - 30 < sizeof XLT)
  {
    puts  ("\nOUT OF MEMORY\n");
    abort (1);
  }

  /* load new driver */
  movmem (XLT, *freemem, sizeof XLT);

  /* search for first floppy disk */
  for (i = 0;;) if (!(XDPHp = bios(SELDSK, i++, 0) - 13)->type) break;

  /* select second floppy disk */
  XDPHp = bios(SELDSK, i, 0) - 13;


  /* save disk parameter header */
  movmem (XDPHp, &XDPH, sizeof XDPH);

  /* alter disk parameter header */
  XDPH.blksiz = 16;
  XDPH.cpmspt = 20;
  XDPH.secmsk =  1;
  XDPH.secshf =  1;
  XDPH.XLT    = *freemem;

  /* load disk parameter header */
  movmem (&XDPH, XDPHp, sizeof XDPH);


  /* load disk parameter block */
  movmem (&DPB, XDPH.DPB, sizeof(struct dpb));

  /* load physical disk parameters */
  system (XMOVE, 1, sizeof dct, dct, DCTBASE + sizeof dct);

  /* adjust freemem pointers */	
  *freespc -= sizeof XLT;
  *freemem += sizeof XLT;

  /* output message */
  puts ("\nDrive 0: Genie III format");
  puts ("\nDrive 1: Osborne I format (single density)\n\n");
  puts ("Press <RESET> to return to normal Genie III mode\n");
 
  /* reset converted drive */
  bdos (37, 1 << i);
}
