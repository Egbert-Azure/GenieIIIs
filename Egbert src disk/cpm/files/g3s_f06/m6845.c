/******************************************************************************
*  M 6 8 4 5  *  U T I L S 0 0 7  *  T h o m a s   H o l t e  *  8 6 0 1 1 9  *
*******************************************************************************
*                                                                             *
*  	A D J U S T M E N T   T O O L   F O R   T H E   M O T O R O L A	      *
*       ===============================================================       *
*                                                                             *
*  		  V I D E O   C O N T R O L L E R   M 6 8 4 5		      *
*                 ===========================================		      *
*									      *
*                                                                             *
*   Thomas Holte                                                 Version 1.0  *
*                                                                             *
******************************************************************************/

#include <stdio.h>
#include <bios.h>

#define ETX      0x03
#define ENQ      0x05
#define CR       0x0D
#define DC2      0x12
#define NAK      0x15
#define CAN      0x18
#define ESC      0x1B

#define CRTREG   0xF6		/* CRT address register */
#define CRTCMD   0xF7		/* CRT command register */

#define SEC_SIZE 512		/* physical sector size */


_main ()
{
  /* language dependent variables */
  extern char error1[], error2[], menu[], national[];

         char SYSTAB[SEC_SIZE];		/* table of system constants   */
	 char *vidpar;			/* parameter table for video
					   controller M6845	       */
	 char first;			/* first floppy disk drive     */
	 char type;			/* type of mass storage medium */
	 char drive;			/* current drive	       */
	 int  i;			/* loop counter		       */
	 
  /* extended disk parameter header */
  struct {
	   char (*_WRITE) ();		/* addr of sector WRITE */
	   char (*_READ ) (); 		/* addr of sector READ  */
	   char (*LOGIN ) ();		/* addr of disk   LOGIN */
	   char (*INIT  ) ();		/* addr of disk   INIT  */
	   char unit;			/* physical unit number */
	   char type;			/* drive type		*/
	   char *XLT;			/* translate vector	*/
	   char scratch[9];		/* scratch area		*/
	   char MF;			/* media flag		*/
	   char *DPB;			/* disk parameter block */
	   char *CSV;			/* check vector		*/
	   char *ALV;			/* alloc vector		*/
	   char **DIRBCB;		/* dir BCB  header  	*/
	   char **DTABCB;		/* data BCB header	*/
	   char *HASH;			/* hashing table	*/
	   char HBANK;			/* hash bank		*/
         } **DTBL,			/* drive table		*/
	   *XDPH;

  /* window parameters */
  static struct {
	   	  int  CTRL;		/* control code			    */
	   	  char PROMPT;		/* prompt character		    */
	   	  char ATTRIB;		/* console attributes		    */
	   	  int  BEG;		/* upper left window corner	    */
	   	  int  END;		/* lower left window corner	    */
	   	  char *BUF;		/* buffer pointer		    */
	 	} w[] = {{3,    0, 1,    0, 2379, menu},
			 {3,    0, 0, 2103, 2142,    0},
			 {4, 0x7F, 0, 2148, 2150,    0}};


  /* deselect error return mode */
  bdos (45, 0);

  /* get current disk */
  drive = bdos(25);

  /* get drive table */
  DTBL = bios(DRVTBL);

  /* search for first floppy disk */
  for (first = 0;; first++)
  {
    XDPH = (char *)DTBL[first] - 10;
    system (15, 1, 1, &XDPH->type, &type);

    if (!type) break;    
  }

  /* read LDRBIOS */
      bios (SELDSK, first );
      bios (HOME  ,      0);
      bios (SETSEC,      9);
      bios (SETDMA, SYSTAB);
  if (bios (READ  ,      0))
  {
    puts   (error1);
    xabort (2, drive);
  }
  
  /* select national character set */
  puts (national);

  /* get video parameter table */
  vidpar = &SYSTAB[0x11E];

  window (&w[0]);		    	      	      /* output menu	     */
  puts   ("\33E"); puts (&menu[1920]); puts ("\33D"); /* display status line */

  for (i = 0;;)
  {
    /* convert binary parameter to ASCII */
    convert (0x11, (long)vidpar[i], 0, 3, w[2].BUF = &menu[2481 + i * 3]);

    /* output parameter */
    w[1].BUF = &menu[2001 + i * 40];
    window (&w[1]);

    /* read keyboard */
    for (;;)
    {
      switch (window(&w[2]))
      {
        case ETX: if (i < 11) i++;		/* next par         */
                  break;

        case ENQ: vidpar[i]--;			/* decrement par    */
		  conv:
		  convert (0x11, (long)vidpar[i], 0, 3, w[2].BUF);
		  goto outpar; 

        case CR : vidpar[i] = convert(0x10, (long)vidpar[i], 0, 3, w[2].BUF);
		  outpar:
		  outp (CRTREG,        i );	/* select CRT reg   */
		  outp (CRTCMD, vidpar[i]);	/* output CRT par   */
		  continue;

        case DC2: if (i) i--;                   	/* previous par     */
                  break;

        case NAK: puts  ("\33E\32\33D\33=7 \n\n");	/* abort program    */
		  xabort (1, drive);

        case CAN: vidpar[i]++;				/* increment par    */
		  goto conv;

	case ESC: goto store;

        default : continue;
      }
      break;
    }
  }

  /* write CBIOS */
  store:
  switch (bios(WRITE)) 
  {
    case 0 : break;
    case 2 : puts (error2);
	     xabort (2, drive);
    default: puts (error1);
	     xabort (2, drive);
  }

  puts ("\33E\32\33D\33=7 \n\n");
 
  /* select current disk */
  bios (SELDSK, drive);
}


static xabort (mode, drive)
  char mode, drive;
{
  bios  (SELDSK, drive);
  _exit (mode);
}
