/******************************************************************************
*  D A N A L  *  X U T I L S 0 0 1  *  T h o m a s   H o l t e  * 8 6 0 9 1 1 *
*******************************************************************************
*  									      *
*        D I S K   A N A L Y Z E R   F O R   T H E   G E N I E   I I I        *
*        =============================================================        *
*                                                                             *
*            	    M I C R O C O M P U T E R   S Y S T E M		      *
*                   =======================================       	      *
*  									      *
*                            						      *
*  Version 3.0                                                  Thomas Holte  *
*  									      *
******************************************************************************/
    
#include <stdio.h>
#include <bios.h>

#define DCTBASE 0x10D7		/* base of drive control table */

/* ASCII control codes */
#define ETX 0x03		/* end of text		*/
#define ENQ 0x05		/* enquiry	        */	
#define DC2 0x12		/* device control 2	*/
#define DC3 0x13		/* device control 3	*/
#define NAK 0x15		/* negative acknowledge */

char dct[8][6];				    /* drive control table 	   */
int  seccount;				    /* sector count		   */


_main ()
{
  extern char menu[];			/* menu buffer			   */
  	 char CPM_drive;		/* logical CP/M drive (0 - 15)	   */
	 char first;			/* number of first floppy	   */
	 char last;			/* number of last  floppy	   */
  	 char drive;			/* current  drive		   */
  	 int  side;			/* current surface		   */
  	 int  track;			/* current track		   */
	 int  sector;			/* current sector		   */
	 BOOL density;			/* density detected ?		   */
	 char address[100][4];		/* address fields		   */
	 int  row, column;		/* current display row & column	   */ 
	 int  index;			/* current menu index		   */
	 char errno;			/* error code rt from read_address */
  	 char key;			/* ASCII code of breaking key	   */
	 BOOL scroll = FALSE;		/* toggle scroll mode		   */
  	 char *p;			/* temporary pointer		   */
  	 int  int2;			/* temp number storage for convert */
  	 int  i;			/* loop counter			   */

  struct {
	   char (*_WRITE) ();		/* address of sector WRITE	   */
	   char (*_READ ) ();		/* address of sector READ	   */
	   char (*LOGIN ) ();		/* address of disk   LOGIN	   */
	   char (*INIT  ) ();		/* address of disk   INIT	   */
	   char unit;		        /* physical unit number		   */
	   char type;			/* drive type			   */
	   char *XLT;			/* translate vector		   */
	   char scratch[9];		/* scratch area			   */
	   char MF;			/* media flag			   */
	   char *DPB;			/* disk parameter block		   */
	   char *CSV;			/* check vector			   */
	   char *ALV;			/* alloc vector			   */
	   char **DIRBCB;		/* dir BCB header		   */
	   char **DTABCB;		/* data BCB header		   */
	   char *HASH;			/* hashing table		   */
	   char HBANK;			/* hash bank			   */
	 } *XDPH,			/* extended disk parameter header  */
	   **dtbl;			/* drive table in memory	   */

  struct {
	   char unit, type;
	 } drives[16];			/* drive types & unit numbers	   */

  /* error messages */
  static char errmsg[][38] = {"                                      ",
			      "  Illegal drive #                     ",
			      "  Track # too high                    ",
			      "  Illegal side #                      ",
			      "  Device not available                ",
			      "Density --> Double                    ",
			      "Density --> Single                    ",
			      "  Data record not found during read   ",
			      "  Parity error during read            ",
			      "  Lost data during read               "};

  /* window parameters */
  static struct {
		  int  CTRL;		/* control code 	    */
		  char PROMPT;		/* prompt character	    */
		  char ATTRIB;		/* console attributes	    */
		  int  BEG;		/* upper left window corner */
		  int  END;		/* lower left window corner */
		  char *BUF;		/* buffer pointer	    */
		  char option;
		  char btab;
		} w[] = {{0x83,    0, 9,    0,   79, menu, 2, 0},
			 {0x83,    0, 0,  100,  379, menu, 0, 0},
			 {0x83,    0, 8,  400,  479, menu, 0, 0},
			 {0x83,    0, 0,  500, 2279, menu, 0, 0},
			 {0x83,    0, 8, 2300, 2379, menu, 0, 0},
			 {0x92, 0x7F, 0,  210,  210, menu, 3, 0},
			 {0x92, 0x7F, 0,  223,  224, menu, 1, 1},
			 {0x83,    0, 0,  223,  224, menu, 4, 0},
			 {0x92, 0x7F, 0,  239,  239, menu, 1, 2},
			 {0x83,    0, 0,  239,  239, menu, 5, 0},
			 {   3,    0, 0,  242,  279,    0, 0, 0},
			 {0x83,    0, 0,  600, 2179, menu, 6, 0}};


  /* get drive table */
  dtbl = bios(DRVTBL);

  /* search for first floppy disk */
  for (first = 0;; first++)
  {
    XDPH = (char *)dtbl[first] - 10;
    system (15, 1, 2, &XDPH->unit, &drives[first]);

    if (!drives[first].type) break;
  }

  /* search for last floppy disk */
  for (last = first;; last++)
    if (dtbl[last])
    {
      XDPH = (char *)dtbl[last] - 10;
      system (15, 1, 2, &XDPH->unit, &drives[last]);

      if (drives[last].type) break;
    }
    else break;
  last--;   

  /* get physical disk parameters */
  for (drive = first; drive <= last; drive++)
    system (15, 1, sizeof dct[0], DCTBASE + drives[drive].unit * sizeof dct[0],
	    dct[drives[drive].unit]); 

  /* increment maximum track numbers by one */
  for (drive = 0; drive < 8; drive++) dct[drive][4]++;

  /* get date */
  memcpy (&menu[16], cpm3_date(), 8);

  /* get parameters loop */
  for (i = 0;; i++)
  {
    key = window(&w[i]);

    /* calc current buffer pointer */
    p = w[i].BUF + w[i].BEG / 100 * 80 + w[i].BEG % 100;

    /* check special keys */
    switch (key)
    {
      case ETX: switch (i)		/* scroll down */
		{
		  case 8: scroll = TRUE;
			  if ((int2 = side + 1) <= dct[drive][0] >> 6 & 1)
			  {
			    *p = int2 + '0';
			    continue;
			  }
			  else
			  {
			    side = 0;
			    *p = side + '0';
			    i -= 2;
			  }

		  case 6: if (++track < dct[drive][4])
			  {
			    convert (0x11, (long)track, 0, 2, &menu[183]);
			    window  (&w[7]);
			    int2 = side;
			    i   += 2;
			  }
			  else
			  {
			    track--;
			    i--;
			  }
			  continue;	 	
		}
		break;

      case ENQ:
      case DC3: i -= w[i].btab + 1;
                continue;

      case DC2: switch (i)		/* scroll up */
		{
		  case 8: scroll = TRUE;
			  if ((int2 = side - 1) < 0)
			  {
			    side = dct[drive][0] >> 6 & 1;
			    *p = side + '0';
			    i -= 2;
			  }
			  else
			  {
			    *p = int2 + '0';
			    continue;
			  }

		  case 6: if (--track < 0)
			  {
			    track++;
			    i--;
			  }
			  else
			  {
			    convert (0x11, (long)track, 0, 2, &menu[183]);
			    window  (&w[7]);
			    int2 = side;
			    i   += 2;
			  }
			  continue;	 	
		}
		break;

      case NAK: system (24, 0);
		puts   ("\33=7 \n\n");
                return;
    }

    /* check options */
    switch (w[i].option)
    {
      /* ASCII --> binary --> ASCII */
      case 1: int2 = convert(0x10, (long)int2, 0, w[i].END - w[i].BEG + 1, p);
              break;

      /* turn on clock */
      case 2: system (24, 1, 0, 0, 26);
	      break;

      case 3: /* get drive # */
	      if ((CPM_drive = toupper(*p) - 'A') < first || CPM_drive > last)
		i--;
	      else
	      {	
		/* get unit number */
		drive = drives[CPM_drive].unit;

  		/* reset init bit */
  		system (15,    1, 1, DCTBASE + 1 + drive * 6, &key);
    		key &= 0xFE;
		system (15, 0x10, 1, &key, DCTBASE + 1 + drive * 6);
	      }	
      	      break;

      case 4: /* get track # */
	      track = int2;
	      break;

      case 5: /* get side # */
	      side = int2;

	      /* clear display buffer */
	      for (int2 = 480; int2 <= 1760; int2 += 80) 
		memcpy (&menu[int2], &menu[400], 80);

	      density = TRUE;

	      /* read address bytes */
	      for (seccount = 0;; seccount++)
		if (errno =
		      read_address(drive, side, track, address[seccount]))
		  if (density && !seccount)
		  {		  
		    dct[drive][0] ^= 0x20;
		    density        = FALSE;
		    seccount       = -1;
		  }
		  else break;
		else if (seccount && address[seccount][2] == address[0][2])
		       break;	
	      
	      /* error ? */
	      w[i + 1].ATTRIB = 9;
	      if (!errno || seccount)
	      {	
		if (!errno)
		{
		  /* get density */
		  errno 	  = dct[drive][0] & 0x20 ? 5 : 6;
		  w[i + 1].ATTRIB = 0;
		}

	        /* move address marks into display buffer */
	        for (sector = 0; sector < seccount; sector++)
	        {
		  /* calc display row & column */
	          for (row  = sector, column = 0; row > 15;
		       row -=     16, column++);

		  /* calc menu index */
		  index = 482 + row * 80 + column * 27;

	          convert (0x11, (long)address[sector][0], 0, 3, 
			   &menu[index     ]);
		  convert (0x11, (long)address[sector][1], 0, 3, 
			   &menu[index +  5]);
		  convert (0x11, (long)address[sector][2], 0, 3,
			   &menu[index + 11]);

		  /* calc sector length */
		  int2 = 128;
		  switch (address[sector][3])
		  {
		    case 3: int2 += 512;
		    case 2: int2 += 256;
		    case 1: int2 += 128;
		  }

		  convert (0x11, (long)int2, 0, 4, &menu[index + 18]);
	        }
	      }	

	      w[i + 1].BUF = errmsg[errno];
	      break;

      case 6: /* reset loop counter */
	      if (scroll)
	      {
		scroll = FALSE;
		i     -= 4;
	      }	
	      else i -= 6; 
    }
  }
}
