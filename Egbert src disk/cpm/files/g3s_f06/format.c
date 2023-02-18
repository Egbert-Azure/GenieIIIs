/******************************************************************************
*  F O R M A T  *  U T I L S 0 0 6  *  T h o m a s   H o l t e * 8 6 0 9 1 1  *
*******************************************************************************
* 									      *
*      F O R M A T T I N G   U T I L I T Y   F O R   C P / M - V E R . 3      *
*      =================================================================      *
* 									      *
* O N   T H E   G E N I E   I I I s   M I C R O C O M P U T E R   S Y S T E M *
* =========================================================================== *
* 									      *
* 									      *
*   Thomas Holte			                         Version 1.0  *
* 									      *
******************************************************************************/

#include <stdio.h>
#include <bios.h>

#define DCTBASE  0x10D7		/* base of drive control blocks      */
#define DCTWBASE 0x1108		/* base of Winchester control blocks */

/* port addresses of Western Digital Winchester Disk Controller */
#define DATA      0x50		/* data register    */
#define ERR       0x51		/* error register   */
#define WPC       0x51		/* write precomp    */
#define SECNT     0x52		/* sector count     */
#define SECNO     0x53		/* sector number    */
#define CYLLO     0x54		/* cylinder low     */
#define CYLHI     0x55		/* cylinder high    */
#define SDH       0x56		/* size/drive/head  */
#define COMND     0x57		/* command register */
#define STATUS    0x57		/* status register  */

/* commands of Western Digital Winchester Disk Controller */
#define REST	  0x10		/* restore	     */
#define FORMAT    0x50		/* format track      */

/* spare directory entry */
struct sde {
	     unsigned track;		/* bad track			*/
	     char     sector;		/* bad sector			*/
	   };

/* disk parameter block */
struct dpb {
	     int  SPT;
	     char BSH, BLM, EXM;
	     int  DSM, DRM;
	     char AL0, AL1;
	     int  CKS, OFF;
	     char PSH, PHM;
	   };

/* extended disk parameter header */
struct xdph {
	     char (*_WRITE) ();		/* addr of sector WRITE */
	     char (*_READ ) (); 	/* addr of sector READ  */
	     char (*LOGIN ) ();		/* addr of disk   LOGIN */
	     char (*INIT  ) ();		/* addr of disk   INIT  */
	     char unit;			/* physical unit number */
	     char type;			/* drive type		*/
	     char *XLT;			/* translate vector	*/
	     char scratch[9];		/* scratch area		*/
	     char MF;			/* media flag		*/
	     struct dpb *DPB;		/* disk parameter block */
	     char *CSV;			/* check vector		*/
	     char *ALV;			/* alloc vector		*/
	     char **DIRBCB;		/* dir BCB  header  	*/
	     char **DTABCB;		/* data BCB header	*/
	     char *HASH;		/* hashing table	*/
	     char HBANK;		/* hash bank		*/
	   };

/* sector table */
char stab[52] = {0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF,
                 0xFF, 0xFF, 0xFF, 0xFF, 0xFF, 0xFF};

       char  dct[6];		/* drive control table 			*/
static char dctw[7];		/* drive control table (Winchester)	*/
static int  seclen;		/* sector length			*/
static char seccount;		/* sectors per track			*/

/* language dependent variables (globals) */
extern char cancel, error2[], error3[], error4[], error5[], error6[], error7[],
	    error8[], error9[], err10[], err11[], prmt10[], proceed, retry;


main (argc, argv)
  int argc;
  char *argv[];
{
  /* language dependent variables */
  extern char error1[], national[], prmpt1[], prmpt2[], prmpt3[], prmpt4[],
	      prmpt5[], prmpt6[], prmpt7[], prmpt8[], prmpt9[], yes;

  	 char type[16];		/* drive type				*/
  static char CPM_drive[18];	/* CP/M drive names			*/
  static char valid_drives[80];	/* string of valid drives		*/
  	 char drive;		/* drive number				*/
  	 char disk;		/* physical driver number		*/
  	 char surface;		/* number of surfaces			*/
  	 char density;		/* disk density				*/
  	 char sl;		/* sector length byte			*/
  	 char skew;		/* interleaving factor			*/
  	 char base;		/* base number of first sector on 
				   current surface			*/
  	 char buf[11000];	/* track buffer				*/
  	 int  beginsec;		/* start of  1st sector in track buffer */
  	 int  endsec;		/* start of last sector in track buffer	*/
  	 int  gap;		/* length of sector gap			*/
  	 int  wholesec;		/* length of sector with gaps		*/
  	 char firsttrk;		/* number of first track		*/
  	 int  lasttrk;		/* number of last  track		*/
	 int  cylinder;		/* current cylinder number		*/
  	 int  track;		/* current track number			*/
  	 char firstsec;		/* number of first sector		*/
  	 char secno;		/* current sector number		*/
  	 char side;		/* current surface number		*/
  	 char errno;		/* error code				*/
  	 char errmsg ();	/* prompts error message		*/
  	 int  i, j;		/* loop counters 			*/
	 char c;		/* temporary character			*/
	 char *buf1;		/* contains formatting data		*/
	 char *buf2;		/* contains worst case bit pattern	*/
	 char *buf3;		/* buffer for spare directory		*/
	 char *buf4;		/* temporary sector buffer		*/
  static char copyright[] = 
   "MM/DD/YY  HH:MM:SS  formatted by FORMAT, written by Thomas Holte (c) 1985";

  struct sde *sd;		/* pointer to spare directory		*/

	 int  sdc = 0;		/* bad sector counter			*/

  struct dpb  *DPB;		/* disk parameter block 		*/
  struct xdph **DTBL;		/* drive table				*/
  struct xdph *XDPH;		/* extended disk parameter header	*/


  /* get drive table */
  DTBL = bios(DRVTBL);

  /* search for formattable drives */
  for (i = 0; i < 16; i++)
  {
    if (DTBL[i])
    {
      XDPH = (char *)DTBL[i] - 10;
      system (15, 1, 1, &XDPH->type, &type[i]);
    }
    else type[i] = 0xFF;

    /* get logical drive name */
    switch (type[i])
    {
      case 0:
      case 1:
      case 3: CPM_drive[i] = i + 'A';
    }
  }

  /* build string of valid drives */
  for (i = 0; i < 16; i++) if (CPM_drive[i])
			   {
			     strncat (valid_drives, &CPM_drive[i], 1);
			     if (CPM_drive[i + 1] && CPM_drive[i + 2])
			     {
			       strcat (valid_drives, " - ");
			       while (++i < 16) if (!CPM_drive[i + 2]) break;
        		     }
			     else for (j = i + 1; j < 16; j++) 
				    if (CPM_drive[j])
				    {
				      strcat (valid_drives, ", ");
				      break;
				    }
			
			   }

  /* select national character set */
  printf (national);

  if (!--argc || (drive = toupper(**++argv) - 'A') > 15 || !CPM_drive[drive])
  {
    printf ("%s%s)\n", error1, valid_drives);
    _exit  (2);
  }
	
  /* prompt */
  printf ("%s%c%s", prmpt1, drive + 'A', prmpt2);
  if (toupper(getchar()) != yes) return;


  /* get unit number */
  XDPH = (char *)DTBL[drive] - 10;
  system (15, 1, 1, &XDPH->unit, &disk);

  /* formatter */
  switch (type[drive])
  {
    /* floppy disk drive */
    case 0: /* get disk parameters */
	    system (15,    1, sizeof dct, DCTBASE + disk * sizeof dct, dct);
            dct[1] &= 0xFE;
            system (15, 0x10, sizeof dct, dct, DCTBASE + disk * sizeof dct);
            goto format;

    /* floppy disk drive (special format) */
    case 1: /* get disk parameters */
            system (15,    1, sizeof dct    , (char *)XDPH + 35, dct);
            dct[1] &= 0xFE;
            system (15, 0x10, sizeof dct - 1, dct, (char *)XDPH + 35);

    	    format:
    	    surface  = dct[0] >> 6 & 1;
            density  = dct[0] >> 5 & 1;
	    firsttrk = density ? !(dct[0] >> 4 & 1) : 0;
    	    firstsec = dct[0] >> 3 & 1;     
    	    seccount = dct[3] / (surface + 1);
    	    base     = dct[1] >> 5 & 1 ? seccount : 0;
      	    lasttrk  = dct[4] + firsttrk;
    	    i = sl   = dct[1] >> 6 & 3;
    	    seclen   = 128;
    	    while (i--) seclen *= 2;
    	    skew     = dct[2];

    	    /* build track buffer */
    	    if (density)
    	    {
      	      gap = 25;
      	      switch (sl)
      	      {
        	case 3: gap += 98;
        	case 2: gap += 15;
              }

      	      wholesec = gap + 60 + seclen;
      	      beginsec = gap + 16;
      	      endsec   = seccount * wholesec;
      	      for (i = 0; i < endsec; i += wholesec)
              {
        	j = 0;	
        	while (j < gap               ) buf[i + j++] = 0x4E;
        	while (j < gap + 12          ) buf[i + j++] = 0x00;
        	while (j < gap + 15          ) buf[i + j++] = 0xF5;
         		               	       buf[i + j  ] = 0xFE;
			                       	       j   += 4   ;
			               	       buf[i + j++] = sl  ;	
    			               	       buf[i + j++] = 0xF7;
        	while (j < gap + 43          ) buf[i + j++] = 0x4E;
        	while (j < gap + 55          ) buf[i + j++] = 0x00;
        	while (j < gap + 58          ) buf[i + j++] = 0xF5;
      			               	       buf[i + j++] = 0xFB;
        	while (j < gap + 59 + seclen)  buf[i + j++] = 0xE5;
			 	       	       buf[i + j++] = 0xF7;
      	      }  
      	      for (i = endsec; i < 11000; i++) buf[i] = 0x4E;
    	    }
    	    else
    	    {
      	      gap = 27;
      	      if (sl == 1) gap -= 11;

      	      wholesec = gap + 31 + seclen;	
      	      beginsec = gap +  7;
      	      endsec   = seccount * wholesec;
      	      for (i = 0; i < endsec; i += wholesec)
      	      {
        	j = 0;
        	while (j < gap    	    ) buf[i + j++] = 0xFF;	
        	while (j < gap +  6	    ) buf[i + j++] = 0x00;
         		     	      	      buf[i + j  ] = 0xFE;
			               	      	      j   += 4   ;	
		       	      	      	      buf[i + j++] = sl  ;
    			      	              buf[i + j++] = 0xF7;
        	while (j < gap + 23	    ) buf[i + j++] = 0xFF;	
        	while (j < gap + 29	    ) buf[i + j++] = 0x00;
      			     	      	      buf[i + j++] = 0xFB;
        	while (j < gap + 30 + seclen) buf[i + j++] = 0xE5;
			      	      	      buf[i + j++] = 0xF7;
      	      }  
      	      for (i = endsec; i < 11000; i++) buf[i] = 0xFF;
    	    }
  
    	    /* number sectors */
    	    for (i = secno = 0; i < seccount; i++)
    	    {
      	      j = secno;
      	      if (stab[j] != 0xFF)
      	      {
        	secno++;
        	i--;
      	      }
      	      else
      	      {
        	stab[j] = i;
        	if ((secno += skew) >= seccount) secno -= seccount;
      	      }
    	    }

    	    /* duplicate sector table with offset */
    	    if (surface) for (i = 0; i < seccount; i++)
		   	   stab[i + seccount] = stab[i] + seccount;

    	    /* formatting */
    	    printf ("\n%s", prmpt3);

    	    for (track = firsttrk; track < lasttrk; track++)
    	    {
      	      for (i = beginsec; i < endsec; i += wholesec) buf[i] = track;
      		printf ("\b\b%02d", track);
    
      	      for (side = 0; side <= surface; side++)
      	      {
        	for (i = beginsec + 1, j = 0; j < seccount; i += wholesec, j++)
        	{	
          	  buf[i    ] = side;
	  	  buf[i + 1] = stab[j] + firstsec + base * side;
        	}

        	/* format track */	
        	if (errno = formtrk(disk, side, track, buf))
        	{
		  if ((c = errmsg(errno)) == cancel) _exit (2);
		  else if (c == retry) track--;

          	  printf (prmpt3);
	  	  break;
        	}		
      	      }
    	    }

    	    /* verifying */
    	    printf ("\r%s", prmpt4);

    	    for (track = firsttrk; track < lasttrk; track++)
    	    {
      	      printf ("\b\b%02d", track);

      	      if (errno = checktrk (disk, track))
      	      {
		if ((c = errmsg(errno)) == cancel) _exit (2);
		else if (c == retry)
          	     {
	  	       for (i = beginsec; i < endsec; i += wholesec) 
			 buf[i] = track;
		       for (side = 0; side <= surface; side++)
		       {
      		         for (i = beginsec + 1, j = 0; j < seccount;
			      i += wholesec, j++)
      		      	 {	
        	           buf[i    ] = side;
		           buf[i + 1] = stab[j] + firstsec + base * side;
      		      	 }
		      	 formtrk (disk, side, track, buf);
		       }
		       track--;
         	     }
         	printf ("\n%s", prmpt4);
      	      }
    	    }
	    break;

    /* Winchester (cartridge) */
    case 3: /* get physical disk parameters */
	    system (15, 1, sizeof dctw, DCTWBASE + disk * sizeof dctw, dctw);

	    /* get logical disk parameters */	
            XDPH = (char *)DTBL[drive] - 10;
            system (15, 1, sizeof DPB, &XDPH->DPB, &DPB);

	    surface  = dctw[0] >> 5;
	    firstsec = dctw[0] >> 4 & 1;
	    seccount = dctw[4];
	    lasttrk  = *(int *)&dctw[5];
	    seclen   = 128;
	    switch (dctw[1] = dctw[1] >> 1 & 0x60)
	    {
	      case 0x40: seclen *= 2;
 	      case 0x20: seclen *= 2;
	      case 0x00: seclen *= 2;
	    }
	    skew = dctw[3];

	    /* restore drive */
	    restore (disk);

	    /* initialize buffers */
	    buf4 = (sd = buf3 = (buf2 = (buf1 = 
	      buf) + seclen) + seclen) + seclen;

	    for (i = 0; i < seccount * 2; i++)
	    {
	      buf1[i++] = 0x00;
	      buf1[i  ] = 0xFF;
	    }

	    for (i = 0; i < seclen; i += 3) buf2[i] = 0x6D;
	    for (i = 1; i < seclen; i += 3) buf2[i] = 0xB6;
	    for (i = 2; i < seclen; i += 3) buf2[i] = 0xDB;

	    memset (buf3, 0xFF, seclen);

	    memset (buf4,    0, seclen);
	    memcpy (buf4, copyright, sizeof copyright);

	    /* get time & date */
	    memcpy ( buf4    , cpm3_date(), 8);
	    memcpy (&buf4[10], cpm3_time(), 8);

	    /* number sectors */
	    for (i = secno = 0; i < seccount; i++)
	    {
	      j = 1 + 2 * secno;
	      if (buf1[j] != 0xFF)
	      {
		secno++;
		i--;
	      }
	      else
	      {
		buf1[j] = i + firstsec;
		if ((secno += skew) >= seccount) secno -= seccount;
	      }
	    }

	    /* formatting */
	    printf (prmpt5);

	    for (cylinder = 0; cylinder < lasttrk / (surface + 1); cylinder++)
	      for (side = 0; side <= surface; side++)
	      {
		printf ("\b\b\b%03d",
			track = cylinder * (surface + 1) + side); 	

		format_track (disk, side, cylinder, buf1);

		/* write worst case bit pattern */
		for (secno = 0; secno < seccount; secno++)
		  system (17, 0x10 + disk, secno, buf2, track);
	      }

	    /* verifying */
	    printf (prmpt6);

	    for (track = 0; track < lasttrk; track++)
	    {
	      printf ("\b\b\b%03d", track);

	      for (secno = 0; secno < seccount; secno++)
		if (system(16, 0x10 + disk, secno, buf2, track))
		  if (track && sdc < 255 && sdc < seclen / sizeof *sd - 1)
		  {
		    sd[sdc].track  = track;
		    sd[sdc].sector = secno;
		    printf ("\n%d%s", ++sdc, prmpt7);
		    printf ("%s%03d", prmpt8, track);
		  }
		  else fatal_error ();
	      }

	      /* calc maximum block count */
	      *(int *)&buf3[seclen - sizeof(int)] = 
		(long)(seccount * (lasttrk - DPB->OFF) - sdc) 
		* (seclen / REC_SIZE) / (DPB->BLM + 1) - 1;

	      /* write spare directory */
	      if (system(17, 0x10 + disk, 0, buf3, 0) || 
		  system(17, 0x10 + disk, 1, buf4, 0)) fatal_error ();

	      /* initializing directory */
	      memset (buf2, 0xE5, seclen);

	      printf (prmpt9);

	      for (i = seccount;
		   i < (DPB->DRM + 1) / 4 / (seclen / REC_SIZE) + seccount;
		   i++)  	
	      {
		track = i / seccount;
		secno = i % seccount;

		/* calc offset */
		for (j = 0;; j++)
		       if (sd[j].track  > track) break; 
		  else if (sd[j].track == track) if (sd[j].sector > secno)
						   break;

		track = (i + j) / seccount;
		secno = (i + j) % seccount;

		if (system(17, 0x10 + disk, secno, buf2, track))
		  fatal_error ();
	      }

  }
}  
	 		

static char errmsg (errno)
  char errno;
{
  char c;

  switch (errno)
  {
    case 1: printf (error2);
	    _exit  (2);	
    case 2: printf (error3);
	    _exit  (2);
    case 3: printf (error4);
	    _exit  (2);	
    case 4: printf (error5);
	    break;
    case 5: printf (error6);
 	    break;
    case 6: printf (error7);
	    _exit  (2);
    case 7: printf (error8);
	    break;
    case 8: printf (error9);
	    break;
    case 9: printf (err10);
	    _exit  (2);
  }
  printf (prmt10);
  do
  {
    printf ("\b");
    c = toupper(getchar());
  }
  while (c != cancel && c != proceed && c != retry);
  printf ("\n");
  return c;
}


static restore (drive)
  char drive;
{
  uptask (drive, 0, 0, 0	     );
  outp   (COMND, REST + dctw[0] & 0xF);

  while (inp(STATUS) & 0x80);
}


static format_track (drive, surface, cylinder, buffer)
  char drive, surface, *buffer;
  int cylinder;
{
  int i;
  
  uptask (drive, surface, cylinder, 0);
  outp   (SECNT, seccount	     );
  outp   (COMND, FORMAT		     );

  for (i = 0; i < seclen; i++) outp (DATA, *buffer++);

  while (inp(STATUS) & 0x80);
}


static uptask (drive, surface, cylinder, sector)
  char drive, surface, sector;
  int cylinder;
{
  outp (WPC  , dctw[2]	    			      );
  outp (SECNO, sector 	    			      );
  outp (CYLHI, cylinder >> 8			      );
  outp (CYLLO, cylinder     			      );
  outp (SDH  , 0x80 + dctw[1] + (drive << 3) + surface);
}


static fatal_error ()
{
  printf (err11);
  _exit  (2);
}
