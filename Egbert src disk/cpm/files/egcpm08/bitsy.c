/******************************************************************************
*  B I T S Y  *  X U T I L S 0 0 0  *  T h o m a s   H o l t e  * 8 6 0 9 1 1 *
*******************************************************************************
*									      *
*   C O P Y   U T I L I T Y :   B I T S Y   <------->   G E N I E   I I I s   *
*   =======================================================================   *
*									      *
*									      *
*  Version 1.0							Thomas Holte  *
*									      *
*******************************************************************************
* Linking mit WINDOW.C | BITSY.C | Bibliothek: LIBCAP.REL                     *
* 27.12.92 Egbert Schr|er                                                     *
******************************************************************************/

#include <stdio.h>
#include <errno.h>
#include <bios.h>

/* ASCII control code */
#define ETX 	    0x03		   /* end of text		    */

#define OFFSET	       1		   /* number of directory tracks    */
#define SECCOUNT      10		   /* number of sectors per track   */
#define SURFACECOUNT   2		   /* number of tracks per cylinder */
#define SECLEN	     512		   /* sector length (bytes)	    */
#define SECMASK      (SECLEN / REC_SIZE)   /* number of 128 byte records
					      per sector		    */
#define FILECOUNT    (OFFSET * SECCOUNT * SECLEN / 16)

static char   drive;			/* current CP/M drive		     */
static char   Genie_drive;		/* disk drive with Genie IIIs format */
static char   unit;			/* unit # of drive P:		     */
static struct dct {
		    char td1, td2, ilf, spt, tc;
		  } DCT;		/* physical disk parameters	     */


_main ()
{
  unsigned base;		/* current physical record address   */
  char	   BITSY_drive;		/* disk drive with BITSY format      */
  char     BITSY_type;		/* type of BITSY drive		     */
  char	   BITSY_unit;		/* unit # of BITSY drive	     */
  char     buf[0x4000];		/* I/O buffer			     */
  char     c;			/* temporary keyboard character      */
  BOOL     empty;		/* Genie IIIs file empty ?	     */
  int	   f;			/* Genie IIIs file number	     */
  int      first;		/* first record being transferred    */
  BOOL	   GenieBITSY;		/* copy direction		     */
  char     Genie_type;		/* type of Genie drive		     */
  char     Genie_unit;		/* unit # of Genie drive	     */	
  char     get_type ();		/* get drive type  of CP/M drive     */
  char	   get_unit ();		/* get unit number of CP/M drive     */	
  int	   i, j;		/* temporary indices		     */
  int	   last;		/* last record being transferred     */
  int	   n;			/* file count			     */
  int      nbytes;		/* number of bytes being transferred */
  char	   opcode;		/* BDOS function code		     */
  char     path[13];		/* CP/M pathname		     */
  char     *p;			/* temporary pathname pointer	     */
  char     s[81];		/* keyboard string		     */ 
  char	   yesnoquit ();	/* check input character	     */

  /* Genie IIIs directory entry */
  struct {
	   char filler1;
	   char f[8];
	   char t[3];
	   char filler2[20];
	 } *direntry;

  /* CP/M file control block */
  static struct {
		  char dr;	/* drive			     */
		  char f[8];	/* file name			     */
		  char t[3];	/* file type			     */
		  char ex;	/* current extent		     */
		  char s[2];	/* internal			     */
		  char rc;	/* record count/ext		     */
		  char d[16];	/* internal			     */
		  char cr;	/* current record in ext	     */
		  char r[3];	/* random record		     */
		} FCB = {0, "????????", "???"};

  /* own directory */
  struct {
	   char     name[8];
	   char     type[3];
	   char     filler;
	   unsigned beg, end;
	 } entry[FILECOUNT];


  /* get current drive */
  drive = bdos(25);

  /* select national character set */
  printf ("\33A");

  for (;;)
  {
    printf ("\nBITSY Ver 2.0\n");
    printf ("\nMode    Function\n");
    printf ("\n1       Genie IIIs --> BITSY");
    printf ("\n2       BITSY --> Genie IIIs");
    printf ("\n3       End program\n\n");

    for (;;)
    {
      printf ("Enter your copy mode: ");
      gets   (s);
      printf ("\n");
      if (strlen(s) != 1) continue;
      switch (*s)
      {
	case '1': GenieBITSY = TRUE;
		  break;
	case '2': GenieBITSY = FALSE;
		  break;
	case '3': xabort (0, drive);
	default : continue;
      }
      break;
    }	

    n = 0;
    if (GenieBITSY)		/* Genie IIIs --> BITSY */
    {
      /* get source drive */
      for (;;)
      {
        printf ("Enter source drive: ");
        gets   (s);
        printf ("\n");
        if (strlen(s) != 1) continue;

	Genie_drive = toupper(*s) - 'A';
	if (Genie_drive < 15 && !bdos(14, Genie_drive)) break;
      }
      Genie_unit = get_unit(Genie_drive);
      Genie_type = get_type(Genie_drive);

      /* get destination drive */
      for (;;)
      {
        printf ("Enter destination drive: ");
        gets   (s);
	printf ("\n");
	if (strlen(s) != 1) continue;

	BITSY_drive = toupper(*s) - 'A';
	if (BITSY_drive < 15    && BITSY_drive    != Genie_drive && 
	    ((BITSY_unit = get_unit(BITSY_drive)) != Genie_unit  ||
	     (BITSY_type = get_type(BITSY_drive)) != Genie_type) &&
	    !select(BITSY_drive)) break;
      }

      memset (entry, 0, sizeof entry);		/* clear own directory */

      /* read Genie directory */
      bdos   (26, buf);
      direntry = buf;
      FCB.dr   = Genie_drive + 1;
      for (opcode = 17; n < FILECOUNT; opcode = 18)
      {
        switch (i = bdos(opcode, &FCB))
	{
	  case 0   :
	  case 1   :
	  case 2   :
	  case 3   : /* convert file name & type to ASCII */
		     for (j = 0; j < 8; j++) direntry[i].f[j] &= 0x7F;
		     for (j = 0; j < 3; j++) direntry[i].f[j] &= 0x7F;

		     printf ("%.8s.%.3s   Copy it (Y/N/Q) ?  ", 
			     direntry[i].f, direntry[i].t);

		     switch (yesnoquit())
		     {
		       /* get file name & type */
		       case 'Y': strncpy (entry[n].name, direntry[i].f, 8);
			         strncpy (entry[n].type, direntry[i].t, 3);
			         n++;

		       /* ignore */
		       case 'N': printf ("\n");
			         continue;
		     }
		     printf ("\n");
 	
	  case 0xFF: break;

	  default  : printf   ("\nFatal I/O error\n");
		     deselect ();	 
		     xabort   (2, drive);
	}
	break;
      }
      printf ("\n");

      base = OFFSET * SECCOUNT * SECMASK;   /* reset basic record address */

      /* big copy loop */
      for (i = 0; i < n; i++)
      {
	/* open Genie IIIs file */
	for (j = 0, p = path; j < 8 && (c = entry[i].name[j]) != ' '; j++)
	  *p++ = c;
          *p++ = '.';
        for (j = 0; j < 3 && (c = entry[i].type[j]) != ' '; j++)
	  *p++ = c;
	  *p++ = '\0';

	if ((f = open(path, 0)) == ERROR)
	{
	  printf ("Can't open %s\n", path);
	  continue;
	}

	printf ("Copying %s\n", path);

	entry[i].beg = entry[i].end = base;
	empty        = TRUE;

	while ((nbytes = read(f, buf, sizeof buf)) > 0)
	{
	  /* flush buffer */
	  empty = FALSE;
	  BITSYio (buf, nbytes, base / SECMASK, WRITE);
	  base += nbytes / REC_SIZE + (nbytes % REC_SIZE != 0);
	}

	if (empty) *entry[i--].name = '\0';
	else
	{
	  entry[i].end = --base;
	  base 	       = base / SECMASK * SECMASK + SECMASK;
	}
	close (f);
      }

      /* write own directory */
      BITSYio (entry, sizeof entry, 0, WRITE);
    }
    else
    {
      /* get source drive */
      for (;;)
      {
        printf ("Enter source drive: ");
        gets   (s);
	printf ("\n");
	if (strlen(s) != 1) continue;

	BITSY_drive = toupper(*s) - 'A';
	if (BITSY_drive < 15 && !select(BITSY_drive)) break;
      }
      BITSY_unit = get_unit(BITSY_drive);
      BITSY_type = get_type(BITSY_drive);

      /* get destination drive */
      deselect ();
      for (;;)
      {
        printf ("Enter destination drive: ");
        gets   (s);
        printf ("\n");
        if (strlen(s) != 1) continue;

	Genie_drive = toupper(*s) - 'A';
	if (Genie_drive < 15    && Genie_drive    != BITSY_drive && 
	    ((Genie_unit = get_unit(Genie_drive)) != BITSY_unit  ||
	     (Genie_type = get_type(Genie_drive)) != BITSY_type) &&
	    !bdos(14, Genie_drive)) break;
      }
      select (BITSY_drive);

      /* read own directory */
      BITSYio (entry, sizeof entry, 0, READ);

      /* big copy loop */
      for (i = 0; i < FILECOUNT; i++)
        if (*entry[i].name)
	{
	  /* create Genie IIIs file */
	  for (j = 0, p = path; j < 8 && entry[i].name[j] != ' '; j++)
	    *p++ = entry[i].name[j];
            *p++ = '.';
          for (j = 0; j < 3 && entry[i].type[j] != ' '; j++)
	    *p++ = entry[i].type[j];
	    *p++ = '\0';

	  if ((f = creat(path, 0)) == ERROR)
	  {
	    printf ("Can't create %s\n", path);
	    continue;
	  }
	  else printf ("Copying %s\n", path);

	  /* get first and last record of file */
	  first = entry[i].beg;
	  last  = entry[i].end;

	  for (base = first; base <= last; base += 128)
	  {
	    if ((j = last - base + 1) > 128) nbytes = 0x4000;
					else nbytes = j * REC_SIZE;
	    BITSYio (buf, nbytes, base / SECMASK, READ);

	    if (write(f, buf, nbytes) != nbytes)
	    {
	      if (errno == EROFS) printf ("\nDiskette write protected\n");
			     else printf ("\nFatal I/O error\n");
	      deselect ();
	      xabort   (2, drive);
	    }
	  }
	  close (f);
	}
    }
    deselect ();
    bdos     (    14, drive);
    bios     (SELDSK, drive);
  }
}


static char yesnoquit ()
{
  char c;		/* keyboard character */

  do
  {
    printf ("\b");
    if ((c = toupper(getchar())) == ETX)
    {
      printf   ("\n");
      deselect ();
      xabort   (1, drive);
    } 
  }
  while (c != 'Y' && c != 'N' && c != 'Q');
  return c;
}


static char get_unit (drive)
  char drive;
{
  char **DTBL;			/* ^drive table    */
  char unit;			/* physical unit # */

  /* get drive table */
  DTBL = bios(DRVTBL);

  /* get unit number */
  system (15, 1, 1, DTBL[drive] - 2, &unit);
  return unit;
}


static char get_type (drive)
  char drive;
{
  char **DTBL;			/* ^drive table */
  char type;			/* drive type   */

  /* get drive table */
  DTBL = bios(DRVTBL);

  /* get drive type */
  system (15, 1, 1, DTBL[drive] - 1, &type);
  return type;
}


static RESULT select (drive)
  char drive;
{
  struct {
	   char unit, type;	
	 } disk;		/* drive unit # & type */
  char   **DTBL;		/* ^drive table        */

  /* physical disk paramaters of BITSY drive */
  static struct dct BITSY_DCT = {0x70, 0x80, 4, 20, 80};


  /* get drive table */
  DTBL = bios(DRVTBL);

  /* does selected drive exist ? */
  if (!DTBL[drive]) return ERROR;

  /* get drive type & unit number */
  system (15, 1, 2, DTBL[drive] - 2, &disk);

  /* 5" floppy disk drive ? */
  if (disk.type || disk.unit >= 4) return ERROR;

  /* save unit number of drive P: */
  system (15, 1, 1, DTBL[15] - 2, &unit);

  /* save physical disk parameters of drive P: */
  system (15, 1, 5, DTBL[15] + 25, &DCT);

  /* load physical parameters of BITSY drive */
  system (15, 0x10, 5, &BITSY_DCT, DTBL[15] + 25);

  /* load unit number of BITSY drive */
  system (15, 0x10, 1, &disk.unit, DTBL[15] - 2);

  return SUCCESS;
}


static deselect ()
{
  char **DTBL;			/* ^drive table */

  /* get drive table */
  DTBL = bios(DRVTBL);

  /* reset init bit */
  DCT.td2 &= 0xFE;

  /* restore unit number of drive P: */
  system (15, 0x10, 1, &unit, DTBL[15] - 2);

  /* restore physical disk parameters of drive P: */
  system (15, 0x10, 5, &DCT, DTBL[15] + 25);
}


static xabort (mode, drive)
  int mode;
  char drive;
{
  bdos (    14, drive);
  bios (SELDSK, drive);
  exit (mode);
}


/*** physical disk I/O *******************************************************/

static BITSYio (buffer, nbytes, record, mode)
  char *buffer;
  unsigned nbytes, record;
  int mode;
{
  char     c;			/* keyboard character 		 */
  unsigned lastrec;		/* last record being transferred */

  if (nbytes)
  {
    bios (SELDSK, 15);			/* select drive P: */

    lastrec = record + (nbytes - 1) / SECLEN;
    while (record <= lastrec)
    {
      bios (SETTRK, record   / (SECCOUNT * SURFACECOUNT));
      bios (SETSEC, record++ % (SECCOUNT * SURFACECOUNT));
      bios (SETDMA, buffer);

      /* read or write sector */
      switch (bios(mode))
      {
	/* no errors */
	case 0 : break;

	/* diskette write protected */
	case 2 : printf ("\nDiskette write protected\n");
		 goto error;

	/* fatal I/O errors */
	default: printf ("\nFatal I/O error\n");

		 error:
		 printf ("\n(C)ancel, (P)roceed, or (R)etry\n");
		 do
		 {
		   printf ("\b");
		   c = toupper(getchar());
	         }
	         while (c != 'C' && c != 'P' && c != 'R');
		 printf ("\n");

		 if (c == 'C')
		 {
		   deselect ();
		   xabort   (2, drive);
		 }
		 if (c == 'R')
		 {
		   record--;
		   continue;
		 }
      }

      buffer += SECLEN;			/* bump buffer pointer     */
    }

    bios (SELDSK, Genie_drive);		/* select Genie IIIs drive */ 
  }
}
