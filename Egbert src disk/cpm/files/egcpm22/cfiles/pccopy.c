/******************************************************************************
*  P C C O P Y  *  X U T I L S 0 0 2  * T h o m a s   H o l t e * 8 7 0 3 0 1 *
*******************************************************************************
*									      *
*  C O P Y   U T I L I T Y :   G E N I E   I I I s   <------>   I B M   P C   *
*  ========================================================================   *
*									      *
*									      *
*  Version 1.1							Thomas Holte  *
*									      *
******************************************************************************/

#include <stdio.h>
#include <errno.h>
#include <bios.h>

/* ASCII control code */
#define ETX      0x03			/* end of text 	     	 */
#define SUB	 0x1A			/* substitute		 */

#define EOF      0xFFF 			/* PC DOS EOF marker 	 */
#define SEC_SIZE 512			/* size of PC DOS sector */

static int    clcount;			/* number of clusters		     */
static int    clmask;			/* # of sectors/cluster		     */
static int    clsize;			/* size of cluster		     */
static struct DCT {
		    char td1, td2, ilf, spt, tc, curtrk;
		  } SDCT;		/* physical disk parameters	     */
       struct DCT dct = {0x7C, 0x80, 1, 18, 40}; /* physical disk parameters 
					   	    of IBM drive (9/2)	     */
static int    dirsize;			/* size of PC DOS directory	     */
static char   drive;			/* current CP/M drive	   	     */
static char   *FAT;			/* ^file alloaction table  	     */
static int    fatsecs;			/* # of sectors/FAT		     */
static int    fatsize;			/* size of file allocation table     */
static int    first_dir_rec;		/* first PC DOS directory record     */
static struct ownentry {		/* own directory entry		     */
	                 char   path[13];	/* path name   		     */
			 int    firstcl;	/* first cluster of file     */
			 long   size;		/* file size		     */
	                 struct ownentry *next;	/* ^next entry 		     */
	               } *entry, *first_entry, *last_entry;
static int    firstrec;			/* record # of first cluster	     */
static BOOL   format;			/* formatter called ?		     */
static char   Genie_drive;		/* disk drive with Genie IIIs format */
static int    max_file_count;		/* maximum PC DOS file count	     */
static struct PCentry {
	        	char file[8];		/* file name	       	     */
			char type[3];		/* file type	       	     */
			char attrib;		/* file attributes     	     */
			char reserved[10];
			int  time;		/* time of last update 	     */
			int  date;		/* date of last update 	     */
			int  firstcl;		/* starting cluster    	     */
			long size;		/* file size	       	     */
	      	      } *pcentry;	/* ^PC DOS directory entry 	     */
static int    secspertrk;		/* sectors per PC DOS track	     */
       char   stab[] = {0,  1,  2,  3,  4,  5,  6,  7,  8, 
			9, 10, 11, 12, 13, 14, 15, 16, 17};  /* sector table */
static char   unit;			/* unit # of drive P:		     */

_main ()
{
  char buf[0x4000]; 		/* I/O buffer 			     */
  int  cluster;			/* current cluster		     */
  char directory[7 * SEC_SIZE]; /* PC DOS directory		     */
  char disk;			/* unit number of IBM drive	     */
  char errmsg ();		/* display error message	     */
  char error;			/* error code of track formatter     */	
  int  f;			/* Genie IIIs file number	     */	
  char fat      [2 * SEC_SIZE];	/* file allocation table	     */
  char file[8];			/* temporary file name		     */
  long file_size;		/* size of file being copied	     */
  BOOL first;			/* first cluster of file ?	     */
  int  firstcl;			/* # of first cluster of file	     */ 
  char Genie_type;		/* type of Genie drive		     */
  char Genie_unit;		/* type of Genie_unit		     */
  char get_type ();		/* get type of CP/M drive	     */
  char get_unit ();		/* get unit number of CP/M drive     */
  int  i, j;			/* temporary indices 		     */
  char IBM_drive;		/* disk drive with PC DOS format     */
  char IBM_type;		/* type of PC DOS drive		     */
  char IBM_unit;		/* unit # of PC DOS drive	     */	
  int  mode;			/* copy mode			     */
  int  nbytes;			/* number of bytes being transferred */
  int  oldcl;			/* # of previous cluster	     */
  char opcode;			/* BDOS function code		     */ 
  char *p;			/* temporary pathname pointer 	     */
  char s[81];			/* keyboard string		     */
  char secno;			/* current sector number	     */	
  BOOL select_file ();		/* select file to be copied	     */
  char side;			/* current surface number	     */
  char track;			/* current track number		     */
  char type[3];			/* temporary file type		     */
  char yesno     ();		/* check input character	     */	
  char yesnoquit ();

  /* Genie IIIs directory entry */
  struct {
	   char filler1;
	   char f[8];
	   char t[3];
	   char filler2[20];
	 } *cpmentry;

  /* CP/M file control block */
  static struct {
		  char dr;	/* drive		 */
		  char f[8];	/* file name		 */
		  char t[3];	/* file type		 */
		  char ex;	/* current extent	 */
		  char s[2];	/* internal		 */
		  char rc;	/* record count/ext	 */
		  char d[16];	/* internal		 */
		  char cr;	/* current record in ext */
		  char r[3];	/* random record	 */
		} FCB = {0, "????????", "???"};

  /* get current drive */
  drive = bdos(25);

  /* initialize globals */
  FAT     = fat;
  pcentry = directory;

  /* select national character set */
  printf ("\33A");

  for (;;)
  {
    printf ("\nPC-COPY Ver 1.1\n");
    printf ("\nMode    Function\n");
    printf ("\n1       Genie IIIs --> PC DOS");
    printf ("\n2       PC DOS --> Genie IIIs");
    printf ("\n3       Format PC DOS diskette");
    printf ("\n4       End program\n\n");

    for (;;)
    {
      format      = FALSE; 	
      first_entry = NULL;

      printf ("Enter your copy mode: ");
      gets   (s);
      printf ("\n");
      if (strlen(s) != 1) continue;
      switch (mode = *s - '0')
      {
        case 1 : /* get source drive */
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

		   IBM_drive = toupper(*s) - 'A';
		   if (IBM_drive < 15     && IBM_drive   != Genie_drive &&
		       ((IBM_unit = get_unit(IBM_drive)) != Genie_unit  ||
			(IBM_type = get_type(IBM_drive)) != Genie_type) &&
	 	       !select(IBM_drive)) break;
		 }
		    
		 /* read Genie directory */
		 bdos (26, buf);
		 cpmentry = buf;
		 FCB.dr   = Genie_drive + 1;
		 for (opcode = 17;; opcode = 18)
		 {
		   switch (i = bdos(opcode, &FCB))
		   {
		     case 0   :
		     case 1   :
		     case 2   :
		     case 3   : /* convert file name & type to ASCII */
				for (j = 0; j < 8; j++) 
				  cpmentry[i].f[j] &= 0x7F;
				for (j = 0; j < 3; j++)
				  cpmentry[i].t[j] &= 0x7F;

				printf ("%.8s.%.3s   Copy it (Y/N/Q) ?  ",
					cpmentry[i].f, cpmentry[i].t);

				switch (yesnoquit())
				{
				  /* build own entry */
				  case 'Y': if (first_entry) 
					      last_entry = entry;
					    if (!(entry = calloc(1, 
						     sizeof(struct ownentry))))
					    {
					      printf   ("\nMemory overflow\n");
					      deselect ();
					      xabort   (2, drive);
					    }
					    if (first_entry) 
					      last_entry->next = entry;
					    else 
					      last_entry = first_entry = entry;

					    for (j = 0, p = entry->path;
						 j < 8 && 
						 cpmentry[i].f[j] != ' '; j++)
					      *p++ = cpmentry[i].f[j];
					      *p++ = '.';
					    for (j = 0; j < 3 &&
						 cpmentry[i].t[j] != ' '; j++)
					      *p++ = cpmentry[i].t[j];
					      *p++ = '\0';

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

		 /* big copy loop */
		 entry = first_entry;
		 while (entry)
		 {
		   /* reset file size */
		   file_size = 0L;
		   first     = TRUE;		

		   /* open Genie IIIs file */
		   if ((f = open(entry->path, 0)) == ERROR)
		   {
		     printf ("Can't open %s\n", entry->path);
		     goto next_open;
		   }

		   printf ("Copying %s\n", entry->path);

		   /* get file name & type */
		   for (i = 0, p = entry->path; i < 8 && *p != '.'; i++, p++)
		     file[i] = *p;
		   for (p++  ; i < 8      ; i++     ) file[i] = ' ';
		   for (i = 0; i < 3 && *p; i++, p++) type[i] = *p;
		   for (     ; i < 3      ; i++	    ) type[i] = ' ';

		   /* does PC DOS file already exist ? */
		   for (i = 0; i < max_file_count; i++)
		     if (!strncmp(file, pcentry[i].file, 8) && 
			 !strncmp(type, pcentry[i].type, 3)) break;
		   
		   if (i >= max_file_count)
		     /* search first free PC DOS directory entry */
		     for (i = 0; i < max_file_count; i++)
		     {
		       switch (*pcentry[i].file)
		       {
		         case 0x00:
		         case 0xE5: break;
		         default  : continue;
		       }
		       break;
		     }		
		   else /* clear old clusters of file */
		        if (oldcl = pcentry[i].firstcl)
		          while (oldcl < 0xFF8)
		          {
		            cluster = nextcl(oldcl);
		            alloccl (oldcl, 0);
		            oldcl   = cluster;
		          }

		   cluster = 1;
		  
		   if (i >= max_file_count)
		   {
		     printf   ("\nNo more directory space\n");
		     err_exit ();
		   }
		   
		   while ((nbytes = read(f, buf, sizeof buf)) > 0)
		   {
		     /* fill rest of buffer with CP/M's end of text marker */
		     memset (buf + nbytes, SUB, sizeof buf - nbytes);

		     /* write buffer */
		     for (j = 0, p = buf; j < nbytes; j += clsize, p += clsize)
		     {
		       /* get next free cluster of PC DOS diskette */
		       for (;;)
		       {
			 switch (nextcl(++cluster))
			 {
			   case ERROR: printf ("\nDisk full\n");

				       /* free clusters already written */
				       if (!first)
				       {
					 alloccl (oldcl, EOF);
					 while (firstcl < 0xFF8)
					 {
					   cluster = nextcl(firstcl);
					   alloccl (firstcl, 0);
					   firstcl = cluster;
					 }
				       }

				       /* erase directory entry */
				       if (*pcentry[i].file) 
					 *pcentry[i].file = 0xE5;

				       err_exit ();

			   case     0: break;

			   default   : continue;
			 }
			 break;
		       }

		       /* write cluster */
		       IBMio (p, clsize, firstrec + cluster * clmask, WRITE);

		       /* first cluster of file ? */
		       if (first)
		       {
			 firstcl = oldcl = cluster;
			 first   = FALSE;
		       }
		       else
		       {
			 alloccl (oldcl, cluster);		
			 oldcl = cluster;
		       }
		     }
		     
		     /* increment file size */
		     file_size += nbytes;
		   }

		   if (nbytes == ERROR)
		   {
		     printf   ("\nFatal I/O error\n");

		     /* free clusters already written */
		     if (!first)
		     {
		       alloccl (oldcl, EOF);
		       while (firstcl < 0xFF8)
		       {
		         cluster = nextcl(firstcl);
			 alloccl (firstcl, 0);
			 firstcl = cluster;
		       }
		     }

		     /* erase directory entry */
		     if (*pcentry[i].file) *pcentry[i].file = 0xE5;

		     err_exit ();
		   }

		   /* write EOF cluster */
		   if (file_size) alloccl (oldcl, EOF);

		   /* fill PC DOS directory entry */	
		   strncpy (pcentry[i].file, file, 8);
		   strncpy (pcentry[i].type, type, 3);

		   pcentry[i].attrib  = 0;
		   memset (pcentry[i].reserved, 0, sizeof pcentry[i].reserved);
		   p                  = cpm3_time();
		   p[2] = p[5]        = '\0';
		   pcentry[i].time    = (atoi(p    ) << 11) 
				      + (atoi(p + 3) <<  5) + atoi(p + 6) / 2;
		   p                  = cpm3_date();
		   p[2] = p[5]        = '\0';
		   pcentry[i].date    = (atoi(p + 6) - 80 << 9) 
				      + (atoi(p    )      << 5) + atoi(p + 3);
		   pcentry[i].firstcl = firstcl;
		   pcentry[i].size    = file_size;

		   /* close Genie IIIs file */
		   close (f);

		   /* get next file */
		   next_open:
		   first_entry = entry;
		   entry       = entry->next;
		   free (first_entry);
		 }

		 /* write FAT */
		 IBMio (FAT, fatsize, 1		 , WRITE);
		 IBMio (FAT, fatsize, 1 + fatsecs, WRITE);

		 /* write directory */
		 IBMio (pcentry, dirsize, first_dir_rec, WRITE);

		 break;

        case 2 : /* get source drive */
		 for (;;)
		 {
		   printf ("Enter source drive: ");
		   gets   (s);
		   printf ("\n");
		   if (strlen(s) != 1) continue;

		   IBM_drive = toupper(*s) - 'A';
		   if (IBM_drive < 15 && !select(IBM_drive)) break;
		 }
		 IBM_unit = get_unit(IBM_drive);
		 IBM_type = get_type(IBM_drive);

		 /* get destination drive */
		 deselect ();
		 for (;;)
		 {
		   printf ("Enter destination drive: ");
		   gets   (s);
		   printf ("\n");
		   if (strlen(s) != 1) continue;

		   Genie_drive = toupper(*s) - 'A';
		   if (Genie_drive < 15     && Genie_drive   != IBM_drive &&
		       ((Genie_unit = get_unit(Genie_drive)) != IBM_unit  ||
			(Genie_type = get_type(Genie_drive)) != IBM_type) &&
	 	       !bdos(14, Genie_drive)) break;
		 }
		 select (IBM_drive);

		 /* read PC DOS directory */
		 for (i = 0; i < max_file_count; i++)
		 {
		   switch (*pcentry[i].file)
		   {
		     /* end of directory */
		     case 0x00: goto endfor;	

		     /* parent directory */
		     case '.' :

		     /* erased file */
		     case 0xE5: continue;

		     /* 1st char of filename = E5 */
		     case 0x05: *pcentry[i].file = 0xE5;

		     /* normal filename (may be volume label) */
		     default  : if (pcentry[i].attrib & 0x08) continue;
		   }

		   /* convert file name & type to ASCII */
		   for (j = 0; j < 8; j++) pcentry[i].file[j] &= 0x7F;
		   for (j = 0; j < 3; j++) pcentry[i].type[j] &= 0x7F;

		   if (pcentry[i].attrib & 0x10)
		   {
		     printf ("\nSubdirectory %.8s   Select it (Y/N) ?  ", 
			     pcentry[i].file);

		     if (yesno() == 'Y')
		     {   
		       printf  ("\n\n");	
		       readsub (&pcentry[i]);
		       printf  ("\nEnd of subdirectory %.8s\n", 
				pcentry[i].file);
		     }	
		     else printf ("\n");
			  printf ("\n");
		   }
		   else if (select_file(&pcentry[i])) 
			{
			  printf ("\n");
			  break;
			}
		 }
		 endfor:
		 printf ("\n");

		 /* big copy loop */
		 entry = first_entry;
		 while (entry)
		 {
		   /* reset file size */
		   file_size = 0L;
		   cluster   = entry->firstcl;

		   /* create Genie IIIs file */
		   if ((f = creat(entry->path, 0)) == ERROR)
		   {
		     printf ("Can't create %s\n", entry->path);
		     goto next_creat;
		   }

		   printf ("Copying %s\n", entry->path);

		   while (file_size < entry->size)
		   {
		     if (entry->size - file_size > sizeof buf) 
		          nbytes = sizeof buf;
		     else nbytes = entry->size - file_size;

		     /* read PC DOS file */
		     for (i = 0, p = buf; i < nbytes; i += clsize, p += clsize)
		     {
		       /* read cluster */
		       IBMio (p, clsize, firstrec + cluster * clmask, READ);

		       /* get next cluster of PC DOS file */
		       cluster = nextcl(cluster);
		     }

		     /* fill rest of buffer with CP/M's EOF */
		     memset (buf + nbytes, SUB, sizeof buf - nbytes);

		     /* write buffer */
		     if ((j = write(f, buf, nbytes)) < nbytes)
		     {
		       if (j == ERROR) 
			 if (errno == EROFS) 
			      printf ("\nDiskette write protected\n");
			 else printf ("\nFatal I/O error\n");
		       else   printf ("\nDisk full\n");     
		
		       deselect ();
		       xabort   (2, drive);
		     }
		
		     /* increment file size */
		     file_size += nbytes;
		   }

		   /* close Genie IIIs file */
		   if (close(f))
		   {
		     printf   ("\nFatal I/O error\n");
		     deselect ();
		     xabort   (2, drive);
		   }

		   /* get next file */
		   next_creat:
		   first_entry = entry;
		   entry       = entry->next;
		   free (first_entry);
		 }	
		 break;
		     
	case 3 : format = TRUE;

		 /* get drive */
		 for (;;)
		 {
		   printf ("Enter drive: ");
		   gets   (s);
		   printf ("\n");
		   if (strlen(s) != 1) continue;

		   IBM_drive = toupper(*s) - 'A';
		   if (IBM_drive < 15 && !select(IBM_drive)) break;
		 }

		 /* prompt */
		 printf ("Do you really want to format disk %c ?  ",
			 IBM_drive + 'A');
		 if (yesno() == 'N') 
		 {
		   printf ("\n\n");
		   break;
		 }

		 /* get unit number */
		 disk = get_unit(IBM_drive);

		 /* build track buffer */
		 for (i = 0, secno = 1; i < 5508; i += 612, secno++)
		 {
		   j = 0;
		   while (j <  40) buf[i + j++] = 0x4E;
		   while (j <  52) buf[i + j++] = 0x00;
		   while (j <  55) buf[i + j++] = 0xF5;
				   buf[i + j  ] = 0xFE;
					   j   += 3;
				   buf[i + j++] = secno;
				   buf[i + j++] = 2;
				   buf[i + j++] = 0xF7;
		   while (j <  83) buf[i + j++] = 0x4E;
		   while (j <  95) buf[i + j++] = 0x00;
		   while (j <  98) buf[i + j++] = 0xF5;
		   		   buf[i + j++] = 0xFB;
		   while (j < 611) buf[i + j++] = 0xE5;
		   		   buf[i + j++] = 0xF7;
		 }
		 for (i = 5508; i < sizeof buf; i++) buf[i] = 0x4E;

		 /* formatting */
		 printf ("\n\nFormatting track   ");

		 for (track = 0; track < 40; track++)
		 {
		   for (i = 56; i < 5508; i += 612) buf[i] = track;
		   printf ("\b\b%02d", track);

		   for (side = 0; side < 2; side++)
		   {
		     for (i = 57; i < 5508; i += 612) buf[i] = side;

		     /* format track */
		     if (error = formtrk(disk, side, track, buf))
		     {
		       switch (errmsg(error))
		       {
			 case 'C': deselect ();
				   xabort   (2, drive);

			 case 'R': track--;
		       }
		       printf ("\nFormatting track   ");		
		       break;
		     }
		   }
		 }		

		 /* verifying */
		 printf ("\rVerifying track    ");

		 for (track = 0; track < 40; track++)
		 {
		   printf ("\b\b%02d", track);

		   if (error = checktrk(disk, track))
		   {
		     switch (errmsg(error))
		     {
		       case 'C': deselect ();
				 xabort   (2, drive);

		       case 'R': for (i = 56; i < 5508; i += 612)
				   buf[i] = track;
		   		 for (side = 0; side < 2; side++)
		   		 {
		     		   for (i = 57; i < 5508; i += 612)
				     buf[i] = side;

		                   formtrk (disk, side, track, buf);
				 }
				 track--;
		     }
		     printf ("\nVerifying track   ");
		   }
		 }		
		
		 /* write empty FAT */
		 memset (FAT, 0, fatsize);
		 FAT[0] = 0xFD; FAT[1] = FAT[2] = 0xFF;

		 IBMio  (FAT, fatsize, 1          , WRITE);
		 IBMio  (FAT, fatsize, 1 + fatsecs, WRITE);

		 printf ("\n\n");
		 break;

	case 4 : xabort (0, drive);

        default: continue;
      }
      deselect ();
      bdos     (    14, drive);
      bios     (SELDSK, drive);
      break;	
    }
  }
}	
      

static char yesno ()
{
  char c;			/* keyboard character */

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
  while (c != 'Y' && c != 'N');
  return c;
}


static char yesnoquit ()
{
  char c;			/* keyboard character */

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


static char yesnoqren ()
{
  char c;			/* keyboard character */

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
  while (c != 'Y' && c != 'N' && c != 'R' && c != 'Q');
  return c;
}


static BOOL select_file (pcentry)
  struct PCentry *pcentry;
{
  char c;			/* temporary keyboard character */
  int  i;			/* temporary index		*/
  char *p;			/* temporary pathname pointer   */
  char s[81];			/* keyboard string		*/

  printf ("%.8s.%.3s   Copy it (Y/N/R/Q) ?  ", pcentry->file, pcentry->type);

  switch (c = yesnoqren())
  {
    /* get new name */
    case 'R': printf ("   New name ? ");
	      gets   (s);

    /* build own entry */
    case 'Y': if (first_entry) last_entry = entry;
	      if (!(entry = calloc(1, sizeof(struct ownentry))))
	      {
		printf   ("\nMemory overflow\n");
		deselect ();
		xabort   (2, drive);
	      }
	      if (first_entry) last_entry->next = entry;
	          else last_entry = first_entry = entry;

	      if (c == 'R')
	      {
		strncpy (entry->path, s, 13);
		for (i = 0; i < 13; i++) if (!entry->path[i]) goto endif;
		entry->path[12] = '\0';
	      }
	      else
	      {
		for (i = 0, p = entry->path; i < 8 && pcentry->file[i] != ' ';
		     i++)
		  *p++ = pcentry->file[i];
		  *p++ = '.';
		for (i = 0; i < 3 && pcentry->type[i] != ' '; i++)
		  *p++ = pcentry->type[i];
		  *p++ = '\0';
	      }

	      endif:
	      entry->firstcl = pcentry->firstcl;
	      entry->size    = pcentry->size;	 

    /* ignore */
    case 'N': if (c != 'R') printf ("\n");
	      return FALSE;
  }

  return TRUE;
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
	   char unit, type;		/* drive unit # & type    */
	 } disk;			
  char   **DTBL;			/* ^drive table	          */
  int    i;				/* drive parameters index */
	
  /* physical parameters of PC DOS diskettes */
  static struct DCT PCDOS_DCT[4] = {{0x7C, 0x80, 1, 16, 40},	/* 8/2 */
				    {0x3C, 0x80, 1,  8, 40},	/* 8/1 */
				    {0x7C, 0x80, 1, 18, 40},	/* 9/2 */
				    {0x3C, 0x80, 1,  9, 40}};	/* 9/1 */

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
  system (15, 1, 5, DTBL[15] + 25, &SDCT);

  /* load physical parameters of PC DOS drive (8/1) */
  system (15, 0x10, 5, &PCDOS_DCT[1], DTBL[15] + 25);

  /* load unit number of PC DOS drive */
  system (15, 0x10, 1, &disk.unit, DTBL[15] - 2);

  /* read file allocation table */
  if (format) *FAT = 0xFD;
  else
  {
    secspertrk = 8;
    IBMio (FAT, 2 * SEC_SIZE, 1, READ);

    /* check diskette type */
    if (*(int *)(FAT + 1) != -1) return ERROR;
  }

  switch (*FAT)
  {
    case 0xFC: clcount	      = 353;			/* 9/1 */
	       clmask	      =   1;
	       clsize	      =     SEC_SIZE;
	       dirsize	      = 4 * SEC_SIZE;
	       fatsecs	      =   2;
	       fatsize	      = 2 * SEC_SIZE;
	       first_dir_rec  =   5;
	       firstrec	      =   7;
	       max_file_count =  64;
	       secspertrk     =   9;
	       i	      =   3;		
	       goto load;

    case 0xFD: clcount	      = 356;			/* 9/2 */
	       clmask	      =   2;
	       clsize         = 2 * SEC_SIZE;
	       dirsize	      = 7 * SEC_SIZE;	
	       fatsecs	      =   2;
	       fatsize	      = 2 * SEC_SIZE;
	       first_dir_rec  =   5;	
	       firstrec	      =   8;
	       max_file_count = 112;
	       secspertrk     =  18;
	       i	      =   2;
	       goto load;
	       
    case 0xFE: clcount	      = 315;			/* 8/1 */
	       clmask	      =   1;
	       clsize         =     SEC_SIZE;
	       dirsize	      = 4 * SEC_SIZE;
	       fatsecs        =   1;
	       fatsize	      =     SEC_SIZE;
	       first_dir_rec  =   3;	
	       firstrec	      =   5;
	       max_file_count =  64;
	       secspertrk     =   8;
	       i	      =   1;
	       goto load;

    case 0xFF: clcount	      = 317;			/* 8/2 */
	       clmask	      =   2;	
	       clsize 	      = 2 * SEC_SIZE;
	       dirsize	      = 7 * SEC_SIZE;
	       fatsecs	      =   1;
	       fatsize	      =     SEC_SIZE;
	       first_dir_rec  =   3;
	       firstrec	      =   6;
	       max_file_count = 112;
	       secspertrk     =  16;
	       i	      =   0;	

	       /* load physical parameters of PC DOS drive */
  	       load: 
	       system (15, 0x10, 5, &PCDOS_DCT[i], DTBL[15] + 25);
		
	       /* read directory */
	       if (!format) IBMio (pcentry, dirsize, first_dir_rec, READ);

	       return SUCCESS;

    default  : deselect ();
	       return ERROR;
  }
}

 	
static deselect ()
{
  char **DTBL;			/* ^drive table */

  /* get drive table */
  DTBL = bios(DRVTBL);

  /* reset init bit */
  SDCT.td2 &= 0xFE;

  /* restore unit number of drive P: */
  system (15, 0x10, 1, &unit, DTBL[15] - 2);

  /* restore physical disk parameters of drive P: */
  system (15, 0x10, 5, &SDCT, DTBL[15] + 25);
}


static readsub (pcentry)
  struct PCentry *pcentry;
{
  char buf[2 * SEC_SIZE];	/* directory buffer		      */
  int  cluster;			/* current cluster number	      */
  int  entry_count;		/* # of active directory entries      */
  int  i, j;   			/* temporary indices		      */
  char *p;			/* temporary buffer pointer	      */
  
  /* get number of first subdirectory cluster */
  cluster = pcentry->firstcl;

  /* process subdirectory */
  for (;;)
  {
    /* read next (two) cluster(s) of subdirectory */
    for (i = entry_count = 0, p = buf;
	 i < clsize / SEC_SIZE && cluster < 0xFF8; 
	 i++, entry_count += clsize / sizeof(struct PCentry), p += clsize)
    {
      IBMio (p, clsize, firstrec + cluster * clmask, READ);
      cluster = nextcl(cluster);
    }

    /* no further entries */
    if (!entry_count) return; 

    for (i = 0, pcentry = buf; i < entry_count; i++)
    {
      switch (*pcentry[i].file)
      {
        /* end of subdirectory */
        case 0x00: return;

	/* parent directory */
	case '.':

	/* erased file */
	case 0xE5: continue;

	/* 1st char of filename = E5 */
	case 0x05: *pcentry[i].file = 0xE5;
      }

      /* convert file name & type to ASCII */
      for (j = 0; j < 8; j++) pcentry[i].file[j] &= 0x7F;
      for (j = 0; j < 3; j++) pcentry[i].type[j] &= 0x7F;

      if (pcentry[i].attrib & 0x10)
      {
	printf ("\nSubdirectory %.8s   Select it (Y/N) ?  ", pcentry[i].file);

	if (yesno() == 'Y')
	{
	  printf  ("\n\n");
	  readsub (&pcentry[i]);
	  printf  ("\nEnd of subdirectory %.8s\n", pcentry[i].file);
	}
	else printf ("\n");
	     printf ("\n");
      }
      else if (select_file(&pcentry[i])) 
	   {
	     printf ("\n");
	     break;
	   }
    }
  }
}


static int nextcl (cluster)
  int cluster;
{
  unsigned nextcl;		/* # of next cluster */

  if (cluster >= clcount && cluster < 0xFF7) return ERROR;

  nextcl = *(unsigned *)(FAT + cluster * 3 / 2);

  return cluster & 1 ? nextcl >> 4 : nextcl & 0xFFF;
}


static alloccl (oldcl, newcl)
  int oldcl, newcl;
{
  unsigned cluster;		/* temporary cluster number */
  unsigned *p;			/* temporary pointer	    */

  p       = FAT + oldcl * 3 / 2;
  cluster = *p;
  *p	  = oldcl & 1 ? cluster & 0xF | newcl << 4 : cluster & 0xF000 | newcl;
}


static char errmsg (errno)
  char errno;
{
  char c;

  switch (errno)
  {
    case 1: printf ("\n\nIllegal drive #\n");
	    goto fatal;
    case 2: printf ("\n\nTrack # too high\n");
	    goto fatal;
    case 3: printf ("\n\nSector # too high\n");
	    goto fatal;
    case 4: printf ("\n\nDevice not available\n");
	    break;
    case 5: printf ("\n\nWrite protected diskette\n");
	    break;
    case 6: printf ("\n\nWrite fault on disk drive\n");
	    goto fatal;
    case 7: printf ("\n\nData record not found\n");
	    break;
    case 8: printf ("\n\nParity error\n");
	    break;
    case 9: printf ("\n\nLost data\n");
	    fatal:
	    deselect ();
	    xabort   (2, drive);
  }
  printf ("\n(C)ancel, (P)roceed, or (R)etry\n");
  do
  {
    printf ("\b");
    c = toupper(getchar());
  }
  while (c != 'C' && c != 'P' && c != 'R');
  printf ("\n");
  return c;
}


static err_exit ()
{
  /* write FAT */
  IBMio (FAT, fatsize, 1	  , WRITE);
  IBMio (FAT, fatsize, 1 + fatsecs, WRITE);

  /* write directory */
  IBMio (pcentry, dirsize, first_dir_rec, WRITE);

  deselect ();
  xabort   (2, drive);
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

static IBMio (buffer, nbytes, record, mode)
  char *buffer;
  unsigned nbytes, record;
  int mode;
{
  char     c;			/* keyboard character 		 */
  unsigned lastrec;		/* last record being transferred */

  if (nbytes)
  {
    bios (SELDSK, 15);			/* select drive P: */

    lastrec = record + (nbytes - 1) / SEC_SIZE;
    while (record <= lastrec)
    {
      bios (SETTRK, record   / secspertrk);
      bios (SETSEC, record++ % secspertrk);
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

      buffer += SEC_SIZE;		/* bump buffer pointer     */
    }

    bios (SELDSK, Genie_drive);		/* select Genie IIIs drive */ 
  }
}
