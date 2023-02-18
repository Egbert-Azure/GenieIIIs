/******************************************************************************
*  B A C K U P  *  U T I L S 0 0 1  *  T h o m a s   H o l t e  * 8 6 0 9 1 1 *
*******************************************************************************
* 									      *
*    B A C K U P   U T I L I T Y   F O R   L A R G E   M A S S   M E D I A    *
*    =====================================================================    *
* 									      *
*    O N   C P / M - B A S E D   M I C R O C O M P U T E R   S Y S T E M S    *
*    =====================================================================    *
* 									      *
* 									      *
*   Thomas Holte			                         Version 4.0  *
* 									      *
*******************************************************************************
* Linking mit BACKUPG.MAC | BACKUP.C \ Bibliothek: LIBCAP.REL                 *
* 27.12.92 Egbert Schr|er                                                     *
******************************************************************************/

#include <stdio.h>
#include <bios.h>

#define VERIFY 17

#define WRUAL 2			/* write to unallocated */

/* disk parameter header */
struct dph {
	     char *XLT, filler[9], MF;
	     struct dpb {
		          int  SPT;
		          char BSH, BLM, EXM;
		          int  DSM, DRM;
		          char AL0, AL1;
		          int  CKS, OFF;
		          char PSH, PHM;
		        } *DPB;
	     char *CSV, *ALV, *DIRBCB, *DTABCB, *HASH, HBANK;
           };

/* language dependent variables (globals) */
extern char cancel, error5[], error6[], prmt12[], prmt13[], prmt14[], prmt15[],
	    proceed, retry;

extern BOOL diskio ();			/* physical disk I/O		   */
static char drive;			/* current drive		   */
static int  block;			/* current block		   */


main (argc, argv)
  int argc;
  char *argv[];
{
  /* language dependent variables */
  extern char error2[], error3[], error4[], error7[], national[], prmpt1[],
	      prmpt2[], prmpt3[], prmpt4[], prmpt5[], prmpt6[], prmpt7[],
	      prmpt8[], prmpt9[], prmt10[], prmt11[], restore[], save[];

	 BOOL blockio ();		/* physical block I/O		   */
         char class;			/* character class		   */
	 char state;			/* state of decision table	   */
	 char val;			/* work value of decision table	   */ 
	 char work;			/* current task			   */
	 char s[80];			/* console input buffer		   */
	 int  firsti, i, j, m, n, no;	/* temporary counters		   */
	 char temp;	
	 int  start;			/* control var for "getpar" loop   */

	 int  sblocksiz;		/* block  size of source      disk */
	 int  dblocksiz;		/* block  size of destination disk */
	 int  ssecsiz;			/* sector size of source      disk */
	 int  dsecsiz;			/* sector size of destination disk */
	 int  secsperblock;		/* dest sectors / source block	   */
	 int  dirsize;			/* directory size of source disk   */
	 BOOL *blocks;			/* allocated blocks		   */ 
	 int  count;			/* source blocks / dest disk	   */
	 int  firstblock;		/* first block			   */
         int  lastblock;		/* last block			   */

	 unsigned entry_count;		/* # of CP/M directory entries/buf */ 
	 unsigned sector;		/* current sector		   */
	 unsigned firstsec;		/* first sector			   */
	 unsigned tempsec;		/* temporary sector counter	   */

	 char *buf;			/* I/O buffer			   */

	 int  blockindex;		/* block index in own directory    */
	 BOOL first;			/* flag for first disk 		   */
	 BOOL init;			/* initialization flag		   */
	 char disk;			/* current backup disk		   */
	 char lastdisk;			/* last backup disk		   */
	 BOOL disks[255];		/* occupied backup disks	   */
	 char adate[ 25];		/* actual date & time header	   */
	 BOOL equal ();			/* compare function		   */

  /* list of received input parameters */
  struct {
	   char mode[5], source, dest;
	   int  verify;
       	 } list;	

  struct dph *DPH;			/* disk parameter header     */
  struct dpb *SDPB, *DDPB;		/* disk parameter blocks     */
  char       *SXLT, *DXLT;		/* sector translation tables */

  /* CP/M directory entry */
  struct {
	   char del, name[8], type[3], extent, reserved[2], eof;
	   union {
		   char sno[16];
		   int lno[8];
		 } block;
	 } *entry;
 
  /* information entry */
  struct {
	   char header[25], number;
	   BOOL last;
	   struct dpb SDPB, DDPB;
	   int blockcount, blockno[0];
	 } *ownentry;

  /* structure of the decision table:
     a) states:     0. initial
 		    1. mode encountered
 		    2. source drive encountered
 		    3. destination drive encountered
 		    4. end
     b) activities: 0. NOP
 		    1. save copy mode
 		    2. save source drive
 		    3. save destination drive
 		    4. save option

   	     		   mode drive option */
  static char tab[][3] = {{0x11, 0x22, 0x44},
			  {0x40, 0x22, 0x44},
			  {0x40, 0x33, 0x44},
			  {0x40, 0x40, 0x44}};


  /* initialize control structure */
  list.mode[0] = list.verify = 0;
  list.source  = list.dest   = 0xFF;

  /* initialize control variables */
  state = 0;
  start = 1;

  /* select national character set */
  printf (national);

  /* get current drive */
  drive = bdos(25);

  printf (prmpt1);

  for (;;)
  {
    /* convert arguments to upper case */
    for (i = start; i < argc; i++) for (j = 0; j < strlen(argv[i]); j++)
      argv[i][j] = toupper(argv[i][j]);

    for (i = start; i < argc; i++)
    {		
        			  			   class = 0xFF;
      if (streq(save, argv[i]) || streq(restore, argv[i])) class = 0;
      if (strlen(argv[i]) == 1 && *argv[i] >= 'A' && *argv[i] <= 'P')
        			  			   class = 1;
      if (streq("[V]", argv[i]))  			   class = 2;

      if (class == 0xFF) break;

      /* get table entry */
      val = tab[state][class]; 

      /* working entry */
      work = val & 7;

      switch (work)
      {
        case 1: strcpy (list.mode, argv[i]);
	        break;
        case 2: list.source = *argv[i] - 'A';
	        break;
        case 3: list.dest   = *argv[i] - 'A';
	        break;
        case 4: list.verify = strchr(argv[i], 'V');
      }
      
      if ((state = val >> 4) == 4) break;
    }

    argc  = 1;
    start = state = 0;
  
    if (list.mode[0] == '\0')
    {
      puts (prmpt2);	
      gets (argv[0] = s);	
      continue;
    }
            
    if (list.source == 0xFF)
    {
      printf (prmpt3);
      gets   (argv[0] = s);
      continue;
    }
    else if (DPH = bios(SELDSK, list.source, 0))
         {
	   /* get source sector translation table */
	   SXLT = DPH->XLT;

	   /* get disk parameter block */
	   SDPB = DPH->DPB;
	 }
	 else
	 {
           printf (error2);
           xabort (2, drive);
         }

    if (list.dest == 0xFF)
    {
      bios   (SELDSK, drive, 0);
      printf (prmpt4);
      gets   (argv[0] = s);
      state   = 2;
      continue;
    }
    else
      if (list.dest != list.source && (DPH = bios(SELDSK, list.dest, 0)))
      {
	/* get sector translation table */
	DXLT = DPH->XLT;

	/* get disk parameter block */
	DDPB = DPH->DPB;
	bios   (HOME);
      }
      else	
      {
        printf (error3);
        xabort (2, drive);
      }

    break;   
  }

  /* calc block size of source & destination disk */
  sblocksiz = (SDPB->BLM + 1) * REC_SIZE;
  dblocksiz = (DDPB->BLM + 1) * REC_SIZE;

  /* calc sector size of source & destination disk */
  ssecsiz = (SDPB->PHM + 1) * REC_SIZE;
  dsecsiz = (DDPB->PHM + 1) * REC_SIZE;

  printf (prmpt5);
  bios   (SELDSK, drive, 0);

  if (list.mode[0] == save[0])	/* save */
  {
    printf ("%s%c%s", prmpt6, list.source + 'A', prmpt7);
    gets   (s);

    /* calc block count of source directory */
    dirsize = 0;
    temp    = SDPB->AL0;
    for (i = 0; i < 8; i++, temp <<= 1) if (temp) dirsize++; else break;
    temp = SDPB->AL1;
    for (i = 0; i < 8; i++, temp <<= 1) if (temp) dirsize++; else break;

    /* calc source block count on destination disk */
    count = ((DDPB->DSM + 1) * (DDPB->BLM + 1) - DDPB->SPT) / (SDPB->BLM + 1);

    /* get buffer for own directory and for table of allocated blocks */
    if (!(ownentry = malloc(sizeof *ownentry + count * sizeof(int))) ||
	!(blocks   = malloc(SDPB->DSM + 1	     		 )))
    {
      printf (error4);
      xabort (2, drive);
    }
   
    /* get buffer for n blocks */
    n = 0;			       /* safety */	
    while (buf = malloc(sblocksiz * ++n + 0x400)) free (buf);
    if (!--n || !(buf = malloc(sblocksiz * n)))
    {
      printf (error4);
      xabort (2, drive);
    }

    /* allocate directory blocks */
    for (i = 0	    ; i <  dirsize  ; i++) blocks[i] = TRUE;
    for (i = dirsize; i <= SDPB->DSM; i++) blocks[i] = FALSE;
    lastblock = dirsize - 1;

    /* read directory of source disk */
    for (block = 0; block < dirsize;)
    {
      for (i = 0; block < dirsize && i < n; block++, i++)
        if (blockio(list.source, &buf[i * sblocksiz], READ)) xabort (2, drive);
     	
      /* detect unallocated blocks */
      entry_count = i * sblocksiz / sizeof *entry;
      for (i = 0, entry = buf; i < entry_count; i++)
        if (entry[i].del < 32)
          if (SDPB->DSM > 255)
            for (j = 0; j < 8; j++)
	    {
 	      if (no = entry[i].block.lno[j]) blocks[no] = TRUE;	
	      lastblock = max(lastblock, no);
	    }
          else 
            for (j = 0; j < 16; j++) 
	    {
	      if (no = entry[i].block.sno[j]) blocks[no] = TRUE;
	      lastblock = max(lastblock, no);
	    }
    }

    /* calc number of destination sectors per source block */
    secsperblock = (SDPB->BLM + 1) / (DDPB->PHM + 1);
		
    /* build header */
    strcpy ( ownentry->header    , "BACKUP "  );
    strcpy (&ownentry->header[ 7], cpm3_date());
    strcpy (&ownentry->header[16], cpm3_time());
    ownentry->header[15] = ' ';

    /* copy disk parameter blocks */
    memcpy (&ownentry->SDPB, SDPB, sizeof(struct dpb));
    memcpy (&ownentry->DDPB, DDPB, sizeof(struct dpb));

    /* reset block indices and disk counter */
    firstblock     = block  = blockindex = disk = ownentry->blockcount = 0;
    firstsec       = sector = (DDPB->OFF + 1) * DDPB->SPT / (DDPB->PHM + 1);
    ownentry->last = FALSE;

    /* copy loop */
    printf ("\n");
    while (block <= lastblock)
    {
      printf ("\r%s", prmt12);

      for (i = 0; i < n; i++)
      {
	if (blocks[block])
	{
          printf  ("\b\b\b\b%04d", block);
          blockio (list.source, &buf[i * sblocksiz], READ);
	}
	else i--;

	if (++block > lastblock)
	{
	  n = ++i;
	  break;
	}
      }

      firsti = 0;
      block  = firstblock;
      next_blocks:
      printf ("\r%s", prmt13);

      for (i = firsti, tempsec = 0; i < n; block++)
	if (blocks[block])
	{
	  if (ownentry->blockcount < count)
	  {
	    printf ("\b\b\b\b%04d", ownentry->blockno[blockindex++] = block);
	    diskio (list.dest, sector + tempsec, secsperblock, 
		    &buf[i * sblocksiz], WRITE);
	  }
	  else break;

	  i++;
	  ownentry->blockcount++;
	  tempsec += secsperblock;	  
	}

      m = i;

      if (list.verify)
      {
	block = firstblock;
	printf ("\r%s", prmt14);

	for (i = firsti, tempsec = 0; i < m; block++)
	  if (blocks[block])
	  {
	    printf ("\b\b\b\b%04d", block);
	    diskio (list.dest, sector + tempsec, secsperblock, 0, VERIFY);

	    i++;	
	    tempsec += secsperblock;
	  }
      }	

      if (ownentry->blockcount >= count)
      {
	/* write own directory */
	ownentry->number = disk++;
			 write_dir  (list.dest, ownentry);
	if (list.verify) verify_dir (list.dest          );
 
	for (;;)
	{
	  /* prompt for next disk */
	  printf  ("%s%c", prmt10, list.dest + 'A');
	  printf  ("%s"  , prmt11);
	  getchar ();
	  printf  ("\n");

	  /* check for previous */
	      bios (HOME  ,  			0);
	      bios (SETTRK, DDPB->OFF		 );	
	      bios (SETSEC, bios(SECTRN, 0, DXLT));
	      bios (SETDMA, buf		 	 );
	  if (bios (READ  ,        		0)) continue;
	  if (streq(ownentry->header, buf)) alarm (); else break;
	}

	/* reset counters */
	blockindex = ownentry->blockcount = 0;
	sector     = firstsec;
	firstblock = block;
	printf ("\n");

	/* remaining blocks ? */
	if (n - m)
	{
	  firsti = m;
	  goto next_blocks;
	}
	else continue;
      }

      firstblock = block;
      sector    += secsperblock * (n - firsti);
    }

    /* write own directory */
    ownentry->number = disk;
    ownentry->last   = TRUE;
		     write_dir  (list.dest, ownentry);
    if (list.verify) verify_dir (list.dest          );
  }	
  else
  {
    printf ("%s%c%s", prmpt8, list.dest + 'A', prmpt9);
    gets   (s);

    /* buffer for own directory */
    if (!(ownentry = malloc(SDPB->SPT * REC_SIZE)))
    {
      printf (error4);
      xabort (2, drive);
    }
   
    /* get buffer for n blocks */
    n = 0;			       /* safety */	
    while (buf = malloc(dblocksiz * ++n + 0x400)) free (buf);
    if (!--n || !(buf = malloc(dblocksiz * n)))
    {
      printf (error4);
      xabort (2, drive);
    }

    /* calc number of source sectors per destination block */
    secsperblock = (DDPB->BLM + 1) / (SDPB->PHM + 1);
		
    /* reset disk remarkers */
    first    = init = TRUE;
    lastdisk = -1;
    for (i = 0; i < 255; i++) disks[i] = FALSE; 

    /* copy loop */
    for (;;)
    {
      for (;;)
      {
        /* insert prompt */
	if (!init)
	{
	  printf  ("%s%c", prmt10, list.source + 'A');
	  printf  ("%s"  , prmt11);
	  getchar ();
	  printf  ("\n");
	}
	
	init = FALSE;

	/* read own directory */
	read_dir (list.source, ownentry);

	if (first)
	{
	  /* check header & disk parameter blocks */
	  if (streq(ownentry->header, "BACKUP")   	       &&
	      equal(&ownentry->SDPB, DDPB, sizeof(struct dpb)) &&
	      equal(&ownentry->DDPB, SDPB, sizeof(struct dpb)))	
	  {
	    strcpy (adate, ownentry->header);
	    first = FALSE;
	    break;
	  }
	}
	else if (streq(ownentry->header, adate)) if (!disks[ownentry->number])
						   break;

	alarm ();
      }		 
 	
      /* remark disk number */ 	
      disks[ownentry->number] = TRUE;
      if (ownentry->last) lastdisk = ownentry->number;

      /* reset counters */
      sector     = (SDPB->OFF + 1) * SDPB->SPT / (SDPB->PHM + 1);
      firstblock = no = 0;

      printf ("\n");

      while (no < ownentry->blockcount)
      {
        printf ("\r%s", prmt12);

	for (i = 0; i < n; i++, sector += secsperblock)
          if (no < ownentry->blockcount)
	  {
	    printf ("\b\b\b\b%04d", ownentry->blockno[no++]);
	    diskio (list.source, sector, secsperblock, &buf[i * dblocksiz],
		    READ);
	  }
	  else break;

	m  = i;
	no = firstblock;

	printf ("\r%s", prmt13);

	for (i = 0; i < m; i++)
	{
	  printf ("\b\b\b\b%04d", block = ownentry->blockno[no++]);
          blockio (list.dest, &buf[i * dblocksiz], WRITE);
        }
		
	if (list.verify)
	{
	  no = firstblock;

	  printf ("\r%s", prmt14);

	  for (i = 0; i < m; i++)
	  {
	    printf  ("\b\b\b\b%04d", block = ownentry->blockno[no++]);
	    blockio (list.dest, 0, VERIFY);
	  }
	}

	firstblock = no;
      }

      /* done ? */
      if (lastdisk != 0xFF)
      {	
	for (i = 0; i <= lastdisk; i++) if (!disks[i]) goto endfor;
	bios (FLUSH);			/* flush buffers */
        break;
      }
      endfor:;
    }  
  } 
  /* select current drive */
  bios   (SELDSK, drive, 0);
  printf ("\n");
}


static BOOL equal (buf1, buf2, n)
  char *buf1, *buf2;
  int n;
{
  int i;

  for (i = 0; i < n; i++) if (buf1[i] != buf2[i]) return FALSE;
						  return TRUE;
}


static read_dir (disk, buffer)
  char disk, *buffer;
{
  unsigned sector;			/* current sector 	    */
  int secspertrk;			/* sectors per track	    */
  int secsiz;				/* sector size		    */
  struct dph *DPH;			/* disk parameter header    */
  struct dpb *DPB;			/* disk parameter block     */
  char   *XLT;				/* sector translation table */

  DPH 	     = bios(SELDSK, disk, 1);
  XLT 	     = DPH->XLT;
  DPB 	     = DPH->DPB;
  secspertrk = DPB->SPT / (DPB->PHM + 1);
  secsiz     = (DPB->PHM + 1) * REC_SIZE;
    
  bios (HOME  ,          0);
  bios (SETTRK, DPB->OFF  );
  bios (MULTIO, secspertrk);

  for (sector = 0; sector < secspertrk; sector++)
  {
        bios (SETSEC, bios(SECTRN, sector, XLT));
        bios (SETDMA, &buffer[sector * secsiz] );
    if (bios (READ  ,                         0)) xabort (2, drive);
  }
}
 	    

static write_dir (disk, buffer)
  char disk, *buffer;
{
  unsigned sector;			/* current sector 	    */
  int secspertrk;			/* sectors per track	    */
  int secsiz;				/* sector size		    */
  struct dph *DPH;			/* disk parameter header    */
  struct dpb *DPB;			/* disk parameter block     */
  char *XLT;				/* sector translation table */

  DPH 	     = bios(SELDSK, disk, 1);
  XLT 	     = DPH->XLT;
  DPB 	     = DPH->DPB;
  secspertrk = DPB->SPT / (DPB->PHM + 1);
  secsiz     = (DPB->PHM + 1) * REC_SIZE;
    
  bios (HOME  ,          0);
  bios (SETTRK, DPB->OFF  );
  bios (MULTIO, secspertrk);

  for (sector = 0; sector < secspertrk; sector++)
  {
        bios (SETSEC, bios(SECTRN, sector, XLT));
        bios (SETDMA, &buffer[sector * secsiz] );
    if (bios (WRITE ,                     WRUAL)) xabort (2, drive);
  }
  if (bios(FLUSH)) xabort (2, drive);
}
 	    

static verify_dir (disk)
  char disk;
{
  unsigned sector;			/* current sector 	    */
  int  secspertrk;			/* sectors per track	    */
  char dummy[1024];			
  struct dph *DPH;			/* disk parameter header    */
  struct dpb *DPB;			/* disk parameter block     */
  char *XLT;				/* sector translation table */

  DPH 	     = bios(SELDSK, disk, 1);
  XLT 	     = DPH->XLT;
  DPB 	     = DPH->DPB;
  secspertrk = DPB->SPT / (DPB->PHM + 1);
    
  bios (HOME  ,        0);
  bios (SETTRK, DPB->OFF);
  bios (SETDMA, dummy   );

  for (sector = 0; sector < secspertrk; sector++)
  {
        bios (SETSEC, bios(SECTRN, sector, XLT));
    if (bios (READ  ,                         0)) xabort (2, drive);
  }
}
 	    

static BOOL blockio (drive, buffer, mode)
  char drive, *buffer, mode;
{
  int secsperblock;			/* sectors per block	 */
  struct dph *DPH;			/* disk parameter header */
  struct dpb *DPB;			/* disk parameter block  */

  DPH 	       = bios(SELDSK, drive, 1);
  DPB 	       = DPH->DPB;
  secsperblock = (DPB->BLM + 1) / (DPB->PHM + 1);
    
  /* transfer sectors */
  return diskio (drive, 
		 DPB->SPT / (DPB->PHM + 1) * DPB->OFF + block * secsperblock,
	  	 secsperblock, buffer, mode);
}


static BOOL diskio (disk, sector, count, buffer, mode)
  char disk, *buffer, mode;
  unsigned sector, count;
{
  BOOL errstat = FALSE;			/* error status		    */
  int  secspertrk;  			/* sectors per track	    */
  int  secsiz;				/* sector size		    */
  unsigned lastsec;			/* last sector 		    */
  char dummy[1024];
  char c;				/* keyboard character	    */
  char mode1;				/* I/O mode		    */
  struct dph *DPH;			/* disk parameter header    */
  struct dpb *DPB;			/* disk parameter block     */
  char *XLT;				/* sector translation table */

  DPH 	     = bios(SELDSK, disk, 1);
  XLT        = DPH->XLT;
  DPB 	     = DPH->DPB;
  secsiz     = (DPB->PHM + 1) * REC_SIZE;
  secspertrk = DPB->SPT / (DPB->PHM + 1);
  lastsec    = sector + count;	

  if (mode == VERIFY)
  {
    buffer = dummy;
    mode1  = READ;
  }
  else
  {
    mode1 = mode;
    bios (MULTIO, count);
  }     

  for (; sector < lastsec; sector++)
  {
    bios (SETTRK, 	       sector / secspertrk      );
    bios (SETSEC, bios(SECTRN, sector % secspertrk, XLT));
    bios (SETDMA, buffer                                );
    switch (bios(mode1, WRUAL))
    {
      /* no errors */
      case 0 : break;

      /* diskette write protected */
      case 2 : printf (error6);
	       goto error;

      /* fatal I/O errors */
      default: printf (error5);
	       
	       error:
	       printf (prmt15);
	       do
	       {
		 printf ("\b");
		 c = toupper(getchar());
	       }
	       while (c != cancel && c != proceed && c != retry);

	       if (c == cancel) xabort (2, drive);
	       errstat = TRUE;	
	       switch (mode)
	       {
		 case READ  : printf ("\n\n%s", prmt12);
			      break;
		 case WRITE : printf ("\n\n%s", prmt13);
			      break;
		 case VERIFY: printf ("\n\n%s", prmt14);
	       }
	       if (c == retry)
	       {
		 printf ("\b\b\b\b%04d", block);
		 sector--;
		 continue;
	       }
    }
    if (mode != VERIFY) buffer += secsiz;
  }
  return errstat;
}


static alarm ()
{
  int i;

  printf (error7);
  for (i = 0; i < 3; i++)
  {
    printf ("\7");		/* sound bell    */
    sleep  (2);			/* pause 0.2 sec */
  }
}


static xabort (mode, drive)
  char mode, drive;
{
  bios   (SELDSK, drive, 0);
  printf ("\n");
  _exit  (mode);
}
