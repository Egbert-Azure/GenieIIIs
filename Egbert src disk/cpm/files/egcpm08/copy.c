/******************************************************************************
*  C O P Y  *  U T I L S 0 0 4  *  T h o m a s   H o l t e  *   8 5 0 9 2 3   *
*******************************************************************************
* 									      *
* 	   B A C K U P   U T I L I T Y   F O R   C P / M - B A S E D	      *
*          =========================================================          *
* 									      *
*		   M I C R O C O M P U T E R   S Y S T E M S 		      *
*                  =========================================                  *
* 									      *
* 									      *
*   Thomas Holte			                         Version 3.2  *
* 									      *
******************************************************************************/

#include <stdio.h>
#include <bios.h>

#define ETX 3

#define VERIFY    17

#define WRDIR      1	/* write to directory   	   */
#define WRUAL      2	/* write to unallocated 	   */

#define SEC_SIZE 512	/* Genie IIIs standard sector size */

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

/* disk parameter block */
static struct dpb *DPB;

static char *XLT;			/* sector translation table */
static char drive;			/* current drive 	    */
static char lasttrk;			/* last track    	    */
static char track;			/* current track 	    */ 
static int  lastsec;			/* last sector   	    */
static int  nullcopy;			/* special copy marker	    */

/* language dependent variables (globals) */
extern char cancel, error6[], error7[], prmt11[], prmt12[], prmt14[], proceed,
	    retry;


main (argc, argv)
  int argc;
  char *argv[];
{
  /* language dependent variables */
  extern char all[], boot[], end[], error2[], error3[], error4[], error5[],
	      files[], national[], prmpt1[], prmpt2[], prmpt3[], prmpt4[],
	      prmpt5[], prmpt6[], prmpt7[], prmpt8[], prmpt9[], prmt10[],
	      prmt13[], yes;

  	 char class;			/* character class		 */
  	 char state;			/* state of decision table	 */
	 char val;			/* work value of decision table  */
  	 char work;			/* current task			 */
  	 char s[80];			/* console input buffer		 */
  	 char *dmaadr;			/* current DMA address		 */
  	 int  trksiz;			/* track length (bytes)		 */
  	 int  first_of_all;		/* first track (incl sys trks)   */
  	 char firsttrk;			/* first track			 */
  	 int  firstsec;			/* first sector			 */
  	 char sector;			/* current sector		 */	
  	 char *tbuf;			/* track buffer			 */
  	 char tracks[160];		/* allocated tracks		 */
  	 int  dirsize;			/* directory size		 */
  	 int  i, j, n, no1, no2;	/* temp counters		 */
  	 int  start;			/* control var for "getpar" loop */

  /* list of received input parameters */
  struct {
	   char mode[7], source, dest;
	   int  automatic, verify;
	 } list;

  /* CP/M directory entry */
  struct {
	   char del, name[8], type[3], extent, reserved[2], eof;
	   union {
		   char sno[16];
		   int  lno[8];
		 } block;
	 } *entry;

  /* pointer to disk parameter header */
  struct dph *DPH;

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
		    4. save options

  	     	           mode drive option */
  static char tab[][3] = {{0x11, 0x22, 0x44},
	                  {0x40, 0x22, 0x44},
		          {0x40, 0x33, 0x44},
		          {0x40, 0x40, 0x44}};

  BOOL notequal ();		/* compare two strings */


  /* initialize control structure */
  list.mode[0] = list.automatic = list.verify = 0;
  list.source  = list.dest      	      = 0xFF;

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
      if (streq(all  , argv[i]) || streq(boot, argv[i]) ||
          streq(files, argv[i]) || streq(end , argv[i]))   class = 0;
      if (strlen(argv[i]) == 1 && *argv[i] >= 'A' && *argv[i] <= 'P')
        class = 1;
      if (*argv[i] == '[') class = 2;

      if (class == 0xFF) break;

      /* get table entry */
      val = tab[state][class]; 

      /* working entry */
      work = val & 7;

      switch (work)
      {
        case 1 : strcpy (list.mode, argv[i]);
	         break;
        case 2 : list.source    = *argv[i] - 'A';
	         break;
        case 3 : list.dest      = *argv[i] - 'A';
	         break;
        case 4 : list.automatic = strchr(argv[i], 'A');
		 list.verify    = strchr(argv[i], 'V');
		 nullcopy	= strchr(argv[i], '0');
      }

      if ((state = val >> 4) == 4) break;
    }

    restart:

    argc  = 1;	
    start = state = 0;
  
    if (list.mode[0] == end[0])
    {
      /* select current disk */
      bios (SELDSK, drive, 0);
      return;
    };

    if (list.mode[0] == '\0')
    {
      puts (prmpt2);
      gets (argv[0] = s);
      if (strlen(s) <= 1) argv[0][0] = '\0';
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
	   /* get sector translation table */
	   XLT = DPH->XLT;

	   /* get disk parameter block */
	   DPB = DPH->DPB;
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
      state = 2;
      continue;
    }
    else
      if (!(DPH = bios(SELDSK, list.dest, 0)))
      {
        printf (error3);
        xabort (2, drive);
      }
      else if (notequal(DPB, DPH->DPB, sizeof(struct dpb)))
	   {
	     printf (error4);
	     xabort (2, drive);
	   }
	   else bios (HOME);

    break;   
  }

  /* big copy loop */
  do
  {
    if (!list.automatic)
    {
      bios   (SELDSK, drive, 0);
      printf (prmpt5);
      printf ("%s%s%c%s%c%s", list.mode, prmpt6, list.source + 'A', prmpt7,
			        		 list.dest   + 'A', prmpt8);	
      gets   (s);
    } 
    printf ("\n");

    /* initialize track numbers */
    firsttrk = 0;
    lasttrk  = DPB->OFF - 1;
    for (track = firsttrk; track <= lasttrk; track++) tracks[track] = TRUE;

    /* read directory of source disk */
    if (list.mode[0] != boot[0])
    {
      /* get directory buffer */
      if (!(dmaadr = entry
		   = malloc(((dirsize = DPB->DRM / 4) + 1) * 128)))
      {
        printf (error5);
        xabort (2, drive);
      }

      lasttrk = (firsttrk = DPB->OFF) + dirsize / DPB->SPT;

      bios (SELDSK, list.source, 0);
      bios (HOME  ,           0, 0);   
      for (track = firsttrk; track <= lasttrk; track++)
      {
        bios(SETTRK, track);
        for (sector = 0; 
	     sector <= dirsize && sector < DPB->SPT / (DPB->PHM + 1); sector++)
        {
              bios (SETSEC, bios(SECTRN, sector, XLT));
              bios (SETDMA, dmaadr		     );
          if (bios (READ  , 			    0)) xabort (2, drive);
          dmaadr += 128 * (DPB->PHM + 1);
        }
        dirsize -= DPB->SPT / (DPB->PHM + 1);
      }

      for (track = firsttrk; track <= lasttrk; track++) tracks[track] = TRUE;
      if (list.mode[0] != files[0]) firsttrk = 0;
      for (track = lasttrk + 1; track < 80   ; track++) tracks[track] = FALSE;

      /* detect unallocated tracks */
      for (i = 0; i <= DPB->DRM; i++)
        if (entry[i].del < 32)
          if (DPB->DSM > 255)
            for (j = 0; j < 8; j++) 
	    {
	      lastsec = (firstsec = entry[i].block.lno[j]
		      * (DPB->BLM + 1)) + DPB->BLM;
	    
	      no1 = firstsec / DPB->SPT + DPB->OFF;
	      no2 = lastsec  / DPB->SPT + DPB->OFF;
	      for (track = no1; track <= no2; track++)
	      {
	        tracks[track] = TRUE;
	        lasttrk       = max(lasttrk, track);
	      }
            }
          else 
            for (j = 0; j < 16; j++) 
	    {
	      lastsec = (firstsec = entry[i].block.sno[j]
		      * (DPB->BLM + 1)) + DPB->BLM;

	      no1 = firstsec / DPB->SPT + DPB->OFF;
	      no2 = lastsec  / DPB->SPT + DPB->OFF;
	      for (track = no1; track <= no2; track++)
	      {
	        tracks[track] = TRUE;
	        lasttrk       = max(lasttrk, track);
	      }
            }

      free (entry);		/* free directory buffer */
    }

    /* get buffer for n tracks */
    trksiz = DPB->SPT * 128;
    n	   = 0;			    /* safety */  	
    while (tbuf = malloc(trksiz * ++n + 0x400)) free (tbuf);
    if (!--n || !(tbuf = malloc(trksiz * n)))
    {
      printf (error5);
      xabort (2, drive);
    }

    /* copying */
    lastsec = DPB->SPT / (DPB->PHM + 1) - 1;
    track   = first_of_all = firsttrk;

    while (track <= lasttrk)
    {
      if (list.source == list.dest && track != first_of_all)
      {
        printf  ("%s%c ", prmpt9, list.source + 'A');
        getchar ();
        printf  ("\n");
      }	

      printf ("\r%s", prmt11);

      for (i = 0; i < n; i++)
      {
	if (tracks[track]) 			
	{
	  printf  ("\b\b\b%03d", track);
	  trackio (list.source, &tbuf[i * trksiz], READ);
	}
	else i--;

	if (++track > lasttrk)
	{
	  n = ++i;
	  break;
	}
      }

      if (list.source == list.dest)
      {
	printf  ("%s%c ", prmt10, list.dest + 'A');
        getchar ();
        printf  ("\n");
      }	
      printf ("\r%s", prmt12);	
      track = firsttrk;

      for (i = 0; i < n; i++, track++)
	if (tracks[track])
	{
	  		   printf  ("\b\b\b%03d", track);
	  		   trackio (list.dest, &tbuf[i * trksiz], WRITE );
	  if (list.verify) trackio (list.dest,  tbuf            , VERIFY);
        }
	else i--;

      firsttrk = track;
    }

    free (tbuf);		/* free track buffer */

    if (list.automatic) 
    {
      /* select current disk */
      bios (SELDSK, drive, 0);
      return;
    }
    printf (prmt13);
  }
  while (toupper(getchar()) == yes);

  /* restart copy program */
  list.mode[0] = '\0';
  list.source  = list.dest = 0xFF;
  printf ("\n");
  goto restart;
}


static BOOL notequal (s1, s2, n)
  char *s1, *s2;
  int n;
{
  register int i;

  for (i = 0; i < n; i++) if (*s1++ != *s2++) return TRUE;
					      return FALSE;
}	


static trackio (disk, buffer, mode)
  char disk, *buffer, mode;
{
  char *buffer1, c, dummy[1024], mode1, sector;

  bios (SELDSK, disk , 1);
  bios (SETTRK, track, 0);
  if (mode == VERIFY)
  {
    buffer1 = dummy;
    mode1  = READ;
  }
  else 
  {
    buffer1 = buffer;
    mode1   = mode;
    bios (MULTIO, lastsec + 1);
  }

  for (sector = 0; sector <= lastsec; sector++)
  {
    bios (SETSEC, bios(SECTRN, sector, XLT));
    bios (SETDMA, buffer1	       	   );    
    switch (bios(mode1, track == lasttrk && sector == lastsec ? WRDIR : WRUAL))
    {
      /* no errors */	
      case 0 : break;

      /* diskette write protected */
      case 2 : printf (error7);	
	       goto error;

      /* fatal I/O errors */
      default: printf (error6);

	       error:
      	       printf (prmt14);
      	       do
      	       {
        	 printf ("\b");
        	 c = toupper(getchar());
      	       } 
      	       while (c != cancel && c != proceed && c != retry);
      	       printf ("\n");

	       if (c    == cancel) xabort (2, drive);
               if (mode == READ  ) printf ("\n%s", prmt11);
		  	      else printf ("\n%s", prmt12);
	       if (c    == retry ) 
	       {	
		 printf ("\b\b\b%03d", track);
		 sector--;
		 continue;
	       }	
    }
    if (mode != VERIFY) buffer1 += 128 * (DPB->PHM + 1);
  }
  if (mode == READ && !track) check_genie (buffer);
}


static check_genie (buffer)
  char *buffer;
{
  int  i;			/* loop counter 	  */
  char *search ();		/* search string function */

  /* encrypted copyright message */
  static char copyright[] = {~'n', ~'E', ~'a', ~'h', ~'c', ~'n', ~'d', ~'e',
			     ~'B', ~' ', ~'O', ~'I', ~' ', ~'S', ~'r', ~'w',
			     ~'t', ~'i', ~'e', ~'t', ~' ', ~'n', ~'y', ~'b',
			     ~'T', ~' ', ~'o', ~'h', ~'a', ~'m', ~' ', ~'s',
			     ~'o', ~'H', ~'t', ~'l', ~' ', ~'e', ~'(', ~' ',
			     ~')', ~'c', ~'1', ~' ', ~'8', ~'9',   ~0, ~'5'};

  /* decode message */
  swab (copyright, copyright, sizeof copyright / 2);
  for (i = 0; i < sizeof copyright; i++) copyright[i] = ~copyright[i];

  /* search message */
  if (search(copyright, buffer, sizeof copyright - 1, SEC_SIZE))

    /* make virgin */
    if (nullcopy) *(int *)(buffer + SEC_SIZE + 0x0036) = 0;
     
    /* get encoded serial number */
    else system (15, 1, 2, 0x0036, buffer + SEC_SIZE + 0x0036);

  /* encode message */
  swab (copyright, copyright, sizeof copyright / 2);
  for (i = 0; i < sizeof copyright; i++) copyright[i] = ~copyright[i];
}


static char *search (pa, pb, na, nb)
  char *pa, *pb;
  unsigned na, nb;
{
  char *a, *b;
  unsigned i;

  while (nb--)
  {
    if (*pa == *pb++)
    {
      a = pa;
      b = pb;
      for (i = 1; i < na; i++) if (*++a != *b++) goto endwhile;
      return --pb;
    }	
    endwhile:;
  }
  return NULL;
}


static xabort (mode, drive)
  char mode, drive;
{
  bios  (SELDSK, drive, 0);
  _exit (mode);
}
