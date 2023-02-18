/******************************************************************************
*  I N I T W  *  C P M S Y S 5  *  T h o m a s   H o l t e  *   8 5 1 1 2 6   *
*******************************************************************************
* 									      *
*    U T I L I T Y   F O R   G E N E R A T I N G   A   G E N I E   I I I s    *
*    =====================================================================    *
* 									      *
*  	   C P / M   V e r . 3 . 0   H A R D   D I S K   S Y S T E M	      *
*          =========================================================          *
* 									      *
* 									      *
*   Thomas Holte			                         Version 2.0  *
*   herumgeschnitzt am 09.12.92 und auf TM502 eingestellt hat E.Schroeer      *
*******************************************************************************
* beim Original von Holte waren folgende bugs zu finden:                      *
* _main --> auf main gesetzt (siehe auch PCCOPY)                              *
* movmem -->  bringt die copyright message auf die Festplatte                 *
*             diese Routine wurde analog der Routine memcpy in format.c       *
*             ausgelegt. Fehler nicht beseitigt -> Routine geloescht          *
*             Programm nicht lauff{hig (mit LIBCAP gelinkt)                   *
*             in LIBCAP ist die Routine _main enthalten in CLIB anscheinend   *
*             die Routinen movemem und setmem 25.12.92                        *
* setmem  --> wurde analog format.c in memset umbenannt und umgeschrieben     *
*             keine Wirkung.                                                  *
* movmem und setmem aus CLIB extrahiert und mit LIBCAP zu NEWLIBC verbunden   *
* 26.12.92/ fehlerfreies LINKING. Program nicht lauff{hig. Als Test mit       *
* Originalversion (TM252.H etc.) compiliert -> nicht lauff{hig. Wie sieht     *
* die Version 1.0 des INITW.C aus ?                                           *
* Routine bios.h eingef}gt: mu~ meiner Meinung nach vorhanden sein ?          *
* am 25.12.92 wieder entfernt                                                 *
*******************************************************************************
* E.Schroeer  11.12.92                                                        *
******************************************************************************/

#define SASI		/* must be defined if Xebec controller used */

#include <stdio.h>

#ifdef SASI

/* port addresses of Xebec SASI Controller */
#define WPORT0 0x00
#define WPORT1 0x01
#define WPORT2 0x02
#define RPORT0 0x00
#define RPORT1 0x01

/* commands of Xebec SASI Controller */
#define TEST   0x00		/* test drive ready 		    */
#define REST   0x01		/* recalibrate	    		    */
#define FORMAT 0x04		/* format drive	    		    */
#define INIDRV 0x0C		/* initialize drive characteristics */

#else

/* port addresses of Western Digital Winchester Disk Controller */
#define DATA      0x50		/* data register    */
#define ERR       0x51		/* error register   */
#define WPC       0x51		/* write precomp    */
#define SECNT     0x52		/* sector count     */
#define CYLLO     0x54		/* cylinder low     */
#define CYLHI     0x55		/* cylinder high    */
#define SDH       0x56		/* size/drive/head  */
#define COMND     0x57		/* command register */
#define STATUS    0x57		/* status register  */

/* commands of Western Digital Winchester Disk Controller */
#define REST	  0x10		/* restore	     */
#define FORMAT    0x50		/* format track      */

#endif

/* language dependent variables (globals) */
extern char drive[], error7[], type0[], type1[], type2[], type3[];

   #include <tm502.h>
/* #include <tm252.h> */

_main ()
{
  /* language dependent variables */
  extern char cancel, error1[], error2[], error3[], error4[], error5[],
	      error6[], national[], prmpt1[], prmpt2[], prmpt3[], prmt31[],
	      prmpt4[], prmpt5[], prmpt6[], prmpt7[], prmpt8[], proceed;

  	 char secno;			/* current sector number	   */
  	 char surface;			/* current surface number	   */
  	 int  cylinder;			/* current cylinder number	   */
  	 int  track;			/* current track number		   */
  	 int  i, j;			/* loop counters		   */
	 char c;			/* temporary char storage	   */
  	 char buf1[SECLEN];		/* contains formatting data	   */
  	 char buf2[SECLEN];		/* contains worst case bit pattern */
  	 char buf3[SECLEN];		/* buffer for spare directory      */
  static char buf4[SECLEN] = 		/* copyright message		   */
    "MM/DD/YY  HH:MM:SS  formatted by INITW, written by Thomas Holte (c) 1985";

  struct {
	   unsigned track;		/* bad track 			   */
	   char     sector;		/* bad sector			   */
	 } *sd = buf3;			/* pointer to spare directory	   */

  	 int  sdc = 0;			/* bad sector counter 		   */
	
         FILE *CPM3_SYS;		/* file containing complete CP/M 3
				           system			   */ 
  	 char print[REC_SIZE];		/* CPM3.SYS print record	   */
  	 char *CPM3;			/* pointer to system buffer	   */
  	 char *tCPM3;			/* temporary pointer		   */
  	 int  size;			/* size of CPM3.SYS		   */
	 char *search ();		/* search a byte string 	   */

  /* disk parameter block */
  static struct {
	     	  int  SPT;
	     	  char BSH, BLM, EXM;
	     	  int  DSM, DRM;
	     	  char AL0, AL1;
	     	  int  CKS, OFF;
	     	  char PSH, PHM;
	        } DPB = DPBA, *pDPB;

  /* CPM3.SYS header record */
  struct header {
	   	  char res_top_page;	/* top page plus one, at which the
				           resident portion of CP/M 3 is to
				           be loaded top down 	 	    */
	   	  char res_length;	/* length in pages of the resident
				      	   portion of CP/M 3		    */
	   	  char bnk_top_page;    /* top page plus one, at which the
				           banked portion of CP/M 3 is to
				           be loaded top down		    */
	   	  char bnk_length;	/* length in pages of the banked 
				           portion of CP/M 3		    */
	   	  char *entry;		/* address of CP/M 3 cold boot
				      	   entry point			    */
	   	  char filler[122];	
	        } HEADER;


  /* deselect error return mode */
  bdos (45, 0);

  /* select national character set */
  printf (national);

  /* get time & date */
  movmem (cpm3_date(),  buf4    , 8);
  movmem (cpm3_time(), &buf4[10], 8);

  printf      (prmpt1);
  disp_config ();

  /* restore drive */
  restore (); 

  /* try to read second sector */
  if (!system(16, 0x10, 1, buf2, 0) && streq(&buf2[20], &buf4[20]))
  {
    printf (prmpt2);
    do
    {
      printf ("\b");
      c = toupper(getchar());
    }
    while (c != cancel && c != proceed);
    printf ("\n");
    if (c == cancel) _exit (1);
  }

  /* initialize buffers */
  for (i = 0; i < SECCOUNT * 2; i++)
  {
    buf1[i++] = 0x00;
    buf1[i  ] = 0xFF;
  }
  for (i = 0; i < SECLEN; i += 3) buf2[i] = 0x6D;
  for (i = 1; i < SECLEN; i += 3) buf2[i] = 0xB6;
  for (i = 2; i < SECLEN; i += 3) buf2[i] = 0xDB;

  setmem (buf3, SECLEN, 0xFF);

  /* number sectors */
  for (i = secno = 0; i < SECCOUNT; i++)
  {
    j = 1 + 2 * secno;
    if (buf1[j] != 0xFF)
    {
      secno++;
      i--;
    }
    else
    {
      buf1[j] = i;
      if ((secno += SKEW) >= SECCOUNT) secno -= SECCOUNT;
    }
  }

  /* formatting */
  printf (prmpt3);

#ifdef SASI

  format_drive ();

#else

  printf (prmt31);

  for (cylinder = 0; cylinder < TRKCOUNT / HEADCOUNT; cylinder++)
    for (surface = 0; surface < HEADCOUNT; surface++)
    {
      printf ("\b\b\b\b%04d", track = cylinder * HEADCOUNT + surface);

      format_track (surface, cylinder, buf1);

      /* write worst case bit pattern */	
      for (secno = 0; secno < SECCOUNT; secno++) 
	system (17, 0x10, secno, buf2, track);
    } 	  
	  
#endif

  /* verifying */
  printf (prmpt4);

  for (track = 0; track < TRKCOUNT; track++)
  {
    printf ("\b\b\b\b%04d", track);

    for (secno = 0; secno < SECCOUNT; secno++)
      if (system(16, 0x10, secno, buf2, track))
	if (track && sdc < 255 && sdc < SECLEN / sizeof *sd)	  
	{
	  sd[sdc].track  = track;
	  sd[sdc].sector = secno;
	  printf ("\n%d %s", ++sdc , prmpt5);
	  printf ("%s %04d", prmpt6, track );
	}
	else fatal_error ();
  } 

  /* write spare directory */
  if (system(17, 0x10, 0, buf3, 0) || system(17, 0x10, 1, buf4, 0))
    fatal_error ();

  /* initialize directory */
  setmem (buf2, SECLEN, 0xE5);

  printf (prmpt7);
   
  for (i = SECCOUNT; i < DIRSIZE / 4 / (SECLEN / REC_SIZE) + SECCOUNT; i++)
  {
    track = i / SECCOUNT;
    secno = i % SECCOUNT;

    /* calc offset */
    for (j = 0;; j++) if (sd[j].track  > track) break; 
      		 else if (sd[j].track == track) if (sd[j].sector > secno)
						  break;

    track = (i + j) / SECCOUNT;
    secno = (i + j) % SECCOUNT;

    if (system(17, 0x10, secno, buf2, track)) fatal_error ();
  }


  /* read system */
  printf (prmpt8);
  if ((CPM3_SYS = open("CPM3HD.SYS", 0)) == ERROR)
  {
    printf (error1);
    _exit  (2);
  }

  /* read header record and print record */
  read (CPM3_SYS, &HEADER, REC_SIZE);
  read (CPM3_SYS, print  , REC_SIZE);
  
  /* get memory for CPM3.SYS data */
  if (!(CPM3 = malloc(size = (HEADER.res_length + HEADER.bnk_length) * 0x100)))
  {
    printf (error2);
    _exit  (2);
  }

  /* read system in reversed order */
  for (tCPM3 = CPM3 + size - REC_SIZE; tCPM3 >= CPM3; tCPM3 -= REC_SIZE)
    read (CPM3_SYS, tCPM3, REC_SIZE);

  close (CPM3_SYS);

  /* search disk parameter block of drive A: */
  if (!(pDPB = search(&DPB, CPM3, sizeof *pDPB, size)))
  {
    printf (error3);
    _exit  (2);
  }

  /* set maximum block count */
  pDPB->DSM = (long)(SECCOUNT * (TRKCOUNT - pDPB->OFF) - sdc) 
	    * (SECLEN / REC_SIZE) / (pDPB->BLM + 1) - 1;

  /* write system */
  if ((CPM3_SYS = creat("C:CPM3.SYS", 1)) == ERROR)
  {
    printf (error4);
    _exit  (2);
  }

  /* write header record and print record */
  write (CPM3_SYS, &HEADER, REC_SIZE);
  write (CPM3_SYS, print  , REC_SIZE);
  
  /* write system in reversed order */
  for (tCPM3 = CPM3 + size - REC_SIZE; tCPM3 >= CPM3; tCPM3 -= REC_SIZE)
    write (CPM3_SYS, tCPM3, REC_SIZE);

  close (CPM3_SYS);


  /* copy CCP.COM */
  if ((CPM3_SYS = open("CCP.COM", 0)) == ERROR)
  {
    printf (error5);
    _exit  (2);
  }
  i = read(CPM3_SYS, CPM3, 0x1000);
  close (CPM3_SYS);

  if ((CPM3_SYS = creat("C:CCP.COM", 0)) == ERROR)
  {
    printf (error6);
    _exit  (2);
  }
  write (CPM3_SYS, CPM3, i);
  close (CPM3_SYS);
}

 
static restore ()
{

#ifdef SASI

  int  i;				 /* loop counter		     */
  char status;				 /* SASI controller status	     */
  char getstat ();			 /* read status from controller	     */

  static struct {
		  unsigned cylinders;	 /* maximum number of cylinders	     */
		  char     heads;	 /* maximum number of heads	     */
		  unsigned rwc_cylinder; /* starting red. write current cyl. */
		  unsigned wpc_cylinder; /* starting write precomp. cylinder */
		  char     ECC_length;   /* maximum ECC data burst length    */
		} drive_characteristics =
  		  {(TRKCOUNT / HEADCOUNT << 8) + (TRKCOUNT / HEADCOUNT >> 8),
		    HEADCOUNT,
  		   (TRKCOUNT / HEADCOUNT << 8) + (TRKCOUNT / HEADCOUNT >> 8),
		   (WPCOMP_CYL << 8) + (WPCOMP_CYL >> 8), 11};

  outp (WPORT1, 0);				   /* reset controller    */

  /* initialize drive data */
  uptask (INIDRV);
  while (!(inp(RPORT1) & 1));
  for (i = 0; (status = inp(RPORT1) & 0x19) != 0x19;)
  {
    if (status != 1) continue;
    outp (WPORT0, ((char *)&drive_characteristics)[i++]);
  }
  getstat ();
  
  /* test drive ready */
  do uptask (TEST); while (getstat());

  /* restore drive */
  uptask  (REST);
  getstat ();

#else

  uptask (0    ,                    0);
  outp   (COMND, REST + STEP_RATE * 2);

  while (inp(STATUS) & 0x80);

#endif

}


#ifdef SASI

static format_drive ()
{
  uptask  (FORMAT);
  getstat ();
}

#else

static format_track (surface, cylinder, buffer)
  char surface, *buffer;
  int cylinder;
{
  int i;

  uptask (surface, cylinder);
  outp   (SECNT  , SECCOUNT);
  outp   (COMND  , FORMAT  );

  for (i = 0; i < SECLEN; i++) outp (DATA, *buffer++);

  while (inp(STATUS) & 0x80);
}

#endif

#ifdef SASI

static uptask (cmd)
  char cmd;
{
  int i;			/* loop counter */

  static struct {
	   	  char     opcode;
	   	  char     lun;
	   	  unsigned block;
		  char     interleave;
		  char     control_field;
		} command = {0, 0, 0, SKEW, 
#if STEP_RATE
					    0 	/* 3msec step rate */
#else
					    5	/* buffered step   */
#endif
					     };

  /* select controller */
  while (inp(RPORT1) & 2);
  outp (WPORT0, 1);
  do outp (WPORT2, 0); while (!(inp(RPORT1) & 2));
 
  /* output command */
  command.opcode = cmd;
  while (!(inp(RPORT1) & 1));
  for (i = 0; i < sizeof command; i++) outp (WPORT0, ((char *)&command)[i]);  
}

#else
  
static uptask (surface, cylinder)
  char surface;
  int cylinder;
{
  outp (WPC  , WPCOMP_CYL / 4          );
  outp (CYLHI, cylinder >> 8           );
  outp (CYLLO, cylinder                );
  outp (SDH  , 0x80 + SECSIZE + surface);
}

#endif

#ifdef SASI

static char getstat ()
{
  char error;			/* error byte from controller */

  /* get error status */
  while (!(inp(RPORT1) & 1));
  error = inp(RPORT0);
  while (!(inp(RPORT1) & 1));
  inp (RPORT0);  
  
  return error;
}

#endif


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


static fatal_error ()
{
  printf (error7);
  _exit  (2);
}
