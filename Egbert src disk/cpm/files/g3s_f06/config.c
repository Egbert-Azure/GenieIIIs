/******************************************************************************
*  C O N F I G  *  U T I L S 0 0 3  *  T h o m a s   H o l t e  * 8 6 0 9 1 1 *
*******************************************************************************
*									      *
* S Y S T E M   C O N F I G U R A T O R   F O R   T H E   G E N I E   I I I s *
* =========================================================================== *
*									      *
*  		    M I C R O C O M P U T E R   S Y S T E M		      *
*		    =======================================		      *
*									      *
*									      *
*  Version 1.1							Thomas Holte  *
*									      *
******************************************************************************/

#include <stdio.h>
#include <bios.h>
#include <errno.h>

/* ASCII control codes */
#define NUL	     0x00	/* null			     */	
#define SOH	     0x01	/* start of heading	     */
#define STX	     0x02	/* start of text	     */
#define ETX	     0x03	/* end of text		     */
#define EOT	     0x04	/* end of transmission	     */
#define ENQ          0x05	/* enquiry		     */
#define ACK	     0x06	/* acknowledge		     */
#define BEL	     0x07	/* bell			     */
#define HT	     0x09	/* horizontal tabulation     */ 	
#define LF	     0x0A	/* line feed		     */	
#define VT	     0x0B	/* vertical tabulation	     */
#define CR	     0x0D	/* carriage return	     */
#define SO	     0x0E	/* shift out		     */
#define SI	     0x0F	/* shift in		     */
#define DLE	     0x10	/* data link escape	     */
#define DC1	     0x11	/* device control 1	     */
#define DC2	     0x12	/* device control 2	     */
#define DC3	     0x13	/* device control 3	     */
#define DC4	     0x14	/* device control 4	     */
#define NAK          0x15	/* negative acknowledge	     */
#define SYN	     0x16	/* synchronous idle	     */
#define ETB	     0x17	/* end of transmission block */
#define CAN	     0x18	/* cancel		     */
#define EM	     0x19	/* end of medium	     */	
#define SUB	     0x1A	/* substitute		     */
#define ESC	     0x1B	/* escape		     */
#define FS	     0x1C	/* file separator	     */
#define GS	     0x1D	/* group separator	     */
#define RS	     0x1E	/* record separator	     */

#define SEC_SIZE     512   	/* physical sector size      */
#define BNKBDOS_SIZE 0x2E00	/* size of banked   BDOS     */
#define RESBDOS_SIZE 0x0600	/* size of resident BDOS     */


/* disk parameter block */
struct dpb {
	     int  SPT;
	     char BSH, BLM, EXM;
	     int  DSM, DRM;
	     char AL0, AL1;
	     int  CKS, OFF;
	     char PSH, PHM;
	   };

/* buffer control block */
struct bcb {
	     char DRV, REC[3], WFLG, scratch;
	     int  TRACK, SECTOR;
	     char *BUFFAD, BANK, *LINK;
	   };

/* extended disk parameter header */
struct xdph {
	      char (*_WRITE) ();	/* addr of sector WRITE */
	      char (*_READ ) (); 	/* addr of sector READ  */
	      char (*LOGIN ) ();	/* addr of disk   LOGIN */
	      char (*INIT  ) ();	/* addr of disk   INIT  */
	      char unit;		/* physical unit number */
	      char type;		/* drive type		*/
	      char *XLT;		/* translate vector	*/
	      char scratch[9];		/* scratch area		*/
	      char MF;			/* media flag		*/
	      struct dpb *DPB;		/* disk parameter block */
	      char *CSV;		/* check vector		*/
	      char *ALV;		/* alloc vector		*/
	      struct bcb **DIRBCB;	/* dir BCB  header  	*/
	      struct bcb **DTABCB;	/* data BCB header	*/
	      char *HASH;		/* hashing table	*/
	      char HBANK;		/* hash bank		*/
	    };

/* character device table */
struct ctbl {
	      char device_name[6], type, baud_rate;
	    };

/* CPM3.SYS header record */
struct header {
	   	char res_top_page;	/* top page plus one, at which the
				           resident portion of CP/M 3 is to
				           be loaded top down 	 	    */
	   	char res_length;	/* length in pages of the resident
				      	   portion of CP/M 3		    */
	   	char bnk_top_page;      /* top page plus one, at which the
				           banked portion of CP/M 3 is to
				           be loaded top down		    */
	   	char bnk_length;	/* length in pages of the banked 
				           portion of CP/M 3		    */
	   	char *entry;		/* address of CP/M 3 cold boot
				      	   entry point			    */
	   	char filler[122];	
	      };

/* physical RS232C parameters */
struct rs232c {
	 	char bits;
	 	char baud_rate;
              };

/* physical drive parameters */
struct dct {
	     char td1, td2, ilf, spt, tc, curtrk;
           };

/* drive types & unit numbers */
struct drv {
	     char unit, type;
	   };

/* window parameters */ 
struct window {
	   	int  CTRL;		/* control code			    */
	   	char PROMPT;		/* prompt character		    */
	   	char ATTRIB;		/* console attributes		    */
	   	int  BEG;		/* upper left window corner	    */
	   	int  END;		/* lower left window corner	    */
	   	char *BUF;		/* buffer pointer		    */
	   	char option;
	   	char btab;
	      };


_main ()
{
  /* language dependent variables */
  extern char block, _double, endmsg[], error1[], error2[], error3[], error4[],
	      error5[], error6[], even, menu[][1920], national[], no, noparity,
	      odd, single, underline, yes;

  FILE *CPM3_SYS;			/* file containing complete CP/M 3
				           system			    */ 
  char print[REC_SIZE];			/* CPM3.SYS print record	    */
  char *CPM3;				/* pointer to system buffer	    */
  char *tCPM3;				/* temporary pointer		    */
  int  size;				/* size of CPM3.SYS		    */
  char *RESBIOS;			/* pointer to resident BIOS	    */
  int  RESBIOS_SIZE;			/* size of resident BIOS	    */
  int  res_base;			/* base offset to RESBIOS	    */
  char *BNKBIOS;			/* pointer to banked BIOS	    */
  int  BNKBIOS_SIZE;			/* size of banked BIOS		    */
  int  bnk_base;			/* base offset to BNKBIOS	    */
  char SYSTAB[SEC_SIZE];		/* table of system constants        */
  char CPM_drive;			/* logical CP/M drive (0 - 15)	    */
  char first;				/* number of first floppy	    */
  char last;				/* number of last  floppy	    */
  char drive;				/* current drive		    */
  char user;				/* current user			    */ 
  char type;				/* type of mass storage medium	    */
  char key;				/* ASCII code of breaking key	    */
  char *p;				/* temporary pointer		    */
  char c;				/* temporary character		    */
  int  int2;      			/* temp number storage  for convert */
  long int4;				/* temp number storage  for convert */
  int  comma;				/* no of decimal digits for convert */
  BOOL *clock;				/* switch byte for hardware clock   */
  char *german;				/* switch byte for character set    */
  char *vidpar;				/* parameter table for video 
					   controller M6845                 */
  int  i, j, k, l, m;			/* loop counters		    */
  int  index;				/* menu buffer index		    */
  long convert  ();			/* ASCII --> binary conversion	    */
  BOOL notequal ();			/* comparison function		    */

  struct xdph   **dtbl;			/* drive table in memory	    */
  struct xdph   **DTBL;			/* drive table in CPM3.SYS file     */
  struct xdph   *XDPHC;			/* extended DPH for drive M:	    */
  struct xdph   *XDPHF;			/* extended DPH for drive P:	    */
  struct xdph   *XDPHp;			/* temporary extended DPH pointer   */
  char          *XLTF;			/* translation table for drive P:   */
  struct dpb    *DPBC;			/* DPB for drive M:		    */
  struct dpb    *DPBF;			/* DPB for drive P:		    */
  struct ctbl   *CTBL;			/* character device table	    */
  struct header HEADER;			/* CPM3.SYS header record	    */
  struct rs232c *RS232C;		/* physical RS232C parameters	    */
  struct dct	*DCT;       		/* physical drive parameters	    */
  struct drv	drives[16];		/* drive types & unit numbers	    */

  /* window parameters */
  static struct window w[] = {{0x83,    0, 9,    0,   33, menu[0],  2,  0},
			      {0x83,    0, 8,   34,   79, menu[0],  0,  0},
		              {0x83,    0, 0,  100, 2379, menu[0],  0,  0},
		              {0x91, 0x7F, 0, 2359, 2359, menu[0],  1,  0},
		              {0x83,    0, 0, 2359, 2359, menu[0],  3,  0},
		              {0x83,    0, 8,   34,   79, menu[1],  0,  0},
		              {0x83,    0, 0,  100,  179, menu[1],  0,  0},
		              {0x83,    0, 8,  200,  217, menu[1],  0,  0},
		              {0x83,    0, 0,  218,  279, menu[1],  0,  0},
		              {0x83,    0, 0,  300,  879, menu[1],  0,  0},
		              {0x83,    0, 8,  900,  920, menu[1],  0,  0},
		              {0x83,    0, 0,  921,  979, menu[1],  0,  0},
		              {0x83,    0, 0, 1000, 1579, menu[1],  0,  0},
		              {0x83,    0, 8, 1600, 1620, menu[1],  0,  0},
		              {0x83,    0, 0, 1621, 1679, menu[1],  0,  0},
		              {0x83,    0, 0, 1700, 2279, menu[1],  0,  0},
		              {0x83,    0, 8, 2300, 2379, menu[1],  0,  0},
		              {0x92, 0x7F, 0,  346,  346, menu[1],  4, 16},
		              {0x92, 0x7F, 0,  446,  446, menu[1],  5,  1},
		              {0x92, 0x7F, 0,  546,  546, menu[1],  6,  1},
			      {0x92, 0x7F, 0,  646,  646, menu[1],  7,  1},
			      {0x92, 0x7F, 0,  746,  748, menu[1],  1,  1},
			      {0x83,    0, 0,  746,  748, menu[1],  8,  0},
		              {0x92, 0x7F, 0, 1046, 1046, menu[1],  1,  2},
		              {0x83,    0, 0, 1046, 1046, menu[1],  9,  0},
		              {0x92, 0x7F, 0, 1146, 1148, menu[1], 10,  2},
		              {0x83,    0, 0, 1146, 1148, menu[1],  0,  0},   
		              {0x92, 0x7F, 0, 1246, 1246, menu[1], 11,  2},
		              {0x92, 0x7F, 0, 1346, 1350, menu[1], 12,  1},
		              {0x83,    0, 0, 1346, 1350, menu[1],  0,  0},
		              {0x92, 0x7F, 0, 1446, 1446, menu[1], 13,  2},
		              {0x92, 0x7F, 0, 1746, 1746, menu[1],  1,  1},
		              {0x83,    0, 0, 1746, 1746, menu[1],  9,  0},
		              {0x92, 0x7F, 0, 1846, 1848, menu[1], 10,  2},
		              {0x83,    0, 0, 1846, 1848, menu[1],  0,  0},   
		              {0x92, 0x7F, 0, 1946, 1946, menu[1], 11,  2},
		              {0x92, 0x7F, 0, 2046, 2050, menu[1], 12,  1},
		              {0x83,    0, 0, 2046, 2050, menu[1],  0,  0},
		              {0x82, 0x7F, 0, 2146, 2146, menu[1], 13,  2},
		              {0x83,    0, 8,   34,   79, menu[2],  0,  0},
		              {0x83,    0, 0,  100, 1679, menu[2],  0,  0},
		              {0x83,    0, 8, 1700, 1726, menu[2],  0,  0},
		              {0x83,    0, 0, 1727, 1779, menu[2],  0,  0},
		              {0x83,    0, 0, 1800, 2279, menu[2],  0,  0},
		              {0x83,    0, 8, 2300, 2379, menu[2],  0,  0},
		              {0x92, 0x7F, 0,  258,  258, menu[2], 14, 44},
			      {0x83,    0, 0,  358,  358, menu[2],  0,  0},
		              {0x92, 0x7F, 0,  458,  458, menu[2], 15,  2},
		              {0x92, 0x7F, 0,  558,  558, menu[2], 16,  1},
		              {0x92, 0x7F, 0,  658,  658, menu[2], 17,  1},
		              {0x83,    0, 0,  658,  658, menu[2],  0,  0},
		              {0x92, 0x7F, 0,  758,  759, menu[2],  1,  0},
		              {0x83,    0, 0,  758,  759, menu[2], 18,  0},
		              {0x92, 0x7F, 0,  858,  858, menu[2],  1,  2},
		              {0x83,    0, 0,  858,  858, menu[2], 19,  0},
		              {0x92, 0x7F, 0, 1058, 1059, menu[2],  1,  2},
		              {0x83,    0, 0, 1058, 1059, menu[2], 20,  0},
		              {0x92, 0x7F, 0, 1158, 1159, menu[2],  1,  2},
		              {0x83,    0, 0, 1158, 1159, menu[2], 21,  0},
		              {0x92, 0x7F, 0, 1258, 1261, menu[2],  1,  2},
		              {0x83,    0, 0, 1258, 1261, menu[2], 22,  0},
		              {0x92, 0x7F, 0, 1358, 1358, menu[2],  1,  2},
		              {0x83,    0, 0, 1358, 1358, menu[2], 23,  0},
		              {   0, 0x7F, 0, 1458, 1458, menu[2], 24,  2},
		              {0x92, 0x7F, 0, 1558, 1559, menu[2],  1,  0},
		              {0x83,    0, 0, 1558, 1559, menu[2], 25,  0},
		              {0x92, 0x7F, 0, 1858, 1859, menu[2],  1,  2},
		              {0x83,    0, 0, 1858, 1859, menu[2], 26,  0},
		              {0x92, 0x7F, 0, 1958, 1958, menu[2],  1,  2},
		              {0x83,    0, 0, 1958, 1958, menu[2], 27,  0},
		              {0x92, 0x7F, 0, 2058, 2059, menu[2],  1,  2},
		              {0x83,    0, 0, 2058, 2059, menu[2], 28,  0},
		              {0x82, 0x7F, 0, 2158, 2158, menu[2],  1,  2},
	         	      {0x83,    0, 0, 2158, 2158, menu[2], 29,  0}};


  /* get current drive */
  drive = bdos(25, 0   );

  /* get current user */
  user  = bdos(32, 0xFF);

  /* set user 0 */
  bdos (32, 0);

  /* get drive table */
  dtbl = bios(DRVTBL);

  /* search for first floppy disk */
  for (first = 0;; first++)
  {
    XDPHp = (char *)dtbl[first] - 10;
    system (15, 1, 2, &XDPHp->unit, &drives[first]);

    if (!drives[first].type) break;
  }

  /* search for last floppy disk */
  for (last = first;; last++)
    if (dtbl[last])
    {
      XDPHp = (char *)dtbl[last] - 10;
      system (15, 1, 2, &XDPHp->unit, &drives[last]);

      if (drives[last].type) break;
    }
    else break;	
  last--;

  /* select national character set */
  puts (national);

  /* read SYSTAB */
      bios (SELDSK, first );
      bios (HOME  ,      0);
      bios (SETSEC,      9);
      bios (SETDMA, SYSTAB);
  if (bios (READ  ,      0)) 
  {
    puts   (error5);
    xabort (2, drive, user);
  }
  bios (SELDSK, 0);

  /* read system */
  if ((CPM3_SYS = open("A:CPM3.SYS", 0)) == ERROR)
  {
    puts   (error1);
    xabort (2, drive, user);
  }

  /* read header record and print record */
  if (read(CPM3_SYS, &HEADER, REC_SIZE) != REC_SIZE ||
      read(CPM3_SYS, print  , REC_SIZE) != REC_SIZE)
  {
    puts   (error5);
    xabort (2, drive, user);
  }
  
  /* get memory for CPM3.SYS data */
  if (!(CPM3 = malloc(size = (HEADER.res_length + HEADER.bnk_length) * 0x100)))
  {
    puts   (error2);
    xabort (2, drive, user);
  }

  /* read system in reversed order */
  for (tCPM3 = CPM3 + size - REC_SIZE; tCPM3 >= CPM3; tCPM3 -= REC_SIZE)
    if (read(CPM3_SYS, tCPM3, REC_SIZE) != REC_SIZE)
    {
      puts   (error5);
      xabort (2, drive, user);
    }

  /* calculate pointer to banked BIOS */
  BNKBIOS_SIZE = HEADER.bnk_length   * 0x100 - BNKBDOS_SIZE;
  BNKBIOS      = CPM3 + BNKBDOS_SIZE;
  bnk_base     = HEADER.bnk_top_page * 0x100 - BNKBIOS_SIZE - BNKBIOS;

  /* calculate pointer to resident BIOS */
  RESBIOS_SIZE = HEADER.res_length   * 0x100 - RESBDOS_SIZE;
  RESBIOS      = BNKBIOS + BNKBIOS_SIZE + RESBDOS_SIZE;
  res_base     = HEADER.res_top_page * 0x100 - RESBIOS_SIZE - RESBIOS; 

  close (CPM3_SYS);

  
  /* check for legal system */
  DTBL = (char *)dtbl - res_base;

  if (notequal(dtbl, DTBL, sizeof *DTBL * 16))
  {
    puts   (error3);
    xabort (2, drive, user);
  }

  /* initialize structure pointers */
  XDPHC  = (char *)DTBL[16] - 10 - bnk_base;
  XDPHF  = (char *)DTBL[15] - 10 - bnk_base;
  XLTF   = (char *)XDPHF->XLT    - bnk_base;
  DPBC   = (char *)XDPHC->DPB    - res_base;
  DPBF   = (char *)XDPHF->DPB	 - res_base;
  DCT    = (char *)XDPHF + 35;

  CTBL   = bios(DEVTBL) + 3 * sizeof(struct ctbl) - res_base;  

  vidpar = &SYSTAB[0x11E];     		/* video parameter table   	 */
  german = &SYSTAB[0x12E];     		/* switch bit for char set 	 */
  RS232C = &SYSTAB[0x12F];   		/* RS232C parameters	   	 */
  clock  = &SYSTAB[0x133];		/* switch bit for hardware clock */


  /* check for legal boot floppy */
  if (vidpar[1] != 80 || vidpar[6] != 25)
  {
    puts   (error3);
    xabort (2, drive, user);
  }


  /* get current date */
  memcpy (&menu[0][16], cpm3_date(), 8);

  /* get parameters loop */
  for (i = 0;; i++)
  {
    key = window(&w[i]);

    /* check special keys */
    switch (key)
    {
      case ENQ:
      case DC3: i -= w[i].btab + 1;
		continue;
      case NAK: if (i == 3)
		{
		  puts   ("\33=7 \n\n");
		  xabort (1, drive, user);
		}
		else
		{
		  i = 73;
		  goto recalc;
		}
    }

    /* calc current buffer pointer */
    p = w[i].BUF + w[i].BEG / 100 * 80 + w[i].BEG % 100;

    /* check options */
    switch (w[i].option)
    {
      /* ASCII --> binary --> ASCII */
      case  1: int2 = convert(0x10, (long)int2, 0, w[i].END - w[i].BEG + 1, p);
	       break;

      /* turn on clock */
      case  2: system (24, 1, 0, 0, 26);
	       break;

      /* get control code */	
      case  3: switch (int2)
	       {
		 /* system parameters */
		 case 1 : /* get cursor character */
			  menu[1][286] = vidpar[10] & 0x1F ? underline : block;

			  /* get blink rate */
			  menu[1][366] = vidpar[10] & 0x60 ? yes : no;

			  /* get switch bit for char set */
			  menu[1][446] = *german == 'G' ? yes : no;

			  /* get switch bit for hardware clock */
 			  menu[1][526] = *clock ? yes : no;

			  /* calc size of additional RAM */
			  if (DTBL[12]) if (DPBC->DSM == 54) int2 =  64;
							else int2 = 128;
			  else int2 = 0;
			  convert (0x11, (long)int2, 0, 3, &menu[1][606]);

		 	  /* RS-232-C parameters */
		          for (j = k = 0; j < 2; j++, k += 560)
			  {
			    /* get no. of data bits */
			    menu[1][846 + k] = (RS232C[j].bits >> 6) + '5';
			   
			    /* get no. of stop bits */
			    int2 = ((RS232C[j].bits >> 2 & 3) + 1) * 5;
			    convert (1, (long)int2, 1, 3, &menu[1][926 + k]); 

			    /* get parity */
			    if (RS232C[j].bits & 1)
			      menu[1][1006 + k] =
				RS232C[j].bits & 2 ? even : odd;
			    else menu[1][1006 + k] = noparity;

			    /* get baud rate */
			    comma = 0;	/* reset digit count of fraction */

    	    		    switch (RS232C[j].baud_rate)
          		    {
          		      case  0: 
			      case  1: int2  = 19200;
          			       break;
          		      case  2: int2  =    50;
          			       break;
          		      case  3: int2  =    75;
          			       break;
          		      case  4: int2  =  1345;
				       comma =     1;
          			       break;
          		      case  6: int2  =   600;
          			       break;
          		      case  7: 
			      case 12: int2  =  2400;
          			       break;
          		      case  8: int2  =  9600;
          			       break;
          		      case  9: int2  =  4800;
          			       break;
          		      case 10: int2  =  1800;
          			       break;
          		      case 11: int2  =  1200;
          			       break;
          		      case 13: int2  =   300;
          			       break;
          		      case 14: int2  =   150;
          			       break;
          		      case 15: int2  =   110;
          			       break;
          		      default: int2  =     0;
          		    }
			    convert (1, (long)int2, comma, 5,
				     &menu[1][1086 + k]);

			    /* XON/XOFF protocol ? */
			    menu[1][1166 + k] =
			      CTBL[j].type & 0x10 ? 'Y' : 'N';
			  }
			  break;

		 /* floppy disk drives */
		 case 2 : menu[2][207] = first + 'A';
			  menu[2][211] = last  + 'A';

			  /* get physical drive and disk size */
			  for (j = first; j <= last; j++)
			    if (XDPHF->unit < 4)
			    {
			      if (drives[j].unit == XDPHF->unit)
			      {		
			        menu[2][298] = '5';	
				break;
			      }
			    }
			    else
			      if (drives[j].unit == XDPHF->unit) 
			      {
				menu[2][298] = '8';
				break;
			      }	

			  menu[2][218] = j + 'A';

		 	  /* get no. of surfaces */
		 	  if (DCT->td1 & 0x40) menu[2][378] = _double;
					  else menu[2][378] = single;

		 	  /* get density */
		 	  if (DCT->td1 & 0x20) menu[2][458] = _double;
					  else menu[2][458] = single;
 
		 	  /* get density of first track */	      
			    menu[2][538] = single;
		 	  if (menu[2][458] == _double) if (DCT->td1 & 0x10) 
			    menu[2][538] = _double;

		 	  /* get track count */
		 	  convert (0x11, (long)DCT->tc, 0, 2, &menu[2][618]);

		 	  /* no. of steps per track to track */
		 	  menu[2][698] = (DCT->td1 >> 2 & 1) + '1';

		 	  /* get track stepping rate */
			  if (XDPHF->unit < 4) switch (DCT->td1 & 3)
					       {
						 case 0: int2 =  6;
							 break;
						 case 1: int2 = 12;
							 break;
						 case 2: int2 = 20;
							 break;
						 case 3: int2 = 30;
					       }
		 			  else switch (DCT->td1 & 3)
		 	    		       {
		   	    			 case 0: int2 =  3;
			   	    			 break;
		   	    			 case 1: int2 =  6;
			   	    			 break;
		   	    			 case 2: int2 = 10;
			   	    			 break;
		   	    			 case 3: int2 = 15;
		 	  		       }
		 	  convert (0x11, (long)int2, 0, 2, &menu[2][858]);

		 	  /* get no. of sectors per track */
		 	  convert (0x11, (long)DCT->spt, 0, 2, &menu[2][938]);

		 	  /* get sector length */
		 	  k    = DCT->td2 >> 6;
		 	  int2 = 128;
		 	  for (j = 0; j < k; j++) int2 *= 2;
		 	  convert (0x11, (long)int2, 0, 4, &menu[2][1018]);

		 	  /* get no. of first sector */
		 	  menu[2][1098] = (DCT->td1 >> 3 & 1) + '0';

		 	  /* sector numbering continued on back side */
			 		   	   menu[2][1178] = no;
		 	  if (menu[2][378] == _double)
		   	    if (DCT->td2 >> 5 & 1) menu[2][1178] = yes;

		 	  /* get interleaving factor */
		 	  convert (0x11, (long)DCT->ilf, 0, 2, &menu[2][1258]);

		 	  /* get block size */
		 	  convert (0x11, (long)((DPBF->BLM + 1) / 8), 0, 2,
				   &menu[2][1498]);

		 	  /* get dir size */
		 	  menu[2][1578] = DPBF->DRM / 32 + 1 + '0';

			  /* get skew factor */
			  convert (0x11, (long)(XLTF[1] - XLTF[0]), 0, 2,
				   &menu[2][1658]);

		 	  /* get no. of system tracks */
		 	  menu[2][1738] = DPBF->OFF + '0';

			  i += 34;
			  break;

		 /* store system and exit program */
		 case 3 : /* turn off clock */
			  system (24, 0);

			  puts ("\33=7 \n\n");

  			  /* write system */
  			  if ((CPM3_SYS = open("A:CPM3.SYS", 1)) == ERROR)
  			  {
    			    puts   (error4);
    			    xabort (2, drive, user);
  			  }

  			  /* write header record and print record */
  			  if (write(CPM3_SYS, &HEADER, REC_SIZE) != REC_SIZE ||
  			      write(CPM3_SYS, print  , REC_SIZE) != REC_SIZE)
			  {
			    if (errno == EROFS) puts (error6); 
					   else puts (error5);
			    xabort (2, drive, user);
			  }
  
  			  /* write system in reversed order */
  			  for (tCPM3 = CPM3 + size - REC_SIZE; tCPM3 >= CPM3;
			       tCPM3 -= REC_SIZE)
         		    if (write(CPM3_SYS, tCPM3, REC_SIZE) != REC_SIZE)
			    {
			      if (errno == EROFS) puts (error6); 
					     else puts (error5);
			      xabort (2, drive, user);
			    }		

			  close (CPM3_SYS);


  			  /* write SYSTAB */
			  bios (SELDSK, first );
      			  bios (HOME  ,      0);
      			  bios (SETSEC,      9);
      			  bios (SETDMA, SYSTAB);
  			  switch (bios(WRITE, 0))
			  {
			    case 0 : break;
			    case 2 : puts   (error6);
				     xabort (2, drive, user);
			    default: puts   (error5);
 		 	    	     xabort (2, drive, user);
			  }

			  /* select current drive */
			  bios (SELDSK, drive);

			  /* select current user */
			  bdos (32, user);

			  puts (endmsg);
			  return;

		 /* illegal entry */
		 default: i -= 2;
	       }
	       break;

      /* check cursor character */
      case  4: if ((c = toupper(*p)) == block) vidpar[10] &= 0x60;
	       else if (c == underline) 
	            {
		      vidpar[10] = (vidpar[10] & 0x60) + vidpar[9];
		      if (vidpar[9] >= 13) vidpar[10]--;
		    }
		    else i--;
	       break;

      /* blinking cursor ? */
      case  5: if ((c = toupper(*p)) == no) vidpar[10] &= 0x1F;
	       else if (c == yes) vidpar[10] |= 0x60; else i--;
	       break;

      /* check character set */
      case  6: if ((c = toupper(*p)) == no) *german = 'A';
	       else if (c == yes) *german = 'G'; else i--;	
	       break;

      /* check clock */
      case  7: if ((c = toupper(*p)) == no) *clock = FALSE;
	       else if (c == yes) *clock = TRUE; else i--;
	       break;

      /* check size of additional RAM */
      case  8: switch (int2)
	       {
		 case   0: DTBL[12]  = NULL;
			   break;

		 case  64: DTBL[12]  = DTBL[16];
			   DPBC->DSM = 54;
			   break;

		 case 128: DTBL[12]  = DTBL[16];
			   DPBC->DSM = 110;
			   break;

		 default : i -= 2;
	       }
	       break;			

      /* check no. of data bits */
      case  9: j = (i - 21) / 8;
	       if (int2 >= 5 && int2 <= 8)
		 RS232C[j].bits = RS232C[j].bits & 0x3F | (int2 - 5) << 6;
	       else i -= 2;
	       break;

      /* check no. of stop bits */
      case 10: switch (int2 =
			 convert(0, (long)int2, 1, w[i].END - w[i].BEG + 1, p))
	       {
		 case 10:
		 case 15:
		 case 20: RS232C[j].bits = RS232C[j].bits & 0xF3 |
				           (int2 / 5 - 1) << 2;
		          break;
	         default: i--;
	       }
	       break;

      /* check parity */
      case 11: if ((c = toupper(*p)) == even) RS232C[j].bits |= 3;
	       else if (c == noparity) RS232C[j].bits &= 0xFE;
		    else if (c == odd) RS232C[j].bits  =
					 RS232C[j].bits & 0xFC | 1;
			 else i--;
	       break;

      /* check baud rate */
      case 12: int2 = (int4 = 
		       convert(-1, (long)int2, 1, w[i].END - w[i].BEG + 1, p))
		    /  10L;

	       /* special case: baud rate = 134.5 */
	       if (int4 == 1345)
	       {
		 convert (1, int4, 1, w[i].END - w[i].BEG + 1, p);
		 RS232C[j].baud_rate = CTBL[j].baud_rate = 4;
		 break;
	       }

	       switch (int2)
	       {	
	         case    50: RS232C[j].baud_rate = 2;
			     CTBL  [j].baud_rate = 1;
			     break;
	         case    75: RS232C[j].baud_rate = 3;
			     CTBL  [j].baud_rate = 2;
			     break;
	         case   110: RS232C[j].baud_rate = 15;
			     CTBL  [j].baud_rate =  3;
			     break;
	         case   150: RS232C[j].baud_rate = 14;
			     CTBL  [j].baud_rate =  5;
			     break;
	         case   300: RS232C[j].baud_rate = 13;
			     CTBL  [j].baud_rate =  6;
			     break;
	         case   600: RS232C[j].baud_rate =  6;
			     CTBL  [j].baud_rate =  7;
			     break;
	         case  1200: RS232C[j].baud_rate = 11;
		      	     CTBL  [j].baud_rate =  8;
			     break;
	         case  1800: RS232C[j].baud_rate = 10;
		      	     CTBL  [j].baud_rate =  9;
		      	     break;
	         case  2400: RS232C[j].baud_rate =  7;
			     CTBL  [j].baud_rate = 10;
			     break;
	         case  4800: RS232C[j].baud_rate =  9;
			     CTBL  [j].baud_rate = 12;
			     break;
	         case  9600: RS232C[j].baud_rate =  8;
			     CTBL  [j].baud_rate = 14;
			     break;
	         case 19200: RS232C[j].baud_rate =  0;
			     CTBL  [j].baud_rate = 15;
			     break;
	         default   : i--;
			     continue;
	       }
	       convert (1, (long)int2, 0, w[i].END - w[i].BEG + 1, p);
	       break;

      /* XON/XOFF protocol */
      case 13: if ((c = toupper(*p)) == no) CTBL[j].type &= 0xEF;
	       else if (c == yes) CTBL[j].type |= 0x10; else i--;	

			   /* return to main menu */
	       if (j == 1) i -= 38;
	       break;

      /* get drive code */
      case 14: if ((CPM_drive = toupper(*p) - 'A') >= first && 
		    CPM_drive 			   <= last)
		 if ((XDPHF->unit = drives[CPM_drive].unit) < 4) 
		      menu[2][298] = '5';
		 else menu[2][298] = '8';
	       else i--;
	       break;
			
      /* check no. of surfaces */
      case 15: if ((c = toupper(*p)) == _double)
	       {
		 DCT->td1      |= 0x40;
		 w[i + 16].CTRL = 0x92;
		 w[i + 17].btab = 1;
	       }
	       else if (c == single)
		    {	
		      DCT->td1      &= 0xBF;
		      menu[2][1178]  = no;
		      w[i + 16].CTRL = 0x83;
		      w[i + 17].btab = 3;
		    }
		    else i--;
	       break;

      /* check density */
      case 16: if ((c = toupper(*p)) == _double)
	       {
	  	 DCT->td1     |= 0x20;
		 w[i + 3].btab = 2;
	       }
	       else if (c == single)
		    {
		      DCT->td1     &= 0xCF;
		      menu[2][538]  = single;
		      w[i + 3].btab = 3;
		      i++;
		    }
		    else i--;
	       break;
						
      /* check density of first track */
      case 17: if ((c = toupper(*p)) == _double) DCT->td1 |= 0x10;
	       else if (c == single) DCT->td1 &= 0xEF; else i--;	
	       break;

      /* check track count */
      case 18: if (int2 > 0) DCT->tc = int2; else i -= 2;
	       break;

      /* check no. of steps per track to track */
      case 19: if (int2 == 1 || int2 == 2) 
		 DCT->td1 = DCT->td1 & 0xFB | (int2 - 1) << 2;
	       else i -= 2;
	       break;

      /* check track stepping rate */
      case 20: if (XDPHF->unit < 4) switch (int2)
	         		    {
		   		      case  6: DCT->td1 = DCT->td1 & 0xFC;
			    	      	       break;
		   		      case 12: DCT->td1 = DCT->td1 & 0xFC | 1;
			    		       break;
		   		      case 20: DCT->td1 = DCT->td1 & 0xFC | 2;
			    		       break;
		   		      case 30: DCT->td1 = DCT->td1        | 3;
			    		       break;
		   		      default: i -= 2;
	         		    }	
	       		       else switch (int2)
	         		    {
		   		      case  3: DCT->td1 = DCT->td1 & 0xFC;
			    		       break;
		   		      case  6: DCT->td1 = DCT->td1 & 0xFC | 1;
			    		       break;
		   		      case 10: DCT->td1 = DCT->td1 & 0xFC | 2;
			    		       break;
		   		      case 15: DCT->td1 = DCT->td1        | 3;
			    		       break;
		   		      default: i -= 2;
	         		    }	
	       break;

      /* check no. of sectors per track */
      case 21: if (int2 > 0) DPBF->SPT = (DCT->spt = int2) * (DPBF->PHM + 1);
 	       else i -= 2;
	       break;

      /* check physical sector length */
      case 22: switch (int2)
	       {
		 case  128: DPBF->SPT = DCT->spt;
			    DPBF->PSH = 0;
			    DPBF->PHM = 0;
			    DCT->td2  = DCT->td2 & 0x3F; 
			    break;
		 case  256: DPBF->SPT = DCT->spt * 2;
			    DPBF->PSH = 1;
			    DPBF->PHM = 1;
			    DCT->td2  = DCT->td2 & 0x3F | 0x40;
			    break;
		 case  512: DPBF->SPT = DCT->spt * 4;
			    DPBF->PSH = 2;
			    DPBF->PHM = 3;
			    DCT->td2  = DCT->td2 & 0x3F | 0x80;
			    break;
		 case 1024: DPBF->SPT = DCT->spt * 8;
			    DPBF->PSH = 3;
			    DPBF->PHM = 7;
			    DCT->td2  = DCT->td2 | 0xC0;
			    break;
		 default  : i -= 2;
	       }
	       break;

      /* check no. of first sector */
      case 23: if (!int2 || int2 == 1)
		 DCT->td1 = DCT->td1 & 0xF7 | int2 << 3;
	       else i -= 2;
	       break;

      /* sector numbering continued on back side ? */
      case 24: if ((c = toupper(*p)) == no) DCT->td2 &= 0xDF;
	       else if (c == yes) DCT->td2 |= 0x20; else i--;
	       break;

      /* check interleaving factor */
      case 25: DCT->ilf = int2;
	       break;

      /* check block size */
      case 26: switch (int2)
	       {
		 case  1: DPBF->BSH =   3;
			  DPBF->BLM =   7;
			  break;
		 case  2: DPBF->BSH =   4;
			  DPBF->BLM =  15;
			  break;
		 case  4: DPBF->BSH =   5;
			  DPBF->BLM =  31;
			  break;
		 case  8: DPBF->BSH =   6;
			  DPBF->BLM =  63;
			  break;
		 case 16: DPBF->BSH =   7;
			  DPBF->BLM = 127;
			  break;
		 default: i -= 2;
 	       }
	       break;

      /* check directory size */
      case 27: if (int2 > 0)
      	       {
		 DPBF->CKS = ((DPBF->DRM = int2 * 32 - 1) + 1) / 4;

		 /* calc. block count for directory */
		 j = (DPBF->BLM + 1) / 8;
		 j = int2 / j + (int2 % j ? 1 : 0);
		 for (DPBF->AL0 = 0, k = 128; k && j; k /= 2, j--)
		   DPBF->AL0 += k;
		 for (DPBF->AL1 = 0, k = 128; k && j; k /= 2, j--)
		   DPBF->AL1 += k;
      	       }
      	       else i -= 2;
      	       break;

      /* check skew factor */
      case 28: if (int2 > 0)
	       {
 	 	 /* build sector translate table */
		 m = DCT->spt / ((DCT->td1 >> 6 & 1) + 1);
		 for (j = k = 0; j < m; j++)
		 {
		   XLTF[j]=k;
		   if ((k += int2) >= m)
		   {
		     k -= m;
		     for (l = 0; l <= j; l++) if (XLTF[l] == k)
		     			      {
		       				k++;	 		  	
		       				l = -1;
		     			      }
		   }
		 }
		 
		 /* double sided ? */
		 if (m != DCT->spt) for (j = 0; j < m; j++) 
				      XLTF[j + m] = XLTF[j] + m;
	       }	
	       else i -= 2;
	       break;

      /* check no. of system tracks */
      case 29: if (int2 >= 0)
	       {
		 DPBF->OFF = int2;
	
		 recalc:
		 if ((DPBF->DSM = (DCT->tc - DPBF->OFF) * DPBF->SPT 
				/ (DPBF->BLM + 1) - 1) <= 255)
		   switch (DPBF->BLM)
		   {
		     case   7: DPBF->EXM =  0;
			       break;
		     case  15: DPBF->EXM =  1;
			       break;
		     case  31: DPBF->EXM =  3;
			       break;
		     case  63: DPBF->EXM =  7;
			       break;
		     case 127: DPBF->EXM = 15;
		   }
 		 else
		   switch (DPBF->BLM)
		   {
		     case   7: i -= 8;			/* error */
			       continue;
		     case  15: DPBF->EXM =  0;
			       break;
		     case  31: DPBF->EXM =  1;
			       break;
		     case  63: DPBF->EXM =  3;
			       break;
		     case 127: DPBF->EXM =  7;
		   } 
	       }	
	       else
	       { 	
		 i -= 2;
		 break;
	       }		
	
	       i -= 73;
    }
  }
}


static BOOL notequal (buf1, buf2, n)
  char *buf1, *buf2;
  int n;
{
  int i;

  for (i = 0; i < n; i++) if (buf1[i] != buf2[i]) return TRUE;
						  return FALSE;
}


static xabort (mode, drive, user)
  char mode, drive, user;
{
  bios   (SELDSK, drive);
  bdos   (    32, user );  
  system (    24,     0);
  _exit  (mode);
}
