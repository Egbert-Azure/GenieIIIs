/******************************************************************************
*  F K E Y  *  U T I L S 0 0 5  *  T h o m a s   H o l t e  *   8 6 0 9 1 1   *
*******************************************************************************
*                                                                             *
*        F U N C T I O N   K E Y   P R O G R A M M E R   F O R   T H E        *
*        =============================================================        *
*                                                                             *
*        G E N I E   I I I s   M I C R O C O M P U T E R   S Y S T E M        *
*        =============================================================        *
*									      *
*                                                                             *
*   Thomas Holte                                                 Version 1.0  *
*   puts durch cputs ersetzt                                                  *
******************************************************************************/

#include <stdio.h>
#include <bios.h>
#include <ctype.h>
#include <cpm.h>
#include <holte.h>
#include <time.h>

#define NAK      0x15
#define DRIVE 'A'
#define TRK  0
#define SECT 2
#define SEC_SIZE 512

void system(int func, int breg,...);		/* Prototyp der Funktion */

  /* language dependent variables */
  extern char endmsg[], error1[], error2[], menu[], national[], no, yes;

  char SYSTAB[3 * SEC_SIZE];		/* table of system constants   */
  char drive;				/* current drive	       */
  char type;				/* drive type		       */
  char *dmaadr;				/* current DMA address	       */
  int  i, j, k;				/* loop counters	       */
  int  index;				/* menu buffer index	       */
  char key;				/* last input key	       */
  char c;				/* temporary character storage */
  BOOL control;				/* marker for control sequence */
  char window();
  char DISK;
  int  TRACK, SECTOR;  

/* Sektor mit der Belegung der Funktionstasten ins RAM laden */

int lies(char disk, int track, int sector, char *dmaadr)
{
  if(bios(SELDSK,disk)==0)
  {
    printf("Laufwerk nicht vorhanden !\n");
    exit();
    }
  else
  {
    bios(SETTRK,track);
    bios(SETSEC,sector);
    bios(SETDMA,SYSTAB);
    return(bios(READ));
    }
}
            
struct tasten {
           char length, string[80];
         } *fkeys;

  /* window parameters */
  static struct {
		  int  CTRL;		/* control code 	    */
		  char PROMPT;		/* prompt character	    */
		  char ATTRIB;		/* console attributes	    */
		  int  BEG;		/* upper left window corner */
		  int  END;		/* lower left window corner */
		  char *BUF;		/* buffer pointer	    */
		  char option;
		} w[] = {{0x83, 0, 9,    0,   79, menu, 1},
			 {0x83, 0, 0,  100, 2079, menu, 0},
			 {0x83, 0, 0, 2100, 2279, menu, 0},
			 {0x83, 0, 8, 2300, 2379, menu, 0},
			 {0x84, 0, 0,  707, 2079, menu, 0},
			 {0x91, 0, 8, 2334, 2334, menu, 2}};

void xabort (mode, drive)
  char mode, drive;
{
  bios   (SELDSK,     3);
  system (    24,     0);
  exit  ();
}

void main()
{
  control = FALSE;

  /* deselect error return mode */
  bdos (45, 0);

  /* select national character set */
  cputs (national);

  /* get current drive */
  drive = bdos(25);

  /* get drive table */
/*  DTBL = bios(DRVTBL);
*/
  /* search for first floppy disk */
/*  for (i = 0;; i++)
  {
    XDPH = (char *)DTBL[i] - 10;
    system (15, 1, 1, &XDPH->type, &type);

    if (!type) break;    
  }
*/

  /* read system */
  dmaadr = SYSTAB;
    if (  lies( DRIVE-65, TRK, SECT, dmaadr) ) 
	{
	printf("FEHLER");
        exit();
	};     	
    dmaadr += SEC_SIZE;

  /* move fkeys into menu buffer */
  fkeys = (struct tasten *) &SYSTAB[0x14C];
  for (i = 0; i < 13; i++)
  {
    k     = 0;
    index = 647 + i * 80;               /* calc buffer index */

    for (j = 0; j < fkeys[i].length; j++)
      if (!fkeys[i].string[j])
      {
        if (k < 73) menu[index + k++] = ' ';    /* convert NULL to space */
      }
      else
        if (fkeys[i].string[j] < ' ')        /* convert controls */
        {
          if (k < 72)
          {
            menu[index + k++] = '^';
            menu[index + k++] = fkeys[i].string[j] + '@';
          }
        }
        else
          if (k < 73)
            menu[index + k++] =
              fkeys[i].string[j] == ' ' ? '_' : fkeys[i].string[j];

    for (; k < 73; k++) menu[index + k] = ' ';
  }


  /* get current date */
  /* memcpy (&menu[16], cpm3_date(), 8); */ 
{
  time_t	t;
  time(&t);
  memcpy (&menu[15], ctime(t), 10);
}	

  /* get parameters loop */
  for (i = 0;; i++)
  {
    key = window(&w[i]);

    /* check ESC function */
    if (key == NAK)
    {
      cputs   ("\33=7 \n\n");
      xabort (1, drive);
    }

    /* check options */
    switch (w[i].option)
    {
      /* turn on clock */
      case  1: system (24, 1, 0, 0, 26);
	       continue;

      /* all entries ok ? */
      case  2: if ((c = toupper(key)) == yes) break; else if (c == no) i--;
               i--;

      default: continue;	
    }
    break;
  }

  /* move fkeys into system buffer */
  for (i = 0; i < 13; i++)
  {
    k     = 0;
    index = 647 + i * 80;               /* calc buffer index */

    for (j = 0; j < 73; j++)
      if (control)
      {
        fkeys[i].string[k++] = menu[index + j] & 0x1F;
        control              = FALSE;
      }
      else
      {
	switch (menu[index + j])
	{
	  case ' ': break;
	  case '^': control = TRUE;
		    continue;
	  case '_': fkeys[i].string[k++] = ' ';
		    continue;
	  default : fkeys[i].string[k++] = menu[index + j];
		    continue;
	}
	break;
      }

    for (; k < 73; k++) fkeys[i].string[k] = 0;

    k = 73;
    while (k-- && !fkeys[i].string[k]);
    fkeys[i].length = ++k ? k : ++k;
  }

  /* turn off clock */
  system (24, 0);

  cputs ("\33=7 \n\n");

  /* write SYSTAB */
  dmaadr = SYSTAB;

  if(bios(SELDSK,DISK)==0)
  {
    printf("Laufwerk nicht vorhanden !\n");
    exit();
    }
  else
  {
    bios(SETTRK,TRACK);
    bios(SETSEC,SECTOR);
    bios (SETDMA, dmaadr);
    switch (bios(WRITE, 0)) 
    {
      case 0 : break;
      case 2 : cputs   (error2);
	       xabort (2, drive);
      default: cputs   (error1);
	       xabort (2, drive);
    }
    dmaadr += SEC_SIZE;
  }

  /* select current disk */
  bios (SELDSK, drive);
  cputs (endmsg);
}
