#include <stdio.h>
#include <bios.h>
#include <ctype.h>
#include <cpm.h>


#define SUB 0x1A
#define NAK 0x15
#define SEC_SIZE 512


void system(int func, int breg,...);		/* Prototyp der Funktion */

char window();

extern char endmsg[],  menu[], national[], yes, no;

char SYSTAB[3 * SEC_SIZE];		/* table of system constants   */
int  drive;				/* current drive	       */
char type;				/* drive type		       */
char *dmaadr;				/* current DMA address	       */
int  i, j, k;				/* loop counters	       */
int  index;				/* menu buffer index	       */
char key;				/* last input key	       */
char c; 				/* temporary character storage */
BOOL control;				/* marker for control sequence */
char *ausgabe;


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
    bios(SETDMA,dmaadr);
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
		} w[] = {{0x0083, 0, 9,    0,   79, menu, 1},
			 {0x0083, 0, 0,  100, 2079, menu, 0},
			 {0x0083, 0, 0, 2100, 2279, menu, 0},
			 {0x0083, 0, 8, 2300, 2379, menu, 0},
			 {0x0084, 0, 0,  807, 2079, menu, 0},
			 {0x0091, 0, 8, 2334, 2334, menu, 2}};


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

  cputs(national);

  /* read system */
  dmaadr = SYSTAB;
  for (i = 2; i <= 4; i++)
  {
    if (  lies( 2, 0, i, dmaadr) ) 
	{
	printf("FEHLER"); 
	break;
	};     	
    dmaadr += SEC_SIZE;
  }

  /* move fkeys into menu buffer */
  fkeys = (struct tasten *) &SYSTAB[0x14C];
  for (i = 0; i < 13; i++)
  {
    k     = 0;
    index = 647 + i * 80;

    for (j = 0; j < fkeys[i].length; j++)
      if (!fkeys[i].string[j])
      {
        if (k < 73) menu[index + k++] = ' ';    
      }
      else
        if (fkeys[i].string[j] < ' ')      
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



  /* turn off clock */
  system (24, 0);

  cputs ("\33=7 \n\n");

  cputs(endmsg);
  	
}
