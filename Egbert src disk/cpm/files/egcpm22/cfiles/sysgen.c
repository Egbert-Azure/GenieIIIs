/******************************************************************************
*  S Y S G E N  *  U T I L S 0 0 7  *  T h o m a s   H o l t e * 8 3 0 9 2 1  *
*******************************************************************************
*									      *
*	 S Y S T E M   G E N E R A T O R   F O R   C P / M - B A S E D        *
*        =============================================================        *
*									      *
*	           M I C R O C O M P U T E R   S Y S T E M S		      *
*		   =========================================		      *
*									      *
*									      *
*  Version 1.0						        Thomas Holte  *
*									      *
******************************************************************************/

#include <stdio.h>

#define HOME     8
#define SELDSK   9
#define SETTRK  10
#define SETSEC  11
#define SETDMA  12
#define READ    13
#define WRITE   14

#define WRDIR    1	/* write to directory   */
#define WRUAL    2	/* write to unallocated */

char c;

struct DPB {
             int SPT;
	     char BSH, BLM, EXM;
	     int DSM, DRM;
	     char AL0, AL1;
	     int CKS, OFF;
	   }; 	 		 
struct DPH {
	     char *XLT, filler[6], *DIRBUF;
	     struct DPB *DPBptr;
	     char *CSV, *ALV;
	   } *DPBASE;


xmain ()
{
  BOOLEAN ungetpar ();

  puts ("SYSGEN VER 2.0\n");

  do puts ("SOURCE DRIVE NAME (OR RETURN TO SKIP)"); while (ungetpar());

  if (c != '\n')
  {
    puts ("\nSOURCE ON "); sysio (READ);
  }

  for (;;)
  {
    do puts ("DESTINATION DRIVE NAME (OR RETURN TO REBOOT)");
    while (ungetpar());

    if (c != '\n')
    {
      puts ("\nDESTINATION ON "); sysio (WRITE);
      puts ("FUNCTION COMPLETE\n");
    }
    else break;
  }  
}


BOOLEAN ungetpar ()
{
  if ((c = toupper(getchar())) == '\n') return FALSE;
  if (!(DPBASE = bios(SELDSK, c - 'A', 0)))
  {
    puts ("\nINVALID DRIVE NAME (USE A, B, C, OR D)\n");
    return TRUE;
  }
  else return FALSE;
}


sysio (mode)
  int mode;
{
  int dmaadr, i, j, lastsec, lasttrk;

  putchar (c); puts(", THEN TYPE RETURN");
  if (getchar() != '\n') exit ();

  bios (HOME, 0, 0);
  dmaadr = 0x900;

  lasttrk = DPBASE->DPBptr->OFF - 1;
  lastsec = DPBASE->DPBptr->SPT - 1;

  for (i = 0; i <= lasttrk; i++)
  {
    bios (SETTRK, i, 0);	
    for (j = 0; j <= lastsec; j++)
    {
      bios (SETSEC, 	 j, 0);
      bios (SETDMA, dmaadr, 0);

      if (bios(mode, i == lasttrk && j == lastsec ? WRDIR : WRUAL, 0))
      {
        puts ("PERMANENT ERROR, TYPE RETURN TO IGNORE");
	if (getchar() != '\n') exit ();
      }

      dmaadr += 128;
    }
  }
}
