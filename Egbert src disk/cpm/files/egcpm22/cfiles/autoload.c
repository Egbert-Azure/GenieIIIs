/******************************************************************************
*  A U T O L O A D  * U T I L S 0 0 1 * T h o m a s   H o l t e * 8 4 0 6 2 7 *
*******************************************************************************
*									      *
*  	   A U T O S T A R T   O F   U S E R   P R O G R A M S   A T	      *
*          =========================================================          *
*									      *
*	            	  P O W E R - O N / R E S E T			      *
*		          ===========================       		      *
*									      *
*									      *
*  Version 1.1						        Thomas Holte  *
*									      *
******************************************************************************/

#include <stdio.h>

#define NAK   0x15

xmain ()
{
  extern char zbuf[];
  extern tab[][4];
  char buf[512], errno, i, key, option; 
  int index;

  /* read boot sector */
  if (errno = system(READD, 0, 0, buf, 0)) errmsg (errno);

  /* move command line into form buffer */
  i = 0;
  while (i < buf[0x59]) zbuf[818 + i  ] = buf[0x5A + i++];
  while (i < 43	      ) zbuf[818 + i++] = ' ';

  /* turn on graphics mode */
  outp (0xF5, 0);

  /* get parameters loop */
  for (i = 0;; i++)
  {
    index  = tab[i][3] >> 4;
    option = tab[i][3] &  1;

    key = window(tab[i][0], tab[i][1], tab[i][2], &zbuf[index]);

    /* check ESC function */
    if (key == NAK)
    {
      puts  ("\33=7 \n");
      abort (0);
    }

    /* check options */
    if (option)
    {
      switch (toupper(zbuf[index]))
      {
	case 'Y': break;
        case 'N': i--;
	default : i--;
	continue;
      }
      break;
    }
  }

  puts ("\33=7 \n\n");

  /* move command line into disk buffer */
  i = 42;
  while (i >= 0 && zbuf[818 + i] == ' ') i--;
  buf[0x59] = ++i;
  for   (i = 0; i < buf[0x59]; i++) buf[0x5A + i  ] = zbuf[818 + i];
  while (       i < 43		  ) buf[0x5A + i++] = 0;  

  /* write boot sector */
  if (errno = system(WRITED, 0, 0, buf, 0)) errmsg (errno);
}


errmsg (errno)
  char errno;
{
  switch (errno)
  {
    case 1: puts ("ILLEGAL DRIVE #\n");
	    break;
    case 2: puts ("TRACK # TOO HIGH\n");
	    break;
    case 3: puts ("SECTOR # TOO HIGH\n");
	    break;
    case 4: puts ("DEVICE NOT AVAILABLE\n");
	    break;
    case 5: puts ("WRITE PROTECTED DISKETTE\n");
	    break;
    case 6: puts ("WRITE FAULT ON DISK DRIVE\n");
	    break;
    case 7: puts ("DATA RECORD NOT FOUND\n");
	    break;
    case 8: puts ("PARITY ERROR\n");
	    break;
    case 9: puts ("LOST DATA\n");
  }
  abort (1);
}
