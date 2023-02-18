/******************************************************************************
*  C H A R G E N  *  U T I L S 0 0 2  * T h o m a s   H o l t e * 8 6 0 9 2 3 *
*******************************************************************************
*  									      *
*    	  F O N T   E D I T O R   F O R   T H E   G E N I E   I I I s         *
*         ===========================================================         *
*                                                                             *
*            	    M I C R O C O M P U T E R   S Y S T E M		      *
*                   =======================================       	      *
*  									      *
*                            						      *
*  Version 1.0                                                  Thomas Holte  *
*  									      *
******************************************************************************/
    
#include <stdio.h>
#include <bios.h>

/* ASCII control codes */
#define ETX 0x03		/* end of text      	 */
#define EOT 0x04		/* end of transmission   */
#define ENQ 0x05		/* enquiry		 */
#define BEL 0x07		/* bell			 */
#define HT  0x09		/* horizontal tabulation */
#define CR  0x0D		/* carriage return	 */
#define SO  0x0E		/* shift out		 */
#define SI  0x0F		/* shift in		 */
#define DC1 0x11		/* device control 1	 */
#define DC2 0x12		/* device control 2	 */
#define DC3 0x13		/* device control 3	 */
#define NAK 0x15		/* negative acknowledge  */
#define SYN 0x16		/* sychronous idle	 */
#define CAN 0x18		/* cancel		 */
#define EM  0x19		/* end of medium	 */
#define SUB 0x1A		/* substitute		 */
#define ESC 0x1B		/* escape		 */
#define DEL 0x7F		/* delete		 */

#define SEC_SIZE 512		/* physical sector size  */

/* language dependent variables (globals) */
extern char exit, store;


_main ()
{
  /* language dependent variables */
  extern char endmsg[], error1[], error2[], menu[], national[];

  static char chr[16];			/* bit pattern			*/
  	 char cfont ();			/* font editor			*/
	 char SYSTAB[0xC00];		/* system constants		*/
	 char (*cset)[16];		/* ^character set		*/
	 char heigth; 			/* heigth of character		*/
	 char key;			/* return key of cfont & window */
  	 int  i;			/* loop counter			*/
	 int  int2;			/* temporary integer		*/
	 char code;			/* current character code	*/
	 char drive;			/* current drive		*/
	 char type;			/* drive type (0 = floppy disk) */
	 char *dmaadr;			/* current buffer address	*/
	 char *p;			/* temporary character pointer  */

  /* extended disk parameter header */
  struct {
	   char (*_WRITE) ();		/* addr of sector WRITE */
	   char (*_READ ) (); 		/* addr of sector READ  */
	   char (*LOGIN ) ();		/* addr of disk   LOGIN */
	   char (*INIT  ) ();		/* addr of disk   INIT  */
	   char unit;			/* physical unit number */
	   char type;			/* drive type		*/
	   char *XLT;			/* translate vector	*/
	   char scratch[9];		/* scratch area		*/
	   char MF;			/* media flag		*/
	   char *DPB;			/* disk parameter block */
	   char *CSV;			/* check vector		*/
	   char *ALV;			/* alloc vector		*/
	   char **DIRBCB;		/* dir BCB  header  	*/
	   char **DTABCB;		/* data BCB header	*/
	   char *HASH;			/* hashing table	*/
	   char HBANK;			/* hash bank		*/
         } **DTBL,			/* drive table		*/
	   *XDPH;

  /* bit pattern for special cursor */
  static char curlft[16] = {0x07, 0x0E, 0x1C, 0x38, 0x70, 0xE0, 0xE0, 0x70,
			    0x38, 0x1C, 0x0E, 0x07, 0x00, 0x00, 0x00, 0x00};
  static char currgt[16] = {0xE0, 0x70, 0x38, 0x1C, 0x0E, 0x07, 0x07, 0x0E,
			    0x1C, 0x38, 0x70, 0xE0, 0x00, 0x00, 0x00, 0x00};

  /* window parameters */
  static struct {
		  int  CTRL;		/* control code 	    */
		  char PROMPT;		/* prompt character	    */
		  char ATTRIB;		/* console attributes	    */
		  int  BEG;		/* upper left window corner */
		  int  END;		/* lower left window corner */
		  char *BUF;		/* buffer pointer	    */
		  char option;
		} w[] = {{0x83,    0, 9,    0,   79,  menu	, 2},
			 {0x83,    0, 0,  100,  479,  menu	, 0},
			 {0x83,    0, 0,  500,    0,  menu	, 0},
			 {   3,    0, 0,    0,    0, &menu[1680], 0},
			 {   0,    0, 0,    0, 2279,           0, 0},
			 {0x83,    0, 8, 2300, 2379,  menu      , 0},
			 {   0, 0x7F, 0,  209,  210,  menu	, 3}, 
			 {0x92, 0x7F, 0,  209,  210,  menu      , 1},
			 {0x83,    0, 0,  209,  210,  menu      , 4},
			 {   3,    0, 0,  248,  255,           0, 5},
			 {0x92, 0x7F, 0,  209,  210,  menu      , 6}};


  /* deselect error return mode */
  bdos (45, 0);

  /* get current drive */
  drive = bdos(25);

  /* get drive table */
  DTBL = bios(DRVTBL);

  /* search for first floppy disk */
  for (i = 0;; i++)
  {
    XDPH = (char *)DTBL[i] - 10;
    system (15, 1, 1, &XDPH->type, &type);
    if (!type) break;    
  }

  /* read video parameters */
      bios (SELDSK, 	 	  i);
      bios (HOME  , 	 	  0);
      bios (SETSEC,      	  9);
      bios (SETDMA, dmaadr = SYSTAB);
  if (bios (READ  ,      	  0))
  {
    puts   (error1);
    xabort (2, drive);
  }

  /* get character heigth */
  heigth = SYSTAB[0x127];

  /* adjust "window" table */
  w[2].END = 579 + (i = heigth * 100);
  w[3].BEG = 600 +  i;
  w[3].END = 679 +  i;
  w[4].BEG = 700 +  i;

  /* read SYSTAB */
  bios (SETTRK, 0);
  for (i = 4; i <= 9; i++)
  {
        bios (SETSEC, i     );
        bios (SETDMA, dmaadr);
    if (bios (READ  ,      0)) 
    {
      puts   (error1);
      xabort (2, drive);
    }
    dmaadr += SEC_SIZE;
  }

  /* cset --> font buffer */
  cset = &SYSTAB[0x1D7];

  /* clear special display character */
  system (20, 0x00, 0, chr);

  /* load special cursor */
  system (20, 0x01, 0, curlft);
  system (20, 0x02, 0, currgt);

  /* select national character set */
  puts (national);

  /* get current date */
  memcpy (&menu[16], cpm3_date(), 8);

  /* get parameters loop */
  for (i = 0;; i++)
  {
    key = window(&w[i]);

    /* check break key */
    switch (key)
    {
      case NAK: puts   ("\33=7 \n\n");
		xabort (1, drive);

      case ESC:	puts ("\33=7 \n\n");

		/* write SYSTAB */
  		dmaadr = SYSTAB;
  		for (i = 4; i <= 9; i++)
  		{
        	  bios (SETSEC, i     );
        	  bios (SETDMA, dmaadr);
    		  switch (bios(WRITE, 0))
		  {
		    case 0 : break;	     	
		    case 2 : puts   (error2);	
			     xabort (2, drive);
		    default: puts   (error1);	
			     xabort (2, drive);
		  }
    		  dmaadr += SEC_SIZE;
  		}

		/* turn off clock */
		system (24, 0);

		puts (endmsg);

		/* restore original character set */
		system (21);

		/* select current drive */
		bios (SELDSK, drive);

		return;
    }  	

    /* calc current buffer pointer */
    p = w[i].BUF + w[i].BEG / 100 * 80 + w[i].BEG % 100;

    /* check options */
    switch (w[i].option)
    {
      case 1: /* ASCII --> binary --> ASCII */
	      int2 = hexconv(0, (long)int2, 2, p);
	      break;

      case 2: /* turn on clock */
	      system (24, 1, 0, 0, 26);
	      break;

      case 3: /* clear displayed character */
	      cfont (0, heigth);
	      break;	

      case 4: /* check font set */
	      if (int2 < 8)
	      {	
		code         = int2 + 136;    	      /* select national set */
		w[i + 1].BUF = &menu[1920];
	      }	
	      else if (int2 >= ' ' && int2 <= 0x7F) 
		   {
		     code 	  = int2;	      /* select ASCII    set */
		     w[i + 1].BUF = &menu[1928];
		   }
		   else if (int2 >= 0x80 && int2 <= 0x9F)
			{
			  code         = int2 - 128;  /* select graphics set */
			  w[i + 1].BUF = &menu[1936];
			}
			else i -= 3;		      /* illegal code        */
	      break; 	
 		
      case 5: /* display character */
	      cfont (3, heigth, cset[code]);
	      break;

      case 6: /* check next key */
	      switch (key)
	      {
		/* next character */
		case ETX: if (int2 < 7 || int2 >= ' ' && int2 < 0x9F) int2++;
			  else if (int2 == 7) int2 = ' ';
	      		  hexconv (1, (long)int2, 2, p);	
			  break;

		/* edit character */
		case HT : memcpy (chr, cset[code], sizeof chr);
			  if (cfont(4, heigth, chr) == store)
			  {
		  	    memcpy (cset[code], chr, sizeof chr);

			    /* load national characters */
			    switch (int2)
			    {
			      case '@' : memcpy (cset[128], chr, sizeof chr);
					 break;             
			      case '[' : memcpy (cset[129], chr, sizeof chr);
					 break;             
			      case '\\': memcpy (cset[130], chr, sizeof chr);
					 break;             
			      case ']' : memcpy (cset[131], chr, sizeof chr);
					 break;             
			      case '{' : memcpy (cset[132], chr, sizeof chr);
					 break;             
			      case '|' : memcpy (cset[133], chr, sizeof chr);
					 break;             
			      case '}' : memcpy (cset[134], chr, sizeof chr);
					 break;             
			      case '~' : memcpy (cset[135], chr, sizeof chr);
			    }
			  }
			  i--;
			  continue;

		/* previous character */
		case DC2: if (int2 > 0   && int2 <  8 ||
			      int2 > ' ' && int2 <= 0x9F) int2--;
			  else if (int2 == ' ') int2 = 7;
	      		  hexconv (1, (long)int2, 2, p);	
			  break;
	
		default : int2 = hexconv(0, (long)int2, 2, p);
	      }
	      i -= 3;
    } 	
  }
}


static xabort (mode, drive)
  char mode, drive;
{
  bios   (SELDSK, drive);
  system (21, 0);
  system (24, 0);
  _exit  (mode);
}


/* font editor */
static char cfont (ctrl, lines, buffer)
  char ctrl, lines, *buffer;
{
  	 char line;			/* current line	       */
  	 char column;			/* current column      */	
  static char zeroes[16];		/* empty font	       */	
	 char code;			/* code of scanned key */
	 BOOL ins    = FALSE;		/* insert mode on/off  */
	 BOOL escape = FALSE;		/* escape character ?  */
 	 char keycode ();		/* returns code of 
					   pressed key	       */


  /* window initialization string */
  static char init[] = "\33F'\33\f\33I%\33J \33K?\33LN\33S\33  \33O\36";

  /* cursor positioning string */
  static char setcur[] = "\33=  ";


  /* Berechnen der letzten Zeile des Fensters */
  init[10] = '%' + lines;

  puts (init);

  switch (ctrl)
  {
    /* clear font */
    case 0: putchar (SUB);
	    system  (20, 0x00, 0, zeroes);
	    return keycode(NULL);

    /* display (and edit font */
    case 3:
    case 4: for (line = 0; line <= lines; line++)
	    {
    	      for (column = 0; column < 8; column++)
      	      if (buffer[line] >> column & 1) puts ("\240\240"); 
					 else puts ("  ");
	      if (line < lines) putchar ('\n');
  	    }

  	    /* display special character */
  	    system (20, 0x00, 0, buffer);
	    
	    if (ctrl == 3) return keycode(NULL);
	    	
    	    line = column = 0;
    	    for (;;)
    	    {
      	      setcur[2] = line       + ' ';
      	      setcur[3] = column * 2 + ' ';
      	      puts (setcur);
      	      if (buffer[line] >> column & 1) puts ("\33R\201\202\33S");
			   		 else puts ("\201\202");
      	      puts ("\b\b");

      	      /* read keyboard */
      	      while ((code = bdos(6, 0xFD)) == DC1 || code == SI)
		escape = TRUE;

      	      if (buffer[line] >> column & 1) puts ("\240\240");
					 else puts ("  ");
	      puts ("\b\b");

	      if (code < ' ' || escape)
      	      {
		escape = ins = FALSE;
		switch (code)
      		{
	          case EOT: /* RIGHT ARROW */
			    if (column < 7) column++;
			    continue;

	          case ENQ: /* UP ARROW */
			    if (line) line--;
			    continue;

		  case BEL: /* F3 */
			    buffer[line] = 
			      (buffer[line] >> 1 & 0xFF << column)
			    + (buffer[line] & 0xFF >> 8 -  column);
			    puts   ("\33Q\33Q");	
			    system (20, 0x00, 0, buffer);
			    continue;	

	          case DC3: /* LEFT ARROW */
			    if (column) column--;
			    continue;

	          case CR : /* NEW LINE */
			    if (line < lines) line++;
			    column = 0;
			    continue;

		  case SO : /* F2 */
			    memcpy (&buffer[line + 1], &buffer[line], 
				    lines - line);
			    buffer[line] = column = 0; 
			    puts   ("\33V");
			    system (20, 0x00, 0, buffer);
			    continue;

		  case NAK: /* BREAK */
			    break;

		  case SYN: /* F1 */
			    ins = TRUE;
			    continue; 

	          case CAN: /* DOWN ARROW */
			    if (line < lines) line++;
			    continue;

		  case EM : /* F5 */
			    memcpy (&buffer[line], &buffer[line + 1], 
				    lines - line);
			    buffer[lines] = column = 0;
			    puts   ("\33W");
			    system (20, 0x00, 0, buffer);
			    continue;	

	          case 'C': /* SHIFT + DOWN ARROW */
			    line = lines;
			    continue;

	          case 'D': /* SHIFT + RIGHT ARROW */
			    column = 7;
			    continue;

		  case 'H': /* F7 */
			    for (line = 0; line <= lines; line++) 
			    {
			      buffer[line] = 
			        (buffer[line] >> 1 & 0xFF << column)
			      + (buffer[line] & 0xFF >> 8 -  column);
      	      		      setcur[2] = line + ' ';
      	      		      puts (setcur);
			      puts ("\33Q\33Q");
			    }	
			    system (20, 0x00, 0, buffer);
			    line = 0;	
			    continue;	

		  case 'J': /* F8 */
			    for (line = 0; line <= lines; line++)
			    {
		              buffer[line] = 
				(buffer[line] << 1 & 0xFF << column + 1)
			      + (buffer[line]      & 0xFF >> 8 - column);
      	      		      setcur[2] = line + ' ';
      	      		      puts (setcur);
		    	      puts ("\33P\33P");
			    }
			    system (20, 0x00, 0, buffer);
			    line = 0;	
			    continue;	

	          case 'R': /* SHIFT + UP ARROW */
			    line = 0;
			    continue;

	          case 'S': /* SHIFT + LEFT ARROW */
			    column = 0;
			    continue;

		  case 'Y': /* CLEAR */
			    buffer[line] &= 0xFF >> 8 - column;
			    putchar (CAN);
			    system  (20, 0x00, 0, buffer);
			    continue;

		  case DEL: /* SHIFT + CLEAR */
			    memset  (buffer, 0, lines + 1);
			    putchar (SUB);
			    system  (20, 0x00, 0, buffer);
			    line = column = 0;

		  default : continue;
        	}
		break;
      	      }

	      if ((code = toupper(code)) == exit || code == store) break;
	      else 
	      {	
		if (code == ' ' || code == '.')
		{
		  /* INS mode ? */
		  if (ins)
		  {
		    buffer[line] = (buffer[line] << 1 & 0xFF << column + 1)
			   	 + (buffer[line]      & 0xFF >> 8 - column);
		    puts ("\33P\33P");
		  }	

		  if (code == ' ')
		  {
		    buffer[line] &= ~(1 << column);
		    puts ("  ");
		  }
		  else
		  {
		    buffer[line] |= 1 << column;
		    puts ("\240\240");
		  }				
		  system (20, 0x00, 0, buffer);
		  if (column < 7) column++;
		}
		continue;
              }
    	    }
	    return keycode(code);	
  } 
}


static char keycode (code)
  char code;
{
  /* restore original screen status */
  puts ("\33I \33J7\33K \33Lo\33N\33F ");

  return code;
}
