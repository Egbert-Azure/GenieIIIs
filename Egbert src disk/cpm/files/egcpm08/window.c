/******************************************************************************
*  W I N D O W  *  U L I B 0 0 3  *  T h o m a s   H o l t e  *  8 5 0 3 2 8  *
*******************************************************************************
*
* Version 3.2 by Thomas Holte
*
*/

#include <stdio.h>


char window (p)
  struct {
           int  CTRL;                  /* control code             */
           char PROMPT;                /* prompt character         */
           char ATTRIB;                /* console attributes       */
           int  BEG;                   /* upper left window corner */
           int  END;                   /* lower left window corner */
           char *BUF;                  /* buffer pointer           */
         } *p;
{
/* This function lays a window of any size over the screen. You can input or
   output data from or to the window. At input all editing keys can be used. The
   black function keys, the TAB key, or the carriage return key break the input
   session and return their ASCII codes to the calling function.

   CTRL
   Bits 15-8: number of columns/lines at control codes 6-9
   Bit   7  : 0  buf = address of first window position
              1  buf = address of first screen position
   Bit   4  : 0  break input session with function key (usual mode)
              1  automatic break at last cursor position
   Bits  3-0: 0  clear window
              1  clear, input
              2  input
              3  output
              4  edit
              5  read
              6  insert columns (# of columns in upper byte of CTRL)
              7  delete columns (# of columns in upper byte of CTRL)
              8  insert lines   (# of lines   in upper byte of CTRL)
              9  delete lines   (# of lines   in upper byte of CTRL)
             10  colour window  (read window and output it with attributes)
             11  get single keyboard character without echo

   PROMPT  ASCII-code of erase character (used at clear window & del/ins funcs)

   ATTRIB
   Bits 3-2: 0  no attributes
             2  inverse
   Bit  0  : 0  no beep
             1  beep

   BEG  line #/column # (decimal) of first window position (picture 1)
        0 <= line number   <= 23
        0 <= column number <= 79

   END  line #/column # (decimal) of last  window position (picture 1)

   BUF  address of input/output buffer


              |-----------------------------------------|
              | begin                                   |
              |                                         |
              |                                         |
              |              w i n d o w                |
              |                                         |
              |                                         |
              |                                     end |
              |-----------------------------------------|
                                Pic.1


   editing keys at input:
   	   UP    ARROW = cursor up
   	   DOWN  ARROW = cursor down
   	   LEFT  ARROW = cursor left
   	   RIGHT ARROW = cursor right
   SHIFT + UP 	 ARROW = first line
   SHIFT + DOWN  ARROW = last  line 
   SHIFT + LEFT  ARROW = start of line
   SHIFT + RIGHT ARROW = end   of line
   	   RETURN      = new line
           F1          = toggle insert mode
           F2          = insert line
           F3          = delete character
           F5          = delete line
           CLEAR       = erase to end of line
   SHIFT + CLEAR       = clear window
*/


/* ASCII control codes */
#define EOT 0x04 		      /* end of text	  */
#define ENQ 0x05		      /* enquiry	  */	
#define BEL 0x07                      /* bell             */
#define LF  0x0A                      /* line feed        */
#define CR  0x0D		      /* carriage return  */
#define SO  0x0E                      /* shift out        */
#define DC1 0x11		      /* device control 1 */
#define DC3 0x13		      /* device control 3 */
#define SYN 0x16                      /* synchronous idle */
#define CAN 0x18                      /* cancel           */
#define EM  0x19                      /* end of medium    */
#define SUB 0x1A                      /* substitute       */
#define DEL 0x7F                      /* delete           */


	 int  lines;                /* line   count of window        */
	 int  columns;              /* column count of window        */
         int  line;                 /* current window line           */
         int  column;               /* current window column         */
         int  count;                /* line/column count for ins/del */
         char ctrl;                 /* control code                  */
         char code;                 /* scanned key code              */
         char keycode ();           /* exit routine                  */
	 char dummy[2000];	    /* screen buffer (scratch)	     */
         BOOL ins = FALSE;          /* insert mode on/off            */
         BOOL flag;                 /* auto break on/off             */
         BOOL full;                 /* marker for full screen buffer */
	 BOOL attrib;		    /* attributes ?		     */
	 BOOL escape = FALSE;       /* escape sequence ?	     */

  static char init  [] = "\33F'\33\f\33I \33J \33K \33L \33 \33  \33O\36",
	      setcur[] = "\33=  ";


  /* calc first/last line/column and line/column count */
  lines   = (init[10] = ' ' + p->END / 100) 
	  - (init[ 7] = ' ' + p->BEG / 100) + 1;
  columns = (init[16] = ' ' + p->END % 100) 
	  - (init[13] = ' ' + p->BEG % 100) + 1;

  /* calc video attributes */
  init[18] = (attrib = p->ATTRIB >> 3 & 1) ? 'R' : 'S';

  /* get prompt character */
  init[21] = p->PROMPT >= ' ' ? p->PROMPT : ' ';

  puts (init);

  /* full screen buffer ? */
  full = p->CTRL >> 7 & 1;

  /* auto break ? */
  flag = p->CTRL & 0x10;

  /* get control code */
  ctrl = p->CTRL & 0xF;

  /* get line/column count */
  count = p->CTRL >> 8;

  /* beep ? */
  if (p->ATTRIB & 1) putchar (BEL);


  switch (ctrl)
  {
    case  0:
    case  1: /* clear window */
             putchar (SUB);
             if (!ctrl) return keycode(NULL);

    case  2:
    case 11: /* input */
             input:
             line = column = 0;
	     puts ("\33\r");		/* turn on cursor */

             for (;;)
             {
               /* output cursor */
               setcur[2] = ' ' + line;
               setcur[3] = ' ' + column;
               puts (setcur);

               /* read keyboard */
               while ((code = bdos(6, 0xFD)) == DC1) escape = TRUE;

               if (ctrl == 11) return keycode(code);

               if (code < ' ' || escape)
               {
                 escape = ins = FALSE;
                 switch (code)
                 {
                   case EOT: /* RIGHT ARROW */
                             if (++column == columns)
                             {
			       column--;	
                               if (line == lines - 1) goto read;
			     }	
                             continue;

                   case ENQ: /* UP ARROW */
                             if (line) line--; else goto read;
                             continue;

                   case BEL: /* F3 */
			     puts ("\33Q");
                             continue;

                   case DC3: /* LEFT ARROW */
                             if (column) column--; else if (!line) goto read;
                             continue;

                   case CR : /* NEW LINE */
                             if (++line == lines) goto read;
                             column = 0;
                             continue;

                   case SO : /* F2 */
			     puts ("\33V");
                             column = 0;
                             continue;

                   case SYN: /* F1 */
                             ins = TRUE;
                             continue;

                   case CAN: /* DOWN ARROW */
                             if (++line == lines) goto read;
                             continue;

                   case EM : /* F5 */
			     puts ("\33W");
                             column = 0;
                             continue;

                   case 'C': /* SHIFT + DOWN ARROW */
                             line = lines - 1;
                             continue;

                   case 'D': /* SHIFT + RIGHT ARROW */
                             column = columns - 1;
                             continue;


                   case 'R': /* SHIFT + UP ARROW */
                             line = 0;
                             continue;

                   case 'S': /* SHIFT + LEFT ARROW */
                             column = 0;
                             continue;

                   case 'Y': /* CLEAR */
                             putchar (CAN);
                             continue;

		   case DEL: /* SHIFT + CLEAR */
                             putchar (SUB);
			     line = column = 0;
			     continue;	
                 }
		 goto read;
	       }
               else     /* displayable character */
               {
                 /* INS mode ? */
                 if (ins) puts ("\33P");

                 /* display character */
                 putchar (code);

                 if (flag && line == lines - 1 && column == columns - 1)
		   goto read;

                 if (++column == columns) column--;
                 continue;
               }
             }

    case  3:
    case  4: /* display buffer */
             system (22, 1, 0, p->BUF, (full << 8) + attrib); 

             if (ctrl == 4) goto input; else return keycode(NULL);

    case  5: /* transfer data */
             read:
             system (22, 0, 0, p->BUF, full << 8);

             return keycode(code);

    case  6: /* insert columns */
             for (line = 0; line < lines; line++)
             {
	       for (column = 0; column < count; column++) puts ("\33P");
               if (line < lines - 1) putchar (LF);						
             }
             return keycode(NULL);

    case  7: /* delete columns */
             for (line = 0; line < lines; line++)
	     {
	       for (column = 0; column < count; column++) puts ("\33Q");
               if (line < lines - 1) putchar (LF);
	     }
             return keycode(NULL);

    case  8: /* insert lines */
	     for (line = 0; line < count; line++) puts ("\33V");
             return keycode(NULL);

    case  9: /* delete lines */
             for (line = 0; line < count; line++) puts ("\33W");
             return keycode(NULL);

    case 10: /* colour window */
             system (22, 0, 0, dummy,      0);
	     system (22, 1, 0, dummy, attrib);
             return keycode(NULL);
  }
}


static char keycode (code)
  char code;
{
  /* restore original screen status */
  puts ("\33I \33J7\33K \33Lo\33N\33F ");

  return code;
}
