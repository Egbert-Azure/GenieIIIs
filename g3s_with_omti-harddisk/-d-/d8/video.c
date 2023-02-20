/*          VIDEO.C    VIDEO.C    VIDEO.C    VIDEO.C    VIDEO.C

VIDEO OUTPUT ROUTINES    X   X  XXX  XXXX   XXXXX   XXX
                         X   X   X   X   X  X      X   X
May 27, 1986             X   X   X   X   X  X      X   X
                          X X    X   X   X  XXX    X   X
                          X X    X   X   X  X      X   X
                           X     X   X   X  X      X   X
                           X    XXX  XXXX   XXXXX   XXX
*/
#include "stdio.h"               /* The standard header information */
#include "dos.h"                 /* The DOS register definitions    */
#include "struct.def"            /* The data structures for VC.C    */
char outlist[160];
char strngout[80];
extern struct vars allvars[12];
int bkattr;                      /* background attribute (boxes)    */
int valattr;                     /* values and variables attribute  */
int trnsattr;                    /* transcript attribute            */
int helpattr;                    /* help attribute                  */
int errattr;                     /* error message attribute         */
int vidstart;                    /* start of video memory           */

/* ******************************************************** monitor */
/* This function looks at location 0449(hex) in memory to determine */
/* what type of monitor is being used and adjusts the video attri-  */
/* butes accordingly. This allows for flexibility.                  */
monitor()
{
char i[2];
struct SREGS segregs;
unsigned int ds;

   segread(&segregs);
   ds = segregs.ds;
   movedata(0,0x449,ds,i,1);     /* get monitor mode from 0000:0449 */
   if (i[0] == 7) {                    /* monochrome monitor in use */
      bkattr = 7;
      valattr = 7;
      trnsattr = 7;
      helpattr = 7;
      errattr = 7;
      vidstart = 0XB000;
   }
   else {                                   /* color monitor in use */
      bkattr = 15;                                  /* bright white */
      valattr = 10;                                  /* light green */
      trnsattr = 14;                                      /* yellow */
      helpattr = 10;                                 /* light green */
      errattr = 128 + 12;                     /* blinking light red */
      vidstart = 0XB800;
   }
}

/* ******************************************************* bkgndvid */
/* This routine sets up the data for the double lines used through- */
/* out the background. The codes for the double lines come from the */
/* extended ASCII set. they are actually output to the display by   */
/* the routine "linedisp()".                                        */ 
bkgndvid() 
{
int index;
   for (index = 0;index < 160;++index)
      outlist[index] = bkattr;
   for (index = 2;index <= 156;index += 2)
        outlist[index] = 205;
   outlist[0] = 201;
   outlist[44] = 203;
   outlist[110] = 203;
   outlist[158] = 187;
   linedisp(1);                                     /* video line 1 */
   outlist[0] = 204;
   outlist[44] = 202;
   outlist[110] = 202;
   outlist[158] = 185;
   linedisp(8);                                     /* video line 8 */
   outlist[44] = 205;
   outlist[110] = 205;
   linedisp(23);                                   /* video line 23 */
   outlist[0] = 200;
   outlist[158] = 188;
   linedisp(25);                                   /* video line 25 */
   for (index = 2;index <= 156;index += 2)
      outlist[index] = 32;
   outlist[0] = 186;
   outlist[158] = 186;
   linedisp(24);                                   /* video line 24 */
   for (index = 9;index <= 22;++index)
      linedisp(index);                       /* video lines 9 to 22 */
   outlist[44] = 186;
   outlist[110] = 186;
   for (index = 2;index <= 7;++index)
      linedisp(index);                        /* video lines 2 to 7 */
}

/* ******************************************************* valusvid */
/* This routine actually outputs the calculated data to the monitor.*/
/* It outputs all values every time it is called, even if only a few*/
/* values are changed. It is therefore somewhat inefficient.        */
valusvid()
{
long int temp;
   sprintf(strngout,"     A = %12.6f",allvars[0].value);
   strngout[21] = 0;
   strngdis(1,1,valattr);
   sprintf(strngout,"     B = %12.6f",allvars[1].value);
   strngout[21] = 0;
   strngdis(2,1,valattr);
   sprintf(strngout,"     C = %12.6f",allvars[2].value);
   strngout[21] = 0;
   strngdis(3,1,valattr);
   sprintf(strngout,"     D = %12.6f",allvars[3].value);
   strngout[21] = 0;
   strngdis(4,1,valattr);
   sprintf(strngout,"     E = %12.6f",allvars[4].value);
   strngout[21] = 0;
   strngdis(5,1,valattr);
   sprintf(strngout,"     F = %12.6f",allvars[5].value);
   strngout[21] = 0;
   strngdis(6,1,valattr);

   temp = allvars[6].value;
   temp = temp & 077777777;
   allvars[6].value = temp;
   sprintf(strngout,"I = %8ld = %8lo = %6lx",temp,temp,temp);
   strngdis(1,23,valattr);
   temp = allvars[7].value;
   temp = temp & 077777777;
   allvars[7].value = temp;
   sprintf(strngout,"J = %8ld = %8lo = %6lx",temp,temp,temp);
   strngdis(2,23,valattr);
   temp = allvars[8].value;
   temp = temp & 077777777;
   allvars[8].value = temp;
   sprintf(strngout,"K = %8ld = %8lo = %6lx",temp,temp,temp);
   strngdis(3,23,valattr);
   temp = allvars[9].value;
   temp = temp & 077777777;
   allvars[9].value = temp;
   sprintf(strngout,"L = %8ld = %8lo = %6lx",temp,temp,temp);
   strngdis(4,23,valattr);
   temp = allvars[10].value;
   temp = temp & 077777777;
   allvars[10].value = temp;
   sprintf(strngout,"M = %8ld = %8lo = %6lx",temp,temp,temp);
   strngdis(5,23,valattr);
   temp = allvars[11].value; 
   temp = temp & 077777777;
   allvars[11].value = temp;
   sprintf(strngout,"N = %8ld = %8lo = %6lx",temp,temp,temp);
   strngdis(6,23,valattr);

   strcpy(strngout,"Function Keys --------");
   strngdis(1,56,helpattr);
   strcpy(strngout,"F1-Help-M   F2-Help-S  ");
   strngdis(2,56,helpattr);
   strcpy(strngout,"F3-Print    F4-Mark    ");
   strngdis(3,56,helpattr);
   strcpy(strngout,"F5-Store    F6-Retrieve");
   strngdis(4,56,helpattr);
   strcpy(strngout,"F7-         F8-        ");
   strngdis(5,56,helpattr);
   strcpy(strngout,"F9-Edit     F10-Quit   ");
   strngdis(6,56,helpattr);
}

/* ********************************************************* disnew */
/* This routine displays the changed variable only.                 */
disnew(varinuse)
int varinuse;
{
long int temp;
double xx;

   if (varinuse < 6) {                       /* display A through F */
      xx = allvars[varinuse].value;
      if (xx < 0.0) xx = -xx;
      if ((xx> 9999999.0) || (xx < .001))
         sprintf(strngout,"%12.5e",allvars[varinuse].value);
      else
         sprintf(strngout,"%12.6f",allvars[varinuse].value);
      strngout[12] = 0;
      strngdis(1+varinuse,10,valattr);
   }
   else {                                    /* display I through N */
      temp = allvars[varinuse].value;
      temp = temp & 077777777;
      allvars[varinuse].value = temp;
      sprintf(strngout,"%8ld = %8lo = %6lx",temp,temp,temp);
      strngdis(varinuse-5,27,valattr);
   }
}

/* ********************************************************** helpm */
/* This outputs the math helps to the monitor.                      */
helpm()
{
strtrans(" ",0);
strtrans("$                Help - Mathematics",0);
strtrans("$   All calculations are done in floating point, then",0);
strtrans("$  converted to fixed point for variables I to N.",0);
strtrans("$     Available    ABS()   SQRT()  EXP()   LOG()",0);
strtrans("$     Functions    SIN()   COS()   ATAN()  FACT()",0);
strtrans("$   Nesting is allowable to any depth, but the line",0);
strtrans("$  length is limited to 62 characters.",0);
}

/* ********************************************************** helps */
/* This outputs the system helps to the monitor.                    */
helps()
{
strtrans(" ",0);
strtrans("$                   Help - System",0);
strtrans("$  Arrow - selected line   ;   Star - marked line",0);
strtrans("$  F3-Toggle print mode to print all input lines",0);
strtrans("$  F4-Toggle the mark indicator on selected line",0);
strtrans("$  F5-Store all marked lines to a file",0);
strtrans("$  F6-Retrieve a file and calculate while inputting",0);
strtrans("$  F9-Load selected line into input window",0);
strtrans("$  up/down-Move selector up/down 1 line",0);
strtrans("$  Pgup/Pgdn-Move selector up/down 8 lines",0);
}

/* ******************************************************* linedisp */
/* This outputs a complete line with attributes already in place.   */
linedisp(line)
int line;
{
struct SREGS segregs;
unsigned int ds;

   segread(&segregs);
   ds = segregs.ds;
   movedata(ds,outlist,vidstart,160*(line-1),160);
}

/* ******************************************************* strngdis */
/* This outputs a part of a line to the monitor, but first it adds  */
/* the attribute bytes between each of the character bytes.         */
strngdis(row,col,attr)
int row,col;
int attr;
{
int i = 0;
int j = 0;
struct SREGS segregs;
unsigned int ds;

   segread(&segregs);
   ds = segregs.ds;

   while (strngout[i] && j <= 160) {
      outlist[j] = strngout[i];
      outlist[j+1] = attr;
      j += 2;
      i++;
   }
   movedata(ds,outlist,vidstart,160*row + 2*col,j);
}

/* ******************************************************* blnkline */
/* This routine outputs blanks from here to column 79.              */
blnkline(row,col)
int row,col;
{
int i,j,number;
struct SREGS segregs;
unsigned int ds;

   segread(&segregs);
   ds = segregs.ds;
   number = 78 - col;
   j = 0;
   for (i = 0;i <= number;++i){
      outlist[j] = ' ';
      outlist[j+1] = trnsattr;
      j += 2;
   }   
  movedata(ds,outlist,vidstart,160*row + 2*col,2*number);
}

/* ******************************************************** chardis */
/* This function outputs one character anywhere on the screen.      */
chardis(row,col,attr,ch)
int row,col,attr;
int ch;
{
struct SREGS segregs;
unsigned int ds;

   segread(&segregs);
   ds = segregs.ds;
   outlist[0] = ch;
   outlist[1] = attr;
   movedata(ds,outlist,vidstart,160*row + 2*col,2);
}

/* ********************************************************* errdis */
/* This function displays the error message with the blinking       */
/* attribute.                                                       */ 
errdis(str)
char str[];
{
int row = 21,col = 50;
int i;
struct SREGS segregs;
unsigned int ds;

   segread(&segregs);
   ds = segregs.ds;
   for (i = 0;i <= 24;++i) {
      outlist[2*i] = str[i];
      outlist[2*i+1] = errattr;
   }
   movedata(ds,outlist,vidstart,160*row + 2*col,50);
}

/* ******************************************************** clrscrn */
/* This function clears the screen.                                 */
clrscrn()
{
int row,col;

   for (row = 0;row < 25;++row)
   for (col = 0;col < 80;++col)
      chardis(row,col,7,' ');
}

/* ******************************************************** poscurs */
/* This function positions the cursor anywhere on the screen. It    */
/* calls the DOS function call 10, the video interrupt.             */
poscurs(row,col)
int row,col;
{
int flags;
union REGS inregs;
union REGS outregs;

   inregs.h.ah = 2;
   inregs.h.dh = row;
   inregs.h.dl = col;
   inregs.h.bh = 0;
   flags = int86(0x10,&inregs,&outregs);
}

/* ******************************************************* prtprblm */
/* This function checks the printer to see if it is turned on and on*/
/* line. It returns a 1 if a problem, 0 if all is OK.               */
prtprblm()
{
union REGS inregs;
union REGS outregs;

   inregs.h.ah = 2;
   inregs.x.dx = 0;
   int86(0x17,&inregs,&outregs);
   if ((outregs.h.ah & 0X80) == 0X80)
      return(0);
   else
      return(1);
}
