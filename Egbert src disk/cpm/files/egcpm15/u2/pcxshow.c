
/* ------------------------------------------------------------------- */
/* dispcx.c   -   Read & Display a *.PCX graphics file in Microsoft's  */
/* Quick C in EGA and in Borland's Turbo C 2.0 and equivilents with    */
/* all supported BGI graphics modes (VGA/EGA/CGA/Herc).                */
/* ------------------------------------------------------------------- */
/* Original by                      :  M. CLYNES  07-24-89             */
/* Borland Compiler Enhancements by :  J. Braatz  02-16-92             */
/* Adapted to Club-80 Terminal by   :  A. Schmid  06-07-93             */
/* ------------------------------------------------------------------- */
/* command line format >DISPCX photo1.pcx program set for 1/4          */
/*                                               file size display.    */
/* ------------------------------------------------------------------- */

#include <stdio.h>
#include <grafik.h>

#define HOROFF   10
#define TOPOFF   70
#define YMAX	 449
struct pcxheader{
       char manufact;
       char version;
       char encode;
       char bpp;
       int xmin;
       int ymin;
       int xmax;
       int ymax;
       int hres;
       int vres;
       char colormap[50];
       int bpline;
       int paletinfo;
       char blank[58];
       } header;

/*========================================================================*/
/*   gobal decoded data   char decodebuff[200];                           */
/*========================================================================*/
/* Decode a line of .PCX                                                  */
/* This procedure reads one encoded line from the image file              */
/* 0 = valid data stored                                                  */
/* EOF = out of data in file                                              */

decodepcx(decodebuff, bytesline, fp)
char *decodebuff;                                 /* where to place data */
int bytesline;                                    /* # of bytes per line */
FILE *fp;                                         /*  image file handle  */
{
  int data;
  int cnt;

  while(bytesline > 0 ){
    cnt = 1;
    if(EOF == (data = getc(fp))) return(EOF);   /* retrive a data byte */
    if(0xc0 == (0xc0 & data)){
      cnt = 0x3f & data;                        /* get repeat count */
      if(EOF == (data = getc(fp))) return(EOF);    /* get real data */
      }

    while(cnt){                         /* expand data into buffer */
      *decodebuff = data;
      ++decodebuff;
      --bytesline;
      --cnt;
      }
    }
  return(0);                                  /* file read status */
  }
/* decodepcx()  end */


/*========================================================================*/
/*                   read .PCX header into structure                      */
/*========================================================================*/
readheader(fp)
FILE *fp;
{
  int count;
  char *loader;

  loader = (char *) &header;

  for(count = 0; count < 127; count++){
     *loader = (char)fgetc(fp);
     ++loader;
     }

/* Note: For Borland Compiler users, the following will need to be     */
/* changed to outtext() if you want to see the header information on   */
/* Hercules monitors (with the old herc.bgi driver)                    */

  printf("Encoding type %d\n",header.encode);
  printf("Window X-min = %4d,  Y-min = %4d\n",header.xmin, header.ymin);
  printf("Window X-max = %4d,  Y-max = %4d\n",header.xmax, header.ymax);
  printf("Hres = %4d,  Vres = %4d\n",header.hres, header.vres);
  printf("# bytes per/scan line %4d   ",header.bpline);
  printf("\n\nPress ENTER\n");
  getch();

  return(header.bpline);
}

/*========================================================================*/
/*                      pixel line dump                                   */
/*========================================================================*/
/* DUMP A RASTER LINE TO DISPLAY                                          */
/* pixeldump((pointer buffer), (# of BYTES), (Y line on screen to dump))  */
/*========================================================================*/

pixeldump(rdbuff, bytes, line)
char *rdbuff;
int bytes, line;
{
  unsigned x;
  unsigned char byte, loop;

  bytes = (bytes * 8) + HOROFF;

  for(x = HOROFF; x < bytes; rdbuff++){
    byte = *rdbuff;              /*  >>= 2 srink HORZ.  or >>= 1 */
    for (loop = 0x80; loop > 0; loop >>= 1){
      if (loop & byte) point(x, YMAX-line);
      ++x;
      }
    }
  }
/* pixeldump() End */


/*========================== MAIN() =====================================*/

int main( argc, argv )
int argc;
char *argv[2];
{
  int line, status, bytesline;
  char decodebuff[200];
  FILE *fp;

  argc = argc;                          /* Nuke's Compiler warning   */

  if((fp = fopen(argv[1],"rb")) == NULL){
    printf("ERROR: FILE '%s' CANNOT BE OPENED.\n",argv[2]);
    exit(0);
    }
  bytesline = readheader(fp);
  line = TOPOFF;
  status = 0;
  putchar(0x1a);
  openpl();
                        
  while(status != EOF){    /* do EXTRA READS to srink vertical */
    /* status = decodepcx(decodebuff, bytesline, fp); */
    /* status = decodepcx(decodebuff, bytesline, fp); */
    status = decodepcx(decodebuff, bytesline, fp);
    pixeldump(decodebuff, bytesline, line);
    ++line;
    }

    printf("PRESS 'ENTER' to contimue");
    getch();
    closepl();
  }
/* End of main() */

