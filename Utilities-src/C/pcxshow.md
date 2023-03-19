# pcxshow.c

This is a C program that reads and displays a graphics file in the ".pcx" format, a raster image file format developed by ZSoft Corporation. The program is designed to run on the CP/M operating system and has been adapted to various machines, including the Club-80 Terminal and Genie IIIs.

The program starts by defining some constants, including the maximum dimensions of the image file, a flag for inverse reading of the image, and offsets for the x and y coordinates of the image.

Next, the program defines a structure for storing information about the PCX header, including data such as the manufacturer ID, version number, resolution, and color palette. It then defines a function for decoding an encoded line of the PCX file and another function for reading the PCX header into the defined structure.

The main function of the program then opens the input file and reads the PCX header into the defined structure. It then sets up the display environment and loops through the image file, decoding and displaying each line of the image in the appropriate graphics mode for the display. Finally, the program closes the input file and exits.

``` c
 
/* ------------------------------------------------------------------- */
/* dispcx.c   -   Read & Display a *.PCX graphics file in Microsoft's  */
/* Quick C in EGA and in Borland's Turbo C 2.0 and equivilents with    */
/* all supported BGI graphics modes (VGA/EGA/CGA/Herc).                */
/* ------------------------------------------------------------------- */
/* Original by                      :  M. CLYNES   07-24-89            */
/* Borland Compiler Enhancements by :  J. Braatz   02-16-92            */
/* Adapted to Club-80 Terminal by   :  A. Schmid   06-07-93            */
/* adapted to Genie IIIs by     :  E. Schroeer 18-11-93                */
/* ------------------------------------------------------------------- */
/* WARNING Only with LIBZ and HOLTE !!!                                */

#include <stdio.h>
#include <conio.h>
#include <grafik.h>

#define YMAX	 449	/* Genie IIIs xmax=639,ymax=449 */
#define XMAX     639

char inversfl='N';
int  xoff=0,yoff=0;
int  eof_flag=0;

struct pcxheader{
       char manufact;	  /*  0    Hersteller-ID           */
       char version;	  /*  1    Versionsnummer          */
       char encode;       /*  2                            */
       char bpp;          /*  3                            */
       int xmin;          /*  4, 5 Bildgroesse             */
       int ymin;          /*  6, 7      "                  */
       int xmax;          /*  8, 9      "                  */
       int ymax;          /* 10,11      "                  */
       int hres;          /* 12,13 Aufloesung der Hardware */
       int vres;          /* 14,15     "       "      "    */
       char colormap[48]; /* 16-63                         */
       char reserved;     /* 64    momentan reserviert     */
       char ebene;        /* 65    Zahl der Farbebenen     */
       int bpline;        /* 66,67 Byte pro Zeile je Ebene */
       int paletinfo;     /* 68,69 Art des Bildes          */
       char blank[58];    /* derzeit keine Informationen   */
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
      if(EOF == (data = getc(fp))) return(EOF);  /* get real data */
      }

    while(cnt--){                         /* expand data into buffer */
      if(data==0x1A) eof_flag=1; else eof_flag=0;
      if(eof_flag && data==0x1A) data=0;
      *decodebuff = data;
      ++decodebuff;
      --bytesline;
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

  for(count = 0; count < 128; ++count){
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
  printf("# bytes per/scan line %4d   \n\n",header.bpline);
  printf("Bild (i)nvers oder (n)ormal einlesen ? ");
  inversfl=getch() & 0x5f;

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
int line, bytes;

{
  unsigned x;
  unsigned char byte, loop;

  bytes = (bytes * 8);
  
/* Sehr grosses Bild in X-Richtung komprimieren */

  if (header.xmax-header.xmin+xoff>XMAX*2){   
    for(x = xoff; x < (bytes/2)+xoff; ++rdbuff){
      if (inversfl=='I')
        byte = ~(*rdbuff);
      else
        byte = *rdbuff;
      for (loop = 0x80; loop > 0; loop >>= 1){
        if (loop & byte) point(x, YMAX-line);
        loop >>= 1;
        if (loop & byte) point(x, YMAX-line);
        ++x;
        }
      }
    }
  else{
    for(x = xoff; x < bytes+xoff; ++rdbuff){
      if (inversfl=='I')
        byte = ~(*rdbuff);
      else
        byte = *rdbuff;
      for (loop = 0x80; loop > 0; loop >>= 1){
        if (loop & byte) point(x, YMAX-line);
        ++x;
        }
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
  char decodebuff[200],ch;
  FILE *fp;
  
  z3vinit();

  argc = argc;                          /* Nuke's Compiler warning   */
  
  cls();
   
  if((fp = fopen(argv[1],"rb")) == NULL){
    printf("ERROR: FILE '%s' CANNOT BE OPENED.\n",argv[2]);
    exit(0);
    }
  bytesline = readheader(fp);
  printf("\n\nOffset aus dem Header nehmen ?");
  ch=getch() & 0xDF;
  if((ch=='J') || (ch=='Y') || (ch==0x0D)){
    xoff=header.xmin;
    yoff=header.ymin;
    }
  else{
    printf("\n  X-Offset: "); scanf("%d",&xoff);
    printf("\n  Y-Offset: "); scanf("%d",&yoff);
    }
  line = yoff;
  status = 0;
  
  printf("\n\nBildschirm vor dem Laden loeschen ? ");
  if((ch=getch() & 0xDF)=='J') erase();
  openpl();
  cls();
                          
  while(status != EOF){    /* do EXTRA READS to srink vertical */
    if ((header.ymax-header.ymin+yoff)>YMAX*2){     /* sehr grosses Bild */
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);

      pixeldump(decodebuff, bytesline, line);
      ++line;
      }
    else if (header.ymax-header.ymin+yoff>YMAX){    /* grosses Bild */
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      status = decodepcx(decodebuff, bytesline, fp);
      pixeldump(decodebuff, bytesline, line);
      ++line;
      }
    else  	/* Bild laesst sich mit Aufloesung darstellen */  
      status = decodepcx(decodebuff, bytesline, fp);
      pixeldump(decodebuff, bytesline, line);
      ++line;
      pixeldump(decodebuff, bytesline, line);
      ++line;
    }
    gotoxy(24,25);
    fprintf(stderr,"PRESS 'ENTER' to continue");
    getch();
    closepl();
  }
/* End of main() */
,,,
