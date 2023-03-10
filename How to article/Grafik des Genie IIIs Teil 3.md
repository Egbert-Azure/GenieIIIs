
# Grafik des Genie IIIs unter Holte CP/M+ Teil 3 #

## Volker Dose, Egbert Schröer, Juni-August 1994 ##

## 1. Grundlagen

Der zweite Beitrag zur kleinen Artikelserie über die Ansteuerung der
Grafik des Genie IIIs unter CP/M zeigte die Möglichkeiten mit Turbo
Pascal. Hier sind nun Möglichkeiten zur Programmierung in ANSI-C.

Thomas Holte selbst hat viele Utilities in C programmiert, nutzte
dazu aber den MIC-Compiler von Rose. Dieser ist zwar für die  
unterschiedlichsten Rechner noch zu kaufen, allerdings lässt Herr Rose mit
sich nicht über den Preis reden und so bleibt dieser C-Compiler mit
ca. 400,- DM in der heutigen CP/M Welt unbeachtet; schade.  Holte hat
mit der ]Überlassung der Source Codes des Genie 3s für den  Club  80
auch die C-Routinen zur Graphik-Ansteuerung zur Verfügung gestellt.
Deshalb  fällt die Entscheidung neue Wege mit dem jetzt  PD-Compiler
von Hitech zu gehen leicht, zumal dieser den ANSI Standard unterstützt.

Die erste Arbeit, die man nun erledigen muss, ist  die  Erstellung
einer Library f}r den C-Compiler aus den Holte'schen MIC  Routinen.
Die eigentliche Graphikfunktionalitaet ist ja im BIOS  implementiert.
Dem C-Compiler wird halt nur in den Funktionen gesagt, welche BIOS-
Funktion zu nutzen ist und wie. Dazu einige Beispiele:

``` c
/******************************************************************************

* C L O S E P L *L I B P L O T 0 1 5* T h o m a s   H o l t e *8 6 0 4 0 1*

*******************************************************************************

*
* Version 1.0 by Thomas Holte.
*
* Closes graphic screen.
*/

closepl ()
{
  system  (25, 0);  /*disable output on graphic screen of phoenix Genie IIIs*/
}

/******************************************************************************

* P O I N T  *L I B P L O T 0 3 5* T h o m a s   H o l t e *8 6 0 4 0 1*

*******************************************************************************

*
* Version 1.0 by Thomas Holte.
*
* Plots point (x,y).
*/

point (x, y)
  int x, y;
{
  system (27, 0x11, 0, y, x);   /*plots point on phoenix Genie IIIs*/
}

/******************************************************************************

* L I N E  *L I B P L O T 0 2 5*  T h o m a s   H o l t e  *8 6 0 4 0 1*

*******************************************************************************

*
* Version 1.0 by Thomas Holte.
*
* Draws a line from (x1,y1) to (x2,y2).
*/

line (x1, y1, x2, y2)
  int x1, y1, x2, y2;
{
  /*common line drawing routine*/
/*
  int dx, dy, px, py;
  int x = x1;
  int y = y1;
  int p = 0;

  if (x2 >= x1)
  {
    px = x2 - x1 + 1;
    dx = 1;
  }
  else
  {
    px = x1 - x2 + 1;
    dx = -1;
  }

  if (y2 >= y1)
  {
    py = y2 - y1 + 1;
    dy = 1;
  }
  else
  {
    py = y1 - y2 + 1;
    dy = -1;
  }

  point (x, y); /*wurde ja in POINT.C definiert !*/

  if (py <= px)
  {
    while (x != x2)
    {
      if ((p += py) >= px)
      {
        p -= px;
        y += dy;
      }

      point (x += dx, y);
    }
  }
  else
  {
    while (y != y2)
    {
      if ((p += px) >= py)
      {
        p -= py;
        x += dx;
      }

      point (x, y += dy);
    }
  }
*/

  /*line drawing routine on phoenix Genie IIIs*/
  system (29, 0x11, 0, &x1);
}
/*=====================================================================*/
```

Diese Library mit Namen HOLTE.LIB enthält die volle Grafikfunktionalitaet
des Genie 3s - selbstverständlich mit den Windows Optionen -
und ist auf einer der V0xx Disk's (für Z3PLUS User) oder direkt bei
uns erhältlich.

``` c
/*****************************************************************************/
/*Funktionsprototypen der Grafiklibrary f}r Genie IIIs  mit  Holte*/
/*CP/M +.                                             HOLTE.LIB-File*/
/*****************************************************************************/
/*Volker Dose, Egbert Schroeer*/
/*function window not included*/

void erase (void);                      /*loescht HRG Seite 0*/
void openpl (void);                     /*loescht HRG Seite 0*/
                                        /*und schaltet an*/
void closepl (void);                    /*schaltet HRG Seite 0 aus*/
void point (int x, int y);              /*setzt einen Punkt (639,449)*/

/*zieht Linie (links,unten,rechts,oben)*/
void line (int x1, int y1, int x2, int y2);

/*Kreisbogen (mittel_x,y,start_x,y,end_x,y*/
void arc (int x, int y, int x0, int y0, int x1, int y1);

void circle (int x, int y, int r);      /*Kreis (mittel_x,y,radius)*/
void move (int x, int y);               /*Grafikcursor nach (x,y)*/
void cont (int x, int y);               /*draw line from (X,Y) to (x,y)*/
void unpoint (int x, int y);            /*loesche einen Punkt (639,449)*/

void unline (int x1, int y1, int x2, int y2);        /*loescht eine Line*/

/*loescht einen Kreis mittelpunkt_x,y radius*/
void uncircle (int x, int y, int r);

/*loescht einen Kreisbogen mittelpunkt_y,y radius*/
void unarc (int x, int y, int x0, int y0, int x1, int y1);

void uncont (int x, int y);             /*undraw line from (X,Y) to (x,y)*/
/*=====================================================================*/
```

> Note: Natürlich wird auch - wie bei der Programmierung unter Turbo Pascal eine Möglichkeit gebraucht, um über den BIOS-Aufruf über  BDOS-
Funktion 50 zu realisieren. BIOS3.C befindet sich in der HOLTE.LIB,
BIOS3.H   wird als Header File in das entsprechende Programm eingebunden
werden (s.a. Disk V057). Zusätzlich sind die 'system()' Funktionen
der original Holte Library eingebunden, um die Spezialfunktionen des
BIOS nutzen zu können.

``` c
/*BIOS-Aufruf fuer CP/M Plus ueber die BDOS-Funktion 50*/

/*CP/M-80 BIOS entry points*/
# define BOOT    0               /*perform cold start initialization*/
# define WBOOT   1               /*perform warm start initialization*/
# define CONST   2               /*check for console input char ready*/
# define CONIN   3               /*read  console character in*/
# define CONOUT  4               /*write console character out*/
# define LIST    5               /*write list    character out*/
# define AUXOUT  6               /*write auxiliary output character*/
# define AUXIN   7               /*read  auxiliary input  character*/
# define HOME    8               /*move to track 00 on selected disk*/
# define SELDSK  9               /*select disk drive*/
# define SETTRK 10               /*set track  number*/
# define SETSEC 11               /*set sector number*/
# define SETDMA 12               /*set DMA address*/
# define READ   13               /*read  specifidd sector*/
# define WRITE  14               /*write specified sector*/
# define LISTST 15               /*return list status*/
# define SECTRN 16               /*translate logical to physical sector*/
# define CONOST 17               /*return output status of console*/
# define AUXIST 18               /*return input  status of aux. port*/
# define AUXOST 19               /*return output status of aux. port*/
# define DEVTBL 20               /*return address of char. I/O table*/
# define DEVINI 21               /*initialize char. I/O devices*/
# define DRVTBL 22               /*return address of disk drive table*/
# define MULTIO 23               /*set number of logically consecutive*/
                                /*sectors to be read or written*/
# define FLUSH  24               /*force physical buffer flushing for*/
                                /*user-supported deblocking*/
# define MOVE   25               /*memory to memory move*/
# define TIME   26               /*time set/get signal*/
# define SELMEM 27               /*select bank of memory*/
# define SETBNK 28               /*specify bank for DMA operation*/
# define XMOVE  29               /*set bank when a buffer is in a bank*/
                                /*other than 0 or 1*/
# define USERF  30               /*reserved for system implementor*/

# ifndef uchar
# define uchar unsigned char
# endif

/*function prototypes*/
unsigned bios(uchar nummer, ...);
unsigned bioshl(uchar nummer, ...);
/*=====================================================================*/
```

> Note : Um kein Plagiat zu begehen: An BIOS3.* war Alexander Schmid  nicht
unbeteiligt.

## 2. Programmierung der Grafik

Die Voraussetzungen sind nun implementiert. Es folgt ein kleines
Beispielprogramm, das einige Grafikspielereien erzeugt.

Das Programm GRATEST zeigt dabei alle Möglichkeiten der Grafik.  Die
Punkte werden nur normiert gesetzt bzw. gelöscht, das schafft Kompatibilitäten
zwischen Rechnern mit verschiedenen Zeichensätzen (z.B. FONT14 und FONT12)
und Kreise werden immer normiert gezeichnet.

``` c
/* GRATEST.C Beispielprogramm mit den
derzeitigen Grafikmoeglichkeiten*/

# include <stdio.h>
# include <time.h>
# include <grafik.h>

int main(void);
int rand();

int zaehler, radius, x0, y0, x1, y1;
int max_x,max_y,max_int;

# define sub 0x1a

/*draws a box (lower_left to upper_right)*/
void box(int xb0, int yb0, int xb1, int yb1)
{
        line(xb0,yb0,xb1,yb0);          /*links unten nach rechts unten*/
        line(xb1,yb0,xb1,yb1);          /*rechts unten nach rechts oben*/
        line(xb1,yb1,xb0,yb1);          /*rechts oben nach links oben*/
        line(xb0,yb1,xb0,yb0);          /*links oben nach links unten*/
}

/*undraws a box ( lower_left to upper_right*/
void unbox(int xb0, int yb0,int xb1, int yb1)
{
        unline(xb0,yb0,xb1,yb0);        /*links unten nach rechts unten*/
        unline(xb1,yb0,xb1,yb1);        /*rechts unten nach rechts oben*/
        unline(xb1,yb1,xb0,yb1);        /*rechts oben nach links oben*/
        unline(xb0,yb1,xb0,yb0);        /*links oben nach links unten*/
}

int main(void)
{

        x0 = 0; y0 = 0; x1 = 100; y1 = 100;
        putch(sub);
        puts("Hallo Welt, wie geht's denn so ? ");   


        openpl();
        line(x0,y0,x1,y1);
        line(100,100,620,400);  
        circle(300,140,30);
        box(30,30,240,100);
        box(240,100,639,300);
        box(250,110,629,290);
        box(260,120,619,280); 
        box(270,130,609,270);
        
        erase();
        for (radius =5; radius <200; radius +=2)
        circle(320,220,radius);

        for (radius = 5; radius < 200; radius +=2)
        uncircle(320,220,radius);
                

        move (330,280);
        srand (2204);
        for (zaehler = 0; zaehler <120; zaehler ++)
        {
                x0 = rand() / 52;
                y0 = rand() / 80;
                        cont(x0,y0);
        }
        
        move(330,280);
        srand(2204);
        for (zaehler = 0; zaehler <121; zaehler ++)
        {
                x0 = rand() / 52;
                y0 = rand() / 80;
                        uncont(x0,y0);
        }
                
        srand(2204);
        for (zaehler =0; zaehler < 52; zaehler ++)
        {
                x0 = rand() / 52; x1 = rand() / 52;
                y0 = rand() / 80; y1 = rand() / 80;
                box(x0,y0,x1,y1);
        }
                 
        srand(2204);
        for (zaehler =0; zaehler < 52; zaehler ++)
        {
                x0 = rand() / 52; x1 = rand() / 52;
                y0 = rand() / 80; y1 = rand() / 80;
                unbox(x0,y0,x1,y1);
        }

        srand(2204);
        for (zaehler = 0; zaehler < 80; zaehler ++)
        {
                x0 = rand() / 52; y0 = rand() / 80;
                radius = ( rand() / 1000 ) + 5;
                circle(x0, y0, radius);
        }        
        
        srand(2204);
        for (zaehler = 0; zaehler < 80; zaehler ++)
        {
                x0 = rand() / 52; y0 = rand() / 80;
                radius = ( rand() / 1000 ) + 5;
                uncircle(x0, y0, radius);
        }       
        
        srand(2204);
        for (zaehler = 0; zaehler < 1500; zaehler ++)
        {
                x0 = rand() / 52; y0 = rand() / 80;
                point(x0,y0);
        }       

        srand(2204);
        for (zaehler = 0; zaehler < 1500; zaehler ++)
        {
                x0 = rand() / 52; y0 = rand() / 80;
                unpoint(x0,y0);
        }       
                
        puts("Fertig, Taste um zurueck ins Betriebssystem !!");
        getch();

        closepl();

}
/*=================================================================*/
```

Jetzt sollten  die  Grundlagen klar sein,  um  ein  komplexeres  C-
Programm vorzustellen, den `PCX - Reader`.

``` c
/*-------------------------------------------------------------------*/
/*dispcx.c   -   Read & Display a*.PCX graphics file in Microsoft's  */
/* Quick C in EGA and in Borland's Turbo C 2.0 and equivilents with    */
/* all supported BGI graphics modes (VGA/EGA/CGA/Herc).                */
/* ------------------------------------------------------------------- */
/* Original by                      :  M. CLYNES   07-24-89            */
/* Borland Compiler Enhancements by :  J. Braatz   02-16-92            */
/* Adapted to Club-80 Terminal by   :  A. Schmid   06-07-93            */
/* adapted to Genie IIIs by         :  E. Schroeer 18-11-93            */
/* ------------------------------------------------------------------- */
/* WARNING Only with LIBZ and HOLTE !!!                                */

# include <stdio.h>
# include <conio.h>
# include <grafik.h>

# define YMAX     449    /*Genie IIIs xmax=639,ymax=449*/
# define XMAX     639

char inversfl='N';
int  xoff=0,yoff=0;
int  eof_flag=0;

struct pcxheader{
       char manufact;     /*0    Hersteller-ID*/
       char version;      /*1    Versionsnummer*/
       char encode;       /*2*/
       char bpp;          /*3*/
       int xmin;          /*4, 5 Bildgroesse*/
       int ymin;          /*6, 7      "*/
       int xmax;          /*8, 9      "*/
       int ymax;          /*10,11      "*/
       int hres;          /*12,13 Aufloesung der Hardware*/
       int vres;          /*14,15     "       "      "*/
       char colormap[48]; /*16-63*/
       char reserved;     /*64    momentan reserviert*/
       char ebene;        /*65    Zahl der Farbebenen*/
       int bpline;        /*66,67 Byte pro Zeile je Ebene*/
       int paletinfo;     /*68,69 Art des Bildes*/
       char blank[58];    /*derzeit keine Informationen*/
       } header;

/*========================================================================*/
/*gobal decoded data   char decodebuff[200];*/
/*========================================================================*/
/*Decode a line of .PCX*/
/*This procedure reads one encoded line from the image file*/
/*0 = valid data stored*/
/*EOF = out of data in file*/

decodepcx(decodebuff, bytesline, fp)
char *decodebuff;                                 /* where to place data */
int bytesline;                                    /* # of bytes per line */
FILE*fp;                                         /*image file handle*/
{
  int data;
  int cnt;

  while(bytesline > 0 ){
    cnt = 1;
    if(EOF == (data = getc(fp))) return(EOF);   /*retrive a data byte*/
    if(0xc0 == (0xc0 & data)){
      cnt = 0x3f & data;                        /*get repeat count*/
      if(EOF == (data = getc(fp))) return(EOF);  /*get real data*/
      }

    while(cnt--){                         /* expand data into buffer */
      if(data==0x1A) eof_flag=1; else eof_flag=0;
      if(eof_flag && data==0x1A) data=0;
      *decodebuff = data;
      ++decodebuff;
      --bytesline;
      }
    }
  return(0);                                  /*file read status*/
  }
/*decodepcx()  end*/

/*========================================================================*/
/*read .PCX header into structure*/
/*========================================================================*/
readheader(fp)
FILE *fp;
{
  int count;
char*loader;

  loader = (char *) &header;

  for(count = 0; count < 128; ++count){
     *loader = (char)fgetc(fp);
     ++loader;
     }

/*Note: For Borland Compiler users, the following will need to be*/
/*changed to outtext() if you want to see the header information on*/
/*Hercules monitors (with the old herc.bgi driver)*/

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
/*pixel line dump*/
/*========================================================================*/
/*DUMP A RASTER LINE TO DISPLAY*/
/*pixeldump((pointer buffer), (# of BYTES), (Y line on screen to dump))*/
/*========================================================================*/

pixeldump(rdbuff, bytes, line)
char *rdbuff;
int line, bytes;

{
  unsigned x;
  unsigned char byte, loop;

  bytes = (bytes * 8);
  
/*Sehr grosses Bild in X-Richtung komprimieren*/

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
FILE*fp;
  
  z3vinit();

  argc = argc;                          /*Nuke's Compiler warning*/
  
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

  while(status != EOF){    /*do EXTRA READS to srink vertical*/
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
    else        /* Bild laesst sich mit Aufloesung darstellen */  
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
/*End of main()*/
```

Diese Beispiele sollten eigentlich ausreichend sein, um eigene Experimente
zu starten.  Gerüchten zufolge soll es ja noch einige
andere Genie  IIIs Besitzer im Club geben. Rafft euch mal auf und
nehmt Kontakt auf.
Das Holte CP/M plus für den Genie IIIs Computer ist leider nie mit
seinen hervorragenden Zusatzfunktionen programmtechnisch ausgenutzt
worden.  Der Rechner ist wohl zu  einer  Unzeit  auf  dem  Markt
erschienen, als schon die MSDOSe Welle über alles hereinbrach.  Aus
heutiger   Sicht bedauerlich, aber so bleibt uns   ein   weites
Betätigungsfeld - solange die Rechner lauffähig sind.

Juni-August '94

Egbert Schröer, Volker Dose
