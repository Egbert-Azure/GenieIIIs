/****************************************************************/
/* PUTPCX.C                                                     */
/*                                                              */
/* Gets pixel data from the screen and writes a PCX format file */
/* Alexander Schmid 9/93                                        */
/****************************************************************/

#include <stdio.h>
#include <conio.h>
#include <grafik.h>   /* computer specific graphic driver */


#define YMAX	 449	/* Genie IIIs xmax=639,ymax=449 */
#define XMAX     639


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
/*                   write .PCX header into file                          */
/*========================================================================*/
writeheader(fp)
FILE *fp;
{
  int  count;
  char *loader;                /* Zeiger auf ein character-array          */

  loader = (char*) &header;    /* LOADER zeigt auf den Header im Speicher */

  for(count = 0; count < 128; ++count){     /* 128 Bytes schreiben        */
    putc(*loader,fp);
    ++loader;                               /* Zeiger eins weiter         */
    }
}

/*========================================================================*/
/*                   read pixel from screen                               */
/*========================================================================*/
unsigned char readscreen(x,y)
int x,y;
{
  return point(x,YMAX-y);
}

/*========================================================================*/
/*                   mark area to write                                   */
/*========================================================================*/
area(x1,y1,x2,y2)
int *x1,*y1,*x2,*y2;
{
  int xa1,ya1,xa2,ya2;
  char ch;

  openpl();                                /* Grafik einblenden    */
/*  color=2;    */                            /* Punkt invertieren    */
  box(*x1,YMAX-*y1,*x2,YMAX-*y2);         /* Ausschnitt markieren */
  while((ch=getch())!=0x0D){              /* auf ENTER warten     */
    xa1=*x1; ya1=*y1; xa2=*x2; ya2=*y2;   /* Werte merken         */
    switch(ch){
      case 'd': if(*x2<XMAX) ++(*x2);       break;    /* rechter Rand ->  */
      case 'f': if(*x2<XMAX-10) (*x2)+=10;  break;    /* rechter Rand ->> */
      case 's': if(*x2>*x1+10) --(*x2);     break;    /* rechter Rand <-  */
      case 'a': if(*x2>*x1+20) (*x2)-=10;   break;    /* rechter Rand <<- */
      case 'x': if(*y2<YMAX) ++(*y2);       break;    /* unterer Rand V   */
      case 'c': if(*y2<YMAX-10) (*y2)+=10;  break;    /* unterer Rand VV  */
      case 'e': if(*y2>*y1+10) --(*y2);     break;    /* unterer Rand ^   */
      case 'r': if(*y2>*y1+20) (*y2)-=10;   break;    /* unterer Rand ^^  */

      case 'D': if(*x1<*x2-10) ++(*x1);     break;    /* linker Rand ->   */
      case 'F': if(*x1<*x2-20) (*x1)+=10;   break;    /* linker Rand ->>  */
      case 'S': if(*x1>0) --(*x1);          break;    /* linker Rand <-   */
      case 'A': if(*x1>=10) (*x1)-=10;      break;    /* linker Rand <<-  */
      case 'X': if(*y1<*y2-10) ++(*y1);     break;    /* oberer Rand V    */
      case 'C': if(*y1<*y2-20) (*y1)+=10;   break;    /* oberer Rand VV   */
      case 'E': if(*y1>0) --(*y1);          break;    /* oberer Rand ^    */
      case 'R': if(*y1>10) (*y1)-=10;       break;    /* oberer Rand ^^   */
      }
    box(xa1,YMAX-ya1,xa2,YMAX-ya2);  /* alten Rand loeschen        */
    box(*x1,YMAX-*y1,*x2,YMAX-*y2);  /* neuen Rand ziehen          */
    }
  box(*x1,YMAX-*y1,*x2,YMAX-*y2);    /* Markierung wieder loeschen */
  textmode();                        /* Grafik abschalten          */
  }

/*========================================================================*/
/*                   do run length encoding                               */
/*========================================================================*/
compress(arr1,bytes,fp)
unsigned char arr1[];
int bytes;
FILE *fp;
{
  unsigned char arr2[150],ch;
  int repeat,ausgabe=0,eingabe=0;

  while(eingabe<bytes){               /* Zahl der Eingangsbytes           */
    ch=arr1[eingabe];                 /* Byte aus dem Eingangsarray holen */
    ++eingabe;                        /* Eingabezeiger eins weiter        */
    repeat=1;                         /* Wiederholungszaehler             */
    while((ch==arr1[eingabe]) && (repeat<0x3F)){  /* Byte wiederholt sich */
      ++repeat; ++eingabe;                        /* Zaehler erhoehen     */
      }
    if(repeat>1){                         /* wenn es Wiederholungen gab   */
      arr2[ausgabe]=(char)repeat + 0xC0;  /* Zaehlbyte zusammensetzen     */
      ++ausgabe;                          /* Ausgabezaehler erhoehen      */
      }
    else{
      if((ch & 0xC0) == 0xC0){     /* Datenbyte mit gesetztem Bit 6 und 7 */
        arr2[ausgabe]=0xC1;        /* Zaehlbyte = 1 schreiben             */
        ++ausgabe;                 /* Ausgabezaehler erhoehen             */
        }
      }
    arr2[ausgabe]=ch;              /* Byte in Ausgabearray schreiben      */
    ++ausgabe;                     /* Ausgabezaehler erhoehen             */
    }
  for(eingabe=0; eingabe<ausgabe; ++eingabe)  /* Ausgabearray auf Floppy  */
    fputc(arr2[eingabe],fp);                  /* schreiben                */
  }

/*========================================================================*/
/*                   write .PCX data                                      */
/*========================================================================*/
writepcx(x1,y1,x2,y2,fp)
int x1,y1,x2,y2;
FILE *fp;
{
  int x,y,bits;
  unsigned char array[150],byte,count;

  openpl();                               /* Grafik einblenden             */
  for(y=y1; y<=y2; ++y){                 /* vom oberen zum unteren Rand   */
    count=0;                             /* Zaehler fuer den Zeilenpuffer */
    for(x=x1; x<=x2; x+=8){              /* vom linken zum rechten Rand   */
      byte=0;
      for(bits=0; bits<8; ++bits){       /* Byte zusammenbasteln          */
        byte |= readscreen(x+bits,y)<<7-bits;
        }
      array[count]=byte;
      ++count;
      }
    compress(array,(x2-x1+1)/8,fp);       /* Zeile komprimieren und auf   */
    }                                     /* Floppy schreiben             */
  textmode();                             /* Grafik abschalten            */
  }

/*=========================== MAIN() =====================================*/

main( argc, argv )
int argc;
char *argv[2];
{
  FILE *fp;
  int x1,y1,x2,y2,xoff=0,yoff=0;
  char ch;

  argc = argc;

  if((fp = fopen(argv[1],"rb")) != NULL){
    printf("ERROR: FILE '%s' DOES ALREADY EXIST.\n",argv[1]);
    exit(0);
    }

  if((fp = fopen(argv[1],"wb")) == NULL){
    printf("ERROR: CAN NOT OPEN FILE '%s' \n",argv[1]);
    exit(0);
    }

  x1=0; y1=0; x2=200; y2=200;
  do{
    area(&x1,&y1,&x2,&y2);
    printf("Die Koordinaten sind %d/%d %d/%d\n",x1,y1,x2,y2);
    }while((ch=getch())==0x0D);   /* Korrektur mit RETURN moeglich */

  printf("\nX-/Y-Offsets so lassen ? ");
  if(((ch=getch() & 0xDF) == 'J') || (ch=='Y') || (ch==0x0D)){
    header.xmin=x1;
    header.ymin=y1;
    header.xmax=x2;
    header.ymax=y2;
    }
  else{
    printf("\n\n  X-Offset: "); scanf("%d",&xoff);
    printf("  Y-Offset: "); scanf("%d",&yoff);
    header.xmin=xoff;
    header.ymin=yoff;
    header.xmax=x2-x1+xoff;
    header.ymax=y2-y1+yoff;
    }

  header.encode=1;
  header.bpp=1;
  header.hres=XMAX;
  header.vres=YMAX;
  header.ebene=1;
  header.bpline=(x2-x1+1)/8;

  writeheader(fp);
  writepcx(x1,y1,x2,y2,fp);
  fclose(fp);
  closepl();
  exit(0);
  }
/* End of main() */
