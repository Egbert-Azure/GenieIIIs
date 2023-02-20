/*==============================*/
/* pcx.h                        */
/* Structure of PCX-File Header */
/* (c) E.Schroeer and DOS 4'91  */
/*==============================*/

typedef unsigned char BYTE;
typedef unsigned int WORD;

/* The structure of PcxKopf display */
/*   the header of a PCX-File       */

/*==========================================================================*/
/* Element   Number of Byte   Meaning                                       */
/*==========================================================================*/
/* id        1                identification Byte, always 10 if PCX-File    */
/* ver       1                version of PCX-File 0=2.5, 2=2.8, 3=2.8       */
/*                            without colour information                    */
/* komp      1                compression art, since now only 1             */
/* bits      1                number of bits per point, monochrom=1,        */
/*                            4 colour=2, 16=4 and so on                    */
/* x1,y1     x=2,y=2          position upper left corner                    */
/* x2,y2     same above       position lower left corner                    */
/* ax,ay     same above       used for scanned pictures                     */
/* pal       48               if colour palette can be used, information    */
/*                            stored here                                   */
/* _r        1                reserved                                      */
/* ebene     1                mono and CGA=1, EGA and VGA up to 4           */
/* bz        2                number of bytes per line, for example:        */
/*                            EGA 640x350 = 80 Bytes if whole display is    */
/*                            stored                                        */
/* pal2      2                type of palette                               */
/*_r2        58               reserved for future expansions                */
/*============================================================================*/

typedef struct
{ BYTE id; 
  BYTE ver;
  BYTE komp; 
  BYTE bits;
  WORD x1,y1; 
  WORD x2,y2;
  WORD ax,ay; 
  BYTE pal[48];
  BYTE _r; 
  BYTE ebene;
  WORD bz; 
  WORD pal2;
  BYTE _r2[58];
} PcxKopf;

/* declare functions */

PcxKopf *PcxLeseKopf(FILE*);
BYTE *PcxResZeile(PcxKopf*);
BYTE PcxLeseZeile(PcxKopf*,FILE*,BYTE *);
void PcxAusgabe (PcxKopf*,FILE *);
