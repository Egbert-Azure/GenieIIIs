/******************************************************************************
*  S T D I O  *  C L I B X X X  *  T h o m a s   H o l t e  *   8 4 0 9 2 0   *
*******************************************************************************
*
* Version 1.0 by Thomas Holte
*
* Header file with common definitions.
*
* Note: This file generates no code !
*/

#define BOOL    char
#define RESULT  int

#define TRUE    1
#define FALSE   0
#define NULL    0
#define SUCCESS 0
#define ERROR   (-1)
#define BDOSERR 0xFF

/* Genie III system entry points */
#define VDINIT  0		/* initialize video controller chip M6845    */
#define RSINIT  1		/* initialize RS232C controller chip INS8250 */
#define KBCHAR  2		/* get a keyboard character if available     */
#define KBWAIT  3		/* wait for keyboard character		     */
#define VDCHAR  4		/* display a character			     */
#define PRSTAT  5		/* test printer status			     */
#define PRCHAR  6		/* output a character to the printer	     */
#define RSRCST  7		/* get a character from the RS232C interface
				   if available				     */
#define RSRCV   8		/* receive a character from the RS232C int.  */
#define RSTXST  9		/* test RS232C output status		     */
#define RSTX   10		/* transmit a character to the RS232C int.   */
#define READD  11		/* read a floppy disk sector		     */
#define WRITED 12		/* write a floppy disk sector		     */
#define GETTIM 13		/* get time and date in ASCII format	     */
#define SETTIM 14		/* set time and date in ASCII format	     */
#define XMOVE  15		/* read/write system data		     */
#define READW  16		/* read a Winchester disk sector	     */
#define WRITW  17		/* write a Winchester disk sector	     */

/* CP/M-80 BIOS entry points */
#define BOOT     0		/* arrive here from cold start load  */
#define WBOOT    1		/* arrive here for warm start	     */
#define CONST    2		/* check for console char ready	     */
#define CONIN    3		/* read console character in	     */
#define CONOUT   4		/* write console character out	     */
#define LIST     5		/* write listing character out	     */
#define PUNCH    6		/* write character to punch device   */
#define READER   7		/* read reader device		     */
#define HOME     8		/* move to track 00 on selected disk */ 
#define SELDSK   9		/* select disk drive		     */
#define SETTRK  10		/* set track number		     */
#define SETSEC  11		/* set sector number		     */
#define SETDMA  12		/* set DMA address		     */
#define READ    13		/* read selected sector		     */
#define WRITE   14		/* write selected sector	     */
#define LISTST  15		/* return list status		     */
#define SECTRAN 16		/* sector translate subroutine	     */

/* file area */
#define CON 	  0x80
#define RDR 	  0x81
#define PUN 	  0x82
#define LST 	  0x83

#define FCB_READ     0
#define FCB_WRITE    1
#define FCB_R_W      2

#define MAX_DRIVE  15
#define REC_SIZE  128

struct filedesc {
		  char dr;	/* drive code (0 - 16)		     */
		  char f[8];	/* file name in ASCII upper case     */
		  char t[3];	/* file type in ASCII upper case     */
		  char ex;	/* current extent number	     */
		  int  s;	/* reserved for internal system use  */
		  char rc;	/* record count for extent "ex"      */
		  char d[16];	/* filled in by CP/M, reserved	     */
		  char cr;	/* current record in "ex"	     */
		  int  r;	/* random record number		     */
		  char r2;	/* random record number overflow     */
		  char fm;	/* access mode			     */
		  char *ps;	/* pointer to seq buffer	     */
		};

#define FILE 	struct filedesc
#define SIZE_FD sizeof(struct filedesc)
