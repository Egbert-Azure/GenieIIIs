/******************************************************************************
*  S T D I O  *  L I B C X X X  *  T h o m a s   H o l t e  *   8 7 0 3 0 9   *
*******************************************************************************
*
* Version 1.1 by Thomas Holte
*
* Header file with common definitions.
*
* Note: This file generates no code !
*/

/* builtin functions */
#define abort()       _exit (2)
#define ferror(fd)    ((fd)->err)
#define getc(fd)      fgetc (fd)
#define isascii(c)    !((c) & 0x80)
#define isxdigit(c)   isnumeric (c, 16)
#define pause()       bdos (6, 0xFD)
#define putc(c,fd)    fputc (c, fd)
#define remove(fspec) unlink (fspec)
#define rewind(fd)    fseek (fd, 0L, 0)

#define BCD    char
#define BOOL   char
#define RESULT int

#define TRUE      1
#define FALSE     0
#define NULL      0
#define SUCCESS   0
#define ERROR   (-1)
#define MAXINT  0x7FFF
#define MAXLONG 0x7FFFFFFF

#define MAXLINE 255		/* maximum length of text line */

/* file area */
#define CON 	  0x80
#define AUX 	  0x81
#define LST 	  0x82

#define FCB_READ     0
#define FCB_WRITE    1
#define FCB_R_W      2

#define REC_SIZE   128
#define BUF_SIZE  1024
#define MASK	  (BUF_SIZE / REC_SIZE)

#define START	     0
#define CURRENT      1
#define LAST	     2

typedef struct {
		 char dr;		/* drive code (0 - 16)		     */
		 char f[8];		/* file name in ASCII upper case     */
		 char t[3];		/* file type in ASCII upper case     */
		 char ex;		/* current extent number	     */
		 int  s;		/* reserved for internal system use  */
		 char rc;		/* record count for extent "ex"      */
		 char d[16];		/* filled in by CP/M, reserved	     */
		 char cr;		/* current record in "ex"	     */
		 long r; 	 	/* random record number              */
		 char pw[8];		/* password field		     */
		 char md;		/* creation mode		     */
		 char fm;	  	/* access   mode		     */
		 long eof_rec;		/* last record			     */
		 char eof_byte;		/* last record byte count	     */
		 BOOL serial;		/* serial device ?		     */
		 char c;		/* buffered character (serial)	     */
		 int  err;		/* error status			     */
		 BOOL eof;		/* EOF reached ? 		     */
		 BOOL stream;		/* buffered I/O ?		     */
		 struct seq_buf {
				  long rec;		/* 1st buffered rec  */
		 		  char *ap;		/* active pointer    */
		 		  int  ac;		/* active count      */
		 		  char st;		/* buffer status     */
		 		  char bu[BUF_SIZE];	/* buffer follows    */
	       			} *ps;  /* pointer to seq buffer	     */
	       } FILE;

#define SIZE_FD sizeof(FILE)
#define SIZE_SB sizeof(struct seq_buf)

#define FS_EMPTY 0
#define FS_VALID 1
#define FS_DIRTY 2

struct stat {
 	      unsigned st_mode;		/* file mode			   */
 	      long     st_size;		/* file size in bytes		   */
 	      long     st_atime;	/* time of last access or creation */
 	      long     st_mtime;	/* time of last data modification  */
	    };
