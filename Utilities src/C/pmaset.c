
/*****************************************************************************
*                         PMASET - Archive Date Set                          *
*   A program to set the date/time stamp of members of PMA or LZH files      *
*                                                                            *
*                      derived from Ross Presser's ADS.COM                   *
*  adapted to Hi-Tech-C and converted for PMA (LZH) archives by              *
*  Alexander Schmid 09.01.94                                                 *
*                                                                            *
*  link with patched GETFCB.OBJ from LIBZ01:LBR to get real DU: references   *
*  instead of Hi-Tech's U:D:  e.g PMASET E1:TEST.PMA 01/01/94                *
*****************************************************************************/

/* Usage:
	PMASET [u/d:]<pmafile> mm/dd/yy [hh:mm[:ss][p]] [amember]
where
	<pmafile>	= PMA file name, required (user/drive: optional)
	mm/dd/yy	= date stamp, required
	[hh:mm[:ss][p]]	= time stamp, optional.  24 hour clock, or append
			  'p' for pm.  defaults to midnight
	[amember]	= member filespec, possibly wild. Defaults to *.*
*/

#include <unixio.h>
#include <stdio.h>
#include <string.h>
#include <hitech.h>

#define ERROR -1

char *index(char *s, char c);

/* structure of a PMA header */

struct header{
	char hdrlen;		/* length of header */
	char chksum;		/* checksum  */
	char method[5];		/* compression method */
	ulong crsize;		/* compressed file size */
	ulong ucsize;		/* original file size */
	unsigned _time;		/* MS-DOS style time stamp */
	unsigned _date;		/* MS-DOS style date stamp */
	char fill[2];
	char namlen;		/* length of file name */
	char name[13];		/* file name */
	} *pmahdr;

/* various variables */

char	offset;			/* offset to calculate new checksum */
char	filnam[20];		/* scratch for filename in archive */
char	buf[256];		/* two-sector buffer */
int 	u,			/* PMA file descriptor */
	changed;		/* count of changed members */
ulong 	pos;			/* offset from start of file in bytes */
char 	*matchmem;		/* member filespec to match */
unsigned date,			/* new date stamp */
	 time;			/* new time stamp */
int	notfirst=0;		/* false only before first header is read */

/* This prints the optional error message, plus a usage message */

void usage(char *s) {
	puts(s);
	puts("\nUsage:");
	puts("\tPMASET [u/d:]<pmafile> mm/dd/yy [hh:mm[:ss][p]] [amember]");
	puts("where");
	puts("\t<pmafile>\t= PMA file name, required. user/drive: optional");
	puts("\tmm/dd/yy\t= date stamp, required. century optional");
	puts("\t[hh:mm[:ss][p]]\t= time stamp, optional.  24 hour clock, or append");
	puts("\t\t\t  'p' for pm.  defaults to midnight");
	puts("\t[amember]\t= member filespec, possibly wild. Defaults to *.*");
}

/*********************************************************************
 This function moves the file position to the beginning of the next
 pma header, and calculates where the one after that will be in prepar-
 ation for the next call.  On all calls except the first, it writes the
 buffer back into the file to preserve any changes that may have been
 made.
*/

int gethdr(void) {
	unsigned rec,		/* record number */
		 offs;		/* offset within record */

	/* If not first call, write the buffer */
	if(notfirst) {
		lseek(u,-256l,1);  /* back up 2 sectors */
		write(u,buf,256); /* write the buffer */
		}
	/* Convert long pos to rec # and offset.
	  In plain language, rec = pos/128; offs = pos % 128 */

	offs= (unsigned)(pos % 128l);
	rec = (unsigned)(pos / 128l);

	/* Go to the record computed and read 2 sectors */
	lseek(u,rec*128l,0);
	if(read(u,buf,256)<256) return EOF;

	/* Set pma header within buffer */
	pmahdr=(struct header*)(buf+offs);

	/* Compute position of next header */
	pos += pmahdr->hdrlen+2;	/* bump past this header */
	pos += pmahdr->crsize;		/* bump past this file */
	notfirst=1;			/* flag not first */
	return 0;
}

/**************************************************************
 This function parses a filename from fn.ext to fn------ext.
*/

char *hack(char *k, char *j) {
	int i=0;	/* scratch index */

	while(*j && *j != '.')	/* do first 8 char */
		if (*j=='*') {		/* expand * to ?s */
			while(i<8) k[i++]='?';
			while(*j && *j!= '.') ++j;
			}
		else 	k[i++]=*j++; 	/* otherwise copy */
	while(i<8) k[i++]=' ';	/* pad out name to 8 */
	if(*j) ++j;		/* move past possible dot */
	while(*j)	/* do last 3 char */
		if (*j=='*') {		/* expand * to ?s */
			while(i<11) k[i++]='?';
			while(*j) ++j;
			}
		else	k[i++]=*j++;	/* otherwise copy */
	while(i<11) k[i++]=' ';	/* pad out ext to 3 */
	k[11]='\0';	/* finish string with null */
	return(k);
}

/**********************************************************
 This function checks whether the unparsed filename s
 matches the parsed filename t.
*/

match (char *s, char *t) {
	char 	a[20],	/* scratch string */
		*b;	/* scratch string ptr */

	hack(a,s);	/* parse the filename */
	b=a;
	while(*b)	/* match until EOS */
		if(*b!=*t && *t!='?')	/* either a match or a ? is ok */
			return(0);	/* otherwise fail */
		else
			++b,++t;	/* Bump ptrs */
	return(1);	/* succeed */
}

/************************************************************************
 This function takes an mm/dd/yy string and returns an MS-DOS date stamp.
*/

getdate(char *s) {
	int mm=0,dd=0,yy=0;			/* init to 0 */
	sscanf(s,"%d/%d/%d",&mm,&dd,&yy);	/* read from string */
	if(yy<100) yy += 1900;			/* force century */
	if(yy<1980) yy += 100;			/* disallow less than 1980 */
	if(mm<1 || mm>12 || dd<1 || dd>31 || yy>2108)	/* range checking */
		usage("Invalid date!\n"),exit();
	return (((yy-1980)<<9) + (mm<<5) + dd);	/* convert to MS-DOS fmt */
}

gettime(char *s) {
	int hh=0,mm=0,ss=0;			/* init to 0 */
	sscanf(s,"%d:%d:%d",&hh,&mm,&ss);	/* read from string */
	if(s[strlen(s)-1]=='P') hh+=12;		/* suffix p for P.M. */
		/* range checking */
	if(hh<0 || hh>23 || mm<0 || mm>59 || ss<0 || ss>59) {
		printf("\7%s is an invalid time, using 12:00a\n",s);
		return(0);	/* invalid times default to midnight */
		}
	return((hh<<11)+(mm<<5)+ss/2);
}

/* This function for debugging purposes; prints out archive header */

/* puthdr(ahp) struct pmahdr *ahp;
{
	char s[15];
	printf("%02x %d %12s %-10s %04x %04x %04x %-10s\n",
		ahp->arcmark,ahp->arcver,ahp->name,
		ltoa(s,ahp->crsize),ahp->_date,ahp->_time,
		ahp->crc,ltoa(s,ahp->ucsize));
}
*/

/* This will try to open fn alone, or with ext1 or ext2 appended */

tryext2(int (*fx)(),char *fn,int mod,char *ext1,char *ext2) {
	int z;
	char s[20];

	if((z=(*fx)(fn,mod))!=ERROR) return(z);
	if(NULL==index(fn,'.')) {
		strcpy(s,fn); strcat(s,ext1);
		if((z=(*fx)(s,mod))!=ERROR) return(z);
		strcpy(s,fn); strcat(s,ext1);
		if((z=(*fx)(s,mod))!=ERROR) return(z);
		}
	printf("\7%s not found!\n",fn);
	exit();
}

main(int argc, char *argv[]) {
	int open();		/* low level file open */

/* must be at least 2 arguments */
	if(argc<3) usage(""),exit();

/* Open the PMA file.  Try .PMA and .LZH extensions if none given */
	u=tryext2(open,argv[1],2,".PMA",".LZH");

/* Process date stamp */
	if (NULL==index(argv[2],'/'))	/* date must be mm/dd/yy */
		usage("Invalid date\n"),exit();
	date=getdate(argv[2]);

/* 3rd arg might be timestamp */
	if(argc>=4 && NULL!=index(argv[3],':')) { /* assume yes if : in it */
		time=gettime(argv[3]);		 /* process the time */
		argv[3]=argv[4]; --argc;	 /* delete arg from list */
		}
	else 	time=0;			/* if no time, use midnight */

/* 4th arg is member filespec, if there */
	matchmem="???????????";		/* default *.* first */
	if(argc>=4)
		hack(matchmem,argv[3]);	/* change fn.ext to filenameext */

	pos=0l;	/* init file position to 0 */

/* main loop */
	while(1) {
	/* read next header */
		if(gethdr()==EOF) break;
	/* must begin with legal pma mark */
		if((strncmp(pmahdr->method,"-lh",3)!=0) && (strncmp(pmahdr->method,"-pm",3)!=0)) {
			puts("Invalid archive format");
			break;
			}

	/* version 0 means end of file */
	/*	if(pmahdr->arcver == 0) break; */

	/* if filename matches our filespec, change it */
		strncpy(filnam,pmahdr->name,14);
		filnam[pmahdr->namlen]=0;  /* else we have no terminating 0 */
                if(strncmp(filnam,"rsion",5)!=0)  /* not self extracting */
			if(match(filnam,matchmem)) {
				printf("Changing %s\n",filnam);	/* say it */
				offset = (date>>8) - (pmahdr->_date>>8);
				pmahdr->chksum += offset;
				offset = (date & 255) - (pmahdr->_date & 255);
				pmahdr->chksum += offset;
				pmahdr->_date=date;	/* change date */
				offset = (time>>8) - (pmahdr->_time>>8);
				pmahdr->chksum += offset;
				offset = (time & 255) - (pmahdr->_time & 255);
				pmahdr->chksum += offset;
				pmahdr->_time=time;		/* and time */
				++changed;			/* count it */
			}
		} /* while */

/* Finish up */
	printf("%d members changed.\n",changed);	/* say how many */
	close(u);
}
