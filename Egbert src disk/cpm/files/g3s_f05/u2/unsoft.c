
/************************************************************************/
/* 		Program to convert Wordstar Document-Mode		*/
/*	    files to plain-vanilla, non-document, ascii format.		*/
/*									*/
/*	by Paul Homchick, One Radnor Station #300, Radnor, PA 19087	*/
/*									*/
/*	  will compile with Software Toolworks C/80, or CI C86,		*/
/*		Hi-Tech-C or Digital Research C				*/
/*		          	       					*/
/*                  last edited: 15:33, 6/09/84				*/
/************************************************************************/

#include	<stdio.h>
#include        <string.h>
#define VERSION	"1.1C"
#define VDATE	"09 Jun 84"
#define CR	0x0d
#define LF	0x0a
#define CPM_EOF 0x1a		/* cp/m end of file mark		*/
#define FF	0x0c		/* form feed ()			*/
#define TAB	0x09
#define CNTRL_O 0x0f		/* non-break space			*/	
#define END_HYP 0x1f		/* soft hyphen at end of line		*/
#define ERROR	-1
#define TRUE	1
#define FALSE	0
#define BIGLINE	512		/* number of char in biggest line	*/

/* #define DRC			define this to compile with Dig Res C*/
				/* comment out if not DRC		*/

static FILE  *inptr, *outptr;
static int   startline;

/************************************************************************/
/*			test input commands				*/
/************************************************************************/

int cmdeq(char *s, char *p)
{
	while(*p)
		if(*s++ != *p++)
			return(FALSE);
	return(TRUE);
}

/************************************************************************/
/*			wordstar translation routine			*/
/************************************************************************/

int translate(int c)
{
char	buf[BIGLINE];

	c&= 0x7f;			/* strip high bit		*/
	if (startline)
	{
		switch (c)
		{
			case '.':	/* process dot commands		*/
				fgets(buf,BIGLINE,inptr);
					/* .pa to form feed		*/
				if ((cmdeq(buf,"PA")) || (cmdeq(buf,"pa")) )
					putc(FF,outptr);
				startline = TRUE;
				return(translate(c= getc(inptr)));
			case LF:
				return(c);
			default:
				startline = FALSE;
		}
	}
	if (c < ' ')			/* check for control character	*/
	{
		switch (c)
		{
			case END_HYP:
				return('-');
			case CNTRL_O:
				return(' ');
			case LF:
			case FF:
			case TAB:
				return(c);
			case CR:
				startline = TRUE;
				return(c);
			case CPM_EOF:
				return(CPM_EOF);
			default:
				c= getc(inptr);
				return(translate(c));
		}	
	}
	else
		return(c);
}

/************************************************************************/
/*			short usage prompt routine			*/
/************************************************************************/

void usage(void)
{
	printf("unsoft version %s %s\n\n",VERSION,VDATE);
	printf("usage: unsoft wordstar_input_name ascii_output_name\n");
	printf("   or: unsoft (? | [help]) for help.\n");
	printf("converts wordstar document-mode files ");
	printf("to plain text format.\n");
}

/************************************************************************/
/*			error print routine				*/
/************************************************************************/

void error(char *s)
{
	printf("\007Error: %s\n",s);
}

/************************************************************************/
/*			on-line program help routine			*/
/************************************************************************/

void help(void)
{
	printf("\nUnsoft is a program to filter files prepared under Wordstar\n");
	printf("document mode.  Given a Wordstar document mode file as an\n");
	printf("input, unsoft will output a file having made the following\n");
	printf("transformations:\n\n");
	printf("\to High bits stripped from all characters.\n");
	printf("\to Converts '.pa' dot commands to form feed (^L).\n");
	printf("\to Removes all other dot command lines from file.\n");
	printf("\to Converts 'non-break-space' (^O) to a space.\n");
	printf("\to Converts soft hyphen at end of line (1F hex) to '-'.\n");
	printf("\to Passes through CR, LF, FF, and TAB characters.\n");
	printf("\to Filters out all other control characters.\n");
	printf("\nUsage: unsoft wordstar_input_name");
	printf(" ascii_output_name\n");
	printf("where (names) are input and output file names.  Unsoft ?,\n");
	printf("or [help] will evoke this help message.\n");
	exit(0);
}

main(int argc, char *argv[])
{
	int c;

	startline = TRUE;
	switch (argc)
	{
		case 1:
			usage();
			exit(0);
		case 2:
			if ( (!strcmp(argv[1],"?")) ||
			     (cmdeq(argv[1],"[HELP]")) ||
			     (cmdeq(argv[1],"[help]")) )
				help();
			else
			{
				usage();
				error("Not enough arguments on command line.");
				exit(0);
			}
		case 3:
			break;
		default:
			usage();
			error("Too many arguments on command line.");
			exit(0);
	}

	if (strcmp(argv[1],argv[2])==0)
	{
		printf("Input and output filenames must differ.");
		printf("  Aborting...\007\n");
		exit(0);
	}

#ifdef DRC
	if (!(inptr= fopenb(argv[1],"r")))
#else
	if (!(inptr= fopen(argv[1],"rb")))
#endif
	{
		printf("Can't open '%s' for input.\n",argv[1]);
		exit(0);
	}

#ifdef DRC
	if (!(outptr=fopenb(argv[2],"w")))
#else
	if (!(outptr=fopen(argv[2],"wb")))
#endif
	{
		printf("Can't open '%s' for output.",argv[2]);
		printf("  The disk directory is probably full.\n");
		exit(0);
	}

/************************************************************************/
/*			 	main loop				*/
/************************************************************************/

	printf("processing... ");
	while ((c=getc(inptr))!=ERROR && c!=CPM_EOF)
		putc(translate(c),outptr);
	putc(CPM_EOF,outptr);
	fclose(inptr);
	fclose(outptr);
	printf("done.\n");
}

