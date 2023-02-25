/* an.c (asm_neat) -- converts assembly language source to uniform
   appearance.	three options are available from the menu:

		a - convert comments to all lower case, code to
		    all upper.
		b - convert comments and code to upper case
		c - convert code to upper case, comments remain
		    unchanged.

	note: characters within a single or double quoted string remain
	      unchanged, if in code (label, mnemonic, or operand) area
	      of source.

	usage: >an oldfile newfile

06/07/86  made menu easier to understand.  frank gaude'

08/10/83  added three function options from menu.  added error
trapping for usage.  frank gaude'

12/23/82  original program (aspretty.c) written by william meyer.
assembly language version (neat.asm) written by joe wright (05/21/83)
then enchanced by irv hoff (05/27/83) into three 'neat' programs.

compiles with c/80 from software toolworks or manx aztec c ii controlled
by the define below.							 */

#define HITECH		/* define as C/80, AZTEC or HITECH compiler here */

#ifdef C/80
#include "tprintf.c"	/* special short "printf" function		 */
#define NULL 0
#define EOF -1
#endif

#ifdef AZTEC
#include "stdio.h"
#endif

#ifdef HITECH
#include "an.h"         /* prototypes of functions */
#include "stdio.h"
#endif

main(argc,argv)
int argc;
char **argv;
	{
	static int comment, cu, lc, onequote, twoquote, uc;
        static char byte, c;
        static FILE *f1,*f2;

	if (argc != 3)		/* test for correct number of auguments */
		{
		printf("usage: >an <oldfile> <newfile>");
		exit(1);
		}
	if ((f1 = fopen(argv[1],"r")) == NULL)	 /* open file 1 	*/
		{
		printf("Unable to find/open/read %s\n", argv[1]);
		exit(1);
		}
	if ((f2 = fopen(argv[2],"w")) == NULL)	 /* open file 2 	*/
		{
		printf("Unable to write to %s\n", argv[2]);
		exit(1);
		}
	byte = lc = uc = cu = 0;
agn:	cls();
	printf("\n\t               AN -- AsmNeat Program Menu\n\n");
	printf("\t     For desired function, enter character then <RETURN>\n\n");
	printf("\t    Conversion: A - Comments to lower case, code to upper\n");
	printf("\t                B - Comments and code to upper case\n");
	printf("\t                C - Code to upper case, comments unchanged\n");
	printf("\t         <CTRL-C> - Exit to Z-System\n\n");
	printf("\t            Choice: ");
	if ((byte = getresp()) == EOF)
		exit(0);
	switch(byte)
		{
		case 'a':
			lc = 1;
			break;
		case 'b':
			uc = 1;
			break;
		case 'c':
			cu = 1;
			break;
		default :
			goto agn;
		}
	printf("\n---> Converting Assembly Source File -- ");
	comment = onequote = twoquote = 0;	 /* initialize		*/
	while ((c = getc(f1)) != EOF)
		{
		switch (c)
			{
			case ';' :
			    comment = 1;
			    break;
			case '\n':
			    comment = onequote = twoquote = 0;
			    break;
			case '"' :
			    twoquote = ~twoquote;	/* toggle	*/
			    break;
			case '\'':
			    onequote = ~onequote;
			    break;
			default  :
			    break;
			}
		if ((onequote != 0 || twoquote != 0) & (comment == 0))
			putc(c,f2);
		else if ((comment == 1) & (cu == 1))
			putc(c,f2);
		else if ((comment == 0) | (comment == 1 && uc == 1))
			putc(toupper(c),f2);
		else if ((comment == 1) | (lc == 1))
			putc(tolower(c),f2);
		}
	fclose(f1);
	fclose(f2);
	printf("done\n");	/* go home msg				*/
	exit(0);
	}

cls()		/* clear screen function				*/
	{
	static int i;

	for (i = 0; i < 25; i++)
		printf("\n");
	}

getresp()	/* get nonwhite space character from keyboard input	*/
	{
	static int byte;

	for (;;)
		{
		if ((byte = getchar()) == EOF)
			break;
		else if ((byte != ' ') && (byte != '\t') &&
			(byte != '\n'))
			break;
		}
	return(tolower(byte));	
	}

#ifdef C/80
#include "stdlib.c"	/* must be at end of source file for inclusion	*/
#endif
