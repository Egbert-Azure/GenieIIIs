/* program 'pcc' (pre-complie checker) scans for open and close brace-
   pairs of a 'c' source file.  indicates if number of open/close braces
   are equal or not.  filename to be checked is a command line argument
   (e.g., >pcc fn.ft).  developed from program written by jack purdum.
   08/05/83  frank gaude'
                         
   08/12/83  added code to count open and closing comments.  made
   more friendly.  mike kelly

   09/11/83  cleaned up file listing style.  fg                         */

#include "printf.c"		/* for C/80 C environment		*/

/* set console control sequences
   edit to your terminal codes  					*/
#define CLS	26		/* clear crt screen code 		*/
#define CURSOR "\033="		/* cursor position lead-in pair		*/

#define BELL 7

main(argc, argv)
int argc;
char **argv;
	{
	int o_count, c_count;
	register char last_char;
	register int c;
	FILE *f1;

	putchar(CLS);
	printf("\n\n        PRE-COMPILE CHECKER 1.1\n");
	if (argc != 2)
		{
		printf("\nUsage >pcc fn.ft%c\n",BELL);
		exit(1);
		}
	if ((f1 = fopen(argv[1], "r")) == NULL)
		{
		printf("\nUnable to find/open %s\n", argv[1]);
		exit(1);
		}
	last_char = ' ';
	o_count = c_count = 0;
	puts("Opening Braces =      Closing Braces =      ");
	while ((c = agetc(f1)) != EOF)
		{
		if (last_char == '/' && c == '*')
			{
			while (c != '/' && last_char != '*')
				{
				last_char = c;
				c = agetc(f1);
				}
			}
		if (c == '\'')
			{
			last_char = c;
			c = agetc(f1);
			if (c == '\\')
				{
				while (c != '\'')
					c = agetc(f1);
				}
			if (c == '{' || c == '}')
				c = agetc(f1);
			}
		if (c == '"' && last_char != '\'')
			{
			c = agetc(f1);
			while (c != '"')
				c = agetc(f1);
			}
		if (c == '{')
			{
			++o_count;
			set_cur(4,18,o_count);
			}
		if (c == '}')
			{
			++c_count;
			set_cur(4,40,c_count);
			}
		last_char = c;
		}
	fclose(f1);
	if (o_count == c_count)
		printf("\nBrace pair-count is okay.  Starting Pass 2...\n");
	else
		printf("\nBrace pair-count is incorrect.  Aborting!\n");

	puts("Opening Comments =      Closing Comments =     ");
	f1 = fopen(argv[1], "r");
	o_count = c_count = 0;
	last_char = ' ';
	while ((c = agetc(f1)) != EOF)
		{
		if (last_char == '*' && c == '/')
			{
			++c_count;
			set_cur(6, 44, c_count);
			}
      		if (last_char == '/' && c == '*')
			{
			++o_count;
			set_cur(6, 20, o_count);
			}
		last_char = c;
  		}
 	fclose(f1);
	if (o_count == c_count)
		{
		printf("\nComment pair-count is okay.  Continuing...\n");
		exit(0);
		}
	else
		{
		printf("\nComment pair-count is incorrect.  Aborting!\n");
		exit(1);
		}
	}

/* crt cursor-setting function						*/
set_cur(row, col, num)
int row, col, num;
	{
	printf("%s%c%c%d", CURSOR, row + 31, col + 31, num);
	}
g function						*/
set_cur(row, col, num)
int row, col, num;
	{
	printf("%s%c%c%d", CURS