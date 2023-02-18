/*******************************************************************************
*  G R E P  *  G R E P 0 0 5   *   T h o m a s	 H o l t e   *	 8 6 0 9 2 2   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*------------------------------------------------------------------------------
*	      GREP.C: A generalized regular expression parser
*
*		       Copyright (c) 1984 Allen Holub
*	    Copyright (c) 1984 Software Engineering Consultants
*			       P.O. Box 5679
*			    Berkeley, CA, 94705
*
*			    All rights reserved
*
*	This program may be copied for personal,  non-commercial use
*	only, provided that this copyright notice is included in all
*	copies	and  that  this  program is not modified in any way.
*	Copying for any other use without previously  obtaining  the
*	written permission of the author is prohibited.
*
*	Machine readable versions of  this  program may be purchased
*	for $35 from  Software	Engineering  Consultants.  Supported
*	disk formats are CP/M 8" SS/SD and PCDOS (v2.x) 5-1/4" DS/DD.
*
*-----------------------------------------------------------------------------*/

#include <stdio.h>
#include "getargs.h"
#include "tools.h"

/* GREP
 *
 * Search a file for a pattern.
 *
 * The algorithm used here is essentially the algorithm in Software Tools in
 * Pascal (pp 145f.). Though the routine have been changed somewhat to put them
 * into good 'C' (really, Allen ?!?!?!?). See tools.c for details.
 *
 * This program is a healthy subset of the UNIX program of the same name. The
 * differences are as follows:
 *
 *		- the -s, -x and -b options are not supported.
 *		- the meta-characters ()+?  are not supported.
 *
 * usage is:
 *		grep [-vclnhyef] [expression] files ...
 */

#define CPM     			/* Comment this out if you're compiling
					   in an MSDOS system */
#define MAXLINE  128			/* Maximum size of an input line */
#define MAX_EXPR  64			/* The maximum number of regular
					   expressions separated by newlines
					   or | allowed */
extern FILE *fopen ();			/* stdlib routine */


/* The following global flags are TRUE if a switch was set in the command line,
 * FALSE otherwise.
 */
static BOOL cflag;		/* print only count		      */
static char *eflag = "";	/* expression begins with "-"	      */
static char *fflag = "";	/* file containing regular expression */
static BOOL hflag;		/* don't print filename headers	      */
static BOOL lflag;		/* print only filenames		      */
static BOOL nflag;		/* print line numbers		      */
static BOOL vflag;		/* print all lines but those matching */
static BOOL yflag;		/* force upper case		      */

static ARG Argtab[] = 
  {{'c', BOOLEAN, &cflag, "print only count of matching lines"	      },
   {'e', STRING , &eflag, "regular expression <str> begins with \"-\""},
   {'f', STRING , &fflag, "file <str> contains regular expression"    },
   {'h', BOOLEAN, &hflag, "don't print filename headers"	      },
   {'l', BOOLEAN, &lflag, "print only filenames once"		      },
   {'n', BOOLEAN, &nflag, "print line numbers"			      },
   {'v', BOOLEAN, &vflag, "print all lines but those matching"	      },
   {'y', BOOLEAN, &yflag, "map all input characters to upper case"    }};

#define NUMARGS (sizeof Argtab / sizeof(ARG))

static BOOL first;			/* 1st line of file being printed */


main (argc, argv)
  int argc;
  CHAR **argv;
{
  register int	 i, j;			/* loop counters		*/
	   int	 linenum;		/* current line number		*/
	   int	 count; 		/* match count			*/

	   int	 line[MAXLINE]; 	/* current input line		*/
	   int	 numfiles;		/* number of input files	*/
#ifdef CPM
	   char  *fname;		/* name of current input file	*/
#else
	   BOOL  match_flag = FALSE;	/* any matches found?		*/
#endif
	   FILE  *stream;		/* file descriptor		*/
	   int	 exprc; 		/* expression count		*/

	   TOKEN *exprv[MAX_EXPR];	/* array of regular expressions */
	   BOOL  match; 		/* match in current line	*/

  i = 1;

  /* Expand switches on the command line */
  if ((argc = getargs(argc, argv, Argtab, NUMARGS)) <= 1 && !*eflag && !*fflag)
  {
#ifdef CPM
     printf (        "\nNo regular expression specified\n");
#else
    fprintf (stderr, "\nNo regular expression specified\n");
#endif
    exit    (2);
  }

  /* Get the pattern string */
  if (!(exprc = get_expr(exprv, MAX_EXPR, &argv[i])))
  {
#ifdef CPM
     printf (        "\nIllegal regular expression\n");
#else
    fprintf (stderr, "\nIllegal regular expression\n");
#endif
    exit    (2);
  }

  if (!*eflag && !*fflag) i++;
  if ((numfiles = argc - i) <= 1)	/* Get the number of files left to */
    hflag = TRUE;			/* process on the command line	   */

  do
  {
    stoupper (argv[i]);
#ifdef CPM
    if (numfiles) fname = argv[i]; else fname = "CON:";

    if (!(stream = fopen(fname, "r")))
    {
      printf ("\nCan't open %s\n", fname);
      continue;
    }
#else
    if (numfiles)
    {
      if (!(stream = fopen(argv[i], "r")))
      {
	fprintf (stderr, "\nCan't open %s\n", argv[i]);
	continue;
      }
    }
    else stream = stdin;
#endif
    count   = 0;
    linenum = 1;
    first   = TRUE;

    while (fgets(line, MAXLINE, stream))
    {
#ifdef CPM
      if (!*fflag || yflag) stoupper (line);
#else
      if (	     yflag) stoupper (line);
#endif

      for (j = exprc, match = FALSE; --j >= 0; cntrl_c ())
	if (matchs(line, exprv[j]))
	{
	  match =
#ifndef CPM
		  match_flag =
#endif
			       TRUE;
	  count++;
	}
      pr_match (linenum, line, argv[i], match, numfiles);
      linenum++;
      if (lflag && count) break;
    }
    pr_count (numfiles, argv[i], count);
    fclose   (stream);
  }
  while (++i < argc);

#ifndef CPM
  exit (!match_flag);
#endif
}

/*----------------------------------------------------------------------------*/

static pr_count (fcount, fname, count)
  int fcount, count;
  CHAR *fname;
{
  /* Process the -c flag by printing out a count and, if more than one file was
   * listed on the command line, the file name too.
   */

  if (!cflag) return;

  if (fcount > 1) printf ("%-12s: ", fname);
		  printf ("%d\n"   , count);
}

/*----------------------------------------------------------------------------*/

static pr_match (linenum, line, fname, match, numfiles)
  int linenum, numfiles;
  CHAR *line, *fname;
  BOOL match;
{
  /* If a match is found print the correct thing as specified by the command
   * line switches.
   */

  if (cflag) return;

  if (vflag && !match || !vflag && match)
  {
    if (first)
    {
      first = FALSE;
      printf ("\n");
    }
    if (!hflag || lflag 	) printf ("%s%c" , fname, lflag ? '\n' : ':');
    if (		   nflag) printf ("%04d:", linenum);
    if (!hflag || lflag || nflag) printf (" ");
    if (	 !lflag 	) printf ("%s"   , line);
  }
}

/*----------------------------------------------------------------------------*/

static int do_or (lp, expr, max)
  CHAR *lp;
  TOKEN **expr;
  int max;
{
  register int	 found; 		/* number of regular expressions   */
  register TOKEN *pat;			/* pattern generated by makepat () */
	   CHAR  *op;			/* separator symbol		   */
	   CHAR  *in_string (); 	/* search 1st occurence of CHAR    */
	   TOKEN *makepat   (); 	/* generate pattern		   */

  found = 0;

  /* Extract regular expressions separated by OR_SYMs from "lp" and put them
   * into "expr". Extract only up to "max" expressions. If "yflag" is true map
   * string to upper case first.
   */

  if (yflag) stoupper (lp);

  while (op = in_string(OR_SYM, lp))
  {
    if (found <= max && (pat = makepat(lp, OR_SYM)))
    {
      *expr++ = pat;
      found++;
    }
    lp = ++op;

    if (!pat) goto fatal_err;
  }

  if (found <= max && (pat = makepat(lp, OR_SYM)))
  {
    found++;
    *expr = pat;
  }

  if (!pat)
  {
    fatal_err: printf ("\nIllegal expression\n");
	       exit   (2);
  }

  return found;
}

/*----------------------------------------------------------------------------*/

static int get_expr (expr, max, defexpr)
  TOKEN *expr[];
  int max;
  register CHAR **defexpr;
{
	   FILE *stream;		/* file with regular expressions */
  register int	count;			/* number of regular expressions */
	   CHAR line[MAXLINE];		/* input line			 */
	   CHAR *p;			/* temporary argument pointer    */
#ifdef DEBUG
  int  i;				/* temporary loop counter	 */
#endif

  /* Get regular expressions separated by | or newlines either out of a file or
   * of the command line depending on whether the -f flag is set. The
   * expressions are converted into pattern templates (see tools.c) and pointers
   * to the templates are put into the array expr[] (which works similar to
   * argv).
   *
   * Return the number of expressions found (which can be used in a similar
   * fashion to argc).
   */

  count = 0;

  if (*fflag)
  {
    /* "fflag" is the file name and expressions should be taken from that file.
     */
    if (!(stream = fopen(fflag, "r")))
    {
#ifdef CPM
       printf (        "\nCan't open %s\n", fflag);
#else
      fprintf (stderr, "\nCan't open %s\n", fflag);
#endif
      exit    (2);
    }

    while (max - count && fgets(line, MAXLINE, stream))
      count += do_or(line, &expr[count], max - count);

    fclose (stream);
  }
  else

    /* "*defexpr" or "eflag" is the expression. */
    if (count += 
      do_or(p = *eflag ? eflag : *defexpr, &expr[count], max - count)) p = " ";

#ifdef DEBUG
  /* Print out all the regular expressions after they have been converted into
   * pattern templates (see tools.c).
   */
  for (i = count; --i >= 0;)
  {
    pr_tok (expr[i]);
    printf ("--------------------------------------------------\n");
  }
#endif

  return count;
}

/*----------------------------------------------------------------------------*/

cntrl_c ()
{
#ifdef CPM
  /* If any character was hit, and that character is a ^C, then abort. */

  if (bdos(11) && (bdos(1) & 0x7F) == ETX) exit (1);
#endif
}
