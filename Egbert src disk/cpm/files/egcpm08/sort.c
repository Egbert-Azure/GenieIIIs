/*******************************************************************************
*  S O R T  *  U N I X 0 2 0   *   T h o m a s	 H o l t e   *	 8 6 0 9 2 5   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*
 *				SORT.C
 *
 *		   Copyright (c) 1986 Allen T. Holub
 *			  All rights reserved.
 */

#define CPM

#include <stdio.h>
#include "getargs.h"

/*------------------------------------------------------------------------------
 *	General purpose #defines.
 */

/* ASCII control codes */
#define ETX	 0x03			/* end of text			   */
#define SUB	 0x1A			/* substitute			   */

#define MAXBUF	 (132 + 1)		/* maximum input line length + 1   */
#ifdef CPM
#define MAXLINEC  512			/* maximum number of lines in an   */
#else					/* input file before merge files   */
#define MAXLINEC 1024			/* start to be created		   */
#endif
#define MAXTMP	   18			/* the maximum of temporary files  */
					/* that will be created. Note that */
					/* fp's are needed for stdout, and */
					/* stderr during output 	   */

#define isnum(cl) (isdigit(cl) || (cl) == '-')
#ifdef CPM
#define _toupper(c) toupper (c)
#endif

/*------------------------------------------------------------------------------
 *	Variables for "getargs ()". The immediately following variables will be
 *	modified by "getargs ()" according to what flags it finds on the command
 *	line.
 */

static BOOL Noblanks;			/* blanks don't count          */
static BOOL Numeric;			/* sort numbers by val	       */
static int  Primary;			/* primary   sort key	       */
static int  Secondary;			/* secondary sort key	       */
static BOOL Dictorder;			/* use dictionary order        */
static BOOL Foldupper;			/* fold upper case	       */
static BOOL Reverse;			/* sort in reverse order       */
static CHAR Delim;			/* field separator	       */
static CHAR *Mdir = "";                 /* put temp files here         */
static BOOL Nodups;			/* don't print duplicate lines */

static ARG Argtab[] =
  {{'b', BOOLEAN  , &Noblanks , "ignore leading whitespace (blanks)"},
   {'d', BOOLEAN  , &Dictorder, "sort in dictionary order"          },
   {'f', BOOLEAN  , &Foldupper, "fold upper into lower case"        },
   {'n', BOOLEAN  , &Numeric  , "sort numbers by numeric value"     },
   {'p', INTEGER  , &Primary  , "use field <num> as primary key"    },
   {'r', BOOLEAN  , &Reverse  , "do a reverse sort"                 },
   {'s', INTEGER  , &Secondary, "use field <num> as secondary key"  },
   {'t', CHARACTER, &Delim    , "use <c> to separate fields"        },
#ifndef CPM
   {'T', STRING   , &Mdir     , "prepend <str> to temp file names"  },
#endif
   {'u', BOOLEAN  , &Nodups   , "delete duplicate lines in output"  }};

#define NUMARGS (sizeof Argtab / sizeof(ARG))

/*------------------------------------------------------------------------------
 *	Global variables. The "Lines" array is used for the initial sorting.
 */

static int  numpasses;    		/* number of merge files used	  */
static BOOL Options;			/* set by main if any options set */
static CHAR *Lines[MAXLINEC];		/* holds array of input lines	  */
static int  Linec;			/* # of items in Lines		  */
static CHAR **Argv;			/* copies of argv and argc	  */
static int  Argc;

/*------------------------------------------------------------------------------
 *	The heap used in the merge process.
 */

typedef struct {
		 CHAR string[MAXBUF];	/* one line from the merge file */
		 FILE *file;		/* pointer to input file	*/
	       } HEAP;

static HEAP *Heap[MAXTMP];		/* the heap itself		*/

/* forward declaration */
extern BOOL isdigit ();

/* stdlib functions */
extern FILE *fopen  ();
extern CHAR *malloc ();

/*----------------------------------------------------------------------------*/

#ifndef DEBUG
#define pheap(s,n)
#else
static pheap (str, n)
  CHAR *str;
  int n;
{
  int i;

  printf ("+--------------------------\n");
  printf ("| %s, heap is:\n", str);
  for (i = 0; i < n; i++)
    printf ("|%02d: %s", i, *Heap[i]->string ? Heap[i]->string : "(null)\n");
  printf ("+--------------------------\n");
}
#endif

/*----------------------------------------------------------------------------*/

static int dedupe (argc, argv)
  int argc;
  register CHAR **argv;
{
  /* Compress an argv-like array of pointers to strings so that adjacent
   * duplicate lines are eliminated. Return the argument count after the
   * compression.
   */

  register int	i;			/* loop counter 	    */
	   int	nargc;			/* # of not duplicate lines */
	   CHAR **dp;			/* ^compressed poiner array */

  nargc = 1;
  dp	= &argv[1];

  for (i = 1; i < argc; i++)
  {
    if (strcmp(argv[i - 1], argv[i]))
    {
      *dp++ = argv[i];
      nargc++;
    }
    else free (argv[i]);
#ifdef CPM
    cntrl_c ();
#endif
  }
  return nargc;
}

/*----------------------------------------------------------------------------*/

static CHAR *skip_field (n, str)
  register int n;
  register CHAR *str;
{
  /* Skip over "n" fields. The delimiter is in the global variable "Delim".
   * Return a pointer to either the character to the right of the delimiter, or
   * to the '\0'.
   */

  while (n > 0 && *str) if (*str++ == Delim) n--;

  return str;
}

/*------------------------------------------------------------------------------
 *		   Comparison functions needed for sorting
 *
 * "ssort ()" will call either "argvcmp ()" or "qcmp ()", passing them pointers
 * to linev entries. "qcmp ()" calls two workhorse functions, "qcmp1 ()" and
 * "qcmp2 ()". The workhorse functions will also be called by the "reheap ()"
 * subroutine used to manipulate merge files.
 */

static int argvcmp (s1p, s2p)
  CHAR **s1p, **s2p;
{
#ifdef CPM
  cntrl_c;
#endif

  return strcmp(*s1p, *s2p);
}

/*----------------------------------------------------------------------------*/

static int qcmp (str1p, str2p)
  CHAR **str1p, **str2p;
{
  /* Takes care of all the sorting of fields, calling qcmp1 to do the actual
   * comparisons. Assuming here that "Secondary" won't be set unless "Primary"
   * is set too.
   */

  return qcmp1(*str1p, *str2p);
}

/*----------------------------------------------------------------------------*/

static int qcmp1 (str1, str2)
  register CHAR *str1, *str2;
{
  /* Workhorse comparison function. Takes care of sorting fields. If a primary
   * sort field is specified then "qcmp1 ()" skips to that field and calls
   * "qcmp2 ()" to do the actual comparison. If the primary fields are equal,
   * then the secondary fields are compared in the same way.
   */

  int rval;				/* return value */

  if (!Primary) return qcmp2(str1, str2);
  else
  {
    if (!(rval =
	 qcmp2(skip_field(Primary - 1, str1), skip_field(Primary - 1, str2))) &&
	Secondary)

      /* The two primary keys are equal, search the secondary keys if one is
       * specified.
       */
      rval =
	qcmp2(skip_field(Secondary - 1, str1), skip_field(Secondary - 1, str2));

    return rval;
  }
}

/*----------------------------------------------------------------------------*/

static int qcmp2 (str1, str2)
  CHAR *str1, *str2;
{
  /* Workhorse comparison function. Deals with all command line options except
   * fields. Returns
   *
   *		0	 if str1 == str2
   *		positive if str1  > str2
   *		negative if str1  < str2
   *
   * This is a general purpose (and therefore relatively slow) routine. Use
   * "strcmp ()" if you need a fast compare. Comparison stops on reaching end of
   * string or on encountering a "Delim" character (if one exists). So make sure
   * "Delim" is set to '\0' if you're not sorting by fields.
   */

  register unsigned c1, c2;		/* temporary string characters */
	   BOOL     isalnum  ();	/* alphanumeric?	       */
	   BOOL     isspace  ();	/* whitespace?		       */
#ifndef CPM
	   CHAR     _toupper ();	/* convert to upper case       */
	   CHAR     conv_umlaut ();	/* convert German umlaut       */
#else
  cntrl_c ();
#endif

  if (Noblanks)
  {
    /* Skip past leading whitespace in both strings. */
    while (isspace(*str1)) str1++;
    while (isspace(*str2)) str2++;
  }

  do
  {
    if (Numeric && isnum(*str1) && isnum(*str2))

      /* Add 0xFF to the two numeric values so that c1 and c2 can't be confused
       * with a "Delim" character later on.
       */
      if ((c1 = stoi(&str1) + 0xFF) == (c2 = stoi(&str2) + 0xFF)) continue;
							     else break;

    c1 = *str1++;
    c2 = *str2++;

    if (Dictorder)
    {
      /* Skip past any non-alphanumeric or blank characters. */
      while (c1 && !isalnum(c1)) c1 = *str1++;
      while (c2 && !isalnum(c2)) c2 = *str2++;
    }

    if (Foldupper)
    {
      /* Map c1 and c2 to upper case. */
      c1 = _toupper(c1);
      c2 = _toupper(c2);
    }

#ifndef CPM
    if (Dictorder)
    {
      /* process German umlauts */
      c1 = conv_umlaut(c1);
      c2 = conv_umlaut(c2);
    }
#endif

    /* Keep processing while the characters are the same unless we've reached
     * end of string or a delimiter.
     */
  }
  while (c1 == c2 && c1 && c1 != Delim);

  if (Delim)				/* If we're sorting on a field */
  {					/* and we've found a delimiter */
    if (c1 == Delim) c1 = 0;		/* then map the delimiter to a */
    if (c2 == Delim) c2 = 0;		/* zero for purposes of        */
  }					/* comparison.		       */

  return Reverse ? c2 - c1 : c1 - c2;
}

/*----------------------------------------------------------------------------*/

static FILE *nextfile ()
{
  /* Return a file pointer for the next input file or NULL if no more input
   * files exist (i.e. all of the files on the command line have been processed)
   * or a file can't be opened. In this last case print an error message. If
   * Argc == 1 the first time we're called, the standard input is returned (the
   * first time only, NULL is returned on subsequent calls).
   */

	 FILE *fp;			/* ^next input file    */
  static BOOL first_time = TRUE;	/* first time entered? */

  if (first_time)
  {
    first_time = FALSE;
    if (Argc == 1)
#ifdef CPM
    {
      if (!(fp = fopen("CON:", "r"))) printf ("\nCan't open CON:\n");

      return fp;
    }
#else
		   return stdin;
#endif
  }

  if (Argc-- > 1)
  {
    if (!(fp = fopen(*++Argv, "r")))
#ifdef CPM
       printf (        "\nCan't open %s\n", *Argv);
#else
      fprintf (stderr, "\nCan't open %s\n", *Argv);
#endif
    return fp;
  }
  return NULL;
}

/*----------------------------------------------------------------------------*/

static BOOL gtext ()
{
  /* Get text from the appropriate input source and put the lines into linev,
   * updating "Linec". Return non-zero if more input remains. Note that non-zero
   * will be returned if there are exactly MAXLINEC lines in the input, even
   * though there isn't any more actual input available. If "malloc ()" can't
   * get space for the line, we'll remember the line in buf and return TRUE (1).
   */

  static   FILE *fp;			/* current input file */
  static   CHAR buf[MAXBUF];		/* input buffer       */
  register int	maxcount;		/* line counter       */
  register CHAR **lv;			/* ^line vectors      */

  /* This is only true the first time we're called. */
  if (!fp) fp = nextfile();

  lv	= Lines;
  Linec = 0;

  for (maxcount = MAXLINEC; --maxcount >= 0;)
  {
    if (!*buf) while (!fgets(buf, MAXBUF, fp))
	       {
		 fclose (fp);

		 if (!(fp = nextfile())) return FALSE;	/* no more input */
	       }

    if (*lv = malloc(strlen(buf) + 1))
    {
      strcpy (*lv++, buf);
      *buf = '\0';
      Linec++;
    }
    else return TRUE;
#ifdef CPM
    cntrl_c ();
#endif
  }
  return maxcount < 0;	    /* return TRUE if there's more input to get */
}

/*----------------------------------------------------------------------------*/

static CHAR *fname (num)
  int num;
{
  /* Return a merge file name for the indicated merge pass. */

  static CHAR name[16]; 		 /* merge file name */

  if (num > MAXTMP)
  {
#ifdef CPM
     printf (	     "\nInput file(s) too large\n");
#else
    fprintf (stderr, "\nInput file(s) too large\n");
#endif
    exit    (2);
  }

  sprintf (name, "%sMERGE.%d", Mdir, num);
  return name;
}

/*----------------------------------------------------------------------------*/

static outtext (passnum, more_to_go)
  int passnum;
  BOOL more_to_go;
{
  /* Print out all the strings in the "Lines" array and free all the memory that
   * they use. Output is sent to standard output if this is pass 1 and there's
   * no more input to process, otherwise output is sent to a merge file.
   */

  register CHAR **lv;			/* ^line vectors      */
  register FILE *fp;			/* current merge file */

  if (passnum == 1 && !more_to_go)
#ifdef CPM
  {
    if (!(fp = fopen("CON:", "w")))
    {
      printf ("\nCan't open CON:\n");
      exit   (2);
    }
  }
#else
				   fp = stdout;
#endif
  else if (!(fp = fopen(fname(passnum), "w")))
       {
#ifdef CPM
	  printf (	  "\nCan't create %s\n", fname(passnum));
#else
	 fprintf (stderr, "\nCan't create %s\n", fname(passnum));
#endif
	 exit	 (2);
       }

  for (lv = Lines; --Linec >= 0;)
  {
    fputs (*lv, fp);
    free  (*lv++);
#ifdef CPM
    cntrl_c ();
#endif
  }
#ifdef CPM
  fputc (SUB, fp);
#endif
  fclose (fp);
}

/*----------------------------------------------------------------------------*/

static open_mergefiles (nfiles)
  int nfiles;
{
  /* Open all the merge files and create the heap. "nfiles" merge-files exist
   * and the heap will have that many elements in it. The heap is unsorted on
   * exit.
   */

  register HEAP **hp;			/* pointer to heap    */
  register int	i;			/* merge file counter */

  for (hp = Heap, i = nfiles; i > 0; hp++, i--)
  {
    if (!(*hp = (HEAP *)malloc(sizeof(HEAP))))
    {
#ifdef CPM
       printf (        "\nOut of memory\n");
#else
      fprintf (stderr, "\nOut of memory\n");
#endif
      exit    (2);
    }

    if (!((*hp)->file = fopen(fname(i), "r")))
    {
#ifdef CPM
       printf (        "\nCan't open %s\n", fname(i));
#else
      fprintf (stderr, "\nCan't open %s\n", fname(i));
#endif
      exit    (2);
    }

    if (!fgets((*hp)->string, MAXBUF, (*hp)->file))
    {
#ifdef CPM
       printf (        "\nMerge file %s is empty\n", fname(i));
#else
      fprintf (stderr, "\nMerge file %s is empty\n", fname(i));
#endif
      exit    (2);
    }
  }
}

/*----------------------------------------------------------------------------*/

static int mcmp (hpp1, hpp2)
  HEAP **hpp1, **hpp2;
{
  /* Comparison routine for sorting the heap. Is passed two pointers to HEAP
   * pointers and compares the string fields of these using the same workhorse
   * functions used in the initial sorting phase.
   */

#ifdef CPM
  cntrl_c ();
#endif

  return Options ? qcmp1 ((*hpp1)->string, (*hpp2)->string)
		 : strcmp((*hpp1)->string, (*hpp2)->string);
}

/*----------------------------------------------------------------------------*/

static reheap (nfiles)
  int nfiles;
{
  /* Reheap the "Heap", assume that the first element (**Heap) is the newly
   * added one.
   */

  register int	parent, child;		/* father & son pointers in heap */
	   HEAP *tmp;			/* temporary heap pointer	 */

  for (parent = 0, child = 1; child < nfiles;)
  {
    /* Find the smaller child. The if the parent is less than the smaller child,
     * we're done. Otherwise swap the parent and child, and continue the
     * reheaping process with a new parent.
     */

    if (child + 1 < nfiles)		/* if child + 1 is in the heap */
      if (mcmp(&Heap[child], &Heap[child + 1]) > 0) child++;

    if (mcmp(&Heap[parent], &Heap[child]) <= 0) break;

    tmp 	 = Heap[parent];		/* exchange	      */
    Heap[parent] = Heap[child ];
    Heap[child ] = tmp;

    child	 = (parent = child) << 1;	/* child = parent * 2 */
  }
}

/*----------------------------------------------------------------------------*/

static merge (nfiles)
  register int nfiles;
{
  CHAR line[MAXBUF];			/* holds line last output */

  open_mergefiles (nfiles);
  ssort 	  (Heap, nfiles, sizeof(HEAP *), mcmp);
  *line = '\0';

  while (nfiles > 0)
  {
    pheap ("Merge: top of while loop", nfiles);

    if (!Nodups || strcmp(line, (*Heap)->string))
    {
#ifdef CPM
       puts    ((*Heap)->string);
       cntrl_c ();
#else
      fputs ((*Heap)->string, stdout);
#endif
      strcpy (line, (*Heap)->string);
    }

    if (!fgets((*Heap)->string, MAXBUF, (*Heap)->file))
    {
      /* This input stream is exhausted. Reduce the heap size to compensate.
       * Note that Heap + 1 is the same as &Heap[1].
       */

      fclose ((*Heap)->file);
      if (--nfiles) memcpy (Heap, Heap + 1, nfiles * sizeof(HEAP));
    }
    reheap (nfiles);
  }
}

/*----------------------------------------------------------------------------*/

static adjust_args ()
{
  /* Adjust various default arguments to fix mistakes made on the command line.
   * In particular "Delim" is always 0 unless either "Primary" or "Secondary"
   * was set. If a secondary field is specified without a primary, then 1 is
   * assumed for the primary. If no "Delim" is specified then tab (\t) is
   * assumed. "Options" is TRUE if any of the options that affect the sort order
   * were specified on the command line.
   */

  if (!(Primary || Secondary)) Delim = 0;
  else
  {
    if (!Delim	) Delim   = '\t';
    if (!Primary) Primary = 1;
  }
  Options = Noblanks || Numeric || Dictorder || Foldupper || Reverse || Delim;
}

/*----------------------------------------------------------------------------*/

main (argc, argv)
  int argc;
  CHAR **argv;
{
  register BOOL more_input;		/* TRUE if input isn't exhausted */

  Argc = getargs(argc, argv, Argtab, NUMARGS);
  Argv = argv;
  adjust_args ();

  do
  {
    more_input = gtext();

    if (Linec)
    {
      ssort (Lines, Linec, sizeof *Lines, Options ? qcmp : argvcmp);
      if (Nodups) Linec = dedupe(Linec, Lines);
      outtext (++numpasses, more_input);
    }
  }
  while (more_input);

  if (numpasses > 1)			/* merge files were created	    */
  {
#ifndef CPM
    fclose (stdin);			/* free up default file descriptors */
    fclose (stdaux);			/* for unused streams so that they  */
    fclose (stdprn);			/* can be used for merge files	    */
#endif
    merge (numpasses);

    for (; numpasses > 0; numpasses--) unlink (fname(numpasses));
  }
}

/*----------------------------------------------------------------------------*/

#ifndef CPM
static BOOL isalnum (c)
  CHAR c;
{
  BOOL isalpha ();			/* alpha character? */

  return isdigit(c) || isalpha(c);
}

/*----------------------------------------------------------------------------*/

static BOOL isalpha (c)
  register CHAR c;
{
  c = _toupper(c);
  if ('A' <= c && c <= 'Z') return TRUE;
  switch ((unsigned)c)			/* German umlauts */
  {
    case 0x8E:
    case 0x99:
    case 0x9A:
    case 0xE1: return TRUE;
  }
  return FALSE;
}

/*----------------------------------------------------------------------------*/

static BOOL isdigit (c)
  CHAR c;
{
  return '0' <= c && c <= '9';
}

/*----------------------------------------------------------------------------*/

static BOOL isspace (c)
  CHAR c;
{
  switch (c)
  {
    case '\b':
    case '\t':
    case '\n':
    case '\f':
    case '\r':
    case ' ' : return TRUE;
  }
  return FALSE;
}

/*----------------------------------------------------------------------------*/

static CHAR _toupper (c)
  register CHAR c;
{
  switch (c = toupper(c))		/* German umlauts */
  {
    case 0x81: c = 0x9A;
	       break;
    case 0x84: c = 0x8E;
	       break;
    case 0x94: c = 0x99;
  }
  return c;
}

/*----------------------------------------------------------------------------*/

static CHAR conv_umlaut (c)
  register CHAR c;
{
  switch (c)
  {
    case 0x81: c = 'u';
	       break;
    case 0x84: c = 'a';
	       break;
    case 0x8E: c = 'A';
	       break;
    case 0x94: c = 'o';
	       break;
    case 0x99: c = 'O';
	       break;
    case 0x9A: c = 'U';
	       break;
    case 0xE1: c = 's';
  }
  return c;
}
#else

/*----------------------------------------------------------------------------*/

/* CNTRL_C - If any character was hit, and that character is a ^C, then abort.
 */

static cntrl_c ()
{
  if (bdos(11) && (bdos(1) & 0x7F) == ETX) exit (1);
}
#endif

/*----------------------------------------------------------------------------*/

static exit (status)
  int status;
{
  fclosall ();
  for (; numpasses > 0; numpasses--) unlink (fname(numpasses));
  _exit (status);
}
