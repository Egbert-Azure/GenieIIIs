/*******************************************************************************
*  F G R E P  *  G R E P 0 2 0	*  T h o m a s	 H o l t e   *	 8 6 0 9 2 2   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/* FGREP.C - Search File(s) For Fixed Pattern(s)
 *
 * Version 1.03       February 11th, 1985
 *
 * Modifications:
 *
 *   V1.00 (84/12/01)	 - beta test release
 *   V1.01 (85/01/01)	 - added -P option
 *			 - improved command line validation
 *   V1.02 (85/01/06)	 - modified "run_fsa ()" and "bd_move ()"
 *   V1.03 (85/02/11)	 - added -S option
 *
 * Copyright 1985:     Ian Ashdown
 *		       byHeart Software
 *		       1089 West 21st Street
 *		       North Vancouver, B.C. V7P 2C6
 *		       Canada
 *
 * This program may be copied for personal, non-commercial use only, provided
 * that the above copyright notice is included in all copies of the source code.
 * Copying for any other use without previously obtaining the written permission
 * of the author is prohibited.
 *
 * Machine readable versions of this program may be purchased for $35.00 (U.S.)
 * from byHeart Software. Supported disk formats are CP/M 8" SSSD and PC-DOS
 * (v2.x) 5-1/4" DSDD.
 *
 * Notes:
 *
 * The algorithm used in this program constructs a deterministic finite state
 * automaton (FSA) for pattern matching from the substrings, then use the FSA to
 * process the text string in one pass. The time taken to construct the FSA is
 * proportional to the sum of the lengths of the substrings. The number of state
 * transitions made by the FSA in processing the next string is independent of
 * the number of substrings.
 *
 * Algorithm Source:
 *
 * "Efficient String Matching: An Aid to Bibliographic Search"
 * Alfred V. Aho & Margaret J. Corasick
 * Communications of the ACM
 * pp. 333 - 340, Vol. 18 No. 6 (June `75)
 *
 * USAGE: fgrep [-cefhlnpsvxy] [strings] <files>
 *
 * where:
 *
 *	-v	All lines but those matching are printed.
 *	-c	Only a count of the matching lines is printed.
 *	-l	The names of the files with matching lines are listed (once),
 *		separated by newlines.
 *	-n	Each line is preceeded by its line number in the file.
 *	-h	Do not print filename headers with output lines.
 *	-y	All characters in the file are mapped to upper case before
 *		matching. (This is the default if the string is given in the
 *		command line under CP/M, as CP/M maps everything on the command
 *		line to lower and upper case. Use the -f option if you need both
 *		lower and upper case.) Not a true UNIX "fgrep" option (normally
 *		available under "grep" only), but too useful to leave out.
 *	-e	<string>. Same as a string argument, but useful when the string
 *		begins with a '-'.
 *	-f	<file>. The strings (separated by newlines) are taken from a
 *		file. If several strings are listed in the file, then a match is
 *		flagged if any of the strings are matched. If -f is given, any
 *		following argument on the command line is taken to be a
 *		filename.
 *	-x	Only lines matched in their entirety are printed.
 *	-p	Each matched line is preceded by the matching substring(s). Not
 *		a UNIX "fgrep" option, but too useful to leave out.
 *	-s	No output is produced, only status. Used when "fgrep" is a run
 *		as a process that returns a status value to its parent process.
 *		Under CP/M, a non-zero value returned by "exit ()" may terminate
 *		a submit file that initiated the program, although this is
 *		implementation-dependent.
 *
 * DIAGNOSTICS:
 *
 * Exit status is 0 if any matches are found, 1 if none, 2 for error condition.
 *
 * BUGS:
 *
 * The following UNIX-specific option is not supported:
 *
 *	-b	Each line is preceded by the block number in which it was found.
 *
 * Lines are limitated to 256 characters.
 */

/*** Definitions ***/
#define MAX_LINE 257			/* Maximum of number of characters per
					 * line plus NULL delimiter.
					 */
#define CPM     			/* Comment out for compilation under
					 * MS-DOS.
					 */
#define CMD_ERR 0			/* Error codes */
#define INP_ERR 1
#define STR_ERR 2
#define MEM_ERR 3

#define ETX 0x03			/* ASCII control code */


/*** Include Files ***/
#include <stdio.h>
#include "getargs.h"


/*** Data Structures ***/

/* Queue element */
typedef struct queue	  {

/* FSA state element */
	struct state_el   {

/* Transition element */
	struct transition {
	CHAR		    lchar;	    /* Transition character	      */
	struct state_el     *nextst_ptr;    /* Transition state pointer       */
	struct transition   *next_el;
			  } *go_ls;	    /* Pointer to head of "go" list   */

	struct transition   *mv_ls;	    /* Pointer to head of "move" list */
	struct state_el     *fail_state;    /* "failure" transition state     */
	CHAR		    *out_str;	    /* Terminal state message (if any)*/
			  } *st_ptr;

	struct queue	    *next_el;
			  } QUEUE;

typedef struct transition TRANSITION;
typedef struct state_el   FSA;


/*** Global Variables and Structures ***/

/* Dummy "failure" state */
static FSA FAIL_STATE;

/* Define a separate data structure for State 0 of the FSA to speed processing
 * of the input while the FSA is in that state. Since the Aho-Corasick algorithm
 * only defines "go" transitions for this state (one for each valid input
 * character) and no "failure" transitions or output messages, only an array of
 * "go" transition state numbers is needed. The array is accessed directly,
 * using the input character as the index.
 */

static FSA *MZ[256];			        /* State 0 of FSA	     */

/* Command-line option flags */
static BOOL cflag;		/* print only count		      	    */
static char *eflag = "";	/* expression begins with "-"	      	    */
static char *fflag = "";	/* file containing regular expression 	    */
static BOOL hflag;		/* don't print filename headers	      	    */
static BOOL lflag;		/* print only filenames		      	    */
static BOOL nflag;		/* print line numbers		      	    */
static BOOL pflag;		/* precede matched lines by matched strings */
static BOOL sflag;		/* non-verbose mode (return only status)    */
static BOOL vflag;		/* print all lines but those matching 	    */
static BOOL xflag;		/* print only lines matched entirely  	    */
static BOOL yflag;		/* force upper case		      	    */

static ARG Argtab[] = 
  {{'c', BOOLEAN, &cflag, "print only count of matching lines"	      },
   {'e', STRING , &eflag, "regular expression <str> begins with \"-\""},
   {'f', STRING , &fflag, "file <str> contains regular expression"    },
   {'h', BOOLEAN, &hflag, "don't print filename headers"	      },
   {'l', BOOLEAN, &lflag, "print only filenames once"		      },
   {'n', BOOLEAN, &nflag, "print line numbers"			      },
   {'p', BOOLEAN, &pflag, "precede matched lines by matched strings"  }, 
   {'s', BOOLEAN, &sflag, "non-verbose mode (return only status)"     },
   {'v', BOOLEAN, &vflag, "print all lines but those matching"	      },
   {'x', BOOLEAN, &xflag, "print only lines matched in entirety"      },
   {'y', BOOLEAN, &yflag, "map all input characters to upper case"    }};

#define NUMARGS (sizeof Argtab / sizeof(ARG))

static BOOL first;			   /* 1st line of file being printed */


/*** Stdlib Functions ***/
extern FILE *fopen  ();
extern CHAR *calloc ();
extern CHAR *strchr ();

/*** Other functions ***/
extern CHAR *stoupper ();		/* map string to upper case */


/*** Main Body of Program ***/
main (argc, argv)
  register int argc;
  register CHAR **argv;
{
  CHAR *temp;				/* command line character */
  BOOL match_flag = FALSE;		/* any matches found?	  */
  BOOL proc_file ();			/* process input file	  */

  /* Parse the command line for user-selected options */
  if ((argc = getargs(argc, argv, Argtab, NUMARGS)) <= 1 && !*eflag && !*fflag)
    error (CMD_ERR, NULL);

  /* "pflag" can only be TRUE if the following flags are FALSE */
  if (vflag || cflag || lflag || xflag || sflag) pflag = FALSE;

  /* Build the "go" transitions */
  argv++;
  argc--;
  if (*eflag) bd_go (eflag);
  else
  {
    bd_go (*argv++);
    argc--;
  }

  /* Build the "failure" and "move" transitions */
  bd_move ();

  /* Process each of the input files if not "stdin" */
  if (argc < 2) hflag = TRUE;
  if (!argc)
  {
    if (proc_file(NULL, FALSE) && !match_flag) match_flag = TRUE;
  }
  else
    while (argc--)
    {
      stoupper (*argv);
      if (proc_file(*argv++, TRUE) && !match_flag) match_flag = TRUE;
    }

  /* Return status to the parent process. Status is zero if any matches are
   * found, 1 if none.
   */
#ifndef CPM
  exit (!match_flag);
#endif
}


/*** Functions and Procedures ***/

/* PROC_FILE - Run the FSA on the input file "in_file". Returns TRUE if a match
 *	       was found, FALSE otherwise.
 */

static BOOL proc_file (in_file, prt_flag)
  CHAR *in_file;
  BOOL prt_flag;
{
	   CHAR buffer[MAX_LINE];	/* input string buffer	       */
	   CHAR *nl;			/* ptr to '\n' in input string */
  register int	line_cnt = 0;		/* line counter 	       */
  register int	mtch_cnt = 0;		/* matched line counter        */
	   BOOL mtch_flag;		/* matched line flag	       */
	   BOOL run_fsa ();		/* search strings	       */
	   FILE *in_fd; 		/* input file descriptor       */
#ifdef CPM
	   CHAR *fname;			/* ^input file name	       */

  if (in_file) fname = in_file; else fname = "CON:";
  
  if (!(in_fd = fopen(fname, "r"))) error (INP_ERR, in_file);
#else
  if (in_file)				/* a file was specified as the input */
  {
    if (!(in_fd = fopen(in_file, "r"))) error (INP_ERR, in_file);
  }
  else in_fd = stdin;
#endif

  first = TRUE;

  /* Read in a line at a time for processing */
  while (fgets(buffer, MAX_LINE, in_fd))
  {
    if (nl = strchr(buffer, '\n')) *nl = '\0';   /* remove newline */
#ifdef CPM
    if (!*fflag || yflag) stoupper (buffer);
#else
    if (	   yflag) stoupper (buffer);
#endif
    line_cnt++; 		     /* increment     the line counter */
				     /* increment matched line counter */
    if (mtch_flag = run_fsa(buffer)) mtch_cnt++;
    if (!cflag && !lflag && !sflag &&
	(mtch_flag && !vflag || !mtch_flag && vflag))
    {
      if (first)
      {
        first = FALSE;
        printf ("\n");
      }
      if (!hflag && prt_flag      ) printf ("%s:"  , in_file);
      if ( 		     nflag) printf ("%04d:", line_cnt);
      if (!hflag || lflag || nflag) printf (" ");
			            printf ("%s\n" , buffer);
    }
#ifdef CPM
    cntrl_c ();
#endif
  }
  if (lflag && mtch_cnt > 0) printf ("%s\n", in_file );
  else 
    if (cflag && !sflag) 
    {
      if (!hflag) printf ("%-12s: ", in_file);
		  printf ("%d\n"   , mtch_cnt);
    }

  if (in_file) fclose(in_fd);

  return mtch_cnt;
}


/* RUN_FSA - Run the finite state automaton with string "str" as input. Return
 *	     TRUE if match, FALSE otherwise.
 */

static BOOL run_fsa (str)
  register CHAR *str;
{
  register FSA	*st_ptr;		/* ^FSA state element */
	   CHAR *message = NULL;	/* ^terminal message  */
	   BOOL msg_flag = FALSE;	/* message printed?   */
	   FSA	*go   ();		/* "go"   transition  */
	   FSA	*move ();		/* "move" transition  */

  st_ptr = NULL;			/* initialize FSA */
  if (!xflag)
  {
    /* Process the next input character in the string */
    while (*str)
    {
      st_ptr = move(st_ptr, *str);

      /* Print terminal state message and update FSA */
      if (!st_ptr && message)
      {
        if (first)
        {
	  printf ("\n");
	  first = FALSE;
        }
	printf ("--> %s\n", message);
	message = NULL;
	st_ptr	= move(st_ptr, *str);
      }
      str++;
      if (st_ptr)
	if (st_ptr->out_str)		/* terminal state? */
	  if (pflag)
	  {
	    /* Save terminal state message */
	    message  = st_ptr->out_str;
	    msg_flag = TRUE;
	  }
	  else return TRUE;
    }
    /* Print any remaining terminal state message */
    if (message) 
    {
      if (first)
      {
	printf ("\n");
	first = FALSE;
      }
      printf ("--> %s\n", message);
    }

    return msg_flag;
  }
  else					/* match exact lines only */
    if (*str)
    {
      while (*str)
        if (!(st_ptr = go(st_ptr, *str++)) || st_ptr == &FAIL_STATE) 
	  return FALSE;			/* line not matched   */
          return TRUE;			/* exact line matched */
    }
    else  return FALSE;
}


/* GO - Process "litchar" and return a pointer to the FSA's corresponding "go"
 *	transition state. If the character is not in the FSA state's "go"
 *	transition list, then return a pointer to FAIL_STATE.
 */

static FSA *go (st_ptr, litchar)
  FSA *st_ptr;
  register CHAR litchar;
{
  register TRANSITION *current; 	/* current ^transition element */

  /* If State 0, then access separate State 0 data structure of the FSA. Note
   * that there are no failure states defined for any input to FSA State 0.
   */

  if (!st_ptr) return MZ[litchar];
  else
  {
    /* Point to the head of the linked list of "go" transitions associated with
     * the state.
     */

    current = st_ptr->go_ls;

    /* Transverse the list looking for a match to the input character */
    while (current)
    {
      if (current->lchar == litchar) break;
      current = current->next_el;
    }

    /* Null value for "current" indicates end of list was reached without having
     * found match to input character.
     */

    return current ? current->nextst_ptr : &FAIL_STATE;
  }
}


/* MOVE - Process "litchar" and return a pointer to the FSA's corresponding
 *	  "move" transition state.
 */

static FSA *move (st_ptr, litchar)
  FSA *st_ptr;
  register CHAR litchar;
{
  register TRANSITION *current;

  /* If State 0, then access separate State 0 data structure of the FSA */
  if (!st_ptr) return MZ[litchar];
  else
  {
    /* Point to the head of the linked list of "move" transitions associated
     * with the state.
     */

    current = st_ptr->mv_ls;

    /* Transverse the list looking for a match to the input character */
    while (current)
    {
      if (current->lchar == litchar) break;
      current = current->next_el;
    }

    /* Null value for "current" indicates end of list was reached without having
     * found match to input character. The returned pointer is then to State 0.
     */

    return current ? current->nextst_ptr : NULL;
  }
}


/* BD_GO - Build the "go" transitions for each state from the command-line
 *	   arguments.
 */

static bd_go (str)
  char *str;
{
  register int  litchar;	    /* character index			   */
  register CHAR *nl;		    /* ^newline character in search string */
	   CHAR buffer[MAX_LINE];   /* line buffer			   */
	   FILE *str_fd;	    /* file containing search strings	   */
	   CHAR *op;		    /* pointer to next delimiter	   */
	   CHAR *in_string ();	    /* get pointer to next delimiter	   */

  /* Initialize FSA State 0 "go" transition array so that every invocation of
   * "go ()" with "state" = 0 initially returns a pointer to FAIL_STATE.
   */

  for (litchar = 1; litchar <= 255; litchar++) MZ[litchar] = &FAIL_STATE;

  /* If the -f option was selected, get the newline-separated strings from the
   * file "str" one at a time and enter them into the FSA. Otherwise, enter the
   * string "str" into the FSA.
   */

  if (*fflag)
  {
    if (!(str_fd = fopen(fflag, "r"))) error (STR_ERR, fflag);

    while (fgets(buffer, MAX_LINE, str_fd))
    {
      if (nl = strchr(buffer, '\n')) *nl = '\0';        /* remove the newline */
      if (yflag) stoupper (buffer);
      enter (buffer);
    }
    fclose (str_fd);
  }
  else
  {
    if (yflag) stoupper (str);

    while (op = in_string('|', str))
    {
      *op = '\0';			   /* terminate first token   */
      nl  = buffer;			   /* copy string into buffer */
      do *nl = esc(&str); while (*nl++);   /* solve escape sequences  */
      enter (buffer);			   /* enter string into FSA   */
      str = op + 1;
    }
    nl = buffer;			   /* copy string into buffer */
    do *nl = esc(&str); while (*nl++);     /* solve escape sequences  */
    enter (buffer);			   /* enter string into FSA   */
  }

  /* For every input character that does not lead to a defined "go" transition
   * from FSA State 0, set the corresponding element in the State 0 "go"
   * transition array to indicate a "go" transition to State 0.
   */

  for (litchar = 1; litchar <= 255; litchar++)
    if (MZ[litchar] == &FAIL_STATE) MZ[litchar] = NULL;
}


/* ENTER - Enter a string into the FSA by running each character in turn through
 *	   the current partially-built FSA. When a failure occurs, add the
 *	   remainder of the string to the FSA as one new state per character.
 *	   (Note that '\0' can never be a valid character - C uses it to
 *	   terminate a string.)
 */

static enter (str)
  char *str;
{
	   FSA	      *s;		/* temporary  FSA state pointer */
	   FSA	      *create  ();	/* create new FSA state 	*/
	   TRANSITION *current; 	/* ^current "go" transition     */
	   TRANSITION *insert  ();	/* append new "go" transition   */
	   CHAR       *strsave ();	/* save string in memory	*/
  register CHAR       *temp;		/* running ^str 		*/
  register FSA	      *st_ptr = NULL;	/* start in FSA State 0 	*/
	   FSA	      *nextst_ptr;	/* running FSA state pointer	*/

  /* Run each character in turn through partially-built FSA until a failure
   * occurs.
   */

  temp = str;
  while ((s = go(st_ptr, *temp)) != &FAIL_STATE)
  {
    temp++;
    st_ptr = s;
  }

  /* Process the remainder of the string */
  while (*temp)
  {
    /* If a new state, then create a new state and insert transition character
     * and "go" transition in current state. (Note special case of FSA State 0.)
     */

    if (!st_ptr) nextst_ptr = MZ[*temp++] = create();
    else
      if (!(current = st_ptr->go_ls))
      {
	nextst_ptr    = create();
	st_ptr->go_ls = insert(nextst_ptr, *temp++);
      }
      else
      {
	/* ... or it was the character that the FSA returned a "failure" for.
	 * Find the tail of the current state's list of "go" transitions, create
	 * a new state and append it to the current state's "go" list.
	 */

	while (current->next_el) current = current->next_el;
	nextst_ptr = create();
	current->next_el = insert(nextst_ptr, *temp++);
      }
      st_ptr = nextst_ptr;
  }

  /* Make string terminal state's output message */
  st_ptr->out_str = strsave(str);
}


/* INSERT - Create a new "go" transition and return a pointer to it. */

static TRANSITION *insert (st_ptr, litchar)
  FSA *st_ptr;
  CHAR litchar;
{
  register TRANSITION *current; 	/* ^new "go" transition */

  if (!(current = (TRANSITION *)calloc(1, sizeof(TRANSITION))))
    error (MEM_ERR, NULL);
  current->lchar      = litchar;
  current->nextst_ptr = st_ptr;
  return current;
}


/* CREATE - Create an FSA state and return a pointer to it. */

static FSA *create ()
{
  FSA *st_ptr; 				/* ^new FSA state */

  if (!(st_ptr = (FSA *)calloc(1, sizeof(FSA)))) error (MEM_ERR, NULL);
  return st_ptr;
}


/* BD_MOVE - Build the "failure" and "move" transitions for each state from the
 *	     "go" transitions.
 */

static bd_move ()
{
  register int        litchar;	   /* character currently being processed */
  register FSA	      *r, *s, *t;  /* temporary FSA state pointers	  */
	   TRANSITION *current;    /* ^added "go" transition              */
	   QUEUE      *first;	   /* pointer to head of queue		  */
	   QUEUE      *last;	   /* pointer to tail of queue		  */

  last = first = NULL;			/* initialize the queue of FSA states */

  /* For each input character with a "go" transition out of FSA State 0, add a
   * pointer to the "go" state to the queue. Note that this will also serve as
   * the "move" transition list for State 0.
   */

  for (litchar = 1; litchar <= 255; litchar++)
    if (s = go(NULL, litchar)) add_queue (&first, &last, s);

  /* While there are still state pointers in the queue, do ... */
  while (first)
  {
    /* Remove State "r" pointer from the head of the queue. */
    r = first->st_ptr;
    delete_queue (&first);

    /* Skip (terminal) state with no "go" transitions */
    if (!r->go_ls) continue;

    /* Make "move" transition list for terminal state same as its "go"
     * tramsition list.
     */
    if (r->out_str) r->mv_ls = r->go_ls;

    /* For every input to State "r" that has a "go" transition to State "s",
     * do ...
     */

    for (litchar = 1; litchar <= 255; litchar++)
    {
      if ((s = go(r, litchar)) != &FAIL_STATE)
      {
	/* If a defined "go" transition exists for State "r" on input "litchar",
	 * add a pointer to State "s" to the end of the queue.
	 */
	add_queue(&first, &last, s);

	/* Calculate the "failure" transition of State "s" using the following
	 * algorithm.
	 */
	t = r->fail_state;
	while (go(t, litchar) == &FAIL_STATE) t = t->fail_state;
	s->fail_state = go(t, litchar);
      }
      else
	/* ... otherwise set the pointer to State "s" to a pointer to the
	 * precalculated "move" transition of State "r"'s failure state on input
	 * "litchar".
	 */
	s = move(r->fail_state, litchar);

      /* Add State "s" as the "move" transition for State "r" on input "litchar"
       * only if it is not State 0 and "r" is not a terminal state.
       */
      if (s && !r->out_str)
	if (!r->mv_ls)			/* first instance of the list? */
	  current = r->mv_ls = insert(s, litchar);
	else				/* No, just another one ...    */
	  current = current->next_el = insert(s, litchar);
    }
  }
}


/* ADD_QUEUE - Add an instance to the tail of a queue */

static add_queue (head_ptr, tail_ptr, st_ptr)
  QUEUE **head_ptr;
  register QUEUE **tail_ptr;
  FSA *st_ptr;
{
  register QUEUE *pq;			/* ^new queue instance */

  /* Allocate the necessary memory and set the variables. */
  if (!(pq = (QUEUE *)calloc(1, sizeof(QUEUE)))) error (MEM_ERR, NULL);

  pq->st_ptr = st_ptr;

  if (!*head_ptr)			/* First instance of the queue? */
    *tail_ptr = *head_ptr = pq;
  else					/* No, just another one ...	*/
    *tail_ptr = (*tail_ptr)->next_el = pq;
}


/* DELETE_QUEUE - Delete an instance from the head of queue */

static delete_queue (head_ptr)
  QUEUE **head_ptr;
{
  *head_ptr = (*head_ptr)->next_el;
}


/* ESC - Map escape sequences into their equivalent symbols. Returns the correct
 *	 ASCII character. If no escape prefix is present then "s" is untouched
 *	 and "**s" is returned, otherwise *s is advanced to point at the escaped
 *	 character and the translated character is returned.
 */

static int esc (s)
  register CHAR **s;
{
  register int rval;			/* return value */

  if (**s != '\\') rval = **s;
  else switch (toupper(*++*s))
       {
	 case '\0': rval = '\\'; break;
	 case 'S' : rval = ' ' ; break;
	 case 'N' : rval = '\n'; break;
	 case 'T' : rval = '\t'; break;
	 case 'B' : rval = '\b'; break;
	 case 'R' : rval = '\r'; break;
	 default  : rval = **s;
       }
  (*s)++;
  return rval;
}

/* IN_STRING - Return a pointer to "delim" if it is in the string, 0 if it is
 *	       not.
 */

static CHAR *in_string (delim, str)
  register int delim;
  register CHAR *str;
{
  while (*str && *str != delim)
  {
    if (*str == '\\' && *(str + 1) && *(str + 1) == delim) str++;
							   str++;
  }

  return *str ? str : NULL;
}


/* STRSAVE - Save a string somewhere in memory */

static CHAR *strsave (str)
  register CHAR *str;
{
  register CHAR *p;			/* ^allocated memory */
	   CHAR *malloc ();		/* stdlib function   */

  if (p = malloc(strlen(str) + 1)) strcpy (p, str); else error (MEM_ERR, NULL);

  return p;
}


/* STOUPPER - Map entire string pointed to by "str" to upper case. */

static CHAR *stoupper (str)
  register CHAR *str;
{
  register CHAR *temp;			/* running pointer to str */

  temp = str;
  while (*temp)
  {
#ifndef CPM
    switch (
#endif
	    *temp = toupper(*temp)
#ifdef CPM
				  ;
#else
				  )
    {					/* German umlauts on IBM PC */
      case 0x81: *temp = 0x9A;
		 break;
      case 0x84: *temp = 0x8E;
		 break;
      case 0x94: *temp = 0x99;
    }
#endif
    temp++;
  }
  return str;
}


/* ERROR - Error reporting. Returns an exit status of 2 to the parent process.
 */

static error (n, str)
  int n;
  CHAR *str;
{
  switch (n)
  {
#ifdef CPM
    case CMD_ERR: printf ("\nNo search string specified\n");
		  break;
    case INP_ERR:
    case STR_ERR: printf ("\nCan't open input file %s\n", str);
		  break;
    case MEM_ERR: printf ("\nOut of memory\n");
#else
    case CMD_ERR: fprintf (stderr, "\nNo search string specified\n");
		  break;
    case INP_ERR:
    case STR_ERR: fprintf (stderr, "\nCan't open input file %s\n", str);
		  break;
    case MEM_ERR: fprintf (stderr, "\nOut of memory\n");
#endif
  }
  exit (2);
}


/* CNTRL_C - If any character was hit, and that character is a ^C, then abort.
 */

#ifdef CPM
static cntrl_c ()
{
  if (bdos(11) && (bdos(1) & 0x7F) == ETX) exit (1);
}
#endif

/*** End of FGREP.C ***/
