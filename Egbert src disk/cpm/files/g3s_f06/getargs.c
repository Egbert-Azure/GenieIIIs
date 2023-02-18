/*******************************************************************************
*  G E T A R G S  *  T O O L S 0 1 0  *  T h o m a s   H o l t e * 8 6 0 9 2 1 *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*	GETARGS.C      Command line argument processor for C program
 *
 *	   (C) Copyright 1985, Allen T. Holub. All rights reserved.
 *	 This program may be copied for personal, non-profit use only.
 */

#define CPM

#include <stdio.h>
#include "getargs.h"

typedef int (*PFI) ();

static char *setarg (argp, linep)
  ARG *argp;
  char *linep;
{
  /* Set an argument. "argp" points at the argument tyble entry corresponding to
   * "*linep". Return "linep", updated to point past the argument being set.
   */

  linep++;

  switch (argp->type)
  {
    case INTEGER  : *(int *)argp->variable   = stoi(&linep);
		    break;
    case BOOLEAN  : *argp->variable	     = TRUE;
		    break;
    case CHARACTER: *argp->variable	     = *linep++;
		    break;
    case STRING   : *(char **)argp->variable = linep;
		    linep		     = "";
		    break;
    case PROC	  : (*(PFI)argp->variable) (linep);
		    linep = "";
		    break;
#ifdef CPM
    default	  :  printf (        "\nInternal error: Bad argument type\n");
#else
    default	  : fprintf (stderr, "\nInternal error: Bad argument type\n");
#endif
  }
  return linep;
}

/*----------------------------------------------------------------------------*/

static ARG *findarg (c, tabp, tabsize)
  int c, tabsize;
  register ARG *tabp;
{
  /* Return pointer to argument table entry corresponding to c (or 0 if c isn't
   * in table).
   */

  for (; --tabsize >= 0; tabp++) if (
#ifdef CPM
				     toupper(
#endif
					     tabp->arg
#ifdef CPM
						      )
#endif
						        == c) return tabp;
						     	      return NULL;
}


static pr_usage (tabp, tabsize)
  register ARG *tabp;
  int tabsize;
{
  /* Print the argtab in the form:
   *		-<arg> <errmsg> 	(value is <*variable>)
   */

  for (; --tabsize >= 0; tabp++)
    switch (tabp->type)
    {
#ifdef CPM
      case INTEGER  : printf ("-%c<num> %-40s (value is ", 
			      tabp->arg, tabp->errmsg);
		      printf ("%-5d)\n", *(int *)tabp->variable);
		      break;

      case BOOLEAN  : printf ("-%c      %-40s (value is ",
			      tabp->arg, tabp->errmsg);
		      printf ("%-5s)\n", *tabp->variable ? "TRUE" : "FALSE");
		      break;

      case CHARACTER: printf ("-%c<c>   %-40s (value is ",
			       tabp->arg, tabp->errmsg);
		      printf ("%-5c)\n", *tabp->variable);
		      break;

      case STRING   : printf ("-%c<str> %-40s (value is ",
			       tabp->arg, tabp->errmsg);
		      printf ("<%s>)\n", *(char **)tabp->variable);
		      break;

      case PROC     : printf ("-%c<str> %-40s\n", tabp->arg, tabp->errmsg);
#else
      case INTEGER  : fprintf (stderr, "-%c<num> %-40s (value is ",
			       tabp->arg, tabp->errmsg);
		      fprintf (stderr, "%-5d)\n", *(int *)tabp->variable);
		      break;

      case BOOLEAN  : fprintf (stderr, "-%c      %-40s (value is ",
			       tabp->arg, tabp->errmsg);
		      fprintf (stderr, "%-5s)\n",
			       *tabp->variable ? "TRUE" : "FALSE");
		      break;

      case CHARACTER: fprintf (stderr, "-%c<c>   %-40s (value is ",
			       tabp->arg, tabp->errmsg);
		      fprintf (stderr, "%-5c)\n", *tabp->variable);
		      break;

      case STRING   : fprintf (stderr, "-%c<str> %-40s (value is ",
			       tabp->arg, tabp->errmsg);
		      fprintf (stderr, "<%s>)\n", *(char **)tabp->variable);
		      break;

      case PROC     : fprintf (stderr, "-%c<str> %-40s\n",
			       tabp->arg, tabp->errmsg);
#endif
    }
}


#define ERRMSG "\nIllegal argument <%c>. Legal arguments are:\n\n"

int getargs (argc, argv, tabp, tabsize)
  int argc, tabsize;
  char **argv;
  ARG *tabp;
{
  /* Process command line arguments. Stripping all command line switches out of
   * argv. Return a new argc. If an error is found exit (2) is called (getargs
   * won't return) and a usage message is printed showing all arguments in the
   * table.
   */

  register int	nargc;			/* new argc		     */
  register char **nargv;		/* new argv		     */
	   char *p;			/* temporary pointer	     */
	   ARG	*argp;			/* ptr running through table */

  nargc = 1;
  for (nargv = ++argv; --argc > 0; argv++)
    if (**argv != '-')
    {
      *nargv++ = *argv;
      nargc++;
    }
    else
    {
      p = *argv + 1;

      while (*p)
	if (argp = findarg(*p, tabp, tabsize)) p = setarg(argp, p);
	else
	{
#ifdef CPM
	   printf  (        ERRMSG, *p);
#else
	  fprintf  (stderr, ERRMSG, *p);
#endif
	  pr_usage (tabp, tabsize);
	  exit	   (2);
	}
    }
  return nargc;
}
