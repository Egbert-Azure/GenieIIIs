/*******************************************************************************
*  T O O L S  *  G R E P 0 1 0	*  T h o m a s	 H o l t e   *	 8 6 0 9 1 6   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*------------------------------------------------------------------------------
*		TOOLS.C: The expression parser used by GREP
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

#define CPM

#include <stdio.h>
#include "tools.h"

/* This module contains the various routines needed by grep to match regular
 * expressions. Routines are ordered alphabeticaly.
 */

static CHAR *amatch (lin, pat, boln)
  CHAR *lin, *boln;
  TOKEN *pat;
{
  /* Scans through the pattern template looking for a match with "lin". Each
   * element of "lin" is compared with the template until either a mis-match is
   * found or the end of the template is reached. In the former case a 0 is
   * returned; in the latter, a pointer into "lin" (pointing to the last
   * character in the matched pattern) is returned.
   *
   *	  "lin"  is a pointer to the line being searched.
   *	  "pat"  is a pointer to a template made by makepat ().
   *	  "boln" is a pointer into "lin" which points at the character at
   *		 the beginning of line.
   */

	   CHAR *max ();		/* compares pointers		      */
  register CHAR *bocl;
  register CHAR *rval;			/* return value from recursive amatch */
	   CHAR *strstart;		/* holds initial ^lin		      */

  if (!pat) return NULL;
  strstart = lin;

  while (pat)
    if (pat->tok == CLOSURE && pat->next)
    {
      /* Process a closure:
       * First skip over the closure token to the object to be repeated. This
       * object can be a character class.
       */

      pat = pat->next;

      /* Now match as many occurrences of the closure pattern as possible. */
      bocl = lin;
      while (*lin && omatch(&lin, pat, boln));

      /* "lin" now points to the character that made us fail. Now go on to
       * process the rest of the string. A problem here is a character following
       * the closure which could have been in the closure.
       * For example, in the pattern "[a-z]*t" (which matches any lower-case
       * word ending in a t), the final 't' will be sucked up in the while loop.
       * So, if the match fails, we back up a notch and try to match the rest of
       * the string again, repeating this process recursively until we get back
       * to the beginning of the closure. The recursion goes, at most, two
       * levels deep.
       */

      if (pat = pat->next)
      { 						       /* success */
	while (bocl <= lin) if (rval = amatch(lin, pat, boln)) return rval;
			    else lin--;

	return NULL;			/* match failed */
      }
    }
    else if (omatch(&lin, pat, boln)) pat = pat->next; else return NULL;

  /* Note that omatch () advances "lin" to point at the next character to be
   * matched. Consequently, when we reach the end of the template, "lin" will be
   * pointing at the character following the last character matched. The
   * exceptions are templates containing only a BOLN or EOLN token. In these
   * cases omatch doesn't advance.
   *
   * So, decrement "lin" to make it point at the end of the matched string.
   * Then, check to make sure that we haven't decremented past the beginning of
   * the string.
   *
   * A philosophical point should be mentioned here. Is $ a position or a
   * character? (I.e. does $ mean the EOL character itself or does it mean the
   * character at the end of the line.) I decided here to make it mean the
   * former, in order to make the behavior of amatch () consistent. If you give
   * amatch the pattern ^$ (match all lines consisting only of an end of line)
   * then, since something has to be returned, a pointer to the end of line
   * character itself is returned.
   *
   * One final point. If you use a macro instead of a real subroutine to define
   * max (), then take the --lin out of the macro call to avoid side-effects
   * ("lin" being decremented twice).
   */

  return max(strstart, --lin);
}

/*----------------------------------------------------------------------------*/

static CHAR *dodash (delim, src, map)
  int delim;
  CHAR *src, *map;
{
  /* Expand the set pointed to by "src" into "map". Stop at "delim". Return 0
   * on error or size of character class on success. Update "src" to point at
   * "delim". A set can have one element {x} or several elements
   * ({abcdefghijklmnopqrstuvwxyz} and {a-z} are equivalent). Note that the dash
   * notation is expanded as sequential numbers. This means (since we are using
   * the ASCII character set) that a-Z will contain the entire alphabet plus the
   * symbols: [\]^_`.
   */

  register int	first;			/* first character of set */
  register int	last;			/* last  character of set */
	   CHAR *start; 		/* start of set 	  */

  start = src;

  while (*src && *src != delim)
	 if (*src != '-'                         ) setbit (esc(&src), map, 1);
    else if ( src == start || *(src + 1) == delim) setbit (	 '-', map, 1);
    else
	 {
	   src++;
	   if (*src < *(src - 2))
	   {
	     first = *src;
	     last  = *(src - 2);
	   }
	   else
	   {
	     first = *(src - 2);
	     last  = *src;
	   }
	   while (++first <= last) setbit (first, map, 1);
	   src++;
	 }

  return src;
}

/*----------------------------------------------------------------------------*/

static int esc (s)
  register CHAR **s;
{
  register int rval;			/* return value */

  /* Map escape sequences into their equivalent symbols. Returns the correct
   * ASCII character. If no escape prefix is present then "s" is untouched and
   * "**s" is returned, otherwise *s is advanced to point at the escaped
   * character and the translated character is returned.
   */

  if (**s != ESCAPE) rval = **s;
  else switch (toupper(*++*s))
       {
	 case '\0': rval = ESCAPE; break;
	 case 'S' : rval = ' '   ; break;
	 case 'N' : rval = '\n'  ; break;
	 case 'T' : rval = '\t'  ; break;
	 case 'B' : rval = '\b'  ; break;
	 case 'R' : rval = '\r'  ; break;
	 default  : rval = **s;
       }
  (*s)++;
  return rval;
}

/*----------------------------------------------------------------------------*/

CHAR *in_string (delim, str)
  register int delim;
  register CHAR *str;
{
  /* Return a pointer to "delim" if it is in the string, 0 if it is not. */
  while (*str && *str != delim)
  {
    if (*str == ESCAPE && *(str + 1) && *(str + 1) == delim) str++;
							     str++;
  }

  return *str ? str : NULL;
}

/*----------------------------------------------------------------------------*/

TOKEN *makepat (arg, delim)
  CHAR *arg;
  int delim;
{
  /* Make a pattern template from the string pointed to by "arg". Stop when
   * "delim" or '\0' or '\n' is found in "arg". Return a pointer to the pattern
   * template. The pattern templates used here are somewhat different than those
   * used in the book; each token is a structure of the form TOKEN (see tools.h).
   * A token consists of an identifier, a pointer to a string, a literal
   * character and a pointer to another token. This last is 0 if there is no
   * subsequent token.
   *
   * The one strangeness here is caused (again) by CLOSURE which has to be put
   * in front of the previous token. To make this insertion a little easier, the
   * 'next' field of the last token in the chain (the one pointed to by 'tail')
   * is made to point at the previous node. When we are finished, "tail->next"
   * is set to 0.
   */

  typedef  char  BITMAP;

	   TOKEN  *calloc ();		/* stdlib routine		     */
	   TOKEN  *head, *tail; 	/* head and tail of token list	     */
  register TOKEN  *ntok;		/* new allocated token		     */
	   CHAR   buf[CLS_SIZE];	/* holds expanded regular expression */
  register BOOL   error;		/* remark error state		     */
	   BITMAP *makebitmap ();	/* make an empty bitmap 	     */
	   CHAR   *errmsg = "\nOut of memory\n";        /* error message     */

  /* Check for characters that aren't legal at the beginning of a template. */
  if (!*arg || *arg == delim || *arg == '\n' || *arg == CLOSURE) return NULL;

  error = FALSE;
  head	= tail = NULL;

  while (*arg && *arg != delim && *arg != '\n' && !error)
  {
    if (error = !(ntok = calloc(TOKSIZE, 1)))
    {
#ifdef CPM
       printf (        errmsg);
#else
      fprintf (stderr, errmsg);
#endif
      exit    (2);
    }

    switch (*arg)
    {
      case ANY	  : ntok->tok = ANY;
		    break;

		       /* then this is the first symbol */
      case BOL	  : if (!head) ntok->tok = BOL; else error = TRUE;
		    break;

      case EOL	  : if (*(arg + 1) == delim || *(arg + 1) == '\0' ||
					       *(arg + 1) == '\n')
			 ntok->tok = EOL;
		    else error	   = TRUE;
		    break;

      case CCL	  : if (*++arg == NEGATE)
		    {
		      ntok->tok = NCCL;
		      arg++;
		    }
		    else ntok->tok = CCL;

		    if (ntok->bitmap = makebitmap(256))
		      arg = dodash(CCLEND, arg, ntok->bitmap);
		    else
		    {
#ifdef CPM
		       printf (        errmsg);
#else
		      fprintf (stderr, errmsg);
#endif
		      exit    (2);
		    }
		    break;

      case CLOSURE: if (head) switch (tail->tok)
			      {
				case BOL    :
				case EOL    :
				case CLOSURE: return NULL;

				default     : ntok->tok = CLOSURE;
			      }
		    break;

      default	  : ntok->tok	= LITCHAR;
		    ntok->lchar = esc(&arg);
		    arg--;
    }

	 if (error || !ntok)
	 {
	   unmakepat (head);
	   return NULL;
	 }
    else if (!head)
	 {
	   /* This is the first node in the chain. */
	   ntok->next  = NULL;
	   head = tail = ntok;
	 }
    else if (ntok->tok != CLOSURE)
	 {
	   /* Insert at end of list (after tail) */
	   tail->next = ntok;
	   ntok->next = tail;
	   tail       = ntok;
	 }
    else if (head != tail)
	 {
	   /* More than one node in the chain. Insert the CLOSURE node
	    * immediately in front of tail.
	    */
	   tail->next->next = ntok;
	   ntok->next	    = tail;
	 }
    else
	 {
	   /* Only one node in the chain. Insert the CLOSURE node at the head
	    * of the linked list.
	    */
	   ntok->next = head;
	   tail->next = ntok;
	   head       = ntok;
	 }

    arg++;
  }

  tail->next = NULL;
  return head;
}

/*----------------------------------------------------------------------------*/

CHAR *matchs (line, pat, ret_endp)
  register CHAR *line;
  TOKEN *pat;
  int ret_endp;
{
  /* Compares "line" and "pat"tern. "line" is a character string while "pat" is
   * a pattern template.
   * Returns:
   *
   *	     1. A zero if no match was found.
   *	     2. A pointer the last character satisfying the match if ret_endp is
   *		non-zero.
   *	     3. A pointer to the beginning of the matched string if ret_endp is
   *		0.
   */

  register CHAR *rval = NULL;		/* return value 	   */
	   CHAR *bptr;			/* points to start of line */

  bptr = line;

  while (*line)
    if (!(rval = amatch(line, pat, bptr))) line++;
    else
    {
      rval = ret_endp ? rval : line;
      break;
    }

  return rval;
}

/*----------------------------------------------------------------------------*/

CHAR *stoupper (str)
  register CHAR *str;
{
  /* Map the entire string pointed to by str to upper case. Return str. */

  CHAR *rval;

  rval = str;

  while (*str)
  {
    if ('a' <= *str && *str <= 'z') *str -= ('a' - 'A');
#ifndef CPM
    else switch (*str)			/* German umlauts on IBM PC */
	 {
	   case 0x81: *str = 0x9A;
		      break;
	   case 0x84: *str = 0x8E;
		      break;
	   case 0x94: *str = 0x99;
	 }
#endif
    str++;
  }
  return rval;
}

/*----------------------------------------------------------------------------*/

static CHAR *max (x, y)
  CHAR *x, *y;
{
  return x > y ? x : y;
}

/*----------------------------------------------------------------------------*/

static int omatch (linp, pat, boln)
  register CHAR **linp;
  CHAR *boln;
  TOKEN *pat;
{
  /* Match one pattern element, pointed at by "pat", with the character at
   * "**linp". Return non-zero on match. Otherwise, return 0. "*linp" is
   * advanced to skip over the matched character; it is not advanced on failure.
   * The amount of the advance is 0 for patterns that match null strings, 1
   * otherwise. "boln" should point at the position that will match a BOL token.
   */

  register int advance; 		/* amount of advance */

  advance = -1;

  if (**linp)
    switch (pat->tok)
    {
      case LITCHAR: if (**linp == pat->lchar	     ) advance = 1;
		    break;

      case BOL	  : if ( *linp == boln		     ) advance = 0;
		    break;

      case ANY	  : if (**linp != '\n'               ) advance = 1;
		    break;

      case EOL	  : if (**linp == '\n'               ) advance = 0;
		    break;

      case CCL	  : if ( testbit(**linp, pat->bitmap)) advance = 1;
		    break;

      case NCCL   : if (!testbit(**linp, pat->bitmap)) advance = 1;
		    break;

      default	  : printf ("omatch: internal error\n");
    }

  if (advance >= 0) *linp += advance;

  return advance + 1;
}

/*----------------------------------------------------------------------------*/
#ifdef DEBUG
pr_tok (head)
  TOKEN *head;
{
  CHAR *str;				/* token string */
  int  i;				/* loop counter */

  /* Print out the pattern template (linked list of TOKENs) pointed to by
   * "head". This is a useful debugging aid. Note that pr_tok () just scans
   * along the linked list, terminating on a null pointer; so, you can't use
   * pr_tok () from inside makepat () because "tail->next" points to the
   * previous node instead of being null.
   */

  for (; head; head = head->next)
  {
    switch (head->tok)
    {
      case BOL	  : str = "BOL";
		    break;

      case EOL	  : str = "EOL";
		    break;

      case ANY	  : str = "ANY";
		    break;

      case LITCHAR: str = "LITCHAR";
		    break;

      case ESCAPE : str = "ESCAPE";
		    break;

      case CCL	  : str = "CCL";
		    break;

      case CCLEND : str = "CCLEND";
		    break;

/*    case NEGATE : str = "NEGATE";
		    break;	      ??????????????????????? */

      case NCCL   : str = "NCCL";
		    break;

      case CLOSURE: str = "CLOSURE";
		    break;

      default	  : str = "**** unknown ****";
    }

    printf ("%-7s at: 0x%04X, ", str, head);

	 if (head->tok == CCL || head->tok == NCCL)
	 {
	   printf ("string (at 0x%04X) = <", head->bitmap);
	   for (i = 0; i < 0x7F; i++) if (testbit(i, head->bitmap))
					printf ("%c", i);
	   printf (">, ");
	 }
    else if (head->tok == LITCHAR)
	   printf ("lchar = %c, ", head->lchar);

    printf ("next = 0x%04X\n", head->next);
  }
  printf ("\n");
}
#endif
/*----------------------------------------------------------------------------*/

static unmakepat (head)
  register TOKEN *head;
{
  /* Free up the memory used for the token string */

  register TOKEN *old_head;

  while (head)
    switch (head->tok)
    {
      case CCL :
      case NCCL: free (head->bitmap);
		 /* no break, fall through to default */

      default  : old_head = head;
		 head	  = head->next;
		 free (old_head);
    }
}
