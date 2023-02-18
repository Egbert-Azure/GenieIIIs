/*******************************************************************************
*  S S O R T  *  S O R T 0 1 0	*  T h o m a s	 H o l t e   *	 8 6 0 9 2 4   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*	SSORT.C        Works just like "qsort ()" except that a shell sort,
 *		       rather than a quick sort, is used. This is more efficient
 *	than quick sort for small numbers of elements and it's not recursive so
 *	will use much less stack space.
 *
 *	Copyright (c) 1985, Allen T. Holub. All rights reserved
 */

ssort (base, nel, width, cmp)
  char *base;
  int nel, width, (*cmp) ();
{
   register int  i, j;			/* temporary indices	     */
	    int  gap, k, tmp;
	    char *p1, *p2;		/* temporary record pointers */

  for (gap = nel >> 1; gap > 0; gap >>= 1)
    for (i = gap; i < nel; i++)
      for (j = i - gap; j >= 0; j -= gap)
      {
	p1 = base +  j	      * width;
	p2 = base + (j + gap) * width;

	if ((*cmp)(p1, p2) <= 0) break;

	for (k = width; --k >= 0;)
	{
	  tmp	= *p1;
	  *p1++ = *p2;
	  *p2++ = tmp;
	}
      }
}
