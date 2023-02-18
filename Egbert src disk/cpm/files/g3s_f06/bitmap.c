/*******************************************************************************
*  B I T M A P	*  G R E P 0 1 5  *  T h o m a s   H o l t e  *  8 6 0 9 0 9   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

/*	BITMAP.C      makebitmap, setbit, testbit: bit map manipulation
 *		      routines.
 *
 *	Copyright (c) Allen T. Holub, all rights reserved. This program may be
 *	copied for personal, non-profit use only.
 */

#include <stdio.h>

typedef CHAR BITMAP;

/*----------------------------------------------------------------------------*/

BITMAP *makebitmap (size)
  register unsigned size;
{
  /* Make a bit map with "size" bits. The first entry in the map is an unsigned
   * int representing the maximum bit. The map itself is concatenated to this
   * integer. Return a pointer to the map on success, 0 if there's not enough
   * memory.
   */

  register unsigned *map;		/* ^bit map			    */
	   unsigned numbytes;		/* no of bytes required for bit map */
	   unsigned *calloc (); 	/* stdlib routine		    */

  numbytes = (size >> 3) + (size & 7 ? 1 : 0);

#ifdef DEBUG
  printf ("Making a %d bit map (%d bytes required)\n", size, numbytes);
#endif

  if (map = calloc(numbytes + sizeof(unsigned), 1)) *map = size;

  return (BITMAP *)map;
}

BOOL setbit (c, map, val)
  register unsigned c;
  unsigned val;
  register CHAR *map;
{
  /* Set bit c in the map to val. If c > map size, 0 is returned, else 1 is
   * returned.
   */

  if (c >= *(unsigned *)map) return FALSE;	/* if c >= map size */

  map += sizeof(unsigned);			/* skip past size   */

  if (val) map[c >> 3] |=   1 << (c & 7);
      else map[c >> 3] &= ~(1 << (c & 7));

  return TRUE;
}

/*----------------------------------------------------------------------------*/

BOOL testbit (c, map)
  register unsigned c;
  register CHAR *map;
{
  /* Return TRUE (1) if the bit corresponding to c in map is set.
   * 0 if it is not.
   */

  if (c >= *(unsigned *)map) return FALSE;

  map += sizeof(unsigned);

  return map[c >> 3] & 1 << (c & 7);
}

#ifdef DEBUG
main ()
{
  int bitnum, set, i, *map;

  printf ("Making a 32 bit wide bit map\n");

  if (!(map = makebitmap(32))) printf ("Can't make map\n");

  for (;;)
  {
    /* Print the bit map. Try to print past the end of the map to make sure
     * overflow detection works (bit 32 should come back as 0).
     */

    for (i = 0; i <= 32; i++) printf ("%c", testbit(i, map) ? 'X' : '.');

    printf ("\n\nBit number: ");
    scanf  ("%d", &bitnum);
    printf ("\n1 to set, 0 to clear: ");
    scanf  ("%d", &set);

    if (!setbit(bitnum, map, set)) printf ("Bit out of range\n");
  }
}
#endif
