/*******************************************************************************
*  S T O I  *  T O O L S 0 1 5	*  T h o m a s	 H o l t e   *	 8 6 0 9 8 6   *
********************************************************************************
*
* Version 1.0 by Thomas Holte
*/

int stoi (instr)
  char **instr;
{
  /* Convert string to integer updating "*instr" to point past the number.
   * Return the integer value represented by the string.
   */

  register int	num  = 0;		/* evaluated number   */
  register char *str;			/* running string ptr */
	   int	sign = 1;		/* sign of num	      */

  str = *instr;

  if (*str == '-')
  {
    sign = -1;
    str++;
  }

  while ('0' <= *str && *str <= '9') num = num * 10 + *str++ - '0';

  *instr = str;
  return num * sign;
}
