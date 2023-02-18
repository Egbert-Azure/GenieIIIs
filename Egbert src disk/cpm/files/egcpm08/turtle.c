/******************************************************************************
*  T U R T L E  * L I B P L O T 0 4 0 * T h o m a s   H o l t e * 8 6 0 4 0 1 *
*******************************************************************************
*
* Version 1.0 by Thomas Holte.
*
* Sets graphics cursor and draws line from current cursor position to
* point (x,y). 
*/

static int Px, Py;		/* graphics cursor */


/* set graphics cursor */
move (x, y)
  int x, y;
{
  Px = x;
  Py = y;
}


/* draw line from (X,Y) to (x,y) */
cont (x, y)
  int x, y;
{
  line (Px, Py, x, y);
  Px = x;
  Py = y;
}
