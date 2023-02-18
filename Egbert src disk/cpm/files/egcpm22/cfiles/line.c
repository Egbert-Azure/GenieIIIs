/******************************************************************************
*  L I N E  *  L I B P L O T 0 2 5  *  T h o m a s   H o l t e  * 8 6 0 4 0 1 *
*******************************************************************************
*
* Version 1.0 by Thomas Holte.
*
* Draws a line from (x1,y1) to (x2,y2).
*/

line (x1, y1, x2, y2)
  int x1, y1, x2, y2;
{
  /* common line drawing routine */
/*
  int dx, dy, px, py;
  int x = x1;
  int y = y1;
  int p = 0;

  if (x2 >= x1)
  {
    px = x2 - x1 + 1;
    dx = 1;
  }
  else
  {
    px = x1 - x2 + 1;
    dx = -1;
  }

  if (y2 >= y1)
  {
    py = y2 - y1 + 1;
    dy = 1;
  }
  else
  {
    py = y1 - y2 + 1;
    dy = -1;
  }

  point (x, y);

  if (py <= px)
  {
    while (x != x2)
    {
      if ((p += py) >= px)
      {
	p -= px;
	y += dy;
      }

      point (x += dx, y);
    }
  }
  else
  {
    while (y != y2)
    {
      if ((p += px) >= py)
      {
        p -= py;
	x += dx;
      }

      point (x, y += dy);
    }
  }
*/

  /* line drawing routine on phoenix Genie IIIs */
  system (29, 0x11, 0, &x1);
}
