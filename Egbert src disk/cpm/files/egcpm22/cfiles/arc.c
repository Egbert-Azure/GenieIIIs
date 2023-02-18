/******************************************************************************
*  A R C  *  L I B P L O T 0 0 5  *  T h o m a s   H o l t e  *  8 6 0 3 3 1  *
*******************************************************************************
*
* Version 1.0 by Thomas Holte.
*
* Draws an arc around (x,y) from (x0,y0) to (x1,y1).
*/

arc (x, y, x0, y0, x1, y1)
  int x, y, x0, y0, x1, y1;
{
  /* common arc drawing routine */
/*
  int dx, dy, m1, m2, n1, n2, n3, p1, p2;
  int d  = 0;
  int xx = x0 - x;
  int yy = y0 - y;

  x1 -= x; y1 -= y;

  point (x + xx, y + yy);

  do
  {
    if (yy >= 0) 
    {
      dx = -1;
      m1 = 1 - (xx + xx);
    }
    else
    {
      dx = 1;
      m1 = 1 + xx + xx;
    }

    if (xx >= 0) 
    {
      dy = 1;
      m2 = 1 + yy + yy;
    }
    else
    {
      dy = -1;
      m2 = 1 - (yy + yy);
    } 

    p1 = m1 + d;
    p2 = m2 + d;
    n1 = abs(p1);
    n2 = abs(p1 + m2);
    n3 = abs(p2);

    if (n1 < n2) 
      if (n1 < n3)
      {
	xx += dx;
	d   = p1;
      }
      else
      {
	yy += dy;
	d   = p2;
      }
    else 
    {
      yy += dy;
      if (n2 < n3)
      {
	xx += dx;
	d   = p1 + m2;
      }
      else d = p2;
    }

    point (x + xx, y + yy);

  }
  while (xx != x1 || yy != y1);
*/

  /* draw arc on phoenix Genie IIIs */
  system (31, 1, 0, &x);   
}
