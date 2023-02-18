/******************************************************************************
*  C I R C L E  * L I B P L O T 0 1 0 * T h o m a s   H o l t e * 8 6 0 4 0 1 *
*******************************************************************************
*
* Version 1.0 by Thomas Holte.
*
* Draws an circle around (x,y) with radian r.
*/

circle (x, y, r)
  int x, y, r;
{
  /* common circle drawing routine */
/*
  int p1, p2;
  int d  = 0;
  int xx = r;
  int yy = 0;
  int m1 = 1 - (xx + xx);
  int m2 = 1;

  point (x + xx, y     );
  point (x - xx, y     );
  point (x     , y + xx);
  point (x     , y - xx);

  do
  {
    p1 = m1 + d;
    p2 = m2 + d;

    yy++;	
    m2 += 2;
    if (abs(p1 + m2) < abs(p2))
    {
      xx--;
      d   = p1 + m2;
      m1 += 2;
    }
    else d = p2;

    point (x + xx, y + yy);
    point (x - xx, y + yy);
    point (x + xx, y - yy);
    point (x - xx, y - yy);
    point (x + yy, y + xx);
    point (x - yy, y + xx);
    point (x + yy, y - xx);
    point (x - yy, y - xx);
  }
  while (xx > yy);
*/

  /* draw circle on phoenix Genie IIIs */
  system (30, 1, 0, &x);
}
