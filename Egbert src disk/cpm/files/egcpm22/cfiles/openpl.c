/******************************************************************************
*  O P E N P L  *  L I B P L O T 0 3 0  * T h o m a s   H o l t e * 8 6 0 3 3 *
*******************************************************************************
*
* Version 1.0 by Thomas Holte.
*
* Erases graphic screen and opens it for output.
*/

openpl ()
{
  erase  ();		/* clear graphic screen			      */
  system (25, 1, 0);	/* opens graphic screen of phoenix Genie IIIs */
}
