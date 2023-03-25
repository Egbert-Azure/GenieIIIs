/******************************************************************************
*  S Q 3 0 6  *  C P M S Y S 5 a  *  T h o m a s   H o l t e  *  8 5 0 7 0 2  *
*******************************************************************************
* 									      *
*  	 M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W	      *
*        =============================================================        *
* 									      *
* 									      *
*  Thomas Holte			                         	 Version 1.0  *
* 									      *
******************************************************************************/
  
/* characteristics of Syquest 306 Winchester drive */
#define STEP_RATE     0		/* buffered step 	    */
#define WPCOMP_CYL 1020		/* no write precompensation */
#define SECLEN     1024		/* sector length       	    */   
#define SECSIZE    0x40		/* size bits for controller */
#define SECCOUNT      9		/* sectors per track   	    */
#define TRKCOUNT    612		/* track count         	    */
#define HEADCOUNT     2		/* surface count       	    */
#define SKEW          1    	/* interleaving factor 	    */

/* CP/M specific parameters */
#define DIRSIZE     512		/* # of directory entries   */

/* disk parameter block of Winchester drive */
#define DPBA {72, 5, 31, 1, 1373, 511, 0xF0, 0, 0x8000, 1, 3, 7}

/* configuration table */
static disp_config ()
{
  printf ("%s A: 5.4 MBytes %s\n", drive, type0);
  printf ("%s B: 800 KBytes %s\n", drive, type2);
  printf ("%s C: 800 KBytes %s\n", drive, type2);
}
