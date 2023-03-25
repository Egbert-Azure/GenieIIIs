/******************************************************************************
*  N E C D 5 1 2 6  * C P M S Y S 5 a * T h o m a s   H o l t e * 8 6 0 5 3 1 *
*******************************************************************************
* 									      *
*  	 M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W	      *
*        =============================================================        *
* 									      *
* 									      *
*  Thomas Holte			                         	 Version 1.0  *
* 									      *
******************************************************************************/
  
/* characteristics of NEC D5124 Winchester drive */
#define STEP_RATE     0		/* buffered step      	    */
#define WPCOMP_CYL  128		/* starting cylinder of
				   write precompensation    */
#define SECLEN     1024		/* sector length       	    */   
#define SECSIZE    0x40		/* size bits for controller */
#define SECCOUNT      9		/* sectors per track   	    */
#define TRKCOUNT   2460		/* track count         	    */
#define HEADCOUNT     4		/* surface count       	    */
#define SKEW          1    	/* interleaving factor 	    */

/* CP/M specific parameters */
#define DIRSIZE    2048		/* # of directory entries   */

/* disk parameter block of Winchester drive */
#define DPBA {72, 7, 127, 7, 1382, 2047, 0xF0, 0, 0x8000, 1, 3, 7}

/* configuration table */
static disp_config ()
{
  printf ("%s A: 21.6 MBytes %s\n", drive, type0);
  printf ("%s B:  800 KBytes %s\n", drive, type2);
  printf ("%s C:  800 KBytes %s\n", drive, type2);
}
