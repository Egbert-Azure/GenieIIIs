/******************************************************************************
*  B A S F 6 1 8 8  * C P M S Y S 5 a * T h o m a s   H o l t e * 8 5 0 7 0 2 *
*******************************************************************************
* 									      *
*  	 M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W	      *
*        =============================================================        *
* 									      *
* 									      *
*  Thomas Holte			                         	 Version 1.0  *
* 									      *
******************************************************************************/
  
/* characteristics of BASF 6188 Winchester drive */
#define STEP_RATE     0		/* buffered step 	    */
#define WPCOMP_CYL  128		/* starting cylinder of
				   write precompensation    */
#define SECLEN     1024		/* sector length       	    */   
#define SECSIZE    0x40		/* size bits for controller */
#define SECCOUNT      9		/* sectors per track   	    */
#define TRKCOUNT   1440		/* track count         	    */
#define HEADCOUNT     4		/* surface count       	    */
#define SKEW          1    	/* interleaving factor 	    */

/* CP/M specific parameters */
#define DIRSIZE   1024		/* # of directory entries   */

/* disk parameter block of Winchester drive */
#define DPBA {72, 6, 63, 3, 1617, 1023, 0xF0, 0, 0x8000, 1, 3, 7}

/* configuration table */
static disp_config ()
{
  printf ("%s A: 12.6 MBytes %s\n", drive, type0);
  printf ("%s B:  800 KBytes %s\n", drive, type2);
  printf ("%s C:  800 KBytes %s\n", drive, type2);
  printf ("%s D:  250 KBytes %s\n", drive, type3);
}
