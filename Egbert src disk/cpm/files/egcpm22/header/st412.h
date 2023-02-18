/******************************************************************************
*  S T 4 1 2  *  C P M S Y S 5 a  *  T h o m a s   H o l t e  *  8 5 0 7 0 2  *
*******************************************************************************
* 									      *
*  	 M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W	      *
*        =============================================================        *
* 									      *
* 									      *
*  Thomas Holte			                         	 Version 1.0  *
* 									      *
******************************************************************************/
  
/* characteristics of Seagate 412 Winchester drive */
#define STEP_RATE     3		/* 3 msec step rate   	    */
#define WPCOMP_CYL   64		/* starting cylinder of
				   write precompensation    */
#define SECLEN     1024		/* sector length       	    */   
#define SECSIZE    0x40		/* size bits for controller */
#define SECCOUNT      9		/* sectors per track   	    */
#define TRKCOUNT   1224		/* track count         	    */
#define HEADCOUNT     4		/* surface count       	    */
#define SKEW          1    	/* interleaving factor 	    */

/* CP/M specific parameters */
#define DIRSIZE    1024		/* # of directory entries   */

/* disk parameter block of Winchester drive */
#define DPBA {72, 6, 63, 3, 1374, 1023, 0xF0, 0, 0x8000, 1, 3, 7}

/* configuration table */
static disp_config ()
{
  printf ("%s A: 10.8 MBytes %s\n", drive, type0);
  printf ("%s B:  5.4 MBytes %s\n", drive, type1);
  printf ("%s C:  800 KBytes %s\n", drive, type2);
  printf ("%s D:  800 KBytes %s\n", drive, type2);
}
