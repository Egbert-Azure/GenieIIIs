/******************************************************************************
*  T M 5 0 2  *  C P M S Y S 5 a  *  T h o m a s   H o l t e  *  8 5 1 1 2 6  *
*******************************************************************************
* 									      *
*  	 M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W	      *
*        =============================================================        *
* 									      *
* 									      *
*  Thomas Holte			                         	 Version 1.0  *
*  TM 252.H umgeschrieben von Egbert Schr|er 09.12.92                         *
******************************************************************************/
  
/* characteristics of Tandon TM502 Winchester drive */
#define STEP_RATE     0		/* buffered step      	    */
#define WPCOMP_CYL  153		/* starting cylinder of
				   write precompensation    */
#define SECLEN      512		/* sector length       	    */   
#define SECSIZE    0x20		/* size bits for controller */
#define SECCOUNT     17		/* sectors per track   	    */
#define TRKCOUNT   1224		/* track count         	    */
#define HEADCOUNT     4		/* surface count       	    */
#define SKEW          1    	/* interleaving factor 	    */

/* CP/M specific parameters */
#define DIRSIZE    1024		/* # of directory entries   */

/* disk parameter block of Winchester drive */
#define DPBA {68, 6, 63, 3, 1298, 1023, 0xF0, 0, 0x8000, 1, 2, 3}

/* configuration table */
static disp_config ()
{
  printf ("%s A: 10.4 MBytes %s\n", drive, type0);
  printf ("%s B:  800 KBytes %s\n", drive, type2);
  printf ("%s C:  800 KBytes %s\n", drive, type2);
}    
