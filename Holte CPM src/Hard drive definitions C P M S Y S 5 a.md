# Hard drive definitions C P M S Y S 5 a #

To initalize with a new INITW tool a Winchester drive

``` c
/******************************************************************************
*  B A S F 6 1 8 8  * C P M S Y S 5 a * T h o m a s   H o l t e * 8 5 0 7 0 2 *
*******************************************************************************
*                                                                             *
*    M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W            *
*    =============================================================            *
*                                                                             *
*                                                                             *
*  Thomas Holte                              Version 1.0                      *
*                                                                             *
******************************************************************************/
  
/* characteristics of BASF 6188 Winchester drive      */
#define STEP_RATE     0  /* buffered step             */
#define WPCOMP_CYL  128  /* starting cylinder of
       write precompensation                         */
#define SECLEN     1024  /* sector length            */   
#define SECSIZE    0x40  /* size bits for controller */
#define SECCOUNT      9  /* sectors per track        */
#define TRKCOUNT   1440  /* track count              */
#define HEADCOUNT     4  /* surface count            */
#define SKEW          1  /* interleaving factor      */

/* CP/M specific parameters */
#define DIRSIZE   1024  /* # of directory entries   */

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
```

``` c
/******************************************************************************
*  N E C D 5 1 2 4  * C P M S Y S 5 a * T h o m a s   H o l t e * 8 5 0 9 2 2 *
*******************************************************************************
*                                                                             *
*    M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W            *
*    =============================================================            *
*                                                                             *
*                                                                             *
*  Thomas Holte                              Version 1.0                      *
*                                                                             *
******************************************************************************/
  
/* characteristics of NEC D5124 Winchester drive */
#define STEP_RATE     0  /* buffered step           */
#define WPCOMP_CYL  128  /* starting cylinder of
       write precompensation    */
#define SECLEN     1024  /* sector length            */   
#define SECSIZE    0x40  /* size bits for controller */
#define SECCOUNT      9  /* sectors per track        */
#define TRKCOUNT   1240  /* track count              */
#define HEADCOUNT     4  /* surface count            */
#define SKEW          1     /* interleaving factor      */

/* CP/M specific parameters */
#define DIRSIZE    1024  /* # of directory entries   */

/* disk parameter block of Winchester drive */
#define DPBA {72, 6, 63, 3, 1392, 1023, 0xF0, 0, 0x8000, 1, 3, 7}

/* configuration table */
static disp_config ()
{
  printf ("%s A: 10.9 MBytes %s\n", drive, type0);
  printf ("%s B:  800 KBytes %s\n", drive, type2);
  printf ("%s C:  800 KBytes %s\n", drive, type2);
}
```

``` c
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
```

``` c
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
```

``` c
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
```

``` c
/******************************************************************************
*  T M 2 5 2  *  C P M S Y S 5 a  *  T h o m a s   H o l t e  *  8 5 1 1 2 6  *
*******************************************************************************
* 									      *
*  	 M A C H I N E   D E P E N D E N T   P A R T   O F   I N I T W	      *
*        =============================================================        *
* 									      *
* 									      *
*  Thomas Holte			                         	 Version 1.0  *
* 									      *
******************************************************************************/
  
/* characteristics of Tandon TM252 Winchester drive */
#define STEP_RATE     0		/* buffered step      	    */
#define WPCOMP_CYL  128		/* starting cylinder of
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
}
```
