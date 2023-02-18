/******************************************************************************
*  T O D  *  U T I L S 0 0 7  *   T h o m a s   H o l t e   *   8 4 0 9 0 9   *
*******************************************************************************
* 									      *
*    S E T / G E T   D A T E / T I M E   F O R   T H E   G E N I E   I I I    *
*    =====================================================================    *
* 									      *
*		    M I C R O C O M P U T E R   S Y S T E M   		      *
*                   =======================================                   *
* 									      *
* 									      *
*   Thomas Holte			                         Version 1.0  *
* 									      *
******************************************************************************/

#include <stdio.h>

main (argc, argv)
  int argc;
  char *argv[];
{
  char class, day, hours, leapyear, minutes, month, seconds, state, timbuf[21],
       val, weekday, work, year, yearcount;
  int daycount, de, i;

  static char *weekdays[] = {"Sat", "Sun", "Mon", "Tue", "Wed", "Thu", "Fri"};

  /* structure of the decision table:
     a) states:     0. initial
		    1. date encountered
		    2. end
     b) activities: 0. NOP
		    1. set date
		    1. set time
  
			   date  time */
  static char tab[][2] = {{0x11, 0x22},
			  {0x20, 0x22}};

  /* get date & time */
  read: system (GETTIM, 0, 0, timbuf, 0);

  year  = atoi(&timbuf[10]);
  month = atoi(&timbuf[ 4]);
  day   = atoi(&timbuf[ 7]);

  for (i = 0; i < 7; i++) if (streq(timbuf, weekdays[i]))
			  {
			    weekday = i;
			    break;
			  }

  hours   = atoi(&timbuf[13]);
  minutes = atoi(&timbuf[16]);
  seconds = atoi(&timbuf[19]);

  /* initialize control variable */
  state = 0;

  if (argc > 1)
  {
    for (i = 1; i < argc; i++)
    {
        class = 0xFF;
      if (strlen(argv[i]) == 8 && argv[i][2] == '/' && argv[i][5] == '/')
        class = 0;
      if (strlen(argv[i]) == 5 && argv[i][2] == ':') class = 1;

      if (class == 0xFF) break;

      /* get table entry */
      val = tab[state][class]; 

      /* working entry */
      work = val & 3;

      switch (work)
      {
        case 1: if ((year = atoi(&argv[i][6])) < 81) daterr ();
	        leapyear = !(year % 4);
                daycount = (yearcount = year - 81) * 365 + yearcount / 4;
	        if ((day = atoi(&argv[i][3])) < 1) daterr ();
                switch (month = atoi(argv[i]))
	        {
                  case  1: if (day > 31) daterr ();
		           daycount += day;
       		           break;
		  case  2: if (day > 28 + leapyear) daterr ();
                           daycount += 31 + day;
     		   	   break;
		  case  3: if (day > 31) daterr ();
            	 	   daycount += 59 + leapyear + day;
			   break;
		  case  4: if (day > 30) daterr ();
            		   daycount += 90 + leapyear + day;
			   break;
		  case  5: if (day > 31) daterr ();
            		   daycount += 120 + leapyear + day;
			   break;
		  case  6: if (day > 30) daterr ();
            		   daycount += 151 + leapyear + day;
			   break;
		  case  7: if (day > 31) daterr ();
            		   daycount += 181 + leapyear + day;
			   break;
		  case  8: if (day > 31) daterr ();
            		   daycount += 212 + leapyear + day;
			   break;
		  case  9: if (day > 30) daterr ();
            		   daycount += 243 + leapyear + day;
			   break;
		  case 10: if (day > 31) daterr ();
            		   daycount += 273 + leapyear + day;
			   break;
		  case 11: if (day > 30) daterr ();
            		   daycount += 304 + leapyear + day;
			   break;
		  case 12: if (day > 31) daterr ();
            		   daycount += 334 + leapyear + day;
			   break;
		  default: daterr ();
                }
	        weekday = (daycount + 4) % 7;
		break;
  
        case 2: if ((hours   = atoi( argv[i]   )) < 0 || hours   > 23)
		  timerr ();
	        if ((minutes = atoi(&argv[i][3])) < 0 || minutes > 59)
		  timerr ();
	        seconds = 0;

	        printf ("\nStrike a key to set time\n");
	        pause  ();
      }
      if ((state = val >> 4) == 2) break;
    }

    /* set date & time */
    de = ((hours / 10 + 8 << 4) + hours % 10 << 8)
       + (minutes / 10 << 4) + minutes % 10;

    system (SETTIM, weekday, (month / 10 << 4) + month % 10,
            ((day / 10 + (leapyear << 2) << 4) + day % 10 << 8) 
            + (year / 10 << 4) + year % 10, de);
	  	
    argc = 0;
    goto read;		/* reread it */
  }

  /* print date & time */
  printf ("\n%.21s\n", timbuf); 
}


daterr ()
{
  puts ("\nBAD DATE\n");
  exit ();
}


timerr ()
{
  puts ("\nBAD TIME\n");
  exit ();
}
