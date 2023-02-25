
/* Program PAUSE.C 					       */
/* Test fuer LIBZ und Holte Funktionen                         */
/*===============================================================
The following are the routines included in LIBZ.LIB:

z3vinit(void);           Initialize VLIB pointer/
cls(void);		 clear screen
ereol(void);		 erase to end of line
gotoxy(int, int);	 move cursor
tinit(void);		 initialize terminal
dinit(void);		 deinitialize terminal
stndout(void);		 enter standout mode
stndend(void);		 end standout mode

Note that envptr is a global variable that points to the
environment descriptor.  You may wish to do something like:

#define isquiet()	(envptr->quiet)
#define iswheel()	(*envptr->wheel)

to test for Z3 'privileges'.  (In fact, these are already in
z3.h.)

There is also a global variable for the internal environment
descriptor, _intenv.  You can check to see whether your program
is 'seeing' an external environment (as you would under Z33 and
later, or a Z3INS'd program) or the internal environment (under
CP/M or an uninstalled program under Z30):

    if (envptr == _intenv)
      printf("internal environment in use!");
    else
      printf("environment at %04x", envptr);
================================================================*/

#include <stdio.h>

#define CLOCK_ON  system (24,1,5,0,32)   /* Function 24 turns clock on */
#define CLOCK_OFF system (24,0,5,0,32)   /* and off                    */ 
#define DELAY     10000

char meldung[] = "* * *  PAUSE  * * * * * *  PAUSE  * * *";

/* Character in octal conversion */

char clock_rahmen[]  =
"\212\201\201\201\201\201\201\201\201\201\201\201\201\211\n"
"                             \200            \200\n" 		
"                             \210\201\201\201\201\201\201"
"\201\201\201\201\201\201\207\n";

char message_rahmen[]  =
"\212\201\201\201\201\201\201\201\201\201\201\201\201"
"\201\201\201\201\201\201\201\201\201\201\201\201\211\n"
"                        \200                        \200\n" 		
"                        \200                        \200\n" 		
"                        \200                        \200\n" 		
"                        \210\201\201\201\201\201\201\201"
"\201\201\201\201\201\201\201\201\201\201\201\201\201\201"
"\201\201\201\207\n";

main()
{
  int count, i=0;
  
  z3vinit();		/* initialise TCAP */
  cls();		/* get ESC-sequ. from TCAP */ 
  
  gotoxy(5,30);		/* How to move cursor -> TCAP */
  puts(clock_rahmen);
  CLOCK_ON;

  gotoxy(24,20);
  printf("--- Ende der Pause mit beliebiger Taste ---");
  
  gotoxy(10,25);
  puts(message_rahmen);
   
  while( !kbhit())
  { 
    gotoxy(12,27);
    printf("%.20s", meldung + i);   /* message on screen */
    for( count = 0; count < DELAY; ++count);	/* output delay */ 

    if(++i >= strlen(meldung)/2) i =0;          /* once more from begin */
  }
  cls();
  CLOCK_OFF;
}
