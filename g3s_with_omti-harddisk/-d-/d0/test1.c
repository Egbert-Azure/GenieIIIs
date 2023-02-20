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

main()
{
z3vinit();
cls();

	printf("Bildschirm geloescht - weiter mit ENTER\n");
	getch();
gotoxy(10,10);
        printf("Cursor an Position 10,10 !\n");
        gotoxy(11,10);
        printf("\227\227\227\227\227\227");

fprintf(stdprn,"Test");

}
		
