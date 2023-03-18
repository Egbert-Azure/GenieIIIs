/* Erstes Drucker Testprogramm */

#include <cpm.h>
#include <stdio.h>

char c, x;

FILE *_printer_;

#define PrIOInit() 	_printer_=fopen("LST:","wb")
#define PrPutChar(c)	putc(c,_printer_)
#define PrIOExit()	fclose(_printer_)
#define printchar(x)    putc(x,stdprn)

main()

{
   PrIOInit();
   c='A';
   x='B'; 
   PrPutChar(c);
   PrIOExit();
   printchar(x);

}
