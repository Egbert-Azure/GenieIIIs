/*-------------------------------*
 *  FIBONACCI-Funktion           *
 *  cp 6/85, Seite 63            *
 *-------------------------------*/

#include "fib.h"

#define wiederholungen 10
#define zahl           24

unsigned short fib(unsigned short x)
{
  if (x>2)
    return(fib(x-1)+fib(x-2));
  else
    return(1);
}

main()
{
  unsigned short i,wert;

  printf("\n%d iterations: \7",wiederholungen);

  for (i=1;i<=wiederholungen;i++){
    printf("%d ",i);
    wert=fib(zahl); 
    }

  printf("\7\nFibonacci(%d) = %u.\n",zahl,wert);
  exit(0);
}
