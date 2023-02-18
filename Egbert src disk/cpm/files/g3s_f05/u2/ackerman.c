
/* Ackermann-Funktion */

#include <stdio.h>
#include "ackerman.h"    /* Prototypes           */

#define ARG1 3           /* 1. Argument maximal  */
#define ARG2 6           /* 2. Argument maximal  */

unsigned short acker(i,j)  /* Types declared in .H */
{
  if (i==0)
    return (j+1);
  else if (j==0)
    return(acker(i-1,1));
  else
    return(acker(i-1,acker(i,j-1)));
}

main()
{
  unsigned int m,n;
  putchar(7);                /* beep */
  for (m=0;m<=ARG1;m++)
    for (n=0;n<=ARG2;n++)
      printf("acker(%d,%d)=%d\n",m,n,acker(m,n));
  putchar(7);               /* beep */
  exit(0);
}
