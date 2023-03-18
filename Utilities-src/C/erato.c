/* Sieb des Eratosthenes in C */
/* cp 20/83, Seite 94         */

#define TRUE   1        /* logisch WAHR              */
#define FALSE  0        /* logisch FALSCH            */
#define SIZE   8190
#define SIZEPL 8191
#define LOOP   100      /* Anzahl der Wiederholungen */

main() 
{
  int  i,prime,k,count,iter;
  char flags[SIZEPL];

  printf("\7\n%d iterations\n",LOOP);
  for(iter=1;iter<=LOOP;iter++){
    count=0;
    for(i=0;i<=SIZE;i++)
      flags[i]=TRUE;
    for(i=0;i<=SIZE;i++){
      if(flags[i]){
        prime=i+i+3;
        k=i+prime;
        while(k<=SIZE){
          flags[k]=FALSE;
          k+=prime;
        }
      count++;
      }
    }
  }
printf("\7\n%d primes\n",count);
exit(0);
}
