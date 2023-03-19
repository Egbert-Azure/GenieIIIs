#include <stdio.h>

int main(void);
int hanoi(int n, int start, int ziel, int hilf);


int n,start,ziel,hilf;
int zahl;
int count=0;

int main(void)
{
	putch(0x1a);				/* Clear Screen         */	
	printf("\n\a\t Wieviele Scheiben ? ");   
	scanf ("%d",&zahl);

	if (hanoi(zahl,1,2,3)  != 0)
		 return 1; 
	else {
		printf("\n\n *********     %d     ********* \n",count);
		return 0;
		}

}

int hanoi(int n, int start, int ziel, int hilf)
{
	if (n>1) hanoi(n-1,start,hilf,ziel);
/*	printf("Scheibe %d von %d nach %d bewegen !\n",n,start,ziel);     */
	count ++;
	if (n>1) hanoi(n-1,hilf,ziel,start);
	return 0;
}
