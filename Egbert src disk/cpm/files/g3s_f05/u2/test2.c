#include <stdio.h>


main()
{
	int a,b,c;
	printf("Wert fuer a eingeben: ");
	scanf("%d",&a);
	printf("Wert fuer b eingeben: ");
	scanf("%d",&b);
	
	c = a + b;
	if ( c != a)
	   printf("Summe a+b = %d ",c);
	else
	   printf("Summe a+b = a");
	getch();
	
}
	