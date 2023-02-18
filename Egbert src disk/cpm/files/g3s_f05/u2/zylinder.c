int v,d;
float r,h,p = 3.14;
 

main()
{
	printf("Hoehenberechnung eines Zylinders \n\n");
	printf("Durchmesser in cm :");
	scanf("%d",&d);
	r=d/2;
	printf("Volumen (1000 ccm = 1 Liter): ");
	scanf("%d",&v);
	h = v/(p*r*r);
	printf("Hoehe: %f cm",h);
	getch();
}
	 