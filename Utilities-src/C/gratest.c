#include <stdio.h>
#include <time.h>
#include <grafik.h>


int main(void);
int rand();


int zaehler, radius, x0, y0, x1, y1;
int max_x,max_y,max_int;

#define sub 0x1a


/* draws a box (lower_left to upper_right)   */
void box(int xb0, int yb0, int xb1, int yb1)
{
	line(xb0,yb0,xb1,yb0);		/* links unten nach rechts unten */
	line(xb1,yb0,xb1,yb1);		/* rechts unten nach rechts oben */
	line(xb1,yb1,xb0,yb1);		/* rechts oben nach links oben   */
	line(xb0,yb1,xb0,yb0);		/* links oben nach links unten   */
}
	    

/* undraws a box ( lower_left to upper_right */
void unbox(int xb0, int yb0,int xb1, int yb1)
{
	unline(xb0,yb0,xb1,yb0);	/* links unten nach rechts unten */
	unline(xb1,yb0,xb1,yb1);	/* rechts unten nach rechts oben */
	unline(xb1,yb1,xb0,yb1);	/* rechts oben nach links oben   */
	unline(xb0,yb1,xb0,yb0);	/* links oben nach links unten   */
}
	 


int main(void)
{

	x0 = 0; y0 = 0; x1 = 100; y1 = 100;
	putch(sub);
	puts("Hallo Welt, wie geht's denn so ? ");   


	openpl();
	line(x0,y0,x1,y1);
	line(100,100,620,400);	
	circle(300,140,30);
	box(30,30,240,100);
	box(240,100,639,300);
	box(250,110,629,290);
	box(260,120,619,280); 
	box(270,130,609,270);
	
	erase();
	for (radius =5; radius <200; radius +=2)
	circle(320,220,radius);

	for (radius = 5; radius < 200; radius +=2)
	uncircle(320,220,radius);
		

	move (330,280);
	srand (2204);
	for (zaehler = 0; zaehler <120; zaehler ++)
	{
		x0 = rand() / 52;
		y0 = rand() / 80;
	 		cont(x0,y0);
	}
	
	move(330,280);
	srand(2204);
	for (zaehler = 0; zaehler <121; zaehler ++)
	{
		x0 = rand() / 52;
		y0 = rand() / 80;
	 		uncont(x0,y0);
	}
		
	
	srand(2204);
	for (zaehler =0; zaehler < 52; zaehler ++)
	{
		x0 = rand() / 52; x1 = rand() / 52;
		y0 = rand() / 80; y1 = rand() / 80;
		box(x0,y0,x1,y1);
	}
		 

	srand(2204);
	for (zaehler =0; zaehler < 52; zaehler ++)
	{
		x0 = rand() / 52; x1 = rand() / 52;
		y0 = rand() / 80; y1 = rand() / 80;
		unbox(x0,y0,x1,y1);
	}

	srand(2204);
	for (zaehler = 0; zaehler < 80; zaehler ++)
	{
		x0 = rand() / 52; y0 = rand() / 80;
		radius = ( rand() / 1000 ) + 5;
		circle(x0, y0, radius);
	}	 
	
	srand(2204);
	for (zaehler = 0; zaehler < 80; zaehler ++)
	{
		x0 = rand() / 52; y0 = rand() / 80;
		radius = ( rand() / 1000 ) + 5;
		uncircle(x0, y0, radius);
	}	

	
	srand(2204);
	for (zaehler = 0; zaehler < 1500; zaehler ++)
	{
		x0 = rand() / 52; y0 = rand() / 80;
		point(x0,y0);
	}	

	srand(2204);
	for (zaehler = 0; zaehler < 1500; zaehler ++)
	{
		x0 = rand() / 52; y0 = rand() / 80;
		unpoint(x0,y0);
	}	
		
	puts("Fertig, Taste um zurueck ins Betriebssystem !!");
	getch();

	closepl();

}
