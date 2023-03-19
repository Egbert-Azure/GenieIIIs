/****************************************************************
	Der San Marcos-Drachen als 3D Bild in C
	*/
	
#include <stdio.h>
#include <grafik.h>
#include <math.h>

#define tiefe 50
#define hoehe 100
#define rmin 2.5
#define rmax 2.5
#define imin 1.3
#define imax 1.3
#define xm 319
#define ym 224

int main()
{
int m,n,k;
int u,v,u1,v1;
double a,x,x2,y,y1,y2,xc,yc,dx,dy;

dx=((rmax-rmin)*(-1))/(double)xm;
dy=(imax-imin)/(double)ym;

xc=1.0;
yc=0.0;
a=(double)tiefe/(double)hoehe;

for(n=0;n<ym;n++) {
	y1=imin+n*dy;
   for(m=0;m<xm;m++) {
   	x=rmin+m*dx;
      y=y1;
      k=0;
      	while(++k<=tiefe) {
      	   x2=x*x;
           y2=y*y;
           y=2*x*y-yc;
           x=x2-y2-xc;
           if ( x2+y2 >= (double)hoehe) break;
      }
      u=m+xm/2-n/2;
      u1=u+1;
      v=n+ym;
      v1=v-(int)((double)k/a-1.0);

openpl();
   
   line(u,v,u,v1);
   line(u1,v,u1,v1);
   line(u,v1,u1,v1);
   if(kbhit())n=ym,m=xm;
  }
}
getch();
closepl();
}
