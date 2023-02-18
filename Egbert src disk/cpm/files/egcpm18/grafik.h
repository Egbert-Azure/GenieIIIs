/******************************************************************************/
/*	Funktionsprototypen der Grafiklibrary f}r Genie IIIs mit Holte        */
/*	CP/M +. Die Routinen move und line sind noch nicht in dem LIB-File    */
/******************************************************************************/

int erase (void);			/* loescht HRG Seite 0            	      */
int openpl (void);			/* loescht HRG Seite 0 und schaltet an        */
int closepl (void);		/* schaltet HRG Seite 0 aus		      */

/* setzt einen Punkt (639,449) 		      */
void point (int x, int y);		

/* zieht Linie (links,unten,rechts,oben)      */
void line (int x1, int y1, int x2, int y2);	

/* Kreisbogen (mittel_x,y,start_x,y,end_x,y   */
void arc (int x, int y, int x0, int y0, int x1, int y1);


/* Kreis (mittel_x,y,radius)		      */
void circle (int x, int y, int r);
				

/* set graphics cursor */
void move (int x, int y);		/* Grafikcursor nach (x,y)		      */	

/* draw line from (X,Y) to (x,y) */
void cont (int x, int y);


/* loesche einen Punkt (639,449)   */
void unpoint (int x, int y);

/* loescht eine Line     */
void unline (int x1, int y1, int x2, int y2);


/* loescht einen Kreis mittelpunkt_x,y radius     */
void uncircle (int x, int y, int r);


/* loescht einen Kreis mittelpunkt_y,y radius    */ 
void unarc (int x, int y, int x0, int y0, int x1, int y1);


/* undraw line from (X,Y) to (x,y) */
void uncont (int x, int y);
