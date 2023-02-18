/******************************************************************************/
/*	Funktionsprototypen der Grafiklibrary f}r Genie IIIs mit Holte        */
/*	CP/M +. Die Routinen move und line sind noch nicht in dem LIB-File    */
/******************************************************************************/

void erase (void);			/* loescht HRG Seite 0                */
void openpl (void);			/* loescht HRG Seite 0 und schaltet an*/
void closepl (void);			/* schaltet HRG Seite 0 aus	      */
void point (int x, int y);		/* setzt einen Punkt (639,449)        */
void line (int x1, int y1, int x2, int y2);/* zieht Linie (links,unten,rechts,oben)      */	
void arc (int x, int y, int x0, int y0, int x1, int y1);/* Kreisbogen (mittel_x,y,start_x,y,end_x,y   */
void circle (int x, int y, int r);      /* Kreis (mittel_x,y,radius)	      */
void move (int x, int y);		/* Grafikcursor nach (x,y)	      */	
void cont (int x, int y);		/* draw line from (X,Y) to (x,y)      */
void unpoint (int x, int y);		/* loesche einen Punkt (639,449)      */
void unline (int x1, int y1, int x2, int y2);        /* loescht eine Line     */
void uncircle (int x, int y, int r);   /* loescht einen Kreis mittelpunkt_x,y radius     */
void unarc (int x, int y, int x0, int y0, int x1, int y1);/* loescht einen Kreisbogen mittelpunkt_y,y radius    */
void uncont (int x, int y);		/* undraw line from (X,Y) to (x,y)    */
