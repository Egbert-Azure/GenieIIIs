/*   BALL.C  -->    Simuliert einen springenden Ball               *
 *                  (Version ohne ESC-Sequenzen)                   *
 *
 *   Mit den Tasten <+> und <-> (des Buchstaben-Blocks) kann die   *
 *   die Geschwindigkeit des Balls ver{ndert werden.               *
 *   Jede andere Taste beendet das Programm.                       */

#include <stdio.h>
#include <conio.h>

#define ATTR   0x17     /* Attribut f}r die Anzeige: wei~ auf blau */

/*       ---   Die Prototypen der eigenen Funktionen:   ---        */
void  set_cur( int zeile, int spalte),  cls( unsigned char attr),
      putcb9( int c, unsigned count, unsigned modus),
      cursor_aus(void),  cursor_an(void);

char  header[] = " ****  SPRINGENDER BALL  **** ";

main()
{
     int   c, x, y,  geschw = 0,  dx = 1;
     long  warten, delay = 40000L;        /* F}r die Schnelligkeit */
                                          /* des Balls             */

     cls(ATTR);  cursor_aus();
     for( x = 0; header[x] != '\0' ; ++x)           /* ]berschrift */
        set_cur(0,x+25), putcb9( header[x], 1, 0x70);

     for( y = 2 ; y < 24 ;  ++y)                    /* linke Wand  */
        set_cur(y,0), putcb9( '!', 1, ATTR);
     set_cur(24,0), putcb9( '+', 1, ATTR);

     set_cur(24,1);   putcb9( '-', 78, ATTR);            /* Boden */

     set_cur(24,79),  putcb9( '+', 1, ATTR);
     for( y = 23 ; y > 1 ;  --y)                    /* rechte Wand */
       set_cur(y,79), putcb9( '!', 1, ATTR);

     set_cur(y,0);

     x = y = 2;
     while( 1)        /* Abbruch mit beliebiger Taste != '+', '-'  */
     {
         set_cur( y,x);
         putcb9( 'o',1, ATTR | 0x8);     /* Ball intensiv anzeigen */

         for( warten = 0 ; warten < delay ; ++warten)
           ;
         if(x == 1  ||  x == 78)  dx =  -dx;     /* An einer Wand? */
         if(y == 23)                             /* Am Boden?      */
         {
             geschw = -geschw;
             if(geschw == 0)  geschw = -7;       /* neu ansto~en   */
         }
         geschw += 1;                        /* Beschleunigung = 1 */

         putcb9( ' ', 1, ATTR);                 /* Anzeige l|schen */

         y += geschw;  x += dx;                 /* Neue Position   */

         if( kbhit())
         {
           if((c = getch()) == '+'  &&  delay > 2000L)
              delay -= 2000L;                         /* schneller */
           else if( c == '-'  &&  delay < 1000000L)
              delay += 2000L;                         /* langsamer */
           else break;                                /* Ende      */
         }
     }
     cursor_an();  cls( 7);               /* Anzeige wieder normal */
     return 0;
}


/*---------------------------------------------------------------*
 * Die Funktionen  cls(), set_cur(), putcb9()                    *
 *                 set_cursor_type(), cursor_aus(), cursor_an()  *
 *                                                               *
 *  Es werden die Funktionen 1,2,3,6 und 9 des Video-Interrupts  *
 *  (ROM-BIOS-Interrupt 0x10) verwendet.                         */

#include <dos.h>
#define  VIDEO_INT  0x10

typedef unsigned char  BYTE;

static  union REGS  intregs;

/*----------------------------------------------------------------
 *   set_cur( zeile, spalte)   setzt den Cursor auf die angegebene
 *                             Position.
 */

void  set_cur( int zeile, int spalte)
{
      intregs.h.ah = 2;                    /* Subfunktion AH = 2 */
      intregs.h.dh = (BYTE)zeile;
      intregs.h.dl = (BYTE)spalte;
      intregs.h.bh = 0;                    /* Bildschirmseite.   */

      int86(VIDEO_INT, &intregs, &intregs);
}

/*----------------------------------------------------------------
 *   cls( attribut)   l|scht den Bildschirm mit dem angegebenen
 *                    Attribut.
 */

void  cls( unsigned char attr)   /* Gesamten Bildschirm l|schen. */
{
      intregs.x.ax = 0x600;             /* Subfunktion AH = 6,   */
                                        /* AL = 0 : ges. Fenster */
      intregs.x.cx = 0;                 /* oben:  Zeile  = 0,    */
                                        /*        Spalte = 0     */
      intregs.x.dx = (24<<8) | 79;      /* unten: Zeile  = 24    */
                                        /*        Spalte = 79    */
      intregs.h.bh = attr;              /* Attribut (Farbe)      */

      int86(VIDEO_INT, &intregs, &intregs);

      set_cur(0,0);                 /* Cursor in Home-Position.  */
}

/*----------------------------------------------------------------
 * putcb9(c,count,mode)  gibt das Zeichen in c count-mal im
 *                       angegebenen Modus auf der aktuellen
 *                       Cursor-Position am Bildschirm aus.
 *                       Der Cursor wird nicht versetzt.
 */

void  putcb9( int c,                            /*  das Zeichen  */
              unsigned count,                   /*  die Anzahl   */
              unsigned mode )   /* Low-Byte:  das Attribut       */
                                /* High-Byte: die Bildschirmseite*/
{
      intregs.h.ah = 9;         /*  Subfunktion 9 des Int 0x10   */
      intregs.h.al = (BYTE)c;
      intregs.x.bx = mode;
      intregs.x.cx = count;

      int86( VIDEO_INT, &intregs, &intregs);
}


/*----------------------------------------------------------------
 *  set_cursor_type()   setzt den Typ des Cursors
 */

void set_cursor_type(unsigned start,    /* Start-Linie und       */
                     unsigned end)      /* End-Linie des Cursors */
{
      intregs.h.ch = (BYTE)start;
      intregs.h.cl = (BYTE)end;
      intregs.h.ah = 1;
      int86(VIDEO_INT, &intregs, &intregs);
}

/*----------------------------------------------------------------
 *   cursor_aus() , cursor_an()   schalten den Cursor aus bzw. an
 */

static  unsigned char curstart, curend;

void  cursor_aus()
{
      intregs.h.ah = 3;                      /* Cursor lesen     */
      intregs.h.bh = 0;                      /* Bildschirm-Seite */
      int86(VIDEO_INT, &intregs, &intregs);  /* Video-Int 10H    */
      curstart = intregs.h.ch;
      curend   = intregs.h.cl;

      set_cursor_type(curstart | 0x20 , curend);
                                       /* Bit 5 in startl setzen */
}

void  cursor_an()
{
      set_cursor_type( curstart & 0x1f , curend);
}
