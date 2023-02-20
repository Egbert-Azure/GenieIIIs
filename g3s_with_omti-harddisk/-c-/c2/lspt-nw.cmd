
* Program..: LSPT-NW.CMD
* Author...: Egbert Schr|er
* Date.....: 29/12/92
* Notice...: Copyright 1992, All Rights Reserved
* Reserved.: pagenum, line, pagehdg, col:hdg, condition,
*            lastrec
*
* Bei neuem Jahr 93 durch naechstes ersetzen !!
*
*
SET TALK OFF
SET BELL OFF
SET MARGIN TO 3
STORE 1 TO pagenum
STORE 254 TO zeile
STORE "Mitglieder-Statistik ASV Dorsten e.V." TO pagehdg
STORE (75-LEN(pagehdg))/2 TO col:hdg
STORE 0 TO summem
STORE 0 TO summew
*
* ---Open the datafile and print the report.
USE C:ASV INDEX ASVIND
ERASE
@ 2, 0 SAY pagehdg
@ 2,65 SAY DATE()
@ 3, 0 SAY "________________________________________"
@ 3,40 SAY "___________________________________"
STORE " " TO select
@ 5,0 SAY "Ausgabe auf (M)onitor oder (D)rucker   ";
      GET select PICTURE "!"
READ
DO CASE
   CASE select = "M"
      ERASE
      STORE 22 TO pagelen
   CASE select = "D"
      ERASE 
      STORE 68 TO pagelen
   OTHERWISE
      ERASE
      SET BELL ON
      SET TALK ON
      RETURN
ENDCASE
* Start
DO WHILE .NOT. EOF
   IF zeile > pagelen
      IF select = "M"
         ERASE
      ELSE
         SET PRINT ON
      ENDIF
      @ 0, 0 SAY "PAGE NO."
      @ 0, 9 SAY STR(pagenum,3)
      @ 0,65 SAY DATE()
      @ 2,col:hdg SAY pagehdg
      @  4, 35 SAY "m{nnlich   weiblich"
      IF select = "D"
         SET PRINT OFF
      ENDIF
      *
      * ---Generate column headings.
      SET INTENSITY OFF
      @  6,  0 SAY "Mitglieder   bis 6 Jahre:"
      @  7,  0 SAY "Mitglieder  7 - 14 Jahre:"
      @  8,  0 SAY "Mitglieder 15 - 18 Jahre:"
      @  9,  0 SAY "Mitglieder 19 - 21 Jahre:"
      @ 10,  0 SAY "Mitglieder 22 - 35 Jahre:"
      @ 11,  0 SAY "Mitglieder 36 - 50 Jahre:"
      @ 12,  0 SAY "Mitglieder 51 - 60 Jahre:"
      @ 13,  0 SAY "Mitglieder }ber 60 Jahre:"
      @ 15,  0 SAY "                   Summe:"
      @ 16,  0 SAY "             Gesamtsumme:"
      STORE  pagenum+1 TO pagenum
      STORE 6 TO zeile
   ENDIF
   * ---Count for the conditions and store in temp. var.
   * ------------
   * Alter max. 6
   * ------------
   COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-6 .AND. gesch<>"w"
   COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-6 .AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder   bis 6 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter 7-14
   * ------------

COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-14 .AND. VAL($(geb,7,2))<=94-7;
.AND. gesch<>"w"
COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-14 .AND. VAL($(geb,7,2))<=94-7;
.AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder  7 - 14 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter 15-18
   * ------------

COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-18 .AND. VAL($(geb,7,2))<=94-15;
.AND. gesch<>"w"
COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-18 .AND. VAL($(geb,7,2))<=94-15;
.AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder 15 - 18 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter 19-21
   * ------------

COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-21 .AND. VAL($(geb,7,2))<=94-19;
.AND. gesch<>"w"
COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-21 .AND. VAL($(geb,7,2))<=94-19;
.AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder 19 - 21 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter 22-35
   * ------------

COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-35 .AND. VAL($(geb,7,2))<=94-22;
.AND. gesch<>"w"
COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-35 .AND. VAL($(geb,7,2))<=94-22;
.AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @ 15,30 SAY summem
   @ 15,40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder 22 - 35 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter 36-50
   * ------------

COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-50 .AND. VAL($(geb,7,2))<=94-36;
.AND. gesch<>"w"
COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-50 .AND. VAL($(geb,7,2))<=94-36;
.AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder 36 - 50 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter 51-60
   * ------------

COUNT TO anzahlm FOR VAL($(geb,7,2))>=94-60 .AND. VAL($(geb,7,2))<=94-51;
.AND. gesch<>"w"
COUNT TO anzahlw FOR VAL($(geb,7,2))>=94-60 .AND. VAL($(geb,7,2))<=94-51;
.AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder 51 - 60 Jahre:"+anzahlm+"          "+anzahlw
      SET PRINT OFF
   ENDIF
   STORE zeile+1 TO zeile
   * ------------
   * Alter gr|~er 60
   * ------------

   COUNT TO anzahlm FOR VAL($(geb,7,2))<=94-61 .AND. gesch<>"w"
   COUNT TO anzahlw FOR VAL($(geb,7,2))<=94-61 .AND. gesch="w"
   STORE anzahlm + summem TO summem
   STORE anzahlw + summew TO summew
   *
   * ---Print detail zeile.
   @ zeile, 30 SAY anzahlm
   @ zeile, 40 SAY anzahlw
   IF select = "D"
      SET PRINT ON
   @ zeile,0 SAY "Mitglieder }ber 60 Jahre:"+anzahlm+"          "+anzahlw
   ENDIF
   @   15, 30 SAY summem
   @   15, 40 SAY summew
   @   16, 30 SAY summem + summew
   SET PRINT OFF
   STORE zeile+1 TO zeile
ENDDO
SET INTENSITY ON
STORE " " TO select
@ zeile+5,10 SAY "Weiter mit einer Taste .....";
GET select PICTURE "!"
READ
SET FORMAT TO SCREEN
ERASE
RETURN
* EOF: LSPT-NW.CMD
