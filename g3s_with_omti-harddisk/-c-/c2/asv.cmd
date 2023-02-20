
* Program..: ASV.CMD
* Author...: Egbert Schr|er
* Date.....: 31/08/92
* Notice...: Copyright 1992, All Rights Reserved
* Notes....: Hauptprogramm
* Reserved.: select, selectnum
*
* Zun{chst mal Datum holen
*
do autodat
SET TALK OFF
SET BELL OFF
SET COLON OFF
*
* Graphik Sonderzeichen
*
store chr(129)+chr(129)+chr(129)+chr(129)+chr(129)+chr(129) to line
store chr(129) to hline
store line+line+line+line+line+line+hline+hline to line
store chr(128) to vline
store chr(151)+chr(151)+chr(151)+chr(151)+chr(151) to schatten
store schatten+schatten+schatten+schatten+schatten+schatten+schatten+schatten to schatten
store chr(153)+chr(154)+chr(155) to hand

DO autodat

ERASE

DO WHILE T
DO asvmsk1

STORE  8 TO selectnum
DO WHILE selectnum < 0 .OR. selectnum >  6
   STORE " " TO select
   @ 15,19 GET select PICTURE "#"
   READ
   STORE VAL(select) TO selectnum
ENDDO

DO CASE
   CASE selectnum= 0
      @  6, 2 SAY hand
      SET COLON ON
      SET BELL ON
      SET TALK ON
      CLEAR
      RETURN
   CASE selectnum= 1
      @  7, 2 SAY hand
      DO AS-MAIN
   CASE selectnum= 2
      @  8, 2 SAY hand
   *  DO Listen
      ERASE
      DO ASVRPT        
   CASE selectnum= 3
      @  9, 2 SAY hand
   *  DO Adressaufkleber
      ERASE        
      DO ASVLBL         
   CASE selectnum= 4
      @ 10, 2 SAY hand
   *  DO Statistik Landessportbund NW
      DO LSPT-NW
   CASE selectnum= 5
   *  DO FREI
   CASE selectnum= 7
      @ 11, 2 SAY hand
   *  DO Hilfe
ENDCASE

ENDDO T
* EOF: ASV.CMD
