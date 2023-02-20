*************************************************************************
*  Autodat.CMD                                                          *
*  Setzt das Datum automatisch durch Einlesen der Systemzeit aus der    *
*  Hardware-Uhr                                                         *
*  Der BDOS-Call ist im CP/M 3.0  standardm{~ig vorhanden               *
*  unter CP/M 2.2 nur mit Z80DOS, P2DOS o.{.                            *
*************************************************************************

SET TALK off
SET INTENSITY off

POKE 42000,17,25,164,14,105,205,5,0,201
SET CALL TO 42000
CALL
STORE PEEK(42009) + 256*PEEK(42010) TO JULIAN
STORE JULIAN + 722099 TO JULIAN
STORE INT(JULIAN/365.26)+1 TO JAHR
STORE JULIAN + INT(395.25-365.25*JAHR) TO TAG

IF INT(JAHR/4)*4 = JAHR
   STORE 1 TO SCHATAG
ELSE
   STORE 2 TO SCHATAG
ENDIF

IF TAG>(91-SCHATAG)
   STORE TAG+SCHATAG TO TAG
ENDIF

STORE INT(TAG/30.57) TO MONAT
STORE TAG-INT(30.57*MONAT) TO TAG
IF MONAT>12
   STORE 1 TO MONAT
   STORE JAHR+1 TO JAHR
ENDIF
STORE JAHR-1900 TO JAHR

STORE STR(TAG,2) TO STAG
DO WHILE $(STAG,1,1)=" "
   STORE "0"+$(STAG,2,1) TO STAG
ENDDO

STORE STR(MONAT,2) TO SMONAT
DO WHILE $(SMONAT,1,1)=" "
   STORE "0"+$(SMONAT,2,1) TO SMONAT
ENDDO
STORE STR(JAHR,2) TO SJAHR
DO WHILE $(SJAHR,1,1)=" "
   STORE "0"+$(SJAHR,2,1) TO SJAHR
ENDDO

STORE STAG+"."+SMONAT+"."+SJAHR TO DATUM
?
?
?
?
@ 20,10 SAY "Das Datum vom "+DATUM+" wurde ins System }bernommen"

SET DATE TO &DATUM

RELEASE TAG,MONAT,JAHR,JULIAN,SCHATAG,DATUM,STAG,SMONAT,SJAHR
