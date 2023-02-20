10 '***********************************************
20 '* SUPER STAR TREK  * Diese Version fuer CP/M  *
30 '*     CO BY        * (c) by E.Schroeer        *
40 '* EGBERT SCHROEER  *                          *
50 '* ORIG. VERS. BY   *	 		  *
60 '* R.LEEDOM +       *			  *	
70 '* D.AHL            *			  *
80 '* DORSTEN,29.03.87 *	Dorsten,30.01.94  *
90 '***********************************************
100 '
120 ' Diese Version ist auf Genie IIIs Zeichensatz eingestellt.
130 ' Bitte nehmen Sie die entsprechenden Anpassungen in der
140 ' Unterroutine "Initilisierungen" vor !
160 '
170 CLEAR 600
180 '
190 ' Initialisierung
200 '
210 GOSUB 6900:PRINT CLS$:PRINT TAB(10)A$
220 PRINT TAB(10)A4$:PRINT TAB(10)A2$:PRINT TAB(10)A4$:PRINT TAB(10)A$
260 PRINT:PRINT:PRINT TAB(12)"Ben|tigen Sie Anweisungen ?  ";:GOSUB 6790:IF K$="N" OR K$="n" THEN 1680
270 PRINT CLS$:PRINT TAB(10)"Anweisungen f}r SUPER STAR TREK":PRINT TAB(10) A3$
280 PRINT:PRINT"1. Wenn Sie 'BEFEHL ?' gedruckt sehen, geben Sie einen dieser":PRINT TAB(3)"g}ltigen Befehle ein (NAV,SRS,LRS,PHA,TOR,SHE,DAM,COM":PRINT TAB(3)"oder XXX)"
290 PRINT:PRINT"2. Wenn Sie einen ung}ltigen Befehl eingeben, erhalten Sie":PRINT TAB(3)"eine kurze Liste der g}ltigen Befehle."
300 PRINT:PRINT"3. Einige Befehle erfordern die Eingabe von Daten, z.B.":PRINT TAB(3)"'NAV' erwidert mit 'KURS (1-9) ?'. Wenn Sie ungueltige Daten,":PRINT TAB(3)"z.B. negative, eingeben, wird der Befehl abgebrochen."
310 PRINT:GOSUB 6820:PRINT CLS$
320 PRINT TAB(3)"Der Weltraum ist in ein Gitter mit 8 * 8 Quadranten aufgeteilt"
330 PRINT TAB(3)"und jeder Quadrant in 8 * 8 Sektoren unterteilt.":PRINT
350 PRINT TAB(3)"Sie starten von einem bestimmten Punkt im Weltraum und"
360 PRINT TAB(3)"beginnen Ihre Laufbahn als Kommandeur des Raumschiffs ";
370 PRINT EIN$;:PRINT"ENTERPRISE";:PRINT AUS$;:PRINT" .":PRINT:PRINT
380 PRINT TAB(3)"Ihr Auftrag :":PRINT
390 PRINT TAB(10)"Die Flotte der Klingonen, die die F|deration"
400 PRINT TAB(10)"der vereinten Planeten bedrohen, zu finden und"
410 PRINT TAB(10)"zu zerst|ren.":PRINT:GOSUB 6820:PRINT CLS$
440 PRINT TAB(10)"Als Kapit{n des Raumschiffs ENTERPRISE k|nnen Sie"
450 PRINT TAB(10)"folgende Befehle eingeben:":PRINT
470 PRINT EIN$;"NAV";AUS$;" BEFEHL = MOTOREN-RICHTUNGSKONTROLLE --":PRINT:PRINT
490 PRINT TAB(10)"Der Kurs ist auf einer kreisf|rmigen      4  3  2"
500 PRINT TAB(10)"Vektoren-Bezeichnung wie angezeigt.        . . ."
510 PRINT TAB(10)"Ganze und dezimale Werte k|nnen             ..."
520 PRINT TAB(10)"verwendet werden ( d.h. Kurs 1.5        5 ---*--- 1"
530 PRINT TAB(10)"liegt halb zwischen 1 und 2)                ..."
540 PRINT TAB(53)". . ."
550 PRINT TAB(10)"Die Werte k|nnen sich bis                 6  7  8"
560 PRINT TAB(10)"auf 9.0 n{hern (= 1.0)"
570 PRINT TAB(54) EIN$;"Kurs";AUS$:PRINT
590 PRINT TAB(6)"Ein Richtungsfaktor hat die Gr|~e eines Quadranten."
600 PRINT TAB(6)"Deshalb sollten Sie Kurs 3, Faktor 1 benutzen,"
610 PRINT TAB(6)"um von Quadrant 6,5 nach 5,5 zu gelangen.":GOSUB 6820:PRINT CLS$
640 PRINT EIN$;"SRS";AUS$;" BEFEHL = KURZBEREICHS-SENSOREN ]BERPR]FUNG":PRINT:PRINT
660 PRINT TAB(10)"Gibt einen ]berblick }ber den gegenw{rtigen Quadranten.":PRINT
680 PRINT TAB(10)"Die Symbole erscheinen auf dem Bildschirm wie folgt:"
690 PRINT TAB(14)"<*> = Die Position der ";EIN$;"ENTERPRISE";AUS$
700 PRINT TAB(14)"+K+ = Klingonen-Schlachtschiff"
710 PRINT TAB(14)">!< = Station der F|deration (Auftanken,Reparatur,"
720 PRINT TAB(21)"Bewaffnen)"
730 PRINT TAB(14)" *  = Stern":PRINT
750 PRINT TAB(6)"Ein zusammenfassender Lagebericht wird auch gegeben.":GOSUB 6820:PRINT CLS$
780 PRINT EIN$;"LRS";AUS$;" BEFEHL = FERNBEREICHS-SENSOREN ]BERPR]FUNG":PRINT:PRINT
800 PRINT TAB(10)"Zeigt die Zust{nde in den Quadranten auf jeder Seite"
810 PRINT TAB(10)"der ";EIN$;"ENTERPRISE";AUS$;" (die sich in der Mitte des Bildes befindet)"
820 PRINT TAB(10)"Der Bericht wird in der Form '###' gegeben, wobei die"
830 PRINT TAB(10)"'EINER' die Anzahl der Sterne, die 'ZEHNER' die Anzahl"
840 PRINT TAB(10)"der Sternenstationen und die 'HUNDERTER' die Anzahl"
850 PRINT TAB(10)"der Klingonen sind.":PRINT
870 PRINT TAB(6)"Beispiel ";HAND$;"  207 = 2 Klingonen, keine Station, 7 Sterne":GOSUB 6820:PRINT CLS$
900 PRINT EIN$;"PHA";AUS$;" BEFEHL = PHASER-KONTROLLE.":PRINT:PRINT
920 PRINT TAB(10)"Erm|glicht Ihnen, die Schlachtschiffe der Klingonen"
930 PRINT TAB(10)"durch Beschiessen mit gen}gend viel Energie zu zerst|ren"
940 PRINT TAB(10)"(Beachten Sie: Klingonen haben auch Phaser !)":GOSUB 6820:PRINT CLS$
970 PRINT EIN$;"TOR";AUS$;" BEFEHL = PHOTONEN-TORPEDO KONTROLLE":PRINT:PRINT
990 PRINT TAB(10)"Die Torpedo-Richtung ist dieselbe wie beim Steuern."
1000 PRINT TAB(10)"Wenn Sie ein Klingonen-Schiff gen}gend stark treffen,"
1010 PRINT TAB(10)"Wird es zerst|rt und kann nicht auf Sie zur}ckfeuern."
1020 PRINT TAB(10)"Wenn Sie verfehlen, werden Sie von seinen Phasern"
1030 PRINT TAB(10)"beschossen. In jedem Fall sind Sie dem Beschuss"
1040 PRINT TAB(10)"von anderen Klingonen im Quadranten ausgesetzt."
1050 PRINT TAB(10)"Der Speicher-Computer ('COM'-Befehl) hat die"
1060 PRINT TAB(10)"M|glichkeit, die Torpedo-Flugbahn f}r Sie"
1070 PRINT TAB(10)"zu berechnen  (Wahl 2)":GOSUB 6820:PRINT CLS$
1100 PRINT EIN$;"SHE";AUS$;" BEFEHL = SCHILD-KONTROLLE":PRINT:PRINT
1120 PRINT TAB(10)"Bestimmt die Anzahl der Energieeinheiten, die zur"
1130 PRINT TAB(10)"Schild-Sicherung verwendet werden sollen, Energie"
1140 PRINT TAB(10)"wird von der Gesamtenergie des Schiffes genommen"
1150 PRINT TAB(10)"Beachten Sie, da~ die Gesamtenergie im Lagebericht"
1160 PRINT TAB(10)"die Schildenergie enth{lt.":GOSUB 6820:PRINT CLS$
1190 PRINT EIN$;"DAM";AUS$;" BEFEHL = ZERST\RUNGSKONTROLLBERICHT":PRINT:PRINT
1210 PRINT TAB(10)"Berichtet den Reparaturzustand aller Ger{te. Ein"
1220 PRINT TAB(10)"negativer 'REPARATURZUSTAND' bedeutet, das das"
1230 PRINT TAB(10)"Ger{t momentan zerst|rt ist.":GOSUB 6820:GOSUB 6850
1260 PRINT TAB(10)"Der Speicher-Computer gibt sechs Wahlm|glichkeiten":PRINT
1280 PRINT TAB(10)"WAHL 0 = ZUSAMMENFASSENDER WELTRAUMBERICHT":PRINT
1300 PRINT TAB(18)"Diese Wahl zeigt die Ergebnisse aller"
1310 PRINT TAB(18)"gespeicherten Kurz-und Fernbereichsberichte"
1320 PRINT TAB(18)"Eine Ausgabe auf den Drucker ist m|glich":PRINT 
1340 PRINT TAB(10)"WAHL 1 = LAGEBERICHT":PRINT
1360 PRINT TAB(18)"Diese Wahl zeigt die Anzahl der Klingonen,"
1370 PRINT TAB(18)"Sternendaten und im Spiel verbliebenen"
1380 PRINT TAB(18)"Sternenstationen.":PRINT
1400 PRINT TAB(10)"WAHL 2 = DATEN F]R PHOTONEN-TORPEDO":PRINT
1420 PRINT TAB(18)"Dies gibt die Richtung und Entfernung von der"
1430 PRINT TAB(18)"ENTERPRISE zu allen im Quadranten"
1440 PRINT TAB(18)"befindlichen Klingonen.":PRINT:GOSUB 6820:GOSUB 6850
1480 PRINT TAB(10)"WAHL 3 = STERNENSTATIONS-NAV-DATEN":PRINT
1500 PRINT TAB(18)"Diese Wahl gibt Richtung und Entfernung"
1510 PRINT TAB(18)"zu jeder im Quadranten befindlichen"
1520 PRINT TAB(18)"Sternenstation.":PRINT
1540 PRINT TAB(10)"WAHL 4 = RICHTUNGS-UND ENTFERNUNGSRECHNER":PRINT
1560 PRINT TAB(18)"Diese Wahl erm|glicht Eingaben zur"
1570 PRINT TAB(18)"Richtungs-und Entfernungskalkulation.":PRINT
1590 PRINT TAB(18)"WAHL 5 = KARTE DER WELTRAUM/GEBIETS-NAMEN":PRINT
1610 PRINT TAB(18)"Diese Wahl gibt die Namen der sechzehn"
1620 PRINT TAB(18)"gro~en Weltraumgebiete, die in diesem"
1630 PRINT TAB(18)"Spiel verwender werden, aus.":GOSUB 6820
1650 '
1660 ' Zeichne Enterprise Logo
1670 '
1680 PRINT CLS$
1690 FOR I=50 TO 77:PRINT CHR$(27)+CHR$(61)+CHR$(32+3)+CHR$(32+I);CHR$(151):NEXT
1720 FOR I=49 TO 51:PRINT CHR$(27)+CHR$(61)+CHR$(32+4)+CHR$(32+I);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+4)+CHR$(32+I+25);CHR$(151):NEXT
1760 FOR I=18 TO 73:PRINT CHR$(27)+CHR$(61)+CHR$(32+5)+CHR$(32+I);CHR$(151):NEXT
1790 FOR I=44 TO 50:PRINT CHR$(27)+CHR$(61)+CHR$(32+5)+CHR$(32+I);SPC(1):NEXT
1820 FOR I=16 TO 19:PRINT CHR$(27)+CHR$(61)+CHR$(32+6)+CHR$(32+I);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+6)+CHR$(32+I+25);CHR$(151):NEXT
1822 PRINT CHR$(27)+CHR$(61)+CHR$(32+6)+CHR$(32+59);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+6)+CHR$(32+63);CHR$(151)
1860 FOR I=19 TO 40:PRINT CHR$(27)+CHR$(61)+CHR$(32+7)+CHR$(32+I);CHR$(151):NEXT
1862 PRINT CHR$(27)+CHR$(61)+CHR$(32+7)+CHR$(32+58);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+7)+CHR$(32+62);CHR$(151)
1890 FOR I=35 TO 36:PRINT CHR$(27)+CHR$(61)+CHR$(32+7)+CHR$(32+I);SPC(1):NEXT
1892 PRINT CHR$(27)+CHR$(61)+CHR$(32+8)+CHR$(32+34);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+8)+CHR$(32+36);CHR$(151)
1894 PRINT CHR$(27)+CHR$(61)+CHR$(32+8)+CHR$(32+57);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+8)+CHR$(32+61);CHR$(151)
1920 FOR I=28 TO 67:PRINT CHR$(27)+CHR$(61)+CHR$(32+9)+CHR$(32+I);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+12)+CHR$(32+I);CHR$(151):NEXT
1922 PRINT CHR$(27)+CHR$(61)+CHR$(32+9)+CHR$(32+34);SPC(1):PRINT CHR$(27)+CHR$(61)+CHR$(32+9)+CHR$(32+35);SPC(1)
1960 FOR I=57 TO 59:PRINT CHR$(27)+CHR$(61)+CHR$(32+9)+CHR$(32+I);SPC(1):NEXT
1990 FOR I=29 TO 30:PRINT CHR$(27)+CHR$(61)+CHR$(32+10)+CHR$(32+I);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+10)+CHR$(32+I+38);CHR$(151)
1992 PRINT CHR$(27)+CHR$(61)+CHR$(32+11)+CHR$(32+I);CHR$(151):PRINT CHR$(27)+CHR$(61)+CHR$(32+11)+CHR$(32+I+38);CHR$(151):NEXT:PRINT:PRINT
2030 PRINT:PRINT:PRINT TAB(25)"DIE USS  ENTERPRISE --- NC 1701"
2040 PRINT TAB(25)EIN$;"READY FOR PLAY==>PRESS <SPACE>";AUS$
2050 RUN"STARGAME.BAS"
6790 PRINT"J/N"
6800 K$=INKEY$:IF K$="" THEN 6800
6810 RETURN
6820 K$=INKEY$:IF K$="" THEN 6820
6830 IF K$<>"J" THEN K$="N"
6840 RETURN
6850 ' ]berschrift f}r Anleitung COM-Befehl
6855 '
6860 PRINT CLS$:PRINT EIN$;"COM";AUS$;" BEFEHL = SPEICHER-COMPUTER":PRINT:PRINT
6890 RETURN
6900 '
6910 ' Initialisierung
6920 '
6930 A$ ="**************************************"
6940 A4$="*                                    *"
6950 Z$="                          "
6960 A2$="*  * *  S U P E R  STAR TREK  * *    *"
6970 A3$="======================================"
6980 DIM G(8,8),C(9,2),K(3,3),N(3),Z(8,8),D(8)
6990 CLS$=CHR$(26):WIDTH 255
7010 EIN$=CHR$(27)+CHR$(82):AUS$=CHR$(27)+CHR$(83):CLS$=CHR$(26)
7020 HAND$=CHR$(&H99)+CHR$(&H9A)+CHR$(&H9B)
7030 RETURN
