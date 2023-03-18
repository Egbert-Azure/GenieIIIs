;******************************************************************************
;*  F K E Y G  *  U T I L S 0 0 5 a  *  T h o m a s   H o l t e * 8 6 0 1 1 9 *
;******************************************************************************
;*                                                                            *
;*  	 L A N G U A G E   D E P E N D E N T   P A R T   O F   F K E Y	      *
;*       =============================================================        *
;*									      *
;*  			 ( G E R M A N   V E R S I O N )		      *
;*			 =============================== 		      *
;*                                                                            *
;*                                                                            *
;*  Version 1.0                                                 Thomas Holte  *
;*                                                                            *
;******************************************************************************

        psect data
        global _endmsg,_error1,_error2,_menu,_national,_no,_yes

;ASCII control codes:
LF	EQU  0AH		;line feed
ESC	EQU  1BH		;escape

_menu:	DEFM '*** CP/M 3 ***  TT.MM.JJ  HH:MM:SS  Funk'
	DEFM 'tionstastenprogrammierer  '
	DEFB 98H
	DEFM ' 1985 by T.H.'
	DEFM '                                        '
	DEFM '                                        '
	DEFM 'Dieses Dienstprogramm erm|glicht es dem '
	DEFM 'Benutzer, die Tasten F1 - F8 und P1 - P5'
	DEFM 'mit bis zu 73 Zeichen zu belegen.       '
	DEFM '                                        '
	DEFM 'Geben Sie  das Leerzeichen  mit der Unte'
	DEFM 'rl{nge "_"  und Steuerzeichen  mit einem'
	DEFM 'Zirkumflex und dem zugeh|rigen Buchstabe'
	DEFM 'n ein, z.B. "^C" f}r CONTROL-C.         '
	DEFM '                                        '
	DEFM '                                        '
	DEFM '                                        '
	DEFM '                                        '
	DEFM 'F1 -->                                  '
	DEFM '                                        '
	DEFM 'F2 -->                                  '
	DEFM '                                        '
	DEFM 'F3 -->                                  '
	DEFM '                                        '
	DEFM 'F4 -->                                  '
	DEFM '                                        '
	DEFM 'F5 -->                                  '
	DEFM '                                        '
	DEFM 'F6 -->                                  '
	DEFM '                                        '
	DEFM 'F7 -->                                  '
	DEFM '                                        '
	DEFM 'F8 -->                                  '
	DEFM '                                        '
	DEFM 'P1 -->                                  '
	DEFM '                                        '
	DEFM 'P2 -->                                  '
	DEFM '                                        '
	DEFM 'P3 -->                                  '
	DEFM '                                        '
	DEFM 'P4 -->                                  '
	DEFM '                                        '
	DEFM 'P5 -->                                  '
	DEFM '                                        '
	DEFM '                                        '
	DEFM '                                        '
	DEFM '                                        '
	DEFM '                                        '
	DEFM 'Alle Angaben richtig (J/N) -----> '
	DEFB 7FH
	DEFM '     '
	DEFM '                                        '

_error1:DEFB LF
	DEFM 'Fataler E/A-Fehler'
	DEFB LF,0
_error2:	DEFB LF
	DEFM 'Diskette schreibgesch}tzt'
	DEFB LF,0

_national:
	DEFB ESC,'G',0

_endmsg:DEFM 'Dr}cken Sie <RESET>,'
	DEFM ' um die programmierten Funktionstasten zu aktivieren'
	DEFB LF,0

_no:	DEFB 'N'
_yes:	DEFB 'J'
