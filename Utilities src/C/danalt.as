;******************************************************************************
;*  D A N A L T * X U T I L S 0 0 1 a * T h o m a s   H o l t e * 8 5 0 5 1 4 *
;******************************************************************************
;*									      *
;*  		 M E N U   F O R   D I S K   A N A L Y Z E R		      *
;*               ===========================================                  *
;*									      *
;*									      *
;*  Version 1.0						        Thomas Holte  *
;*									      *
;******************************************************************************
        psect data
	global   _national,_endmsg,_yes,_no,_menu	

;ASCII control codes:
LF	EQU  0AH		;line feed
ESC	EQU  1BH		;escape


_national:
	DEFB ESC,'G',0

_endmsg:	DEFM 'Dr}cken Sie <RESET>,'
	DEFM ' um die programmierten Funktionstasten zu aktivieren'
	DEFB LF,0

_no:	DEFB 'N'
_yes:	DEFB 'J'


_menu:	DEFM '*** CP/M 3 ***  MM/DD/YY  HH:MM:SS     D'
	DEFM 'isk Analyzer      '
	DEFB 98H
	DEFM ' 1985 by Thomas Holte'
	DEFM '                                        '
	DEFM '                                        '
	DEFM 'Drive --> '
	DEFB 7FH
	DEFM '  Track --> '
	DEFW 7F7FH
	DEFM '  Surface --> '
	DEFB 7FH
	DEFM '                                        '
	DEFM '                                        '
	DEFM '                                        '
	DEFM ' Track Side Sector Length   Track Side S'
	DEFM 'ector Length   Track Side Sector Length '

;	REPT 18
	DEFM '                          '
	DEFB 80H
	DEFM '             '
	DEFM '             '
	DEFB 80H
	DEFM '                          '
;	ENDM

	DEFB 8FH,'/',91H
	DEFM ' = previous    '
	DEFB 90H,'/',92H
	DEFM ' = next    P4/P5 = '
	DEFM 'previous/next track/side    BREAK = quit'
