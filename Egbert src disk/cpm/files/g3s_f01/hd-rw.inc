
*EJECT
;hard disk read routine:
DS$READ:LD   A,16  		;function number --> accu
	JR   DS$TASK

;hard disk write routine:
DS$WRITE:
	LD   A,17		;function number --> accu
DS$TASK:LD   (DS$MODE),A	;save function number

;*****************************************************
;* Bad block administration (sector skipping method) *
;* originally written by Andy Johnson-Laird	     *
;* modified by Thomas Holte (c) 1985		     *
;* (you've fooled me with many errors, Andy !!!)     *	Š;*****************************************************
SECTORS$PER$TRACK EQU 9 
SPARE$LENGTH	  EQU 1024	;341 entries, 3 bytes each

	LD   HL,SPARE$DIRECTORY-3
				;HL -> spare directory - 1 entry

	LD   DE,(@TRK)		;get requested track
	LD   A,(@SECT)		;get sector number

CHECK$ENTRY:
	LD   C,A		;save sector number for later
	LD   B,0   		;set counter

CHECK$ENTRY1:
	INC  HL			;update to next (or first entry)
	INC  HL
	INC  HL

	INC  B			;update count

	CALL CMPM		;compare req. track to table entry

	JR   Z,TRACKS$MATCH	;possible match of track and sector
	JR   NC,COMPUTE$INCREMENT
				;requested track < table entry
	JR   CHECK$ENTRY1	;requested track > table entry

TRACKS$MATCH:
	INC  HL			;HL -> MS byte of track
	INC  HL			;HL -> sector
	LD   A,(HL)		;get sector from table
	DEC  HL
	DEC  HL

	CP   C			;compare with requested sector
	JR   Z,SECTORS$MATCH	;track/sector matches
	JR   NC,COMPUTE$INCREMENT
				;req. trk/sec < spare trk/sec
	JR   CHECK$ENTRY1	;move to next table entry

SECTORS$MATCH:
	INC  B			;if track and sectors match with
				;a table entry, then an additional 
				;sector must be skipped

COMPUTE$INCREMENT:		;B contains number of cumulative
				;number of sectors to skip
	DEC  B			;0 sectors to skip ?
	JP   Z,DS$IO		;if yes, go to physical sector read/write 
	LD   A,C		;get requested sector
	ADD  A,B		;skip required number
	LD   B,SECTORS$PER$TRACK;determine final sector number
				;and track increment
	CALL DIV$A$BY$B		;returns C = quotient, A = remainder
	LD   (@SECT),A          ;A = new sector number

	LD   B,0		;make track increment a word
	EX   DE,HL    		;get requested track
	ADD  HL,BC		;add on incrementŠ	LD   (@TRK),HL		;save updated track
	EX   DE,HL		;get table pointer
	JP   CHECK$ENTRY	;go to physical disk read/write
 Bad block administration (sector skipping method) *
;* originally written by Andy Johnson-Laird	