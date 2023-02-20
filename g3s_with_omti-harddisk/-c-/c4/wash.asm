;	TITLE 'MICRO RESOURCES "WASH" Directory Maintence Utility Ver 1.4'

;*************************************************************************
;    MICRO RESOURCES "WASH"; THE SUPER DIRECTORY MAINTENANCE UTILITY
;*************************************************************************
;
;
;	This program is a super-duper CP/M disk directory maintence
;	utility that is designed as an "almost" all inclusive routine
;	to make it easy to do disk directory house keeping. The
;	inspiration to produce this program came from use of an older
;	utility program called "CLEAN" that I came across at a meeting of
;	the Valley Computer Club about a year and a half ago. At that
;	time "CLEAN" seemed like a dream come true. Unfortunately it
;	had several major problems that limited its overall usefullness.
;	The disadvantages of CLEAN have all been overcome with the new
;	WASH program. Program features are listed below:
;
;		a) Alphabetical list oriented file operations
;
;		b) Any legal CP/M drive (A: to P:) may be selected
;
;		c) Operator interface to the file list is at the console
;		   in sequential apha order in forward or backup mode.
;
;		d) The file list is treated as a circular buffer. Forward
;		   or backward scanning of the list wraps around the list
;		   back to the beginning or ending respectively.
;
;		e) The current list position file can be viewed at the
;		   Console, printed on the CP/M List device, or sent
;		   to the CP/M Punch device.
;
;		f) The current list position file may be deleted or renamed.
;		   If renamed, only the new name must be entered.
;
;		g) The current list position file may be copied, with the
;		   same name, to any other operator selected disk drive.
;		   The copy utilizes all of available memory as the copy
; 		   buffer for the ultimate in copy speed.
;
;	      	h) The current list can be deleted and the "WASH" operation
;		   may be begun upon another operator selected disk drive.
;
;		i) The program is fully implemented in 8080 assembly language
;		   for speed, small size, and portability to any CP/M 2.2
;		   or 1.4 system. No assumption is made upon the maximum
;		   number of directory files other than available memory
;		   space for the list. (A directory with 1024 directory
;		   file names takes 12 K bytes of storage. Assuming the
;		   copy buffer minimum size requirement of 128 bytes, then
;		   WASH should easily run in the minimum CP/M 2.2 20K
;		   System with no problems at all.) ALL directory and
;		   disk I/O is handled through calls to the BDOS. This
;		   will guarentee WASH compatibility with any CP/M system
;		   implementation. This makes the program disk media
;		   independent (all you have to do is get WASH.COM onto
;		   your diskette or hard disk in the first place.
;
;	Complete documentation on the operation of WASH is given in the
;	accompaning documentation file "WASH.DOC". For the most part the
;	program is self documenting and contains extensive error message
;	reporting.
;
;
;		This Program was Written by:
;
;		Michael J. Karas
;		MICRO RESOURCES
;		2468 Hansen Court
;		Simi Valley, California 93065
;		(805) 527-7299
;		September 20, 1981
;
;	NOTE:  The WASH program, its source code, documentation file,
;	       and object code, has been released to the PUBLIC DOMAIN
;	       by Michael J. Karas. This program may be modified to suit
;	       your personal requirements or those of your friends. In
;	       any case no COMMERCIAL or MONEY MAKING ventures with
;	       regard to SOFTWARE SALES or MODIFICATION and the subsequent
;	       SALE of the WASH program in WHOLE or any PART is permitted
;	       by the author. Further modification and public domain
;	       distribution of the WASH program must include:
;			a) This NOTE,
;			b) The name "WASH" must be retained,
;			c) The original authorship notice
;			   from above, and
;			d) The MICRO RESOURCES Name in the Sign-on
;			   Menu.
;
;	MICRO RESOURCES reserves the right to modify this program at any
;	time for any purpose. The Intent of the above NOTE is intended
;	for the public domain distribution of the WASH program and
;	MICRO RESOURCES reserves the right to utilize the WASH program
;	for any application whatsoever including but not limited to
;	commercial distribution and modification for custom applications
;	with or without the "WASH" name.
;
;
;
;	Modification History:
;
;	If you modify, enhance, or correct bugs in this program, please
;	include a short statement of the modifications done and include
;	your name and the date. The modification history log should be
;	kept intact with this source code in "most recent first" order.
;	Changes to program structure will generally require a change in
;	the program version. The version number is documented in the
;	signon message and the distribution program name as "WASH-10.ASM"
;	in the specific case of the initial release 1.0.
;
;	Date: Oct 13, 1981  Version Number: 1.4  Bob Zimmerman
;	Changed Delete (D) command to ^D to make deleting files
;	a two-handed operation
;
;	9-26-81 V 1.2 Chuck Forsberg
;
;	Changed BDOS write record call to detect ANY non zero return as
;	error in accordance with CP/M 2.2 documentaion.  This fix should
;	minimize file diversion to the proverbial bit bucket.
;	Other BDOS calls checked for the same error.
;
;
;	Date: Sept. 25, 1981  Version Number: 1.3  Name: Ted Shapin
;
;	Added BASE for standard or modified CP/M systems.  Added BUFR
;	equate for same purpose and to make code clearer.  Shortened long
;	labels to be unique in first six chars.  Fixed code to compute
;	buffer address correctly for BUFR other than at 80H.  Reprint
;	help menu on any unknown command.  Changed abort to stay at same
;	position in list (good for abort while viewing file).
;	CP/M 1.4 always returns 0FFH when
;	deleting a file, therefore after a file was deleted always
;	printed "Not found".  I added a switch to bypass this code.
;
;	Date: Sept. 24, 1981   Version Number: 1.1  Name: Keith B. Petersen
;
;	Fixed problem with incorrect drive select when default disk used
;	and running in other than user 0.  Added code to turn up two new
;	lines to console when entering VIEW mode.
;
;	Date: Sept. 20, 1981   Version Number: 1.0  Name: Michael J. Karas
;
;	Initial release to the public domain via the CP/M NET remote
;	software access program operated by Kelly Smith, 3055 Waco Ave,
;	Simi Valley, CA 93063. (805) 527-9321/ PMMI modem.
;
;******************************************************************************
;
;
;
;
;DEFINE TRUE AND FALSE ASSEMBLY PARAMETERS
;
FALSE	EQU	0       	;DEFINE FALSE
TRUE	EQU	NOT FALSE	;DEFINE TRUE
;
BASE	EQU	0H		; 0 FOR STD, 4200H FOR MODIFIED CP/M
BUFR	EQU	80H+BASE	; DEFAULT CP/M BUFFER
CPM14	EQU	FALSE 		; TRUE IF RUNNING 1.4
;
;
;CP/M BDOS INTERFACE EQUATES
;
WBOOT	EQU	0+BASE		;WARM BOOT ENTRY ADRS
KEYIN	EQU	1		;CONSOLE INPUT FUNCTION
WRCON	EQU	2		;WRITE CHARACTER TO CONSOLE
WRLST	EQU	5		;WRITE CHARACTER TO LIST DEVICE
WRPUN	EQU	4		;WRITE CHARACTER TO PUNCH DEVICE
LOGDRIV	EQU	0004H+BASE	;LOCATION OF CURRENTLY LOGGED DRIVE
BDOS	EQU	0005H+BASE	;THE BDOS I/O VECTOR
PRINT	EQU	9		;PRINT STRING (DE) UNTIL '$'
GETBUF	EQU	10		;READ INPUT STRING
GETST	EQU	11		;GET CONSOLE STATUS
OPEN	EQU	15		;OPEN FILE
CLOSE	EQU	16		;CLOSE FILE
SRCHF	EQU	17		;SEARCH DIR FOR FIRST OCCUR.
SRCHN	EQU	18		;SEARCH DIR FOR NEXT OCCUR.
DELETE	EQU	19		;ERASE FILE
READR	EQU	20		;READ RECORD
WRITER	EQU	21		;WRITE RECORD
MAKE	EQU	22		;MAKE FILE
RENAM	EQU	23		;RENAME FILE
STDMA	EQU	26		;SET DMA ADDRESS
FCB	EQU	5CH+BASE	;DEFAULT FILE CONTROL BLOCK
FCBEXT	EQU	FCB+12      	;EXTENT BYTE IN FCB
FCBRNO	EQU	FCB+32		;RECORD NUMBER IN FCB
;
;
;ASCII CHARACTER DEFINITIONS
;
BS	EQU	008H		;ASCII BACK SPACE CHARACTER
LF	EQU	00AH		;ASCII LINE FEED CHARACTER
CR	EQU	00DH		;ASCII CARRIAGE RETURN CHARACTER
ESC	EQU	01BH		;ASCII ESCAPE CHARACTER
RUBOUT	EQU	07FH		;ASCII RUBOUT CHARACTER
EOT	EQU	004H		;ASCII EOT CHARACTER (^D)
;
;*****************************************************************************
;
;
	ORG	0100H+BASE
;
	DI
	LXI	SP,STCKK	;SETUP LOCAL UTILITY STACK
;
RESTART:
	CALL	MENU
;
;
;START UP BY ASSEMBLING LIST OF FILE NAMES OF SPECIFIED DISK DRIVE
;
	LDA	FCB+1		;CHECK IF A COMMAND PARAMETER WAS ENTERED
	CPI	' '		;..FILE NAME = SPACES?
	JNZ	NAMEPRES	;NAME NOT SPACE SO PARM WAS ENTERED
	LDA	FCB+9		;EXTENT NAME ALSO SPACES?
	CPI	' '		;..IF SO THEN MUST TREAT AS *.*
	JNZ	NAMEPRES
;
;
;NO COMMAND PARAMETER FOR A FILE NAME SO SET LIKE *.*
;
	LXI	H,ALFN		;POINT AT ALL FILE WILD CARD NAME
	LXI	D,FCB+1		;PLACE TO PUT IT
	MVI	B,11		;SIZE TO SET
	CALL	MOVBYT		;SET FIELD TO *.*
	JMP	NAMEPRES
;
ALFN:
	DB	'???????????'	;DUMMY ALL FILE SPECIFIED NAME
;
;
;HERE IF NAME PROPERLY POSITIONED IN THE DEFAULT FCB AREA FOR LIST BUILD
;
NAMEPRES:
	MVI	C,STDMA		;INITIALIZE DMA ADDRESS TO DEFAULT BUFFER
	LXI	D,BUFR
	CALL	BDOS
;
	XRA	A		;CLEAR APPROPIATE FIELDS OF SEARCH FCB
	STA	FCBEXT		;EXTENT BYTE
	STA	FCBRNO		;AND RECORD NUMBER
;
	LXI	D,FCB		;USE DEFAULT FCB FOR SEARCH
	MVI	C,SRCHF		;SEARCH FOR FIRST OCCURRANCE
	CALL	BDOS
	CPI	0FFH		;SEE IF FOUND
	JNZ	LOADLIST	;IF SOME FOUND THEN GO BUILE LIST
;
	CALL	PRTMSG		;TELL THEM THAT NONE FOUND
	DB	CR,LF,' ++ Not Found ++',0
	JMP	EXIT
;
;
MENU:
	CALL	PRTMSG		;PRINT SIGNON MESSAGE
;
	DB	CR,LF,'   MICRO RESOURCES DIRECTORY "WASH UTILITY" Ver 1.3'
	DB	CR,LF
	DB	CR,LF,'         Command        Function'
	DB	CR,LF,'         -------    ----------------------------'
	DB	CR,LF,'            V       View file at Console'
	DB      CR,LF,'                     (any key aborts)'
	DB      CR,LF,'            L       Print file to List Device'
	DB	CR,LF,'            P       Send file to Punch Device'
	DB	CR,LF,'            C       Copy file to another Disk'
	DB	CR,LF,'            R       Rename file'
	DB	CR,LF,'           ^D       Delete file'
	DB	CR,LF,'            X       Exit to CP/M'
	DB	CR,LF,'            B       Backup one file in List'
	DB	CR,LF,'            S       Restart on another Drive'
	DB	CR,LF,'         sp or cr   Forward to next file in List'
	DB	CR,LF,'H or anything else  Display this help message'
	DB	CR,LF,CR,LF,CR,LF,0
	RET
;
;
;BUILD UP LIST WITH ALL FOUND ENTRIES
;
LOADLIST:
	LXI	H,LIST		;INITIALIZE LIST POINTER PARAMETERS
	SHLD	LISTPOS		;START = CURRENT POS OF LIST
;
;
;PUT CURRENTLY FOUND NAME TO LIST
;(A) = OFFSET IN DEFAULT BUFFER OF NAME
;
;
NM2LST:
	ANI	3		;ZERO BASED TWO BIT INDEX
	ADD	A		;TIMES 32 TO MAKE POSITION INDEX
	ADD	A
	ADD	A
	ADD	A
	ADD	A
	MOV	C,A		;PUT IN BC
	XRA	B		;CLEAR HIGH ORDER
	LXI	H,BUFR		;TO NAME POSITION IN DEFAULT BUFFER
	DAD	B       	;(HL) = CURRENT FOUND NAME POINTER
	LDA	FCB		;PUT DISK DRIVE NUMBER INTO NAME PLACE
	ORA	A		;SEE IF DEFAULT DRIVE
	JNZ	NOTDEF		;SKIP SETTING IF NOT DEFAULT
	LDA	LOGDRIV
	ANI	0FH		;GET RID OF USER NUMBER
	INR	A		;MAKE 1 = A:
NOTDEF:
	MOV	M,A		;INTO BUFFER
	XCHG
	LHLD	LISTPOS		;POINTER TO CURRENT LOAD POINT IN LIST
	XCHG
	MVI	B,12		;MOVE DRIVE DESIGNATOR AND NAME TO LIST
	CALL	MOVBYT
	XCHG			;(DE) WAS LEFT WITH LEXT LOAD POINT ADDRESS
	SHLD	LISTPOS
;
;
;SEARCH FOR NEXT OCCURANCE OF SPECIFIED FILE NAME
;
	MVI	C,SRCHN		;SEARCH NEXT FUNCTION CODE
	LXI	D,FCB		;FILE NAME SPECIFICATION FIELD
	CALL	BDOS
	CPI	0FFH		;SEE IF ALL THROUGH DIRECTORY YET
	JNZ	NM2LST	;IF NOT GO PUT NAME INTO LIST
;
;
;ALL FILE NAMES NOW IN LIST SO LETS SETUP PARAMETERS ASSOCIATED
;THE LIST SIZE AND COPY BUFFER START POINT.
;
	LHLD	LISTPOS		;NEXT LOAD POINT OF LIST IS START OF BUFFER
	SHLD	LISTEND		;SET LIST END MARKER
	SHLD	BUFSTART
	LXI	D,LIST+12	;COMPARE LISTEND TAB BASE+12
	CALL	CDEHL
	JZ	CMDST		;GO TO COMMAND LOOP IF NO SORT
;
;
;SORT LIST OF FILE NAMES IN ALPHA ORDER AS AN OPERATOR NICEITY
;THIS MAY TAKE A LITTLE WHILE AS THIS IS A VERY SIMPLE SORT ROUTINE
;
SORT:
	LXI	H,LIST		;INITIALIZE I SORT VARIABLE
	SHLD	LISTI
	LXI	D,12		;INITIALIZE ALSO THE J VARIABLE
	DAD	D
	SHLD	LISTJ
;
SORT1:
	LHLD	LISTJ		;COMPARE NAMES I & J
	XCHG
	LHLD	LISTI
	PUSH	H		;SAVE POSITION POINTERS FOR MAYBE SWAP
	PUSH	D
	MVI	B,12		;COMPARE SIZE
	CALL	CMPSTR		;GO COMPARE
	POP	D
	POP	H
	MVI	B,12
	CC	SWAP		;GO SWAP IF J STRING LARGER THAN I
;
	LHLD	LISTJ		;INCREMENT J POINTER
	LXI	D,12
	DAD	D
	SHLD	LISTJ
	XCHG			;SEE IF END OF J LOOP
	LHLD	LISTEND
	CALL	CDEHL
	JNZ	SORT1		;..NO SO MORE J LOOP
;
	LHLD	LISTI		;BUMP I POINTER
	LXI	D,12
	DAD	D
	SHLD	LISTI
	DAD	D		;SET START OVER J POINTER
	SHLD	LISTJ
	XCHG			;SEE IF END OF I LOOP
	LHLD	LISTEND
	CALL	CDEHL
	JNZ	SORT1		;MUST BE MORE I LOOP TO DO
;
;
;FILE PROCESSING AND DISPLAY LOOP
;
CMDST:
	LXI	H,LIST		;SET START POINT OF LISTING
	SHLD	LISTPOS
;
;
;DISPLAY CURRENT FILE POSITION
;
LOOP:
	LHLD	LISTPOS		;MOVE FROM LOCATION
	LXI	D,PNAME		;START POINT OF LIST POINT
	MVI	B,1		;ONE CHARACTER OF DRIVE ID TO MOVE
	CALL	MOVBYT
;
	LXI	D,PNAME+3	;PLACE TO PUT FILE NAME FOR PRINT
	MVI	B,8		;8 CHARS IN FILE NAME
	CALL	MOVBYT		;MOVE NAME FIELD
;
	LXI	D,PNAME+12	;PLACE TO PUT EXTENSION FOR PRINT
	MVI	B,3		;3 CHARS IN FILE TYPE
	CALL	MOVBYT		;MOVE TYPE NAME
;
	SHLD	LISTPOS		;SAVE NEXT POSITION OF LISTING
;
	LDA	PNAME		;MAKE DRIVE NAME PRINTABLE
	ADI	'A'-1		;THIS ONE O.K. SO MAKE DRIVE PRINTABLE
	STA	PNAME
;
SAMEPOS:
	CALL	PRTMSG		;PRINT CURRENT READY TO TRANSFER FILE NAME
PNAME:
	DB	'X: XXXXXXXX.XXX   : ' ;DUMMY FILE NAME PRINTING FIELD
	DB	0
;
	MVI	C,KEYIN		;CONSOLE CHAR IN FUNCTION
	CALL	BDOS		;GET THE CHARACTER
	CALL	UPCASE
	CPI	' '		;IF SPACE THEN ON TO NEXT FILE
	JZ	NEXTPOS
	CPI	CR		;IF CARRIAGE RETURN THEN ON TO NEXT FILE
	JZ	NEXTPOS
	CPI	'B'		;IF BACKUP THEN ADJUST POSITION POINTER -1 FILE
	JZ	BACKUP
	CPI	'X'		;IF EXIT THEN TO CP/M
	JZ	EXIT
	CPI	'R'		;IF RENAME THEN GO DO IT
	JZ	RENAME
	CPI	EOT		;TO DELETE A FILE IF "^D"
	JZ	DELFLE
	CPI	'V'		;IF TO VIEW FILE AT CONSOLE
	JZ	VIEW
	CPI	'L'		;IF TO PRINT FILE TO LIST DEVICE
	JZ	PRTFIL
	CPI	'P'		;IF TO SEND FILE TO THE PUNCH DEVICE
	JZ	PUNFIL
	CPI	'C'		;IF TO COPY FILE TO ANOTHER DISK
	JZ	COPY
	CPI	'S'		;IF TO START OVER ON ANOTHER DRIVE
	JZ	LOG
CMDERR:
	CALL	MENU  		;GIVE HELP MESSAGE
	JMP	SAMEPOS		;STAY IN THIS POSITION
;
BACKUP:
	LHLD	LISTPOS		;SEE IF AT BEGINNING OF LIST
	LXI	D,LIST+12
	CALL	CDEHL
	JNZ	BAKUP1		;SKIP POSITION POINTER RESET IF NOT
				;..AT BEGINNING
	CALL	PRTMSG
	DB	CR,LF,CR,LF,'      Beginning of List',CR,LF,0
	LHLD	LISTEND		;SET TO END +1 TO BACKUP TO END
	LXI	D,12
	DAD	D
	SHLD	LISTPOS
;
BAKUP1:
	CALL	PRTMSG		;PRINT BACKUP INDICATOR
	DB	CR,LF,'<  ',0
	LHLD	LISTPOS
	LXI	D,12*2		;ONE LIST POSITION BACKWARDS SIZE
	CALL	SDEHL		;SUBTRACT
	SHLD	LISTPOS
	JMP	LOOP		;DISPLAY THAT GUY WITHOUT CRLF
;
NEXTPOS:
	CALL	PRTMSG		;DO CARRIAGE RETURN LINE FEED
	DB	CR,LF,0
	LHLD	LISTPOS		;SEE IF AT END OF LOOP YET
	XCHG
	LHLD	LISTEND
	CALL	CDEHL		;COMPARE
	JNZ	LOOP		;GO ON TO NEXT POSITION TO PRINT
;
;
;AT END OF DIRECTORY. PRINT MESSAGE AND GO BACK TO TOP
;
	CALL	PRTMSG		;PRINT END OF DIRECTORY MESSAGE
	DB	CR,LF,'      End of List',CR,LF,CR,LF,0
	LXI	H,LIST		;SET POSITION POINTER TO BEGINNING
	SHLD	LISTPOS
	JMP	LOOP		;TO REDISPLAY START ENTRY
;
;
;CLEAN COMMAND LOOP EXIT POINT TO CP/M
;
EXIT:
	JMP	WBOOT		;BACK TO CP/M
;
;
;ROUTINE TO RENAME THE FILE CURRENTLY DISPLAYED
;
RENAME:
	LHLD	LISTPOS		;MOVE NAME FROM LIST TO RENAME FCB
	LXI	D,12
	CALL	SDEHL		;POINT TO NAME POSITION
	LXI	D,DESTFCB	;PLACE TO MOVE THE NAME
	MVI	B,12		;AMOUNT TO MOVE
	CALL	MOVBYT
;
	CALL	PRTMSG		;NEW NAME PROMPT
	DB	'  New Name ? ',0
	MVI	C,GETBUF	;GET COMMAND BUFFER FUNCTION
	LXI	D,CBUFF		;PLACE TO PUT COMMAND LINE
	CALL	BDOS
;
;
;CONVERT TO UPPER CASE
;
	LXI	H,CBUFFL	;GET BUFFER LENGTH
	MOV	B,M		;..TO (B)
	XRA	A
	ORA	B		;IF ZERO LENGTH SKIP RENAME
	JZ	NEXTPOS
;
CONV:
	INX	H		;POINT AT A CHAR TO CONVERT
	MOV	A,M
	CALL	UPCASE
	MOV	M,A		;PUT BACK INTO BUFFER
	DCR	B
	JNZ	CONV
;
;
;SCAN INPUT BUFFER TO MOVE A FILE NAME TO THE RENAME POSITION OF
;DESTINATION FCB
;
	LXI	H,DESTFCB+16
	MVI	M,0		;BDOS EXPECTS ZERO HERE
	INX	H
;
;
;PREBLANK THE NEW FILE NAME FIELD
;
	PUSH	H		;SAVE START POINTER
	MVI	B,11		;NUMBER OF SPACES TO BLANK
PREFILL:
	MVI	M,' '		;PUT IN A SPACE CODE
	INX	H
	DCR	B		;CHECK BYTE COUNT TO SEE IF DONE
	JNZ	PREFILL
	POP	H
;
	XCHG
	LXI	H,CBUFFL	;GET LENGTH TO (C)
	MOV	C,M
	INX	H
	XCHG			;(DE) = BUFFER POINTER
				;..& (HL) = FCB POINTER
;
;
;PURGE SPACES OFF THE BEGINNING OF BUFFER
;
DEBLANK:
	LDAX	D		;GET A CHARACTER
	CPI	' '
	JNZ	EXTND		;NOT BLANK, WE HAVE SOMETHING
	INX	D		;TO NEXT CHAR
	DCR	C
	JZ	CMDERR		;ALL SPACES SO ERROR
	JMP	DEBLANK
;
;
;EXTEND BUFFER TO BLANKS BEYOND COMMAND LENGTH
;
EXTND:
	PUSH	H
	MOV	L,C		;DOUBLE BYTE REMAINING LENGTH
	MVI	H,0
	DAD	D		;TO BUFFER END +1
	MVI	M,' '		;FORCE ILLEGAL CHAR END
	POP	H
;
;
;START FILE NAME SCAN
;
SCAN:
	MVI	B,8		;MAX OF 8 CHAR IN FILE NAME
SCAN1:
	CALL	CHKLEGL		;GET AND SEE IF LEGAL CHARACTER
	JC	CMDERR		;CHECK IF ALL OF COMMAND LINE
	CPI	' '		;SEE IF END OF PARAMETER FIELD
	JZ	CPYBITS		;GO TO RENAME THE FILE
	CPI	'.'		;AT END OF FILE NAME
	JZ	SCAN2		;GO TO PROCESS THE FILE TYPE FIELD
	MOV	M,A		;PUT CHAR INTO DESTINATION FCB
	INX	H
	DCR	B		;CHECK NAME CHARACTER COUNT
	JNZ	SCAN1
;
;
;GOT HERE IF EIGHT CHAR WITHOUT A "."
;
SCAN1A:
	CALL	CHKLEGL		;SCAN BUFFER UP TO PERIOD
				;OR END
	JC	CPYBITS		;NO EXTENT IF NOT LEGAL
	CPI	' '		;END OF PARAMETER FIELD?
	JZ	CPYBITS
	CPI	'.'
	JZ	SCAN2		;NOW GO DO EXTENT FIELD
	JMP	SCAN1A		;DO TILL END OR PERIOD
;
;
;NOW BUILD THE EXTENT FIELD
;
SCAN2:
	MVI	B,3		;MAX LENGTH OF EXTENT FIELD
	LXI	H,DESTFCB+25	;DESTINATION RENAME EXTENT START
SCAN3:
	CALL	CHKLEGL		;GET AND CHECK CHARACTER
	JC	SCAN4		;NAME DONE IF ILLEGAL
	CPI	' '		;END OF PARAMETER FIELD?
	JZ	SCAN4
	CPI	'.'		;CHECK IF ANOTHER PERIOD
	JZ	SCAN4
	MOV	M,A
	INX	H
	DCR	B
	JNZ	SCAN3		;GET NEXT EXTENT CHARACTER
;
SCAN4:
	MVI	B,4		;FILL EX,S1,S2,RC WITH ZEROS
	LXI	H,DESTFCB+28	;SET POINTER TO RENAME EXTENT FIELD
SCAN5:
	MVI	M,0
	INX	H
	DCR	B
	JNZ	SCAN5
;
;
;COPY OLD FILES ATTRIBUTE BITS TO THE NEW FILE NAME TO HAVE THEN BE
;THE SAME AS OLD FILE NAME
;
CPYBITS:
	LXI	D,DESTFCB+1	;FIRST CHAR OF OLD NAME
	LXI	H,DESTFCB+17	;FIRST CHAR OF NEW NAME
	MVI	C,011		;NUMBER OF BYTES WITH TAG BITS
CBITS1:
	LDAX	D		;FETCH BIT OF OLD NAME CHAR
	ANI	080H		;STRIP ONLY UPPER BIT OUT
	MOV	B,A		;SAVE BIT INTO (B)
	MVI	A,07FH		;FETCH A MASK FOR CHAR ONLY
	ANA	M		;GET MASKED CHAR TO (A)
	ORA	B		;PUT OLD BIT IN
	MOV	M,A		;SAVE NEW BYTE BACK
	INX	H		;BUMP COPY POINTERS
	INX	D
	DCR	C		;DEC BYTE COPY COUNT
	JNZ	CBITS1
;
;
;SEE IF THE NEW FILE NAME ALREADY EXISTS. IF SO SAY SO AND THEN GO
;TO THE COMMAND LOOP AGAIN AT SAME POSITION
;
	LDA	DESTFCB		;COPY NEW NAME TO SOURCE FCB
	STA	SORCFCB
	MVI	B,11
	LXI	H,DESTFCB+17	;COPY NEW NAME TO
	LXI	D,SORCFCB+1	;..SOURCE FCB FOR EXISTENCE CHECK
	CALL	MOVBYT
	MVI	B,4		;ZERO THE OPERATIONAL FIELDS
	LXI	H,SORCFCB+12
RNFILLP:
	MVI	M,0		;PUT A ZERO IN THERE
	INX	H		;TAKE CARE OF COUNTERS
	DCR	B
	JNZ	RNFILLP
;
	LXI	D,SORCFCB	;SEARCH TO SEE IF THIS FILE EXISTS
	MVI	C,SRCHF		;SEARCH FIRST FUNCTION
	CALL	BDOS
	CPI	0FFH		;RETURN CODE IF NOT FOUND
	JZ	RNFILE		;TO RENAME COMMAND IF NOT A DUPLICATE NAME
;
	CALL	PRTMSG		;ASK IF TO REPLACE
	DB	CR,LF,' ++ Name Already Exists ++',CR,LF,0
	JMP	SAMEPOS		;BACK TO LET HIM TRY ALL THAT AGAIN ON
				;... THE SAME FILE NAME
;
;
;COPY NEW NAME INTO LIST POSITION IN CASE HE BACKS UP
;
RNFILE:
	LHLD	LISTPOS		;GET LIST POSITION POINTER
	LXI	D,11		;BACK 11 TO LEAVE DRIVE ID THE SAME
	CALL	SDEHL
	XCHG
	LXI	H,DESTFCB+17	;POINT AT THE NEW NAME
	MVI	B,11		;AMOUNT TO MOVE
	CALL	MOVBYT
;
;
;NOW DO THE RENAME BDOS FUNCTION
;
RNFIL1:
	LXI	D,DESTFCB	;RENAME FCB LOCATION
	MVI	C,RENAM		;RENAME FUNCTION CODE
	CALL	BDOS		;GO DO IT
	CPI	0FFH		;SEE IF RENAME ERROR
	JNZ	NEXTPOS		;IF O.K. PROCEDE
	CALL	PRTMSG		;OR ELSE PRINT ERROR MESSAGE
	DB	' ++ Not Found ++',0
	JMP	NEXTPOS		;GO TO NEXT LIST POSITION
;
;
;DELETE A FILE NAME AS SPECIFIED IN CURRENT POSITION POINTER
;
DELFLE:
	LHLD	LISTPOS		;MOVE NAME FROM LIST TO RENAME FCB
	LXI	D,12
	CALL	SDEHL		;POINT TO NAME POSITION
	LXI	D,DESTFCB	;PLACE TO MOVE THE NAME
	MVI	B,12		;AMOUNT TO MOVE
	CALL	MOVBYT
;
;
;DELETE THE FILE NOW
;
	LXI	D,DESTFCB	;POINT AT DELETE FCB
	MVI	C,DELETE	;DELETE FUNCTION CODE
	CALL	BDOS
	IF	NOT CPM14	;AVOID 1.4 BUG
	CPI	0FFH
	JNZ	DELFL1		;FILE DELETED O.K.
	CALL	PRTMSG		;TYPE ERROR MESSAGE
	DB	' ++ Not Found ++',0
	JMP	NEXTPOS
	ENDIF
;
DELFL1:
	CALL	PRTMSG		;SAY IT HAS BEEN DELETED
	DB	'  Deleted',0
;
;
;MOVE LIST UP ONE POSITION TO CLOSE UP THE ERASED POSITION
;
	LHLD	LISTPOS		;FIXUP MOVE UP POINTERS
	PUSH	H
	LXI	D,12
	CALL	SDEHL
	SHLD	LISTPOS		;RESET CURRENT POSITION FOR MOVE
	XCHG			;(DE) = TO LOCATION
	POP	H		;(HL) = FROM LOCATION
;
MOVUP:
	XCHG
	PUSH	H		;CHECK IF AT END
	LHLD	LISTEND		;GET OLD END POINTER
	CALL	CDEHL		;CHECK AGAINST CURRENT END LOCATION
	POP	H
	XCHG
	JZ	MOVDONE		;MUST BE AT END OF LIST
	MVI	B,12		;ONE NAME SIZE
	CALL	MOVBYT		;MOVE ONE NAME UP
	JMP	MOVUP		;GO CHECK END PARAMETERS
;
MOVDONE:
	XCHG
	SHLD	LISTEND		;SET NEW LIST END IF ALL MOVED
	LXI	D,LIST		;SEE IF LIST IS EMPTY
	CALL	CDEHL		;..IE LISTEND=LISTPOS=LIST
	JNZ	NEXTPOS
	LHLD	LISTPOS
	CALL	CDEHL
	JNZ	NEXTPOS		;NEITHER EQUAL SO NOT EMPTY
	CALL	PRTMSG
	DB	CR,LF,CR,LF,'      List Empty',0
	JMP	EXIT		;BALE OUT IF DONE
;
;
;ENTRY POINT TO SEND CURRENT FILE NAME CONTENTS TO THE CONSOLE
;
VIEW:
	CALL	PRTMSG
	DB	CR,LF,CR,LF,0	;TURN UP TWO NEW LINES
	MVI	A,WRCON		;WRITE CONSOLE OUT FUNCTION
	JMP	OUTCM		;TO COMMON I/O PROCESSING
;
;
;ENTRY POINT TO SEND CURRENT FILE NAME CONTENTS TO THE LIST DEVICE
;
PRTFIL:
	MVI	A,WRLST		;WRITE LIST DEVICE OUT FUNCTION
	JMP	OUTCM		;TO COMMON I/O PROCESSING
;
;
;ENTRY POINT TO SEND CURRENT FILE NAME CONTENTS TO THE PUNCH DEVICE
;
PUNFIL:
	MVI	A,WRPUN		;WRITE PUNCH DEVICE OUT FUNCTION
;
;
;COMMON OUTPUT CHARACTER I/O PROCESSING ENTRY
;
OUTCM:
	STA	OUTOP		;SAVE BDOS FUNCTION
;
;
;FETCH THE FILE NAME TO SEND OUT
;
	LHLD	LISTPOS		;MOVE NAME FROM LIST TO SOURCE FCB
	LXI	D,12
	CALL	SDEHL		;POINT TO NAME POSITION
	LXI	D,SORCFCB	;PLACE TO MOVE THE NAME
	MVI	B,12		;AMOUNT TO MOVE
	CALL	MOVBYT
;
	LXI	D,BUFR		;SET TO USE DEFAULT DMA BUFFER
	MVI	C,STDMA		;ADDRESS SET FUNCTION
	CALL	BDOS
;
	MVI	B,4		;FILL EX,S1,S2,RC WITH ZEROS
	LXI	H,SORCFCB+12	;SET POINTER TO SOURCE EXTENT FIELD
OUTCM1:
	MVI	M,0
	INX	H
	DCR	B
	JNZ	OUTCM1
;
	LXI	D,SORCFCB	;OPEN THE FILE FOR READING
	MVI	C,OPEN		;FILE OPEN FUNCTION CODE
	CALL	BDOS
	CPI	0FFH
	JNZ	OUTCM2		;SKIP ERROR MESSAGE IF O.K.
	CALL	PRTMSG
	DB	' ++ File Cannot Be Opened ++',0
	JMP	NEXTPOS
;
OUTCM2:
	XRA	A		;ZERO THE CURRENT RECORD FIELD
	STA	SORCFCB+32
;
OUTCM3:
	LXI	D,SORCFCB	;POINT AT FILE FCB FOR READING
	MVI	C,READR		;FUNCTION SET FOR RECORD READ
	CALL	BDOS
	ORA	A		;CHECK IF READ WAS O.K.
	JZ	OUTCM4		;READ O.K. SO GO TO SEND OUT
	JMP	NEXTPOS		;EOF SO TO NEXT LIST NAME POSITION
;
OUTCM4:
	LXI	H,BUFR		;POINT AT SECTOR JUST READ
	MVI	B,080H		;SET SECTOR CHARACTER COUNTER TO OUTPUT
OUTCM5:
	MOV	A,M		;GET A CHARACTER
	CPI	01AH		;SEE IF LOGICAL END OF FILE
	JZ	NEXTPOS		;BACK TO LIST LOOP IF LOG EOF
	MOV	E,A		;POSITION CHARACTER FOR BDOS
	PUSH	B
	PUSH	H
	LDA	OUTOP		;GET DEVICE CODE FOR OUTPUT
	MOV	C,A
	CALL	BDOS		;SEND THE CHARACTER
;
	MVI	C,GETST		;GET CONSOLE STATUS FUNCTION
	CALL	BDOS		;GO GET THE ABORT STATUS
	POP	H
	POP	B
;
	ORA	A		;IF CHAR THERE, THEN ABORT
	JNZ	BACKUP 		;  TO SAME LIST POSITION
;
	INX	H		;INC BUFFER POINTER
	DCR	B		;ALL BYTES OF SECTOR SENT YET?
	JNZ	OUTCM5		;MORE TO DO
;
	JMP	OUTCM3		;GO GET THE NEXT SECTOR
;
;
;ROUTINE TO COPY A DISK FILE NAMED BY THE CURRENT LIST POSITON TO
;A DIFFERENT DISK DRIVE OF OPERATOR SELECTION.
;
COPY:
	LHLD	LISTPOS		;MOVE NAME FROM LIST TO SOURCE FCB
	LXI	D,12
	CALL	SDEHL		;POINT TO NAME POSITION
	LXI	D,SORCFCB	;PLACE TO MOVE THE NAME
	MVI	B,12		;AMOUNT TO MOVE
	CALL	MOVBYT
;
	MVI	B,4		;FILL EX,S1,S2,RC WITH ZEROS
	LXI	H,SORCFCB+12	;SET POINTER TO SOURCE EXTENT FIELD
COPY1:
	MVI	M,0		;PUT ZERO IN
	INX	H
	DCR	B
	JNZ	COPY1
;
	XRA	A		;ZERO THE CURRENT RECORD FIELD
	STA	SORCFCB+32
;
	MVI	B,33		;COPY SOURCE FCB TO DESTINATION FCB
	LXI	H,SORCFCB	;FROM COPY POINT
	LXI	D,DESTFCB	;TO COPY POINT
	CALL	MOVBYT		;MOVE IT ACROSS
;
	LXI	D,SORCFCB	;OPEN THE FILE FOR READING
	MVI	C,OPEN		;FILE OPEN FUNCTION CODE
	CALL	BDOS
	CPI	0FFH
	JNZ	COPY2		;SKIP ERROR MESSAGE IF O.K.
	CALL	PRTMSG
	DB	CR,LF,' ++ Source File Cannot Be Opened ++',0
	JMP	NEXTPOS
;
COPY2:
	CALL	PRTMSG		;PROMPT FOR DRIVE SELECT NAME
	DB	'  Destination Drive ? ',0
	MVI	C,KEYIN		;GET SELECTION
	CALL	BDOS
	CALL	UPCASE		;CONVERT TO UPPER CASE
	SUI	'A'		;ZERO BASE FOR RANGE CHECK
	CPI	'P'-'A'+1
	JNC	CMDERR		;BACK TO COMMAND LOOP WITH "?"
;
	INR	A		;BASE DRIVE SELECT FOR 1=A:
	STA	DESTFCB		;SET INTO THE DESTINATION FCB
;
	MOV	B,A		;SEE IF HE PICKED SAME DISK AS CURRENT
	LDA	SORCFCB		;GET SOURCE DRIVE ID
	CMP	B
	JNZ	COPY2A		;NO THEY ARE DIFFERENT?
	CALL	PRTMSG
	DB	CR,LF,' ++ Cannot Select Same Disk as Source ++',CR,LF,0
	JMP	SAMEPOS		;LET HIM TRY AGAIN
;
COPY2A:
	LXI	D,DESTFCB	;SEARCH TO SEE IF THIS FILE EXISTED
	MVI	C,SRCHF		;SEARCH FIRST FUNCTION
	CALL	BDOS
	CPI	0FFH		;RETURN CODE IF NOT FOUND
	JZ	COPY3		;GO TO MAKE FUNCTION FOR NEW FILE
;
	CALL	PRTMSG		;ASK IF TO REPLACE
	DB	'  Replace ? ',0
	MVI	C,KEYIN		;GET HIS ANSWER
	CALL	BDOS
	CALL	UPCASE
	CPI	'Y'		;IF YES THEN DELETE IT AND COPY OVER
	JNZ	NEXTPOS		;IF COPY NOT WANTED THEN ON TO NEXT
;
	LXI	D,DESTFCB	;DELETE FILE THAT ALREADY EXISTS
	MVI	C,DELETE	;ERASE FILE FUNCTION CODE
	CALL	BDOS
;
COPY3:
	LXI	D,DESTFCB	;MAKE NEW FILE AND OPEN FOR WRITING
	MVI	C,MAKE		;MAKE FUNCTION CODE
	CALL	BDOS		;GO MAKE IT
	CPI	0FFH		;CHECK IF THE DIRECTORY WAS FULL
	JNZ	COPY4
	CALL	PRTMSG
	DB	CR,LF,' ++ Destination Directory Full ++',0
	JMP	NEXTPOS		;BACK TO LIST PROCESSOR IF ERROR
;
COPY4:
	LXI	B,0		;FIGURE OUT HOW MANY SECTORS
	LHLD	BDOS+1		;FIT INTO MEMORY BUFFER
	DCX	H
	XCHG			;(DE) = MAX ADDRESS ALLOWABLE
	LHLD	BUFSTART	;START ADDRESS OF BUFFER
COPY5:
	INX	B		;INCREASE SECTOR COUNT BY ONE
	PUSH	D
	LXI	D,080H		;ONE SECTOR SIZE
	DAD	D		;BUFFER ADDRESS + SEC SIZE
	POP	D
	CALL	CDEHL		;COMPARE VALUES TO SEE IF ALL DONE
	JNC	COPY5		;MORE WILL FIT?
;
	DCX	B		;SET MAX SECTOR COUNT LESS BY ONE
	MOV	A,B		;CHECK IF ANY MEMORY FOR COPY AT ALL
	ORA	C
	JNZ	COPY6		;THERE IS MEMORY FOR BUFFER
	CALL	PRTMSG
	DB	CR,LF,' ++ No Memory Available for Copy Buffer ++',0
	JMP	NEXTPOS
;
COPY6:
	MOV	L,C
	MOV	H,B		;PUT MAX SECTOR COUNT IN STORAGE
	SHLD	BUFMAX
	XRA	A		;CLEAR EOF FLAG
	STA	EOFLG
;
COPY6A:
	LXI	H,0		;SET CURRENT BUFFER COUNTER TO ZERO
	SHLD	BUFCNT
	LHLD	BUFSTART	;SET BUFFER START POINTER TO BEGIN
	SHLD	BUFPNT
;
;
;FILE SOURCE READING LOOP TO READ ALL MEMORY FULL OR STOP ON EOF
;
COPY7:
	LHLD	BUFPNT		;SET DMA ADDRESS TO BUFFER POINTER
	XCHG
	MVI	C,STDMA
	CALL	BDOS
;
	LXI	D,SORCFCB	;POINT AT FILE FCB FOR READING
	MVI	C,READR		;FUNCTION SET FOR RECORD READ
	CALL	BDOS
	ORA	A		;CHECK IF READ WAS O.K. OF EOF
	JNZ	COPY8		;END OF FILE SO SET EOF FLAG
;
	LHLD	BUFPNT		;SET BUFFER POINTER UP ONE SECTOR
	LXI	D,080H
	DAD	D
	SHLD	BUFPNT
	LHLD	BUFCNT		;INCREASE BUFFER SECTOR COUNT
	INX	H
	SHLD	BUFCNT
	XCHG			;CHECK TO SEE IF MEMORY IS FULL
	LHLD	BUFMAX		;MAXIMUM SECTOR COUNT
	CALL	CDEHL		;COMPARE
	JNZ	COPY7		;IF NOT FULL GO GET NEXT SECTOR
	JMP	COPY9		;GO HANDLE WRITE OPERATION
;
;
;HERE IF READ OPERATION INDICATES THAT THE FILE IS AT ITS END ON READ
;
COPY8:
	MVI	A,0FFH		;SET EOF FLAG
	STA	EOFLG
;
;
;WRITE OUTPUT FILE PROCESSING LOOP TO SEND MEMORY BUFFER TO
;DESTINATION DISK FILE
;
COPY9:
	LHLD	BUFSTART	;SET BUFFER POINTER TO START
	SHLD	BUFPNT
;
COPY10:
	LHLD	BUFCNT		;SEE IF BUFFER IS EMPTY YET
	MOV	A,H
	ORA	L
	JZ	COPY11		;BUFFER EMPTY SO CHECK EOF FLAG
	DCX	H		;DEC BUFFER SECTOR COUNT FOR EACH WRITE
	SHLD	BUFCNT
;
	LHLD	BUFPNT		;SET UP DMA ADDRESS
	PUSH	H		;SAVE FOR SIZE BUMP
	XCHG
	MVI	C,STDMA
	CALL	BDOS
	POP	H
	LXI	D,080H		;INCREASE ADDRESS FOR SECTOR SIZE
	DAD	D
	SHLD	BUFPNT
;
	LXI	D,DESTFCB	;POINT TO OUTPUT FILE FCB
	MVI	C,WRITER	;WRITE RECORD FUNCTION CODE
	CALL	BDOS		;GO WRITE
	ORA	A   		;CHECK IF ANY ERROR-CAF
	JZ	COPY10		;O.K. SO DO NEXT RECORD
;
	CALL	PRTMSG		;TELL OF DISK WRITE ERROR
	DB	CR,LF,' ++ Disk or Directory Full or error on Write ++',0 ;ts
	JMP	NEXTPOS		;BACK TO LIST PROCESSING
;
COPY11:
	LDA	EOFLG		;BUFFER ALL WRITTEN SO GO CHECK EOF
	ORA	A
	JZ	COPY6A		;GO TO READ NEXT BUFFER FULL
;
	LXI	D,DESTFCB	;POINT AT FCB FOR FILE CLOSE
	MVI	C,CLOSE		;CLOSE FILE FUNCTION CODE
	CALL	BDOS
	CPI	0FFH		;CHECK IF CLOSE ERROR
	JNZ	NEXTPOS
	CALL	PRTMSG
	DB	CR,LF,' ++ Destination Close Error ++',0
	JMP	NEXTPOS
;
;
;ROUTINE TO INQUIRE ABOUT SELECTING OPERATION ON ANOTHER DRIVE
;
LOG:
	CALL	PRTMSG		;PROMPT FOR DRIVE SELECT NAME
	DB	'  New Drive ? ',0
	MVI	C,KEYIN		;GET SELECTION
	CALL	BDOS
	CALL	UPCASE		;CONVERT TO UPPER CASE
	CPI	CR		;IF CARRIAGE RETURN SKIP RE-LOG
	JZ	NEXTPOS
;
	SUI	'A'		;ZERO BASE FOR RANGE CHECK
	CPI	'P'-'A'+1
	JNC	CMDERR		;BACK TO COMMAND LOOP WITH "?"
;
	INR	A		;BASE DRIVE SELECT FOR 1=A:
	STA	FCB		;SET INTO THE START DEFAULT FCB
	MVI	A,' '		;SET DEFAULT FCB TO LOOK LIKE *.*
	STA	FCB+1
	STA	FCB+9
	JMP	RESTART		;GO RESTART WITH MENU PRINT OVER
;
;
;
;***************************************************************************
;
;	UTILITY SUBROUTINES
;
;COMPARE DOUBLE REGISTERS (DE) TO (HL) SIMILAR TO CMP B INSTRUCTION AND SET
;FLAGS ACCORDINGLY.
;
CDEHL:
	MOV	A,D		;SEE IF HIGH BYTES SET FLAGS
	CMP	H
	RNZ			;RETURN IF NOT EQUAL
	MOV	A,E
	CMP	L		;LOW BYTES SET THE FLAGS INSTEAD
	RET
;
;
;SUBTRACT DOUBLE REGISTERS (DE) FROM (HL) LEAVING RESULT IN (HL)
;
SDEHL:
	MOV	A,L		;DO LOW BYTES FIRST
	SUB	E
	MOV	L,A		;SET LOW BYTE OF RESULT BACK
	MOV	A,H		;HIGH BYTES NOW
	SBB	D		;SUBTRACT HIGH AND BORROW
	MOV	H,A		;HIGH BYTE OF RESULT BCK TO (H)
	RET
;
;
;MOVE SUBROUTINE  MOVE (B) BYTES FROM (HL) TO (DE)
;
MOVBYT:
	MOV	A,M		;GET (HL) REFERENCED SOURCE BYTE
	ANI	7FH		;STRIP CP/M 2.x ATTRIBUTES
	STAX	D		;PUT TO (DE) REFERENCED DESTINATION
	INX	H		;FIX POINTERS FOR NEXT SEARCH
	INX	D
	DCR	B		;DEC BYTE COUNT AND SEE IF DONE
	JNZ	MOVBYT
	RET
;
;
;INLINE PRINT OF MESSAGE TILL A ZERO
;
PRTMSG:
	XTHL			;SAVE HL, GET MSG POINTER
;
PRTMLP:
	MOV	A,M		;GET CHARACTER
	ANI	07FH		;STRIP USER TYPE BITS
	MOV	E,A
	INX	H		;INCREMENT POINTER TO NEXT CHAR
				;..OR RETURN ADDRESS
	JZ	PMXIT		;EXIT IF ZERO
;
	PUSH	H		;SAVE MESSAGE POINTER
	MVI	C,WRCON		;CHARACTER OUTPUT FUNCTION CODE
	CALL	BDOS		;OUTPUT IT
	POP	H
	JMP	PRTMLP		;GO CHECK/DO NEXT CHAR
;
PMXIT:
	XTHL			;RESTORE HL, RET ADDR
	RET			;RET PAST MSG
;
;
;CONVERT CHARACTER IN (A) TO UPPER CASE
;
UPCASE:
	ANI	07FH		;STRIP UPPER BIT
	CPI	060H		;IS CHAR ONE REQUIRING CONVERT
	RC			;NO CONVERT NEEDED
	ANI	05FH		;CONVERT
	RET
;
;
;CHECK FOR LEGAL FILE NAME CHARACTER
;
CHKLEGL:
	LDAX	D		;GET CHARACTER FROM .(DE)
	INX	D		;TO POINT AT NEXT CHARACTER
	CPI	' '		;IS IT LESS THAN SPACE
	RC			;RETURN CARRY IF ERROR CHAR
	CPI	05FH		;IF GREATER THAN UNDERSCORE ERROR
	JNC	CHKERR		;CARRY SET EXIT
	CPI	'='
	JZ	CHKERR
	CPI	':'
	JZ	CHKERR
	CPI	';'
	JZ	CHKERR
	CPI	'<'
	JZ	CHKERR
	CPI	'>'
	JZ	CHKERR
	ORA	A		;CLEAR CARRY FOR GOOD CHAR
	RET
;
CHKERR:
	STC			;ERROR EXIT
	RET
;
;
;ROUTINE TO COMPARE TWO STRINGS LEFT TO RIGHT SIMILAR TO CMP B INSTRUCTION
;
;	(B)=STRING LENGTH
;	(DE)=STRING A BASE POINTER
;	(HL)=STRING B BASE POINTER
;
CMPSTR:
	LDAX	D		;GET AN A STRING CHARACTER
	CMP	M		;CHECK AGAINST B STRING CHARACTER
	RNZ			;NOT EQUAL SETS RETURN FLAGS
	DCR	B		;DECREMENT COMPARE BYTE COUNT
	RZ			;SET LIKE EQUAL IF ALL COMPARED EQUAL
	INX	H		;BUMP COMPARE POINTERS
	INX	D
	JMP	CMPSTR		;TO DO NEXT CHARACTER
;
;
;ROUTINE TO INTERCHANGE TWO STRINGS FOR SORTING
;
;	(B)=STRING LENGTH
;	(DE)=STRING A BASE POINTER
;	(HL)=STRING B BASE POINTER
;
SWAP:
	MOV	C,M		;GET CHAR FROM ONE STRING
	LDAX	D		;AND ONE FROM OTHER STRING
	MOV	M,A		;SECOND INTO FIRST
	MOV	A,C		;FIRST INTO SECOND
	STAX	D
	INX	H		;BUMP SWAP POINTERS
	INX	D
	DCR	B		;SEE IF ALL BYTES SWAPPED YET
	JNZ	SWAP
	RET
;
;
;*************************************************************************
;
;
;
	DS	080H		;DEFINE STORAGE FOR AN EXECUTION STACK
STCKK:
	DS	2
;
SORCFCB:
	DS	33		;FCB FOR SOURCE FILE
;
DESTFCB:
	DS	33		;FCB FOR DESTINATION FILE IF COPY
				;AND NEW NAME IF RENAME FUNCTION
;
MAXLEN	EQU	25		;MAXIMUM BUFFER INPUT LENGTH
;
CBUFF:
	DB	MAXLEN		;COMMAND BUFFER LENGTH
CBUFFL:
	DB	0		;CURRENT INPUT LENGTH
CSTRING:
	DS	MAXLEN+3	;STORAGE FOR INPUT
;
LISTPOS:
	DS	2		;CURRENT LIST POSITION IN SCAN
;
LISTI:
	DS	2		;LIST SORT POINTER
;
LISTJ:
	DS	2		;ANOTHER LIST SORT POINTER
;
LISTEND:
	DS	2		;CURRENT LIST END POINTER
;
BUFSTART:
	DS	2		;START POINTER FOR COPY BUFFER
;
BUFPNT:
	DS	2		;CURRENT POINTER FOR COPY BUFFER
;
BUFMAX:
	DS	2		;MAXIMUM NUMBER OF BUFFER SECTORS
;
BUFCNT:
	DS	2		;NUMBER OF SECTORS CURRENTLY IN BUFFER
;
OUTOP:
	DS	1		;SIMPLE FILE OUTPUT DESTINATION FUNCTION
				;EQUAL TO BDOS FLAG
;
EOFLG:
	DS	1		;FILE COPY LOOP EOF FLAG
;
LIST:
	DS	1		;BASE OF FILE NAME LIST

;
;
;
	END
;
;
;+++...END OF FILE
