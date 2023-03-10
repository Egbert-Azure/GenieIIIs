# Creating a "Virtual Genie IIIs with CP/M 3.0 #

> Note: First Draft

The old DISKIO.MAC, modified for 21 MB, is in the DMK file, HD2.MAC is not needed (those were your adaptations for the OMTI controller). To generate a new CP/M 3, there are BOOTGEN.SUB and CPM3.SUB. A system disk can be created using KOPIER.COM in Holte-CP/M. The (here link) DMK image is specified as the "boot drive":

``` consol
sdltrs -disk0 g3s-holte-21.dmk
```

Then create a "Hard Disk" image (Alt-H) with the following parameters in `sdltrs`:

![image](https://user-images.githubusercontent.com/55332675/219958640-1cf13e8e-c9d4-4292-b11b-0bffc0cffedc.png)

> Note: muss noch geschrieben werden: Hier ist die "Boot-Disk" (Holte CP/M 3b, 4 * Festplatten mit je 21,6 MB, 60K TPA und 55K RAM-Disk) ... INITDIR und set [update=on, create=on] wurden aufgerufen ... in GENCPM.DAT gibt es bestimmt noch "Optimierungsbedarf" ...

### DISCIO.MAC

DS1 is the second HD, DBP03 defined with 12.6 MB and `drive type` is 2 for `Winchester`

``` as

        DEFW M$WRITE
        DEFW M$READ
        DEFW FD$LOGIN
        DEFW M$INIT0
        DEFB 0,2
DS1:    DEFW 0                  ;no translation table
        DEFW 0,0,0,0            ;BDOS scratch area
        DEFB 0,0                ;media flag
        DEFW DPB03              ;disk parameter block
        DEFW 0                  ;no CSV
        DEFW 0FFFEH,0FFFEH      ;ALV, DIRBCB, DTABCB
        DEFW 0FFFEH             ;alloc'd by GENCPM
        DEFW 0FFFFH             ;no HASH
        DEFB 0                  ;hash bank
```

D I S K I O.MAC is  *  C P M S Y S 4 f  

> Note: wip - waiting for right MAC file

``` as
;******************************************************************************
;*  D I S K I O  *  C P M S Y S 4 f  *	T h o m a s   H o l t e * 8 5 0 9 2 5 *
;******************************************************************************
;*									      *
;*			    D I S K   H A N D L E R			      *
;*			    =======================			      *
;*									      *
;*									      *
;*  Thomas Holte						 Version 1.0  *
;*									      *
;******************************************************************************

;------------------------------------------------------------------------------
; Comment:
; Version D0 
; 21,4 MB Interface ST412 MFM-Format betrieben mit OMTI 5527 
;
; RAM-Disk auf 55K eingestellt und in BANK 3 verschoben
; Schafft Platz fuer CP/M und ev. spaetere Festplatten-Partitionen
; Code dazu   :  LD  A,3  ; Destination Bank 3
; und in TASKM:  ADD A,3  ; hier wird Bank errechnet !
;
;-----------------------------------------------------------------------------

	.Z80

	TITLE 'CP/M 3 DISKETTE HANDLER'

	DSEG

;Disk drive dispatching tables for linked BIOS
;GLOBAL DS0,MF0,MF1,FD0,IBMPC,KDS,RAM,RAIR,ALPHAP3,DRIVEP

	GLOBAL	MF0,MF1,DS0,RAM,DRIVEP

;Variables containing parameters passed by BDOS
	EXTERNAL @DTBL,@ADRV,@RDRV,@DMA,@TRK,@SECT,@DBNK,@CBNK

;System control block variables
	EXTERNAL @ERDME		; BDOS error mode

	EXTERNAL ?PMSG		; Print message ^HL up to 0, saves reg. BC & DE
	EXTERNAL ?PDERR		; Print BIOS disk error header
	EXTERNAL ?CONIN,?CONO	; Con in and out
	EXTERNAL ?CONST		; Get console status

	EXTERNAL ?BANK,?USERF
;Harddiskroutinen aus HD2.MAC

        external hdwrit,hdread,hdlogi,hdinit

 
;ASCII control codes:
SUB	EQU	1AH		; Substitute
ESC	EQU	1BH		; Escape



;**Extended Disk Parameter Headers (XDPHs)


;Double Density
	DEFW	FD$WRITE
	DEFW	FD$READ
	DEFW	FD$LOGIN
	DEFW	FD$INIT
	DEFB	0		; Relative drive zero
	DEFB	0		; Drive type
				; 0 = floppy disk
				; 1 = floppy disk (special format)
				; 2 = Winchester
				; 3 = Winchester (cartridge)
				; 4 = RAM disk
MF0:	DEFW	0		; No translation table
	DEFW	0,0,0,0		; BDOS scratch area
	DEFB	0,0		; Media flag
	DEFW	DPB01		; Disk parameter block
	DEFW	0FFFEH,0FFFEH	; CSV, ALV, DIRBCB, DTABCB, HASH
	DEFW	0FFFEH,0FFFEH	; Alloc'd by GENCPM
	DEFW	0FFFEH
	DEFB	0		; Hash bank

	DEFW	FD$WRITE
	DEFW	FD$READ
	DEFW	FD$LOGIN
	DEFW	FD$INIT
	DEFB	1,0		; Relative drive one
MF1:	DEFW	0		; No translation table
	DEFW	0,0,0,0		; BDOS scratch area
	DEFB	0,0		; Media flag
	DEFW	DPB01		; Disk parameter block
	DEFW	0FFFEH,0FFFEH	; CSV, ALV, DIRBCB, DTABCB, HASH
	DEFW	0FFFEH,0FFFEH	; Alloc'd by GENCPM
	DEFW	0FFFEH
	DEFB	0		; Hash bank

;EJECT
;RAM disk:
;--------:
	DEFW	M$WRITE
	DEFW	M$READ
	DEFW	FD$LOGIN
	DEFW	M$INIT0
	DEFB	0,4
RAM:	DEFW	0		; No translation table
	DEFW	0,0,0,0		; BDOS scratch area
	DEFB	0,0		; Media flag
	DEFW	DPBC		; Disk parameter block
	DEFW	0		; No CSV
	DEFW	0FFFEH,0FFFEH	; ALV, DIRBCB alloc'd by GENCPM
	DEFW	0FFFFH		; No data buffer
	DEFW	0FFFEH		; HASH alloc'd by GENCPM
	DEFB	0		; Hash bank

;
;XDPH fuer Harddisk 21.40 MB


	DEFW	hdwrit
	DEFW	hdread
	DEFW	FD$LOGIN
	DEFW	hdinit
	DEFB	0		;relative Drive zero
	DEFB	2		;drive type
				; 0 : floppy disk
				; 1 : floppy disk (special format)
				; 2 : Winchester
				; 3 : Winchester (cartridge)
				; 4 : Ramdisk
DS0:	DEFW	0		;no translation table
	DEFW	0,0,0,0		;BDOS scratch area
	DEFB	0,0		;media flag
	DEFW	DPBHD0		;disk parameter block
	DEFW	0		;no CSV
	DEFW	0FFFEH,0FFFEH	;ALV, DIRBCB, DTABCB, HASH
	DEFW	0FFFEH,0FFFEH	;allocated by GENCPM
	DEFB	0		;hash bank



;----------------------------------------------------------------
; Formatdefinitionen:
; Fuer Sonderformate wird Drive B als relatives laufwerk genutzt.
;----------------------------------------------------------------

;EJECT
;virtual disk drive P:
;--------------------:
	DEFW	P$WRITE
	DEFW	P$READ
	DEFW	FD$LOGIN
	DEFW	FD$INIT
	DEFB	1,1		; Relative drive one
DRIVEP:	DEFW	XLTF       	; Translation table
	DEFW	0,0,0,0		; BDOS scratch area
	DEFB	0,0		; Media flag
	DEFW	DPBF		; Disk parameter block
	DEFW	CSVF,ALVF	; Checksumm vector, allocation vector
	DEFW	DIBCBH,DTBCBH	; BCB list header
	DEFW	0FFFFH		; No HASH
	DEFB	0		; Hash bank
PDCT:	DEFB	01110000B
	DEFB	10000000B
	DEFB	2,10,80

;sector translation tables:

XLTF:	DEFB	0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
	DEFB	17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33
	DEFB	34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51


;checksum vectors
CSVF:   DEFS    64

  
;allocation vectors:
ALVF:	DEFS	100

;BCB list header:
DIBCBH:	DEFW	DIRBCB
DTBCBH:	DEFW	DTABCB

;directory control block:
DIRBCB:	DEFB	0FFH
	DEFS	9
	DEFW	DIRBUF
	DEFB	0
	DEFW	0

;data control block:
DTABCB:	DEFB	0FFH
	DEFS	9
	DEFW	DTABUF
	DEFB	0
	DEFW	0

;directory buffer:
DIRBUF:	DEFS	1024

;data buffer:
DTABUF:	DEFS	1024


;EJECT
	CSEG			; DPB must be resident


;Disk Parameter Blocks (DPB)

;Double Density Holte:
;--------------------:
DPB01:	DEFW	80		; 128 byte records per track
	DEFB	4,15		; Block shift and mask
	DEFB	0		; Extent mask
	DEFW	389		; Maximum block number
	DEFW	191		; Maximum directory entry number
	DEFB	0E0H,0		; Alloc vector for directory
	DEFW	48		; Checksum size
	DEFW	2		; Offset for system tracks
	DEFB	2,3		; Physical sector size shift and mask




;RAMDISK:
;-------:
DPBC:	DEFW	447		; 128 byte records per track
	DEFB	3,7		; Block shift and mask
	DEFB	0		; Extent mask
	DEFW	55		; Maximum block number = 55 Kbyte
	DEFW	31		; Maximum directory entry number
	DEFB	80H,0		; Alloc vector for directory
	DEFW	8000H		; Checksum size
	DEFW	0		; Offset for system tracks
	DEFB	0,0		; Physical sector size shift and mask

;VIRTUELLES DRIVE -P-:
;--------------------:
DPBF:	DEFW	80		; 128 byte records per track
	DEFB	5,31		; Block shift and mask
	DEFB	3		; Extent mask
	DEFW	191		; Maximum block number
	DEFW	255		; Maximum directory entry number
	DEFB	192,0		; Alloc vector for directory
	DEFW	64		; Checksumm size
	DEFW	3		; Offset for system tracks
	DEFB	3,7		; Physical sector size shift and mask

;:------------------------:
;:HARDISK 20.4 MByte      :
;:------------------------:
;: hier mit 2 Systemspuren:
;:------------------------:

DPBHD0:	DEFB  	010H,001H   ;SPT= 272      128 Byte records/track
	DEFB	005H        ;BSH=   5      block shift factor  
        DEFB    01FH        ;BLM=  31      block mask   
	DEFB	001H        ;EXM=   1      extend mask  
        DEFB    059H,014H   ;DSM=5226      maximum block number 
	DEFB	0FFH,007H   ;DRM=2047      maximum directory number
	DEFB	0FFH        ;AL0= 255      allocation vector 0
        DEFB    0FFH        ;AL1= 255      allocation vector 1
	DEFB	000H,080H   ;CKS=   0      checksum size is zero
                            ;              ,permanently mounted
	DEFB	002H,000H   ;OFF=   0      reserved tracks 
 	DEFB	002H        ;PSH=   2      physical sector shift
	DEFB	003H        ;PHM=   3      physical record mask 

;das heisst auf Deutsch:
;			512 Bytes/Sector
;			 68 Sectoren/Zylinder (4 K|pfe*17 Sec/Cyl)
;			615 Cylinder/Laufwerk
;			4KB Blockgr|~e
;		       2048 Directory Eintr{ge
;			  2 Reservierte Systemspur
;		      8000h Flag f}r nonremovable medium 			




	

          DSEG

;EJECT
;RAM disk init routine:
;---------------------:
POWUP	EQU	111DH		; Power up/reset marker

M$INIT0:LD	A,(POWUP)	; Power up ?
	OR	A
	RET	Z

;clear RAM disk:
	LD	C,32		; Entry count		 --> reg. C
	LD	DE,128		; ^first directory entry --> reg. DE
CLRNXT:	PUSH	BC		; Save remaining entry count
	PUSH	DE		; Save ^directory entry
	LD	HL,CLRBYT	; ^E5		   --> reg. HL
	LD	BC,1 SHL 8+15	; Byte count	   --> reg. B
				; Function #	   --> reg. C
	LD	A,3		; Source      bank # = 0
				; Destination bank # = 3
	CALL	?USERF		; Clear current entry
	POP	HL		; ^current directory entry --> reg. HL
	LD	DE,32		; Offset to next entry	   --> reg. DE
	ADD	HL,DE		; Add offset
	EX	DE,HL		; ^next directory entry    --> reg. DE
	POP	BC		; Restore entry count
	DEC	C		; Decrement it
	JR	NZ,CLRNXT	; Clear next entry
	RET
CLRBYT:	DEFB	0E5H
          


;EJECT

          DSEG


;EJECT
;disk read routine:
FD$READ:LD	A,(@SECT)	; Sector number 	--> reg. B
	LD	B,A
	LD	A,(@TRK)	; Track number		--> reg. E
	LD	E,A
FD$REA1:LD	A,(@RDRV)	; Relative drive number --> reg. C
	LD	C,A
	LD	A,(@DBNK)	; DMA bank --> accu (upper nibble)
	SLA	A
	SLA	A
	SLA	A
	SLA	A
	OR	C		; Set drive number
	LD	HL,(@DMA)	; User buffer address	--> reg. HL
	LD	C,11		; Function number	--> reg. C
	CALL	?USERF		; Read physical disk sector
	LD	(DISK$STATUS),A	; Save status for error messages
	OR	A		; Any errors ?
	JP	NZ,RDERR
	RET

;suppress error message if BDOS is returning errors to application ...
RDERR:	LD	A,(@ERDME)
	INC	A
	JR	Z,R$HARD$ERROR

;had permanent error, print message like:
;	BIOS Error on d: T-nn, S-nn, <type>, Retry ?
	CALL	?PDERR		; Print message header
	LD	HL,(DISK$STATUS) ; Get status byte from last error
	LD	H,0
	DEC	L
	ADD	HL,HL		; Make byte offset
	LD	BC,R$ERROR$TABLE ; Point at table of message addresses
	ADD	HL,BC
	LD	A,(HL)		; Get specific message address
	INC	HL
	LD	H,(HL)
	LD	L,A
	CALL	?PMSG		; Print message
	LD	HL,ERROR$MSG	; Print "<BEL>, Retry (Y/N) ? "
	CALL	?PMSG
	CALL	U$CONIN$ECHO	; Get operator response
	LD	C,A		; Save response
	LD	HL,MSG$END	; Disable status line
	CALL	?PMSG
	LD	A,C		; Restore response
	CP	'Y'		; Yes, then retry 10 more times
	JP	Z,FD$READ
R$HARD$ERROR:			; Otherwise,
	LD	A,1		; Return hard error to BDOS
	RET


;EJECT
;disk write routine:
FD$WRITE:
	LD	A,(@SECT)	; Sector number 	--> reg. B
	LD	B,A
	LD	A,(@TRK)	; Track number		--> reg. E
	LD	E,A
FD$WRI1:LD	A,(@RDRV)	; Relative drive number --> reg. C
	LD	C,A
	LD	A,(@DBNK)	; DMA bank --> accu (upper nibble)
	SLA	A
	SLA	A
	SLA	A
	SLA	A
	OR	C		; Set drive number
	LD	HL,(@DMA)	; User buffer address	--> reg. HL
	LD	C,12		; Function number	--> reg. C
	CALL	?USERF		; Write physical disk sector
	LD	(DISK$STATUS),A	; Save status for error messages
	OR	A		; Any errors ?
	JP	NZ,WRERR
	RET

;suppress error message if BDOS is returning errors to application ...
WRERR:	LD	A,(@ERDME)
	INC	A
	JR	Z,W$HARD$ERROR

;had permanent error, print message like:
;	BIOS Error on d: T-nn, S-nn, <type>, Retry ?
	CALL	?PDERR		; Print message header
	LD	HL,(DISK$STATUS) ; Get status byte from last error
	LD	H,0
	DEC	L
	ADD	HL,HL		; Make byte offset
	LD	BC,W$ERROR$TABLE ; Point at table of message addresses
	ADD	HL,BC
	LD	A,(HL)		; Get specific message address
	INC	HL
	LD	H,(HL)
	LD	L,A
	CALL	?PMSG		; Print message
	LD	HL,ERROR$MSG	; Print "<BEL>, Retry (Y/N) ? "
	CALL	?PMSG
	CALL	U$CONIN$ECHO	; Get operator response
	LD	C,A		; Save response
	LD	HL,MSG$END	; Disable status line
	CALL	?PMSG
	LD	A,C		; Restore response
	CP	'Y'		; Yes, then retry 10 more times
	JP	Z,FD$WRITE
W$HARD$ERROR:			; Otherwise,
	LD	A,(DISK$STATUS)	; Return hard error to BDOS
	CP	5		; Diskette write protected ?
	LD	A,1		; Common error code
	RET	NZ
	INC	A

FD$LOGIN:
FD$INIT:
	RET

U$CONIN$ECHO:			; Get console input, echo it, and shift to
				; Upper case
	CALL	?CONST		; See if any character already struck
	OR	A
	JR	Z,U$C1
	CALL	?CONIN		; Yes, eat and try again
	JR	U$CONIN$ECHO
U$C1:	CALL	?CONIN
	PUSH	AF
	LD	C,A
	CALL	?CONO
	POP	AF
	CP	'A'
	RET	C
	AND	0DFH		; Make upper case
	RET

DISK$STATUS:
	DEFS	1		; Last error status code for messages
;EJECT




;EJECT
;RAM disk I/O routines
M$READ:	LD	A,0FFH		; Switch on read flag
	JR	TASKM
M$WRITE:XOR	A		; Clear read flag
TASKM:	LD	(RDFLAG),A
	LD	A,(@TRK)	; Track # --> accu
	ADD	A,3		; Calc source bank #
	ADD	A,A
	ADD	A,A
	ADD	A,A
	ADD	A,A
	LD	HL,@DBNK	; Get destination bank #
	ADD	A,(HL)
	PUSH	AF		; Save bank numbers
	LD	DE,(@DMA)	; DMA address --> reg. DE
	LD	HL,(@SECT)	; Sector #    --> reg. HL
	INC	HL		; Adjust it
	LD	B,7		; Sector # * 128
	ADD	HL,HL
	DJNZ	$-1
	LD	BC,128 SHL 8+15	; Sector length --> reg. B
				; Function #	--> reg. C
	LD	A,(RDFLAG)	; Read or write ?
	OR	A
	JR	NZ,TASKM1	; Jump if read
	POP	AF		; Restore bank numbers
	RLCA			; Swap bank numbers
	RLCA
	RLCA
	RLCA
	PUSH	AF		; Push bank numbers again
	EX	DE,HL
TASKM1:	POP	AF		; Restore bank numbers
	CALL	?USERF		; Transfer "sector"
	XOR	A
	RET
RDFLAG:	DEFS	1




;EJECT
;------------------:
;virtual disk drive:

; DriveControllTable mu~ f}r Konfig eine feste Adresse haben.
DCT	EQU	10D7H		; Drive control table (SYSTAB)

P$READ:	LD	IX,FD$READ	; Read	routine vector	   --> reg. IX
	JR	P$TASK
P$WRITE:LD	IX,FD$WRITE	; Write routine vector	   --> reg. IX
P$TASK:	LD	A,(DRIVEP-2)	; Relative drive number    --> accu
	ADD	A,A		; *2
	LD	C,A		; Save it
	ADD	A,A		; *4
	ADD	A,C		; *6
	LD	B,0		; Relative drive # * 6	   --> reg. BC
	LD	C,A
	LD	HL,DCT		; ^drive control table	   --> reg. HL
	ADD	HL,BC		; Calc real pointer
	PUSH	HL		; Save DCT pointer
	LD	DE,TDCT		; ^temporary storage area  --> reg. DE
	LD	BC,5		; Number of bytes	   --> reg. BC
	LDIR			; Save disk parameters of drive B:
	LD	HL,PDCT		; ^parameters for drive P: --> reg. HL
	POP	DE		; DCT pointer		   --> reg. DE
	PUSH	DE		; Save DCT pointer again
	LD	BC,5		; Number of bytes	   --> reg. BC
	LDIR			; Load disk parameters of drive P:
	LD	HL,P$RET	; Return address	   --> reg. HL
	PUSH	HL		; Push it
	JP	(IX)		; Jump to driver
P$RET:	LD	HL,TDCT		; Restore original disk parameters
	POP	DE
	LD	BC,5
	LDIR
	LD	HL,PDCT+1	; Get drive status
	SET	0,(HL)		; Set init bit
	RET
TDCT:	DEFS	5		; Temporary storage area


;EJECT
;tables of pointers to error message strings
R$ERROR$TABLE:
	DEFW	R1MSG
	DEFW	R2MSG
	DEFW	R3MSG
	DEFW	R4MSG
	DEFW	R5MSG
	DEFW	R6MSG
	DEFW	R7MSG
	DEFW	R8MSG
	DEFW	R9MSG

W$ERROR$TABLE:
	DEFW	W1MSG
	DEFW	W2MSG
	DEFW	W3MSG
	DEFW	W4MSG
	DEFW	W5MSG
	DEFW	W6MSG
	DEFW	W7MSG
	DEFW	W8MSG
	DEFW	W9MSG

R1MSG:	DEFM	' Illegal drive #,'
	DEFB	0
R2MSG:	DEFM	' Track # too high,'
	DEFB	0
R3MSG:	DEFM	' Sector # too high,'
	DEFB	0
R4MSG:	DEFM	' Device not available,'
	DEFB	0
R5MSG:	DEFB	0
R6MSG:	DEFM	' Locked/deleted record,'
	DEFB	0
R7MSG:	DEFM	' Data record not found,'
	DEFB	0
R8MSG:	DEFM	' Parity error during read,'
	DEFB	0
R9MSG:	DEFM	' Lost data during read,'
	DEFB	0

W1MSG:	DEFM	' Illegal drive #,'
	DEFB	0
W2MSG:	DEFM	' Track # too high,'
	DEFB	0
W3MSG:	DEFM	' Sector # too high,'
	DEFB	0
W4MSG:	DEFM	' Device not available,'
	DEFB	0
W5MSG:	DEFM	' Write protected diskette,'
	DEFB	0
W6MSG:	DEFM	' Write fault on disk drive,'
	DEFB	0
W7MSG:	DEFM	' Data record not found,'
	DEFB	0
W8MSG:	DEFM	' Parity error during write,'
	DEFB	0
W9MSG:	DEFM	' Lost data during write,'
	DEFB	0

ERROR$MSG:
	DEFM	' Retry (Y/N) ? '
	DEFB	0

MSG$END:DEFB	SUB,ESC,'D',0

	END