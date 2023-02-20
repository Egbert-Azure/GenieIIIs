	psect	text,global,pure
	psect	data,global
	psect	bss,global

	psect	text
	defs	100h		;Base of CP/M's TPA

	global	start,_main,_exit,__Hbss, __Lbss, __argc_, startup
	global	_envptr, __intenv

start:		jp	start1		; Sneak in a ZCPR3 header
		defm	'Z3ENV'		;
		defb	1		;
_envptr:	defw	__intenv	; Pointer to environment
		defw	start		; For type 3/4 compatibility.

start1:		ld	hl,(6)		;base address of fdos
		ld	sp,hl		;stack grows downwards
		ld	de,__Lbss	;Start of BSS segment
		or	a		;clear carry
		ld	hl,__Hbss
		sbc	hl,de		;size of uninitialized data area
		ld	c,l
		ld	b,h
		dec	bc	
		ld	l,e
		ld	h,d
		inc	de
		ld	(hl),0
		ldir			;clear memory (BSS segment)
		ld	hl,(_envptr)	; are we running Z3?
		ld	a,h		;
		or	l		;
		jr	z,noexfcb	; no external fcb then...
		ld	de,24h		; offset to efcb pointer
		add	hl,de		;
		ld	e,(hl)		;
		inc	hl		;
		ld	d,(hl)		; de is now pointing to efcb
		ld	a,e		;
		or	d		; Make sure the efcb is non-NULL
		jr	z,noexfcb	;
		push	de		; Save the efcb for a bit
		ld	hl,14		;
		add	hl,de		;
		ld	a,(hl)		; get drive designator
		add	a,'A'-1		; make it ASCII
		ld	de,nularg	;
		ld	(de),a		;
		inc	de		;
		dec	hl		;
		ld	a,(hl)		;
		cp	10		;
		jr	c,1f		;
		ld	b,-1		;
2:		inc	b		;
		sub	10		;
		jr	nc,2b		;
		push	af		; save units
		ld	a,b		;
		add	a,'0'		;
		ld	(de),a		;
		inc	de		;
		pop	af		;
		add	a,10		; add to make positive... fall through
1:		add	a,'0'		;
		ld	(de),a		;
		inc	de		;
		ex	de,hl		;
		ld	(hl),':'	; shove in a colon
		inc	hl		;
		ld	b,8		; at most 8 characters to move
		pop	de		;
		inc	de		; point at first character of fname
movename:	ld	a,(de)		;
		cp	' '		;
		jr	z,namedone	;
		ld	(hl),a		;
		inc	de		;
		inc	hl		;
		djnz    movename	;
namedone:	ld	(hl),0		; shove in an EOS.
noexfcb:	ld	hl,nularg	; save arg[0] (program name)
		push	hl
		ld	hl,80h		;argument buffer
		ld	c,(hl)
		inc	hl
		ld	b,0
		add	hl,bc
		ld	(hl),0		;zero terminate it
		ld	hl,81h
		push	hl
		call	startup
		pop	bc		;unjunk stack
		pop	bc
		push	hl
		ld	hl,(__argc_)
		push	hl
		call	_main
		push	hl
		call	_exit
		jp	0

__intenv:	
		jp	0		; Dummy jump address
		defm	'Z3ENV'		; Environment ID
		defb	81h

		defw	0		; external path address
		defb	0		; number of 2-byte elements in path

		defw	0		; RCP address
		defb	0		; number of 128-byte blocks in RCP

		defw	0		; IOP address
		defb	0		; number of 128-byte blocks in IOP

		defw	0		; FCP address
		defb	0		; number of 128-byte blocks in FCP

		defw	0		; NDR address
		defb	0		; number of 18-byte entries in NDR

		defw	0		; ZCPR3 Command Line
		defb	0		; number of bytes in Command Line

		defw	__intenv	; ZCPR3 Environment Descriptor
		defb	2		; number of 128-byte blocks in Descriptor

		defw	0		; Shell Stack address
		defb	0		; number of shsize-byte entires in Shell Stack
		defb	0		; size of a Shell Stack entry

		defw	0		; ZCPR3 Message buffer

		defw	0		; ZCPR3 External FCB

		defw	0		; ZCPR3 External Stack

		defb	0		; quiet flag (1=quiet, 0=not quiet)

		defw	0		; address of Wheel Byte

		defb	4		; Processor Speed in MHz

		defb	15		; maximum disk
		defb	15		; maximum user

		defb	1		; 1=OK to accept DU, 0=not OK

		defb	0		; CRT selection (0=CRT 0, 1=CRT 1)
		defb	0		; Printer selection (n=Printer n)

		defb	80		; width of CRT
		defb	24		; number of lines on CRT
		defb	22		; number of lines of text on CRT

		defw	0ffh
		defb	0

		defb	80		; data for printer
		defb	66
		defb	55
		defb	1

		defb	0,0,0,0

		defw	0
		defb	0

		defw	0
		defb	0

		defw	0

		defm	'SH      '	; shell variable filename
		defm	'VAR'		; shell variable filetype

		defm	'        '	; filename 1
		defm	'   '		; filetype 1

		defm	'        '	; filename 2
		defm	'   '		; filetype 2

		defm	'        '	; filename 3
		defm	'   '		; filetype 3

		defm	'        '	; filename 4
		defm	'   '		; filetype 4

;  Fill unused space with nulls
		defs	128 - ($ - __intenv)

;  End of Environment Descriptor -- beginning of TCAP

; ***** USER EDIT *****

; NZTCAP:  NZWYS50.Z80
; Author:  Joe Wright, modified by Jay Sage
; Date:    12 October 87, modified 15 June, 1991
; Version: 1.0

; Extended Termcap Data for NZ

; This is the proposed TermCap Data for the New Z-System.
; It more fully describes the terminal and its capabilities.

ESC	EQU	27		; ASCII escape character

; I have adopted the convention that a terminal name is terminated
; with a space character, therefore no spaces within the name.
; Also that the terminal name is unique in the first eight characters.

NZTCAP:	defm	'WYSE-50D     ' ; Name of terminal (13 bytes)

; The Graphics section is no longer fixed so we must provide an
; offset to it.  One byte is sufficient for a two-record TCAP.

B13:	defb	GOELD-NZTCAP	; Offset to GOELD

; Bit 7 of B14 indicates the new Extended TCAP.  Bits 6-0 are undefined.

B14:	defb	10000000B	; Extended TCAP

; It is often desirable to differentiate terminals with other than
; their commands.  For example TeleVideo 955 has the same command
; set as Wyse 60 but takes less time to initialize (reset).

; 16 bits are now reserved for indicating terminal charactistics
; which cannot be known from the strings.  I have defined five
; of these bits for my own purposes.  

; B15 b0	Standout	0 = Half-Intensity, 1 = Reverse Video
; B15 b1	Power Up Delay	0 = None, 1 = 10-second delay
; B15 b2	No Wrap		0 = Line Wrap, 1 = No Wrap
; B15 b3	No Scroll	0 = Scroll, 1 = No Scroll
; B15 b4	ANSI		0 = ASCII, 1 = ANSI

B15:	defb	00000111B	; Power Up Delay

	defb	'K'-'@'		; Cursor up
	defb	'J'-'@'		; Cursor down
	defb	'L'-'@'		; Cursor right
	defb	'H'-'@'		; Cursor left

	defb	00		; Clear-screen delay
	defb	00		; Cursor movement delay
	defb	00		; Clear-to-end-of-line delay

; Strings start here.

CL:	defb	ESC,'+',0	; Clear-screen string		0
	defb	ESC
	defm	'=%+ %+ '
	defb	0		; Cursor movement string	1
	defb	ESC,'T',0	; Clear-to-end-of-line		2
	defb	ESC,')',0	; Standout-on string		3
	defb	ESC,'(',0	; Standout-end string		4
	defb	0		; Terminal init string		5
	defb	ESC,'(',0	; Terminal deinit string	6

; Extensions to Standard Z3TCAP 

	defb	ESC,'R',0	; Line Delete			7
	defb	ESC,'E',0	; Line Insert			8
	defb	ESC,'Y',0	; Clear-to-end-of-screen	9

; Set Attribute strings once again included.

	defb	ESC,'G',0	; Set Attributes		10
;	defb	ESC,'G%+0',0	; Set Attributes		10
	defm	'0248'
	defb	0		; Attributes			11

; These two allow reading the Terminal's screen.

	defb	ESC,'?',0	; Read current cursor position	12
	defb	ESC,'6',0	; Read line until cursor	13

; Graphics start here.

GOELD:	defb	0		; On/Off Delay			0

; Graphics strings offset from Delay value.

	defb	ESC,'H',2,0     ; Graphics mode On
	defb	ESC,'H',3,0     ; Graphics mode Off
	defb	ESC,'`','0',0      ; Cursor Off
	defb	ESC,'`','1',0      ; Cursor On

; Graphics Characters

	defb	'2'		; Upper left corner
	defb	'3'		; Upper right corner
	defb	'1'		; Lower left corner
	defb	'5'		; Lower right corner
	defb	':'		; Horizontal line
	defb	'6'		; Vertical line
	defb	'7'		; Full block
	defb	';'		; Hashed block
	defb	'0'		; Upper intersect
	defb	'='		; Lower intersect
	defb	'8'		; Mid intersect
	defb	'9'		; Right intersect
	defb	'4'		; Left intersect

;  Fill unused space with nulls

	defs	128-($-NZTCAP)


		psect	bss
nularg:		defs	14	; 3(duu) + 1(:) + 8(fn) +1 (.) + EOS
		end	start

