;RSXDEMO.ASM  version 1.0   Copyright (c) 1983 by George F Peace   7-15-83

;This is a demonstration of a Resident System Extension (RSX) under CP/M Plus.
;The routine intercepts all console-related BDOS function calls and displays
;the function number for each one called. If BDOS was CALLed via address 0005h,
;the address of the CALL will also be displayed.

;See RSXDEMO.DOC for information on RSX configuration and operation.

conout	equ	2		;output character to console
pstring	equ	9		;print string function
cr	equ	00dh
lf	equ	00ah
esc	equ	01bh

;	BDOS function 60 sub-functions used by this RSX

rsxfunc1 equ	13		;RSX initialize function code
rsxfunc2 equ	14		;RSX terminate function code

;	RSX Prefix Structure

serial:	db	0,0,0,0,0,0	;room to insert serial number
start:	jmp	ftest		;beginning of program
next:	db	0C3H		;jump instruction op-code
	dw	0		;next module in line (or BDOS)
prev:	dw	0		;previous module
remov:	db	00h		;remove flag clear
nonbnk:	db	0		;>0 to load only in non-banked CP/M
rsxnam:	db	'RSXDEMO '	;the name of this RSX
loader:	db	0		;loader flag
	db	0,0		;reserved

ftest:

;The first thing we want to do is save the caller's environment to allow
;normal processing of the BDOS request in the next (and subsequent) RSX.

	push	h		;save caller's hl on the original stack
	lxi	h,0		;clear hl
	dad	sp		;compute caller's stack address
	shld	ret$stack	;save so we can restore caller's environment
	lxi	sp,loc$stack	;load new stack address
	push	d		;save caller's de on local stack
	push	b		;save caller's bc on local stack

;Pick up the BDOS function number and test for one of those getting
;special treatment.

	mov	a,c		;get BDOS function
	cpi	0		;warm boot?
	jz	finish		;  yes - ignore it
	cpi	50		;direct BIOS call?
	jz	bdos50
	cpi	60		;one of my calls?
	jz	bdos60
	cpi	111		;print block?
	jz	bdos111
	cpi	112		;list block?
	jz	bdos112
	cpi	12		;less than 12?
	jnc	finish		;nope - not my call
	cpi	1		;console input?
	jz	bdos1
	cpi	2		;console output?
	jz	bdos2
	cpi	3		;aux: input?
	jz	bdos3
	cpi	4		;aux. output?
	jz	bdos4
	cpi	5		;list output?
	jz	bdos5
	cpi	6		;direct console i/o?
	jz	bdos6
	cpi	7		;aux: input status?
	jz	bdos7
	cpi	8		;aux: output status?
	jz	bdos8
	cpi	9		;print string?
	jz	bdos9
	cpi	10		;read console buffer?
	jz	bdos10
	cpi	11		;console status?
	jz	bdos11
	jmp	finish

bdos1:
	jmp	bdostrap

bdos2:
	jmp	bdostrap

bdos3:
	jmp	bdostrap

bdos4:
	jmp	bdostrap

bdos5:
	jmp	bdostrap

bdos6:
	jmp	bdostrap

bdos7:
	jmp	bdostrap

bdos8:
	jmp	bdostrap

bdos9:
	jmp	bdostrap

bdos10:
	jmp	bdostrap

bdos11:
	jmp	bdostrap

bdos50:
	jmp	bdostrap

bdos60:
;This is the RSX-specific BDOS call. Check to see if it's one of our own.
;It might be necessary to add checks for both function and parameter match
;before assuming that this is for us in case multiple RSXs are in effect.
	ldax	d		;get RSX function number from parameter block
	cpi	rsxfunc1	;is it initialize?
	jz	rsxinit		;yes - initialize the RSX environment
	cpi	rsxfunc2	;is it terminate?
	jnz	finish		;no match - continue
	lda	remov		;get RSX remove flag
	cpi	0FFh		;already flagged to be removed?
	jz	finish		;yes - exit with no further activity
	mvi	a,0FFh		;get remove flag
	sta	remov		;flag to drop this RSX on next warm boot
	jmp	finish		;finished with RSX function, continue
rsxinit:
	mvi	c,pstring	;print string on console
	lxi	d,initmsg	;print initialization message
	call	next		;call next RSX
	jmp	finish		;exit this RSX

bdos111:
	jmp	bdostrap

bdos112:
	jmp	bdostrap

bdostrap:
;General BDOS function trap routine.
;This routine displays pertinent data about the BDOS call being performed.
	mvi	c,pstring	;display string on console
	lxi	d,string1	;first part of trap message
	call	next		;call next RSX

	pop	b		;retrieve bc register pair
	push	b		;replace bc pair on stack
	mov	a,c		;position the BDOS function for print
	call	outbyte		;print as two hex digits
	mvi	c,pstring	;print string on console
	lxi	d,string2	;finish up trap notification
	call	next		;call next RSX

	lhld	ret$stack	;get caller's stack address

;Check the three bytes preceding the address stored at the top of the caller's
;stack. If the instruction was (or at least looks like) a CALL 5, print its
;address. Although the code checks for both Jxx and Cxx instruction op-codes,
;only the Cxx codes will be encountered as the Jxx instructions don't update
;the stack.
	inx	h		;move up to previous entry on stack since
	inx	h		;  our initialization added HL to the stack
	mov	e,m		;get low order byte of last entry on stack
	inx	h		;point to next byte on stack
	mov	d,m		;get high order byte of last entry on stack
	xchg			;move stack entry to HL

	dcx	h		;point to high byte of possible call/jmp 5
	mov	a,m		;get the byte
	cpi	00h		;is it 00h (as in call/jmp 0005)?
	jnz	nobdos		;no - skip display
	dcx	h		;point to low byte of possible call/jmp 5
	mov	a,m		;get the byte
	cpi	05h		;is it 00h (as in call/jmp 0005)?
	jnz	nobdos		;no - skip display
	dcx	h
	mov	a,m
;all the jmp/call series instructions have bits 6 and 7 set to 1
	ani	0c0h		;and out all bits except the common ones
	cpi	0c0h		;have (likely) match on opcode?
	jnz	nobdos		;not a match - skip display
bdosaddr:
	push	h		;hl points to call/jmp BDOS
	mvi	c,pstring	;print string on console
	lxi	d,string3	;print call address header
	call	next		;call next RSX
	pop	h		;retrieve BDOS call address
	call	printhl		;display address in HL
	call	space		;print a space
nobdos:
	jmp	finish

finish:
;We're finished with the BDOS call. Restore the caller's original environment
;and pass control to the next RSX or BDOS.
	pop	b		;restore caller's bc
	pop	d		;restore caller's de
	lhld	ret$stack	;restore user stack address
	sphl			;load stack pointer
	pop	h		;load caller's hl
	jmp	next		;pass control to next RSX

;------ subroutines ------

outbyte:
;display the single byte in the accumulator as two hex digits
	push	psw		;save the byte
	rrc			;shift accumulator right 4 bits
	rrc
	rrc
	rrc
	call	outnib		;print high digit
	pop	psw		;restore byte
	jmp	outnib		;print low digit
outnib:
	ani	0fh		;mask 4 bits
	adi	90h		;add offset
	daa			;dec adjust
	aci	40h		;add offset
	daa			;dec adjust
	mov	e,a		;to E for output
	mvi	c,conout	;console output request
	jmp	next		;call BDOS

printhl:
;print HL on the console as four hex digits
	push	h		;save address
	mov	a,h		;position H for display
	push	h		;save address on our stack
	call	outbyte		;display the high order byte
	pop	h		;restore address
	mov	a,l		;position L for display
	call	outbyte		;display the byte
	pop	h		;restore address
	ret
space:
;print a single space on the console
	mvi	c,conout	;console output request
	mvi	e,' '		;outout a space
	call	next		;call BDOS
	ret

;------ literals ------

initmsg:
	db	cr,lf,'RSXDEMO has been loaded'
	db	'$'

string1:
	db	cr,lf,'BDOS function '
	db	'$'

string2:
	db	'h trapped '
	db	'$'

string3:
	db	'at address '
	db	'$'

ret$stack:
;caller's stack pointer
	dw	0

;local stack storage
	ds	40		; 20 level stack
loc$stack:

	end
    