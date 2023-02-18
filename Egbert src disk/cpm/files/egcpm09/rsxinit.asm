
; RSXINIT.ASM	copyright (c) 1983 by George F Peace

; usage: RSXINIT			loads the RSX
; 	 RSXINIT <any parameter>	unloads the RSX

dma	equ	80h
bdos	equ	5
wboot	equ	0
callrsx	equ	60
rsx$init equ	13
rsx$term equ	14

	org	100h
	lda	dma		;get command tail length
	cpi	0		;anything there?
	jnz	termrsx		;yes - assume terminate request
	mvi	a,rsx$init	;get initialization function
loadrsx:
	sta	rsxpb		;store subfunction in parameter block
	mvi	c,callrsx	;get RSX command index
	lxi	d,rsxpb		;get RSX parameter block address
	call	bdos
	mvi	c,wboot		;terminate the program but not the RSX
	jmp	bdos
termrsx:
	mvi	a,rsx$term	;get RSX termination function
	jmp	loadrsx		;now go set up the RSX

rsxname: db	'RSXTEST '	;name of RSX we're calling
rsxpb:	db	rsx$init	;RSX initialization function
	db	1		;1 parameter word follows
	dw	rsxname		;parameter 1 address
	end
