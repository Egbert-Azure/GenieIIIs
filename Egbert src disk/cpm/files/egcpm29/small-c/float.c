#include iolib.h
#include float.h
#asm
;
;	name...
;		float
;
;	purpose...
;		floating point routines for C programs
;
;
;	double float(),	/* integer to floating point conversion */
;	mod(),		/* mod(x,y) */
;	fabs(),		/* absolute value */
;	floor(),	/* largest integer not greater than */
;	ceil(),		/* smallest integer not less than */
;	rand();		/* random number in range 0...1 */
;	int ifix();	/* floating point to integer
;			(takes floor first) */
;
EXTRA:	DEFS	6
FA:	DEFS	6	;floating point accumulator
FASIGN:	DEFS	1	;msb indicates sign of FA
;			0 => negative, 1 => positive

;
L0F2E:	DEFB	0
SEED:	DEFB	80H,80H,0,0,0,0 ;seed for random number generator
;
afreg:  defw    0
bcreg:  defw    0
dereg:  defw    0
hlreg:  defw    0

afafex: push	bc
	push	hl

	push	af
	pop	hl
	ld	bc,(afreg)
	ld	(afreg),hl
	push	bc
	pop	af

	pop	hl
	pop	bc
	ret	

exdreg: push   bc
        push   hl
        ld     hl,(hlreg)
        pop    bc
        ld     (hlreg),bc
        pop    bc

        push   bc
        push   de
        ld     de,(dereg)
        pop    bc
        ld     (dereg),bc
        pop    bc

        push   de
        push   bc
        ld     bc,(bcreg)
        pop    de
        ld     (bcreg),de
        pop    de
        ret
;
DIVZERO: CALL	GRIPE
	DEFB	'can''t /0',0
ILLFCT:	CALL	GRIPE
	DEFB	'Illegal function',0
OFLOW:	CALL	GRIPE
	DEFB	'Arithmetic overflow',0
GRIPE:	CALL	QERR	;top word on stack points to message
	JP	0	;error was fatal
;
;	push the floating point accumulator
;	(preserving return address)
;
DPUSH:	POP	DE
	LD	HL,(FA+4)
	PUSH	HL
	LD	HL,(FA+2)
	PUSH	HL
	LD	HL,(FA)
	PUSH	HL
	EX	DE,HL
	JP	(HL)
;
;	push floating point accumulator
;	(preserve return address and next stacked word)
;
DPUSH2:	POP	DE	;save return address
	POP	BC	;save next word
	LD	HL,(FA+4)
	PUSH	HL
	LD	HL,(FA+2)
	PUSH	HL
	LD	HL,(FA)
	PUSH	HL
	EX	DE,HL
	PUSH	BC	;restore next word
	JP	(HL)	;return
;
;	convert the integer in hl to
;	a floating point number in FA
;
QFLOAT:	LD	A,H	;fetch MSB
	CPL		;reverse sign bit
	LD	(FASIGN),A ;save sign (msb)
	RLA		;move sign into cy
	JR	C,FL4	;c => nonnegative number
	EX	DE,HL
	SBC	HL,HL	;clear hl
	SBC	HL,DE	;get positive number into hl
FL4:	LD	A,L
	DEFB	0DDH
	LD	H,A	;move LSB to hx
	LD	C,H	;move MSB to c
	LD	DE,0	;clear rest of registers
	LD	B,D
	DEFB	0DDH
	LD	L,D	;clear lx
	LD	A,16+128
	LD	(FA+5),A ;preset exponent
	JP	NORM	;go normalize c ix de b
;
;	convert the floating point number in FA
;	to an integer in hl  (rounds toward negative numbers)
;
QIFIX:	CALL	QFLOOR		;take floor first
	LD	HL,0		;initialize the result
	LD	A,(FA+5)	;get the exponent
	LD	B,A		;  and save it
	OR	A
	RET	Z		;z => number was zero
	LD	HL,(FA+3)	;get most significant bits
	LD	C,H		;save sign bit (msb)
	LD	A,B		;get exponent again
	CP	80H+16
	JP	M,IFIX5		;m => fabs(fa) < 32768
	JR	NZ,OFLOW	;nz => fabs(fa) > 32768
;				(overflow)
	LD	A,H
	CP	80H
	JR	NZ,OFLOW	;nz => fa isn't -32768
	LD	A,L
	OR	A
	JR	NZ,OFLOW	;nz => overflow
	RET			;return -32768.
;
IFIX5:	SET	7,H		;restore hidden bit
IFIX6:	SRL	H		;shift right (0 fill)
	RR	L		;shift right (cy fill)
	INC	A
	CP	16+80H
	JR	NZ,IFIX6	;nz => haven't shifted enough
	RL	C
	RET	NC		;nc => positive number
	EX	DE,HL
	LD	HL,1		;compensate for cy bit
	SBC	HL,DE		;negate result
	RET
;
ADDHALF: LD	HL,HALF
HLADD:	CALL	LDBCHL
	JR	FADD
HALF:	DEFB	0,0,0,0,0,80H	;0.5
;
L247E:	CALL	PUSHFA
	CALL	L27EC
	POP	BC
	POP	IX
	POP	DE
	JR	FADD
;
HLSUB:	CALL	LDBCHL
	JR	FSUB
;
;	fmod(z,x) = z-x*floor(z/x)
;		if x>0 then  0 <= fmod(z,x) < x
;		if x<0 then  x < fmod(z,x) <= 0
;
QFMOD:	POP	HL	;return addr
	POP	DE	;discard next number
	POP	DE	; (already in FA)
	POP	DE
	POP	DE	;fetch next number
	POP	IX	; (1st operand, or "z")
	POP	BC
	PUSH	DE	;restore stack
	PUSH	DE
	PUSH	DE
	PUSH	DE
	PUSH	DE
	PUSH	DE
	PUSH	HL	;replace return addr
	PUSH	DE	;save another copy of z
	PUSH	IX
	PUSH	BC
	CALL	PUSHFA	;save a copy of 2nd operand ("x")
	CALL	FDIV	;z/x
	CALL	QFLOOR	;floor(z/x)
	POP	BC
	POP	IX
	POP	DE
	CALL	FMUL	;x*floor(z/x)
	POP	BC
	POP	IX
	POP	DE
;		to find mod(z,x)=z-x*floor(z/x), fall into...
FSUB:	CALL	MINUSFA
	JR	FADD
;
;	subtract the floating point accumulator from the value
;	on the stack (under the return address), leave result
;	in the floating point accumulator.
;
DSUB:	CALL	MINUSFA
;
;	add the value on the stack (under the return address)
;	to the floating point accumulator
;
DADD:	POP	HL	;save return address
	POP	DE
	POP	IX
	POP	BC
	PUSH	HL	;replace return address
;
;	add bc ix de to floating point accumulator
;
FADD:	LD	A,B
	OR	A
	RET	Z	;z => number to be added is zero
	LD	A,(FA+5)
	OR	A
	JP	Z,LDFABC ;z => accumulator is zero,
;				just load number
	SUB	B
	JR	NC,ADD2 ;nc => accumulator has larger number
	NEG		;reverse accumulator & bc ix de...
	call    exdreg  ;EXX
	PUSH	IX
	CALL	LDBCFA
	call	exdreg  ;EXX
	EX	(SP),IX
	CALL	LDFABC
	call	exdreg  ;EXX
	POP	IX	;...end of reversing
ADD2:	CP	29H
	RET	NC	;nc => addition makes no change
	PUSH	AF	;save difference of exponents
	CALL	UNPACK	;restore hidden bit & compare signs
	LD	H,A	;save difference in signs
	POP	AF	;recall difference of exponents
	CALL	RSHIFT	;shift  c ix de b  right by (a)
	OR	H
	LD	HL,FA
	JP	P,ADD4	;p => opposite signs, must subtract
	CALL	FRADD	;c ix de += FA
	JR	NC,PACK	;nc => adding caused no carry
	INC	HL
	INC	(HL)	;increment exponent
	JP	Z,OFLOW
	LD	L,1
	CALL	RSH8	;shift  c ix de b  right by 1
	JR	PACK	;round, hide msb, & load into FA
;
ADD4:	XOR	A	;negate b...
	SUB	B
	LD	B,A
	LD	A,(HL)	;c ix de -= FA...
	SBC	A,E
	LD	E,A
	INC	HL
	LD	A,(HL)
	SBC	A,D
	LD	D,A
	INC	HL
	LD	A,(HL)
	DEFB	0DDH
	SBC	A,L
	DEFB	0DDH
	LD	L,A
	INC	HL
	LD	A,(HL)
	DEFB	0DDH
	SBC	A,H
	DEFB	0DDH
	LD	H,A
	INC	HL
	LD	A,(HL)
	SBC	A,C
	LD	C,A	;...end of subtraction, fall into...
;
;	reverse sign if necessary (cy set) and normalize
;	(sign reversal necessary because we're using
;	sign-magnitude representation rather than
;	twos-complement)
;
NORMA:	CALL	C,MINUSBC
;
;	normalize the 48 bit number in c ix de b
;	current exponent is in FA+5
;
;	result loaded into FA
;
NORM:	LD	L,B
	LD	H,E
	XOR	A
NORM2:	LD	B,A
	LD	A,C
	OR	A
	JR	NZ,NORM12  ;nz => 7 or fewer shifts needed
;			shift c ix d hl  left by one byte
	DEFB	0DDH
	LD	C,H
	DEFB	0DDH
	LD	A,L
	DEFB	0DDH
	LD	H,A
	DEFB	0DDH
	LD	L,D
	XOR	A
	LD	D,H
	LD	H,L
	LD	L,A	;...end of shifting
;
	LD	A,B
	SUB	8	;adjust exponent
	CP	0D0H
	JR	NZ,NORM2
;
NORM4:	XOR	A
NORM6:	LD	(FA+5),A
	RET
;
NORM8:	DEC	B
;			shift  c ix d hl  left one bit...
	ADD	HL,HL
	RL	D
	call    afafex  ;(EX AF,AF')
	ADD	IX,IX
	call    afafex  ;(EX AF,AF')
	JR	NC,NORM10
	INC	IX
NORM10:	call	afafex  ;(EX AF,AF')
	RL	C	;...end of shifting
;
NORM12:	JP	P,NORM8	;p => high order bit still zero
	LD	A,B
;			move number to  c ix de b
	LD	E,H
	LD	B,L
	OR	A
	JR	Z,PACK	;z => exponent unchanged
	LD	HL,FA+5		;update exponent
	ADD	A,(HL)
	LD	(HL),A
	JR	NC,NORM4	;nc => underflow (set to 0)
	RET	Z		;z => underflow (leave as 0)
PACK:	LD	A,B
PACK2:	LD	HL,FA+5	;round c ix de b to 40 bits
	OR	A
	CALL	M,INCR
	LD	B,(HL)	;load exponent
	INC	HL
	LD	A,(HL)	;recover sign
	AND	80H	;mask out all but sign
	XOR	C	;add to high
	LD	C,A	;   order byte
	JP	LDFABC	;place answer in FA
;
INCR:	INC	E	;increment c ix de
	RET	NZ
	INC	D
	RET	NZ
	DEFB	0DDH
	INC	L
	RET	NZ
	DEFB	0DDH
	INC	H
	RET	NZ
	INC	C
	RET	NZ	;z => carry
	LD	C,80H	;set high order bit
	INC	(HL)	;   and increment exponent
	RET	NZ
	JP	OFLOW
;
;	fraction add: c ix de += (hl)
;
FRADD:	LD	A,(HL)
	ADD	A,E
	LD	E,A
	INC	HL
	LD	A,(HL)
	ADC	A,D
	LD	D,A
	INC	HL
	LD	A,(HL)
	DEFB	0DDH
	ADC	A,L
	DEFB	0DDH
	LD	L,A
	INC	HL
	LD	A,(HL)
	DEFB	0DDH
	ADC	A,H
	DEFB	0DDH
	LD	H,A
	INC	HL
	LD	A,(HL)
	ADC	A,C
	LD	C,A
	RET
;
;	complement FASIGN and negate the fraction c ix de b
;
MINUSBC: LD	HL,FASIGN
	LD	A,(HL)
	CPL
	LD	(HL),A
	XOR	A
	LD	L,A
	LD	H,A
	SUB	B
	LD	B,A
	LD	A,L
	SBC	HL,DE
	EX	DE,HL
	LD	L,A
	DEFB	0DDH
	SBC	A,L
	DEFB	0DDH
	LD	L,A
	LD	A,L
	DEFB	0DDH
	SBC	A,H
	DEFB	0DDH
	LD	H,A
	LD	A,L
	SBC	A,C
	LD	C,A
	RET
;
;	shift  c ix de b  right by (a)
;
RSHIFT:	LD	B,0
RSH2:	SUB	8
	JR	C,RSH4	;c => 7 or fewer shifts remain
	LD	B,E	;shift  c ix de b  right by 8...
	LD	E,D
	DEFB	0DDH
	LD	D,L
	call    afafex  ;(EX AF,AF')
	DEFB	0DDH
	LD	A,H
	DEFB	0DDH
	LD	L,A
	call	afafex	;(EX AF,AF')
	DEFB	0DDH
	LD	H,C
	LD	C,0	;...end of shifting
	JR	RSH2
;
RSH4:	ADD	A,9
	LD	L,A
RSH6:	XOR	A
	DEC	L
	RET	Z	;z => requested shift is complete
	LD	A,C
RSH8:	RRA		;shift  c ix de b  right by one...
	LD	C,A
	DEFB	0DDH
	LD	A,H
	RRA
	DEFB	0DDH
	LD	H,A
	DEFB	0DDH
	LD	A,L
	RRA
	DEFB	0DDH
	LD	L,A
	RR	D
	RR	E
	RR	B	;...end of shifting
	JR	RSH6
;
;	multiply the floating point accumulator by the value
;	on the stack (under the return address), leave result
;	in the floating point accumulator.
;
DMUL:	POP	HL	;return addr
	POP	DE	;multiplier...
	POP	IX
	POP	BC
	PUSH	HL	;replace return addr
;
;	multiply floating point accumulator by  bc ix de
;
FMUL:	CALL	SGN
	RET	Z	; z => accumulator has zero
	LD	L,0	;"product" flag
	CALL	DIV14	;find exponent of product
	LD	A,C  ;c' h'l' d'e' (multiplicand) = c ix de...
	PUSH	DE
	call	exdreg  ;EXX
	LD	C,A
	POP	DE
	PUSH	IX
	POP	HL
	call	exdreg  ;EXX ;...end of multiplicand loading
	LD	BC,0	; c ix de b (product) = 0...
	LD	D,B
	LD	E,B
	LD	IX,0
	LD	HL,NORM	; push addr of normalize routine
	PUSH	HL
	LD	HL,MULLOOP	; push addr of top of loop
	PUSH	HL	; (5 iterations wanted,
	PUSH	HL	; once per byte of fraction)
	PUSH	HL
	PUSH	HL
	LD	HL,FA	;point to LSB
MULLOOP: LD	A,(HL)	;get next byte of multiplier
	INC	HL
	OR	A
	JR	NZ,MUL2	; z => next 8 bits of multiplier are 0
	LD	B,E	;shift  c ix de b  right by 8...
	LD	E,D
	DEFB	0DDH
	LD	D,L
	call	afafex	;(EX AF,AF')
	DEFB	0DDH
	LD	A,H
	DEFB	0DDH
	LD	L,A
	call	afafex	;(EX AF,AF')
	DEFB	0DDH
	LD	H,C
	LD	C,A	;...end of shifting
	RET		;go to top of loop or NORM
;
MUL2:	PUSH	HL	;save multiplier pointer
	EX	DE,HL
	LD	E,8	;8 iterations (once per bit)
MUL4:	RRA		;rotate a multiplier bit into cy
	LD	D,A
	LD	A,C
	JR	NC,MUL6	; nc => no addition needed
	PUSH	HL	; c ix hl (product)  +=
	call	exdreg  ;EXX ; c' h'l' d'e' (multiplicand)
	EX	(SP),HL
	ADD	HL,DE
	EX	(SP),HL
	EX	DE,HL
	PUSH	IX
	EX	(SP),HL
	ADC	HL,DE
	EX	(SP),HL
	POP	IX
	EX	DE,HL
	ADC	A,C
	call	exdreg	;EXX
	POP	HL
;
MUL6:	RRA	   ;shift  c ix hl b (product)  right by 1...
	LD	C,A
	DEFB	0DDH
	LD	A,H
	RRA
	DEFB	0DDH
	LD	H,A
	DEFB	0DDH
	LD	A,L
	RRA
	DEFB	0DDH
	LD	L,A
	RR	H
	RR	L
	RR	B		;...end of shifting
	DEC	E
	LD	A,D
	JR	NZ,MUL4		; z => 8 iterations complete
	EX	DE,HL
MUL8:	POP	HL		;recover multiplier pointer
	RET			;go to top of loop or NORM
;
;	divide floating point accumulator by 10
;
DIV10:	CALL	PUSHFA
	LD	BC,8420H	; 10.0
	LD	IX,0
	LD	DE,0
	CALL	LDFABC
DIV1:	POP	BC
	POP	IX
	POP	DE
	JR	FDIV
;
;	divide the value on the stack (under the return
;	address) by the floating point accumulator, leave
;	result in the floating point accumulator.
;
DDIV:	POP	HL	;save return address
	POP	DE
	POP	IX
	POP	BC
	PUSH	HL	;replace return address
;
;	divide  bc ix de  by FA, leave result in FA
;
FDIV:	CALL	SGN
	JP	Z,DIVZERO ; z => attempting to divide by 0
	LD	L,0FFH	;"quotient" flag
	CALL	DIV14	;find quotient exponent
	PUSH	IY
	INC	(HL)
	INC	(HL)
	DEC	HL
	PUSH	HL	; c' h'l' d'e' (divisor) = FA...
	call	exdreg  ;EXX
	POP	HL
	LD	C,(HL)
	DEC	HL
	LD	D,(HL)
	DEC	HL
	LD	E,(HL)
	DEC	HL
	LD	A,(HL)
	DEC	HL
	LD	L,(HL)
	LD	H,A
	EX	DE,HL
	call	exdreg	;EXX
	LD	B,C	; b iy hl (dividend) = c ix de...
	EX	DE,HL
	PUSH	IX
	POP	IY
	XOR	A	; c ix de (quotient) = 0...
	LD	C,A
	LD	D,A
	LD	E,A
	LD	IX,0
	LD	(EXTRA),A
DIV2:	PUSH	HL	;save b iy hl in case the subtraction
	PUSH	IY	; proves to be unnecessary
	PUSH	BC
	PUSH	HL	; EXTRA b iy hl (dividend)  -=
	LD	A,B	;	c' h'l' d'e' (divisor)...
	call	exdreg  ;EXX
	EX	(SP),HL
	OR	A
	SBC	HL,DE
	EX	(SP),HL
	EX	DE,HL
	PUSH	IY
	EX	(SP),HL
	SBC	HL,DE
	EX	(SP),HL
	POP	IY
	EX	DE,HL
	SBC	A,C
	call	exdreg	;EXX
	POP	HL
	LD	B,A
	LD	A,(EXTRA)
	SBC	A,0
	CCF
	JR	NC,DIV4	; nc => subtraction caused carry
	LD	(EXTRA),A
	POP	AF	;discard saved value of dividend...
	POP	AF
	POP	AF
	SCF
	JR	DIV6
DIV4:	POP	BC	;restore dividend...
	POP	IY
	POP	HL
;
DIV6:	INC	C
	DEC	C
	RRA
	JP	M,DIV12
	RLA	  ;shift  c ix de a (quotient)  left by 1...
	RL	E
	RL	D

	call    afafex    ;(EX AF,AF') (these 6 lines are  adc ix,ix...)
	ADD	IX,IX
	call	afafex	;(EX AF,AF')
	JR	NC,DIV8
	INC	IX
DIV8:	call	afafex	;(EX AF,AF')
	RL	C	;...end of  c ix de a  shifting
	ADD	HL,HL	;shift  EXTRA b iy hl  left by 1...
	call	afafex  ;(EX AF,AF')
	ADD	IY,IY
	call	afafex	;(EX AF,AF')
	JR	NC,DIV9
	INC	IY
DIV9:	call	afafex  ;(EX AF,AF')
	RL	B
	LD	A,(EXTRA)
	RLA
	LD	(EXTRA),A  ;...end of  EXTRA b iy hl  shifting
	LD	A,C	;test  c ix de...
	OR	D
	OR	E
	DEFB	0DDH
	OR	H
	DEFB	0DDH
	OR	L	;...end of  c ix de  testing
	JR	NZ,DIV2	;nz => dividend nonzero
	PUSH	HL
	LD	HL,FA+5
	DEC	(HL)
	POP	HL
	JR	NZ,DIV2
	JR	OFLOW2
;
DIV12:	POP	IY
	JP	PACK2
;
;	find exponent for product (L=0) or quotient (L=ff)
;
DIV14:	LD	A,B
	OR	A
	JR	Z,DIV20
	LD	A,L	;get product/quotient flag
	LD	HL,FA+5
	XOR	(HL)	;get +-FA exponent
	ADD	A,B	;find and...
	LD	B,A	;...load new exponent
	RRA
	XOR	B
	LD	A,B
	JP	P,DIV18
	ADD	A,80H
	LD	(HL),A
	JP	Z,MUL8
	CALL	UNPACK	;restore hidden bits & compare signs
	LD	(HL),A	;save difference of signs
	DEC	HL	;point to MSB of fraction
	RET
;
DIV17:	CALL	SGN
	CPL
	OR	A
	DEFB	21H	;"ignore next 2 bytes"
DIV18:	OR	A
DIV20:	POP	HL
	JP	P,NORM4
OFLOW2:	JP	OFLOW
;
;	multiply FA by 10
;
MUL10:	CALL	LDBCFA
	LD	A,B	;multiply bc ix de by 4...
	OR	A
	RET	Z
	ADD	A,2
	JR	C,OFLOW2
	LD	B,A	;...end of multiplication
	CALL	FADD	;add to FA, yields FA*5
	LD	HL,FA+5
	INC	(HL)	;double again, yielding FA*10
	RET	NZ
	JR	OFLOW2
;
; L27DB:	LD	BC,9980H	; -2**24
	LD	IX,0
	LD	DE,0
	CALL	COMPARE
	RET	Z
	JP	ILLFCT
;
L27EC:	LD	B,88H	; 128.
	LD	DE,0
L27F1:	LD	HL,FA+5
	LD	C,A
	PUSH	DE
	POP	IX
	LD	DE,0
	LD	(HL),B	;store exponent
	LD	B,0
	INC	HL
	LD	(HL),80H	;store minus sign
	RLA
	JP	NORMA
;
	EX	DE,HL
	XOR	A
	LD	B,98H
	JR	L27F1
	LD	B,C
L280C:	LD	D,B
	LD	E,0
	LD	HL,L0F2E
	LD	(HL),E
	LD	B,90H
	JR	L27F1
	LD	B,A
	XOR	A
	JR	L280C
;
L281B:	CALL	SGN
	JP	M,ILLFCT
	LD	A,(FA+5)
	CP	91H
	JP	C,INT2
	LD	BC,9180H	; -2**16
	LD	IX,0
	LD	DE,0
	CALL	COMPARE
	LD	D,C
	RET	Z
L2838:	JP	ILLFCT
	CALL	L281B
	LD	A,D
	OR	A
	JR	NZ,L2838
	LD	A,E
	RET
;
;	set z & s flags per FA
;
SGN:	LD	A,(FA+5)
	OR	A
	RET	Z
	LD	A,(FA+4)
	DEFB	0FEH	;"ignore next byte"
SETFLGS: CPL
	RLA
	SBC	A,A
	RET	NZ
	INC	A
	RET
;
;	Double precision comparisons
;
;	each compares top of stack
;	(under two return addresses) to FA
;
;TOS >= FA?
DGE:	CALL	DCOMPAR
	JR	Z,YES	;z => equal
	JR	DG	;remaining tests are shared
;
;TOS > FA?
DGT:	CALL	DCOMPAR
	JR	Z,NO	;z => equal
DG:	JP	P,NO	;p => not greater than
YES:	LD	HL,1	;load "true"
	RET
;
;TOS <= FA?
DLE:	CALL	DCOMPAR
	JR	Z,YES
	JR	DL
;
;TOS < FA?
DLT:	CALL	DCOMPAR
	JR	Z,NO
DL:	JP	P,YES	;p => less than
NO:	LD	HL,0	;load "false"
	RET
;
;TOS == FA?
DEQ:	CALL	DCOMPAR
	JR	Z,YES
	JR	NO
;
;TOS != FA?
DNE:	CALL	DCOMPAR
	JR	NZ,YES
	JR	NO
;
;common routine to perform double precision comparisons
DCOMPAR: POP	HL	;save 1st return addr
	POP	IY	;save 2nd return addr
	POP	DE	;get number to compare
	POP	IX
	POP	BC
	PUSH	IY	;replace 2nd addr
	PUSH	HL	;replace 1st addr, fall into...
;
;	sets flags per FA - (bc ix de)
;
COMPARE: LD	A,B
	OR	A
	JR	Z,SGN		;bc ix de = 0, so
;				sign of FA is result

	CALL	SGN
	LD	A,C
	JR	Z,SETFLGS	;FA = 0, so sign of
;				-(bc ix de) is result
	LD	HL,FA+4
	XOR	(HL)
	LD	A,C
	JP	M,SETFLGS	;operands have opposite
;				signs, so result is sign
;				of -(bc ix de)

	CALL	CPFRAC
	RRA			;recover cy bit
	XOR	C	;reverse sign if numbers are negative
	JR	SETFLGS
;
CPFRAC:	INC	HL	;compare  bc ix de  to (HL)
	LD	A,B
	CP	(HL)
	RET	NZ
	DEC	HL
	LD	A,C
	CP	(HL)
	RET	NZ
	DEC	HL
	DEFB	0DDH
	LD	A,H
	CP	(HL)
	RET	NZ
	DEC	HL
	DEFB	0DDH
	LD	A,L
	CP	(HL)
	RET	NZ
	DEC	HL
	LD	A,D
	CP	(HL)
	RET	NZ
	DEC	HL
	LD	A,E
	SUB	(HL)
	RET	NZ
	POP	HL	;return zero to program
	RET		;that called "COMPARE"
;
QFABS:	CALL	SGN
	RET	P
MINUSFA: LD	HL,FA+4
	LD	A,(HL)
	XOR	80H
	LD	(HL),A
	RET
;
PUSHFA:	EX	DE,HL
L2895:	LD	HL,(FA)
	EX	(SP),HL
	PUSH	HL
	LD	HL,(FA+2)
	EX	(SP),HL
	PUSH	HL
	LD	HL,(FA+4)
	EX	(SP),HL
	PUSH	HL
	EX	DE,HL
	RET
;			This can be reinstated when the compiler
;			understands "extern". Until then, pi
;			can't be declared without reserving
;			storage again.
; QPI:	DEFW	0A222H	;3.1415926535 = pi
; 	DEFW	0FDAH
; 	DEFW	8249H
;
;	FA = (hl)
;
DLOAD:	LD	DE,FA
	LD	BC,6
	LDIR
	RET
;
;	exchange floating point accumulator with
;	top of stack (under return address)
;
DSWAP:	POP	HL	;return addr
	POP	DE
	POP	IX
	POP	BC
	call	exdreg  ;EXX ;protect the values
	CALL	DPUSH	;push FA
	call	exdreg  ;EXX ;recover the values
	PUSH	HL	;replace return addr, fall into...
;
;	FA = bc ix de
;
LDFABC:	LD	(FA),DE
	LD	(FA+2),IX
	LD	(FA+4),BC
	RET
;
;	bc ix de = FA
;
LDBCFA:	LD	DE,(FA)
	LD	IX,(FA+2)
	LD	BC,(FA+4)
	RET
;
;	bc ix de = (hl)
;
LDBCHL:	LD	E,(HL)
	INC	HL
	LD	D,(HL)
	INC	HL
	LD	C,(HL)
	DEFB	0DDH
	LD	L,C
	INC	HL
	LD	C,(HL)
	DEFB	0DDH
	LD	H,C
	INC	HL
	LD	C,(HL)
	INC	HL
	LD	B,(HL)
	INC	HL
	RET
;
;	(hl) = FA
;
DSTORE:	LD	DE,FA
	LD	BC,6
	EX	DE,HL
	LDIR
	EX	DE,HL
	RET
;
UNPACK:	LD	HL,FA+4
	LD	A,(HL)	;get MSB of fraction
	RLCA		;rotate sign bit into lsb
	SCF		;set carry
	RRA		;rotate sign bit into cy, cy into msb
	LD	(HL),A	;restore MSB (with hidden bit restored)
	CCF		;complement sign bit...
	RRA
	INC	HL
	INC	HL
	LD	(HL),A	;...and save in msb of FASIGN
	LD	A,C	;similarly, get sign bit of bc ix de...
	RLCA
	SCF
	RRA
	LD	C,A	;...restore hidden bit...
	RRA
	XOR	(HL)	;...and compare with sign of FA.
	RET
;
INT2:	LD	B,A	;if a==0, return with  bc ix de = 0...
	LD	C,A
	LD	D,A
	LD	E,A
	DEFB	0DDH
	LD	H,A
	DEFB	0DDH
	LD	L,A
	OR	A
	RET	Z
	PUSH	HL
	CALL	LDBCFA	;copy FA into bc ix de,
	CALL	UNPACK	; restore hidden bits
	XOR	(HL)
	LD	H,A	;put sign in msb of h
	JP	P,INT4 ;p => positive number
	DEC	DE	;decrement c ix de...
	LD	A,D
	AND	E
	INC	A
	JR	NZ,INT4
	DEC	IX
	DEFB	0DDH
	LD	A,H
	DEFB	0DDH
	AND	L
	INC	A
	JR	NZ,INT4
	DEC	C	;...end of c ix de decrementing
;
INT4:	LD	A,0A8H	;shift  c ix de  right so bits to
	SUB	B	; the right of the binary point
	CALL	RSHIFT	; are discarded
	LD	A,H
	RLA
	CALL	C,INCR	;c => negative, increment  c ix de
	LD	B,0
	CALL	C,MINUSBC ;negate the fraction c ix de
	POP	HL
	RET
;
	POP	BC
	POP	IX
	POP	DE
;
;	divide with integer result
;	(truncates toward zero)
;
DIVI:	CALL	FDIV
	CALL	SGN
	JP	P,QFLOOR
	CALL	MINUSFA
	CALL	QFLOOR
	JP	MINUSFA
;
;	return -(floor(-x))
QCEIL:	CALL	ODD
;
;	return largest integer not greater than
QFLOOR:	LD	HL,FA+5
	LD	A,(HL)	;fetch exponent
	CP	0A8H
	LD	A,(FA)
	RET	NC	;nc => binary point is right of lsb
	LD	A,(HL)
	CALL	INT2
	LD	(HL),0A8H  ;place binary pt at end of fraction
	LD	A,E
	PUSH	AF
	LD	A,C
	RLA
	CALL	NORMA
	POP	AF
	RET
;
	LD	HL,FA+5
	LD	(HL),0A8H
	INC	HL
	LD	(HL),80H
	LD	A,C
	RLA
	JP	NORMA
;	amax(a,b)	returns the greater of a and b
QAMAX:	LD	HL,8	;offset for 1st argument
	ADD	HL,SP
	CALL	LDBCHL	;bcixde := 1st argument
	CALL	COMPARE
	JP	M,LDFABC
	RET
;
;	amin(a,b)
QAMIN:	LD	HL,8
	ADD	HL,SP
	CALL	LDBCHL
	CALL	COMPARE
	JP	P,LDFABC
	RET
;
;	negate FA, and push address of MINUSFA
;	called to evaluate functions f(x) when the argument is
;	negative and f() satisfies f(-x)=-f(x)
ODD:	CALL	MINUSFA
L29D1:	LD	HL,MINUSFA
	EX	(SP),HL
	JP	(HL)
;
QRAND:	CALL	SGN
	LD	HL,SEED
	JP	M,DSTORE
	CALL	DLOAD
	RET	Z
	LD	BC,9835H	; 11879545.
	LD	IX,447AH
	LD	DE,0
	CALL	FMUL
	LD	BC,6828H	; 3.92767775e-8
	LD	IX,0B146H
	LD	DE,0
	CALL	FADD
	CALL	LDBCFA
	LD	A,E
	LD	E,C
	LD	C,A
	LD	HL,FASIGN
	LD	(HL),80H
	DEC	HL
	LD	B,(HL)
	LD	(HL),80H
	CALL	NORM
	LD	HL,SEED
	JP	DSTORE
#endasm
