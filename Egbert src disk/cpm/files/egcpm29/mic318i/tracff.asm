;MI - C  v-3.18   (C) COPYRIGHT  G. KERSTING / H. ROSE, 1983
;
;
;
;
;LAUFZEITSYSTEM - FUNKTIONENTRACE
;
;	ENTRY CCTRCFF
;	ENTRY CCTRCZ
;	ENTRY CCTRCCO
;	ENTRY CCTRODEC
;	ENTRY CCTASTEIN
;	ENTRY CCTRTEXT
;	EXT CCTRCV
;	EXT GETCHAR
;	EXT PUTCHAR
;	EXT CHRRDY
; EXT CCAND
; EXT CCEQ,CCNE,CCGT,CCLE,CCGE,CCLT
; EXT CCDIV
; EXT CCSDIV,CCSASR,CCSASL
;
;	ENTRY CLTRCZ
;	EXT CCTRCV
;	EXT CCTRCCO
;	EXT CCTASTEIN
;	EXT CCTRTEXT
;	EXT GETCHAR
;	EXT PUTCHAR
;	EXT CHRRDY
; EXT CCAND
; EXT CCEQ,CCNE,CCGT,CCLE,CCGE,CCLT
; EXT CCSASL
; EXT CLPRIM,CLSEC
; EXT CCTOL,CCSTOL
; EXT CLGDIR,CLPUTM,CLSPIND,CLSGPIND,CLGPIND,CLLT,CLSGSIND
; EXT CLGE,CLDIV,CLEQ,CLMOD,CLSWAP,CLSASR,CLAND
;
;
;	ENTRY CFTRCZ
;	EXT CCTRODEC
;	EXT CCTASTEIN
;	EXT CCTRTEXT
;	EXT CCTRCCO
;	EXT CCTRCV
;	EXT GETCHAR
;	EXT PUTCHAR
;	EXT CHRRDY
;	EXT CFPRIM
;	EXT CFNEG
;
;
;
;
CCTRCFF:
	XTHL
	PUSH PSW
;
	PUSH B
	PUSH D
	PUSH H
;
	CALL CCTASTEIN
;
	POP H
;
	LDA CCTRCV
	ANA A
	JZ CCTRC3
;
	CALL CCTRTEXT
	JMP CCTRC2
CCTRC3:
	CALL CCTRAUT
CCTRC2:
	POP D
	POP B
;
	POP PSW
	XTHL
	RET
;
;
CCTRCCO:
	PUSH H
	MOV L,A
	PUSH H
	CALL PUTCHAR
	POP H
	POP H
	RET
;
;
CCTASTEIN:
	CALL CHRRDY
	MOV A,L
	ANA A
	RZ
	CALL GETCHAR
	MOV A,L
	ANI 05FH
	CPI 'N'
	JNZ CCTRC5
	MVI A,0
	STA CCTRCV
	RET
CCTRC5:
	CPI 'T'
	JNZ CCTRC6
	MVI A,1
	STA CCTRCV
	RET
CCTRC6:
	CPI 'X'
	RNZ
	MVI A,0FFH
	STA CCTRCV
	RET
;
CCTRTEXT:
	MVI A,0DH
	CALL CCTRCCO
	MVI A,0AH
	CALL CCTRCCO
;
CCTRC1:
	MOV A,M
	INX H
	ANA A
	RZ
	CALL CCTRCCO
	JMP CCTRC1
;
;
;
CCTRAUT:
	MOV A,M
	INX H
	ANA A
	JNZ CCTRAUT
	RET
;
;
;
CCTRCZ:
	XTHL
	PUSH PSW
;
	PUSH B
	PUSH D
	PUSH H
;
	CALL CCTASTEIN
;
	POP H
;
	LDA CCTRCV
	INR A
	JNZ CCTRC3
;
	CALL CCTRTEXT
	JMP CCTRC4
;
CCTRC8:
	CALL CCTRAUT
CCTRC4:
	POP D
	POP B
;
	POP PSW
	XTHL
	JMP TRACZZ
;
;
TRACZZ:
	PUSH PSW
	LDA CCTRCV
	INR A
	JNZ CCTRC7
	PUSH B
	PUSH D
	PUSH H
;
	PUSH H
	CALL CCTROHEX
	POP B
	MVI A,'/'
	CALL CCTRCCO
	POP H
	PUSH H
	PUSH H
	CALL CCTRODEC
	POP B
	POP H
	POP D
	POP B
CCTRC7:	POP PSW
	RET
;
C32303:

	DW   C32301+9
C32301:

	DB   57,56,55,54,53,52,51,50,49,48
	DB   49,50,51,52,53,54,55,56,57,65
	DB   66,67,68,69,70,0
CCTRODEC:
	LXI H,0
	SHLD C32302+3
	LXI H,10000
	SHLD C32302+1
	POP B
	POP D
	PUSH D
	PUSH B
	LXI H,0
	CALL CCLT
	JZ C32305
	LXI D,45
	PUSH D
	CALL PUTCHAR
	POP B
C32305:
C32306:
	LHLD C32302+1
	LXI D,1
	CALL CCLE
	JZ C32307
	LHLD C32303
	PUSH H
	LXI H,4
	DAD SP
	MOV E,M
	INX H
	MOV D,M
	LHLD C32302+1
	CALL CCDIV
	POP D
	DAD D
	MOV L,M
	MVI H,0
	SHLD C32302+5
	LHLD C32302+5
	LXI D,48
	CALL CCNE
	JNZ C32309
	LHLD C32302+1
	LXI D,1
	CALL CCEQ
	JNZ C32309
	LHLD C32302+3
	MOV A,H
	ORA L
	JNZ C32309
	JMP C32310
C32309:
	LXI H,1
C32310:
	MOV A,H
	ORA L
	JZ C32308
	LXI H,1
	SHLD C32302+3
	LHLD C32302+5
	PUSH H
	CALL PUTCHAR
	POP B
C32308:
	POP B
	POP D
	PUSH D
	PUSH B
	LHLD C32302+1
	CALL CCDIV
	XCHG
	POP B
	POP PSW
	PUSH H
	PUSH B
	LHLD C32302+1
	LXI D,10
	CALL CCSDIV
	SHLD C32302+1
	JMP C32306
C32307:
	RET
CCTROHEX:
	LXI H,4
	SHLD C32302+8
C32311:
	LHLD C32302+8
	DCX H
	SHLD C32302+8
	INX H
	LXI D,0
	CALL CCLT
	JZ C32312
	LHLD C32303
	PUSH H
	LHLD C32302+8
	LXI D,2
	CALL CCSASL
	XCHG
	LXI H,4
	DAD SP
	MOV A,M
	INX H
	MOV H,M
	MOV L,A
	CALL CCSASR
	LXI D,15
	CALL CCAND
	POP D
	DAD D
	MOV E,M
	MVI D,0
	PUSH D
	CALL PUTCHAR
	POP B
	JMP C32311
C32312:
	RET
;
;	DSEG
;CCTRCV:	DB 0FFH
;
C32302:
	DS   10
;	CSEG
;
;
;
;
CLTROUT:
	MOV A,M
	INX H
	ANA A
	JNZ CLTROUT
	RET
;
;
;
CLTRCZ:
	XTHL
	PUSH PSW
;
	PUSH B
	PUSH D
	PUSH H
;
	CALL CCTASTEIN
;
	POP H
;
	LDA CCTRCV
	INR A
	JZ CLTRC3
	CALL CLTROUT
	POP D
	POP B
	POP PSW
	XTHL
	RET
;
CLTRC3:
	CALL CCTRTEXT
	JMP CLTRC4
;
CLTRC8:
	CALL CLTROUT
CLTRC4:
	POP D
	POP B
;
	POP PSW
	XTHL
;	JMP TRACLZ
;
;
;TRACLZ:
	PUSH PSW
	LDA CCTRCV
	INR A
	JNZ CLTRC7
	PUSH B
	PUSH D
	PUSH H
	XCHG
	LHLD CLSEC
	PUSH H
	LHLD CLPRIM
	PUSH H
;
	PUSH H
	PUSH D
	PUSH H
	PUSH D
	CALL CLTROHEX
	POP B
	POP B
	MVI A,'/'
	CALL CCTRCCO
;
	CALL CLTRODEC
	POP B
	POP B
;
	POP H
	SHLD CLPRIM
	POP H
	SHLD CLSEC
	POP H
	POP D
	POP B
CLTRC7:	POP PSW
	RET
;
;
C32353:

	DW   C32351+9
C32351:

	DB   57,56,55,54,53,52,51,50,49,48
	DB   49,50,51,52,53,54,55,56,57,65
	DB   66,67,68,69,70,0
CLTRODEC:
	LXI H,0
	SHLD C32352+1
	CALL CLGDIR

	DW   -13824,15258
	LXI B,C32352+5
	CALL CLPUTM
	LXI H,2
	DAD SP
	CALL CLSGPIND
	LXI H,0
	CALL CCTOL
	CALL CLLT
	JZ C32355
	LXI D,45
	PUSH D
	CALL PUTCHAR
	POP B
C32355:
C32356:
	LXI H,1
	LXI D,C32352+5
	CALL CLSGSIND
	CALL CCTOL
	CALL CLGE
	JZ C32357
	LHLD C32353
	PUSH H
	LXI H,4
	DAD SP
	CALL CLSGPIND
	LXI H,C32352+5
	CALL CLGPIND
	CALL CLDIV
	POP D
	DAD D
	MOV L,M
	MVI H,0
	SHLD C32352+3
	LHLD C32352+3
	LXI D,48
	CALL CCNE
	JNZ C32359
	LXI H,1
	LXI D,C32352+5
	CALL CLSGSIND
	CALL CCTOL
	CALL CLEQ
	JNZ C32359
	LHLD C32352+1
	MOV A,H
	ORA L
	JNZ C32359
	JMP C32360
C32359:
	LXI H,1
C32360:
	MOV A,H
	ORA L
	JZ C32358
	LXI H,1
	SHLD C32352+1
	LHLD C32352+3
	PUSH H
	CALL PUTCHAR
	POP B
C32358:
	LXI H,2
	DAD SP
	CALL CLSGPIND
	LXI H,C32352+5
	CALL CLGPIND
	CALL CLMOD
	CALL CLSWAP
	LXI H,2
	DAD SP
	CALL CLSPIND
	LXI H,10
	LXI D,C32352+5
	CALL CLSGSIND
	CALL CCTOL
	CALL CLDIV
	LXI B,C32352+5
	CALL CLPUTM
	JMP C32356
C32357:
	RET
CLTROHEX:
	LXI H,8
	SHLD C32352+10
C32361:
	LHLD C32352+10
	DCX H
	SHLD C32352+10
	INX H
	LXI D,0
	CALL CCLT
	JZ C32362
	LHLD C32353
	PUSH H
	LHLD C32352+10
	LXI D,2
	CALL CCSASL
	XCHG
	LXI H,4
	DAD SP
	CALL CLGPIND
	CALL CLSASR
	LXI D,15
	CALL CCSTOL
	CALL CLAND
	POP D
	DAD D
	MOV E,M
	MVI D,0
	PUSH D
	CALL PUTCHAR
	POP B
	JMP C32361
C32362:
	RET
;
;
;	DSEG
C32352:
	DS   12
;	CSEG
;
;
;
;
CFTROUT:
	MOV A,M
	INX H
	ANA A
	JNZ CFTROUT
	RET
;
;
;
CFTRCZ:
	XTHL
	PUSH PSW
;
	PUSH B
	PUSH D
	PUSH H
;
	CALL CCTASTEIN
;
	POP H
;
	LDA CCTRCV
	INR A
	JZ CFTRC3
	CALL CFTROUT
	POP D
	POP B
	POP PSW
	XTHL
	RET
;
CFTRC3:
	CALL CCTRTEXT
	JMP CFTRC4
;
CFTRC8:
	CALL CFTROUT
CFTRC4:
	POP D
	POP B
;
	POP PSW
	XTHL
;
;
	PUSH PSW
	LDA CCTRCV
	INR A
	JNZ CFTRC7
	PUSH B
	PUSH D
	PUSH H
	LXI H,CFPRIM+7
	MOV A,M
	ANA A
	JNZ CRTRA1
	MVI A,'0'
	CALL CCTRCCO
	JMP CRTRA2
CRTRA1:
	DCX H
	MOV A,M
	ANI 0F0H
	STA CFTRMI
	JZ CRTRA3
	MVI A,'-'
	CALL CCTRCCO
	CALL CFNEG
CRTRA3:
	MVI A,'0'
	CALL CCTRCCO
	MVI A,'.'
	CALL CCTRCCO
	LXI H,CFPRIM+6
	MOV A,M
	ADI '0'
	CALL CCTRCCO
	MVI A,6
	PUSH PSW
;
CRTRA4:
	DCX H
	MOV A,M
	RAR
	RAR
	RAR
	RAR
	ANI 0FH
	ADI '0'
	CALL CCTRCCO
	MOV A,M
	ANI 0FH
	ADI '0'
	CALL CCTRCCO
	POP PSW
	DCR A
	PUSH PSW
	JNZ CRTRA4
;
	POP PSW
	MVI A,'E'
	CALL CCTRCCO
	LDA CFPRIM+7
	MOV L,A
	MVI H,0FFH
	LXI B,80H
	DAD B
	PUSH H
	CALL CCTRODEC
	POP B
	LDA CFTRMI
	ANA A
	JZ CRTRA2
	CALL CFNEG
;
CRTRA2:
	POP H
	POP D
	POP B
CFTRC7:
	POP PSW
	RET
;
;	DSEG
;
CFTRMI:	DS 1
;	CSEG
;
