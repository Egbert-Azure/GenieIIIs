# D I S K   H A N D L E R #

## Angepasste D I S K I O  *  C P M S Y S 4 f ##

### Anpassung

### disk egcpm33

1. Die Harddiskroutine fuer `10MByte Platte` mit 2 Koepfen und 612 Cylindern wird jetzt eingebunden. Die Routinen sind die BIOS Sources von Helmut Bernhardt aus `HD2.MAC`. Der `DPB` wurde mit `XCPM3.LIB` aus `HDDTBL.ASM` errechnet.
2. Die `RAM-Floppy` hat 770Kbyte Speicher bekommen, um den 1 MB Speicher Umbau zu unterstuetzen. Deshalb wurde die XMOVE-Funktion in DRIVER.MAC zusaeztlich geandert
3. Am 21.12.93 auf `Seagate ST 225` mit 2 Partitionen a 10.4 MB eingestellt und Ramdisk auf 55K reduziert zum Betrieb ohne 1MB Erweiterung

``` as
;******************************************************************************
;*  D I S K I O  *  C P M S Y S 4 f  * T h o m a s   H o l t e * 8 5 0 9 2 5 *
;******************************************************************************
;*                                                                            *
;*       D I S K   H A N D L E R                                              *
;*       =======================                                              *
;*                                                                            *
;*                                                                            *
;*  Thomas Holte       Version 1.0                                            *
;*                                                                            *
;******************************************************************************

;------------------------------------------------------------------------------
; Anmerkungen:
;
; Aus verschiedenen Versionen zusammengelegt und Ansteuerung f}r Hard-Disk
; entfernt um Pseudo-Laufwerke zur Formatkonvertirung einzubinden. In den
; Originaldateien  DISKIO.MAC, TABLES.MAC und DRIVER.MAC sind dir Werte f}r
; das High-Density Format 'FRITZ' als Standartwerte einzutragen. Zur
; Portabilitaet waere eine identische Formatdefinition von Vorteil. Beim HD
; Format habe ich die Formatgr|~e durch Versuche festgelegt. Die Systemspur
; beinhaltet auf/f}r meine CPU 280 eine Formatangabe, so da~ das Format auf
; meinem anderen Rechner automatisch erkannt wird. Da nicht von High Density
; gebootet wird k|nnte man die Systemspur auch entfallen lassen. (siehe vorher).
;
; Fritz Chwolka, Saarstr.34, 5173 Aldenhoven (02464)8920
;
; Hier m|chte ich mich noch f}r das Entgegenkommen des Herrn Holte bedanken,
; welcher mir seine Originaldisketten zur Systemerstellung vertrauensvoll
; zusandte, wobei ich anschlie~end das Systembios bei Herrn Holte erstandt.
; Das BIOS darf unter beibehalten der COPYRIGHT - Meldung des Herrn HOLTE
; weitergegeben werden.
; Die Copyrechte der Firma Digital Research betreff dem CPM+ , hier speziell
; der ???????.SPR Dateien ist von obigen Aussagen unbeeintr{chtigt. Sie
; sollten auf jeden Fall eine Systemdiskette von Digital Research erworben
; haben, um so als Lizensnehmer die SPR-Systemdateien der Firma Digital
; Research zu nutzen.
; Bei Problemen oder Fragen bin ich unter obiger Adresse immer erreichbar und
; freue mich f}r jede Zuschrift.
;
; 1989 F.Chwolka
;
; Zus{tzlich rumgepfuscht hat Volker Dose. Zum einen um eine Harddisk
; mit OMTI-Controller lesen zu k|nnen. 
; Die Harddiskroutine f}r 10MByte Platte mit 2 K|pfen und 612 Cylindern
; wird jetzt eingebunden. Die Routinen sind die BIOS Sources von
; Helmut Bernhardt aus HD2.MAC. Der DPB wurde mit XCPM3.LIB aus 
; HDDTBL.ASM errechnet.
; Ausserdem soll die RAM-Floppy 770Kbyte Speicher bekommen, da bei mir
; der Helmutsche 1MB-Umbau eingebaut ist. Daf}r war es notwendig die
; XMOVE-Funktion in DRIVER.MAC zu {ndern.
;
; Au~erdem noch verschieden Kleinigkeiten in DRIVER.MAC
;
; Jetzt ist ein 8 Zoll Laufwerk als logisches Laufwerk E: eingetragen.
; Format IBM 3740 Standard 77 Tr. SS SD
; Zur Zeit ist LW E: wieder raus.
;
; Die Benutzung des IX-Registers in den Routinen zum Lesen und Schreiben
; vom/auf das logische Laufwerk P: vertr{gt sich nicht mit dem ZPM3, welchem
; ich den Vorzug gebe. IX ist also durch IY vertauscht worden.
; Das neueste ist jetzt die Aufteilung der HardDisk in zwei logische
; Laufwerke C: und D:.     19.6.93
;
; Es ist jetzt eine Tandon Festplatte mit 20 MB Kapazit{t eingebaut, also
; Einteilung in C: mit 7 MB und D: mit 14 MB.
;       15.9.93
; Auf Seagate ST 225 mit 2 Partitionen a 10.4 MB eingestellt.
; Ramdisk auf 55K
;       21.12.93
;-----------------------------------------------------------------------------


SBANK equ 3 ;Erste Bank, die von der RAMDisk benutzt wird

 .Z80

 TITLE 'CP/M 3 DISKETTE HANDLER'

 DSEG

;Disk drive dispatching tables for linked BIOS
;GLOBAL DS0,MF0,MF1,FD0,IBMPC,KDS,RAM,RAIR,ALPHAP3,DRIVEP

 GLOBAL MF0,MF1,DS0,DS1,RAM,DRIVEP

;Variables containing parameters passed by BDOS
 EXTERNAL @DTBL,@ADRV,@RDRV,@DMA,@TRK,@SECT,@DBNK,@CBNK

;System control block variables
 EXTERNAL @ERDME  ; BDOS error mode

 EXTERNAL ?PMSG  ; Print message ^HL up to 0, saves reg. BC & DE
 EXTERNAL ?PDERR  ; Print BIOS disk error header
 EXTERNAL ?CONIN,?CONO ; Con in and out
 EXTERNAL ?CONST  ; Get console status

 EXTERNAL ?BANK,?USERF
;Harddiskroutinen aus HD2.MAC

        external hdwrit,hdread,hdlogi,hdinit,hd1ini

 
;ASCII control codes:
SUB EQU 1AH  ; Substitute
ESC EQU 1BH  ; Escape



;**Extended Disk Parameter Headers (XDPHs)


;Double Density
 DEFW FD$WRITE
 DEFW FD$READ
 DEFW FD$LOGIN
 DEFW FD$INIT
 DEFB 0  ; Relative drive zero
 DEFB 0  ; Drive type
    ; 0 = floppy disk
    ; 1 = floppy disk (special format)
    ; 2 = Winchester
    ; 3 = Winchester (cartridge)
    ; 4 = RAM disk
MF0: DEFW 0  ; No translation table
 DEFW 0,0,0,0  ; BDOS scratch area
 DEFB 0,0  ; Media flag
 DEFW DPB01  ; Disk parameter block
 DEFW 0FFFEH,0FFFEH ; CSV, ALV, DIRBCB, DTABCB, HASH
 DEFW 0FFFEH,0FFFEH ; Alloc'd by GENCPM
 DEFW 0FFFEH
 DEFB 0  ; Hash bank

 DEFW FD$WRITE
 DEFW FD$READ
 DEFW FD$LOGIN
 DEFW FD$INIT
 DEFB 1,0  ; Relative drive one
MF1: DEFW 0  ; No translation table
 DEFW 0,0,0,0  ; BDOS scratch area
 DEFB 0,0  ; Media flag
 DEFW DPB01  ; Disk parameter block
 DEFW 0FFFEH,0FFFEH ; CSV, ALV, DIRBCB, DTABCB, HASH
 DEFW 0FFFEH,0FFFEH ; Alloc'd by GENCPM
 DEFW 0FFFEH
 DEFB 0  ; Hash bank

;8 Zoll Floppy Laufwerk
;
; DEFW FD$WRITE
; DEFW FD$READ
; DEFW FD$LOGIN
; DEFW FD$INIT
; DEFB 4,0  ; Relative drive four (first 8 inch)
;FD0: DEFW XLT3  ; translation table
; DEFW 0,0,0,0  ; BDOS scratch area
; DEFB 0,0  ; Media flag
; DEFW DPB3  ; Disk parameter block
; DEFW 0FFFEH,0FFFEH ; CSV, ALV, DIRBCB, DTABCB, HASH
; DEFW 0FFFEH,0FFFFH ; Alloc'd by GENCPM
; DEFW 0FFFEH
; DEFB 0  ; Hash bank



;EJECT
;RAM disk:
;--------:
 DEFW M$WRITE
 DEFW M$READ
 DEFW FD$LOGIN
 DEFW M$INIT0
 DEFB 0,4
RAM: DEFW 0  ; No translation table
 DEFW 0,0,0,0  ; BDOS scratch area
 DEFB 0,0  ; Media flag
 DEFW DPBC  ; Disk parameter block
 DEFW 0  ; No CSV
 DEFW 0FFFEH,0FFFEH ; ALV, DIRBCB alloc'd by GENCPM
 DEFW 0FFFFH  ; No data buffer
 DEFW 0FFFEH  ; HASH alloc'd by GENCPM
 DEFB 0  ; Hash bank

;
;XDPH f}r Harddisk Partition 1 C:
 DEFW hdwrit
 DEFW hdread
 DEFW FD$LOGIN
 DEFW hdinit
 DEFB 0  ;relative Drive zero
 DEFB 2  ;drive type
    ; 0 : floppy disk
    ; 1 : floppy disk (special format)
    ; 2 : Winchester
    ; 3 : Winchester (cartridge)
    ; 4 : Ramdisk
DS0: DEFW 0  ;no translation table
 DEFW 0,0,0,0  ;BDOS scratch area
 DEFB 0,0  ;media flag
 DEFW DPBHD1  ;disk parameter block
 DEFW 0  ;no CSV
 DEFW 0FFFEH,0FFFEH ;ALV, DIRBCB, DTABCB, HASH
 DEFW 0FFFEH,0FFFEH ;allocated by GENCPM
 DEFB 0  ;hash bank
       
;
;XDPH f}r Harddisk Partition 2 D:
 DEFW hdwrit
 DEFW hdread
 DEFW FD$LOGIN
 DEFW hdinit
 DEFB 1  ;relative Drive zero
 DEFB 2  ;drive type
    ; 0 : floppy disk
    ; 1 : floppy disk (special format)
    ; 2 : Winchester
    ; 3 : Winchester (cartridge)
    ; 4 : Ramdisk
DS1: DEFW 0  ;no translation table
 DEFW 0,0,0,0  ;BDOS scratch area
 DEFB 0,0  ;media flag
 DEFW DPBHD2  ;disk parameter block
 DEFW 0  ;no CSV
 DEFW 0FFFEH,0FFFEH ;ALV, DIRBCB, DTABCB, HASH
 DEFW 0FFFEH,0FFFEH ;allocated by GENCPM
 DEFB 0  ;hash bank
       



;----------------------------------------------------------------
; Formatdefinitionen:
; F}r Sonderformate wird Drive B als relatives laufwerk genutzt.
;----------------------------------------------------------------

;EJECT
;virtual disk drive P:
;--------------------:
 DEFW P$WRITE
 DEFW P$READ
 DEFW FD$LOGIN
 DEFW FD$INIT
 DEFB 1,1  ; Relative drive one
DRIVEP: DEFW XLTF        ; Translation table
 DEFW 0,0,0,0  ; BDOS scratch area
 DEFB 0,0  ; Media flag
 DEFW DPBF  ; Disk parameter block
 DEFW CSVF,ALVF ; Checksumm vector, allocation vector
 DEFW DIBCBH,DTBCBH ; BCB list header
 DEFW 0FFFEH  ; No HASH
 DEFB 0  ; Hash bank
PDCT: DEFB 01110000B
 DEFB 11000000B
 DEFB 2,10,80

;sector translation tables:
;Drive E:
;8" Drive 
;XLT3: defb 0,6,12,18,24,4,10,16,22,2,8,14,20
; defb 1,7,13,19,25,5,11,17,23,3,9,15,21


; Drive P:
XLTF: DEFB 0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16
 DEFB 17,18,19,20,21,22,23,24,25,26,27,28,29,30,31,32,33
 DEFB 34,35,36,37,38,39,40,41,42,43,44,45,46,47,48,49,50,51


;checksum vectors
CSVF:   DEFS    64

  
;allocation vectors:
ALVF: DEFS 100

;BCB list header:
DIBCBH: DEFW DIRBCB
DTBCBH: DEFW DTABCB

;directory control block:
DIRBCB: DEFB 0FFH
 DEFS 9
 DEFW DIRBUF
 DEFB 0
 DEFW 0

;data control block:
DTABCB: DEFB 0FFH
 DEFS 9
 DEFW DTABUF
 DEFB 0
 DEFW 0

;directory buffer:
DIRBUF: DEFS 1024

;data buffer:
DTABUF: DEFS 1024


;EJECT
 CSEG   ; DPB must be resident


;Disk Parameter Blocks (DPB)

;Double Density Holte:
;--------------------:
DPB01: DEFW 80  ; 128 byte records per track
 DEFB 4,15  ; Block shift and mask
 DEFB 0  ; Extent mask
 DEFW 389  ; Maximum block number
 DEFW 191  ; Maximum directory entry number
 DEFB 0E0H,0  ; Alloc vector for directory
 DEFW 48  ; Checksum size
 DEFW 2  ; Offset for system tracks
 DEFB 2,3  ; Physical sector size shift and mask

;8 Zoll IBM 3740 SS SD:
;---------------------:
;DPB3: DEFW 26  ; 128 byte records per track
; DEFB 3,7  ; Block shift and mask
; DEFB 0  ; Extent mask
; DEFW 242  ; Maximum block number
; DEFW 63  ; Maximum directory entry number
; DEFB 0C0H,0  ; Alloc vector for directory
; DEFW 16  ; Checksum size
; DEFW 2  ; Offset for system tracks
; DEFB 0,0  ; Physical sector size shift and mask


;RAMDISK:
;-------:
DPBC: DEFW 447  ; 128 byte records per track
 DEFB 3,7  ; Block shift and mask
 DEFB 0  ; Extent mask
 DEFW 55  ; Maximum block number(385 bei 770k, 330 wenn
    ; Bank 2 (bzw. bei 256k Memory 110/55 k)
    ; als Data-Buffer benutzt wird )
 DEFW 31  ; Maximum directory entry number
 DEFB 080H,0  ; Alloc vector for directory
 DEFW 8000H  ; Checksum size
 DEFW 0  ; Offset for system tracks
 DEFB 0,0  ; Physical sector size shift and mask

;VIRTUELLES DRIVE -P-:
;--------------------:
DPBF: DEFW 80  ; 128 byte records per track
 DEFB 5,31  ; Block shift and mask
 DEFB 3  ; Extent mask
 DEFW 191  ; Maximum block number
 DEFW 255  ; Maximum directory entry number
 DEFB 192,0  ; Alloc vector for directory
 DEFW 64  ; Checksumm size
 DEFW 3  ; Offset for system tracks
 DEFB 3,7  ; Physical sector size shift and mask

;:---------------------------------:
;:HARDISK 10MByte, Partition 1 C:  :
;:---------------------------------:
;:Seagate 21,4 MB Hartddisk        :
;:---------------------------------:

DPBHD1: DEFB   010H,001H   ;SPT= 136      128 Byte records/track
 DEFB 005H        ;BSH=   5      block shift factor  
        DEFB    01FH        ;BLM=  31      block mask   
 DEFB 001H        ;EXM=   1      extend mask  
        DEFB    01FH,00AH   ;DSM=2591    maximum block number 
 DEFB 0FFH,003H   ;DRM=1024      maximum directory number
 DEFB 0FFH        ;AL0= 255      allocation vector 0
        DEFB    000H        ;AL1= 000      allocation vector 1
 DEFB 000H,080H   ;CKS=   0      checksum size is zero
                            ;              ,permanently mounted
 DEFB 002H,000H   ;OFF=   2      reserved tracks 
  DEFB 002H        ;PSH=   2      physical sector shift
 DEFB 003H        ;PHM=   3      physical record mask 

;das hei~t auf Deutsch:
;   512 Bytes/Sector
;    68 Sectoren/Zylinder (2 K|pfe*17 Sec/Cyl)
;   615 Cylinder/Laufwerk
;   4KB Blockgr|~e
;         2048 Directory Eintr{ge
;     2 Reservierte Systemspur
;        8000h Flag f}r nonremovable medium    

;:------------------------------------:
;:HARDISK 10MByte Partition 2 D:      :
;:------------------------------------:
DPBHD2: DEFB   010H,001H   ;SPT= 136      128 Byte records/track
 DEFB 005H        ;BSH=   5      block shift factor  
        DEFB    01FH        ;BLM=  31      block mask   
 DEFB 001H        ;EXM=   1      extend mask  
        DEFB    039H,00AH   ;DSM=2617      maximum block number 
 DEFB 0FFH,003H   ;DRM=1024      maximum directory number
 DEFB 0FFH        ;AL0= 255      allocation vector 0
        DEFB    000H        ;AL1= 000      allocation vector 1
 DEFB 000H,080H   ;CKS=   0      checksum size is zero
                            ;              ,permanently mounted
 DEFB 033H,001H   ;OFF= 307      reserved tracks 
  DEFB 002H        ;PSH=   2      physical sector shift
 DEFB 003H        ;PHM=   3      physical record mask 

;das hei~t auf Deutsch:
;   512 Bytes/Sector
;    68 Sectoren/Zylinder (2 K|pfe*17 Sec/Cyl)
;   615 Cylinder/Laufwerk
;   4KB Blockgr|~e
;         2048 Directory Eintr{ge
;   307 Reservierte Systemspuren
;        8000h Flag f}r nonremovable medium    


 

          DSEG

;EJECT
;RAM disk init routine:
;---------------------:
POWUP EQU 111DH  ; Power up/reset marker

M$INIT0:LD A,(POWUP) ; Power up ?
 OR A
 RET Z

;clear RAM disk:
;modfied for 256k ram
;
 LD C,32  ; Entry count   --> reg. C
 LD DE,128  ; ^first directory entry --> reg. DE
CLRNXT: PUSH BC  ; Save remaining entry count
 PUSH DE  ; Save ^directory entry
 LD HL,CLRBYT ; ^E5     --> reg. HL
 LD BC,1 SHL 8+15 ; Byte count    --> reg. B
    ; Function #    --> reg. C
 LD A,SBANK  ; Source      bank # = 0
    ; Destination bank # = 2
 CALL ?USERF  ; Clear current entry
 POP HL  ; ^current directory entry --> reg. HL
 LD DE,32  ; Offset to next entry    --> reg. DE
 ADD HL,DE  ; Add offset
 EX DE,HL  ; ^next directory entry    --> reg. DE
 POP BC  ; Restore entry count
 DEC C  ; Decrement it
 JR NZ,CLRNXT ; Clear next entry
 RET
CLRBYT: DEFB 0E5H
          


;EJECT

          DSEG


;EJECT
;disk read routine:
FD$READ:LD A,(@SECT) ; Sector number  --> reg. B
 LD B,A
 LD A,(@TRK) ; Track number  --> reg. E
 LD E,A
FD$REA1:LD A,(@RDRV) ; Relative drive number --> reg. C
 LD C,A
 LD A,(@DBNK) ; DMA bank --> accu (upper nibble)
 SLA A
 SLA A
 SLA A
 SLA A
 OR C  ; Set drive number
 LD HL,(@DMA) ; User buffer address --> reg. HL
 LD C,11  ; Function number --> reg. C
 CALL ?USERF  ; Read physical disk sector
 LD (DISK$STATUS),A ; Save status for error messages
 OR A  ; Any errors ?
 JP NZ,RDERR
 RET

;suppress error message if BDOS is returning errors to application ...
RDERR: LD A,(@ERDME)
 INC A
 JR Z,R$HARD$ERROR

;had permanent error, print message like:
; BIOS Error on d: T-nn, S-nn, <type>, Retry ?
 CALL ?PDERR  ; Print message header
 LD HL,(DISK$STATUS) ; Get status byte from last error
 LD H,0
 DEC L
 ADD HL,HL  ; Make byte offset
 LD BC,R$ERROR$TABLE ; Point at table of message addresses
 ADD HL,BC
 LD A,(HL)  ; Get specific message address
 INC HL
 LD H,(HL)
 LD L,A
 CALL ?PMSG  ; Print message
 LD HL,ERROR$MSG ; Print "<BEL>, Retry (Y/N) ? "
 CALL ?PMSG
 CALL U$CONIN$ECHO ; Get operator response
 LD C,A  ; Save response
 LD HL,MSG$END ; Disable status line
 CALL ?PMSG
 LD A,C  ; Restore response
 CP 'Y'  ; Yes, then retry 10 more times
 JP Z,FD$READ
R$HARD$ERROR:   ; Otherwise,
 LD A,1  ; Return hard error to BDOS
 RET


;EJECT
;disk write routine:
FD$WRITE:
 LD A,(@SECT) ; Sector number  --> reg. B
 LD B,A
 LD A,(@TRK) ; Track number  --> reg. E
 LD E,A
FD$WRI1:LD A,(@RDRV) ; Relative drive number --> reg. C
 LD C,A
 LD A,(@DBNK) ; DMA bank --> accu (upper nibble)
 SLA A
 SLA A
 SLA A
 SLA A
 OR C  ; Set drive number
 LD HL,(@DMA) ; User buffer address --> reg. HL
 LD C,12  ; Function number --> reg. C
 CALL ?USERF  ; Write physical disk sector
 LD (DISK$STATUS),A ; Save status for error messages
 OR A  ; Any errors ?
 JP NZ,WRERR
 RET

;suppress error message if BDOS is returning errors to application ...
WRERR: LD A,(@ERDME)
 INC A
 JR Z,W$HARD$ERROR

;had permanent error, print message like:
; BIOS Error on d: T-nn, S-nn, <type>, Retry ?
 CALL ?PDERR  ; Print message header
 LD HL,(DISK$STATUS) ; Get status byte from last error
 LD H,0
 DEC L
 ADD HL,HL  ; Make byte offset
 LD BC,W$ERROR$TABLE ; Point at table of message addresses
 ADD HL,BC
 LD A,(HL)  ; Get specific message address
 INC HL
 LD H,(HL)
 LD L,A
 CALL ?PMSG  ; Print message
 LD HL,ERROR$MSG ; Print "<BEL>, Retry (Y/N) ? "
 CALL ?PMSG
 CALL U$CONIN$ECHO ; Get operator response
 LD C,A  ; Save response
 LD HL,MSG$END ; Disable status line
 CALL ?PMSG
 LD A,C  ; Restore response
 CP 'Y'  ; Yes, then retry 10 more times
 JP Z,FD$WRITE
W$HARD$ERROR:   ; Otherwise,
 LD A,(DISK$STATUS) ; Return hard error to BDOS
 CP 5  ; Diskette write protected ?
 LD A,1  ; Common error code
 RET NZ
 INC A

FD$LOGIN:
FD$INIT:
 RET

U$CONIN$ECHO:   ; Get console input, echo it, and shift to
    ; Upper case
 CALL ?CONST  ; See if any character already struck
 OR A
 JR Z,U$C1
 CALL ?CONIN  ; Yes, eat and try again
 JR U$CONIN$ECHO
U$C1: CALL ?CONIN
 PUSH AF
 LD C,A
 CALL ?CONO
 POP AF
 CP 'A'
 RET C
 AND 0DFH  ; Make upper case
 RET

DISK$STATUS:
 DEFS 1  ; Last error status code for messages
;EJECT




;EJECT
;RAM disk I/O routines
M$READ: LD A,0FFH  ; Switch on read flag
 JR TASKM
M$WRITE:XOR A  ; Clear read flag
TASKM: LD (RDFLAG),A
 LD A,(@TRK) ; Track # --> accu
 ADD A,SBANK  ; Calc source bank #
 ADD A,A
 ADD A,A
 ADD A,A
 ADD A,A
 LD HL,@DBNK ; Get destination bank #
 ADD A,(HL)
 PUSH AF  ; Save bank numbers
 LD DE,(@DMA) ; DMA address --> reg. DE
 LD HL,(@SECT) ; Sector #    --> reg. HL
 INC HL  ; Adjust it
 LD B,7  ; Sector # * 128
 ADD HL,HL
 DJNZ $-1
 LD BC,128 SHL 8+15 ; Sector length --> reg. B
    ; Function # --> reg. C
 LD A,(RDFLAG) ; Read or write ?
 OR A
 JR NZ,TASKM1 ; Jump if read
 POP AF  ; Restore bank numbers
 RLCA   ; Swap bank numbers
 RLCA
 RLCA
 RLCA
 PUSH AF  ; Push bank numbers again
 EX DE,HL
TASKM1: POP AF  ; Restore bank numbers
 CALL ?USERF  ; Transfer "sector"
 XOR A
 RET
RDFLAG: DEFS 1




;EJECT
;------------------:
;virtual disk drive:
;
; Drive Controll Table mu~ f}r Konfig eine feste Adresse haben.

DCT EQU 10D7H  ; Drive control table (SYSTAB)

P$READ: LD IY,FD$READ ; Read routine vector    --> reg. IY
 JR P$TASK
P$WRITE:LD IY,FD$WRITE ; Write routine vector    --> reg. IY
P$TASK: LD A,(DRIVEP-2) ; Relative drive number    --> accu
 ADD A,A  ; *2
 LD C,A  ; Save it
 ADD A,A  ; *4
 ADD A,C  ; *6
 LD B,0  ; Relative drive # * 6    --> reg. BC
 LD C,A
 LD HL,DCT  ; ^drive control table    --> reg. HL
 ADD HL,BC  ; Calc real pointer
 PUSH HL  ; Save DCT pointer
 LD DE,TDCT  ; ^temporary storage area  --> reg. DE
 LD BC,5  ; Number of bytes    --> reg. BC
 LDIR   ; Save disk parameters of drive B:
 LD HL,PDCT  ; ^parameters for drive P: --> reg. HL
 POP DE  ; DCT pointer     --> reg. DE
 PUSH DE  ; Save DCT pointer again
 LD BC,5  ; Number of bytes    --> reg. BC
 LDIR   ; Load disk parameters of drive P:
 LD HL,P$RET ; Return address    --> reg. HL
 PUSH HL  ; Push it
 JP (IY)  ; Jump to driver
P$RET: LD HL,TDCT  ; Restore original disk parameters
 POP DE
 LD BC,5
 LDIR
 LD HL,PDCT+1 ; Get drive status
 SET 0,(HL)  ; Set init bit
 RET
TDCT: DEFS 5  ; Temporary storage area


;EJECT
;tables of pointers to error message strings
R$ERROR$TABLE:
 DEFW R1MSG
 DEFW R2MSG
 DEFW R3MSG
 DEFW R4MSG
 DEFW R5MSG
 DEFW R6MSG
 DEFW R7MSG
 DEFW R8MSG
 DEFW R9MSG

W$ERROR$TABLE:
 DEFW W1MSG
 DEFW W2MSG
 DEFW W3MSG
 DEFW W4MSG
 DEFW W5MSG
 DEFW W6MSG
 DEFW W7MSG
 DEFW W8MSG
 DEFW W9MSG

R1MSG: DEFM ' Illegal drive #,'
 DEFB 0
R2MSG: DEFM ' Track # too high,'
 DEFB 0
R3MSG: DEFM ' Sector # too high,'
 DEFB 0
R4MSG: DEFM ' Device not available,'
 DEFB 0
R5MSG: DEFB 0
R6MSG: DEFM ' Locked/deleted record,'
 DEFB 0
R7MSG: DEFM ' Data record not found,'
 DEFB 0
R8MSG: DEFM ' Parity error during read,'
 DEFB 0
R9MSG: DEFM ' Lost data during read,'
 DEFB 0

W1MSG: DEFM ' Illegal drive #,'
 DEFB 0
W2MSG: DEFM ' Track # too high,'
 DEFB 0
W3MSG: DEFM ' Sector # too high,'
 DEFB 0
W4MSG: DEFM ' Device not available,'
 DEFB 0
W5MSG: DEFM ' Write protected diskette,'
 DEFB 0
W6MSG: DEFM ' Write fault on disk drive,'
 DEFB 0
W7MSG: DEFM ' Data record not found,'
 DEFB 0
W8MSG: DEFM ' Parity error during write,'
 DEFB 0
W9MSG: DEFM ' Lost data during write,'
 DEFB 0

ERROR$MSG:
 DEFM ' Retry (Y/N) ? '
 DEFB 0

MSG$END:DEFB SUB,ESC,'D',0

 END
```
