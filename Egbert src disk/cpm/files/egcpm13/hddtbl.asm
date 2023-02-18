;Um den richtigen DPB in DISKIO1.MAC einzubauen,mu~ er zun{chst
;mit XCPM3.LIB errechnet werden. Dies besorgt RMAC.
;
;  RMAC hddtbl $pz sz
;
;	Dann m}ssen nach dem Linken die Bytes herausgesucht
;	werden und von Hand in DISKIO1.MAC eingetragen
;	werden.
;	Die Bytefolge aus dem .COM File sieht folgenderma~en aus:
;
;	10 01 05 1f 01 30 0a ff 07 ff ff 00 80 00 00 02 03 ; Partition 0
;	10 01 05 1f 01 39 0a ff 07 ff ff 00 80 33 01 02 03 ; Partition 1
;
;	Dies ist die Version f}r Egbert Schr|er.
;	Platte mit 	4 K|pfen
;			615 Zylindern
;			17 Sektoren/Track
;			
;	Da dein Booteprom nicht von der Harddisk booten kann,
;	fallen die Bootspuren des ersten logischen Laufwerkes weg.
;
;	Die Aufteilung sieht physikalisch folgenderma~en aus:
;
;	 	 
;
;
 
     title 'DPH f}r Harddisk 21.40 MByte'


	maclib	xcpm3


hdsk0	dph	0,hddph0


hddph0	hdpb	512,68,615,4096,2048,0,8000h	;partition 0

;		 !  !   !   !    !    !	   !		
;		 !  !   !   !    !    !	   ------ Flag f}r Non-Removal-Medium	
;		 !  !   !   !    !    ----------- reserv. Systemspuren	
;    		 !  !   !   !    ---------------- 2048 Directory Eintr{ge  
;		 !  !   !   --------------------- 4 KB Blockgr|~e
;		 !  !   ------------------------- Anzahl Zylinder des Laufwerkes
;		 !  ----------------------------- Sektoren/Zylinder (4 K|pfe               
;		 !				  * 17 Sektoren)
;		 -------------------------------- Bytes / Sektor
;
;
	end
	