;Um den richtigen DPB in DISKIO1.MAC einzubauen,mu~ er zun{chst
;mit XCPM3.LIB errechnet werden. Dies besorgt RMAC.
;
;  RMAC hddtbl $pz sz
;
;
 
     title 'DPH f}r Harddisk 10 MByte'


	maclib	xcpm3


hdsk0	dph	0,hddph0



hddph0	hdpb	512,34,611,4096,2048,2,8000h


	end
	