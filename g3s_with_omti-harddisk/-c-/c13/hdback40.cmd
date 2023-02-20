;; HDBACK40 aliases, by Rick Charnes
;;
;; incorporate the following alias, which in turn calls STARTUP.ZEX,
;; into your startup routine:
start		dw;zex start %day %month $rf02 $dy $rf03:$dn $da
;;
;; BACKUP alias for HDBACK40:
backup		$tp1:;reg s2 $dd q;go s3 $dc q; backup2
backup2		rs echo last time $td1$tu1: backed up: %budat$td1$tu1$$|zex backup budat$hb %day, %month $rf02, 19$dy $rf03:$dn $da $1
;;
;; If your terminal turns reverse video on and off with ^G4 and ^G0
;; then use the following along with LASTBACK.COM instead of the 
;; BACKUP2 alias:
backup-qume	lastback $td1$tu1: %budat$td1$tu1$$|zex backup budat$hb %day, %month $rf02, 19$dy $rf03:$dn $da $1
;;
;; The following three aliases are used with the ZEX script when PPIP19
;; detects a disk full in the backup disk or if the user aborts:
;;
inputera	$"^j^mEnter files to erase as a filelist:  "if ~nu $'l1;erase $'l1;sp;else;echo ^m^j n%>othing entered!;fi
input		$z$"^m^jEnter penultimate command ^[) ('EXIT' to exit ZEX completely):  ^[("$zif eq $'e1 exit;echo ^m^jr%>eturning to %<zcpr3...;rs %curdu;poke $+m0008 0;else;$'l1;fi
inputret	$z$"^m^jEnter final command ^[)('EXIT' to exit ZEX completely):  ^[("$zfi;if eq $'e1 exit;echo ^m^ja%>borting %<zex %>script...;rs %curdu;poke $+m0008 0;else;$'l1;rs e r%>eturning to %<zex %>script at letter%< %$rf05...;hold 2;fi
