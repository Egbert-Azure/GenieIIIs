# Creating a "Virtual Genie IIIs with CP/M 3.0 #

> Note: First Draft

Das alte, für 21 MB geänderte, DISKIO.MAC ist in dem DMK, HD2.MAC braucht man nicht (das waren ja euere Anpassungen für den OMTI-Controller). Um ein neues CP/M 3 zu generieren, gibt es BOOTGEN.SUB und CPM3.SUB. Eine System-Disk kann man über KOPIER.COM im Holte-CP/M erstellen. Das obige DMK-Image wird als "Boot-Laufwerk" angegeben:

... dann erstellt man ein "Hard Disk"-Image (Alt-H) mit den Parametern:

``` consol
sdltrs -disk0 g3s-holte-21.dmk
```

![image](https://user-images.githubusercontent.com/55332675/219958640-1cf13e8e-c9d4-4292-b11b-0bffc0cffedc.png)


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
