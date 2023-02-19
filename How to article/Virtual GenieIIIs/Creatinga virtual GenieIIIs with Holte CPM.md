# Creating a "Virtual Genie IIIs with CP/M 3.0 #

> Note: First Draft

The old DISKIO.MAC, modified for 21 MB, is in the DMK file, HD2.MAC is not needed (those were your adaptations for the OMTI controller). To generate a new CP/M 3, there are BOOTGEN.SUB and CPM3.SUB. A system disk can be created using KOPIER.COM in Holte-CP/M. The (here link) DMK image is specified as the "boot drive":

``` consol
sdltrs -disk0 g3s-holte-21.dmk
```

Then create a "Hard Disk" image (Alt-H) with the following parameters in `sdltrs`:

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
