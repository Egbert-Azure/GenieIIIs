# Test Setup

### Different versions we tested

21 MB hardisk parameter for `SDLTRS64`:
`ALT-H`
- Cylinder count: 2460
- Head count : 4
- Sector Count: 36
>Note 02/24/23: change in sdltrs Hard Disk Management. The parameters ("Cylinder Count", "Head Count" and "Sector Count") of the "Disk" selected above are now displayed at the bottom. Also the "manual conversion" for "Sector Count" no longer has to be done: if there is e.g. "Head Count" 4 and "Sector Count" 9, it is now automatically converted to 36 (= "Head Count" * "Sector Count") when creating a new HDV file. So the "real" parameters can be entered.
>
### 1. Test

- g3s-holte-21.dmk
- Systemdisk for 1 HD, 21MB
- sdl2trs64.exe GIIIsHolte-HD21.t8c

``` console
disk0=g3s-holte-21.dmk
hard0=g3s-hard21-f1.hdv
romfile1=g3s_8501004_bootrom_2732.bin
```

>Result: works

### 2. Test

- g3s-holte-cd.dmk
- Systemdisk for 2 HD, 21MB, 12MB
- sdl2trs64.exe GenieIIIs-2HD-Feb22.t8c

``` console
disk0=g3s-holte-cd.dmk
hard0=g3s-hard21-f2.hdv
hard1=g3s-hard36
romfile1=g3s_8501004_bootrom_2732.bin
```

>Result: works

### 3. Test

- g3s-holte-4wd.dmk
- sdl2trs64.exe GenieIIIs-4HD-Feb22.t8c
- Systemdisk for 4 HD, each 21MB

``` console
disk0=g3s-holte-4wd.dmk
hard0=g3s-C21.hdv
hard1=g3s-D21.hdv
romfile1=g3s_8501004_bootrom_2732.bin
```

``` consol
A: RW, Space:        16k
C: RW, Space:     4,848k  
E: RW, Space:     4,848k
and all HDV-files have 22 MB (22671616 Bytes) 
```

>Result:    
copy to d: works, back to c: doesn't. Looks like WD controler limitations due the fact `maxdrive` is `3` in `driver.mac`
