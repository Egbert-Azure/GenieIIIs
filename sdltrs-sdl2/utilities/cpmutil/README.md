# CP/M Utilities for the SDLTRS/XTRS Emulator

Roland Gerlach's CP/M Utilities source files converted for `Z80ASM` by SLR Systems.

Build the libraries:
--------------------
```sh
Z80ASM LIBCFN/M
Z80ASM LIBHEX/M
Z80ASM LIBHFN/M
Z80ASM LIBSTR/M
Z80ASM LIBXTRS/M
```

Build and link the utilities:
-----------------------------
```sh
Z80ASM EXPORT/M
Z80ASM IMPORT/M
Z80ASM XTRS/M
LINK EXPORT=EXPORT,LIBCFN,LIBHFN,LIBSTR,LIBXTRS
LINK IMPORT=IMPORT,LIBCFN,LIBHFN,LIBSTR,LIBXTRS
LINK XTRS=XTRS,LIBHEX,LIBHFN,LIBSTR,LIBXTRS
```
