(*
 * NewChain - Herbert Oppmann 1/93
 * Replacement Chain/Execute. Turbo Pascal Version 3.01A only!
 * Chain/Execute had to be removed from the run time library to make some
 * badly needed room for the patches.
 * Library support:
 *   a) If NCLib is <>0, then the file is supposed to be already open.
 *      NCLib sectors are read starting from the current position in the file.
 *   b) If NCLib is =0, the file is opened and read in whole.
 * Command line support:
 *      NCCmdLine is converted to uppercase and put to address $0080.
 *      The first two arguments are parsed and put in the DefaultFCB.
 * Load / start address:
 *      Chain: the file is always loaded behind the runtime library and
 *      started at $0100.
 *      Execute: load and start address are determined by NCStart. This
 *      feature allows the execution of CIM (non-standard executable) files.
 * ZCPR3 support:
 *      COM or CIM files starting with a ZCPR3 header get the address of
 *      the environment descriptor stored in the header.
 * Version control:
 *      The Turbo Pascal version used to create a CHN file is compared
 *      against the version used to compile this program. If there is
 *      no match, an I/O error CE is caused.
 *)

CONST
  NCLib : INTEGER = 0;
  NCStart : INTEGER = $0100;
VAR
  IORes : BYTE ABSOLUTE $00D0;
  NCCmdLine : STRING[127]; {only 126 characters usable!}

PROCEDURE ChkIORes; EXTERNAL $201C;

PROCEDURE Chain(VAR f); {any file}
BEGIN
InLine($18/$0A);
END;
{don't insert something here or your machine will probably crash!}

PROCEDURE Execute(VAR f); {any file}
VAR
  NCSecBuf : ARRAY[0..127] OF BYTE;
BEGIN
InLine (
$3E
/$AF
/$32/*+$0088
/$7E
/$E6/$0F
/$3E/$20
/$20/$5E
/$3A/$D8/$00
/$A7
/$3E/$21
/$28/$56
/$11/$0C/$00
/$19
/$22/*+$003A
/$1E/$0D
/$19
/$E5
/$2A/$06/$00
/$11/$38/$FF
/$19
/$22/*+$01DA
/$11/*+$022E
/$A7
/$ED/$52
/$DA/$A9/$20
/$3E/$FF
/$CD/*+$0039
/$32/*+$0203
/$32/*+$0178
/$32/*+$0185
/$E1
/$7E
/$32/*+$001F
/$E5
/$CB/$7F
/$CC/*+$0025
/$2A/NCLIB
/$7C
/$B5
/$20/$22
/$11/$00/$00
/$0E/$0F
/$CD/$05/$00
/$3C
/$20/$17
/$3E/$01
/$E1
/$F5
/$36/$00
/$3A/*+$01D7
/$CD/*+$0007
/$F1
/$32/$D0/$00
/$C9
/$5F
/$0E/$20
/$C3/$05/$00
/$11/NCSECBUF
/$0E/$1A
/$CD/$05/$00
/$ED/$5B/*+$FFD5
/$0E/$21
/$CD/$05/$00
/$B7
/$3E/$99
/$20/$D5
/$3E/$00
/$A7
/$20/$34
/$3E/$CE
/$2A/NCSECBUF+0
/$11/$31/$00
/$ED/$52
/$20/$C4
/$2A/NCSECBUF+2
/$11/$01/$21
/$ED/$52
/$20/$BA
/$2A/NCSECBUF+10
/$11/$65/$03
/$ED/$52
/$20/$B0
/$18/$3C
/$11/*+$000C
/$06/$05
/$1A
/$BE
/$C0
/$23
/$13
/$10/$F9
/$C9
/$5A/$33/$45/$4E/$56
/$21/$03/$01
/$CD/*+$FFEA
/$20/$22
/$2A/$09/$01
/$22/*+$0184
/$21/NCSECBUF+3
/$CD/*+$FFDC
/$20/$14
/$2A/$09/$01
/$7C
/$B5
/$28/$0D
/$7C
/$32/*+$013C
/$7D
/$32/*+$0135
/$3E/$01
/$32/*+$0123
/$AF
/$32/$80/$00
/$21/NCCMDLINE
/$3E/$7E
/$BE
/$38/$01
/$7E
/$23
/$EB
/$6F
/$26/$00
/$19
/$36/$1A
/$CD/*+$0099
/$28/$34
/$D5
/$21/$81/$00
/$36/$20
/$23
/$06/$01
/$1A
/$FE/$1A
/$28/$09
/$CD/$A7/$04
/$77
/$13
/$23
/$04
/$18/$F2
/$74
/$78
/$32/$80/$00
/$D1
/$CD/*+$00A1
/$D5
/$11/*+$007D
/$ED/$B0
/$D1
/$CD/*+$006D
/$28/$08
/$CD/*+$0092
/$11/*+$007F
/$ED/$B0
/$11/$5C/$00
/$21/*+$0067
/$01/$20/$00
/$ED/$B0
/$2A/*+$00BA
/$F9
/$EB
/$21/$80/$FF
/$19
/$22/*+$0088
/$21/$21/$00
/$19
/$22/*+$00C9
/$2A/*+$FEF9
/$01/$24/$00
/$ED/$B0
/$D5
/$21/$12/$00
/$19
/$22/*+$0078
/$21/$37/$00
/$19
/$22/*+$00A6
/$21/$74/$00
/$19
/$22/*+$00CE
/$3A/*+$FF13
/$A7
/$21/$00/$01
/$28/$03
/$2A/NCSTART
/$22/*+$00D2
/$21/*+$0050
/$01/$81/$00
/$ED/$B0
/$ED/$4B/NCLIB
/$ED/$5B/$01/$01
/$C8
/$ED/$5B/NCSTART
/$C9
/$1A
/$FE/$1A
/$C8
/$FE/$20
/$C0
/$13
/$18/$F6
/$00/$20/$20/$20/$20/$20/$20/$20/$20/$20/$20/$20/$00/$00/$00/$00
/$00/$20/$20/$20/$20/$20/$20/$20/$20/$20/$20/$20/$00/$00/$00/$00
/$CD/$EF/$03
/$21/$5C/$00
/$01/$10/$00
/$3A/$69/$00
/$3C
/$C0
/$0E/$0C
/$C9
/$21/$00/$00
/$ED/$52
/$30/$1B
/$11/$00/$00
/$0E/$09
/$CD/$05/$00
/$C3/$00/$00
/$4F/$75/$74/$20/$6F/$66/$20/$6D/$65/$6D/$6F/$72/$79/$0D/$0A/$24
/$C5
/$D5
/$0E/$1A
/$CD/$05/$00
/$11/$00/$00
/$0E/$21
/$CD/$05/$00
/$D1
/$C1
/$B7
/$20/$24
/$3E/$00
/$A7
/$28/$0D
/$AF
/$32/$00/$00
/$21/$09/$00
/$19
/$36/$00
/$23
/$36/$00
/$21/$00/$00
/$34
/$20/$02
/$23
/$34
/$21/$80/$00
/$19
/$EB
/$0B
/$78
/$B1
/$20/$A6
/$D5
/$11/$80/$00
/$0E/$1A
/$CD/$05/$00
/$1E/$00
/$0E/$20
/$CD/$05/$00
/$D1
/$21/$00/$00
/$A7
/$ED/$52
/$EB
/$06/$FF
/$70
/$23
/$1B
/$7A
/$B3
/$20/$F9
/$21/$00/$00
/$C3/$00/$00
)
END;
