# HOLTE.LIB for HITECH-C compiler

OPENPL.OBJ

      U _erase        D_openpl       U _system       U cret
      U indir         U ncsv
CLOSEPL.OBJ

     D_closepl      U _system       U cret          U indir
     U ncsv

CIRCLE.OBJ

      D_circle       U _system       U cret          U indir
      U ncsv

ARC.OBJ

    D_arc          U _system       U cret          U indir
    U ncsv

LINE.OBJ

    D_line         U _system       U cret          U indir
    U ncsv

POINT.OBJ

    D_point        U _system       U cret          U indir
    U ncsv

SYSTEM.OBJ

    D_system       U cret          U csv
    ERASE.OBJ       D _erase        U_system       U cret          U indir
    U ncsv

GRAFIK.OBJ

    D _cont         U_line         D _move         U_system
    D _unarc        D_uncircle     D _uncont       D_unline
    D _unpoint      U cret          U indir         U ncsv

UNARC.OBJ

    U_system       D _unarc        U cret          U indir
    U ncsv

UNPOINT.OBJ

     U_system       D _unpoint      U cret          U indir
     U ncsv

UNLINE.OBJ

      U_system       D _unline       U cret          U indir
      U ncsv

MOVE.OBJ

      D_cont         U _line         D_move         D _uncont
      U _unline       U cret          U indir         U ncsv

UNCIRCLE.OBJ

  U _system       D_uncircle     U cret          U indir
  U ncsv

BIOS3.OBJ

    U _bdos         U_bdoshl       D _bios         D_bioshl
    U cret          U indir         U ncsv

WINDOW.OBJ

    U __iob         U _bdos         U _fputc        D _keycode
    U _puts         U _system       D _window       U adiv
    U amod          U brelop        U cret          U indir
    U ncsv          U shal          U shar          U wrelop
