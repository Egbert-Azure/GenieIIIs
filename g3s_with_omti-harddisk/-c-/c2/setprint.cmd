***** setprint.cmd ******* for use with db2shell.cmd **********
STORE chr(27) TO xesc
STORE chr(30) TO x10
STORE chr(31) TO xdbl
STORE chr(29) TO x17
STORE chr(28) TO x12
STORE chr(24) TO xcan
STORE xesc+chr(49) TO xcorr
STORE xesc+chr(72) TO xenh
STORE xesc+chr(84) TO xemph
STORE xesc+chr(73) TO xstop
STORE xesc+chr(54) TO x6line
STORE xesc+chr(56) TO x8line
STORE xesc+chr(48) TO xdata
STORE t TO okisw
SET PRINT ON
?? xcan
SET PRINT OFF
DO WHILE okisw
  ERASE
  SET CONSOLE ON
  @ 1,12 SAY hl
  @ 2,15 SAY ' *** CONFIGURE THE OKIDATA ML92 PRINTER *** '
  @ 4,0 SAY '1. 5 CPI 40 CPL'
  @ 4,40 SAY '2. 6 CPI 48 CPL'
  @ 5,0 SAY '3. 8.5 CPI 66 CPL'
  @ 5,40 SAY '4. 10 CPI 80 CPL'
  @ 6,0 SAY '5. 12 CPI 96 CPL'
  @ 6,40 SAY '6. 17 CPI 132 CPL'
  @ 7,0 SAY '7. Data Processing Mode'
  @ 7,40 SAY '8. Correspondence Mode'
  @ 8,0 SAY '9. Enhanced Mode'
  @ 8,40 SAY 'A. Emphasized Mode'
  @ 9,0 SAY 'B. Cancel Emphasized/Enhanced'
  @ 9,40 SAY 'C.  Clear to default settings'
  @ 10,0 SAY 'D. 6 LPI'
  @ 10,40 SAY 'E. 8 LPI'
  @ 11,0 SAY 'F. Send Line Feed to printer'
  @ 11,40 SAY 'G. Send Form Feed to printer'
  @ 16,28 SAY '**** ENTER OPTION ****'
  @ 18,18 SAY "Make selections at will, <RETURN> to exit"
  WAIT TO option
  STORE !(option) TO option
  SET PRINT ON
  SET CONSOLE OFF
  DO CASE
    CASE option = '1'
      ?? x10+xdbl
    CASE option = '2'
      ?? x12+xdbl
    CASE option = '3'
      ?? x17+xdbl
    CASE option = '4'
      ?? x10
    CASE option = '5'
      ?? x12
    CASE option = '6'
      ?? x17
    CASE option = '7'
      ?? xdata
    CASE option = '8'
      ?? xcorr
    CASE option = '9'
      ?? xenh
    CASE option = 'A'
      ?? xemph
    CASE option = 'B'
      ?? xstop
    CASE option = 'C'
      ?? xcan
    CASE option = 'D'
      ?? x6line
    CASE option = 'E'
      ?? x8line
    CASE option = 'F'
      ?? chr(10)
    CASE option = 'G'
      ?? chr(12)
    OTHERWISE
      STORE f TO okisw
  ENDCASE
  SET CONSOLE ON
  SET PRINT OFF
ENDDO WHILE okisw
RELEASE xesc, x10, x12, x17, xcan, xokisw, xcan, x6line, x8line, xstop, xcorr
RELEASE xdata, xenh, xemph, xdbl
?
SET PRINT OFF
SET CONSOLE ON
RETURN
