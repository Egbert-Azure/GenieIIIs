
/* BIOS-Aufruf fuer CP/M Plus ueber die BDOS-Funktion 50 */
/* BIOS-Call with BDOS-Function 50                       */
  

#include <cpm.h>

unsigned bios(uchar nummer, unsigned BC, unsigned DE, unsigned HL, uchar A)
{
  struct {
           uchar     fktnr;
           uchar     Akku;
           unsigned  BC_reg;
           unsigned  DE_reg;
           unsigned  HL_reg;
           } bios_PB;

  bios_PB.fktnr  = nummer;
  bios_PB.Akku   = A;
  bios_PB.BC_reg = BC;
  bios_PB.DE_reg = DE;
  bios_PB.HL_reg = HL;
  return(bdos(50,&bios_PB);
}

unsigned bioshl(uchar nummer, unsigned BC, unsigned DE, unsigned HL, uchar A)
{
  struct {
           uchar     fktnr;
           uchar     Akku;
           unsigned  BC_reg;
           unsigned  DE_reg;
           unsigned  HL_reg;
           } bios_PB;

  bios_PB.fktnr  = nummer;
  bios_PB.Akku   = A;
  bios_PB.BC_reg = BC;
  bios_PB.DE_reg = DE;
  bios_PB.HL_reg = HL;
  return(bdoshl(50,&bios_PB);
}

