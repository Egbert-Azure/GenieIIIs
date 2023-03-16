#ifndef _TRS_CP500_H
#define _TRS_CP500_H

extern void   cp500_reset_mode();
extern Uint8  cp500_switch_mode(int mode);
extern Uint8  cp500_mem_read(int address, int mem_map, Uint8 *rom, Uint8 *ram);
extern void   cp500_mem_write(int address, Uint8 value, int mem_map, Uint8 *ram);
extern Uint8 *cp500_mem_addr(int address, int mem_map, Uint8 *rom, Uint8 *ram, int writing);

#endif
