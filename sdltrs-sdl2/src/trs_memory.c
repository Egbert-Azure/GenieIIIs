/*
 * Copyright (C) 1992 Clarendon Hill Software.
 *
 * Permission is granted to any individual or institution to use, copy,
 * or redistribute this software, provided this copyright notice is retained.
 *
 * This software is provided "as is" without any expressed or implied
 * warranty.  If this software brings on any sort of damage -- physical,
 * monetary, emotional, or brain -- too bad.  You've got no one to blame
 * but yourself.
 *
 * The software may be modified for your own purposes, but modified versions
 * must retain this notice.
 */

/*
 * Copyright (c) 1996-2020, Timothy P. Mann
 *
 * Permission is hereby granted, free of charge, to any person
 * obtaining a copy of this software and associated documentation
 * files (the "Software"), to deal in the Software without
 * restriction, including without limitation the rights to use, copy,
 * modify, merge, publish, distribute, sublicense, and/or sell copies
 * of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be
 * included in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
 * EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
 * MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
 * NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS
 * BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN
 * ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
 * CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
 * SOFTWARE.
 */

/*
 * trs_memory.c -- memory emulation functions for the TRS-80 emulator
 *
 * Routines in this file perform operations such as mem_read and mem_write,
 * and are the top level emulation points for memory-mapped devices such
 * as the screen and keyboard.
 */

/*#define MEMDEBUG 1*/

#include <stdlib.h>
#include <string.h>
#include "error.h"
#include "trs.h"
#include "trs_clones.h"
#include "trs_cp500.h"
#include "trs_disk.h"
#include "trs_memory.h"
#include "trs_imp_exp.h"
#include "trs_state_save.h"
#include "trs_uart.h"

#define MAX_ROM_SIZE       (18432)  /* Holmes VID-80 (VX-3) */
#define MAX_VIDEO_SIZE     (3072)   /* CP-500 M80 has 3K */
#define MAX_MEMORY_SIZE    (4 * 1024 * 1024) + 65536
/* Start MegaMem > 1MB to leave space for SuperMem or HyperMem */
#define MEGAMEM_START      (1 * 1024 * 1024) + 65536

/* Model I printer addresses 0x37E8 to 0x37EB */
#define PRINTER_1(addr)    (((addr) &~ 3) == 0x37E8)
/* Model III/4/4P printer addresses 0x37E8 and 0x37E9: Read-Only */
#define PRINTER_3(addr)    (((addr) &~ 1) == 0x37E8)

/* Interrupt latch register in EI (Model I) */
#define TRS_INTLATCH(addr) (((addr) &~ 3) == 0x37E0)

/* Check address in video memory */
#define VIDEO_ADDR(vaddr)  (Uint16)vaddr < MAX_VIDEO_SIZE

int lowercase = 1;
int trs_model = 1;
int trs_mem_size = Z80_ADDRESS_LIMIT;
int trs_rom_size;
int huffman;  /* Dave Huffman (4/4P) */
int hypermem; /* Anitek HyperMem (4/4P) */
int lubomir;  /* Lubomir Soft Banker */
int megamem;  /* Anitek MegaMem (III/4/4P) */
int selector; /* Selector (TRS-80 Model I) */
int supermem; /* Alpha Technology SuperMem */
int xmem80;   /* X-MEM/80 16K page Model I */

/* private data */

/* +1 so strings from mem_pointer are NUL-terminated */
static Uint8 memory[MAX_MEMORY_SIZE + 1];
static Uint8 video[MAX_VIDEO_SIZE + 1];
static Uint8 rom[MAX_ROM_SIZE + 1];
static int memory_map;
static int video_addr = -VIDEO_START;
static int bank_offset[3];
static Uint32 bank_base;
static int megamem_addr;
static Uint32 megamem_base;
static Uint8 mem_command;
static int supermem_base;
static Uint32 supermem_high;
static int sys_byte;   /* Various clones and 4P boot ROM */
static int sys_byte_2; /* LNW80 II and TCS Genie IIIs */

static Uint8 mem_video_read(int vaddr)
{
  if (VIDEO_ADDR(vaddr))
    return video[vaddr];

#if MEMDEBUG
  error("[PC=%04X] Reading video address %04X out of range (%04X)",
      Z80_PC, vaddr, MAX_VIDEO_SIZE);
#endif
  return 0xFF;
}

static int mem_video_write(int vaddr, Uint8 value)
{
  if (VIDEO_ADDR(vaddr)) {
    if (video[vaddr] != value) {
      video[vaddr] = value;
      return 1;
    } else {
      return 0;
    }
  }

#if MEMDEBUG
  error("[PC=%04X] Writing %02X to video address %04X out of range (%04X)",
      Z80_PC, value, vaddr, MAX_VIDEO_SIZE);
#endif
  return 0;
}

void mem_video_page(int offset)
{
  video_addr = -VIDEO_START + offset;
}

Uint8 mem_video_page_read(int vaddr)
{
  return mem_video_read(vaddr + video_addr);
}

int mem_video_page_write(int vaddr, Uint8 value)
{
  return mem_video_write(vaddr + video_addr, value);
}

Uint8 *mem_video_page_addr(int vaddr)
{
  vaddr = vaddr + video_addr;
  if (VIDEO_ADDR(vaddr))
    return video + vaddr;

#if MEMDEBUG
  error("[PC=%04X] Video page address %04X out of range (%04X)",
      Z80_PC, vaddr, MAX_VIDEO_SIZE);
#endif
  return NULL;
}

void mem_bank(int command)
{
  switch (command) {
    case 0:
      /* L64 Lower / Upper */
      bank_offset[0] = 0;
      bank_offset[1] = 0;
      break;
    case 2:
      /* L64 Lower / H64 Lower */
      bank_offset[0] = 0;
      bank_offset[1] = bank_base - 32768;
      break;
    case 3:
      /* L64 Lower / H64 upper */
      bank_offset[0] = 0;
      bank_offset[1] = bank_base;
      break;
    case 6:
      /* H64 Lower / L64 upper */
      bank_offset[0] = bank_base;
      bank_offset[1] = 0;
      break;
    case 7:
      /* H64 Upper / L64 Upper */
      bank_offset[0] = bank_base + 32768;
      bank_offset[1] = 0;
      break;
    default:
      error("unknown mem_bank command %d", command);
      break;
  }
  mem_command = command;
}

void ct80_ramdisk_out(int port, Uint8 byte)
{
  switch (port) {
    case 0:
      if (bank_base < MAX_MEMORY_SIZE)
        memory[bank_base] = byte;
      return;
    case 1:
      if (bank_base < MAX_MEMORY_SIZE - 1)
        memory[bank_base++] = byte;
      return;
    default:
      /* Port 2/3 = 0, 4/5 = 1, 6/7 = 2 */
      bank_offset[(port >> 1) - 1] = byte;
  }
  /* RAM disk starting at 64 KB */
  bank_base = (bank_offset[0]
            | (bank_offset[1] << 8)
            | (bank_offset[2] << 16))
            + 65536;
}

Uint8 ct80_ramdisk_in(int port)
{
  switch (port) {
    case 0:
      if (bank_base < MAX_MEMORY_SIZE)
        return memory[bank_base];
      break;
    case 1:
      if (bank_base < MAX_MEMORY_SIZE - 1)
        return memory[bank_base++];
      break;
    default:
      /* Port 2/3 = 0, 4/5 = 1, 6/7 = 2 */
      return bank_offset[(port >> 1) - 1];
  }
  return 0xFF; /* nonexistent memory address */
}

void ct80_video_addr(int address)
{
  if (video_addr != address) {
    int i;

    video_addr = address;
    /* Scroll Aster CT-80 80*25 video page one line up */
    for (i = 0; i < 0x800; i++)
      trs_screen_write_char(i, video[i] = video[i + 80]);
  }
}

void eg3200_init_out(int value)
{
  sys_byte = value;
  trs_clones_model(EG3200);
  trs_timer_init();
  trs_screen_init(0);
}

void eg3200_genieplus_out(int bits)
{
  /* Select 64K bank: 0 = System, 1 - 7 = Genieplus Card */
  bank_base = (bits & 0x07) << 16;
  if (bank_base) {
    /* Select lower or upper 32K of bank */
    bank_base += (bits & (1 << 3)) << 12;
  }
  bank_offset[0] = bank_base; /* Low 32K for banking */
}

void eg64_mba_out(int value)
{
  /* Disable EG-64 MBA */
  if (value == 7) {
    memory_map = 0x10;
    sys_byte = 0;
    return;
  }

  if (value & (1 << 3))
    /* RAM access */
    sys_byte |=  (1 << (value & 7));
  else
    /* ROM access */
    sys_byte &= ~(1 << (value & 7));

  memory_map = 0x18;
}

void genie3s_bank_out(int value)
{
  if (value == sys_byte_2)
    return;

  /* Select HRG page 0/1 */
  if ((value & (1 << 5)) != (sys_byte_2 & (1 << 5)))
    grafyx_write_y((value & (1 << 5)) << 4);

  /* Show HRG page 0/1 */
  if ((value & (1 << 4)) != (sys_byte_2 & (1 << 4)))
    grafyx_write_yoffset((value & (1 << 4)) << 5);

  /* Redraw if Font-SRAM is disabled */
  if ((value & (1 << 1)) == 0 && (sys_byte_2 & (1 << 1)))
    trs_screen_refresh();

  sys_byte_2 = value;

  /* Bit 7 : Bit 6 : Bank# : Memory
   *   0   :   0   :   0   :    64K
   *   0   :   1   :   1   :   128K
   *   1   :   0   :   2   :   192K
   *   1   :   1   :   3   :   256K
   */
  bank_base = (value & 0xC0) << 10;
}

void genie3s_init_out(int value)
{
  genie3s_bank_out(value);
  mem_video_page(0);
  trs_clones_model(GENIE3S);
  trs_timer_init();
}

void genie3s_mem_out(int bits)
{
  if (bits == mem_command)
    return;

  mem_command = bits;
  /* D0/D1: 256K block */
  bank_offset[0] = (bits & 0x03) << 18;
  /* D7: Common area */
  bank_offset[1] = (bits & 0x80) ? bank_offset[0] : 0;
}

void genie3s_sys_out(int value)
{
  if (value == sys_byte)
    return;

  /* HRG page on or off */
  if ((value & (1 << 1)) != (sys_byte & (1 << 1)))
    genie3s_hrg(value & (1 << 1));

  /* Slow-Down on ROM access */
  if ((value & (1 << 2)) != (sys_byte & (1 << 2)))
    trs_timer_speed(value & (1 << 2));

  /* Text screen on or off */
  if ((value & (1 << 7)) != (sys_byte & (1 << 7)))
    m6845_text(value & (1 << 7));

  sys_byte = value;
}

/*
 * Dave Huffman (and some other) memory expansions. These decode
 * port 0x94 off U50 as follows
 *
 * 7: only used with Z180 board (not emulated - would need Z180 emulation!)
 * 6: write protect - not emulated
 * 5: sometimes used for > 4MHz turbo mod
 * 4-0: Bits A20-A16 of the alt bank
 *
 * Set to 1 on a reset so that you get the 'classic' memory map
 * This port is read-write and the drivers depend upon it
 * (See RAMDV364.ASM)
 */
void huffman_out(int bits)
{
  bank_base = (bits & 0x1F) << 16;
  mem_bank(mem_command);
}

int huffman_in(void)
{
  return (bank_base >> 16) & 0x1F;
}

/*
 * The Hypermem is very similar and also changes the upper
 * 64K bank between multiple banks. However the values
 * are on port 0x90 (sound) bits 4-1, which is a much poorer
 * design IMHO as sound using apps can randomly change the
 * upper bank. Fine for a ramdisc but means other software
 * must take great care.
 */
void hypermem_out(int bits)
{
  /* HyperMem replaces the upper 64K bank with multiple
     banks according to port 0x90 bits 4-1.
     0 base is upper bank of 64K */
  bank_base = ((bits & 0x1E) + 2) << 15;
  mem_bank(mem_command);
}

void lnw80_bank_out(int value)
{
  if (value == sys_byte_2)
    return;

  /* LNW80 II additional 32K RAM: banking overrides D5 */
  bank_offset[1] = (value & (1 << 6)) << 9;

  /* LNW80 II 80x24 text mode video page */
  if ((sys_byte & (1 << 1)) == 0) {
    mem_video_page((value & (1 << 3)) << 7);
    if ((value & (1 << 3)) && text80x24 == 0)
      trs_screen_80x24(1);
  }

  /* Disable or enable interrupts */
  if ((value & (1 << 2)) != (sys_byte_2 & (1 << 2)))
    z80_state.iff1 = z80_state.iff2 = ((value & (1 << 2)) == 0);

  sys_byte_2 = value;
}

void lubomir_out(int bits)
{
  /* 32K Expander-RAM */
  bank_offset[1] = (bits & (1 << 4)) << 11;

  memory_map = bits ? 0x19 : 0x10;
  sys_byte = bits;
}

void megamem_out(int mem_slot, Uint8 value)
{
  if (mem_slot == 0 && value == 0) {
    megamem_addr = 0;
    megamem_base = 0;
  } else {
    megamem_addr = (value & 0xC0) * 256;
    megamem_base = (((value - (value & 0xC0)) * 16)
                 + (mem_slot * 1024)) * 1024
                 + MEGAMEM_START;
  }
}

void s80z_out(int value)
{
  if ((value & (1 << 2)) != (sys_byte & (1 << 2))) {
    if (value & (1 << 2)) {
      if (speedup == 4)
        video_addr = 0x3900; /* Homebrew 80*22 SYS80.SYS */
      else
        video_addr = (value & (1 << 1)) ? 0xB000 : 0xF000;
    } else {
      video_addr = VIDEO_START;
    }
    trs_screen_mode(REVERSE, value & (1 << 6));
  }

  sys_byte = value;
  memory_map = 0x17;
}

void selector_out(int bits)
{
  /* Not all bits are necessarily really present but hey what
     you can't read back you can't tell */
  mem_command = bits;
  /* 0x10 is already the default tandy map we add 11-17 in the style
     of the model 4 approach */
  memory_map = 0x10 + (bits & 7);
  /* Set default memory map for Selector Modes 6 & 7 */
  if (memory_map >= 0x16) memory_map = 0x10;
  /* External RAM enabled */
  if (bits & 0x8) {
    /* Effectively the selector bits << 15 */
    /* Low 64K is the base memory */
    bank_base = 32768 + ((bits & 0xF0) << 11);
    /* Now are we mapping it high or low */
    if (bits & 1) /* Low */
      bank_base += 32768;
  } else
    bank_base = 0;
  /* Bank low on odd mode */
  if (bits & 1) {
    bank_offset[0] = bank_base;
    bank_offset[1] = 0;
  } else {
    bank_offset[0] = 0;
    bank_offset[1] = bank_base;
  }
}

void supermem_out(int bits)
{
  /* Support 1024 KB which is the full range of the mapping. */
  supermem_base = (bits & 0x1F) << 15;
  /* The supermem can flip the low or high 32K.
     Set bit 5 to map low */
  supermem_high = (bits & 0x20) ? 0x0000 : 0x8000;
}

int supermem_in(void)
{
  return (supermem_base >> 15) |
         ((supermem_high == 0) ? 0x20 : 0);
}

void sys_byte_out(int value)
{
  if (value == sys_byte)
    return;

  switch (memory_map) {
    case 0x17:
      /* Hack for Schmidtke-CP/M with Homebrew 80*22 SYS80.SYS */
      sys_byte = ((value & (1 << 4)) >> 1) | (sys_byte & 0x07);
      return;
    case 0x10:
    case 0x14:
      /* Banking-Modification from Martin Doppelbauer */
      memory_map = (value & (1 << 4)) ? 0x14 : 0x10;
      /* Fall through - Accept speed up kit on bit 0 */
    case 0x1C: /* Aster CT-80 */
      if ((value & (1 << 0)) != (sys_byte & (1 << 0)))
        trs_timer_speed(value & 1);
      break;
    case 0x1D: /* LNW80/II */
      if ((value & (1 << 7)) != (sys_byte & (1 << 7)))
        trs_screen_mode(INVERSE, value & (1 << 7));
      if ((value & (1 << 1)) != (sys_byte & (1 << 1)))
        hrg_onoff(value & (1 << 1));
      if ((value & (1 << 0)) != (sys_byte & (1 << 0))) {
        if ((value & 1) == 0)
          trs_screen_80x24(0);
        if (text80x24 == 0)
          trs_screen_mode(REVERSE, value & 1);
      }
      break;
    case 0x1E: /* TCS Genie IIs/SpeedMaster */
      if ((value & (1 << 1)) != (sys_byte & (1 << 1)))
        hrg_onoff(value & (1 << 1));
      if ((value & (1 << 2)) != (sys_byte & (1 << 2)))
        trs_timer_speed(value & (1 << 2));
      break;
  }

  sys_byte = value;
}

void sys_byte_write(int value)
{
  sys_byte = value;
}

int sys_byte_in(void)
{
  return sys_byte;
}

int sys_byte_2_in(void)
{
  return sys_byte_2;
}

void tcs_ram192b_out(int bits)
{
  /* TCS Genie IIs/SpeedMaster RAM 192 B */
  bank_base = ((bits & 0x0C) * 192) /* card */
            + ((bits & 0x30) *  48) /* block */
            * 1024 + 65536;
  mem_command = bits;
}

/* Holmes VID-80 (VX-3) */
void vid80_vx3_out(int bits)
{
  if (bits == sys_byte) return;
  if ((bits & (1 << 7)) != (sys_byte & (1 << 7)))
    trs_screen_mode(INVERSE, bits & (1 << 7));

  sys_byte = bits;
  memory_map = 0x34;
}

void xmem80_out(int port, int page)
{
  bank_offset[port - 0x41] = page << 14;
  if (port == 0x43)
    /* Set both 16K pages */
    bank_offset[0] = bank_offset[1] = bank_offset[2];

  memory_map = 0x16;
}

int xmem80_in(int port)
{
  return bank_offset[port - 0x41] >> 14;
}

static void mem_init(void)
{
  /* Initialize RAM & ROM */
  int i;

  for (i = 0; i < MAX_MEMORY_SIZE - 1;) {
    memory[i++] = 0xFF;
    memory[i++] = 0x00;
  }
  memset(&rom, 0xFF, MAX_ROM_SIZE);

  mem_map(0);
  mem_video_page(0);
  clear_key_queue(); /* init the key queue */
}

/* Handle reset button if poweron=0;
   handle hard reset or initial poweron if poweron=1 */
void trs_reset(int poweron)
{
  bank_base = 0x10000;
  bank_offset[0] = 0;
  bank_offset[1] = 0;
  bank_offset[2] = 0;
  megamem_addr = 0;
  megamem_base = 0;
  mem_command = 0;
  supermem_base = 0;
  supermem_high = 0x8000;
  sys_byte = sys_byte_2 = 0;
  trs_emu_mouse = 0;

  /* Close disks opened by Z80 programs */
  do_emt_resetdisk();
  /* Reset devices (Model I SYSRES, Model III/4 RESET) */
  trs_cassette_reset(poweron); /* Reset tape position on poweron */
  trs_disk_init(poweron); /* also inits trs_hard and trs_stringy */
  trs_uart_init();

  trs_kb_reset(); /* Part of keyboard stretch kludge */
  trs_cancel_event();
  trs_timer_interrupt(0);

  if (poweron || trs_model >= 3 || trs_clones.model & (CT80 | GENIE3S)) {
    if (poweron) {
      mem_init();
      trs_rom_init();
    }
    /* Blank Video */
    memset(&video, ' ', MAX_VIDEO_SIZE);
    m6845_crtc_reset();
    trs_clones_model(0);
    trs_screen_reset();
    trs_screen_init(1);
    trs_timer_init();

    if (trs_show_led) {
      trs_disk_led(-1, -1);
      trs_hard_led(-1, -1);
    }

    /* Reset processor */
    z80_reset();
  } else {
    /* Signal a nonmaskable interrupt. */
    trs_reset_button_interrupt(1);
    trs_schedule_event(trs_reset_button_interrupt, 0, 2000);
  }

  /* Set 64*16 video mode */
  trs_screen_80x24(0);

  if (trs_model == 5) {
    /* Switch in boot ROM */
    sys_byte = 1;
  }

  if (trs_model >= 4) {
    /* Turn off various memory map and video mode bits */
    z80_out(0x84, 0);
    if (huffman)
      bank_base = 0;
  }

  if (trs_model >= 3) {
    memory[0x405B] = 0; /* Clear Break Key Indicator */
    grafyx_write_mode(0);
    trs_interrupt_mask_write(0);
    trs_nmi_mask_write(0);
  }

  if (trs_model == 3) {
    grafyx_m3_reset();
    trs_clones_model(0);
  }

  if (trs_model == 1) {
    hrg_onoff(0); /* Switch off HRG1B hi-res graphics. */
    trs_interrupt_latch_clear();
    trs_screen_mode(INVERSE + REVERSE, 0);

    switch (speedup) {
      case 5: /* Aster CT-80 */
        m6845_text(80);
        trs_clones_model(CT80);
        trs_screen_init(0);
        break;
      case 6: /* LNW80/II */
        trs_clones_model(LNW80);
        break;
      case 7: /* TCS SpeedMaster 5.3 */
        trs_clones_model(SPEEDMASTER);
        trs_screen_init(0);
        break;
    }
  }
}

int get_mem_cmd(void)
{
  return mem_command;
}

int get_mem_map(void)
{
  return memory_map;
}

void mem_map(int which)
{
  memory_map = which + (trs_model << 4);
}

/*
 * hack to let us initialize the ROM memory
 */
void rom_write(int address, int value)
{
  if (address < MAX_ROM_SIZE)
    rom[address] = value;
}

static int trs80_model1_ram(int address)
{
  if (address >= trs_mem_size) return 0xFF; /* nonexistent memory address */

  if (selector) {
    int const bank = bank_offset[address >> 15];

    /* Selector mode 6 remaps RAM from 0000-3FFF to C000-FFFF while keeping
       the ROMs visible */
    if ((mem_command & 7) == 6 && address >= 0xC000) {
      /* Use the low 16K, and then bank it. I'm not 100% sure how the
         PAL orders the two */
      address &= 0x3FFF;
    }
    /* Deal with 32K banking from selector */
    address += bank;
  }

  return memory[address];
}

static int trs80_model1_mmio(int address)
{
  if (address >= VIDEO_START) return video[address - VIDEO_START];
  if (address < trs_rom_size) return rom[address];
  if (TRS_INTLATCH(address)) return trs_interrupt_latch_read();
  if (address == TRSDISK_DATA) return trs_disk_data_read();
  if (address == TRSDISK_STATUS) return trs_disk_status_read();
  if (address == TRSDISK_TRACK) return trs_disk_track_read();
  if (address == TRSDISK_SECTOR) return trs_disk_sector_read();
  /* With a selector 768 bytes poke through the hole */
  if (address >= 0x3900 && selector) return trs80_model1_ram(address);
  if (address >= KEYBOARD_START) return trs_kb_mem_read(address);
  if (PRINTER_1(address)) return trs_printer_read();
#if MEMDEBUG
  error("[PC=%04X] Invalid read of address %04X, returning FF, mem_map=%02X",
      Z80_PC, address, memory_map);
#endif
  return 0xFF;
}

int trs80_model3_mem_read(int address)
{
  if (address >= trs_mem_size) return 0xFF; /* nonexistent memory address */
  if (address >= RAM_START) return memory[address];
  if (address >= VIDEO_START) return grafyx_m3_read_byte(address - VIDEO_START);
  if (address >= KEYBOARD_START) return trs_kb_mem_read(address);
  if (PRINTER_3(address)) return trs_printer_read();
  if (address < trs_rom_size) return rom[address];
#if MEMDEBUG
  error("[PC=%04X] Invalid read of address %04X, returning FF, mem_map=%02X",
      Z80_PC, address, memory_map);
#endif
  return 0xFF;
}

int mem_peek(int address)
{
  if (address < MAX_MEMORY_SIZE)
    return memory[address];
  else
    return 0xFF;
}

void mem_poke(int address, int value)
{
  if (address < MAX_MEMORY_SIZE)
    memory[address] = value;
}

int mem_read(int address)
{
  address &= 0xFFFF; /* allow callers to be sloppy */

  /* There are some adapters that sit above the system and
     either intercept before the hardware proper, or adjust
     the address. Deal with these first so that we take their
     output and feed it into the memory map */

  /* Anitek MegaMem */
  if (megamem_addr) {
    if (address >= megamem_addr && address <= megamem_addr + 0x3FFF)
      return memory[megamem_base + (address & 0x3FFF)];
  }

  /* The SuperMem sits between the system and the Z80 */
  if (supermem && supermem_base) {
    if (!((address ^ supermem_high) & 0x8000))
      return memory[supermem_base + (address & 0x7FFF)];
    /* Otherwise the request comes from the system */
  }

  switch (memory_map) {
    case 0x10: /* Model I */
      if (address < RAM_START)
        return trs80_model1_mmio(address);
      else
        return trs80_model1_ram(address);

    case 0x11: /* Model I: selector mode 1 (all RAM except I/O high) */
      if (address >= 0xF7E0 && address <= 0xF7FF)
        return trs80_model1_mmio(address & 0x3FFF);
      else
        return trs80_model1_ram(address);

    case 0x12: /* Model I: selector mode 2 (ROM disabled) */
      if (address >= 0x37E0 && address < RAM_START)
        return trs80_model1_mmio(address);
      else
        return trs80_model1_ram(address);

    case 0x13: /* Model I: selector mode 3 (CP/M mode) */
      if (address >= 0xF7E0)
        return trs80_model1_mmio(address & 0x3FFF);
      /* Fall through */

    case 0x14: /* Model I: All RAM banking high */
    case 0x15: /* Model I: All RAM banking low */
      return trs80_model1_ram(address);

    case 0x16: /* X-MEM/80 16K page */
      if (address < RAM_START)
        return trs80_model1_mmio(address);
      if (address < 0x8000)
        return memory[address];
      else
        return memory[address + bank_offset[(address - 0x8000) >> 14]];

    case 0x17: /* Schmidtke 80-Z Video Card */
      if (sys_byte & (1 << 0)) {
        if (address >= video_addr && address <= video_addr + 0xFFF)
          return video[((address - video_addr) & 0x7FF)];
      }
      if ((sys_byte & (1 << 3)) || address >= RAM_START)
        return memory[address];
      else
        return trs80_model1_mmio(address);

    case 0x18: /* EG-64 Memory-Banking-Adaptor */
      if (address < RAM_START) {
        if (((sys_byte & (1 << 0)) && address <= 0x2FFF) ||
            ((sys_byte & (1 << 2)) && address >= 0x3000 && address <= 0x35FF) ||
            ((sys_byte & (1 << 4)) && address >= 0x3600 && address <= 0x37FF) ||
            ((sys_byte & (1 << 5)) && address >= 0x3800 && address <= 0x3BFF) ||
            ((sys_byte & (1 << 6)) && address >= 0x3C00))
          return memory[address];
        else
          return trs80_model1_mmio(address);
      }
      return memory[address];

    case 0x19: /* Lubomir Soft Banker */
      if (address < RAM_START) {
        if (((sys_byte & (1 << 6)) && address <= 0x37DF) ||
            ((sys_byte & (1 << 5)) && address >= 0x37E0))
          return memory[address];
        else
          return trs80_model1_mmio(address);
      }
      return memory[address + bank_offset[address >> 15]];

    case 0x1A: /* EACA EG 3200: bit set to 0 => bank enabled */
      if (address <= 0x43FF && sys_byte != 0x0F) {
        /* Bank 1: ROM */
        if ((sys_byte & (1 << 0)) == 0 && address < trs_rom_size)
          return rom[address];
        /* Bank 2: Video RAM 0 = 1K, 64x16, TRS-80 M1 compatible */
        if ((sys_byte & (1 << 1)) == 0) {
          if (address >= VIDEO_START && address < RAM_START)
            return video[address - VIDEO_START];
        }
        /* Bank 3: Video RAM 1 = additional 1K for 80x24 video mode */
        if ((sys_byte & (1 << 2)) == 0 && address >= RAM_START)
          return video[address - VIDEO_START];
        /* Bank 4: Disk I/O and Keyboard */
        if ((sys_byte & (1 << 3)) == 0 && address < VIDEO_START) {
          if (address >= 0x37E0 && address <= 0x37EF)
            return trs80_model1_mmio(address);
          if (address >= KEYBOARD_START)
            return trs_kb_mem_read(address);
        }
      }
      return memory[address + bank_offset[address >> 15]];

    case 0x1B: /* TCS Genie IIIs */
      if (address < RAM_START) {
        if ((sys_byte & (1 << 0)) == 0) {
          if ((sys_byte & (1 << 4)) && address >= KEYBOARD_START)
            /* 2K Video RAM */
            return video[(address + video_addr) & 0x7FF];
          if (address >= VIDEO_START)
            return video[address + video_addr];
          if (address >= KEYBOARD_START && address <= 0x38FF)
            return trs_kb_mem_read(address);
          /* Disk and Printer MMIO */
          if (TRS_INTLATCH(address))
            return genie3s_latch_read(trs_interrupt_latch_read());
          if (address >= 0x37E8 && address <= 0x37EF)
            return trs80_model1_mmio(address);
        }
        if ((sys_byte & (1 << 2)) == 0 && address <= 0x2FFF)
          return rom[address];
        /* "Constant bit" points to Bank 0 */
        if ((sys_byte_2 & (1 << 0)) == 0)
          return memory[address + bank_offset[1]];
      }
      if ((sys_byte & (1 << 3)) && address >= 0x8000)
        return genie3s_hrg_read(address - 0x8000);
      /* "Constant bit" points to Bank 0 */
      if ((sys_byte_2 & (1 << 0)) && address >= 0xE000)
        return memory[address + bank_offset[1]];
      else
        return memory[address + bank_offset[0] + bank_base];

    case 0x1C: /* Aster CT-80 */
      if ((sys_byte & (1 << 5)) == 0) { /* device bank */
        if (address <= 0x2FFF) {
          /* Boot-ROM */
          if (sys_byte & (1 << 1))
            return rom[(address & 0x7FF) | 0x3000];
          /* BASIC */
          if ((sys_byte & (1 << 2)) == 0)
            return rom[address];
        } else {
          if ((sys_byte & (1 << 3)) == 0) {
            /* TRS-80 mode */
            if (address < RAM_START)
              return trs80_model1_mmio(address);
          } else {
            /* CP/M mode */
            /* 2K Video RAM */
            if (address >= 0xF800)
              return video[(address - video_addr) & 0x7FF];
            /* Keyboard */
            if (address >= 0xF400)
              return trs_kb_mem_read(address);
            /* Disk and Printer MMIO */
            if (address >= 0xEFE0 && address <= 0xEFEF)
              return trs80_model1_mmio(address - 0xB800);
            /* Boot-ROM copy */
            if (address >= 0xEC00)
              return rom[address - 0xBC00];
          }
        }
      }
      return memory[address];

    case 0x1D: /* LNW80/II */
      if (sys_byte & (1 << 3) && address < RAM_START)
        /* HRG in low 16K */
        return hrg_read_data(address);
      if ((sys_byte_2 & (1 << 0)) == 0) {
        if ((sys_byte_2 & (1 << 4)) == 0 && address < RAM_START) {
          if (address >= VIDEO_START)
            return mem_video_page_read(address);
          if (address >= 0x37E0 && address <= 0x38FF)
            return trs80_model1_mmio(address);
          if ((sys_byte_2 & (1 << 1)) == 0 && address <= 0x2FFF)
            return rom[address];
        }
      } else {
        /* CP/M mode */
        if (address >= 0xC000) {
          if ((sys_byte_2 & (1 << 1)) == 0)
            return trs80_model1_mmio(address - 0xC000);
          if (address >= 0xFC00)
            return mem_video_page_read(address & 0x3FFF);
          if (address >= 0xF900)
            return memory[address + 0x8000];
          if (address >= 0xF800)
            return trs_kb_mem_read(address);
          if (address >= 0xF700)
            return trs80_model1_mmio(address & 0x3FFF);
        }
      }
      return memory[address + bank_offset[address >> 15]];

    case 0x1E: /* TCS Genie IIs/SpeedMaster */
      /* Expansions bit */
      if ((sys_byte & (1 << 7)) && address <= 0xBFFF) {
        if (sys_byte & (1 << 0))
          /* RAM 192 B */
          return memory[address + bank_base];
        if (address >= RAM_START)
          /* Load BASIC in ROM-Card */
          return rom[(address & 0x2FFF) | 0x1000];
      }
      if (address < RAM_START) {
        /* HRG in low 16K */
        if (sys_byte & (1 << 3))
          return hrg_read_data(address);
        /* MMIO and ROM */
        if ((sys_byte & (1 << 0)) == 0) {
          if (address >= 0x3400)
            return trs80_model1_mmio(address);
          if ((sys_byte & (1 << 2)) == 0 && address <= 0x2FFF)
            return rom[address];
        }
      }
      return memory[address];

    case 0x30: /* Model III */
      return trs80_model3_mem_read(address);

    case 0x31: /* CP-500 */
    case 0x32: /* CP-500 M80 64K RAM */
    case 0x33: /* CP-500 M80 80x24 video */
      return cp500_mem_read(address, memory_map, rom, memory);

    case 0x34: /* Holmes VID-80 (VX-3) */
      /* 48 KB extended memory */
      if ((sys_byte & (1 << 6)) && address <= 0xBFFF)
        return memory[address + 65536];
      /* 80*24 Video RAM */
      if ((sys_byte & (1 << 2)) == 0 && address >= 0xF800)
        return video[address - 0xF800];
      if (address < RAM_START) {
        if ((sys_byte & (1 << 4)) == 0) {
          if ((sys_byte & (1 << 0)) && address < KEYBOARD_START)
            /* Model III ROM appended to VID-80 4K ROM */
            return rom[address + 0x1000];
        }
        if ((sys_byte & (1 << 3)) && address >= VIDEO_START)
          return video[address - VIDEO_START];
        if ((sys_byte & (1 << 1)) == 0) {
          if (address >= KEYBOARD_START && address < 0x3900)
            return trs_kb_mem_read(address);
        }
        if ((sys_byte & (1 << 0)) == 0 && address <= 0x0FFF)
          return rom[address];
      }
      return memory[address];

    case 0x40: /* Model 4 map 0 */
      if (address >= RAM_START) {
        return memory[address + bank_offset[address >> 15]];
      }
      if (address >= VIDEO_START) return mem_video_page_read(address);
      if (address >= KEYBOARD_START) return trs_kb_mem_read(address);
      if (PRINTER_3(address)) return trs_printer_read();
      if (address < trs_rom_size) return rom[address];
      break;

    case 0x50: /* Model 4P map 0 */
    case 0x51: /* Model 4P map 1 */
      if (sys_byte && address < trs_rom_size) return rom[address];
      /* Fall through */
    case 0x41: /* Model 4  map 1 */
      if (address >= RAM_START || address < KEYBOARD_START) {
        return memory[address + bank_offset[address >> 15]];
      }
      if (address >= VIDEO_START) return mem_video_page_read(address);
      if (address >= KEYBOARD_START) return trs_kb_mem_read(address);
      break;

    case 0x42: /* Model 4  map 2 */
    case 0x52: /* Model 4P map 2 */
      if (address < 0xF400) {
        return memory[address + bank_offset[address >> 15]];
      }
      if (address >= 0xF800) return video[address - 0xF800];
      return trs_kb_mem_read(address);

    case 0x43: /* Model 4  map 3 */
    case 0x53: /* Model 4P map 3 */
      return memory[address + bank_offset[address >> 15]];
  }

  return 0xFF;
}

static void lnw80_screen_write_char(int vaddr, int value)
{
  vaddr += video_addr;

  if (mem_video_write(vaddr, value)) {
    if (text80x24) {
      /* LNW80 II 80x24 text mode */
      if (vaddr >= 1536)
        /* 16x24 text extension region */
        trs_screen_write_char((vaddr & 15) + 64
         + (((vaddr - 1536) / 64 + ((vaddr >> 4) & 3) * 8) * 80), value);
      else
        /* 64x24 text region */
        trs_screen_write_char((vaddr % 64) + ((vaddr / 64) * 80), value);
    } else {
      trs_screen_write_char(vaddr, value);
    }
  }
}

static void trs80_screen_write_char(int vaddr, int value)
{
  if (mem_video_write(vaddr, value))
    trs_screen_write_char(vaddr, value);
}

static void trs80_model1_write_mem(int address, int value)
{
  if (selector) {
    int const bank = bank_offset[address >> 15];

    /* Selector mode 6 remaps RAM from 0000-3FFF to C000-FFFF while keeping
       the ROMs visible */
    if ((mem_command & 7) == 6 && address >= 0xC000) {
      /* We have no low 16K of RAM. This is for the LNW80 really */
      if (!(mem_command & 8))
        return;
      /* Use the low 16K, and then bank it. I'm not 100% sure how the
         PAL orders the two */
      address &= 0x3FFF;
    }
    /* Deal with 32K banking from selector */
    address += bank;
  }

  memory[address] = value;
}

static void trs80_model1_write_mmio(int address, int value)
{
  if (address >= VIDEO_START) {
    if (lowercase == 0) {
      /*
       * Video write.  Hack here to make up for the missing bit 6
       * video ram, emulating the gate in Z30.
       */
      if (value & 0xA0)
        value &= 0xBF;
      else
        value |= 0x40;
    }
    trs80_screen_write_char(address - VIDEO_START, value);
  } else if (address == TRSDISK_DATA) {
    trs_disk_data_write(value);
  } else if (address == TRSDISK_COMMAND) {
    trs_disk_command_write(value);
  } else if (address == TRSDISK_TRACK) {
    trs_disk_track_write(value);
  } else if (address == TRSDISK_SECTOR) {
    trs_disk_sector_write(value);
  } else if (TRSDISK_SELECT(address)) {
    trs_disk_select_write(value);
  } else if (address >= 0x3900 && selector) {
    trs80_model1_write_mem(address, value);
  } else if (PRINTER_1(address)) {
    trs_printer_write(value);
#if MEMDEBUG
  } else {
    error("[PC=%04X] Invalid write of %02X to address %04X, mem_map=%02X",
        Z80_PC, value, address, memory_map);
#endif
  }
}

void trs80_model3_mem_write(int address, int value)
{
  if (address >= RAM_START) {
    memory[address] = value;
  } else if (address >= VIDEO_START) {
    if (grafyx_m3_write_byte(address - VIDEO_START, value))
      return;
    else
      trs80_screen_write_char(address - VIDEO_START, value);
#if MEMDEBUG
  } else {
    error("[PC=%04X] Invalid write of %02X to address %04X, mem_map=%02X",
        Z80_PC, value, address, memory_map);
#endif
  }
}

void mem_write(int address, int value)
{
  address &= 0xFFFF;

  /* Anitek MegaMem */
  if (megamem_addr) {
    if (address >= megamem_addr && address <= megamem_addr + 0x3FFF) {
      memory[megamem_base + (address & 0x3FFF)] = value;
      return;
    }
  }

  /* The SuperMem sits between the system and the Z80 */
  if (supermem && supermem_base) {
    if (!((address ^ supermem_high) & 0x8000)) {
      memory[supermem_base + (address & 0x7FFF)] = value;
      return;
    }
    /* Otherwise the request comes from the system */
  }

  switch (memory_map) {
    case 0x10: /* Model I */
      if (address >= RAM_START)
        trs80_model1_write_mem(address, value);
      else
        trs80_model1_write_mmio(address, value);
      break;

    case 0x11: /* Model I: selector mode 1 (all RAM except I/O high) */
      if (address >= 0xF7E0 && address <= 0xF7FF)
        trs80_model1_write_mmio(address & 0x3FFF, value);
      else
        trs80_model1_write_mem(address, value);
      break;

    case 0x12: /* Model I: selector mode 2 (ROM disabled) */
      if (address >= 0x37E0 && address < RAM_START)
        trs80_model1_write_mmio(address, value);
      else
        trs80_model1_write_mem(address, value);
      break;

    case 0x13: /* Model I: selector mode 3 (CP/M mode) */
      if (address >= 0xF7E0)
        trs80_model1_write_mmio(address & 0x3FFF, value);
      else
        /* Fall through */

    case 0x14: /* Model I: All RAM banking high */
    case 0x15: /* Model I: All RAM banking low */
      trs80_model1_write_mem(address, value);
      break;

    case 0x16: /* X-MEM/80 16K page */
      if (address < RAM_START)
        trs80_model1_write_mmio(address, value);
      else if (address < 0x8000)
        memory[address] = value;
      else
        memory[address + bank_offset[(address - 0x8000) >> 14]] = value;
      break;

    case 0x17: /* Schmidtke 80-Z Video Card */
      if (sys_byte & (1 << 0)) {
        if (address >= video_addr && address <= video_addr + 0xFFF) {
          trs80_screen_write_char(((address - video_addr) & 0x7FF), value);
          return;
        }
      }
      if ((sys_byte & (1 << 3)) || address >= RAM_START)
        memory[address] = value;
      else
        trs80_model1_write_mmio(address, value);
      break;

    case 0x18: /* EG-64 Memory-Banking-Adaptor */
      if (address < RAM_START) {
        if (((sys_byte & (1 << 1)) && address <= 0x2FFF) ||
            ((sys_byte & (1 << 3)) && address >= 0x3000 && address <= 0x35FF) ||
            ((sys_byte & (1 << 4)) && address >= 0x3600 && address <= 0x37FF) ||
            ((sys_byte & (1 << 5)) && address >= 0x3800 && address <= 0x3BFF) ||
            ((sys_byte & (1 << 6)) && address >= 0x3C00))
          memory[address] = value;
        else
          trs80_model1_write_mmio(address, value);
        return;
      }
      memory[address] = value;
      break;

    case 0x19: /* Lubomir Soft Banker */
      if (address < RAM_START) {
        if (((sys_byte & (1 << 7)) && address <= 0x37DF) ||
            ((sys_byte & (1 << 5)) && address >= 0x37E0))
          memory[address] = value;
        else
          trs80_model1_write_mmio(address, value);
        return;
      }
      memory[address + bank_offset[address >> 15]] = value;
      break;

    case 0x1A: /* EACA EG 3200: bit set to 0 => bank enabled */
      if (address <= 0x47FF && sys_byte != 0x0F) {
        /* Bank 2: Video RAM 0 = 1K, 64x16, TRS-80 M1 compatible */
        if ((sys_byte & (1 << 1)) == 0) {
          if (address >= VIDEO_START && address < RAM_START) {
            trs80_screen_write_char(address - VIDEO_START, value);
            return;
          }
        }
        /* Bank 3: Video RAM 1 = additional 1K for 80x24 video mode
         *         Video RAM 2 = EG 3210 Programmable Graphics Adaptor */
        if ((sys_byte & (1 << 2)) == 0) {
          if (address >= RAM_START && address <= 0x43FF) {
            trs80_screen_write_char(address - VIDEO_START, value);
            return;
          }
          if (address >= 0x4400) {
            address -= 0x4400;
            eg3210_char((address / 16) + 192, address & 0x0F, value);
            return;
          }
        }
        /* Bank 4: Disk I/O */
        if ((sys_byte & (1 << 3)) == 0) {
          if (address >= 0x37E0 && address <= 0x37EF) {
            trs80_model1_write_mmio(address, value);
            return;
          }
        }
      }
      memory[address + bank_offset[address >> 15]] = value;
      break;

    case 0x1B: /* TCS Genie IIIs */
      if (address < RAM_START) {
        if ((sys_byte & (1 << 0)) == 0) {
          if ((sys_byte & (1 << 4)) && address >= KEYBOARD_START) {
            /* 2K Video RAM */
            trs80_screen_write_char((address + video_addr) & 0x7FF, value);
            return;
          }
          if (address >= VIDEO_START) {
            trs80_screen_write_char(address + video_addr, value);
            return;
          }
          /* Disk and Printer MMIO */
          if (address >= 0x37E0 && address <= 0x37EF) {
            trs80_model1_write_mmio(address, value);
            return;
          }
        }
        /* Write protect "Pseudo-ROM" */
        if ((sys_byte & (1 << 5)) && address <= 0x2FFF)
          return;
        /* "Constant bit" points to Bank 0 */
        if ((sys_byte_2 & (1 << 0)) == 0) {
          memory[address + bank_offset[1]] = value;
          return;
        }
      }
      if ((sys_byte & (1 << 3)) && address >= 0x8000) {
        genie3s_hrg_write(address - 0x8000, value);
        return;
      }
      /* Write to Font-SRAM */
      if (sys_byte_2 & (1 << 1) && address >= 0x8000) {
        genie3s_char(video[(VIDEO_START + video_addr)],
            (address - 0x8000) >> 11, value);
        return;
      }
      /* "Constant bit" points to Bank 0 */
      if ((sys_byte_2 & (1 << 0)) && address >= 0xE000)
        memory[address + bank_offset[1]] = value;
      else
        memory[address + bank_offset[0] + bank_base] = value;
      break;

    case 0x1C: /* Aster CT-80 */
      if ((sys_byte & (1 << 5)) == 0) { /* device bank */
        if ((sys_byte & (1 << 3)) == 0) {
          /* TRS-80 mode */
          if (address >= 0x37E0 && address < RAM_START) {
            trs80_model1_write_mmio(address, value);
            return;
          }
        } else {
          /* CP/M mode */
          /* 2K Video RAM */
          if (address >= 0xF800) {
            trs80_screen_write_char((address - video_addr) & 0x7FF, value);
            return;
          }
          /* Disk and Printer MMIO */
          if (address >= 0xEFE0 && address <= 0xEFEF) {
            trs80_model1_write_mmio(address - 0xB800, value);
            return;
          }
        }
      }
      memory[address] = value;
      break;

    case 0x1D: /* LNW80/II */
      if (sys_byte & (1 << 3) && address < RAM_START) {
        /* HRG in low 16K */
        hrg_write_data(address, value);
        return;
      }
      if ((sys_byte_2 & (1 << 0)) == 0) {
        if (address < RAM_START) {
          if (sys_byte_2 & (1 << 7) && address <= 0x2FFF)
            return;
          if ((sys_byte_2 & (1 << 4)) == 0) {
            if (address >= VIDEO_START) {
              lnw80_screen_write_char(address, value);
              return;
            }
            if (address >= 0x37E0 && address <= 0x37EF) {
              trs80_model1_write_mmio(address, value);
              return;
            }
          }
        }
      } else {
        /* CP/M mode */
        if (address >= 0xF700) {
          if ((sys_byte_2 & (1 << 1)) == 0) {
            trs80_model1_write_mmio(address & 0x3FFF, value);
            return;
          }
          if (address >= 0xFC00) {
            lnw80_screen_write_char(address & 0x3FFF, value);
            return;
          }
          if (address >= 0xF900) {
            memory[address + 0x8000] = value;
            return;
          }
          if (address <= 0xF7FF) {
            trs80_model1_write_mmio(address & 0x3FFF, value);
            return;
          }
        }
      }
      memory[address + bank_offset[address >> 15]] = value;
      break;

    case 0x1E: /* TCS Genie IIs/SpeedMaster */
      /* Expansions bit */
      if ((sys_byte & (1 << 7)) && address <= 0xBFFF) {
        if (sys_byte & (1 << 0)) {
          /* RAM 192 B */
          memory[address + bank_base] = value;
          return;
        }
      }
      if (address < RAM_START) {
        /* HRG in low 16K */
        if (sys_byte & (1 << 3)) {
          hrg_write_data(address, value);
          return;
        }
        /* MMIO */
        if ((sys_byte & (1 << 0)) == 0) {
          if (address >= 0x3400) {
            trs80_model1_write_mmio(address, value);
            return;
          }
          /* Write protect "Pseudo-ROM" */
          if ((sys_byte & (1 << 5)) && address <= 0x2FFF)
            return;
        }
      }
      memory[address] = value;
      break;

    case 0x30: /* Model III */
      trs80_model3_mem_write(address, value);
      break;

    case 0x31: /* CP-500 */
    case 0x32: /* CP-500 M80 64K RAM */
    case 0x33: /* CP-500 M80 80x24 video */
      cp500_mem_write(address, value, memory_map, memory);
      break;

    case 0x34: /* Holmes VID-80 (VX-3) */
      /* 48 KB extended memory */
      if ((sys_byte & (1 << 6)) && address <= 0xBFFF) {
        memory[address + 65536] = value;
        return;
      }
      if ((sys_byte & (1 << 3)) && address < RAM_START) {
        if (address >= VIDEO_START) {
          if (sys_byte & (1 << 5))
            video[address - VIDEO_START] = value;
          else
            trs80_screen_write_char(address - VIDEO_START, value);
        }
        return;
      }
      /* 80*24 Video RAM */
      if ((sys_byte & (1 << 2)) == 0 && address >= 0xF800) {
        trs80_screen_write_char(address - 0xF800, value);
        return;
      }
      memory[address] = value;
      break;

    case 0x40: /* Model 4  map 0 */
    case 0x50: /* Model 4P map 0 */
      if (address >= RAM_START) {
        memory[address + bank_offset[address >> 15]] = value;
      } else if (address >= VIDEO_START) {
        if (mem_video_page_write(address, value))
          trs_screen_write_char(address + video_addr, value);
      }
      break;

    case 0x41: /* Model 4  map 1 */
    case 0x51: /* Model 4P map 1 */
      if (address >= RAM_START || address < KEYBOARD_START) {
        memory[address + bank_offset[address >> 15]] = value;
      } else if (address >= VIDEO_START) {
        if (mem_video_page_write(address, value))
          trs_screen_write_char(address + video_addr, value);
      }
      break;

    case 0x42: /* Model 4  map 2 */
    case 0x52: /* Model 4P map 2 */
      if (address < 0xF400) {
        memory[address + bank_offset[address >> 15]] = value;
      } else if (address >= 0xF800) {
        trs80_screen_write_char(address - 0xF800, value);
      }
      break;

    case 0x43: /* Model 4  map 3 */
    case 0x53: /* Model 4P map 3 */
      memory[address + bank_offset[address >> 15]] = value;
      break;
  }
}

/*
 * Words are stored with the low-order byte in the lower address.
 */
int mem_read_word(int address)
{
  return mem_read(address) | (mem_read(address + 1) << 8);
}

void mem_write_word(int address, int value)
{
  mem_write(address, value & 0xFF);
  mem_write(address + 1, value >> 8);
}

static Uint8 *trs80_model1_ram_addr(int address)
{
  if (address >= trs_mem_size) return NULL; /* nonexistent memory address */

  if (selector) {
    int const bank = bank_offset[address >> 15];

    /* Selector mode 6 remaps RAM from 0000-3FFF to C000-FFFF while keeping
       the ROMs visible */
    if ((mem_command & 7) == 6 && address >= 0xC000) {
      /* Use the low 16K, and then bank it. I'm not 100% sure how the
         PAL orders the two */
      address &= 0x3FFF;
    }
    /* Deal with 32K banking from selector */
    address += bank;
  }

  return memory + address;
}

static Uint8 *trs80_model1_mmio_addr(int address, int writing)
{
  if (address >= VIDEO_START) return &video[address - VIDEO_START];
  if (address < trs_rom_size && writing == 0) return &rom[address];
  /* With a selector 768 bytes poke through the hole */
  if (address >= 0x3900 && selector) return trs80_model1_ram_addr(address);
  return NULL;
}

Uint8 *trs80_model3_mem_addr(int address, int writing)
{
  if (address >= trs_mem_size) return NULL; /* nonexistent memory address */
  if (address >= RAM_START) return &memory[address];
  if (address >= VIDEO_START) return &video[address - VIDEO_START];
  if (address < trs_rom_size && writing == 0) return &rom[address];
  return NULL;
}

/*
 * Get a pointer to the given address.  Note that there is no checking
 * whether the next virtual address is physically contiguous.  The
 * caller is responsible for making sure his strings don't span
 * memory map boundaries.
 */
Uint8 *mem_pointer(int address, int writing)
{
  address &= 0xFFFF;

  /* Anitek MegaMem */
  if (megamem_addr) {
    if (address >= megamem_addr && address <= megamem_addr + 0x3FFF)
      return &memory[megamem_base + (address & 0x3FFF)];
  }

  /* The SuperMem sits between the system and the Z80 */
  if (supermem && supermem_base) {
    if (!((address ^ supermem_high) & 0x8000))
      return &memory[supermem_base + (address & 0x7FFF)];
    /* Otherwise the request comes from the system */
  }

  switch (memory_map) {
    case 0x10: /* Model I */
      if (address < RAM_START)
        return trs80_model1_mmio_addr(address, writing);
      else
        return trs80_model1_ram_addr(address);

    case 0x11: /* Model I: selector mode 1 (all RAM except I/O high) */
      if (address >= 0xF7E0 && address <= 0xF7FF)
        return NULL;
      else
        return trs80_model1_ram_addr(address);

    case 0x12: /* Model I: selector mode 2 (ROM disabled) */
      if (address >= 0x37E0 && address < RAM_START)
        return trs80_model1_mmio_addr(address, writing);
      else
        return trs80_model1_ram_addr(address);

    case 0x13: /* Model I: selector mode 3 (CP/M mode) */
      if (address >= 0xF7E0)
        return trs80_model1_mmio_addr(address & 0x3FFF, writing);
      /* Fall through */

    case 0x14: /* Model I: All RAM banking high */
    case 0x15: /* Model I: All RAM banking low */
      return trs80_model1_ram_addr(address);

    case 0x16: /* X-MEM/80 16K page */
      if (address < RAM_START)
        return trs80_model1_mmio_addr(address, writing);
      if (address < 0x8000)
        return &memory[address];
      else
        return &memory[address + bank_offset[(address - 0x8000) >> 14]];

    case 0x17: /* Schmidtke 80-Z Video Card */
      if (sys_byte & (1 << 0)) {
        if (address >= video_addr && address <= video_addr + 0xFFF)
          return &video[((address - video_addr) & 0x7FF)];
      }
      if ((sys_byte & (1 << 3)) || address >= RAM_START)
        return &memory[address];
      else
        return trs80_model1_mmio_addr(address, writing);

    case 0x18: /* EG-64 Memory-Banking-Adaptor */
      if (address < RAM_START) {
        if (((sys_byte & (1 << 0)) && address <= 0x2FFF) ||
            ((sys_byte & (1 << 2)) && address >= 0x3000 && address <= 0x35FF) ||
            ((sys_byte & (1 << 4)) && address >= 0x3600 && address <= 0x37FF) ||
            ((sys_byte & (1 << 5)) && address >= 0x3800 && address <= 0x3BFF) ||
            ((sys_byte & (1 << 6)) && address >= 0x3C00))
          return &memory[address];
        else
          return trs80_model1_mmio_addr(address, writing);
      }
      return &memory[address];

    case 0x19: /* Lubomir Soft Banker */
      if (address < RAM_START) {
        if (((sys_byte & (1 << 6)) && address <= 0x37DF) ||
            ((sys_byte & (1 << 5)) && address >= 0x37E0))
          return &memory[address];
        else
          return trs80_model1_mmio_addr(address, writing);
      }
      return &memory[address + bank_offset[address >> 15]];

    case 0x1A: /* EACA EG 3200: bit set to 0 => bank enabled */
      if (address <= 0x43FF && sys_byte != 0x0F) {
        /* Bank 1: ROM */
        if ((sys_byte & (1 << 0)) == 0 && address < trs_rom_size)
          return writing ? NULL : &rom[address];
        /* Bank 2: Video RAM 0 = 1K, 64x16, TRS-80 M1 compatible */
        if ((sys_byte & (1 << 1)) == 0) {
          if (address >= VIDEO_START && address < RAM_START)
            return &video[address - VIDEO_START];
        }
        /* Bank 3: Video RAM 1 = additional 1K for 80x24 video mode */
        if ((sys_byte & (1 << 2)) == 0 && address >= RAM_START)
          return &video[address - VIDEO_START];
      }
      return &memory[address + bank_offset[address >> 15]];

    case 0x1B: /* TCS Genie IIIs */
      if (address < RAM_START) {
        if ((sys_byte & (1 << 0)) == 0) {
          if ((sys_byte & (1 << 4)) && address >= KEYBOARD_START)
            /* 2K Video RAM */
            return &video[(address + video_addr) & 0x7FF];
          if (address >= VIDEO_START)
            return &video[address + video_addr];
        }
        if ((sys_byte & (1 << 2)) == 0 && address <= 0x2FFF)
          return writing ? NULL : &rom[address];
        /* "Constant bit" points to Bank 0 */
        if ((sys_byte_2 & (1 << 0)) == 0)
          return &memory[address + bank_offset[1]];
      }
      /* "Constant bit" points to Bank 0 */
      if ((sys_byte_2 & (1 << 0)) && address >= 0xE000)
        return &memory[address + bank_offset[1]];
      else
        return &memory[address + bank_offset[0] + bank_base];

    case 0x1C: /* Aster CT-80 */
      if ((sys_byte & (1 << 5)) == 0) { /* device bank */
        if (address <= 0x2FFF) {
          /* Boot-ROM */
          if (sys_byte & (1 << 1))
            return writing ? NULL : &rom[(address & 0x7FF) | 0x3000];
          /* BASIC */
          if ((sys_byte & (1 << 2)) == 0)
            return writing ? NULL : &rom[address];
        } else {
          if ((sys_byte & (1 << 3)) == 0) {
            /* TRS-80 mode */
            if (address < RAM_START)
              return trs80_model1_mmio_addr(address, writing);
          } else {
            /* CP/M mode */
            /* 2K Video RAM */
            if (address >= 0xF800)
              return &video[(address - video_addr) & 0x7FF];
            /* Boot-ROM */
            if (address >= 0xEC00 && address <= 0xF3FF)
              return writing ? NULL : &rom[address - 0xBC00];
          }
        }
      }
      return &memory[address];

    case 0x1D: /* LNW80/II */
      if (sys_byte & (1 << 3) && address < RAM_START)
        /* HRG in low 16K */
        return NULL;
      if ((sys_byte_2 & (1 << 0)) == 0) {
        if ((sys_byte_2 & (1 << 4)) == 0 && address < RAM_START) {
          if (address >= VIDEO_START)
            return mem_video_page_addr(address);
          if ((sys_byte_2 & (1 << 1)) == 0 && address <= 0x2FFF)
            return writing ? NULL : &rom[address];
        }
      } else {
        /* CP/M mode */
        if (address >= 0xC000) {
          if ((sys_byte_2 & (1 << 1)) == 0)
            return trs80_model1_mmio_addr(address & 0x3FFF, writing);
          if (address >= 0xFC00)
            return mem_video_page_addr(address & 0x3FFF);
          if (address >= 0xF900)
            return &memory[address + 0x8000];
        }
      }
      return &memory[address + bank_offset[address >> 15]];

    case 0x1E: /* TCS Genie IIs/SpeedMaster */
      /* Expansions bit */
      if ((sys_byte & (1 << 7)) && address <= 0xBFFF) {
        if (sys_byte & (1 << 0))
          /* RAM 192 B */
          return &memory[address + bank_base];
        if (address >= RAM_START)
          /* Load BASIC in ROM-Card */
          return writing ? NULL : &rom[(address & 0x2FFF) | 0x1000];
      }
      if (address < RAM_START) {
        if (sys_byte & (1 << 3)) /* HRG */
          return NULL;
        /* MMIO and ROM */
        if ((sys_byte & (1 << 0)) == 0) {
          if (address >= 0x3400)
            return trs80_model1_mmio_addr(address, writing);
          if ((sys_byte & (1 << 2)) == 0 && address <= 0x2FFF)
            return writing ? NULL : &rom[address];
        }
      }
      return &memory[address];

    case 0x30: /* Model III reading */
      return trs80_model3_mem_addr(address, writing);

    case 0x31: /* CP-500 */
    case 0x32: /* CP-500 M80 64K RAM */
    case 0x33: /* CP-500 M80 80x24 video */
      return cp500_mem_addr(address, memory_map, rom, memory, writing);

    case 0x34: /* Holmes VID-80 (VX-3) */
      /* 48 KB extended memory */
      if ((sys_byte & (1 << 6)) && address <= 0xBFFF)
        return &memory[address + 65536];
      /* 80*24 Video RAM */
      if ((sys_byte & (1 << 2)) == 0 && address >= 0xF800)
        return &video[address - 0xF800];
      if (address < RAM_START) {
        if ((sys_byte & (1 << 4)) == 0) {
          if ((sys_byte & (1 << 0)) && address < KEYBOARD_START)
            /* Model III ROM appended to VID-80 4K ROM */
            return writing ? NULL : &rom[address + 0x1000];
        }
        if ((sys_byte & (1 << 3)) && address >= VIDEO_START)
          return &video[address - VIDEO_START];
        if ((sys_byte & (1 << 0)) == 0 && address <= 0x0FFF)
          return writing ? NULL : &rom[address];
      }
      return &memory[address];

    case 0x40: /* Model 4 map 0 */
      if (address >= RAM_START) {
        return &memory[address + bank_offset[address >> 15]];
      }
      if (address >= VIDEO_START) return mem_video_page_addr(address);
      if (address < trs_rom_size && writing == 0) return &rom[address];
      break;

    case 0x50: /* Model 4P map 0 */
    case 0x51: /* Model 4P map 1 */
      if (sys_byte && address < trs_rom_size && writing == 0)
        return &rom[address];
      /* Fall through */
    case 0x41: /* Model 4  map 1 */
      if (address >= RAM_START || address < KEYBOARD_START) {
        return &memory[address + bank_offset[address >> 15]];
      }
      if (address >= VIDEO_START) return mem_video_page_addr(address);
      break;

    case 0x42: /* Model 4  map 2 */
    case 0x52: /* Model 4P map 2 */
      if (address < 0xF400) {
        return &memory[address + bank_offset[address >> 15]];
      }
      if (address >= 0xF800) return &video[address - 0xF800];
      break;

    case 0x43: /* Model 4  map 3 */
    case 0x53: /* Model 4P map 3 */
      return &memory[address + bank_offset[address >> 15]];
  }

  return NULL;
}

void trs_mem_save(FILE *file)
{
  trs_save_uint8(file, memory, MAX_MEMORY_SIZE + 1);
  trs_save_uint8(file, rom, MAX_ROM_SIZE + 1);
  trs_save_uint8(file, video, MAX_VIDEO_SIZE + 1);
  trs_save_int(file, &trs_model, 1);
  trs_save_int(file, &trs_mem_size, 1);
  trs_save_int(file, &trs_rom_size, 1);
  trs_save_int(file, &lowercase, 1);
  trs_save_int(file, &memory_map, 1);
  trs_save_int(file, bank_offset, 3);
  trs_save_int(file, &video_addr, 1);
  trs_save_uint32(file, &bank_base, 1);
  trs_save_uint8(file, &mem_command, 1);
  trs_save_int(file, &huffman, 1);
  trs_save_int(file, &hypermem, 1);
  trs_save_int(file, &supermem, 1);
  trs_save_int(file, &supermem_base, 1);
  trs_save_uint32(file, &supermem_high, 1);
  trs_save_int(file, &selector, 1);
  trs_save_int(file, &lubomir, 1);
  trs_save_int(file, &megamem, 1);
  trs_save_int(file, &megamem_addr, 1);
  trs_save_uint32(file, &megamem_base, 1);
  trs_save_int(file, &sys_byte, 1);
  trs_save_int(file, &sys_byte_2, 1);
  trs_save_int(file, &xmem80, 1);
}

void trs_mem_load(FILE *file)
{
  trs_load_uint8(file, memory, MAX_MEMORY_SIZE + 1);
  trs_load_uint8(file, rom, MAX_ROM_SIZE + 1);
  trs_load_uint8(file, video, MAX_VIDEO_SIZE + 1);
  trs_load_int(file, &trs_model, 1);
  trs_load_int(file, &trs_mem_size, 1);
  trs_load_int(file, &trs_rom_size, 1);
  trs_load_int(file, &lowercase, 1);
  trs_load_int(file, &memory_map, 1);
  trs_load_int(file, bank_offset, 3);
  trs_load_int(file, &video_addr, 1);
  trs_load_uint32(file, &bank_base, 1);
  trs_load_uint8(file, &mem_command, 1);
  trs_load_int(file, &huffman, 1);
  trs_load_int(file, &hypermem, 1);
  trs_load_int(file, &supermem, 1);
  trs_load_int(file, &supermem_base, 1);
  trs_load_uint32(file, &supermem_high, 1);
  trs_load_int(file, &selector, 1);
  trs_load_int(file, &lubomir, 1);
  trs_load_int(file, &megamem, 1);
  trs_load_int(file, &megamem_addr, 1);
  trs_load_uint32(file, &megamem_base, 1);
  trs_load_int(file, &sys_byte, 1);
  trs_load_int(file, &sys_byte_2, 1);
  trs_load_int(file, &xmem80, 1);
}
