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
 * Debug flags.  Update help_message in debug.c if these change.
 */
#define IODEBUG_IN  (1 << 0)  /* IN instructions */
#define IODEBUG_OUT (2 << 0)  /* OUT instructions */

#include <time.h>

#include "error.h"
#include "trs.h"
#include "trs_clones.h"
#include "trs_cp500.h"
#include "trs_disk.h"
#include "trs_hard.h"
#include "trs_memory.h"
#include "trs_state_save.h"
#include "trs_stringy.h"
#include "trs_uart.h"

int trs_io_debug_flags;

static int modesel;         /* Model I */
static int modeimage = 0x8; /* Model III/4/4p & SIO */
static int ctrlimage;       /* Model 4/4p & M6845 */
static int rominimage;      /* Model 4p */

/* RTC addr/data */
static int rtc_reg;

/* 6845 CRTC */
static int cursor_csr;
static int cursor_cer;
static int cursor_old;
static int cursor_pos;
static int cursor_vis;
static int interlaced;
static int max_raster;
static int start_addr;

void m6845_crtc_reset(void)
{
  cursor_csr =
  cursor_cer =
  cursor_old =
  cursor_pos =
  cursor_vis =
  interlaced =
  max_raster =
  start_addr = 0;
}

static void m6845_crtc(int value)
{
  switch (ctrlimage) {
    case 0x01: /* Chars displayed */
      if (value == 64 || value == 80)
        m6845_screen(value, 0, 0, 0);
      break;
    case 0x06: /* Lines displayed */
      if (interlaced)
        value *= 2;
      if (value <= 32)
        m6845_screen(0, value, 0, 0);
      break;
    case 0x08: /* Interlace Mode */
      interlaced = ((value & 0x03) == 3);
      m6845_screen(0, 0, 0, interlaced ? 1 : 2);
      break;
    case 0x09: /* Maximum Raster address */
      if (value < 16) {
        max_raster = value;
        m6845_screen(0, 0, max_raster + 1, 0);
      }
      m6845_cursor(cursor_pos - start_addr, cursor_csr, cursor_cer, cursor_vis);
      break;
    case 0x0A: /* Cursor visible / Cursor Start Raster */
      cursor_vis = (value & 0x60) != 0x20;
      value &= 0x1F;
      cursor_csr = value > max_raster ? max_raster : value;
      m6845_cursor(cursor_pos - start_addr, cursor_csr, cursor_cer, cursor_vis);
      break;
    case 0x0B: /* Cursor End Raster */
      value &= 0x1F;
      cursor_cer = value > max_raster ? max_raster : value;
      m6845_cursor(cursor_pos - start_addr, cursor_csr, cursor_cer, cursor_vis);
      break;
    case 0x0C: /* Start Address LSB */
      start_addr = ((value & 0x3F) << 8) | (start_addr & 0x00FF);
      break;
    case 0x0D: /* Start Address MSB */
      start_addr = ((value & 0xFF) << 0) | (start_addr & 0xFF00);
      if (genie3s)
        mem_video_page(start_addr - 1024);
      break;
    case 0x0E: /* Cursor LSB */
      cursor_pos = ((value & 0x3F) << 8) | (cursor_pos & 0x00FF);
      break;
    case 0x0F: /* Cursor MSB */
      cursor_pos = ((value & 0xFF) << 0) | (cursor_pos & 0xFF00);
      if (cursor_pos != cursor_old) {
        if (cursor_vis) {
          m6845_cursor(cursor_old - start_addr, 0, 0, 0);
          m6845_cursor(cursor_pos - start_addr, cursor_csr, cursor_cer, 1);
        }
        cursor_old = cursor_pos;
      }
      break;
  }
}

static int rtc_read(int port)
{
  time_t time_secs = time(NULL);
  struct tm *time_info = localtime(&time_secs);

  switch (port & 0x0F) {
    case 0xC: /* year (high) */
      return (time_info->tm_year / 10) % 10;
    case 0xB: /* year (low) */
      return (time_info->tm_year % 10);
    case 0xA: /* month (high) */
      return ((time_info->tm_mon + 1) / 10);
    case 0x9: /* month (low) */
      return ((time_info->tm_mon + 1) % 10);
    case 0x8: /* date (high) and leap year (bit 2) */
      return ((time_info->tm_mday / 10) | ((time_info->tm_year % 4) ? 0 : 4));
    case 0x7: /* date (low) */
      return (time_info->tm_mday % 10);
    case 0x6: /* day-of-week */
      return time_info->tm_wday;
    case 0x5: /* hours (high) and PM (bit 2) and 24hr (bit 3) */
      return ((time_info->tm_hour / 10) | 8);
    case 0x4: /* hours (low) */
      return (time_info->tm_hour % 10);
    case 0x3: /* minutes (high) */
      return (time_info->tm_min / 10);
    case 0x2: /* minutes (low) */
      return (time_info->tm_min % 10);
    case 0x1: /* seconds (high) */
      return (time_info->tm_sec / 10);
    case 0x0: /* seconds (low) */
      return (time_info->tm_sec % 10);
    default:
      return 0xFF;
  }
}

void z80_out(int port, int value)
{
  if (trs_io_debug_flags & IODEBUG_OUT) {
    debug("out (0x%02x), 0x%02x; pc 0x%04x\n", port, value, z80_state.pc.word);
  }

  /* EG 3200 Genie III */
  if (eg3200) {
    switch (port) {
      case 0x28: /* Genieplus Memory Card */
        mem_bank_base(GENIEPLUS, value);
        break;
      case 0x48: /* TRS_HARD_DATA */
      case 0x49: /* TRS_HARD_PRECOMP */
      case 0x4A: /* TRS_HARD_SECCNT */
      case 0x4B: /* TRS_HARD_SECNUM */
      case 0x4C: /* TRS_HARD_CYLLO */
      case 0x4D: /* TRS_HARD_CYLHI */
      case 0x4E: /* TRS_HARD_SDH */
      case 0x4F: /* TRS_HARD_COMMAND */
        trs_hard_out(port + 0x80, value); /* 0xC8 - 0xCF */
        break;
      case 0x80: /* Genie III VideoExtension HRG */
        grafyx_write_x(value);
        break;
      case 0x81: /* Genie III VideoExtension HRG */
        grafyx_write_y(value);
        break;
      case 0x82: /* Genie III VideoExtension HRG */
        grafyx_write_data(value);
        break;
      case 0x83: /* Genie III VideoExtension HRG */
        grafyx_write_mode(value);
        break;
      case 0xE0:
        rtc_reg = value >> 4;
        break;
      case 0xF5:
        trs_screen_inverse(value & 1);
        break;
      case 0xF6:
        ctrlimage = value;
        break;
      case 0xF7:
        m6845_crtc(value);
        break;
      case 0xFA:
        eg3200 = value;
        break;
      case 0xFD:
        trs_printer_write(value);
        break;
      default:
        break;
    }
    return;
  }

  /* TCS Genie IIIs */
  if (genie3s) {
    switch (port) {
      case 0x50: /* TRS_HARD_DATA */
      case 0x51: /* TRS_HARD_PRECOMP */
      case 0x52: /* TRS_HARD_SECCNT */
      case 0x53: /* TRS_HARD_SECNUM */
      case 0x54: /* TRS_HARD_CYLLO */
      case 0x55: /* TRS_HARD_CYLHI */
      case 0x56: /* TRS_HARD_SDH */
      case 0x57: /* TRS_HARD_COMMAND */
        trs_hard_out(port + 0x78, value); /* 0xC8 - 0xCF */
        break;
      case 0x5B:
        rtc_reg = value >> 4;
        break;
      case 0xE0:
      case 0xE1:
      case 0xE2:
      case 0xE3:
        trs_disk_select_write(value);
        break;
      case 0xE8:
      case 0xE9:
      case 0xEA:
      case 0xEB:
        trs_printer_write(value);
        break;
      case 0xEC:
        trs_disk_command_write(value);
        break;
      case 0xED:
        trs_disk_track_write(value);
        break;
      case 0xEE:
        trs_disk_sector_write(value);
        break;
      case 0xEF:
        trs_disk_data_write(value);
        break;
      case 0xF1:
        modeimage = value;
        break;
      case 0xF6:
        ctrlimage = value;
        break;
      case 0xF7:
        m6845_crtc(value);
        break;
      case 0xF9:
        genie3s_bank_out(0x100 | value);
        break;
      case 0xFA:
        genie3s_sys_out(value);
        break;
      case 0xFD:
        trs_printer_write(value);
        break;
      case 0xFE:
      case 0xFF:
        modesel = (value >> 3) & 1;
        trs_screen_expanded(modesel);
        trs_cassette_out(value & 0x3);
        break;
      default:
        break;
    }
    return;
  }

  /* Ports common to all TRS-80 models */
  switch (port) {
  case TRS_HARD_WP:       /* 0xC0 */
    if (trs_model == 1 && lubomir) {
      lsb_bank_out(value);
      return;
    }
    /* Fall through */
  case TRS_HARD_CONTROL:  /* 0xC1 */
  case TRS_HARD_DATA:     /* 0xC8 */
  case TRS_HARD_PRECOMP:  /* 0xC9 */
  case TRS_HARD_SECCNT:   /* 0xCA */
  case TRS_HARD_SECNUM:   /* 0xCB */
  case TRS_HARD_CYLLO:    /* 0xCC */
  case TRS_HARD_CYLHI:    /* 0xCD */
  case TRS_HARD_SDH:      /* 0xCE */
  case TRS_HARD_COMMAND:  /* 0xCF */
    trs_hard_out(port, value);
    break;
  case TRS_UART_RESET:    /* 0xE8 */
    trs_uart_reset_out(value);
    break;
  case TRS_UART_BAUD:     /* 0xE9 */
    trs_uart_baud_out(value);
    break;
  case TRS_UART_CONTROL:  /* 0xEA */
    trs_uart_control_out(value);
    break;
  case TRS_UART_DATA:     /* 0xEB */
    trs_uart_data_out(value);
    break;
  case 0x43: /* Alpha Technologies SuperMem */
    if (trs_model < 4 && supermem)
      mem_bank_base(SUPERMEM, value);
    break;
  }

  if (trs_model == 1) {
    /* Next, Model I only */
    switch (port) {
    case 0x00: /* HRG off */
    case 0x01: /* HRG on */
      hrg_onoff(port);
      break;
    case 0x02: /* HRG write address low byte */
      hrg_write_addr(value, 0xFF);
      break;
    case 0x03: /* HRG write address high byte */
      hrg_write_addr(value << 8, 0x3F00);
      break;
    case 0x05: /* HRG write data byte */
      hrg_write_data(value);
      break;
      /* Selector doesn't decode A5 */
    case 0x1F:
    case 0x3F:
      if (selector)
        mem_bank_base(SELECTOR, value);
      break;
    case 0x7E: /* TCS Genie IIs/SpeedMaster RAM 192 B */
      if (speedup == 6)
        mem_bank_base(RAM192B, value);
      return;
    case 0xB5: /* Orchestra-85 right channel */
      trs_orch90_out(2, value);
      break;
    case 0xB9: /* Orchestra-85 left channel */
      trs_orch90_out(1, value);
      break;
    case 0xD0:
    case 0x10: /* Homebrew 80*22 SYS80.SYS */
      if (speedup <= 4)
        ctrlimage = value;
      break;
    case 0xD1:
    case 0x11: /* Homebrew 80*22 SYS80.SYS */
      if (speedup <= 4)
        m6845_crtc(value);
      break;
    case 0xD2:
      if (speedup <= 4)
        s80z_out(value);
      break;
    case 0xDF:
      if (speedup <= 3 && lubomir == 0)
        eg64_mba_out(value);
      break;
    case 0xEC:
      if (lowe_le18)
        lowe_le18_write_data(value);
      else if (speedup == 3) /* Seatronics Super Speed-Up */
        trs_timer_speed(value);
      break;
    case 0xED:
      lowe_le18_write_x(value);
      break;
    case 0xEE:
      lowe_le18_write_y(value);
      break;
    case 0xEF:
      lowe_le18_write_control(value);
      break;
    case 0xF0:
    case 0xF1:
    case 0xF2:
    case 0xF3:
    case 0xF4:
    case 0xF5:
    case 0xF6:
    case 0xF7:
      if (stringy)
        stringy_out(port & 7, value);
      break;
    case 0xF9:
      if (speedup < 4 && trs_rom_size <= 0x2000)
        genie3s_init_out(value);
      break;
    case 0xFA:
      if (speedup < 4 && trs_rom_size <= 0x2000)
        eg3200_init_out(value);
      break;
    case 0xFC:
      if (speedup == 7) /* 6845 CRTC Aster CT-80 */
        ctrlimage = value;
      break;
    case 0xFD:
      if (speedup == 7) /* 6845 CRTC Aster CT-80 */
        m6845_crtc(value);
      else
        /* Printer port of EACA Genie/System 80 */
        trs_printer_write(value);
      break;
    case 0xFE:
      /* Speedup kit or Banking/LNW80/TCS Genie IIs and SpeedMaster */
      if (speedup) {
        if (speedup < 4)
          trs_timer_speed(value);
        else
          sys_byte_out(value);
      }
      break;
    case 0xFF:
      /* screen mode select is on D3 line */
      modesel = (value >> 3) & 1;
      trs_screen_expanded(modesel);
      /* do cassette emulation */
      trs_cassette_motor((value >> 2) & 1);
      trs_cassette_out(value & 0x3);
      /* Lubomir Bits 7 - 5 for EG 64.1 */
      if (lubomir)
        lsb_bank_out(value & 0xE0);
      break;
    default:
      break;
    }

  } else {
    /* Next, Models III/4/4P only */
    switch (port) {
    case 0x50: /* MegaMem memory slot */
    case 0x51:
    case 0x52:
    case 0x60:
    case 0x61:
    case 0x62:
      if (megamem)
        megamem_out(port & 0x0F, value);
      break;
    case 0x5f: /* Sprinter III */
      if (trs_model == 3)
        trs_timer_speed(value);
      break;
    case 0x75: /* Orchestra-90 right channel */
      trs_orch90_out(2, value);
      break;
    case 0x79: /* Orchestra-90 left channel */
      trs_orch90_out(1, value);
      break;
    case 0x80:
      grafyx_write_x(value);
      break;
    case 0x81:
      grafyx_write_y(value);
      break;
    case 0x82:
      grafyx_write_data(value);
      break;
    case 0x83:
      grafyx_write_mode(value);
      break;
    case 0x84:
    case 0x85:
    case 0x86:
    case 0x87:
      if (trs_model >= 4) {
	int changes = value ^ ctrlimage;
	if (changes & 0x80) {
	  mem_video_page(((value & 0x80) >> 7) ? 1024 : 0);
	}
	if (changes & 0x70) {
	  mem_bank((value & 0x70) >> 4);
	}
	if (changes & 0x08) {
	  trs_screen_inverse((value & 0x08) >> 3);
	}
	if (changes & 0x04) {
	  trs_screen_80x24((value & 0x04) >> 2);
	}
	if (changes & 0x03) {
	  mem_map(value & 0x03);
	}
	ctrlimage = value;
      }
      break;
    case 0x8C:
      if (trs_model >= 4) grafyx_write_xoffset(value);
      break;
    case 0x8D:
      if (trs_model >= 4) grafyx_write_yoffset(value);
      break;
    case 0x8E:
      if (trs_model >= 4) grafyx_write_overlay(value);
      break;
    case 0x90:
      /* HyperMem uses bits 4-1 of this port, 0 is the existing
         sound */
      if (trs_model >= 4 && hypermem)
        mem_bank_base(HYPERMEM, value);
      /* Fall through - we affect the sound as well */
    case 0x91:
    case 0x92:
    case 0x93:
      trs_sound_out(value & 1);
      break;
    case 0x94:			/* Huffman memory expansion */
      if (trs_model >= 4 && huffman)
        mem_bank_base(HUFFMAN, value);
      break;
    case 0x9C:
    case 0x9D: /* !!? */
    case 0x9E: /* !!? */
    case 0x9F: /* !!? */
      if (trs_model == 5 /*4p*/) {
	rominimage = value & 1;
	mem_romin(rominimage);
      }
      break;
    case 0xE0:
    case 0xE1:
    case 0xE2:
    case 0xE3:
      trs_interrupt_mask_write(value);
      break;
    case TRSDISK3_INTERRUPT: /* 0xE4 */
    case 0xE5:
    case 0xE7:
      trs_nmi_mask_write(value);
      break;
    case 0xE6:
      /* RTC of Holmes FDC DX-3D board */
      if (trs_model == 3)
        rtc_reg = value >> 4;
      else
        trs_nmi_mask_write(value);
      break;
    case 0xEC:
    case 0xED:
    case 0xEE:
    case 0xEF:
      modeimage = value;
      /* cassette motor is on D1 */
      trs_cassette_motor((modeimage & 0x02) >> 1);
      /* screen mode select is on D2 */
      trs_screen_expanded((modeimage & 0x04) >> 2);
      /* alternate char set is on D3 */
      trs_screen_alternate(!((modeimage & 0x08) >> 3));
      /* Skip clock speed for Holmes Sprinter III & SO-08 (CP-500/M80) */
      if (trs_model == 3) {
        if (speedup == 2 || trs_clones.model == CP500_M80)
          break;
      }
      /* clock speed is on D6; it affects timer HZ too */
      trs_timer_speed(modeimage & 0xC0);
      break;
    case TRSDISK3_COMMAND: /* 0xF0 */
      trs_disk_command_write(value);
      break;
    case TRSDISK3_TRACK: /* 0xF1 */
      trs_disk_track_write(value);
      break;
    case TRSDISK3_SECTOR: /* 0xF2 */
      trs_disk_sector_write(value);
      break;
    case TRSDISK3_DATA: /* 0xF3 */
      trs_disk_data_write(value);
      break;
    case TRSDISK3_SELECT: /* 0xF4 */
    case 0xF5:
    case 0xF6:
    case 0xF7:
      /* This should cause a 1-2us wait in T states... */
      trs_disk_select_write(value);
      break;
    case 0xF8:
    case 0xF9:
    case 0xFA:
    case 0xFB:
      trs_printer_write(value);
      break;
    case 0xFC:
    case 0xFD:
    case 0xFE:
    case 0xFF:
      if (trs_model == 3 && (value & 0x20) && grafyx_get_microlabs()) {
	/* do Model III Micro-Labs graphics card */
	grafyx_m3_write_mode(value);
      } else {
	/* do cassette emulation */
	trs_cassette_out(value & 3);
      }
      break;
    default:
      break;
    }
  }
}

int z80_in(int port)
{
  int value = 0xFF; /* value returned for nonexistent ports */

  /* EG 3200 Genie III */
  if (eg3200) {
    switch (port) {
      case 0x48: /* TRS_HARD_DATA */
      case 0x49: /* TRS_HARD_ERROR */
      case 0x4A: /* TRS_HARD_SECCNT */
      case 0x4B: /* TRS_HARD_SECNUM */
      case 0x4C: /* TRS_HARD_CYLLO */
      case 0x4D: /* TRS_HARD_CYLHI */
      case 0x4E: /* TRS_HARD_SDH */
      case 0x4F: /* TRS_HARD_STATUS */
        value = trs_hard_in(port + 0x80); /* 0xC8 - 0xCF */
        break;
      case 0x82: /* Genie III VideoExtension HRG */
        value = grafyx_read_data();
        break;
      case 0x83: /* Genie III VideoExtension HRG */
        value = grafyx_read_mode();
        break;
      case 0xE0:
        value = rtc_read(rtc_reg);
        break;
      case 0xF7:
        switch (ctrlimage) {
          case 0x0E: /* Cursor LSB */
            value = (cursor_pos >> 8) & 0xFF;
            break;
          case 0x0F: /* Cursor MSB */
            value = (cursor_pos >> 0) & 0xFF;
            break;
        }
        break;
      case 0xFD:
        value = trs_printer_read();
        break;
      default:
        break;
    }
    goto done;
  }

  /* TCS Genie IIIs */
  if (genie3s) {
    switch (port) {
      case 0x50: /* TRS_HARD_DATA */
      case 0x51: /* TRS_HARD_ERROR */
      case 0x52: /* TRS_HARD_SECCNT */
      case 0x53: /* TRS_HARD_SECNUM */
      case 0x54: /* TRS_HARD_CYLLO */
      case 0x55: /* TRS_HARD_CYLHI */
      case 0x56: /* TRS_HARD_SDH */
      case 0x57: /* TRS_HARD_STATUS */
        value = trs_hard_in(port + 0x78); /* 0xC8 - 0xCF */
        break;
      case 0x5A:
        value = rtc_read(rtc_reg);
        break;
      case 0xE0:
      case 0xE1:
      case 0xE2:
      case 0xE3:
        trs_disk_intrq_interrupt(0);
        value = trs_interrupt_latch_read();
        break;
      case 0xE8:
      case 0xE9:
      case 0xEA:
      case 0xEB:
        value = trs_printer_read();
        break;
      case 0xEC:
        value = trs_disk_status_read();
        break;
      case 0xED:
        value = trs_disk_track_read();
        break;
      case 0xEE:
        value = trs_disk_sector_read();
        break;
      case 0xEF:
        value = trs_disk_data_read();
        break;
      case 0xF1:
        value = modeimage;
        break;
      case 0xF7:
        switch (ctrlimage) {
          case 0x0E: /* Cursor LSB */
            value = (cursor_pos >> 8) & 0xFF;
            break;
          case 0x0F: /* Cursor MSB */
            value = (cursor_pos >> 0) & 0xFF;
            break;
        }
        break;
      case 0xF9:
        value = genie3s & 0xFF;
        break;
      case 0xFA:
        value = sys_byte_in();
        break;
      case 0xFD:
        value = trs_printer_read();
        break;
      case 0xFE:
      case 0xFF:
        value = modesel ? 0xBF : 0xFF;
        break;
      default:
        break;
    }
    goto done;
  }

  /* Ports common to all TRS-80 models */
  switch (port) {
  case 0x00:
    value = trs_joystick_in();
    goto done;
  case TRS_HARD_WP:       /* 0xC0 */
  case TRS_HARD_CONTROL:  /* 0xC1 */
  case TRS_HARD_DATA:     /* 0xC8 */
  case TRS_HARD_ERROR:    /* 0xC9 */
  case TRS_HARD_SECCNT:   /* 0xCA */
  case TRS_HARD_SECNUM:   /* 0xCB */
  case TRS_HARD_CYLLO:    /* 0xCC */
  case TRS_HARD_CYLHI:    /* 0xCD */
  case TRS_HARD_SDH:      /* 0xCE */
  case TRS_HARD_STATUS:   /* 0xCF */
    value = trs_hard_in(port);
    goto done;
  case TRS_UART_MODEM:    /* 0xE8 */
    value = trs_uart_modem_in();
    goto done;
  case TRS_UART_SWITCHES: /* 0xE9 */
    value = trs_uart_switches_in();
    goto done;
  case TRS_UART_STATUS:   /* 0xEA */
    value = trs_uart_status_in();
    goto done;
  case TRS_UART_DATA:     /* 0xEB */
    value = trs_uart_data_in();
    goto done;
  case 0x43: /* Supermem memory expansion */
    if (trs_model < 4 && supermem) {
      value = mem_read_bank_base(SUPERMEM);
      goto done;
    }
  }

  /* Support for a special HW real-time clock (TimeDate80?)
   * I used to have.  It was a small card-edge unit with a
   * battery that held the time/date with power off.
   * - Joe Peterson (joe@skyrush.com)
   *
   * According to the LDOS Quarterly 1-6, TChron1, TRSWatch, and
   * TimeDate80 are accessible at high ports 0xB0-0xBC, while
   * T-Timer is accessible at high ports 0xC0-0xCC.  It does
   * not say where the low ports were; Joe's code had 0x70-0x7C,
   * so I presume that's correct at least for the TimeDate80.
   * Newclock-80 (by Alpha Products) uses 0x70-0x7C or 0xB0-0xBC.
   * Note: 0xC0-0xCC conflicts with Radio Shack hard disk, so
   * clock access at these ports is disabled starting in xtrs 4.1.
   *
   * These devices were based on the MSM5832 chip, which returns only
   * a 2-digit year.  It's not clear what software will do with the
   * date in years beyond 1999.
   */
  if ((port >= 0x70 && port <= 0x7C) ||
      (port >= 0xB0 && port <= 0xBC)) {
    value = rtc_read(port);
    goto done;
  } else {
    /* Ports in David Keil's TRS-80 Emulator */
    if (port >= 0x68 && port <= 0x6D) {
      time_t time_secs = time(NULL);
      struct tm *time_info = localtime(&time_secs);

      switch (port) {
        case 0x68:
          value = time_info->tm_sec;
          break;
        case 0x69:
          value = time_info->tm_min;
          break;
        case 0x6A:
          value = time_info->tm_hour;
          break;
        case 0x6B:
          value = (time_info->tm_year + 1900) % 100;
          break;
        case 0x6C:
          value = time_info->tm_mday;
          break;
        case 0x6D:
          value = (time_info->tm_mon) + 1;
          break;
      }
      /* BCD value */
      value = (value / 10 * 16 + value % 10);
      goto done;
    }
  }

  if (trs_model == 1) {
    /* Model I only */
    switch (port) {
#if 0 /* Conflicts with joystick port */
    case 0x00: /* HRG off (undocumented) */
#endif
    case 0x01: /* HRG on (undocumented) */
      hrg_onoff(port);
      goto done;
    case 0x04: /* HRG read data byte */
      value = hrg_read_data();
      goto done;
    case 0x7E: /* TCS Genie IIs/SpeedMaster RAM 192 B */
      if (speedup == 6)
        value = mem_read_bank_base(RAM192B);
      goto done;
    case 0xDF:
      if (speedup <= 3 && lubomir == 0) {
        eg64_mba_out(7);
        value = 0;
      }
      goto done;
    case 0xEC:
      value = lowe_le18_read();
      goto done;
    case 0xF0:
    case 0xF1:
    case 0xF2:
    case 0xF3:
    case 0xF4:
    case 0xF5:
    case 0xF6:
    case 0xF7:
      if (stringy)
        value = stringy_in(port & 7);
      goto done;
    case 0xFD:
      /* GENIE location of printer port */
      value = trs_printer_read();
      goto done;
    case 0xFE:
      if (speedup >= 4)
        value = sys_byte_in();
      goto done;
    case 0xFF:
      value = (!modesel ? 0x7F : 0x3F) | trs_cassette_in();
      goto done;
    }

  } else {
    /* Models III/4/4P only */
    switch (port) {
    case 0x82:
      value = grafyx_read_data();
      goto done;
    case 0x94: /* Huffman memory expansion */
      if (trs_model >= 4 && huffman)
        value = mem_read_bank_base(HUFFMAN);
      goto done;
    case 0x9C: /* !!? */
    case 0x9D: /* !!? */
    case 0x9E: /* !!? */
    case 0x9F: /* !!? */
      if (trs_model == 5 /*4p*/) {
	value = rominimage;
	goto done;
      }
      break;
    case 0xE0:
    case 0xE1:
    case 0xE2:
    case 0xE3:
      value = trs_interrupt_latch_read();
      goto done;
    case 0xE7:
      /* RTC of Holmes FDC DX-3D board */
      if (trs_model == 3)
        value = rtc_read(rtc_reg);
      goto done;
    case 0xEC:
    case 0xED:
    case 0xEE:
    case 0xEF:
      trs_timer_interrupt(0); /* acknowledge */
      value = 0xFF;
      goto done;
    case TRSDISK3_INTERRUPT: /* 0xE4 */
      value = trs_nmi_latch_read();
      goto done;
    case TRSDISK3_STATUS: /* 0xF0 */
      value = trs_disk_status_read();
      goto done;
    case TRSDISK3_TRACK: /* 0xF1 */
      value = trs_disk_track_read();
      goto done;
    case TRSDISK3_SECTOR: /* 0xF2 */
      value = trs_disk_sector_read();
      goto done;
    case TRSDISK3_DATA: /* 0xF3 */
      value = trs_disk_data_read();
      goto done;
    case 0xF4:
      value = cp500_switch_mode(Z80_A);
      goto done;
    case 0xF8:
    case 0xF9:
    case 0xFA:
    case 0xFB:
      value = trs_printer_read() | (ctrlimage & 0x0F);
      goto done;
    case 0xFC:
    case 0xFD:
    case 0xFF:
      value = (modeimage & 0x7E) | trs_cassette_in();
      goto done;
    }
  }

 done:
  if (trs_io_debug_flags & IODEBUG_IN) {
    debug("in (0x%02x) => 0x%02x; pc %04x\n", port, value, z80_state.pc.word);
  }

  return value;
}

void trs_io_save(FILE *file)
{
  trs_save_int(file, &modesel, 1);
  trs_save_int(file, &modeimage, 1);
  trs_save_int(file, &ctrlimage, 1);
  trs_save_int(file, &rominimage, 1);
  trs_save_int(file, &cursor_csr, 1);
  trs_save_int(file, &cursor_cer, 1);
  trs_save_int(file, &cursor_old, 1);
  trs_save_int(file, &cursor_pos, 1);
  trs_save_int(file, &cursor_vis, 1);
  trs_save_int(file, &interlaced, 1);
  trs_save_int(file, &max_raster, 1);
  trs_save_int(file, &start_addr, 1);
  trs_save_int(file, &rtc_reg, 1);
}

void trs_io_load(FILE *file)
{
  trs_load_int(file, &modesel, 1);
  trs_load_int(file, &modeimage, 1);
  trs_load_int(file, &ctrlimage, 1);
  trs_load_int(file, &rominimage, 1);
  trs_load_int(file, &cursor_csr, 1);
  trs_load_int(file, &cursor_cer, 1);
  trs_load_int(file, &cursor_old, 1);
  trs_load_int(file, &cursor_pos, 1);
  trs_load_int(file, &cursor_vis, 1);
  trs_load_int(file, &interlaced, 1);
  trs_load_int(file, &max_raster, 1);
  trs_load_int(file, &start_addr, 1);
  trs_load_int(file, &rtc_reg, 1);
}

