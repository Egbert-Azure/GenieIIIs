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

#include <string.h>
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

#ifdef ZBX
int trs_io_debug_flags;
#endif

static int hrg_addr;        /* HRG1B memory address */
static int modesel;         /* Model I/III/4/4P */
static int modeimage = 0x8; /* Model III/4/4P & SIO */
static int m6845_reg;       /* M6845 CRTC register */
static int rtc_reg;         /* RTC address/data */

/* 6845 CRTC */
static struct {
  int csr;
  int cer;
  int old;
  int pos;
  int vis;
  int ilm;
  int ras;
  int sta;
} m6845;

void m6845_crtc_reset(void)
{
  memset(&m6845, 0, sizeof(m6845));
}

static void m6845_crtc(int value)
{
  switch (m6845_reg) {
    case 0x01: /* Chars displayed */
      if (value == 64 || value == 80)
        m6845_screen(value, 0, 0, 0);
      /* Toggle text output */
      m6845_text(value);
      return;
    case 0x06: /* Lines displayed */
      if (m6845.ilm)
        value *= 2;
      if (value <= 32)
        m6845_screen(0, value, 0, 0);
      return;
    case 0x08: /* Interlace Mode */
      m6845.ilm = ((value & 0x03) == 3);
      m6845_screen(0, 0, 0, m6845.ilm ? 1 : 2);
      return;
    case 0x09: /* Maximum Raster address */
      if (value < 16) {
        m6845.ras = value;
        m6845_screen(0, 0, m6845.ras + 1, 0);
      }
      break;
    case 0x0A: /* Cursor visible / Cursor Start Raster */
      m6845.vis = (value & 0x60) != 0x20;
      value &= 0x1F;
      m6845.csr = value > m6845.ras ? m6845.ras : value;
      break;
    case 0x0B: /* Cursor End Raster */
      value &= 0x1F;
      m6845.cer = value > m6845.ras ? m6845.ras : value;
      break;
    case 0x0C: /* Start Address MSB */
      m6845.sta = ((value & 0x3F) << 8) | (m6845.sta & 0x00FF);
      return;
    case 0x0D: /* Start Address LSB */
      m6845.sta = ((value & 0xFF) << 0) | (m6845.sta & 0xFF00);
      switch (trs_clones.model) {
        case CT80:
          ct80_video_addr(m6845.sta);
          return;
        case GENIE3S:
          mem_video_page(1024 - m6845.sta);
          return;
      }
      return;
    case 0x0E: /* Cursor MSB */
      m6845.pos = ((value & 0x3F) << 8) | (m6845.pos & 0x00FF);
      return;
    case 0x0F: /* Cursor LSB */
      m6845.pos = ((value & 0xFF) << 0) | (m6845.pos & 0xFF00);
      if (m6845.pos != m6845.old) {
        if (m6845.vis)
          m6845_cursor(m6845.old - m6845.sta, 0, 0, 0);
        m6845.old = m6845.pos;
      }
      break;
    default:
      return;
  }

  m6845_cursor(m6845.pos - m6845.sta, m6845.csr, m6845.cer, m6845.vis);
}

static int m6845_read(void)
{
  switch (m6845_reg) {
    case 0x0A: /* Cursor Start / Visible */
      return m6845.csr | (m6845.vis ? 0x00 : 0x20);
    case 0x0B: /* Cursor End */
      return m6845.cer;
    case 0x0E: /* Cursor MSB */
      return (m6845.pos >> 8) & 0xFF;
    case 0x0F: /* Cursor LSB */
      return (m6845.pos >> 0) & 0xFF;
    default:
      return 0xFF;
  }
}

static int rtc_read(int port)
{
  const time_t time_secs = time(NULL) + trs_timeoffset;
  const struct tm *time_info = localtime(&time_secs);

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
#ifdef ZBX
  if (trs_io_debug_flags & IODEBUG_OUT)
    debug("[PC=%04X] out (0x%02X), 0x%02X\n", Z80_PC, port, value);
#endif

  if (trs_model == 1) {
    switch (trs_clones.model) {
      case CT80:
        switch (port) {
          case 0xD0:
          case 0xD1:
          case 0xD2:
          case 0xD3:
          case 0xD4:
          case 0xD5:
          case 0xD6:
          case 0xD7:
            ct80_ramdisk_out(port & 7, value);
            return;
          case 0xFC:
            m6845_reg = value;
            return;
          case 0xFD:
            m6845_crtc(value);
            return;
          case 0xFE:
            sys_byte_out(value);
            return;
          case 0xFF:
            modesel = (value >> 3) & 1;
            trs_screen_mode(EXPANDED, modesel);
            trs_cassette_motor((value >> 2) & 1);
            trs_cassette_out(value & 0x3);
            return;
          default:
            break;
        }
        break;

      case EG3200:
        switch (port) {
          case 0x28: /* Genieplus Memory Card */
            eg3200_genieplus_out(value);
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
            trs_screen_mode(INVERSE, value & 1);
            break;
          case 0xF6:
            m6845_reg = value;
            break;
          case 0xF7:
            m6845_crtc(value);
            break;
          case 0xFA:
            sys_byte_write(value & 0x0F);
            break;
          case 0xFD:
            trs_printer_write(value);
            break;
          default:
            break;
        }
        return;

      case GENIE3S:
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
            /* Save SIO for GENIE-DOS 2.4 */
            modeimage = value;
            break;
          case 0xF6:
            m6845_reg = value;
            break;
          case 0xF7:
            m6845_crtc(value);
            break;
          case 0xF9:
            genie3s_bank_out(value);
            break;
          case 0xFA:
            genie3s_sys_out(value);
            break;
          case 0xFB:
            genie3s_mem_out(value);
            break;
          case 0xFD:
            trs_printer_write(value);
            break;
          case 0xFE:
          case 0xFF:
            modesel = (value >> 3) & 1;
            trs_screen_mode(EXPANDED, modesel);
            trs_cassette_out(value & 0x3);
            break;
          default:
            break;
        }
        return;

      case LNW80:
        switch (port) {
          case 0x1F:
            lnw80_bank_out(value);
            return;
          case 0xFE:
            sys_byte_out(value);
            return;
          case 0xFF:
            modesel = (value >> 3) & 1;
            trs_screen_mode(EXPANDED, modesel);
            trs_cassette_motor((value >> 2) & 1);
            trs_cassette_out(value & 0x3);
            return;
          default:
            break;
        }
        break;

      case SPEEDMASTER:
        switch (port) {
          case 0x5B:
            rtc_reg = value >> 4;
            break;
          case 0x7E:
            tcs_ram192b_out(value);
            break;
          case 0xFE:
            sys_byte_out(value);
            break;
          case 0xFF:
            modesel = (value >> 3) & 1;
            trs_screen_mode(EXPANDED, modesel);
            trs_cassette_out(value & 0x3);
            break;
          default:
            break;
        }
        return;

      default:
        /* Next, Model I only */
        switch (port) {
          case 0x00: /* HRG off */
          case 0x01: /* HRG on */
            hrg_onoff(port);
            return;
          case 0x02: /* HRG write address low byte */
            hrg_addr = ((value & 0xFF) << 0) | (hrg_addr & 0xFF00);
            return;
          case 0x03: /* HRG write address high byte */
            hrg_addr = ((value & 0x3F) << 8) | (hrg_addr & 0x00FF);
            return;
          case 0x05: /* HRG write data byte */
            hrg_write_data(hrg_addr, value);
            return;
            /* Selector doesn't decode A5 */
          case 0x1F:
          case 0x3F:
            if (selector)
              selector_out(value);
            return;
          /* X-MEM/80 */
          case 0x41: /* lower 16K 8000-BFFF */
          case 0x42: /* upper 16K C000-FFFF */
          case 0x43: /* both  16K same page */
            if (xmem80) {
              xmem80_out(port, value & 0x1F);
              return;
            }
            break;
          case 0x85: /* Real Time Clock/Calendar Card */
            rtc_reg = value;
            return;
          case 0xB5: /* Orchestra-85 right channel */
            trs_orch90_out(2, value);
            return;
          case 0xB9: /* Orchestra-85 left channel */
            trs_orch90_out(1, value);
            return;
          case 0xC0: /* Lubomir Soft Banker (EG 64.3) */
            if (lubomir)
              lubomir_out(value);
            return;
          case 0xD0: /* Schmidtke 80-Z Card: 6845 CRTC */
          case 0x10: /* Homebrew 80*22 SYS80.SYS */
            m6845_reg = value;
            return;
          case 0xD1: /* Schmidtke 80-Z Card: 6845 CRTC */
          case 0x11: /* Homebrew 80*22 SYS80.SYS */
            m6845_crtc(value);
            return;
          case 0xD2: /* Schmidtke 80-Z Card: Banking */
            s80z_out(value);
            return;
          case 0xDF:
            if (speedup <= 3 && lubomir == 0)
              eg64_mba_out(value);
            return;
          case 0xEC:
            if (lowe_le18)
              lowe_le18_write_data(value);
            if (speedup == 3) /* Seatronics Super Speed-Up */
              trs_timer_speed(value & 0xC0);
            return;
          case 0xED:
            if (lowe_le18) grafyx_write_x(value & 63);
            return;
          case 0xEE:
            if (lowe_le18) grafyx_write_y(value);
            return;
          case 0xEF:
            if (lowe_le18) lowe_le18_write_control(value);
            return;
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
            return;
          case 0xF9:
            if (speedup < 4 && trs_rom_size <= 0x2000)
              genie3s_init_out(value);
            return;
          case 0xFA:
            if (speedup < 4 && trs_rom_size <= 0x2000)
              eg3200_init_out(value);
            return;
          case 0xFC:
            /* Printer port of EACA Genie/System 80 */
            trs_printer_write(value);
            return;
          case 0xFE:
            /* Speedup kit or Banking */
            if (speedup) {
              if (speedup < 4)
                trs_timer_speed(value);
              else
                sys_byte_out(value);
            }
            return;
          case 0xFF:
            /* screen mode select is on D3 line */
            modesel = (value >> 3) & 1;
            trs_screen_mode(EXPANDED, modesel);
            trs_cassette_motor((value >> 2) & 1);
            trs_cassette_out(value & 0x3);
            return;
          default:
            break;
      }
    }
  } else {
    if (trs_model == 3) {
      switch (port) {
        case 0x3C:
          m6845_reg = value;
          return;
        case 0x3D:
          m6845_crtc(value);
          return;
        case 0x3F:
          vid80_vx3_out(value);
          return;
        case 0x5F: /* Sprinter III */
          trs_timer_speed(value);
          return;
        case 0x80:
        case 0x81:
        case 0x82:
        case 0x83:
          if (grafyx_microlabs) return;
          break;
        case 0xE6:
          /* RTC of Holmes FDC DX-3D board */
          rtc_reg = value >> 4;
          return;
        case 0xFC:
        case 0xFD:
        case 0xFE:
        case 0xFF:
          if (value & 0x20 && grafyx_microlabs)
            /* do Model III Micro-Labs graphics card */
            grafyx_m3_write_mode(value);
          else
            trs_cassette_out(value & 3);
          return;
        default:
          break;
      }
    } else {
      switch (port) {
        case 0x84:
        case 0x85:
        case 0x86:
        case 0x87:
        {
          int const changes = value ^ modesel;

          if (changes & 0x80)
            mem_video_page((value & 0x80) << 3);

          if (changes & 0x70)
            mem_bank((value & 0x70) >> 4);

          if (changes & 0x08)
            trs_screen_mode(INVERSE, (value & 0x08) >> 3);

          if (changes & 0x04)
            trs_screen_80x24((value & 0x04) >> 2);

          if (changes & 0x03)
            mem_map(value & 0x03);

          modesel = value;
        }
        return;
      case 0x8C:
        grafyx_write_xoffset(value);
        return;
      case 0x8D:
        grafyx_write_yoffset(value);
        return;
      case 0x8E:
        grafyx_write_overlay(value);
        return;
      case 0x90:
        /* HyperMem uses bits 4-1 of this port, 0 is the existing
           sound */
        if (hypermem)
          hypermem_out(value);
        /* Fall through - we affect the sound as well */
      case 0x91:
      case 0x92:
      case 0x93:
        trs_sound_out(value & 1);
        return;
      case 0x94: /* Huffman memory expansion */
        if (huffman)
          huffman_out(value);
        return;
      case 0x9C:
      case 0x9D:
      case 0x9E:
      case 0x9F:
        if (trs_model == 5) /* Switch 4P boot ROM in/out */
          sys_byte_write(value & 1);
        return;
      default:
        break;
      }
    }
    switch (port) {
      case 0x50: /* MegaMem memory slot */
      case 0x51:
      case 0x52:
      case 0x60:
      case 0x61:
      case 0x62:
        if (megamem)
          megamem_out(port & 0x0F, value);
        return;
      case 0x75: /* Orchestra-90 right channel */
        trs_orch90_out(2, value);
        return;
      case 0x79: /* Orchestra-90 left channel */
        trs_orch90_out(1, value);
        return;
      case 0x80:
        grafyx_write_x(value);
        return;
      case 0x81:
        grafyx_write_y(value);
        return;
      case 0x82:
        grafyx_write_data(value);
        return;
      case 0x83:
        grafyx_write_mode(value);
        return;
      case 0xE0:
      case 0xE1:
      case 0xE2:
      case 0xE3:
        trs_interrupt_mask_write(value);
        return;
      case TRSDISK3_INTERRUPT: /* 0xE4 */
      case 0xE5:
      case 0xE6:
      case 0xE7:
        trs_nmi_mask_write(value);
        return;
      case 0xEC:
      case 0xED:
      case 0xEE:
      case 0xEF:
        if (value == modeimage)
          return;
        modeimage = value;
        /* cassette motor is on D1 */
        trs_cassette_motor((value & 0x02) >> 1);
        /* screen mode select is on D2 */
        trs_screen_mode(EXPANDED, (value & 0x04) >> 2);
        /* alternate char set is on D3 */
        trs_screen_mode(ALTERNATE, !((value & 0x08) >> 3));
        /* Skip clock speed for Holmes Sprinter III & SO-08 (CP-500/M80) */
        if (trs_model == 3) {
          if (speedup == 2 || trs_clones.model == CP500_M80)
            return;
        }
        /* clock speed is on D6; it affects timer HZ too */
        trs_timer_speed(value & 0xC0); /* D6/D7 for Seatronic */
        return;
      case TRSDISK3_COMMAND: /* 0xF0 */
        trs_disk_command_write(value);
        return;
      case TRSDISK3_TRACK:   /* 0xF1 */
        trs_disk_track_write(value);
        return;
      case TRSDISK3_SECTOR:  /* 0xF2 */
        trs_disk_sector_write(value);
        return;
      case TRSDISK3_DATA:    /* 0xF3 */
        trs_disk_data_write(value);
        return;
      case TRSDISK3_SELECT:  /* 0xF4 */
      case 0xF5:
      case 0xF6:
      case 0xF7:
        /* This should cause a 1-2us wait in T states... */
        trs_disk_select_write(value);
        return;
      case 0xF8:
      case 0xF9:
      case 0xFA:
      case 0xFB:
        trs_printer_write(value);
        return;
      case 0xFC:
      case 0xFD:
      case 0xFE:
      case 0xFF:
        trs_cassette_out(value & 3);
        return;
      default:
        break;
    }
  }

  /* Ports common to all TRS-80 models */
  switch (port) {
    case 0x43: /* Alpha Technology SuperMem */
      if ((value & 0x1F) < supermem)
        supermem_out(value);
      break;
    case TRS_HARD_WP:       /* 0xC0 */
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
      trs_uart_reset_out();
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
  }
}

int z80_in(int port)
{
  int value = 0xFF; /* value returned for nonexistent ports */

  if (trs_model == 1) {
    switch (trs_clones.model) {
      case CT80:
        switch (port) {
          case 0xD0:
          case 0xD1:
          case 0xD2:
          case 0xD3:
          case 0xD4:
          case 0xD5:
          case 0xD6:
          case 0xD7:
            value = ct80_ramdisk_in(port & 7);
            goto done;
          case 0xFF:
            value = (modesel ? 0x3F : 0x7F) | trs_cassette_in();
            goto done;
          default:
            break;
        }
        break;

      case EG3200:
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
            value = m6845_read();
            break;
          case 0xFD:
            value = trs_printer_read();
            break;
          default:
            break;
        }
        goto done;

      case GENIE3S:
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
            value = genie3s_latch_read(trs_interrupt_latch_read());
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
            /* Saved SIO baud rate */
            value = modeimage;
            break;
          case 0xF7:
            value = m6845_read();
            break;
          case 0xF9:
            value = sys_byte_2_in();
            break;
          case 0xFA:
            value = sys_byte_in();
            break;
          case 0xFB:
            value = get_mem_cmd();
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

      case LNW80:
        switch (port) {
          case 0xFE:
            value = sys_byte_in();
            goto done;
          case 0xFF:
            value = (modesel ? 0x3F : 0x7F) | trs_cassette_in();
            goto done;
          default:
            break;
        }
        break;

      case SPEEDMASTER:
        switch (port) {
          case 0x5A:
            value = rtc_read(rtc_reg);
            break;
          case 0x7E:
            value = get_mem_cmd();
            break;
          case 0xFE:
            value = sys_byte_in();
            break;
          case 0xFF:
            value = (modesel ? 0x3F : 0x7F) | trs_cassette_in();
            break;
          default:
            break;
        }
        goto done;

      default:
        /* Model I only */
        switch (port) {
#if 0 /* Conflicts with joystick port */
          case 0x00: /* HRG off (undocumented) */
#endif
          case 0x01: /* HRG on (undocumented) */
            hrg_onoff(port);
            goto done;
          case 0x04: /* HRG read data byte */
            value = hrg_read_data(hrg_addr);
            goto done;
          /* X-MEM/80 */
          case 0x41: /* lower 16K 8000-BFFF */
          case 0x42: /* upper 16K C000-FFFF */
          case 0x43: /* both  16K same page */
            if (xmem80) {
              value = xmem80_in(port);
              goto done;
            }
            break;
          case 0x84: /* RTCC */
            value = rtc_read(rtc_reg);
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
            /* Printer port of EACA Genie/System 80 */
            value = trs_printer_read();
            goto done;
          case 0xFF:
            value = (modesel ? 0x3F : 0x7F) | trs_cassette_in();
            goto done;
        }
    }

  } else {
    /* Models III/4/4P only */
    switch (port) {
      case 0x3D:
        if (trs_model == 3)
          value = m6845_read();
        goto done;
      case 0x3F:
        if (trs_model == 3)
          value = sys_byte_in();
        goto done;
      case 0x82:
        value = grafyx_read_data();
        goto done;
      case 0x94: /* Huffman memory expansion */
        if (trs_model >= 4 && huffman)
          value = huffman_in();
        goto done;
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
        goto done;
      case TRSDISK3_INTERRUPT: /* 0xE4 */
        value = trs_nmi_latch_read();
        goto done;
      case TRSDISK3_STATUS:    /* 0xF0 */
        value = trs_disk_status_read();
        goto done;
      case TRSDISK3_TRACK:     /* 0xF1 */
        value = trs_disk_track_read();
        goto done;
      case TRSDISK3_SECTOR:    /* 0xF2 */
        value = trs_disk_sector_read();
        goto done;
      case TRSDISK3_DATA:      /* 0xF3 */
        value = trs_disk_data_read();
        goto done;
      case 0xF4:
        if (trs_model == 3)
          value = cp500_switch_mode(Z80_A);
        goto done;
      case 0xF8:
      case 0xF9:
      case 0xFA:
      case 0xFB:
        value = trs_printer_read() | (modesel & 0x0F);
        goto done;
      case 0xFC:
      case 0xFD:
      case 0xFF:
        value = (modeimage & 0x7E) | trs_cassette_in();
        goto done;
    }
  }

  /* Ports common to all TRS-80 models */
  switch (port) {
    case 0x00:
      value = trs_joystick_in();
      goto done;
    case 0x43: /* Alpha Technology SuperMem */
      if (supermem)
        value = supermem_in();
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
  } else {
    /* Ports in David Keil's TRS-80 Emulator */
    if (port >= 0x68 && port <= 0x6D) {
      const time_t time_secs = time(NULL) + trs_timeoffset;
      const struct tm *time_info = localtime(&time_secs);

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
    }
  }

done:
#ifdef ZBX
  if (trs_io_debug_flags & IODEBUG_IN)
    debug("[PC=%04X] in (0x%02X) = 0x%02X\n", Z80_PC, port, value);
#endif

  return value;
}

void trs_io_save(FILE *file)
{
  trs_save_int(file, &hrg_addr, 1);
  trs_save_int(file, &modesel, 1);
  trs_save_int(file, &modeimage, 1);
  trs_save_int(file, &m6845_reg, 1);
  trs_save_int(file, &m6845.csr, 1);
  trs_save_int(file, &m6845.cer, 1);
  trs_save_int(file, &m6845.old, 1);
  trs_save_int(file, &m6845.pos, 1);
  trs_save_int(file, &m6845.vis, 1);
  trs_save_int(file, &m6845.ilm, 1);
  trs_save_int(file, &m6845.ras, 1);
  trs_save_int(file, &m6845.sta, 1);
  trs_save_int(file, &rtc_reg, 1);
}

void trs_io_load(FILE *file)
{
  trs_load_int(file, &hrg_addr, 1);
  trs_load_int(file, &modesel, 1);
  trs_load_int(file, &modeimage, 1);
  trs_load_int(file, &m6845_reg, 1);
  trs_load_int(file, &m6845.csr, 1);
  trs_load_int(file, &m6845.cer, 1);
  trs_load_int(file, &m6845.old, 1);
  trs_load_int(file, &m6845.pos, 1);
  trs_load_int(file, &m6845.vis, 1);
  trs_load_int(file, &m6845.ilm, 1);
  trs_load_int(file, &m6845.ras, 1);
  trs_load_int(file, &m6845.sta, 1);
  trs_load_int(file, &rtc_reg, 1);
}

