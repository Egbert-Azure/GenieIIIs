/*
 * Copyright (C) 2006-2011, Mark Grebe
 * Copyright (C) 2018-2024, Jens Guenther
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 *
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in the
 *    documentation and/or other materials provided with the distribution.
 *
 * THIS SOFTWARE IS PROVIDED BY THE AUTHOR AND CONTRIBUTORS ``AS IS'' AND
 * ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
 * IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE
 * ARE DISCLAIMED.  IN NO EVENT SHALL THE AUTHOR OR CONTRIBUTORS BE LIABLE
 * FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
 * DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
 * OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
 * HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT
 * LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY
 * OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/stat.h>
#include "error.h"
#include "trs.h"
#include "trs_cassette.h"
#include "trs_disk.h"
#include "trs_hard.h"
#include "trs_memory.h"
#include "trs_sdl_gui.h"
#include "trs_sdl_keyboard.h"
#include "trs_state_save.h"
#include "trs_stringy.h"
#include "trs_uart.h"

#define MAX_SCALE   4

#define BLACK       0
#define GREEN       0x344843
#define WHITE       0xe0e0ff

/* Public data */
int foreground;
int background;
int gui_foreground;
int gui_background;
int trs_charset1;
int trs_charset3;
int trs_charset4;
int trs_paused;
int trs_emu_mouse;
int trs_show_led;
int fullscreen;
int grafyx_microlabs;
int lowe_le18;
int mousepointer = 1;
int resize3;
int resize4;
int scale;
int scanlines;
int scanshade;
int window_border_width;
int turbo_paste;
char romfile1[FILENAME_MAX];
char romfile3[FILENAME_MAX];
char romfile4p[FILENAME_MAX];
char trs_disk_dir[FILENAME_MAX];
char trs_hard_dir[FILENAME_MAX];
char trs_cass_dir[FILENAME_MAX];
char trs_disk_set_dir[FILENAME_MAX];
char trs_state_dir[FILENAME_MAX];
char trs_printer_dir[FILENAME_MAX];
char trs_cmd_file[FILENAME_MAX];
char trs_config_file[FILENAME_MAX];
char trs_state_file[FILENAME_MAX];

extern int  aspect_ratio;
extern char scale_quality;

/* Private data */
static int debugger;

static int disksizes[8];
#ifdef __linux
static int disksteps[8];
#endif

static void trs_opt_cass(char *arg, int intarg, int *stringarg);
static void trs_opt_charset(char *arg, int intarg, int *stringarg);
static void trs_opt_clock(char *arg, int intarg, int *stringarg);
static void trs_opt_color(char *arg, int intarg, int *color);
static void trs_opt_disk(char *arg, int intarg, int *stringarg);
static void trs_opt_diskset(char *arg, int intarg, int *stringarg);
static void trs_opt_dirname(char *arg, int intarg, int *stringarg);
static void trs_opt_doubler(char *arg, int intarg, int *stringarg);
#ifdef __linux
static void trs_opt_doublestep(char *arg, int intarg, int *stringarg);
static void trs_opt_stepmap(char *arg, int intarg, int *stringarg);
#endif
static void trs_opt_halt(char *arg, int intarg, int *stringarg);
static void trs_opt_hard(char *arg, int intarg, int *stringarg);
static void trs_opt_intval(char *arg, int intarg, int *stringarg);
static void trs_opt_joybuttonmap(char *arg, int intarg, int *stringarg);
static void trs_opt_joystick(char *arg, int intarg, int *stringarg);
static void trs_opt_memory(char *arg, int intarg, int *variable);
static void trs_opt_model(char *arg, int intarg, int *stringarg);
static void trs_opt_printer(char *arg, int intarg, int *stringarg);
static void trs_opt_rom(char *arg, int intarg, int *stringarg);
static void trs_opt_scalequality(char *arg, int intarg, int *stringarg);
static void trs_opt_serial(char *arg, int intarg, int *stringarg);
static void trs_opt_shiftbracket(char *arg, int intarg, int *stringarg);
static void trs_opt_sizemap(char *arg, int intarg, int *stringarg);
static void trs_opt_speedup(char *arg, int intarg, int *stringarg);
static void trs_opt_switches(char *arg, int intarg, int *stringarg);
static void trs_opt_value(char *arg, int intarg, int *variable);
static void trs_opt_wafer(char *arg, int intarg, int *stringarg);

/* Option handling */
static const struct {
  const char *name;
  void (*handler)(char *, int, int *);
  int hasArg;
  int intArg;
  void *strArg;
} options[] = {
  { "aspectratio",     trs_opt_value,         0, 1, &aspect_ratio         },
  { "background",      trs_opt_color,         1, 0, &background           },
  { "bg",              trs_opt_color,         1, 0, &background           },
  { "borderwidth",     trs_opt_intval,        1, 1, NULL                  },
  { "bw",              trs_opt_intval,        1, 1, NULL                  },
  { "c",               trs_opt_cass,          1, 0, NULL                  },
  { "cass",            trs_opt_cass,          1, 0, NULL                  },
  { "cassdir",         trs_opt_dirname,       1, 0, trs_cass_dir          },
  { "cassette",        trs_opt_cass,          1, 0, NULL                  },
  { "charset",         trs_opt_charset,       1, 0, NULL                  },
  { "charset1",        trs_opt_charset,       1, 1, NULL                  },
  { "charset3",        trs_opt_charset,       1, 3, NULL                  },
  { "charset4",        trs_opt_charset,       1, 4, NULL                  },
  { "cl1",             trs_opt_clock,         1, 1, NULL                  },
  { "cl3",             trs_opt_clock,         1, 3, NULL                  },
  { "cl4",             trs_opt_clock,         1, 4, NULL                  },
  { "clock1",          trs_opt_clock,         1, 1, NULL                  },
  { "clock3",          trs_opt_clock,         1, 3, NULL                  },
  { "clock4",          trs_opt_clock,         1, 4, NULL                  },
  { "cs",              trs_opt_charset,       1, 0, NULL                  },
  { "cs1",             trs_opt_charset,       1, 1, NULL                  },
  { "cs3",             trs_opt_charset,       1, 3, NULL                  },
  { "cs4",             trs_opt_charset,       1, 4, NULL                  },
#ifdef ZBX
  { "debug",           trs_opt_value,         0, 1, &debugger             },
  { "nodebug",         trs_opt_value,         0, 0, &debugger             },
  { "diskdebug",       trs_opt_switches,      1, 1, &trs_disk_debug_flags },
  { "iodebug",         trs_opt_switches,      1, 2, &trs_io_debug_flags   },
#endif
  { "d0",              trs_opt_disk,          1, 0, NULL                  },
  { "d1",              trs_opt_disk,          1, 1, NULL                  },
  { "d2",              trs_opt_disk,          1, 2, NULL                  },
  { "d3",              trs_opt_disk,          1, 3, NULL                  },
  { "d4",              trs_opt_disk,          1, 4, NULL                  },
  { "d5",              trs_opt_disk,          1, 5, NULL                  },
  { "d6",              trs_opt_disk,          1, 6, NULL                  },
  { "d7",              trs_opt_disk,          1, 7, NULL                  },
  { "disk0",           trs_opt_disk,          1, 0, NULL                  },
  { "disk1",           trs_opt_disk,          1, 1, NULL                  },
  { "disk2",           trs_opt_disk,          1, 2, NULL                  },
  { "disk3",           trs_opt_disk,          1, 3, NULL                  },
  { "disk4",           trs_opt_disk,          1, 4, NULL                  },
  { "disk5",           trs_opt_disk,          1, 5, NULL                  },
  { "disk6",           trs_opt_disk,          1, 6, NULL                  },
  { "disk7",           trs_opt_disk,          1, 7, NULL                  },
  { "diskdir",         trs_opt_dirname,       1, 0, trs_disk_dir          },
  { "diskset",         trs_opt_diskset,       1, 0, NULL                  },
  { "disksetdir",      trs_opt_dirname,       1, 0, trs_disk_set_dir      },
  { "doubler",         trs_opt_doubler,       1, 0, NULL                  },
#ifdef __linux
  { "doublestep",      trs_opt_doublestep,    0, 2, NULL                  },
  { "nodoublestep",    trs_opt_doublestep,    0, 1, NULL                  },
  { "stepmap",         trs_opt_stepmap,       1, 0, NULL                  },
#endif
  { "emtsafe",         trs_opt_value,         0, 1, &trs_emtsafe          },
  { "fdc",             trs_opt_value,         0, 1, &trs_disk_controller  },
  { "floppy",          trs_opt_value,         0, 1, &trs_disk_controller  },
  { "fg",              trs_opt_color,         1, 0, &foreground           },
  { "foreground",      trs_opt_color,         1, 0, &foreground           },
  { "fullscreen",      trs_opt_value,         0, 1, &fullscreen           },
  { "fs",              trs_opt_value,         0, 1, &fullscreen           },
  { "guibackground",   trs_opt_color,         1, 0, &gui_background       },
  { "guibg",           trs_opt_color,         1, 0, &gui_background       },
  { "guifg",           trs_opt_color,         1, 0, &gui_foreground       },
  { "guiforeground",   trs_opt_color,         1, 0, &gui_foreground       },
  { "halt",            trs_opt_halt,          1, 0, NULL                  },
  { "h0",              trs_opt_hard,          1, 0, NULL                  },
  { "h1",              trs_opt_hard,          1, 1, NULL                  },
  { "h2",              trs_opt_hard,          1, 2, NULL                  },
  { "h3",              trs_opt_hard,          1, 3, NULL                  },
  { "hard0",           trs_opt_hard,          1, 0, NULL                  },
  { "hard1",           trs_opt_hard,          1, 1, NULL                  },
  { "hard2",           trs_opt_hard,          1, 2, NULL                  },
  { "hard3",           trs_opt_hard,          1, 3, NULL                  },
  { "harddir",         trs_opt_dirname,       1, 0, trs_hard_dir          },
  { "hdboot",          trs_opt_value,         0, 1, &trs_hd_boot          },
  { "hideled",         trs_opt_value,         0, 0, &trs_show_led         },
  { "huffman",         trs_opt_memory,        0, 2, &huffman              },
  { "hypermem",        trs_opt_memory,        0, 3, &hypermem             },
  { "joyaxismapped",   trs_opt_value,         0, 1, &jaxis_mapped         },
  { "joybuttonmap",    trs_opt_joybuttonmap,  1, 0, NULL                  },
  { "joystick",        trs_opt_joystick,      1, 0, NULL                  },
  { "joysticknum",     trs_opt_joystick,      1, 0, NULL                  },
  { "js",              trs_opt_joystick,      1, 0, NULL                  },
  { "keypadjoy",       trs_opt_value,         0, 1, &trs_keypad_joystick  },
  { "kp",              trs_opt_value,         0, 1, &trs_keypad_joystick  },
  { "keystretch",      trs_opt_intval,        1, 2, NULL                  },
  { "ks",              trs_opt_intval,        1, 2, NULL                  },
  { "lc",              trs_opt_value,         0, 1, &lowercase            },
  { "le18",            trs_opt_value,         0, 1, &lowe_le18            },
  { "led",             trs_opt_value,         0, 1, &trs_show_led         },
  { "lower",           trs_opt_value,         0, 1, &lowercase            },
  { "lowercase",       trs_opt_value,         0, 1, &lowercase            },
  { "lubomir",         trs_opt_value,         0, 1, &lubomir              },
  { "microlabs",       trs_opt_value,         0, 1, &grafyx_microlabs     },
  { "m1",              trs_opt_value,         0, 1, &trs_model            },
  { "m3",              trs_opt_value,         0, 3, &trs_model            },
  { "m4",              trs_opt_value,         0, 4, &trs_model            },
  { "m4p",             trs_opt_value,         0, 5, &trs_model            },
  { "megamem",         trs_opt_value,         0, 1, &megamem              },
  { "m",               trs_opt_model,         1, 0, NULL                  },
  { "model",           trs_opt_model,         1, 0, NULL                  },
  { "mouse",           trs_opt_value,         0, 1, &mousepointer         },
  { "mousepointer",    trs_opt_value,         0, 1, &mousepointer         },
  { "mute",            trs_opt_value,         0, 0, &trs_sound            },
  { "noaspectratio",   trs_opt_value,         0, 0, &aspect_ratio         },
  { "noemtsafe",       trs_opt_value,         0, 0, &trs_emtsafe          },
  { "nofdc",           trs_opt_value,         0, 0, &trs_disk_controller  },
  { "nofloppy",        trs_opt_value,         0, 0, &trs_disk_controller  },
  { "nofullscreen",    trs_opt_value,         0, 0, &fullscreen           },
  { "nofs",            trs_opt_value,         0, 0, &fullscreen           },
  { "nohdboot",        trs_opt_value,         0, 0, &trs_hd_boot          },
  { "nohuffman",       trs_opt_value,         0, 0, &huffman              },
  { "nohypermem",      trs_opt_value,         0, 0, &hypermem             },
  { "nojoyaxismapped", trs_opt_value,         0, 0, &jaxis_mapped         },
  { "nokeypadjoy",     trs_opt_value,         0, 0, &trs_keypad_joystick  },
  { "nokp",            trs_opt_value,         0, 0, &trs_keypad_joystick  },
  { "nolc",            trs_opt_value,         0, 0, &lowercase            },
  { "nole18",          trs_opt_value,         0, 0, &lowe_le18            },
  { "noled",           trs_opt_value,         0, 0, &trs_show_led         },
  { "nolower",         trs_opt_value,         0, 0, &lowercase            },
  { "nolowercase",     trs_opt_value,         0, 0, &lowercase            },
  { "nolubomir",       trs_opt_value,         0, 0, &lubomir              },
  { "nomegamem",       trs_opt_value,         0, 0, &megamem              },
  { "nomicrolabs",     trs_opt_value,         0, 0, &grafyx_microlabs     },
  { "nomouse",         trs_opt_value,         0, 0, &mousepointer         },
  { "nomousepointer",  trs_opt_value,         0, 0, &mousepointer         },
  { "noresize3",       trs_opt_value,         0, 0, &resize3              },
  { "noresize4",       trs_opt_value,         0, 0, &resize4              },
  { "nosb",            trs_opt_shiftbracket,  0, 0, NULL                  },
  { "noscan",          trs_opt_value,         0, 0, &scanlines            },
  { "noscanlines",     trs_opt_value,         0, 0, &scanlines            },
  { "noselector",      trs_opt_value,         0, 0, &selector             },
  { "noshiftbracket",  trs_opt_shiftbracket,  0, 0, NULL                  },
  { "nosound",         trs_opt_value,         0, 0, &trs_sound            },
  { "nostringy",       trs_opt_value,         0, 0, &stringy              },
  { "nosupermem",      trs_opt_value,         0, 0, &supermem             },
  { "notruedam",       trs_opt_value,         0, 0, &trs_disk_truedam     },
  { "noturbo",         trs_opt_value,         0, 0, &timer_overclock      },
  { "pause",           trs_opt_value,         0, 1, &trs_paused           },
  { "printer",         trs_opt_printer,       1, 0, NULL                  },
  { "printerdir",      trs_opt_dirname,       1, 0, trs_printer_dir       },
  { "resize3",         trs_opt_value,         0, 1, &resize3              },
  { "resize4",         trs_opt_value,         0, 1, &resize4              },
  { "rom",             trs_opt_rom,           1, 0, NULL                  },
  { "rom1",            trs_opt_rom,           1, 1, NULL                  },
  { "rom3",            trs_opt_rom,           1, 3, NULL                  },
  { "rom4p",           trs_opt_rom,           1, 5, NULL                  },
  { "romfile",         trs_opt_rom,           1, 0, NULL                  },
  { "romfile1",        trs_opt_rom,           1, 1, NULL                  },
  { "romfile3",        trs_opt_rom,           1, 3, NULL                  },
  { "romfile4p",       trs_opt_rom,           1, 5, NULL                  },
  { "s",               trs_opt_speedup,       1, 0, NULL                  },
  { "samplerate",      trs_opt_intval,        1, 3, NULL                  },
  { "sb",              trs_opt_shiftbracket,  0, 1, NULL                  },
  { "sc",              trs_opt_intval,        1, 4, NULL                  },
  { "scale",           trs_opt_intval,        1, 4, NULL                  },
  { "scalequality",    trs_opt_scalequality,  1, 0, NULL                  },
  { "scan",            trs_opt_value,         0, 1, &scanlines            },
  { "scanlines",       trs_opt_value,         0, 1, &scanlines            },
  { "scanshade",       trs_opt_intval,        1, 5, NULL                  },
  { "selector",        trs_opt_memory,        0, 5, &selector             },
  { "serial",          trs_opt_serial,        1, 0, NULL                  },
  { "shiftbracket",    trs_opt_shiftbracket,  0, 1, NULL                  },
  { "showled",         trs_opt_value,         0, 1, &trs_show_led         },
  { "sizemap",         trs_opt_sizemap,       1, 0, NULL                  },
  { "sound",           trs_opt_value,         0, 1, &trs_sound            },
  { "speedup",         trs_opt_speedup,       1, 0, NULL                  },
  { "sr",              trs_opt_intval,        1, 3, NULL                  },
  { "statedir",        trs_opt_dirname,       1, 0, trs_state_dir         },
  { "stringy",         trs_opt_value,         0, 1, &stringy              },
  { "supermem",        trs_opt_memory,        0, 6, &supermem             },
  { "sw",              trs_opt_switches,      1, 0, &trs_uart_switches    },
  { "switches",        trs_opt_switches,      1, 0, &trs_uart_switches    },
  { "tr",              trs_opt_intval,        1, 6, NULL                  },
  { "truedam",         trs_opt_value,         0, 1, &trs_disk_truedam     },
  { "turbo",           trs_opt_value,         0, 1, &timer_overclock      },
  { "turbopaste",      trs_opt_value,         0, 1, &turbo_paste          },
  { "noturbopaste",    trs_opt_value,         0, 0, &turbo_paste          },
  { "turborate",       trs_opt_intval,        1, 6, NULL                  },
  { "w0",              trs_opt_wafer,         1, 0, NULL                  },
  { "w1",              trs_opt_wafer,         1, 1, NULL                  },
  { "w2",              trs_opt_wafer,         1, 2, NULL                  },
  { "w3",              trs_opt_wafer,         1, 3, NULL                  },
  { "w4",              trs_opt_wafer,         1, 4, NULL                  },
  { "w5",              trs_opt_wafer,         1, 5, NULL                  },
  { "w6",              trs_opt_wafer,         1, 6, NULL                  },
  { "w7",              trs_opt_wafer,         1, 7, NULL                  },
  { "wafer0",          trs_opt_wafer,         1, 0, NULL                  },
  { "wafer1",          trs_opt_wafer,         1, 1, NULL                  },
  { "wafer2",          trs_opt_wafer,         1, 2, NULL                  },
  { "wafer3",          trs_opt_wafer,         1, 3, NULL                  },
  { "wafer4",          trs_opt_wafer,         1, 4, NULL                  },
  { "wafer5",          trs_opt_wafer,         1, 5, NULL                  },
  { "wafer6",          trs_opt_wafer,         1, 6, NULL                  },
  { "wafer7",          trs_opt_wafer,         1, 7, NULL                  },
  { "y",               trs_opt_intval,        1, 7, NULL                  },
  { "year",            trs_opt_intval,        1, 7, NULL                  },
};

static const int num_options = sizeof(options) / sizeof(options[0]);

static void strip(char *inputStr)
{
  char *pos = inputStr;

  while (*pos && isspace((unsigned char)*pos))
    pos++;

  memmove(inputStr, pos, strlen(pos) + 1);
  pos = inputStr + strlen(inputStr) - 1;

  while (*pos && isspace((unsigned char)*pos))
    pos--;

  *(pos + 1) = '\0';
}

static const char *charset_name(int charset)
{
  switch (charset) {
    case 0:
      return "early";
    case 1:
      return "stock";
    case 2:
      return "lcmod";
    case 3:
    default:
      return "wider";
    case 4:
    case 7:
      return "katakana";
    case 5:
    case 8:
      return "international";
    case 6:
    case 9:
      return "bold";
    case 10:
      return "genie";
    case 11:
      return "ht-1080z";
    case 12:
      return "meritum";
    case 13:
      return "ct80";
    case 14:
      return "videogenie";
  }
}

static const char *option(int trs_option)
{
  return trs_option ? "" : "no";
}

static void trs_opt_cass(char *arg, int intarg, int *stringarg)
{
  if (arg[0])
    trs_cassette_insert(arg);
  else
    trs_cassette_remove();
}

static void trs_opt_charset(char *arg, int intarg, int *stringarg)
{
  if (intarg == 0)
    intarg = trs_model;

  if (intarg == 1) {
    switch (tolower((int)*arg)) {
      case 'e': /* early */
        trs_charset1 = 0;
        break;
      case 's': /* stock */
        trs_charset1 = 1;
        break;
      case 'l': /* lcmod */
        trs_charset1 = 2;
        break;
      case 'w': /* wider */
        trs_charset1 = 3;
        break;
      case 'g': /* genie or german */
        trs_charset1 = 10;
        break;
      case 'h': /* ht-1080z */
        trs_charset1 = 11;
        break;
      case 'm': /* meritum (uppercase only) */
        trs_charset1 = 12;
        lowercase = 0;
        break;
      case 'c': /* ct-80 */
        trs_charset1 = 13;
        break;
      case 'v': /* video genie */
        trs_charset1 = 14;
        break;
      default:
        error("unknown charset1: '%s'", arg);
    }
  } else {
    int charset;

    switch (tolower((int)*arg)) {
      case 'k': /* katakana */
        charset = 4;
        break;
      case 'i': /* international */
        charset = 5;
        break;
      case 'b': /* bold */
        charset = 6;
        break;
      default:
        error("unknown charset%d: '%s'", intarg, arg);
        return;
    }

    if (intarg == 3)
      trs_charset3 = charset;
    else
      trs_charset4 = charset + 3;
  }
}

static void trs_opt_clock(char *arg, int intarg, int *stringarg)
{
  float const clock_mhz = atof(arg);

  if (clock_mhz >= 0.1 && clock_mhz <= 99.0) {
    switch (intarg) {
      case 1:
        clock_mhz_1 = clock_mhz;
        break;
      case 3:
        clock_mhz_3 = clock_mhz;
        break;
      case 4:
        clock_mhz_4 = clock_mhz;
        break;
    }
  }
}

static void trs_opt_color(char *arg, int intarg, int *color)
{
  *color = strtol(arg, NULL, 16);
}

static void trs_opt_disk(char *arg, int intarg, int *stringarg)
{
  if (arg[0])
    trs_disk_insert(intarg, arg);
  else
    trs_disk_remove(intarg);
}

static void trs_opt_diskset(char *arg, int intarg, int *stringarg)
{
  if (arg[0])
    trs_diskset_load(arg);
}

static void trs_opt_dirname(char *arg, int intarg, int *stringarg)
{
  struct stat st = { 0 };

  if (stat(arg, &st) < 0)
    strcpy(arg, ".");

  if (arg[strlen(arg) - 1] == DIR_SLASH)
    snprintf((char *)stringarg, FILENAME_MAX, "%s", arg);
  else
    snprintf((char *)stringarg, FILENAME_MAX, "%s%c", arg, DIR_SLASH);
}

static void trs_opt_doubler(char *arg, int intarg, int *stringarg)
{
  switch (tolower((int)*arg)) {
    case 'p':
      trs_disk_doubler = TRSDISK_PERCOM;
      break;
    case 'r':
    case 't':
      trs_disk_doubler = TRSDISK_TANDY;
      break;
    case 'b':
    default:
      trs_disk_doubler = TRSDISK_BOTH;
      break;
    case 'n':
      trs_disk_doubler = TRSDISK_NODOUBLER;
      break;
    }
}

#ifdef __linux
static void trs_opt_doublestep(char *arg, int intarg, int *stringarg)
{
  int i;

  for (i = 0; i < 8; i++)
    disksteps[i] = intarg;
}

static void trs_opt_stepmap(char *arg, int intarg, int *stringarg)
{
  sscanf(arg, "%d,%d,%d,%d,%d,%d,%d,%d",
         &disksteps[0], &disksteps[1], &disksteps[2], &disksteps[3],
         &disksteps[4], &disksteps[5], &disksteps[6], &disksteps[7]);
}
#endif

static void trs_opt_halt(char *arg, int intarg, int *stringarg)
{
  if (arg[0])
    z80_halt = tolower((int)*arg);
  else
    z80_halt = 0;
}

static void trs_opt_hard(char *arg, int intarg, int *stringarg)
{
  if (arg[0])
    trs_hard_attach(intarg, arg);
  else
    trs_hard_remove(intarg);
}

static void trs_opt_intval(char *arg, int intarg, int *stringarg)
{
  int value = atoi(arg);

  switch (intarg) {
    case 1: /* borderwidth */
      if (value < 0 || value > 50)
        value = 2;

      window_border_width = value;
      break;
    case 2: /* keystretch */
      if (value < 0)
        value = STRETCH_AMOUNT;

      stretch_amount = value;
      break;
    case 3: /* samplerate */
      if (value < 0 || value > MAX_SAMPLE_RATE)
        value = MAX_SAMPLE_RATE;

      cassette_default_sample_rate = value;
      break;
    case 4: /* scale */
      if (value <= 0)
        value = 1;
      else if (value > MAX_SCALE)
        value = MAX_SCALE;

      scale = value;
      break;
    case 5: /* scanshade */
      scanshade = value & 255;
      break;
    case 6: /* turborate */
      if (value <= 0)
        value = 1;

      timer_overclock_rate = value;
      break;
    case 7: /* year */
      if (value >= 0)
        trs_year = value;
      break;
    default:
      break;
  }
}

static void trs_opt_joybuttonmap(char *arg, int intarg, int *stringarg)
{
  int i;

  for (i = 0; i < N_JOYBUTTONS; i++) {
    char *ptr = strchr(arg, ',');

    if (ptr != NULL)
      *ptr = '\0';

    if (sscanf(arg, "%d", &jbutton_map[i]) == 0)
      jbutton_map[i] = -1;

    if (ptr != NULL)
      arg = ptr + 1;
  }
}

static void trs_opt_joystick(char *arg, int intarg, int *stringarg)
{
  if (strcasecmp(arg, "none") == 0)
    trs_joystick = -1;
  else
    trs_joystick = atoi(arg);
}

static void trs_opt_memory(char *arg, int intarg, int *variable)
{
  *variable = 1;

  switch (intarg) {
    case HUFFMAN:
      hypermem = 0;
      break;
    case HYPERMEM:
      huffman  = 0;
      break;
    case SELECTOR:
      supermem = 0;
      break;
    case SUPERMEM:
      selector = 0;
      break;
  }
}

static void trs_opt_model(char *arg, int intarg, int *stringarg)
{
  if (strcmp(arg, "1") == 0 || strcasecmp(arg, "I") == 0)
    trs_model = 1;
  else if (strcmp(arg, "3") == 0 || strcasecmp(arg, "III") == 0)
    trs_model = 3;
  else if (strcmp(arg, "4") == 0 || strcasecmp(arg, "IV") == 0)
    trs_model = 4;
  else if (strcasecmp(arg, "4P") == 0 || strcasecmp(arg, "IVp") == 0)
    trs_model = 5;
  else
    error("TRS-80 Model '%s' not supported", arg);
}

static void trs_opt_rom(char *arg, int intarg, int *stringarg)
{
  switch (intarg ? intarg : trs_model) {
    case 1:
      snprintf(romfile1, FILENAME_MAX, "%s", arg);
      break;
    case 3:
    case 4:
      snprintf(romfile3, FILENAME_MAX, "%s", arg);
      break;
    case 5:
      snprintf(romfile4p, FILENAME_MAX, "%s", arg);
      break;
   }
}

static void trs_opt_printer(char *arg, int intarg, int *stringarg)
{
  switch (tolower((int)*arg)) {
    case 'n': /* none */
    case '0':
      trs_printer = 0;
      break;
    case 't': /* text */
    case '1':
      trs_printer = 1;
      break;
    default:
      error("unknown printer type: '%s'", arg);
  }
}

static void trs_opt_scalequality(char *arg, int intarg, int *stringarg)
{
  switch (*arg) {
    case '0':
    case '1':
    case '2':
      scale_quality = *arg;
      break;
    default:
      error("unknown render scale quality: %s", arg);
  }
}

static void trs_opt_serial(char *arg, int intarg, int *stringarg)
{
  snprintf(trs_uart_name, FILENAME_MAX, "%s", arg);
}

static void trs_opt_shiftbracket(char *arg, int intarg, int *stringarg)
{
  trs_kb_bracket(intarg);
}

static void trs_opt_sizemap(char *arg, int intarg, int *stringarg)
{
  sscanf(arg, "%d,%d,%d,%d,%d,%d,%d,%d",
         &disksizes[0], &disksizes[1], &disksizes[2], &disksizes[3],
         &disksizes[4], &disksizes[5], &disksizes[6], &disksizes[7]);
}

static void trs_opt_speedup(char *arg, int intarg, int *stringarg)
{
  switch (tolower((int)*arg)) {
    case 'n': /* None */
      speedup = 0;
      break;
    case 'a': /* Archbold */
      speedup = 1;
      break;
    case 'h': /* Holmes */
      speedup = 2;
      break;
    case 's': /* Seatronics */
      speedup = 3;
      break;
    case 'b': /* Banking */
      speedup = 4;
      break;
    case 'c': /* Aster CT-80 */
      speedup = 5;
      break;
    case 'l': /* LNW80 */
      speedup = 6;
      break;
    case 't': /* TCS SpeedMaster */
      speedup = 7;
      break;
    default:
      error("unknown speedup kit: '%s'", arg);
  }
}

static void trs_opt_switches(char *arg, int intarg, int *variable)
{
  int base = 10;

  if (!strncasecmp(arg, "0x", 2))
    base = 16;

  *variable = strtol(arg, NULL, base) & 0xFF;
}

static void trs_opt_value(char *arg, int intarg, int *variable)
{
  *variable = intarg;
}

static void trs_opt_wafer(char *arg, int intarg, int *stringarg)
{
  if (arg[0])
    stringy_insert(intarg, arg);
  else
    stringy_remove(intarg);
}

static void trs_disk_setsizes(void)
{
  int i;

  for (i = 0; i < 8; i++) {
    if (disksizes[i] == 5 || disksizes[i] == 8)
      trs_disk_setsize(i, disksizes[i]);
    else
      error("bad value %d for disk %d size", disksizes[i], i);
  }
}

#ifdef __linux
static void trs_disk_setsteps(void)
{
  int i;

  /* Disk Steps are 1 for Single Step or 2 for Double Step */
  for (i = 0; i < 8; i++) {
    if (disksteps[i] == 1 || disksteps[i] == 2)
      trs_disk_setstep(i, disksteps[i]);
    else
      error("bad value %d for disk %d single/double step", disksteps[i], i);
  }
}
#endif

int trs_load_config_file(void)
{
  char line[FILENAME_MAX];
  char *arg;
  FILE *config_file;
  int i;

  for (i = 0; i < 8; i++)
    trs_disk_remove(i);

  for (i = 0; i < 4; i++)
    trs_hard_remove(i);

  for (i = 0; i < 8; i++)
    stringy_remove(i);

  trs_cassette_remove();

  background = BLACK;
  cassette_default_sample_rate = MAX_SAMPLE_RATE;
  /* Disk Sizes are 5" or 8"
     Corrected by Larry Kraemer 08-01-2011 */
  disksizes[0] = 5;
  disksizes[1] = 5;
  disksizes[2] = 5;
  disksizes[3] = 5;
  disksizes[4] = 8;
  disksizes[5] = 8;
  disksizes[6] = 8;
  disksizes[7] = 8;
  trs_disk_setsizes();
#ifdef __linux
  /* Disk Steps are 1 for Single Step, 2 for Double Step
     Corrected by Larry Kraemer 08-01-2011 */
  disksteps[0] = 1;
  disksteps[1] = 1;
  disksteps[2] = 1;
  disksteps[3] = 1;
  disksteps[4] = 1;
  disksteps[5] = 1;
  disksteps[6] = 1;
  disksteps[7] = 1;
  trs_disk_setsteps();
#endif
  foreground = WHITE;
  fullscreen = 0;
  grafyx_microlabs = 0;
  gui_background = GREEN;
  gui_foreground = WHITE;
  huffman = 0;
  hypermem = 0;
  lowe_le18 = 0;
  lowercase = 1;
  lubomir = 0;
  megamem = 0;
  mousepointer = 1;
  resize3 = 1;
  resize4 = 0;
  scale = 1;
  scanlines = 0;
  scanshade = 127;
  selector = 0;
  strcpy(romfile1, "level2.rom");
  strcpy(romfile3, "model3.rom");
  strcpy(romfile4p, "model4p.rom");
  stringy = 0;
  speedup = 1;
  sprintf(trs_cass_dir, "%c%c", '.', DIR_SLASH);
  sprintf(trs_disk_dir, "%c%c", '.', DIR_SLASH);
  sprintf(trs_disk_set_dir, "%c%c", '.', DIR_SLASH);
  sprintf(trs_hard_dir, "%c%c", '.', DIR_SLASH);
  sprintf(trs_printer_dir, "%c%c", '.', DIR_SLASH);
  sprintf(trs_state_dir, "%c%c", '.', DIR_SLASH);
  stretch_amount = STRETCH_AMOUNT;
  supermem = 0;
  timer_overclock = 0;
  timer_overclock_rate = 5;
  trs_charset1 = 3;
  trs_charset3 = 4;
  trs_charset4 = 8;
  trs_disk_controller = 1;
  trs_disk_doubler = TRSDISK_BOTH;
  trs_disk_truedam = 0;
  trs_emtsafe = 1;
  trs_hd_boot = 0;
  trs_joystick = 0;
  trs_kb_bracket(0);
  trs_keypad_joystick = 1;
  trs_model = 1;
  trs_printer = 0;
  trs_show_led = 1;
  trs_sound = 1;
  trs_uart_switches = 0x7 | TRS_UART_NOPAR | TRS_UART_WORD8;
  trs_year = 0;
  turbo_paste = 0;
  window_border_width = 2;
  z80_halt = 0;

  if (trs_config_file[0] == 0) {
    const char *home = getenv("HOME");

    if (home)
      snprintf(trs_config_file, FILENAME_MAX, "%s/.sdltrs.t8c", home);
    else
      snprintf(trs_config_file, FILENAME_MAX, "./sdltrs.t8c");

    if ((config_file = fopen(trs_config_file, "r")) == NULL) {
      debug("create default configuration: '%s'\n", trs_config_file);
      trs_write_config_file(trs_config_file);
      return -1;
    }
  } else {
    if ((config_file = fopen(trs_config_file, "r")) == NULL) {
      file_error("read configuration '%s'", trs_config_file);
      return -1;
    }
  }

  while (fgets(line, sizeof(line), config_file)) {
    arg = strchr(line, '=');
    if (arg != NULL) {
      *arg++ = '\0';
      strip(arg);
    }

    strip(line);

    for (i = 0; i < num_options; i++) {
      if (strcasecmp(line, options[i].name) == 0) {
        if (options[i].hasArg) {
          if (arg)
            (*options[i].handler)(arg, options[i].intArg, options[i].strArg);
        } else
          (*options[i].handler)(NULL, options[i].intArg, options[i].strArg);
        break;
      }
    }
  }

  fclose(config_file);
  return 0;
}

int trs_parse_command_line(int argc, char **argv)
{
  int i, j, len;

  /* Check for config or state files and CMD file on the command line */
  trs_config_file[0] = 0;
  trs_state_file[0] = 0;
  trs_cmd_file[0] = 0;

  for (i = 1; i < argc; i++) {
    if (argv[i][0] == '-') {
      for (j = 0; j < num_options; j++) {
        if (strcasecmp(&argv[i][1], options[j].name) == 0) {
          if (options[j].hasArg)
            i++;
          break;
        }
      }
    }
    else if ((len = strlen(argv[i]) - 4) > 0) {
      if (strcasecmp(&argv[i][len], ".t8c") == 0)
        snprintf(trs_config_file, FILENAME_MAX, "%s", argv[i]);
      else if (strcasecmp(&argv[i][len], ".t8s") == 0)
        snprintf(trs_state_file, FILENAME_MAX, "%s", argv[i]);
      else if (strcasecmp(&argv[i][len], ".cmd") == 0)
        snprintf(trs_cmd_file, FILENAME_MAX, "%s", argv[i]);
    }
  }

  trs_load_config_file();

  for (i = 1; i < argc; i++) {
    int const argAvail = ((i + 1) < argc); /* is argument available? */

    for (j = 0; j < num_options; j++) {
      if (argv[i][0] == '-') {
        if (strcasecmp(&argv[i][1], options[j].name) == 0) {
          if (options[j].hasArg) {
            if (argAvail)
              (*options[j].handler)(argv[++i], options[j].intArg, options[j].strArg);
          } else
            (*options[j].handler)(NULL, options[j].intArg, options[j].strArg);
          break;
        }
      }
    }

    if (j == num_options && argv[i][0] == '-')
      error("unrecognized option '%s'", argv[i]);
  }

  trs_disk_setsizes();
#ifdef __linux
  trs_disk_setsteps();
#endif
  return debugger;
}

int trs_write_config_file(const char *filename)
{
  FILE *config_file;
  int i;

  if ((config_file = fopen(filename, "w")) == NULL) {
    file_error("write configuration '%s'", filename);
    return -1;
  }

  fprintf(config_file, "%saspectratio\n", option(aspect_ratio));
  fprintf(config_file, "background=0x%x\n", background);
  fprintf(config_file, "borderwidth=%d\n", window_border_width);
  fprintf(config_file, "cassdir=%s\n", trs_cass_dir);
  fprintf(config_file, "cassette=%s\n", trs_cassette_getfilename());
  fprintf(config_file, "charset1=%s\n", charset_name(trs_charset1));
  fprintf(config_file, "charset3=%s\n", charset_name(trs_charset3));
  fprintf(config_file, "charset4=%s\n", charset_name(trs_charset4));
  fprintf(config_file, "clock1=%.2f\n", clock_mhz_1);
  fprintf(config_file, "clock3=%.2f\n", clock_mhz_3);
  fprintf(config_file, "clock4=%.2f\n", clock_mhz_4);

  for (i = 0; i < 8; i++)
    fprintf(config_file, "disk%d=%s\n", i, trs_disk_getfilename(i));

  fprintf(config_file, "diskdir=%s\n", trs_disk_dir);
  fprintf(config_file, "disksetdir=%s\n", trs_disk_set_dir);

  fprintf(config_file, "doubler=%s\n",
      trs_disk_doubler == TRSDISK_PERCOM ? "percom" :
      trs_disk_doubler == TRSDISK_TANDY  ? "tandy"  :
      trs_disk_doubler == TRSDISK_BOTH   ? "both"   : "none");

  fprintf(config_file, "%semtsafe\n", option(trs_emtsafe));
  fprintf(config_file, "%sfdc\n", option(trs_disk_controller));
  fprintf(config_file, "%sfullscreen\n", option(fullscreen));
  fprintf(config_file, "foreground=0x%x\n", foreground);
  fprintf(config_file, "guibackground=0x%x\n", gui_background);
  fprintf(config_file, "guiforeground=0x%x\n", gui_foreground);

  fprintf(config_file, "halt=%s\n",
      z80_halt == 'd' ? "debugger" :
      z80_halt == 'h' ? "halt"     :
      z80_halt == 'r' ? "reset"    : "");

  for (i = 0; i < 4; i++)
    fprintf(config_file, "hard%d=%s\n", i, trs_hard_getfilename(i));

  fprintf(config_file, "harddir=%s\n", trs_hard_dir);
  fprintf(config_file, "%shdboot\n", option(trs_hd_boot));
  fprintf(config_file, "%shuffman\n", option(huffman));
  fprintf(config_file, "%shypermem\n", option(hypermem));
  fprintf(config_file, "%sjoyaxismapped\n", option(jaxis_mapped));

  fprintf(config_file, "joybuttonmap=");
  for (i = 0; i < N_JOYBUTTONS; i++)
    fprintf(config_file, i < N_JOYBUTTONS - 1 ? "%d," : "%d\n", jbutton_map[i]);

  fprintf(config_file, "joystick=");
  if (trs_joystick == -1)
    fprintf(config_file, "none\n");
  else
    fprintf(config_file, "%d\n", trs_joystick);

  fprintf(config_file, "%skeypadjoy\n", option(trs_keypad_joystick));
  fprintf(config_file, "keystretch=%d\n", stretch_amount);
  fprintf(config_file, "%sle18\n", option(lowe_le18));
  fprintf(config_file, "%sled\n", option(trs_show_led));
  fprintf(config_file, "%slowercase\n", option(lowercase));
  fprintf(config_file, "%slubomir\n", option(lubomir));
  fprintf(config_file, "%smegamem\n", option(megamem));
  fprintf(config_file, "%smicrolabs\n", option(grafyx_microlabs));

  fprintf(config_file, "model=%d%s\n",
      trs_model == 5 ? 4 : trs_model, trs_model == 5 ? "P" : "");

  fprintf(config_file, "%smousepointer\n", option(mousepointer));
  fprintf(config_file, "printer=%s\n", trs_printer ? "text" : "none");
  fprintf(config_file, "printerdir=%s\n", trs_printer_dir);
  fprintf(config_file, "%sresize3\n", option(resize3));
  fprintf(config_file, "%sresize4\n", option(resize4));
  fprintf(config_file, "romfile1=%s\n", romfile1);
  fprintf(config_file, "romfile3=%s\n", romfile3);
  fprintf(config_file, "romfile4p=%s\n", romfile4p);
  fprintf(config_file, "samplerate=%d\n", cassette_default_sample_rate);
  fprintf(config_file, "scale=%d\n", scale);
  fprintf(config_file, "scalequality=%c\n", scale_quality);
  fprintf(config_file, "%sscanlines\n", option(scanlines));
  fprintf(config_file, "scanshade=%d\n", scanshade);
  fprintf(config_file, "%sselector\n", option(selector));
  fprintf(config_file, "serial=%s\n", trs_uart_name);
  fprintf(config_file, "%sshiftbracket\n", option(trs_kb_bracket_state));

  fprintf(config_file, "sizemap=%d,%d,%d,%d,%d,%d,%d,%d\n",
      trs_disk_getsize(0), trs_disk_getsize(1), trs_disk_getsize(2), trs_disk_getsize(3),
      trs_disk_getsize(4), trs_disk_getsize(5), trs_disk_getsize(6), trs_disk_getsize(7));

  fprintf(config_file, "%ssound\n", option(trs_sound));

  fprintf(config_file, "speedup=%s\n",
      speedup == 1 ? "archbold"        :
      speedup == 2 ? "holmes"          :
      speedup == 3 ? "seatronics"      :
      speedup == 4 ? "banking"         :
      speedup == 5 ? "ct80"            :
      speedup == 6 ? "lnw80"           :
      speedup == 7 ? "tcs-speedmaster" : "none");

  fprintf(config_file, "statedir=%s\n", trs_state_dir);
#ifdef __linux
  /* Corrected to trs_disk_getstep vs getsize by Larry Kraemer 08-01-2011 */
  fprintf(config_file, "stepmap=%d,%d,%d,%d,%d,%d,%d,%d\n",
      trs_disk_getstep(0), trs_disk_getstep(1), trs_disk_getstep(2), trs_disk_getstep(3),
      trs_disk_getstep(4), trs_disk_getstep(5), trs_disk_getstep(6), trs_disk_getstep(7));
#endif
  fprintf(config_file, "%sstringy\n", option(stringy));
  fprintf(config_file, "%ssupermem\n", option(supermem));
  fprintf(config_file, "switches=0x%x\n", trs_uart_switches);
  fprintf(config_file, "%struedam\n", option(trs_disk_truedam));
  fprintf(config_file, "%sturbo\n", option(timer_overclock));
  fprintf(config_file, "%sturbopaste\n", option(turbo_paste));
  fprintf(config_file, "turborate=%d\n", timer_overclock_rate);

  for (i = 0; i < 8; i++)
    fprintf(config_file, "wafer%d=%s\n", i, stringy_get_name(i));

  fprintf(config_file, "year=%d\n", trs_year);

  fclose(config_file);
  return 0;
}
