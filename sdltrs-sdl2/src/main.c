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

#include <errno.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SDL.h>
#include "error.h"
#include "load_cmd.h"
#include "trs.h"
#include "trs_disk.h"
#include "trs_sdl_keyboard.h"
#include "trs_state_save.h"

/* Include ROMs */
#include "trs_fakerom.c"
#include "trs_rom4p.c"
#include "trs_romesf.c"

/* Include Model I HDD auto-boot patch */
#include "trs_hd_boot.c"

#define BUFFER_SIZE 256

int trs_model = 1;
const char *program_name;

static int hex_byte(const char *string)
{
    char buf[3];

    buf[0] = string[0];
    buf[1] = string[1];
    buf[2] = '\0';

    return(strtol(buf, (char **)NULL, 16));
}

static int load_hex(FILE *file)
{
    char buffer[BUFFER_SIZE];
    int high = 0;

    while(fgets(buffer, BUFFER_SIZE, file))
    {
	if(buffer[0] == ':')
	{
	    /* colon */
	    const char *b = buffer + 1;

	    /* number of bytes on the line */
	    int num_bytes = hex_byte(b);
	    int check = num_bytes;
	    int address;

	    b += 2;

	    /* the starting address */
	    address = hex_byte(b) << 8;  b += 2;
	    address |= hex_byte(b);  b+= 2;
	    check += (address >> 8) + (address & 0xff);

	    /* a zero? */
	    b += 2;

	    /* the data */
	    if(num_bytes)
	    {
		int value;

		while(num_bytes--)
		{
		    value = hex_byte(b);

		    b += 2;
		    rom_write(address++, value);
		    check += value;
		}
		if (address > high) high = address;

		/* the checksum */
		value = hex_byte(b);
		if(((0x100 - check) & 0xff) != value)
		{
		    return(-1);
		}
	    }
	}
    }
    return high; /* returns highest address loaded + 1 */
}

static void trs_load_compiled_rom(int address, int size, const Uint8 rom[])
{
  int i;

  for (i = 0; i < size; i++)
    rom_write(address++, rom[i]);

  trs_rom_size = address;
}

static int trs_load_rom(const char *filename)
{
  FILE *program;
  int c;

  if (filename[0] == 0)
    return -1;

  if ((program = fopen(filename, "rb")) == NULL) {
    error("failed to load ROM file '%s': %s", filename, strerror(errno));
    return -1;
  }
  c = getc(program);
  if (c == ':') {
    /* Assume Intel hex format */
    rewind(program);
    trs_rom_size = load_hex(program);
    fclose(program);
    if (trs_rom_size == -1) {
      error("ROM file '%s' not in Intel hex format", filename);
      return -1;
    } else
      return 0;
  } else if (c == 1 || c == 5) {
    /* Assume MODELA/III file */
    Uint8 loadrom[Z80_ADDRESS_LIMIT] = { 0 };

    rewind(program);
    if (load_cmd(program, loadrom, NULL, 0, NULL, -1, NULL, NULL, 1) == LOAD_CMD_OK) {
      trs_rom_size = Z80_ADDRESS_LIMIT;
      while (trs_rom_size > 0) {
        if (loadrom[--trs_rom_size] != 0) {
          trs_rom_size++;
          break;
        }
      }
      fclose(program);
      if (trs_rom_size > 0x3800) {
        error("ROM file '%s' size %d exceeds 14 kB", filename, trs_rom_size);
        return -1;
      } else {
        trs_load_compiled_rom(0, trs_rom_size, loadrom);
        return 0;
      }
    } else {
      /* Guess it wasn't one */
      rewind(program);
      c = getc(program);
    }
  }

  /* Assume raw binary */
  trs_rom_size = 0;

  while (c != EOF) {
    rom_write(trs_rom_size++, c);
    c = getc(program);
  }

  return 0;
}

int trs_load_cmd(const char *filename)
{
  FILE *program;
  extern Uint8 memory[];
  int entry;

  if ((program = fopen(filename,"rb")) == NULL) {
    error("failed to load CMD file '%s': %s", filename, strerror(errno));
    return -1;
  }

  if (load_cmd(program, memory, NULL, 0, NULL, -1, NULL, &entry, 1) == LOAD_CMD_OK) {
    debug("entry point of '%s': 0x%x (%d) ...\n", filename, entry, entry);
    if (entry >= 0)
      Z80_PC = entry;
  } else {
    error("unknown CMD format: '%s'", filename);
    fclose(program);
    return -1;
  }

  fclose(program);
  return 0;
}

void trs_rom_init(void)
{
  switch (trs_model) {
    case 1:
      if (trs_load_rom(romfile) != 0)
        trs_load_compiled_rom(0, sizeof(trs_fakerom), trs_fakerom);

      if (stringy)
        trs_load_compiled_rom(0x3000, sizeof(trs_romesf), trs_romesf);

      /* Do not overwrite memory mapped disk I/O */
      if (trs_rom_size > 0x37E0 && trs_disk_controller)
          trs_rom_size = 0x37E0;

      if (trs_hd_boot)
        trs_boot_hd();
      break;
    case 3:
    case 4:
      if (trs_load_rom(romfile3) != 0)
        trs_load_compiled_rom(0, sizeof(trs_fakerom), trs_fakerom);
      break;
    case 5:
      if (trs_load_rom(romfile4p) != 0)
        trs_load_compiled_rom(0, sizeof(trs_rom4p), trs_rom4p);
      break;
  }
  /* Limit ROM size to maximum of 14 kB */
  if (trs_rom_size > 0x3800)
      trs_rom_size = 0x3800;
}

int main(int argc, char *argv[])
{
  int debug = FALSE;
  wordregister x;

  /* program_name must be set first because the error
   * printing routines use it. */
  program_name = strrchr(argv[0], DIR_SLASH);
  if (program_name == NULL) {
    program_name = argv[0];
  } else {
    program_name++;
  }

  x.byte.low = 1;
  x.byte.high = 0;
  if (x.word != 1)
    fatal("Program compiled with wrong ENDIAN: please recompile for this architecture.");
 
#ifdef _WIN32
  SDL_setenv("SDL_AUDIODRIVER", "directsound", 1);
#endif

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_JOYSTICK | SDL_INIT_AUDIO | SDL_INIT_TIMER) != 0)
    fatal("failed to initialize SDL: %s", SDL_GetError());

  if (atexit(trs_sdl_cleanup))
    fatal("failed to register SDL cleanup");

  trs_parse_command_line(argc, argv, &debug);
  trs_set_keypad_joystick();
  trs_open_joystick();
  trs_reset(1);

  if (trs_state_file[0]) {
    trs_state_load(trs_state_file);
    trs_screen_init(1);
  }

  if (trs_cmd_file[0])
    trs_load_cmd(trs_cmd_file);

  if (!debug || fullscreen) {
    /* Run continuously until exit or request to enter debugger */
    z80_run(TRUE);
  }

#ifdef ZBX
  puts("Entering debugger.");
  debug_init();
  debug_shell();
#endif

  puts("Quitting.");
  exit(EXIT_SUCCESS);
}
