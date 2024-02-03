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

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SDL.h>
#include "error.h"
#include "trs.h"
#include "trs_sdl_keyboard.h"
#include "trs_state_save.h"

const char *program_name;

int main(int argc, char *argv[])
{
  int zbx = 0;
  wordregister x;

  /* program_name must be set first because the error
   * printing routines use it. */
  program_name = strrchr(argv[0], DIR_SLASH);
  if (program_name == NULL)
    program_name = argv[0];
  else
    program_name++;

  x.byte.low = 1;
  x.byte.high = 0;
  if (x.word != 1)
    fatal("Program compiled with wrong ENDIAN: please recompile for this architecture.");
 
#ifdef _WIN32
  SDL_setenv("SDL_AUDIODRIVER", "directsound", 1);
#endif

  if (SDL_Init(SDL_INIT_VIDEO | SDL_INIT_JOYSTICK | SDL_INIT_AUDIO | SDL_INIT_TIMER) != 0)
    fatal("SDL_Init failed: %s", SDL_GetError());

  if (atexit(SDL_Quit))
    fatal("failed to register SDL_Quit.");

  zbx = trs_parse_command_line(argc, argv);

  trs_set_keypad_joystick();
  trs_open_joystick();
  trs_reset(1);

  if (trs_state_file[0]) {
    trs_state_load(trs_state_file);
    trs_screen_init(1);
  }

  if (trs_cmd_file[0])
    trs_load_cmd(trs_cmd_file);

  if (!zbx || fullscreen)
    /* Run continuously until exit or request to enter debugger */
    z80_run(1);

#ifdef ZBX
  puts("Entering debugger.");
  debug_shell();
#endif

  puts("Quitting.");
  exit(EXIT_SUCCESS);
}
