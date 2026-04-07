/* Copyright (c): 2006, Mark Grebe */

/* Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
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

/*
   Modified by Mark Grebe, 2006
   Last modified on Wed May 07 09:12:00 MST 2006 by markgrebe
*/

#ifndef _TRS_SDL_GUI_H
#define _TRS_SDL_GUI_H

#ifdef _WIN32
#undef  INTERFACE
#endif

#define GUI          (-10)
#define KEYBRD       (-11)
#define SAVE         (-12)
#define LOAD         (-13)
#define RESET        (-14)
#define EXIT         (-15)
#define PAUSE        (-16)
#define JOYGUI       (-17)
#define DISK         (-18)
#define HARD         (-19)
#define STRINGY      (-20)
#define TAPE         (-21)
#define READ         (-22)
#define WRITE        (-23)
#define EMULATOR     (-24)
#define INTERFACE    (-25)
#define OTHER        (-26)
#define KEYS         (-27)
#define EXEC         (-28)
#define SAVE_BMP     (-29)

#define CAS          ".cas.cpt.wav"
#define CMD          ".cmd"
#define DSK          ".dmk.dsk.jv1.jv3"
#define ESF          ".esf"
#define HDV          ".hdv"
#define ROM          ".bin.hex.iii.rom"
#define SET          ".set"
#define T8C          ".t8c"
#define T8S          ".t8s"

extern int jbutton_map[];
extern int jaxis_mapped;
extern int mousepointer;
extern int scanlines;
extern int scanshade;

#if defined(SDL2) || !defined(NOX)
extern int turbo_paste;
#endif

extern int  gui_exit(void);
extern int  gui_file(const char *path, char *filename, const char *mask, int dir, const char *type);
extern void gui_function(int function);
extern void gui_pause(void);
extern void gui_rect(int x, int y, int w, int h, int frame);
extern void gui_text(const char *text, int col, int row, int len, int font);
extern int  trs_sdl_savebmp(const char *name);

#endif /* _TRS_SDL_GUI_H */
