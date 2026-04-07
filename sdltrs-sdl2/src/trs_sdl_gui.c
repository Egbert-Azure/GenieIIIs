/*
 * Copyright (C) 2006-2011, Mark Grebe
 * Copyright (C) 2018-2026, Jens Guenther
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

#include <dirent.h>
#include <errno.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>
#include <sys/stat.h>
#include <unistd.h>
#ifdef __OS2__
#include <os2.h>
#endif
#ifdef _WIN32
#include <windows.h>
#endif
#include <SDL.h>
#include "error.h"
#include "trs.h"
#include "trs_cassette.h"
#include "trs_disk.h"
#include "trs_hard.h"
#include "trs_memory.h"
#include "trs_mkdisk.h"
#include "trs_sdl_gui.h"
#include "trs_sdl_keyboard.h"
#include "trs_state_save.h"
#include "trs_stringy.h"
#include "trs_uart.h"

#define ENTRY         5
#define TITLE         6
#define SAVE_SET      7
#define LOAD_SET      8

#define MAX_JOYSTICKS 8

#define N_KEYS        52
#define SHIFT         39

static char filename[FILENAME_MAX];
static char **filelist;
static int filelistcount;
static int filelistsize;

typedef struct menu_entry {
  char text[64];
  int const type;
} MENU;

static const char *drives[] = {
  " None",
  "    0",
  "    1",
  "    2",
  "    3",
  "    4",
  "    5",
  "    6",
  "    7"
};

static const char *yes_no[] = {
  "        No",
  "       Yes"
};

static const char *function_menu[] = {
  "  GUI Menu  ", "  Keyboard  ",
  " Save State ", " Load State ",
  "   Reset    ", "    Quit    ",
  "   Pause    ", "Joystick GUI"
};

static int const function_codes[] = {
  GUI,   KEYBRD,
  SAVE,  LOAD,
  RESET, EXIT,
  PAUSE, JOYGUI
};

static const char *key_names[] = {
  " 1 ", " 2 ", " 3 ", " 4 ", " 5 ", " 6 ", " 7 ", " 8 ", " 9 ", " 0 ", " : ", " - ", "BRK",
  " UP", " q ", " w ", " e ", " r ", " t ", " y ", " u ", " i ", " o ", " p ", "LFT", "RGT",
  "DWN", " a ", " s ", " d ", " f ", " g ", " h ", " j ", " k ", " l ", " ; ", "ENT", "CLR",
  "SHF", " z ", " x ", " c ", " v ", " b ", " n ", " m ", " , ", " . ", " / ", " @ ", "SPC"
};
static int const key_syms[] = {
  SDLK_1, SDLK_2, SDLK_3, SDLK_4, SDLK_5, SDLK_6, SDLK_7, SDLK_8, SDLK_9,     SDLK_0,      SDLK_COLON,     SDLK_MINUS,  SDLK_ESCAPE,
  0x111,  SDLK_q, SDLK_w, SDLK_e, SDLK_r, SDLK_t, SDLK_y, SDLK_u, SDLK_i,     SDLK_o,      SDLK_p,         0x114,       0x113,
  0x112,  SDLK_a, SDLK_s, SDLK_d, SDLK_f, SDLK_g, SDLK_h, SDLK_j, SDLK_k,     SDLK_l,      SDLK_SEMICOLON, SDLK_RETURN, 0x116,
  -1,     SDLK_z, SDLK_x, SDLK_c, SDLK_v, SDLK_b, SDLK_n, SDLK_m, SDLK_COMMA, SDLK_PERIOD, SDLK_SLASH,     SDLK_AT,     SDLK_SPACE
};
static const char *key_names_shifted[] = {
  " ! ", " \" ", " # ", " $ ", " % ", " & ", " ' ", " ( ", " ) ", " _ ", " * ", " = ", " ~ ",
  "TAB", " Q ",  " W ", " E ", " R ", " T ", " Y ", " U ", " I ", " O ", " P ", " [ ", " ] ",
  " ^ ", " A ",  " S ", " D ", " F ", " G ", " H ", " J ", " K ", " L ", " + ", " { ", " } ",
  "SHF", " Z ",  " X ", " C ", " V ", " B ", " N ", " M ", " < ", " > ", " ? ", " \\ ", " | "
};
static int const key_syms_shifted[] = {
  SDLK_EXCLAIM, SDLK_QUOTEDBL, SDLK_HASH, SDLK_DOLLAR, 0x25, SDLK_AMPERSAND, SDLK_QUOTE, SDLK_LEFTPAREN, SDLK_RIGHTPAREN, SDLK_UNDERSCORE, SDLK_ASTERISK, SDLK_EQUALS, SDLK_CARET,
  SDLK_TAB,     0x51,          0x57,      0x45,        0x52, 0x54,           0x59,       0x55,           0x49,            0x4f,            0x50,          0xc4,        0xdc,
  0x7e,         0x41,          0x53,      0x44,        0x46, 0x47,           0x48,       0x4a,           0x4b,            0x4c,            SDLK_PLUS,     0xe4,        0xfc,
  -1,           0x5a,          0x58,      0x43,        0x56, 0x42,           0x4e,       0x4d,           SDLK_LESS,       SDLK_GREATER,    SDLK_QUESTION, 0xd6,        0xf6
};

int jbutton_map[] = {-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1};
int jaxis_mapped;

static void gui_write(const char *text, int x, int y, int font);
static void gui_clear(void);
static void gui_limit(const char *text, char *limited, int limit);
static void gui_add_extension(char *name, const char *ext);
static int  gui_cmp_extension(const char *mask, const char *ext);
static int  gui_key(void);
static int  gui_select(const char *text, int x, int y);
static void gui_error(const char *name);
static void gui_message(const char *title, const char *message);
static void gui_delete_filelist(void);
static void gui_add_to_filelist(char *name);
static int  gui_filename_cmp(const void *nptr1, const void *nptr2);
static int  gui_read_dir(const char *path, const char *mask, int dir);
static int  gui_input(const char *title, const char *input, char *output, int limit, int dir);
static int  gui_menu(const char *title, const MENU *entry, int selection);
static int  gui_popup(const char *title, const char **entry, int num, int selection);
static int  gui_matrix(const char *title, const char **entry, int rows, int cols, int selection);
static int  gui_question(const char *text);
static int  gui_file_overwrite(void);
static void gui_disk_creation(void);
#ifdef __linux__
static void gui_disk_steps(void);
#endif
static void gui_disk_options(void);
static void gui_diskset_load(void);
static void gui_diskset_save(void);
static void gui_disk_menu(void);
static void gui_hard_menu(void);
static void gui_stringy_menu(void);
static void gui_cassette_menu(void);
static void gui_emulator_menu(void);
static void gui_main_menu(void);
static void gui_misc_menu(void);
static void gui_save_state(void);
static int  gui_load_state(void);
static void gui_write_config(void);
static int  gui_read_config(void);
static int  gui_config_menu(void);
static const char *gui_key_name(int key);
static int  gui_virtual_keyboard(void);
static void gui_virtual_key(void);
static void gui_joy_gui(void);
static int  gui_joystick_button(void);
static void gui_joystick_map(int button);
static void gui_joystick_menu(void);
static void gui_default_dirs(void);
static void gui_roms(void);
static void gui_about(void);
static void gui_keys(void);
static void gui_exec_cmd(void);
static void gui_save_bmp(void);

void gui_write(const char *text, int x, int y, int font)
{
  int const len = strlen(text);

  if (x == 0) {
    x = (64 - len) / 2;
    if (x < 2) x = 2;
  }

  if (len > 62 - x) {
    int const pos = (59 - x) / 2;

    gui_text(text, x, y, pos, font);
    gui_text("...", x + pos, y, 3, font);
    gui_text(&text[len - (59 - x - pos)], x + pos + 3, y, 32 - x, font);
  } else {
    gui_text(text, x, y, len, font);
  }
}

void gui_clear(void)
{
  gui_rect(0, 0, 64, 16, 1);
}

void gui_limit(const char *text, char *limited, int limit)
{
  int const len = strlen(text);

  if (len > limit) {
    int const pos = (limit - 3) / 2;

    snprintf(limited, limit + 1, "%.*s...%s", pos, text,
        text + len - (limit - pos - 3));
  } else
    snprintf(limited, limit + 1, "%s", text);
}

void gui_add_extension(char *name, const char *ext)
{
  int const len = strlen(name);

  if (len >= 4)
    if (strcasecmp(&name[len - 4], ext) == 0)
      return;

  if (len && name[len - 1] != DIR_SLASH)
    snprintf(name + len, FILENAME_MAX - len, "%s", ext);
}

int gui_cmp_extension(const char *mask, const char *ext)
{
  size_t i;

  for (i = 0; i < strlen(mask); i += 4)
    if (strncasecmp(&mask[i], ext, 4) == 0)
      return 1;

  return 0;
}

int gui_key(void)
{
  SDL_Event event;

  while (1) {
    SDL_WaitEvent(&event);
    switch (event.type) {
      case SDL_QUIT:
        trs_exit(0);
        break;
      case SDL_MOUSEBUTTONDOWN:
        switch (event.button.button) {
          case SDL_BUTTON_LEFT:
            return SDLK_RETURN;
          case SDL_BUTTON_MIDDLE:
            return SDLK_TAB;
          case SDL_BUTTON_RIGHT:
            return SDLK_ESCAPE;
#ifndef SDL2
          case 4: /* SDL_BUTTON_WHEELUP */
            return SDLK_UP;
          case 5: /* SDL_BUTTON_WHEELDOWN */
            return SDLK_DOWN;
#endif
          default:
            break;
        }
        break;
#ifdef SDL2
      case SDL_MOUSEWHEEL:
        if (event.wheel.y > 0)
          return SDLK_UP;
        if (event.wheel.y < 0)
          return SDLK_DOWN;
        break;

      case SDL_TEXTINPUT:
        return event.text.text[0];

      case SDL_WINDOWEVENT:
        trs_screen_update();
        break;
#endif
      case SDL_KEYDOWN:
        if (event.key.keysym.mod & KMOD_ALT) {
          switch (event.key.keysym.sym) {
#if defined(__OS2__) || defined(_WIN32)
            case SDLK_F4:
#endif
            case SDLK_q:
            case SDLK_END:
              trs_exit(!(SDL_GetModState() & KMOD_SHIFT));
              break;
            case SDLK_BACKSPACE:
              return SDLK_F9;
            case SDLK_DELETE:
              return SDLK_F10;
            default:
              break;
          }
        }
        else if (event.key.keysym.sym == SDLK_F7)
          return SDLK_ESCAPE;
        else if (event.key.keysym.sym == SDLK_F8)
          trs_exit(!(event.key.keysym.mod & KMOD_SHIFT));
#ifdef SDL2
        else if (event.key.keysym.sym < 0x20 ||
                 event.key.keysym.sym > 0x7E)
#else
        else if (event.key.keysym.sym < 0x100 &&
            event.key.keysym.unicode >= 0x20 &&
            event.key.keysym.unicode <= 0x7E)
          return event.key.keysym.unicode;
        else
#endif
          return event.key.keysym.sym;
        break;
      case SDL_JOYBUTTONDOWN:
        if (event.jbutton.button < JOY_BUTTONS) {
          int const key = jbutton_map[event.jbutton.button];

          if (key >= 0)
            return key;
        }
        break;
      case SDL_JOYAXISMOTION:
        if (event.jaxis.axis == 0 || event.jaxis.axis == 1) {
          static int hor_value, ver_value;
          int value = 0, trigger_keydown = 0, key = -1;

          if (event.jaxis.axis == 0)
            value = hor_value;
          else
            value = ver_value;

          if (event.jaxis.value < -JOY_BOUNCE) {
            if (value != -1)
              trigger_keydown = 1;
            value = -1;
          }
          else if (event.jaxis.value > JOY_BOUNCE) {
            if (value != 1)
              trigger_keydown = 1;
            value = 1;
          }
          else if (abs(event.jaxis.value) < JOY_BOUNCE / 8)
            value = 0;

          if (trigger_keydown) {
            if (event.jaxis.axis == 0)
              key = (value == -1 ? SDLK_LEFT : SDLK_RIGHT);
            else
              key = (value == -1 ? SDLK_UP : SDLK_DOWN);
          }

          if (event.jaxis.axis == 0)
            hor_value = value;
          else
            ver_value = value;

          if (key != -1)
            return key;
        }
        break;
      case SDL_JOYHATMOTION:
        switch (event.jhat.value) {
          case SDL_HAT_UP:
            return SDLK_UP;
          case SDL_HAT_DOWN:
            return SDLK_DOWN;
          case SDL_HAT_LEFT:
            return SDLK_LEFT;
          case SDL_HAT_RIGHT:
            return SDLK_RIGHT;
          default:
            break;
        }
        break;
    }
  }
}

int gui_select(const char *text, int x, int y)
{
  int key;

  gui_write(text, x, y, 5);
  trs_screen_update();
  key = gui_key();
  gui_write(text, x, y, 4);

  return TOUPPER(key);
}

void gui_error(const char *name)
{
  if (errno) {
    char text[60];

    if (snprintf(text, 60, "%s: %s", strerror(errno), name))
      gui_message("ERROR", text);
  }
}

void gui_message(const char *title, const char *message)
{
  gui_rect(1, 6, 62, 3, 1);
  gui_text(title, 3, 6, 32, 4);
  gui_text(message, 3, 7, 60, 4);
  gui_text(" Press any key to continue ", 34, 8, 28, 5);
  trs_screen_update();
  gui_key();
}

void gui_delete_filelist(void)
{
  int i = filelistcount;

  while (i--)
    free(filelist[i]);

  filelistcount = 0;
}

void gui_add_to_filelist(char *name)
{
  filelist[filelistcount++] = name;
  if (filelistcount == filelistsize) {
    char **filelist_new;

    if ((filelist_new = realloc(filelist, 2 *
        filelistsize * sizeof(char*))) == NULL) {
      free(filelist);
      fatal("failed to reallocate filelist");
    }

    filelist = filelist_new;
    filelistsize *= 2;
  }
}

int gui_filename_cmp(const void *nptr1, const void *nptr2)
{
  const char *name1 = *(const char **)nptr1;
  const char *name2 = *(const char **)nptr2;

  if (name1[0] == '<') {
    if (name2[0] != '<')
      return -1;
    if (name1[1] == '.') {
      if (name2[1] != '.')
        return -1;
    }
    else if (name2[1] == '.')
      return 1;
  }
  else if (name2[0] == '<')
    return 1;

  return strcasecmp(name1, name2);
}

int gui_read_dir(const char *path, const char *mask, int dir)
{
  DIR *directory = opendir(path);

  if (directory) {
    const struct dirent *dir_entry;

    if (filelist == NULL) {
      if ((filelist = (char **)malloc(256 * sizeof(char *))) == NULL)
        fatal("failed to allocate filelist");

      filelistsize = 256;
    }

    while ((dir_entry = readdir(directory))) {
      char   pathname[FILENAME_MAX];
      char  *name = NULL;
      struct stat st = { 0 };
      int const len = strlen(dir_entry->d_name);

      if (dir_entry->d_name[0] == '.' && dir_entry->d_name[1] != '.')
        continue;

      if (snprintf(pathname, FILENAME_MAX, "%s%s",
          path, dir_entry->d_name) >= FILENAME_MAX) {
        closedir(directory);
        return -1;
      }

      stat(pathname, &st);
      if (S_ISDIR(st.st_mode)) {
        if ( (name = (char *)malloc(len + 3)) )
          snprintf(name, len + 3, "<%s>", dir_entry->d_name);
      }
      else if (dir) {
        continue;
      } else {
        if (mask != NULL) {
          if (len < 4)
            continue;
          if (gui_cmp_extension(mask, &dir_entry->d_name[len - 4]) == 0)
            continue;
        }
        if ( (name = (char *)malloc(len + 2)) )
          snprintf(name, len + 2, " %s", dir_entry->d_name);
      }

      if (name == NULL) {
        closedir(directory);
        return -1;
      }

      gui_add_to_filelist(name);
    }
    closedir(directory);

    qsort(filelist, filelistcount, sizeof(char *), gui_filename_cmp);
#if defined(__OS2__) || defined(_WIN32)
    {
      char letter;
#ifdef __OS2__
      ULONG drive_curr = 0, drive_mask = 0;

      DosQueryCurrentDisk(&drive_curr, &drive_mask);
#else
      DWORD drive_mask = GetLogicalDrives();
#endif

      for (letter = 'A'; letter <= 'Z'; letter++) {
        if (drive_mask & 1) {
          static char drive[5] = "[C:]";

          drive[1] = letter;
          gui_add_to_filelist(strdup(drive));
        }
        drive_mask >>= 1;
      }
    }
#endif
    return 0;
  } else {
    file_error("open directory: '%s'", path);
    return -1;
  }
}

int gui_file(const char *path, char *name, const char *mask, int dir, const char *type)
{
  char directory[FILENAME_MAX];
  struct stat st = { 0 };
  const char *dir_entry;
  int i;
  int all = 0;
  int cnt;
  int num;
  int row;
  int top;
  int redraw;
  int selection;

  gui_clear();
  gui_text("Select ", 2, 0, 8, 4);
  gui_text(type, 9, 0, 32, 4);

  if (dir) {
    gui_write(" Directory", 8 + strlen(type), 0, 4);
    gui_write(" INS/TAB:Select Directory ", 0, 15, 5);
  } else {
    gui_write(" ENTER/INS/SPACE/TAB:Select  BACKSPACE/ESC/F7:Return ", 0, 15, 5);
  }

  i = snprintf(directory, FILENAME_MAX, "%s", path);
  while (i--) {
    if (directory[i] == DIR_SLASH) {
      directory[i + 1] = 0;
      break;
    }
  }

  stat(directory, &st);
  if (S_ISDIR(st.st_mode) == 0 || directory[1] == DIR_SLASH) {
    if (getcwd(directory, FILENAME_MAX) == NULL) {
      snprintf(directory, 2, "%c", DIR_SLASH);
    } else {
      i = strlen(directory);
      snprintf(directory + i, FILENAME_MAX - i, "%c", DIR_SLASH);
    }
  }

read_dir:
  gui_delete_filelist();

  if (gui_read_dir(directory, all ? NULL : mask, dir) != 0)
    return -1;

  if (dir == 0)
    gui_text(all ? " F1:Ext " : " F1:All ", 54, 0, 9, 5);

  gui_rect(2, 1, 60, 1, 0);
  gui_write(directory, 0, 1, 4);

  cnt = filelistcount < 13 ? filelistcount - 1 : 12;
  num = filelistcount - cnt - 1;
  row = top = 0;
  redraw = 1;

  while (1) {
    int key;

    if (redraw) {
      gui_rect(2, 2, 60, 13, 0);

      for (i = 0; i <= cnt; i++)
        gui_write(filelist[top + i], 2, i + 2, 4);

      redraw = 0;
    }

    selection = row + top;
    dir_entry = filelist[selection];

    key = gui_select(dir_entry, 2, row + 2);
    if (key >= '!' && key <= 'Z') {
      int sel = selection;

      do {
        if (++sel >= filelistcount)
          sel = 0;
      } while (sel != selection && (TOUPPER((int)filelist[sel][1]) != key));

      if (sel < 13) {
        top = 0;
        row = sel;
      } else if (sel + 13 > filelistcount) {
        top = num;
        row = sel - top;
      } else {
        top = sel - row;
      }

      redraw = 1;
    } else {
      switch (key) {
        case SDLK_DOWN:
        case SDLK_RIGHT:
          if (row < cnt)
            row++;
          else
            if (top < num) {
              top++;
              redraw = 1;
            }
          break;
        case SDLK_UP:
        case SDLK_LEFT:
          if (row > 0)
            row--;
          else
            if (top > 0) {
              top--;
              redraw = 1;
            }
          break;
        case SDLK_PAGEUP:
          if (top - 13 >= 0) {
            top -= 13;
            redraw = 1;
            break;
          }
          /* Fall through */
        case SDLK_HOME:
          if (top) {
            top = 0;
            redraw = 1;
          }
          row = 0;
          break;
        case SDLK_PAGEDOWN:
          if (top + 13 <= num) {
            top += 13;
            redraw = 1;
            break;
          }
          /* Fall through */
        case SDLK_END:
          if (top < num) {
            top = num;
            redraw = 1;
          }
          row = cnt;
          break;
        case SDLK_F1:
          if (dir)
            break;
          all = !all;
          goto read_dir;
        case SDLK_INSERT:
        case SDLK_TAB:
          if (dir)
            if (dir_entry[1] != '.' && dir_entry[2] != '.')
              goto done;
          /* Fall through */
        case SDLK_RETURN:
        case SDLK_SPACE:
          if (dir_entry[0] == '<') {
            int const len = strlen(directory);

            if (dir_entry[1] == '.' && dir_entry[2] == '.') {
              for (i = len - 2; i >= 0; i--) {
                if (directory[i] == DIR_SLASH) {
                  directory[i + 1] = 0;
                  break;
                }
              }
            } else {
              i = snprintf(directory + len, FILENAME_MAX - len, "%s", &dir_entry[1]);
              directory[i + len - 1] = DIR_SLASH;
            }
            goto read_dir;
          }
#if defined(__OS2__) || defined(_WIN32)
          /* Select a new drive */
          else if (dir_entry[0] == '[') {
            snprintf(directory, 4, "%c:\\", dir_entry[1]);
            goto read_dir;
          }
#endif
          goto done;
        case SDLK_BACKSPACE:
        case SDLK_ESCAPE:
          gui_delete_filelist();
          return -1;
      }
    }
  }

done:
#if defined(__OS2__) || defined(_WIN32)
  if (dir && dir_entry[0] == '[')
    i = snprintf(name, 4, "%c: ", dir_entry[1]);
  else
#endif
  i = snprintf(name, FILENAME_MAX, "%s%s", directory, &dir_entry[1]);
  if (dir && i >= 1)
    name[i - 1] = DIR_SLASH;

  gui_delete_filelist();
  return selection;
}

int gui_input(const char *title, const char *input, char *output, int limit, int dir)
{
  int ins = 1;
  int pos;
  int len;
  int col;

  if (input != output)
    snprintf(output, limit, "%s", input);

  pos = len = strlen(output);

redraw:
  col = pos > 60 ? pos - 59 : 0;

  gui_rect(1, 6, 62, 3, 1);
  gui_write(title, 0, 6, 4);

  if (ins)
    gui_text(" INS ", 56, 8, 6, 5);

  if (dir)
    gui_write(" TAB:Select Directory ", 0, 8, 5);

  while (1) {
    int key;
    int i;

    gui_rect(2, 7, 60, 1, 0);
    gui_text(&output[col], 2, 7, 60, 4);
    gui_text(pos >= len ? " " : &output[pos], 2 + (pos - col), 7, 1, 5);
    trs_screen_update();

    key = gui_key();
    switch (key) {
      case SDLK_LEFT:
        if (pos > 0) {
          if (pos == col)
            col--;
          pos--;
        }
        break;
      case SDLK_RIGHT:
        if (pos < len) {
          if (pos == col + 59)
            col++;
          pos++;
        }
        break;
      case SDLK_HOME:
      case SDLK_PAGEUP:
        col = pos = 0;
        break;
      case SDLK_END:
      case SDLK_PAGEDOWN:
        pos = len;
        col = pos > 60 ? pos - 59 : 0;
        break;
      case SDLK_BACKSPACE:
        if (pos > 0) {
          for (i = pos; i < len; i++)
            output[i - 1] = output[i];
          len--;
          output[len] = 0;
          if (pos == col)
            col--;
          pos--;
        }
        break;
      case SDLK_DELETE:
        if (pos < len) {
          len--;
          for (i = pos; i < len; i++)
            output[i] = output[i + 1];
          output[i] = 0;
        }
        break;
      case SDLK_INSERT:
        ins = !ins;
        goto redraw;
      case SDLK_RETURN:
        return len;
      case SDLK_ESCAPE:
        return -1;
      case SDLK_DOWN:
      case SDLK_TAB:
      case SDLK_UP:
        if (dir) {
          char directory[FILENAME_MAX];

          if (gui_file(input, directory, NULL, 1, "") >= 0) {
            pos = len = snprintf(output, limit, "%s", directory);
          }
          goto redraw;
        }
        break;
      case SDLK_F9:
        col = pos = 0;
        /* Fall through */
      case SDLK_F10:
        len = pos;
        output[len] = 0;
        break;
      default:
        if (key >= ' ' && key <= 0xFF && pos < limit) {
          if (ins && len < limit) {
            for (i = len; i > pos; i--)
              output[i] = output[i - 1];
            len++;
          }
          output[pos] = (char)key;
          if (pos == col + 59)
            col++;
          pos++;
          if (pos > len)
            len++;
          output[len] = 0;
        }
        break;
    }
  }
}

int gui_menu(const char *title, const MENU *entry, int selection)
{
  int num = 0;

  gui_write(title, 0, 0, 4);

  if (strstr(title, "Man"))
    gui_write(" ENTER/INS/TAB:Insert  DEL:Remove  SPACE:Write-Protect ", 0, 15, 5);
  else
    gui_write(" ENTER/INS/SPACE/TAB:Select  BACKSPACE/ESC/F7:Return ", 0, 15, 5);

  while (entry[num].type != 0) {
    gui_text(entry[num].text, 2, num + 1, 60, 4);
    num++;
  }
  num--;

  while (1) {
    int key = gui_select(entry[selection].text, 2, selection + 1);

    if (key >= '0' && key <= '9') {
      if (strstr(title, "Creat")) return key; /* Floppy Disk Creation Drive */
      key -= '0';
      if (key <= num && entry[key].text[1] == (key + '0'))
        selection = key;
    } else
    if (key >= 'A' && key <= 'Z') {
      int const sel = selection;

      do {
        if (++selection > num)
          selection = 0;
      } while (selection != sel && (int)*entry[selection].text != key);

      while (entry[selection].type == TITLE) {
        if (selection < num)
          selection++;
        else
          selection = 0;
      }
    } else {
      switch (key) {
        case SDLK_DOWN:
        case SDLK_RIGHT:
          do {
            selection++;
            if (selection > num)
              selection = 0;
          } while (entry[selection].type == TITLE);
          break;
        case SDLK_UP:
        case SDLK_LEFT:
          do {
            selection--;
            if (selection < 0)
              selection = num;
          } while (entry[selection].type == TITLE);
          break;
        case SDLK_HOME:
        case SDLK_PAGEUP:
          selection = 0;
          while (entry[selection].type == TITLE) {
            if (selection < num)
              selection++;
          }
          break;
        case SDLK_END:
        case SDLK_PAGEDOWN:
          selection = num;
          while (entry[selection].type == TITLE) {
            if (selection > 0)
              selection--;
          }
          break;
        case SDLK_DELETE:
          switch (entry[selection].type) {
            case DISK_DRIVE:
              trs_disk_remove(selection);
              break;
            case HARD_DRIVE:
              trs_hard_remove(selection);
              break;
            case WAFER:
              stringy_remove(selection);
              break;
            case CASSETTE:
              trs_cassette_remove();
              break;
            default:
              continue;
          }
          return selection;
        case SDLK_INSERT:
        case SDLK_RETURN:
        case SDLK_TAB:
          switch (entry[selection].type) {
            case ENTRY:
              return selection;
            case DISK_DRIVE:
              if (gui_file(trs_disk_getfilename(selection)[0] ?
                  trs_disk_getfilename(selection) : trs_disk_dir,
                  filename, DSK, 0, "Floppy Disk Image") >= 0)
                trs_disk_insert(selection, filename);
              break;
            case HARD_DRIVE:
              if (gui_file(trs_hard_getfilename(selection)[0] ?
                  trs_hard_getfilename(selection) : trs_hard_dir,
                  filename, HDV, 0, "Hard Disk Image") >= 0)
                trs_hard_attach(selection, filename);
              break;
            case WAFER:
              if (gui_file(stringy_get_name(selection)[0] ?
                  stringy_get_name(selection) : trs_cass_dir,
                  filename, ESF, 0, "Wafer Image") >= 0)
                stringy_insert(selection, filename);
              break;
            case CASSETTE:
              if (gui_file(trs_cassette_getfilename()[0] ?
                  trs_cassette_getfilename() : trs_cass_dir,
                  filename, CAS, 0, "Cassette Image") >= 0)
                trs_cassette_insert(filename);
              break;
            case SAVE_SET:
              gui_diskset_save();
              break;
            case LOAD_SET:
              gui_diskset_load();
              break;
          }
          return selection;
        case SDLK_SPACE:
          if (entry[selection].type < ENTRY) {
            if (trs_write_protect(entry[selection].type, selection) < 0)
              gui_error("Write-Protection");
          }
          return selection;
        case SDLK_BACKSPACE:
        case SDLK_ESCAPE:
          return -1;
      }
    }

    if (entry[selection].type == HARD_DRIVE)
      return selection; /* Update Hard Disk Geometry */
  }
}

int gui_popup(const char *title, const char **entry, int num, int selection)
{
  int const len = strlen(entry[0]);
  int const saved_selection = selection;
  int const x = (64 - len) / 2;
  int const y = (16 - num) / 2;
  int i;

  gui_rect(x - 1, y - 1, len + 2, num + 2, 1);
  gui_write(title, 0, y - 1, 4);

  for (i = 0; i < num; i++)
    gui_text(entry[i], x, y + i, len, 4);
  num--;

  while (1) {
    int const key = gui_select(entry[selection], x, selection + y);

    if (key >= '0' && key <= 'Z') {
      if (num == 1) {
        if (key == 'N')
          return 0;
        if (key == 'Y')
          return 1;
      }

      i = selection;
      do {
        if (selection++ >= num)
          selection = 0;
      } while (selection != i && strchr(entry[selection], key) == NULL);

    } else {
      switch (key) {
        case SDLK_DOWN:
        case SDLK_RIGHT:
          selection++;
          if (selection > num)
            selection = 0;
          break;
        case SDLK_UP:
        case SDLK_LEFT:
          selection--;
          if (selection < 0)
            selection = num;
          break;
        case SDLK_HOME:
        case SDLK_PAGEUP:
          selection = 0;
          break;
        case SDLK_END:
        case SDLK_PAGEDOWN:
          selection = num;
          break;
        case SDLK_INSERT:
        case SDLK_RETURN:
        case SDLK_SPACE:
        case SDLK_TAB:
          return selection;
        case SDLK_BACKSPACE:
        case SDLK_ESCAPE:
          return saved_selection;
      }
    }
  }
}

int gui_matrix(const char *title, const char **entry, int rows, int cols, int selection)
{
  int const len = strlen(entry[0]) + 1;
  int const num = rows * cols;
  int const width = cols * len - 1;
  int const x = (64 - width) / 2;
  int const y = (16 - rows) / 2;
  int row, col;

  gui_rect(x - 1, y - 1, width + 2, rows + 2, 1);
  gui_write(title, 0, y - 1, 4);

  for (row = 0; row < rows; row++)
    for (col = 0; col < cols; col++)
      gui_text(entry[row * cols + col], x + col * len, y + row, len, 4);

  if (selection < 0)
    selection = 0;
  else if (selection >= num)
    selection = num - 1;

  row = selection / cols;
  col = selection % cols;

  while (1) {
    if (col < 0)
      col = cols - 1;
    else if (col >= cols)
      col = 0;
    if (row < 0)
      row = rows - 1;
    else if (row >= rows)
      row = 0;

    selection = row * cols + col;
    switch (gui_select(entry[selection], x + col * len, y + row)) {
      case SDLK_DOWN:
        row++;
        if (row == rows)
          col++;
        break;
      case SDLK_UP:
        row--;
        if (row < 0)
          col--;
        break;
      case SDLK_RIGHT:
        col++;
        if (col == cols)
          row++;
        break;
      case SDLK_LEFT:
        col--;
        if (col < 0)
          row--;
        break;
      case SDLK_HOME:
        col = 0;
        break;
      case SDLK_END:
        col = cols - 1;
        break;
      case SDLK_PAGEUP:
        row = 0;
        break;
      case SDLK_PAGEDOWN:
        row = rows - 1;
        break;
      case SDLK_INSERT:
      case SDLK_RETURN:
      case SDLK_SPACE:
      case SDLK_TAB:
        return selection;
      case SDLK_BACKSPACE:
      case SDLK_ESCAPE:
        return -1;
    }
  }
}

int gui_question(const char *text)
{
  return gui_popup(text, yes_no, 2, 0);
}

int gui_file_overwrite(void)
{
  struct stat st = { 0 };

  if (stat(filename, &st) == 0 && S_ISREG(st.st_mode))
    return gui_question("Overwrite?");

  return 1;
}

void gui_disk_creation(void)
{
  MENU menu[] =
  {{"Type of Disk Image                                     ", ENTRY},
   {"", TITLE},
   {"Sides of Disk                                          ", ENTRY},
   {"Density                                                ", ENTRY},
   {"Physical Size                                          ", ENTRY},
   {"", TITLE},
   {"Ignore Density Flag                                    ", ENTRY},
   {"", TITLE},
   {"Insert Created Floppy Disk Image Into Drive            ", ENTRY},
   {"Create Disk Image with Above Parameters", ENTRY},
   {"", 0}};
  const char *disk_type[] = {"   JV1", "   JV3", "   DMK"};
  const char *disk_side[] = {"     1", "     2"};
  const char *disk_dens[] = {"Single", "Double"};
  const char *disk_size[] = {"5 Inch", "8 Inch"};
  static int type = 2;
  static int sides = 1;
  static int density = 1;
  static int size;
  static int ignore_density;
  static int drive;
  int selection = 9;

  while (1) {
    snprintf(&menu[0].text[54], 7, "%s", disk_type[type]);
    snprintf(&menu[2].text[54], 7, "%s", disk_side[sides]);
    snprintf(&menu[3].text[54], 7, "%s", disk_dens[density]);
    snprintf(&menu[4].text[54], 7, "%s", disk_size[size]);
    snprintf(&menu[6].text[50], 11, "%s", yes_no[ignore_density]);
    snprintf(&menu[8].text[55], 6, "%s", drives[drive]);
    gui_clear();

    selection = gui_menu(" Floppy Disk Creation ", menu, selection);
    switch (selection) {
      case 0:
        type = gui_popup("Type", disk_type, 3, type);
        break;
      case 2:
        sides = !sides;
        break;
      case 3:
        density = !density;
        break;
      case 4:
        size = !size;
        break;
      case 6:
        ignore_density = !ignore_density;
        break;
      case 8:
        drive = gui_popup("Drive", drives, 9, drive);
        break;
      case 9:
        filename[0] = 0;
        if (gui_input(" Enter Filename for Disk Image ",
            trs_disk_dir, filename, FILENAME_MAX, 1) > 0) {
          const char *disk_ext[] = {".jv1", ".jv3", ".dmk"};

          gui_add_extension(filename, disk_ext[type]);
          if (gui_file_overwrite()) {
            int ret = 0;

            switch (type) {
              case 0:
                ret = trs_create_blank_jv1(filename);
                break;
              case 1:
                ret = trs_create_blank_jv3(filename);
                break;
              default:
                ret = trs_create_blank_dmk(filename,
                    sides + 1, density + 1, size, ignore_density);
                break;
            }

            if (ret)
              gui_error(filename);
            else if (drive)
              trs_disk_insert(drive - 1, filename);
            return;
          }
        }
        break;
      case -1:
        return;
      default:
        drive = selection <= '7' ? selection - '/' : 0;
        selection = 8;
        break;
    }
  }
}

#ifdef __linux__
void gui_disk_steps(void)
{
  MENU menu[] =
  {{"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", 0}};
  const char *steps[] = {"Single", "Double"};
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      snprintf(menu[i].text, 63,
          " %d: Drive Step                                        %s",
          i, steps[trs_disk_getstep(i) - 1]);
    }

    gui_clear();
    if ((selection = gui_menu(" Floppy Disk Step ", menu, selection)) == -1)
      return;
    trs_disk_setstep(selection, !(trs_disk_getstep(selection) - 1) + 1);
  }
}
#endif

void gui_disk_options(void)
{
  MENU menu[] =
  {{"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", ENTRY},
   {"", TITLE},
   {"Floppy Disk Controller                             ", ENTRY},
   {"Doubler Type                                       ", ENTRY},
   {"True DAM Emulation                                 ", ENTRY},
#ifdef __linux__
   {"Set Drive Steps", ENTRY},
#endif
   {"", 0}};
  const char *doubler[]   = {"     None", "   Percom", " Tandy/RS", "     Both"};
  const char *disk_size[] = {"5 Inch", "8 Inch"};
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      snprintf(menu[i].text, 63,
          " %d: Drive Size                                        %s",
          i, disk_size[trs_disk_getsize(i) == 5 ? 0 : 1]);
    }

    snprintf(&menu[9].text[50], 11, "%s", yes_no[trs_disk_controller]);
    snprintf(&menu[10].text[51], 10, "%s", doubler[trs_disk_doubler]);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[trs_disk_truedam]);
    gui_clear();

    selection = gui_menu(" Floppy Disk Options ", menu, selection);
    switch (selection) {
      case 0: case 1: case 2: case 3: case 4: case 5: case 6: case 7:
        trs_disk_setsize(selection, (trs_disk_getsize(selection) == 8) ? 5 : 8);
        break;
      case 9:
        trs_disk_controller = !trs_disk_controller;
        break;
      case 10:
        trs_disk_doubler = gui_popup("Doubler", doubler, 4, trs_disk_doubler);
        break;
      case 11:
        trs_disk_truedam = !trs_disk_truedam;
        break;
#ifdef __linux__
      case 12:
        gui_disk_steps();
        break;
#endif
      case -1:
        return;
    }
  }
}

void gui_diskset_load(void)
{
  if (gui_file(trs_disk_set_dir, filename, SET, 0, "Disk Set") >= 0) {
    if (trs_diskset_load(filename) != 0)
      gui_error(filename);
  }
}

void gui_diskset_save(void)
{
  filename[0] = 0;
  if (gui_input(" Enter Filename for Disk Set ",
      trs_disk_set_dir, filename, FILENAME_MAX - 5, 1) > 0) {
    gui_add_extension(filename, SET);
    if (gui_file_overwrite()) {
      if (trs_diskset_save(filename) != 0)
        gui_error(filename);
    }
  }
}

void gui_disk_menu(void)
{
  MENU menu[] =
  {{" 0: ", DISK_DRIVE},
   {" 1: ", DISK_DRIVE},
   {" 2: ", DISK_DRIVE},
   {" 3: ", DISK_DRIVE},
   {" 4: ", DISK_DRIVE},
   {" 5: ", DISK_DRIVE},
   {" 6: ", DISK_DRIVE},
   {" 7: ", DISK_DRIVE},
   {"", TITLE},
   {"Save Disk Set", SAVE_SET},
   {"Load Disk Set", LOAD_SET},
   {"Create Blank Floppy Disk", ENTRY},
   {"Disk Drive Options", ENTRY},
   {"", 0}};
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      gui_limit(trs_disk_getfilename(i), &menu[i].text[4], 56);
      menu[i].text[0] = trs_disk_getwriteprotect(i) ? '*' : ' ';
    }

    gui_clear();

    selection = gui_menu(" Floppy Disk Management ", menu, selection);
    switch (selection) {
      case 11:
        gui_disk_creation();
        break;
      case 12:
        gui_disk_options();
        break;
      case -1:
        return;
    }
  }
}

void gui_hard_menu(void)
{
  MENU menu[] =
  {{" 0: ", HARD_DRIVE},
   {" 1: ", HARD_DRIVE},
   {" 2: ", HARD_DRIVE},
   {" 3: ", HARD_DRIVE},
   {"", TITLE},
   {"Save Disk Set", SAVE_SET},
   {"Load Disk Set", LOAD_SET},
   {"", TITLE},
   {"Cylinder Count                                           ", ENTRY},
   {"Head Count                                               ", ENTRY},
   {"Sector Count                                             ", ENTRY},
   {"Insert Created Hard Disk Image Into Drive                ", ENTRY},
   {"Create Hard Disk Image with Above Parameters", ENTRY},
   {"", 0}};
  static int drive;
  int cylinders = 202;
  int heads     = 0;
  int sectors   = 256;
  int selection = 0;

  while (1) {
    char input[5];
    int  i;
    int  value;

    for (i = 0; i < 4; i++) {
      gui_limit(trs_hard_getfilename(i), &menu[i].text[4], 56);
      menu[i].text[0] = trs_hard_getwriteprotect(i) ? '*' : ' ';
    }

    if (selection < 4)
      trs_hard_getgeometry(selection, &cylinders, &heads, &sectors);

    snprintf(&menu[8].text[55], 6, "%5d", cylinders);
    snprintf(&menu[9].text[57], 4, "%3d", heads);
    snprintf(&menu[10].text[57], 4, "%3d", sectors);
    snprintf(&menu[11].text[55], 6, "%s", drives[drive]);
    gui_clear();

    selection = gui_menu(" Hard Disk Management ", menu, selection);
    switch (selection) {
      case 8:
        snprintf(input, 5, "%d", cylinders);
        if (gui_input(" Enter Cylinder Count ", input, input, 4, 0) > 0) {
          value = atoi(input);
          if (value != cylinders) {
            if (value >= 3 && value <= 8192) {
              cylinders = value;
              if (cylinders > 203)
                debug("Cylinder Count > 203 is incompatible with XTRSHARD/DCT\n");
            } else {
              gui_message("ERROR", "Cylinder Count must be between 3 and 8192");
            }
          }
        }
        break;
      case 9:
        snprintf(input, 2, "%d", heads);
        if (gui_input(" Enter Head Count ", input, input, 1, 0) > 0) {
          value = atoi(input);
          if (value != heads) {
            if (value >= 0 && value <= 8)
              heads = value;
            else
              gui_message("ERROR", "Head Count must be between 0 and 8");
          }
        }
        break;
      case 10:
        snprintf(input, 4, "%d", sectors);
        if (gui_input(" Enter Sector Count ", input, input, 3, 0) > 0) {
          value = atoi(input);
          if (value != sectors) {
            if (value >= 4 && value <= 256)
              sectors = value;
            else
              gui_message("ERROR", "Sector Count must be between 4 and 256");
          }
        }
        break;
      case 11:
        drive = gui_popup("Drive", drives, 5, drive);
        break;
      case 12:
        filename[0] = 0;
        if (gui_input(" Enter Filename for Hard Disk Image ",
            trs_hard_dir, filename, FILENAME_MAX, 1) > 0) {
          gui_add_extension(filename, HDV);
          if (gui_file_overwrite()) {
            if (trs_create_blank_hard(filename, cylinders, heads, sectors) != 0)
              gui_error(filename);
            else if (drive)
              trs_hard_attach(drive - 1, filename);
            return;
          }
        }
        break;
      case -1:
        return;
    }
  }
}

void gui_stringy_menu(void)
{
  MENU menu[] =
  {{" 0: ", WAFER},
   {" 1: ", WAFER},
   {" 2: ", WAFER},
   {" 3: ", WAFER},
   {" 4: ", WAFER},
   {" 5: ", WAFER},
   {" 6: ", WAFER},
   {" 7: ", WAFER},
   {"", TITLE},
   {"Save Disk Set", SAVE_SET},
   {"Load Disk Set", LOAD_SET},
   {"Insert Created Wafer Image Into Stringy Drive          ", ENTRY},
   {"Create Blank Floppy Wafer", ENTRY},
   {"", 0}};
  static int drive;
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      gui_limit(stringy_get_name(i), &menu[i].text[4], 56);
      menu[i].text[0] = stringy_get_writeprotect(i) ? '*' : ' ';
    }

    snprintf(&menu[11].text[55], 6, "%5s", drives[drive]);
    gui_clear();

    selection = gui_menu(" Stringy Wafer Management ", menu, selection);
    switch (selection) {
      case 11:
        drive = gui_popup("Drive", drives, 9, drive);
        break;
      case 12:
        filename[0] = 0;
        if (gui_input(" Enter Filename for Wafer Image ",
            trs_cass_dir, filename, FILENAME_MAX, 1) > 0) {
          gui_add_extension(filename, ESF);
          if (gui_file_overwrite()) {
            if (stringy_create(filename) != 0)
              gui_error(filename);
            else
              if (drive)
                stringy_insert(drive - 1, filename);
            return;
          }
        }
        break;
      case -1:
        return;
    }
  }
}

void gui_cassette_menu(void)
{
  MENU menu[] =
  {{" Cass : ", CASSETTE},
   {"", TITLE},
   {"Position of Tape                                      ", ENTRY},
   {"Default Sample Rate                                   ", ENTRY},
   {"", TITLE},
   {"Type of Cassette Image                                ", ENTRY},
   {"", TITLE},
   {"Insert Created Cassette Into Drive                    ", ENTRY},
   {"Create Blank Cassette Image with Above Parameters", ENTRY},
   {"", 0}};
  const char *cass_type[] = {"   CAS", "   CPT", "   WAV"};
  static int type;
  static int insert = 1;
  int selection = 0;

  while (1) {
    char input[12];
    int  value;

    gui_limit(trs_cassette_getfilename(), &menu[0].text[8], 52);
    menu[0].text[0] = trs_cass_getwriteprotect() ? '*' : ' ';

    snprintf(&menu[2].text[36], 25, "%10d of %10d", trs_get_cassette_position(),
             trs_get_cassette_length());
    snprintf(&menu[3].text[50], 11, "%10d", cassette_default_sample_rate);
    snprintf(&menu[5].text[54], 7, "%s", cass_type[type]);
    snprintf(&menu[7].text[50], 11, "%s", yes_no[insert]);
    gui_clear();

    selection = gui_menu(" Cassette Management ", menu, selection);
    switch (selection) {
      case 2:
        snprintf(input, 11, "%d", trs_get_cassette_position());
        if (gui_input(" Enter Position of Tape in Bytes ",
            input, input, 10, 0) > 0) {
          value = atoi(input);
          if (value != trs_get_cassette_position() &&
              value >= 0 && value <= trs_get_cassette_length())
            trs_set_cassette_position(value);
        }
        break;
      case 3:
        snprintf(input, 11, "%d", cassette_default_sample_rate);
        if (gui_input(" Enter Default Sample Rate ",
            input, input, 10, 0) > 0) {
          value = atoi(input);
          if (value != cassette_default_sample_rate &&
              value >= 0 && value <= MAX_SAMPLE_RATE)
            cassette_default_sample_rate = value;
        }
        break;
      case 5:
        type = gui_popup("Type", cass_type, 3, type);
        break;
      case 7:
        insert = !insert;
        break;
      case 8:
        filename[0] = 0;
        if (gui_input(" Enter Filename for Cassette Image ",
            trs_cass_dir, filename, FILENAME_MAX, 1) > 0) {
          const char *cass_ext[] = {".cas", ".cpt", ".wav"};

          gui_add_extension(filename, cass_ext[type]);
          if (gui_file_overwrite()) {
            FILE *cassette_file = fopen(filename, "wb");

            if (cassette_file) {
              if (type == 2) {
                 if (create_wav_header(cassette_file) < 0)
                   gui_message("ERROR", "Failed to create WAVE header");
              }
              fclose(cassette_file);
              if (insert)
                trs_cassette_insert(filename);
            } else
              gui_error(filename);
          }
        }
        break;
      case -1:
        return;
    }
  }
}

void gui_emulator_menu(void)
{
  MENU menu[] =
  {{"Model and Memory Size                             ", ENTRY},
   {"CPU Clock Speed                                   ", ENTRY},
   {"Speedup Kit or Banking for Model I/III/4/4P       ", ENTRY},
   {"Exatron Stringy Floppy Emulation for Model I      ", ENTRY},
   {"LE18 (Lowe Electronics) Graphics for Model I      ", ENTRY},
   {"Lowercase Modification for Model I                ", ENTRY},
   {"Lubomir Soft Banker Emulation for Model I         ", ENTRY},
   {"Selector (Dutch TRS-80 Users Society) Model I     ", ENTRY},
   {"X-MEM/80 16K page (Michael Wessel) for Model I    ", ENTRY},
   {"SuperMem (Alpha Technology) I/III/4/4P Memory     ", ENTRY},
   {"Grafyx Solution (Micro-Labs) III/4/4P Graphics    ", ENTRY},
   {"MegaMem (Anitek Software) III/4/4P Memory Board   ", ENTRY},
   {"Dave Huffman (and other) 4/4P Memory Expansion    ", ENTRY},
   {"HyperMem (Anitek Software) 4/4P Memory Expansion  ", ENTRY},
   {"", 0}};
  const char *model[] = {"  TRS-80 Model I",
                         "TRS-80 Model III",
                         "  TRS-80 Model 4",
                         " TRS-80 Model 4P"};
  const char *speed[] = {"           None",
                         "       Archbold",
                         "  Holmes II/III",
                         "     Seatronics",
                         "Banking Model I",
                         "    CT-80 Aster",
                         "       LNW80/II",
                         "TCS SpeedMaster"};
  const char *super[] = {"        No",
                         "    256 KB",
                         "    512 KB",
                         "    768 KB",
                         "   1024 KB"};
  int model_mem = (trs_mem_size - RAM_START) >> 10;
  int model_trs = trs_model == 1 ? 0 : trs_model - 2;
  int selection = 0;
  float clock_mhz[4];

  clock_mhz[0] = clock_mhz_1;
  clock_mhz[1] = clock_mhz_3;
  clock_mhz[2] = clock_mhz_4;
  clock_mhz[3] = clock_mhz_4;

  while (1) {
    char input[8];

    snprintf(&menu[0].text[37], 18, "%s", model[model_trs]);
    snprintf(&menu[0].text[53], 8, " %3d KB", model_trs < 2 ? model_mem : 128);
    snprintf(&menu[1].text[50], 11, "%6.2f MHz", clock_mhz[model_trs]);
    snprintf(&menu[2].text[45], 16, "%s", speed[speedup]);
    snprintf(&menu[3].text[50], 11, "%s", yes_no[stringy]);
    snprintf(&menu[4].text[50], 11, "%s", yes_no[lowe_le18]);
    snprintf(&menu[5].text[50], 11, "%s", yes_no[lowercase]);
    snprintf(&menu[6].text[50], 11, "%s", yes_no[lubomir]);
    snprintf(&menu[7].text[50], 11, "%s", yes_no[selector]);
    snprintf(&menu[8].text[50], 11, "%s", yes_no[xmem80]);
    snprintf(&menu[9].text[50], 11, "%s", super[supermem >> 3]);
    snprintf(&menu[10].text[50], 11, "%s", yes_no[grafyx_microlabs]);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[megamem]);
    snprintf(&menu[12].text[50], 11, "%s", yes_no[huffman]);
    snprintf(&menu[13].text[50], 11, "%s", yes_no[hypermem]);
    gui_clear();

    selection = gui_menu(" Emulator Settings ", menu, selection);
    switch (selection) {
      case 0:
        model_trs = gui_popup("Model", model, 4, model_trs);
        if (model_trs <= 1) {
          snprintf(input, 3, "%d", model_mem);
          if (gui_input(" Enter Memory Size in KB (4 - 48) ",
              input, input, 2, 0) > 0) {
            int const value = atoi(input);

            if (value != model_mem && value >= 4 && value <= 48)
              model_mem = value;
          }
        }
        break;
      case 1:
        snprintf(input, 6, "%.2f", clock_mhz[model_trs]);
        if (gui_input(" Enter CPU Clock Speed in MHz ",
            input, input, 6, 0) > 0) {
          float const value = atof(input);

          if (value >= 0.1 && value <= 99.0) {
            clock_mhz[model_trs] = value;
            switch (model_trs) {
              case 0:
              default:
                clock_mhz_1 = value;
                break;
              case 1:
                clock_mhz_3 = value;
                break;
              case 2:
              case 3:
                clock_mhz_4 = value;
                break;
            }
            trs_timer_init();
          }
        }
        break;
      case 2:
        speedup = gui_popup("Speedup", speed, 8, speedup);
        break;
      case 3:
        stringy = !stringy;
        break;
      case 4:
        lowe_le18 = !lowe_le18;
        break;
      case 5:
        lowercase = !lowercase;
        break;
      case 6:
        lubomir = !lubomir;
        if (lubomir)
          selector = supermem = xmem80 = 0;
        break;
      case 7:
        selector = !selector;
        if (selector)
          lubomir = supermem = xmem80 = 0;
        break;
      case 8:
        xmem80 = !xmem80;
        if (xmem80)
          lubomir = selector = supermem = 0;
        break;
      case 9:
        supermem = gui_popup("SuperMem", super, 5, supermem >> 3) << 3;
        if (supermem)
          huffman = hypermem = lubomir = selector = xmem80 = 0;
        break;
      case 10:
        grafyx_microlabs = !grafyx_microlabs;
        break;
      case 11:
        megamem = !megamem;
        if (megamem)
          huffman = 0;
        break;
      case 12:
        huffman = !huffman;
        if (huffman)
          megamem = hypermem = supermem = 0;
        break;
      case 13:
        hypermem = !hypermem;
        if (hypermem)
          huffman = supermem = 0;
        break;
      case -1:
        model_mem = (model_mem << 10) + RAM_START;
        model_trs = model_trs == 0 ? 1 : model_trs + 2;
        if (trs_model != model_trs || trs_mem_size != model_mem) {
          trs_mem_size = model_mem;
          trs_model = model_trs;
          trs_reset(1);
        }
        return;
    }
  }
}

void gui_display_menu(void)
{
  MENU menu[] =
  {{"Emulator Background Color                              ", ENTRY},
   {"Emulator Foreground Color                              ", ENTRY},
   {"GUI Background Color                                   ", ENTRY},
   {"GUI Foreground Color                                   ", ENTRY},
   {"Character Set for Model I                              ", ENTRY},
   {"Character Set for Model III                            ", ENTRY},
   {"Character Set for Model 4/4P                           ", ENTRY},
   {"Border Width                                           ", ENTRY},
   {"Resize Window on Mode Change for Model III             ", ENTRY},
   {"Resize Window on Mode Change for Model 4/4P            ", ENTRY},
   {"Scale Factor for Window                                ", ENTRY},
   {"LED Display for Disks and Turbo Mode                   ", ENTRY},
#ifdef OLD_SCANLINES
   {"Display Scanlines with Background Color                ", ENTRY},
#else
   {"Display Scanlines with brightness                      ", ENTRY},
#endif
   {"", 0}};
  const char *font1[]  = {"      Early",
                          "      Stock",
                          "      Lcmod",
                          "      Wider",
                          "      Genie",
                          "   HT-1080Z",
                          "  Meritum I",
                          "CT-80 Aster",
                          "Video Genie"};
  const char *font34[] = {"     Katakana",
                          "International",
                          "         Bold"};
  const char *scales[] = {"  1  ", "  2  ", "  3  ", "  4  "};
  int selection = 0;
  int gui_charset1 = trs_charset1 >= 10 ? trs_charset1 - 6 : trs_charset1;

  while (1) {
    char input[8];
    int  resize = -1;
    int  value = 0;

    snprintf(&menu[0].text[52], 9, "0x%06X", background);
    snprintf(&menu[1].text[52], 9, "0x%06X", foreground);
    snprintf(&menu[2].text[52], 9, "0x%06X", gui_background);
    snprintf(&menu[3].text[52], 9, "0x%06X", gui_foreground);
    snprintf(&menu[4].text[49], 12, "%s", font1[gui_charset1]);
    snprintf(&menu[5].text[47], 14, "%s", font34[trs_charset3 - 4]);
    snprintf(&menu[6].text[47], 14, "%s", font34[trs_charset4 - 7]);
    snprintf(&menu[7].text[52], 9, "%8d", border_width);
    snprintf(&menu[8].text[50], 11, "%s", yes_no[resize3]);
    snprintf(&menu[9].text[50], 11, "%s", yes_no[resize4]);
    snprintf(&menu[10].text[55], 6, "%5d", scale);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[trs_show_led]);
#ifdef OLD_SCANLINES
    snprintf(&menu[12].text[50], 11, "%s", yes_no[scanlines]);
#else
    snprintf(&menu[12].text[34], 27, "%-3d%23s", scanshade, yes_no[scanlines]);
#endif
    gui_clear();

    selection = gui_menu(" Display Settings ", menu, selection);
    switch (selection) {
      case 0:
        snprintf(input, 7, "%06X", background);
        if (gui_input(" Enter Background RGB color (Hex, RRGGBB) ",
            input, input, 6, 0) > 0) {
          value = strtol(input, NULL, 16);
          if (value != background) {
            background = value;
            resize = 0;
          }
        }
        break;
      case 1:
        snprintf(input, 7, "%06X", foreground);
        if (gui_input(" Enter Foreground RGB color (Hex, RRGGBB) ",
            input, input, 6, 0) > 0) {
          value = strtol(input, NULL, 16);
          if (value != foreground) {
            foreground = value;
            resize = 0;
          }
        }
        break;
      case 2:
        snprintf(input, 7, "%06X", gui_background);
        if (gui_input(" Enter GUI Background RGB color (Hex, RRGGBB) ",
            input, input, 6, 0) > 0) {
          value = strtol(input, NULL, 16);
          if (value != gui_background) {
            gui_background = value;
            resize = 0;
          }
        }
        break;
      case 3:
        snprintf(input, 7, "%06X", gui_foreground);
        if (gui_input(" Enter GUI Foreground RGB color (Hex, RRGGBB) ",
            input, input, 6, 0) > 0) {
          value = strtol(input, NULL, 16);
          if (value != gui_foreground) {
            gui_foreground = value;
            resize = 0;
          }
        }
        break;
      case 4:
        value = gui_popup("Charset I", font1, 9, gui_charset1);
        if (value != gui_charset1) {
          gui_charset1 = value;
          trs_charset1 = value >= 4 ? value + 6 : value;
          resize = 1;
        }
        break;
      case 5:
        value = gui_popup("Charset III", font34, 3, trs_charset3 - 4) + 4;
        if (value != trs_charset3) {
          trs_charset3 = value;
          resize = 0;
        }
        break;
      case 6:
        value = gui_popup("Charset 4", font34, 3, trs_charset4 - 7) + 7;
        if (value != trs_charset4) {
          trs_charset4 = value;
          resize = 0;
        }
        break;
      case 7:
        snprintf(input, 3, "%d", border_width);
        if (gui_input(" Enter Window border width in pixels (0 to 50) ",
            input, input, 2, 0) > 0) {
          value = atoi(input);
          if (value != border_width && value >= 0 && value <= 50) {
            border_width = value;
            resize = 1;
          }
        }
        break;
      case 8:
        resize3 = !resize3;
        if (trs_model == 3)
          resize = 1;
        break;
      case 9:
        resize4 = !resize4;
        if (trs_model >= 4)
          resize = 1;
        break;
      case 10:
        value = gui_popup("Scale", scales, 4, scale - 1) + 1;
        if (value != scale) {
          scale = value;
          fullscreen = 0;
          resize = 1;
        }
        break;
      case 11:
        trs_show_led = !trs_show_led;
        resize = 1;
        break;
      case 12:
        scanlines = !scanlines;
        resize = 0;
#ifndef OLD_SCANLINES
        if (scanlines) {
          snprintf(input, 4, "%d", scanshade);
          if (gui_input(" Enter brightness (0 = dark - 255 = light) ",
              input, input, 3, 0) > 0) {
            value = atoi(input) & 255;
            if (value != scanshade) {
              scanshade = value;
              resize = 0;
            }
          }
        }
#endif
        break;
      case -1:
        return;
    }

    if (resize >= 0)
      trs_screen_init(resize);
  }
}

void gui_misc_menu(void)
{
  MENU menu[] =
  {{"Close and Reopen Printer Output File", ENTRY},
   {"Emulator Traps Safe                                     ", ENTRY},
   {"Fake year for TRS-80 time-of-day clock                  ", ENTRY},
   {"Keystretch Value                                        ", ENTRY},
   {"Printer Type                                            ", ENTRY},
   {"Serial Port Name:", TITLE},
   {"                                                        ", ENTRY},
   {"Serial Switches                                         ", ENTRY},
   {"Shift Bracket Emulation                                 ", ENTRY},
   {"Sound Output                                            ", ENTRY},
   {"Turbo Mode                                              ", ENTRY},
   {"Turbo Speed                                             ", ENTRY},
#if defined(SDL2) || !defined(NOX)
   {"Turbo Paste                                             ", ENTRY},
#endif
   {"", 0}};
  const char *printer[] = {"     None", "     File", "     Text"};
  int selection = 0;

  while (1) {
    char input[12];

    snprintf(&menu[1].text[50], 11, "%s", yes_no[trs_emtsafe]);
    snprintf(&menu[2].text[56], 5, "%4d", trs_year);
    snprintf(&menu[3].text[50], 11, "%10d", stretch_amount);
    snprintf(&menu[4].text[51], 10, "%s", printer[trs_printer]);
    gui_limit(trs_uart_name, &menu[6].text[2], 58);
    snprintf(&menu[7].text[56], 5, "0x%02X", trs_uart_switches);
    snprintf(&menu[8].text[50], 11, "%s", yes_no[trs_kb_bracket_state]);
    snprintf(&menu[9].text[50], 11, "%s", yes_no[trs_sound]);
    snprintf(&menu[10].text[50], 11, "%s", yes_no[turbo_mode]);
    snprintf(&menu[11].text[50], 11, "%10d", turbo_rate);
#if defined(SDL2) || !defined(NOX)
    snprintf(&menu[12].text[50], 11, "%s", yes_no[turbo_paste]);
#endif
    gui_clear();

    selection = gui_menu(" Miscellaneous/Printer ", menu, selection);
    switch (selection) {
      case 0:
        if (trs_printer_reset() == 0)
          gui_message("Status", "Printer file closed");
        else
          gui_message("Warning", "No Printer Output in File");
        break;
      case 1:
        trs_emtsafe = !trs_emtsafe;
        break;
      case 2:
        snprintf(input, 5, "%d", trs_year);
        if (gui_input(" Enter year (0 to disable) ",
            input, input, 4, 0) > 0) {
          trs_year = atoi(input);
          if (trs_year < 0)
            trs_year = 0;
        }
        break;
      case 3:
        snprintf(input, 11, "%d", stretch_amount);
        if (gui_input(" Enter Keystretch in Cycles ",
            input, input, 10, 0) > 0) {
          stretch_amount = atoi(input);
          if (stretch_amount < 0)
            stretch_amount = STRETCH_AMOUNT;
        }
        break;
      case 4:
        trs_printer = gui_popup("Printer", printer, 3, trs_printer);
        break;
      case 6:
        filename[0] = 0;
        if (gui_input(" Enter Serial Port Name ", trs_uart_name,
            filename, FILENAME_MAX, 0) >= 0) {
          snprintf(trs_uart_name, FILENAME_MAX, "%s", filename);
          trs_uart_init();
        }
        break;
      case 7:
        snprintf(input, 3, "%2X", trs_uart_switches);
        if (gui_input(" Enter Serial Switches (Hex, XX) ",
            input, input, 2, 0) > 0) {
          trs_uart_switches = strtol(input, NULL, 16);
          trs_uart_init();
        }
        break;
      case 8:
        trs_kb_bracket(!trs_kb_bracket_state);
        break;
      case 9:
        trs_sound = !trs_sound;
        trs_screen_caption();
        break;
      case 10:
        trs_timer_mode(!turbo_mode);
        break;
      case 11:
        snprintf(input, 11, "%d", turbo_rate);
        if (gui_input(" Enter Turbo Rate Multiplier ",
            input, input, 10, 0) > 0) {
          turbo_rate = atoi(input);
          if (turbo_rate <= 1) {
            turbo_rate = 5;
            turbo_mode = 0;
          }
          trs_timer_mode(turbo_mode);
        }
        break;
#if defined(SDL2) || !defined(NOX)
      case 12:
        turbo_paste = !turbo_paste;
        break;
#endif
      case -1:
        return;
    }
  }
}

void gui_save_state(void)
{
  filename[0] = 0;
  if (gui_input(" Save Emulator State ",
      trs_state_file[0] != 0 ? trs_state_file : trs_state_dir,
      filename, FILENAME_MAX - 5, 1) > 0) {
    gui_add_extension(filename, T8S);
    if (gui_file_overwrite()) {
      if (trs_state_save(filename) == 0)
        snprintf(trs_state_file, FILENAME_MAX, "%s", filename);
      else
        gui_error(filename);
    }
  }
}

int gui_load_state(void)
{
  if (gui_file(trs_state_dir, filename, T8S, 0, "State") >= 0) {
    if (trs_state_load(filename) == 0) {
      snprintf(trs_state_file, FILENAME_MAX, "%s", filename);
      trs_screen_init(1);
      return 0;
    } else
      gui_error(filename);
  }
  return -1;
}

void gui_write_config(void)
{
  filename[0] = 0;
  if (gui_input(" Write Configuration ",
      trs_config_file[0] != 0 ? trs_config_file : trs_state_dir,
      filename, FILENAME_MAX - 5, 1) > 0) {
    gui_add_extension(filename, T8C);
    if (gui_file_overwrite()) {
      if (trs_write_config_file(filename) == 0)
        snprintf(trs_config_file, FILENAME_MAX, "%s", filename);
      else
        gui_error(filename);
    }
  }
}

int gui_read_config(void)
{
  if (gui_file(trs_config_file, trs_config_file, T8C, 0, "Configuration") >= 0) {
    if (trs_load_config_file() == 0) {
      trs_reset(1);
      return 0;
    }
    gui_error(trs_config_file);
  }
  return -1;
}

static int gui_config_menu(void)
{
  MENU menu[] =
  {{"Save Emulator State (Alt-S)", ENTRY},
   {"Load Emulator State (Alt-L)", ENTRY},
   {"Write Configuration (Alt-W)", ENTRY},
   {"Read Configuration  (Alt-R)", ENTRY},
   {"", 0}};
  int selection = 0;

  while (1) {
    gui_clear();
    gui_write("State File:", 2, 8, 4);
    gui_write(trs_state_file, 4, 9, 4);
    gui_write("Configuration File:", 2, 11, 4);
    gui_write(trs_config_file, 4, 12, 4);

    selection = gui_menu(" Configuration/State Files ", menu, selection);
    switch (selection) {
      case 0:
        gui_save_state();
        break;
      case 1:
        if (gui_load_state() == 0)
          return 0;
        break;
      case 2:
        gui_write_config();
        break;
      case 3:
        if (gui_read_config() == 0)
          return 0;
        break;
      case -1:
        return -1;
    }
  }
}

const char *gui_key_name(int key)
{
  switch (key) {
    case -1:     return "---";
    case GUI:    return "<GUI>";
    case KEYBRD: return "<KEYBRD>";
    case SAVE:   return "<SAVE>";
    case LOAD:   return "<LOAD>";
    case RESET:  return "<RESET>";
    case EXIT:   return "<EXIT>";
    case PAUSE:  return "<PAUSE>";
    case JOYGUI: return "<JOYGUI>";
    default:
    {
      int i;

      for (i = 0; i < N_KEYS; i++) {
        if (key == key_syms[i])
          return key_names[i];

        if (key == key_syms_shifted[i])
          return key_names_shifted[i];
      }
    }
  }
  return "???";
}

int gui_virtual_keyboard(void)
{
  static int saved_selection;
  int key_index = SHIFT, shifted = 0;

  while (key_index == SHIFT || (shifted && key_syms_shifted[key_index] == -1)) {
    if ((key_index = gui_matrix(" Select Key ",
        !shifted ? key_names : key_names_shifted, 4, 13, saved_selection)) == -1)
      return -1;

    if (key_index == SHIFT)
      shifted = !shifted;

    saved_selection = key_index;
  }
  return !shifted ? key_syms[key_index] : key_syms_shifted[key_index];
}

void gui_virtual_key(void)
{
  int const key = gui_virtual_keyboard();

  if (key != -1)
    trs_xlate_keysym(key);
}

void gui_joy_gui(void)
{
  int const selection = gui_matrix(" Joystick GUI ", function_menu, 3, 2, 0);

  if (selection == -1)
    return;

  switch (function_codes[selection]) {
    case GUI:
      gui_main_menu();
      break;
    case KEYBRD:
      gui_virtual_key();
      break;
    case SAVE:
      gui_save_state();
      break;
    case LOAD:
      gui_load_state();
      break;
    case RESET:
      trs_reset(1);
      break;
    case EXIT:
      trs_exit(1);
      break;
  }
}

int gui_joystick_button(void)
{
  SDL_Event event;

  gui_rect(25, 7, 14, 3, 1);
  gui_text("Press Button", 26, 8, 13, 4);
  trs_screen_update();

  while (1) {
    SDL_WaitEvent(&event);
    switch (event.type) {
      case SDL_QUIT:
        trs_exit(0);
        break;
      case SDL_KEYDOWN:
        switch (event.key.keysym.sym) {
#if defined(__OS2__) || defined(_WIN32)
          case SDLK_F4:
#endif
          case SDLK_q:
          case SDLK_END:
            if (event.key.keysym.mod & KMOD_ALT)
              trs_exit(1);
            break;
          case SDLK_F8:
            trs_exit(!(event.key.keysym.mod & KMOD_SHIFT));
            break;
          case SDLK_BACKSPACE:
          case SDLK_ESCAPE:
            return -1;
          default:
            break;
        }
        break;
      case SDL_JOYBUTTONDOWN:
      case SDL_MOUSEBUTTONDOWN:
        if (event.type == SDL_MOUSEBUTTONDOWN)
          event.jbutton.button = event.button.button;
        if (event.jbutton.button < JOY_BUTTONS)
          return event.jbutton.button;
        else {
          gui_message("ERROR", "Unsupported Button");
          return -1;
        }
    }
  }
}

void gui_joystick_map(int button)
{
  int row, col;
  char text[12];

  for (col = 0; col < 5; col++) {
    for (row = 0; row < 4; row++) {
      int const pos = col * 4 + row;

      snprintf(text, 12, "%2d:%s", pos, gui_key_name(jbutton_map[pos]));
      gui_text(text, 2 + col * 12, 10 + row, 12, 4 + (button == pos));
    }
  }
}

void gui_joystick_menu(void)
{
  MENU menu[] =
  {{"Use Keypad for Joystick                           ", ENTRY},
   {"USB Joystick/Gamepad                              ", ENTRY},
   {"Map Joystick/Mouse to Arrow Keys                  ", ENTRY},
   {"Map Button to Key", ENTRY},
   {"Map Button to Function", ENTRY},
   {"Unmap Button", ENTRY},
   {"Unmap All Buttons", ENTRY},
   {"Check Button Mapping", ENTRY},
   {"", 0}};
  int selection = 0;
  int button, key;
  int i;

  while (1) {
    snprintf(&menu[0].text[50], 11, "%s", yes_no[trs_keypad_joystick]);

    if (trs_joystick == -1)
      snprintf(&menu[1].text[50], 11, "      None");
    else
      snprintf(&menu[1].text[50], 11, "Joystick %1d", trs_joystick);

    snprintf(&menu[2].text[50], 11, "%s", yes_no[jaxis_mapped]);
    gui_clear();
    gui_joystick_map(-1);

    selection = gui_menu(" Joystick Settings ", menu, selection);
    switch (selection) {
      case 0:
        trs_keypad_joystick = !trs_keypad_joystick;
        trs_set_keypad_joystick();
        break;
      case 1:
      {
        char *joystick[MAX_JOYSTICKS + 1];
        char joysticks[MAX_JOYSTICKS + 1][64];
        int joy_index;
        int num_joysticks = SDL_NumJoysticks();

        if (num_joysticks > MAX_JOYSTICKS)
          num_joysticks = MAX_JOYSTICKS;

        joystick[0] = joysticks[0];
        snprintf(joystick[0], 61, "%60s", "None");

        for (i = 0; i < num_joysticks; i++) {
          joystick[i + 1] = joysticks[i + 1];
          snprintf(joystick[i + 1], 61, "Joystick %1d - %47s", i,
#ifdef SDL2
              SDL_JoystickName(SDL_JoystickOpen(i)));
#else
              SDL_JoystickName(i));
#endif
        }

        if ((trs_joystick == -1) || (trs_joystick >= num_joysticks))
          joy_index = 0;
        else
          joy_index = trs_joystick + 1;

        joy_index = gui_popup("Joystick", (const char**)joystick,
            num_joysticks + 1, joy_index);
        trs_joystick = joy_index - 1;
        trs_open_joystick();
        break;
      }
      case 2:
        jaxis_mapped = !jaxis_mapped;
        break;
      case 3:
        if ((key = gui_virtual_keyboard()) != -1) {
          if ((button = gui_joystick_button()) != -1)
            jbutton_map[button] = key;
        }
        break;
      case 4:
        if ((key = gui_matrix(" Select Function ",
            function_menu, 4, 2, 0)) != -1) {
          if ((button = gui_joystick_button()) != -1)
            jbutton_map[button] = function_codes[key];
        }
        break;
      case 5:
        if ((button = gui_joystick_button()) != -1)
          jbutton_map[button] = -1;
        break;
      case 6:
        if (gui_question("Sure")) {
          for (i = 0; i < JOY_BUTTONS; i++)
            jbutton_map[i] = -1;
         }
        break;
      case 7:
        while ((button = gui_joystick_button()) != -1) {
          gui_joystick_map(button);
          trs_screen_update();
        }
        break;
      case -1:
        return;
    }
  }
}

void gui_default_dirs(void)
{
  MENU menu[] =
  {{"Floppy Disk:", TITLE},
   {"   ", ENTRY},
   {"Hard Disk:", TITLE},
   {"   ", ENTRY},
   {"Cassette/Wafer:", TITLE},
   {"   ", ENTRY},
   {"Disk Set:", TITLE},
   {"   ", ENTRY},
   {"State/Configuration:", TITLE},
   {"   ", ENTRY},
   {"Printer Output/Screenshot:", TITLE},
   {"   ", ENTRY},
   {"", 0}};
  int selection = 1;

  while (1) {
    gui_limit(trs_disk_dir, &menu[1].text[2], 58);
    gui_limit(trs_hard_dir, &menu[3].text[2], 58);
    gui_limit(trs_cass_dir, &menu[5].text[2], 58);
    gui_limit(trs_disk_set_dir, &menu[7].text[2], 58);
    gui_limit(trs_state_dir, &menu[9].text[2], 58);
    gui_limit(trs_printer_dir, &menu[11].text[2], 58);
    gui_clear();

    selection = gui_menu(" Default Directories ", menu, selection);
    switch (selection) {
      case 1:
        gui_file(trs_disk_dir, trs_disk_dir, NULL, 1, "Floppy Disk ");
        break;
      case 3:
        gui_file(trs_hard_dir, trs_hard_dir, NULL, 1, "Hard Disk ");
        break;
      case 5:
        gui_file(trs_cass_dir, trs_cass_dir, NULL, 1, "Cassette/Wafer ");
        break;
      case 7:
        gui_file(trs_disk_set_dir, trs_disk_set_dir, NULL, 1, "Disk Set ");
        break;
      case 9:
        gui_file(trs_state_dir, trs_state_dir, NULL, 1, "State/Configuration ");
        break;
      case 11:
        gui_file(trs_printer_dir, trs_printer_dir, NULL, 1, "Printer/Screenshot ");
        break;
      case -1:
        return;
    }
  }
}

void gui_roms(void)
{
  MENU menu[] =
  {{"Model I ROM:", TITLE},
   {"   ", ENTRY},
   {"", TITLE},
   {"Model III / 4 ROM:", TITLE},
   {"   ", ENTRY},
   {"", TITLE},
   {"Model 4P ROM:", TITLE},
   {"   ", ENTRY},
   {"", TITLE},
   {"", TITLE},
   {"", TITLE},
   {"Patch Model I ROM for auto-boot from hard drive   ", ENTRY},
   {"", 0}};
  int selection = 1;

  while (1) {
    gui_limit(romfile1, &menu[1].text[2], 58);
    gui_limit(romfile3, &menu[4].text[2], 58);
    gui_limit(romfile4p, &menu[7].text[2], 58);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[trs_hd_boot]);
    gui_clear();

    selection = gui_menu(" ROM File Selection ", menu, selection);
    switch (selection) {
      case 1:
        gui_file(romfile1, romfile1, ROM, 0, "Model I ROM");
        break;
      case 4:
        gui_file(romfile3, romfile3, ROM, 0, "Model III / 4 ROM");
        break;
      case 7:
        gui_file(romfile4p, romfile4p, ROM, 0, "Model 4P ROM");
        break;
      case 11:
        trs_hd_boot = !trs_hd_boot;
        break;
      case -1:
        return;
    }
  }
}

void gui_about(void)
{
  gui_clear();
  gui_write(" About ", 0, 0, 4);
  gui_write("SDLTRS", 0, 3, 4);
  gui_write("Version 1.2.34", 0, 4, 4);
  gui_write("BSD 2-Clause License", 0, 5, 4);
  gui_write("Copyright (C) 2006-2011 Mark Grebe, 2018-2026", 0, 6, 4);
  gui_write("Alan Cox, Jens Guenther, Leonardo Brondani Schenkel", 0, 7, 4);
  gui_write("<https://gitlab.com/jengun/sdltrs>", 0, 8, 4);
  gui_write("Based on xtrs 4.9d by Tim Mann", 0, 10, 4);
  gui_write("<http://www.tim-mann.org/xtrs>", 0, 11, 4);
  gui_write("xtrs 1.0 Copyright (C) 1992 Clarendon Hill Software", 0, 12, 4);
  gui_write(" Press Any Key To Return ", 0, 15, 5);
  trs_screen_update();
  gui_key();
}

void gui_keys(void)
{
  gui_clear();
  gui_text(" Keys ", 28, 0, 9, 4);
  gui_text("F1-F3: Functions Keys F1/F2/F3  PgUp/PgDn: Left/Right Shift ", 2, 1, 60, 4);
  gui_text("F4: F4/CapsLock on TRS-80 4/4P  Insert: TRS-80 Underscore   ", 2, 2, 60, 4);
  gui_text("F5/ScrollLock: TRS-80 '@' Key   Shift-Up Arrow: TRS-80 ESC  ", 2, 3, 60, 4);
  gui_text("F6: TRS-80 Shifted '0' Key      Alt-PgUp/PgDn: Scale Window ", 2, 4, 60, 4);
  gui_text("F7/Alt-M: Main Menu of SDLTRS   Alt-Enter: Toggle Fullscreen", 2, 5, 60, 4);
  gui_text("F8/Shift-F8: Quit/Abort SDLTRS  Alt-A/C/V: Select/Copy/Paste", 2, 6, 60, 4);
  gui_text("F9/Alt-Z:"
#ifdef ZBX
  " Enter debugger (zbx)  "
#else
  " Toggle Fullscreen     "
#endif
  "Alt-D/F: Floppy Disk Menu   ", 2, 7, 60, 4);
  gui_text("F10/Shift-F10: Soft/Hard Reset  Alt-H: Hard Disk Menu       ", 2,  8, 60, 4);
  gui_text("F11/Alt-K: Show this key help   Alt-T: Cassette/Tape Menu   ", 2,  9, 60, 4);
  gui_text("F12/Alt-N: Switch Turbo On/Off  Alt-L/S: Load / Save State  ", 2, 10, 60, 4);
  gui_text("ESC: TRS-80 Break Key           Alt-R/W: Read / Write Config", 2, 11, 60, 4);
  gui_text("Delete/Home: TRS-80 Clear Key   Alt-P/Pause: Pause Emulator ", 2, 12, 60, 4);
  gui_text("End: TRS-80 Shifted Down Arrow  Alt-0...7: Insert Disk Drive", 2, 13, 60, 4);
  gui_text("Control: TRS-80 Control Key     Alt-Shift-0...7: Remove Disk", 2, 14, 60, 4);
  gui_write(" Press Any Key To Return ", 0, 15, 5);
  trs_screen_update();
  gui_key();
}

void gui_pause(void)
{
  gui_rect(29, 6, 8, 3, 1);
  gui_text("PAUSED", 30, 7, 7, 4);
  trs_screen_update();
}

void gui_exec_cmd(void)
{
  if (gui_file(trs_cmd_file, trs_cmd_file, CMD, 0, "CMD") >= 0) {
    if (trs_load_cmd(trs_cmd_file) != 0)
      gui_message("ERROR", "Failed to load CMD file");
  }
}

int gui_exit(void)
{
  return gui_question("Quit");
}

void gui_save_bmp(void)
{
  filename[0] = 0;
  if (gui_input(" Save Screenshot ",
      trs_printer_dir, filename, FILENAME_MAX - 5, 1) > 0) {
    gui_add_extension(filename, ".bmp");
    if (gui_file_overwrite()) {
      if (trs_sdl_savebmp(filename) != 0)
        gui_error(filename);
    }
  }
}

void gui_main_menu(void)
{
  MENU menu[] =
  {{"Floppy Disk Management   (Alt-D)", ENTRY},
   {"Hard Disk Management     (Alt-H)", ENTRY},
   {"Cassette/Tape Management (Alt-T)", ENTRY},
   {"Stringy Wafer Management (Alt-G)", ENTRY},
   {"Emulator Settings        (Alt-E)", ENTRY},
   {"Display Settings         (Alt-I)", ENTRY},
   {"Miscellaneous/Printer    (Alt-O)", ENTRY},
   {"Configuration/State Files", ENTRY},
   {"Joystick Settings", ENTRY},
   {"Default Directories", ENTRY},
   {"ROM File Selection", ENTRY},
   {"TRS-80 Power Reset", ENTRY},
   {"About SDLTRS", ENTRY},
   {"Quit SDLTRS", ENTRY},
   {"", 0}};
  int selection = 0;

  while (1) {
    gui_clear();

    selection = gui_menu(" Main Menu ", menu, selection);
    switch (selection) {
      case 0:
        gui_disk_menu();
        break;
      case 1:
        gui_hard_menu();
        break;
      case 2:
        gui_cassette_menu();
        break;
      case 3:
        gui_stringy_menu();
        break;
      case 4:
        gui_emulator_menu();
        break;
      case 5:
        gui_display_menu();
        break;
      case 6:
        gui_misc_menu();
        break;
      case 7:
        if (gui_config_menu() == 0)
          return;
        break;
      case 8:
        gui_joystick_menu();
        break;
      case 9:
        gui_default_dirs();
        break;
      case 10:
        gui_roms();
        break;
      case 11:
        if (gui_question("Reset")) {
          trs_reset(1);
          return;
        }
        break;
      case 12:
        gui_about();
        break;
      case 13:
        trs_exit(1);
        break;
      case -1:
        return;
    }
  }
}

void gui_function(int function)
{
#ifdef SDL2
  /* Prevent double chars */
  SDL_FlushEvent(SDL_TEXTINPUT);
#endif

  SDL_PauseAudio(1);

  switch (function) {
    case PAUSE:
      trs_paused = !trs_paused;
      trs_screen_caption();
      break;
    case RESET:
      trs_reset(1);
      return;
    case EXIT:
      trs_exit(1);
      return;
    case GUI:
      gui_main_menu();
      break;
    case JOYGUI:
      gui_joy_gui();
      break;
    case KEYBRD:
      gui_virtual_key();
      break;
    case SAVE:
      gui_save_state();
      break;
    case LOAD:
      gui_load_state();
      break;
    case DISK:
      gui_disk_menu();
      break;
    case HARD:
      gui_hard_menu();
      break;
    case STRINGY:
      gui_stringy_menu();
      break;
    case TAPE:
      gui_cassette_menu();
      break;
    case WRITE:
      gui_write_config();
      break;
    case READ:
      gui_read_config();
      break;
    case EMULATOR:
      gui_emulator_menu();
      break;
    case INTERFACE:
      gui_display_menu();
      break;
    case OTHER:
      gui_misc_menu();
      break;
    case KEYS:
      gui_keys();
      break;
    case EXEC:
      gui_exec_cmd();
      break;
    case SAVE_BMP:
      gui_save_bmp();
      break;
  }

  trs_screen_refresh();
  SDL_PauseAudio(0);
}
