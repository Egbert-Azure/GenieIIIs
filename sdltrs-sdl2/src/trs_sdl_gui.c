/*
 * Copyright (C) 2006-2011, Mark Grebe
 * Copyright (C) 2018-2023, Jens Guenther
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
#include "trs_mkdisk.h"
#include "trs_sdl_gui.h"
#include "trs_sdl_keyboard.h"
#include "trs_state_save.h"
#include "trs_stringy.h"
#include "trs_uart.h"

#define MENU_NORMAL   1
#define MENU_TITLE    2
#define MENU_DISK     3
#define MENU_HARD     4
#define MENU_WAFER    5
#define MENU_CASS     6
#define MENU_SAVE_SET 7
#define MENU_LOAD_SET 8

#define MAX_JOYSTICKS 8

#define N_KEYS        52
#define SHIFT         39

static char filename[FILENAME_MAX];
static char **filenamelist;
static int filenamecount;
static int filenamelistsize;

typedef struct menu_entry_type {
  char text[64];
  int const type;
} MENU_ENTRY;

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

static void trs_gui_write_text(const char *text, int x, int y, int invert);
static void trs_gui_center_text(const char *text, int y, int invert);
static void trs_gui_frame(int x1, int y1, int x2, int y2);
static void trs_gui_clear_screen(void);
static void trs_gui_limit_string(const char *orig, char *limited, int limit);
static void trs_add_extension(char *name, const char *ext);
static int  trs_gui_get_key(void);
static int  trs_gui_select(const char *text, int x, int y);
static void trs_gui_display_error(const char *name);
static void trs_gui_display_message(const char *title, const char *message);
static void trs_gui_create_filename_list(void);
static void trs_gui_add_to_filename_list(char *name);
static int  trs_gui_filename_cmp(const void *nptr1, const void *nptr2);
static void trs_gui_delete_filename_list(void);
static int  trs_gui_readdirectory(const char *path, const char *mask, int browse_dir);
static int  trs_gui_input_string(const char *title, const char *input, char* output,
                                 int limit, int file);
static int  trs_gui_display_menu(const char* title, MENU_ENTRY *entry, int selection);
static int  trs_gui_display_popup(const char *title, const char **entry,
                                  int num, int selection);
static int  trs_gui_display_popup_matrix(const char *title, const char **entry,
                                         int rows, int cols, int selection);
static int  trs_gui_display_question(const char *text);
static int  trs_gui_file_overwrite(void);
static void trs_gui_disk_creation(void);
#ifdef __linux
static void trs_gui_disk_steps(void);
#endif
static void trs_gui_disk_options(void);
static void trs_gui_diskset_load(void);
static void trs_gui_diskset_save(void);
static void trs_gui_disk_management(void);
static void trs_gui_hard_management(void);
static void trs_gui_stringy_management(void);
static void trs_gui_cassette_management(void);
static void trs_gui_emulator_settings(void);
static void trs_gui_display_settings(void);
static void trs_gui_misc_settings(void);
static void trs_gui_save_state(void);
static int  trs_gui_load_state(void);
static void trs_gui_write_config(void);
static int  trs_gui_read_config(void);
static int  trs_gui_config_management(void);
static const char *trs_gui_get_key_name(int key);
static int  trs_gui_virtual_keyboard(void);
static void trs_gui_get_virtual_key(void);
static void trs_gui_joy_gui(void);
static int  trs_gui_joystick_get_button(void);
static void trs_gui_joystick_display_map(int button);
static void trs_gui_joystick_settings(void);
static void trs_gui_default_dirs(void);
static void trs_gui_rom_files(void);
static void trs_gui_about_sdltrs(void);
static void trs_gui_keys_sdltrs(void);
static void trs_gui_exec_cmd(void);
static void trs_gui_save_bmp(void);
static void trs_gui(void);

void trs_gui_write_text(const char *text, int x, int y, int invert)
{
  int const len = strlen(text);
  int const end = 62 - x;
  int i;

  if (len > end) {
    int const len_1st_part = (59 - x) / 2;
    int pos_2nd_part = len - (59 - x - len_1st_part);

    for (i = 0; i < len_1st_part; i++)
      trs_gui_write_char(x++, y, text[i], invert);

    for (; i < len_1st_part + 3; i++)
      trs_gui_write_char(x++, y, '.', invert);

    for (; i < end; i++)
      trs_gui_write_char(x++, y, text[pos_2nd_part++], invert);
  } else {
    for (i = 0; i < len; i++)
      trs_gui_write_char(x++, y, text[i], invert);
  }
}

void trs_gui_center_text(const char *text, int y, int invert)
{
  trs_gui_write_text(text, (64 - strlen(text)) / 2, y, invert);
}

void trs_gui_frame(int x1, int y1, int x2, int y2)
{
  int i;

  for (i = x1 + 1; i < x2; i++) {
    trs_gui_write_char(i, y1, 131, 0);
    trs_gui_write_char(i, y2, 176, 0);
  }

  for (i = y1 + 1; i < y2; i++) {
    trs_gui_write_char(x1, i, 149, 0);
    trs_gui_write_char(x2, i, 170, 0);
  }

  trs_gui_write_char(x1, y1, 151, 0);
  trs_gui_write_char(x2, y1, 171, 0);
  trs_gui_write_char(x1, y2, 181, 0);
  trs_gui_write_char(x2, y2, 186, 0);
}

void trs_gui_clear_screen(void)
{
  trs_gui_clear_rect(0, 0, 64, 16);
}

void trs_gui_limit_string(const char *orig, char *limited, int limit)
{
  int const len = strlen(orig);

  if (len > limit) {
    int const len_1st_part = (limit - 3) / 2;
    int const pos_2nd_part = len - (limit - len_1st_part - 3);

    snprintf(limited, limit + 1, "%.*s...%s", len_1st_part, orig,
        orig + pos_2nd_part);
  } else
    snprintf(limited, limit + 1, "%s", orig);
}

void trs_add_extension(char *name, const char *ext)
{
  int const len = strlen(name);

  if (len >= 4)
    if (strcasecmp(&name[len - 4], ext) == 0)
      return;

  if (len && name[len - 1] != DIR_SLASH)
    snprintf(name + len, FILENAME_MAX - len, "%s", ext);
}

int trs_gui_get_key(void)
{
  SDL_Event event;

  /* Stop Text input first to prevent double chars */
  SDL_StopTextInput();
  SDL_StartTextInput();

  while (1) {
    SDL_WaitEvent(&event);
    switch (event.type) {
      case SDL_QUIT:
        trs_exit(0);
        break;
      case SDL_WINDOWEVENT:
        trs_screen_update();
        break;
      case SDL_MOUSEBUTTONDOWN:
        switch (event.button.button) {
          case SDL_BUTTON_LEFT:
            return SDLK_RETURN;
          case SDL_BUTTON_MIDDLE:
            return SDLK_TAB;
          case SDL_BUTTON_RIGHT:
            return SDLK_ESCAPE;
          default:
            break;
        }
        break;
      case SDL_MOUSEWHEEL:
        if (event.wheel.y > 0)
          return SDLK_UP;
        if (event.wheel.y < 0)
          return SDLK_DOWN;
        break;
      case SDL_TEXTINPUT:
        SDL_StopTextInput();
        return event.text.text[0];
      case SDL_KEYDOWN:
        if (event.key.keysym.mod & KMOD_ALT) {
          switch (event.key.keysym.sym) {
#if defined(__OS2__) || defined(_WIN32)
            case SDLK_F4:
#endif
            case SDLK_q:
            case SDLK_END:
              trs_exit(1);
              break;
            case SDLK_BACKSPACE:
              return SDLK_F9;
            case SDLK_DELETE:
              return SDLK_F10;
            default:
              break;
          }
        }
        else if (event.key.keysym.sym == SDLK_F8)
          trs_exit(!(event.key.keysym.mod & KMOD_SHIFT));
        else if (event.key.keysym.sym < 0x20 ||
                 event.key.keysym.sym > 0x7E)
          return event.key.keysym.sym;
        break;
      case SDL_JOYBUTTONDOWN:
        if (event.jbutton.button < N_JOYBUTTONS) {
          int const key = jbutton_map[event.jbutton.button];

          if (key >= 0)
            return key;
          else if (key == KEYBRD || key == JOYGUI)
            return trs_gui_virtual_keyboard();
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

int trs_gui_select(const char *text, int x, int y)
{
  int key;

  trs_gui_write_text(text, x, y, 1);
  trs_screen_update();
  key = trs_gui_get_key();
  trs_gui_write_text(text, x, y, 0);

  return key;
}

void trs_gui_display_error(const char *name)
{
  if (errno) {
    char text[60];

    if (snprintf(text, 60, "%s: %s", strerror(errno), name))
      trs_gui_display_message("ERROR", text);
  }
}

void trs_gui_display_message(const char* title, const char *message)
{
  trs_gui_frame(1, 6, 62, 8);
  trs_gui_clear_rect(2, 7, 60, 1);
  trs_gui_write_text(title, 3, 6, 0);
  trs_gui_write_text(message, 3, 7, 0);
  trs_gui_write_text(" Press ENTER/ESC to continue ", 32, 8, 1);
  trs_screen_update();

  while (1) {
    switch (trs_gui_get_key()) {
      case SDLK_ESCAPE:
      case SDLK_RETURN:
        return;
    }
  }
}

void trs_gui_create_filename_list(void)
{
  if (filenamelist == NULL) {
    if ((filenamelist = (char **)malloc(256 * sizeof(char *))) == NULL)
      fatal("failed to allocate filenamelist");

    filenamelistsize = 256;
  }
}

void trs_gui_add_to_filename_list(char *name)
{
  filenamelist[filenamecount++] = name;
  if (filenamecount == filenamelistsize) {
    char **filenamelist_new;

    if ((filenamelist_new = realloc(filenamelist, 2 *
        filenamelistsize * sizeof(char*))) == NULL) {
      free(filenamelist);
      fatal("failed to reallocate filenamelist");
    }

    filenamelist = filenamelist_new;
    filenamelistsize *= 2;
  }
}

int trs_gui_filename_cmp(const void *nptr1, const void *nptr2)
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

void trs_gui_delete_filename_list(void)
{
  int i;

  for (i = 0; i < filenamecount; i++)
    free(filenamelist[i]);

  filenamecount = 0;
}

int trs_gui_readdirectory(const char *path, const char *mask, int browse_dir)
{
  DIR *directory = opendir(path);

  if (directory) {
    char   pathname[FILENAME_MAX];
    char  *name = NULL;
    struct dirent *dir_entry;
    struct stat st = { 0 };

    trs_gui_create_filename_list();
    while ((dir_entry = readdir(directory))) {
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
      else if (browse_dir) {
        continue;
      } else {
        if (mask != NULL) {
          if (len < 4)
            continue;
          if (strcasecmp(&dir_entry->d_name[len - 4], mask) != 0)
            continue;
        }
        name = (char *)strdup(dir_entry->d_name);
      }
      if (!name) {
        closedir(directory);
        return -1;
      }
      trs_gui_add_to_filename_list(name);
    }
    closedir(directory);

    qsort(filenamelist, filenamecount, sizeof(char *), trs_gui_filename_cmp);
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
          trs_gui_add_to_filename_list(strdup(drive));
        }
        drive_mask >>= 1;
      }
    }
#endif
    return 0;
  } else {
    error("failed to open directory '%s': %s", path, strerror(errno));
    return -1;
  }
}

int trs_gui_file_browse(const char *path, char *name, const char *mask,
                        int browse_dir, const char* type)
{
  char current_dir[FILENAME_MAX];
  char text[64];
  struct stat st = { 0 };
  const char *new_dir;
  int i;
  int selection;
  int first_row;
  int drawcount;
  int redraw;

  snprintf(current_dir, FILENAME_MAX, "%s", path);

  for (i = strlen(current_dir); i > 0; i--) {
    if (current_dir[i] == DIR_SLASH) {
      current_dir[i + 1] = 0;
      break;
    }
  }

  stat(current_dir, &st);
  if (S_ISDIR(st.st_mode) == 0 || current_dir[1] == DIR_SLASH) {
    if (getcwd(current_dir, FILENAME_MAX) == NULL)
      current_dir[0] = 0;

    snprintf(current_dir + strlen(current_dir), FILENAME_MAX - strlen(current_dir),
        "%c", DIR_SLASH);
  }

read_directory:
  trs_gui_delete_filename_list();

  if (trs_gui_readdirectory(current_dir, mask, browse_dir) != 0)
    return -1;

  trs_gui_limit_string(current_dir, text, 58);
  trs_gui_clear_screen();
  trs_gui_frame(0, 0, 63, 15);
  trs_gui_center_text(text, 1, 0);

  if (browse_dir) {
    snprintf(text, 63, "Choose %sDirectory", type);
    trs_gui_center_text(" INS/TAB:Select Directory ", 15, 1);
  } else {
    snprintf(text, 63, "Select %s File To Load", type);
    trs_gui_center_text(" ENTER/INS/SPACE/TAB:Select  BACKSPACE/ESC/F7:Return ", 15, 1);
  }

  trs_gui_write_text(text, 2, 0, 0);

  drawcount = filenamecount < 13 ? filenamecount : 13;
  first_row = selection = 0;
  redraw = 1;

  while (1) {
    int key;
    int sel;

    if (redraw) {
      trs_gui_clear_rect(2, 2, 60, 13);

      for (i = 0; i < drawcount; i++)
        trs_gui_write_text(filenamelist[first_row + i], 2, i + 2, 0);

      redraw = 0;
    }
    key = trs_gui_select(filenamelist[first_row + selection], 2, selection + 2);
    if (key >= '0' && key <= 'z') {
      key = tolower(key);
      sel = i = first_row + selection;

      do {
        if (++i > filenamecount - 1)
          i = 0;
      } while (i != sel && (tolower((int)*filenamelist[i]) != key));

      if (i < 13) {
        first_row = 0;
        selection = i;
      } else if (i + 13 > filenamecount) {
        first_row = filenamecount - 13;
        selection = i - first_row;
      } else {
        first_row = i - selection;
      }

      redraw = 1;
    } else {
      switch (key) {
        case SDLK_DOWN:
        case SDLK_RIGHT:
          if (selection < drawcount - 1)
            selection++;
          else
            if (first_row < filenamecount - drawcount) {
              first_row++;
              redraw = 1;
            }
          break;
        case SDLK_UP:
        case SDLK_LEFT:
          if (selection > 0)
            selection--;
          else
            if (first_row > 0) {
              first_row--;
              redraw = 1;
            }
          break;
        case SDLK_PAGEUP:
          first_row -= drawcount;
          if (first_row < 0)
            first_row = selection = 0;
          redraw = 1;
          break;
        case SDLK_PAGEDOWN:
          first_row += drawcount;
          if (first_row > filenamecount - drawcount) {
            first_row = filenamecount - drawcount;
            selection = drawcount - 1;
          }
          redraw = 1;
          break;
        case SDLK_HOME:
          selection = first_row = 0;
          redraw = 1;
          break;
        case SDLK_END:
          selection = drawcount - 1;
          first_row = filenamecount - drawcount;
          redraw = 1;
          break;
        case SDLK_INSERT:
        case SDLK_TAB:
          if (browse_dir)
            goto done;
          /* Fall through */
        case SDLK_RETURN:
        case SDLK_SPACE:
          new_dir = filenamelist[first_row + selection];
          if (new_dir[0] == '<') {
            if (new_dir[1] == '.' && new_dir[2] == '.') {
              for (i = strlen(current_dir) - 2; i >= 0; i--) {
                if (current_dir[i] == DIR_SLASH) {
                  current_dir[i + 1] = 0;
                  break;
                }
              }
            } else {
              snprintf(current_dir + strlen(current_dir),
                  FILENAME_MAX - strlen(current_dir), "%s", &new_dir[1]);
              current_dir[strlen(current_dir) - 1] = DIR_SLASH;
            }
            goto read_directory;
          }
#if defined(__OS2__) || defined(_WIN32)
          /* Select a new drive */
          else if (new_dir[0] == '[') {
            current_dir[0] = new_dir[1];
            current_dir[1] = new_dir[2];
            current_dir[2] = DIR_SLASH;
            current_dir[3] = 0;
            goto read_directory;
          }
#endif
          else
            goto done;
          break;
        case SDLK_BACKSPACE:
        case SDLK_ESCAPE:
        case SDLK_F7:
          selection = -1;
          goto done;
          break;
      }
    }
  }

done:
  if (selection >= 0) {
    selection += first_row;
    snprintf(name, FILENAME_MAX, "%s", current_dir);
    if (browse_dir) {
      new_dir = filenamelist[selection];
#if defined(__OS2__) || defined(_WIN32)
      if (new_dir[0] == '[') {
        name[0] = new_dir[1];
        name[1] = new_dir[2];
        name[2] = DIR_SLASH;
        name[3] = 0;
      } else
#endif
      if (new_dir[1] != '.' && new_dir[2] != '.') {
        snprintf(name + strlen(name), FILENAME_MAX - strlen(name),
            "%s", &new_dir[1]);
        name[strlen(name) - 1] = DIR_SLASH;
      }
    }
    else
      snprintf(name + strlen(name), FILENAME_MAX - strlen(name),
          "%s", filenamelist[selection]);
  }

  trs_gui_delete_filename_list();
  return selection;
}

int trs_gui_input_string(const char *title, const char* input, char* output,
                         int limit, int file)
{
  int insert = 1;
  int pos;
  int len;
  int first_disp;

  if (input != output)
    snprintf(output, limit, "%s", input);

  pos = len = strlen(output);

redraw:
  if (pos > 60)
    first_disp = pos - 59;
  else
    first_disp = 0;

  trs_gui_frame(1, 6, 62, 8);
  trs_gui_center_text(title, 6, 0);

  if (file)
    trs_gui_center_text(" TAB:Select Directory ", 8, 1);

  while (1) {
    int key;
    int i;

    for (i = 0; i < 60; i++) {
      int const cur_pos = first_disp + i;

      trs_gui_write_char(i + 2, 7,
          (cur_pos >= len) ? ' ' : output[cur_pos],
          (cur_pos == pos));
    }

    trs_gui_write_text((insert ? " INS " : " OVR "), 56, 8, 1);
    trs_screen_update();

    key = trs_gui_get_key();
    switch (key) {
      case SDLK_LEFT:
        if (pos > 0) {
          if (pos == first_disp)
            first_disp--;
          pos--;
        }
        break;
      case SDLK_RIGHT:
        if (pos < len) {
          if (pos == first_disp + 59)
            first_disp++;
          pos++;
        }
        break;
      case SDLK_HOME:
      case SDLK_PAGEUP:
        first_disp = pos = 0;
        break;
      case SDLK_END:
      case SDLK_PAGEDOWN:
        pos = len;
        if (pos > 60)
          first_disp = pos - 59;
        else
          first_disp = 0;
        break;
      case SDLK_BACKSPACE:
        if (pos > 0) {
          for (i = pos; i < len; i++)
            output[i - 1] = output[i];
          len--;
          if (pos == first_disp)
            first_disp--;
          pos--;
        }
        break;
      case SDLK_DELETE:
        if (pos < len) {
          len--;
          for (i = pos; i < len; i++)
            output[i] = output[i + 1];
        }
        break;
      case SDLK_INSERT:
        insert = !insert;
        break;
      case SDLK_RETURN:
        output[len] = 0;
        return 0;
      case SDLK_ESCAPE:
      case SDLK_F7:
        return -1;
      case SDLK_DOWN:
      case SDLK_TAB:
      case SDLK_UP:
        if (file) {
          char directory[FILENAME_MAX];

          if (trs_gui_file_browse(input, directory, NULL, 1, "") >= 0) {
            snprintf(output, limit, "%s", directory);
            pos = len = strlen(output);
          }
          goto redraw;
        }
        break;
      case SDLK_F9:
        first_disp = len = pos = 0;
        output[0] = 0;
        break;
      case SDLK_F10:
        len = pos;
        output[pos] = 0;
        break;
      default:
        if (key >= 0x20 && key <= 0xFF && pos < limit) {
          if (insert && len < limit) {
            for (i = len; i > pos; i--)
              output[i] = output[i - 1];
            len++;
          }
          output[pos] = (char) key;
          if (pos == first_disp + 59)
            first_disp++;
          pos++;
          if (pos > len)
            len++;
        }
        break;
    }
  }
}

int trs_gui_display_menu(const char *title, MENU_ENTRY *entry, int selection)
{
  int num = 0;

  trs_gui_frame(0, 0, 63, 15);
  trs_gui_write_text(title, 2, 0, 0);

  if (strstr(title, "Man"))
    trs_gui_center_text(" ENTER/INS/TAB:Insert  DEL:Remove  SPACE:Write-Protect ", 15, 1);
  else
    trs_gui_center_text(" ENTER/INS/SPACE/TAB:Select  BACKSPACE/ESC/F7:Return ", 15, 1);

  while (entry[num].type != 0) {
    trs_gui_write_text(entry[num].text, 2, num + 2, 0);
    num++;
  }
  num--;

  while (1) {
    int key = trs_gui_select(entry[selection].text, 2, selection + 2);
    int sel;

    if (key >= '0' && key <= '9') {
      key -= '0';
      if (key <= num && entry[key].type != MENU_TITLE)
        selection = key;
    } else
    if (key >= 'A' && key <= 'z') {
      key = toupper(key);
      sel = selection;

      do {
        if (++selection > num)
          selection = 0;
      } while (selection != sel && (int)*entry[selection].text != key);

      while (entry[selection].type == MENU_TITLE) {
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
          } while (entry[selection].type == MENU_TITLE);
          break;
        case SDLK_UP:
        case SDLK_LEFT:
          do {
            selection--;
            if (selection < 0)
              selection = num;
          } while (entry[selection].type == MENU_TITLE);
          break;
        case SDLK_HOME:
        case SDLK_PAGEUP:
          selection = 0;
          while (entry[selection].type == MENU_TITLE) {
            if (selection < num)
              selection++;
          }
          break;
        case SDLK_END:
        case SDLK_PAGEDOWN:
          selection = num;
          while (entry[selection].type == MENU_TITLE) {
            if (selection > 0)
              selection--;
          }
          break;
        case SDLK_DELETE:
          switch (entry[selection].type) {
            case MENU_DISK:
              trs_disk_remove(selection);
              break;
            case MENU_HARD:
              trs_hard_remove(selection);
              break;
            case MENU_WAFER:
              stringy_remove(selection);
              break;
            case MENU_CASS:
              trs_cassette_remove();
              break;
          }

          if (entry[selection].type != MENU_NORMAL) {
            entry[selection].text[0] = ' ';
            return selection;
          }

          break;
        case SDLK_INSERT:
        case SDLK_RETURN:
        case SDLK_TAB:
          switch (entry[selection].type) {
            case MENU_NORMAL:
              return selection;
            case MENU_DISK:
              if (trs_gui_file_browse(trs_disk_getfilename(selection)[0] ?
                  trs_disk_getfilename(selection) : trs_disk_dir,
                  filename, NULL, 0, "Floppy Disk Image") >= 0)
                trs_disk_insert(selection, filename);
              break;
            case MENU_HARD:
              if (trs_gui_file_browse(trs_hard_getfilename(selection)[0] ?
                  trs_hard_getfilename(selection) : trs_hard_dir,
                  filename, NULL, 0, "Hard Disk Image") >= 0)
                trs_hard_attach(selection, filename);
              break;
            case MENU_WAFER:
              if (trs_gui_file_browse(stringy_get_name(selection)[0] ?
                  stringy_get_name(selection) : trs_cass_dir,
                  filename, NULL, 0, "Wafer Image") >= 0)
                stringy_insert(selection, filename);
              break;
            case MENU_CASS:
              if (trs_gui_file_browse(trs_cassette_getfilename()[0] ?
                  trs_cassette_getfilename() : trs_cass_dir,
                  filename, NULL, 0, "Cassette Image") >= 0)
                trs_cassette_insert(filename);
              break;
            case MENU_SAVE_SET:
              trs_gui_diskset_save();
              break;
            case MENU_LOAD_SET:
              trs_gui_diskset_load();
              break;
          }
          return selection;
        case SDLK_SPACE:
          trs_write_protect(entry[selection].type, selection);
          return selection;
        case SDLK_BACKSPACE:
        case SDLK_ESCAPE:
        case SDLK_F7:
          return -1;
      }
    }

    if (entry[selection].type == MENU_HARD)
      return selection;
  }
}

int trs_gui_display_popup(const char *title, const char **entry,
                          int num, int selection)
{
  int const len = strlen(entry[0]);
  int const saved_selection = selection;
  int const x = (64 - len) / 2;
  int const y = (16 - num) / 2;
  int i;

  trs_gui_frame(x - 1, y - 1, x + len, y + num);
  trs_gui_center_text(title, y - 1, 0);

  for (i = 0; i < num; i++)
    trs_gui_write_text(entry[i], x, y + i, 0);
  num--;

  while (1) {
    int key = trs_gui_select(entry[selection], x, selection + y);

    if (key >= '0' && key <= 'z') {
      key = toupper(key);
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
        case SDLK_F7:
          return saved_selection;
      }
    }
  }
}

int trs_gui_display_popup_matrix(const char* title, const char **entry,
                                 int rows, int cols, int selection)
{
  int const len = strlen(entry[0]) + 1;
  int const num = rows * cols;
  int const width = cols * len - 1;
  int const x = (64 - width) / 2;
  int const y = (16 - rows) / 2;
  int row, col;

  trs_gui_frame(x - 1, y - 1, x + width, y + rows);
  trs_gui_clear_rect(x, y, width, rows);
  trs_gui_center_text(title, y - 1, 0);

  for (row = 0; row < rows; row++)
    for (col = 0; col < cols; col++)
      trs_gui_write_text(entry[row * cols + col], x + col * len, y + row, 0);

  if (selection < 0)
    selection = 0;
  else if (selection >= num)
    selection = num - 1;

  row = selection / cols;
  col = selection % cols;

  while (1) {
    if (col < 0)
      col = cols - 1;
    else if (col > cols - 1)
      col = 0;
    if (row < 0)
      row = rows - 1;
    else if (row > rows - 1)
      row = 0;

    selection = row * cols + col;
    switch (trs_gui_select(entry[selection], x + col * len, y + row)) {
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
      case SDLK_F7:
        return -1;
    }
  }
}

int trs_gui_display_question(const char *text)
{
  return trs_gui_display_popup(text, yes_no, 2, 0);
}

int trs_gui_file_overwrite(void)
{
  struct stat st = { 0 };

  if (stat(filename, &st) == 0 && S_ISREG(st.st_mode))
    return trs_gui_display_question("Overwrite?");

  return 1;
}

void trs_gui_disk_creation(void)
{
  MENU_ENTRY menu[] =
  {{"Image Type                                             ", MENU_NORMAL},
   {"Number of Sides                                        ", MENU_NORMAL},
   {"Density                                                ", MENU_NORMAL},
   {"Physical Size                                          ", MENU_NORMAL},
   {"Ignore Density Flag                                    ", MENU_NORMAL},
   {"Insert Created Floppy Disk Image Into Drive            ", MENU_NORMAL},
   {"Create Disk Image with Above Parameters", MENU_NORMAL},
   {"", 0}};
  const char *disk_type[] = {"   JV1", "   JV3", "   DMK"};
  const char *disk_side[] = {"     1", "     2"};
  const char *disk_dens[] = {"Single", "Double"};
  const char *disk_size[] = {"5 Inch", "8 Inch"};
  static int type = 2;
  static int sides = 2;
  static int density = 2;
  static int size;
  static int ignore_density;
  static int drive;
  int selection = 6;

  while (1) {
    snprintf(&menu[0].text[54], 7, "%s", disk_type[type]);
    snprintf(&menu[1].text[54], 7, "%s", disk_side[sides - 1]);
    snprintf(&menu[2].text[54], 7, "%s", disk_dens[density - 1]);
    snprintf(&menu[3].text[54], 7, "%s", disk_size[size]);
    snprintf(&menu[4].text[50], 11, "%s", yes_no[ignore_density]);
    snprintf(&menu[5].text[55], 6, "%s", drives[drive]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Floppy Disk Creation", menu, selection);
    switch (selection) {
      case 0:
        type = trs_gui_display_popup("Type", disk_type, 3, type);
        break;
      case 1:
        sides = trs_gui_display_popup("Sides", disk_side, 2, sides - 1) + 1;
        break;
      case 2:
        density = trs_gui_display_popup("Dens", disk_dens, 2, density - 1) + 1;
        break;
      case 3:
        size = trs_gui_display_popup("Size", disk_size, 2, size);
        break;
      case 4:
        ignore_density = trs_gui_display_popup("Ignore", yes_no, 2, ignore_density);
        break;
      case 5:
        drive = trs_gui_display_popup("Drive", drives, 9, drive);
        break;
      case 6:
        filename[0] = 0;
        if (trs_gui_input_string("Enter Filename for Disk Image",
            trs_disk_dir, filename, FILENAME_MAX, 1) == 0) {
          if (trs_gui_file_overwrite()) {
            int ret = 0;

            switch (type) {
              case 0:
                ret = trs_create_blank_jv1(filename);
                break;
              case 1:
                ret = trs_create_blank_jv3(filename);
                break;
              default:
                ret = trs_create_blank_dmk(filename, sides, density, size, ignore_density);
                break;
            }

            if (ret)
              trs_gui_display_error(filename);
            else if (drive)
              trs_disk_insert(drive - 1, filename);
            return;
          }
        }
        break;
      case -1:
        return;
    }
  }
}

#ifdef __linux
void trs_gui_disk_steps(void)
{
  MENU_ENTRY menu[] =
  {{"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", 0}};
  const char *steps[] = {"Single", "Double"};
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      snprintf(menu[i].text, 63,
          " %d: Drive Step                                        %s",
          i, steps[trs_disk_getstep(i) == 1 ? 0 : 1]);
    }

    trs_gui_clear_screen();
    if ((selection = trs_gui_display_menu("Floppy Disk Step", menu, selection)) == -1)
      return;
    trs_disk_setstep(selection, trs_gui_display_popup("Step", steps, 2,
        trs_disk_getstep(selection) == 2) == 0 ? 1 : 2);
  }
}
#endif

void trs_gui_disk_options(void)
{
  MENU_ENTRY menu[] =
  {{"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_NORMAL},
   {"", MENU_TITLE},
   {"Floppy Disk Controller                             ", MENU_NORMAL},
   {"Doubler Type                                       ", MENU_NORMAL},
   {"True DAM Emulation                                 ", MENU_NORMAL},
#ifdef __linux
   {"Set Drive Steps", MENU_NORMAL},
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
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Floppy Disk Options", menu, selection);
    if (selection >= 0 && selection < 8) {
      trs_disk_setsize(selection, trs_gui_display_popup("Size", disk_size, 2,
          trs_disk_getsize(selection) == 8) == 0 ? 5 : 8);
    }
    else switch (selection) {
      case 9:
        trs_disk_controller = trs_gui_display_popup("FDC", yes_no, 2, trs_disk_controller);
        break;
      case 10:
        trs_disk_doubler = trs_gui_display_popup("Doubler", doubler, 4, trs_disk_doubler);
        break;
      case 11:
        trs_disk_truedam = trs_gui_display_popup("True DAM", yes_no, 2, trs_disk_truedam);
        break;
#ifdef __linux
      case 12:
        trs_gui_disk_steps();
        break;
#endif
      case -1:
        return;
    }
  }
}

void trs_gui_diskset_load(void)
{
  if (trs_gui_file_browse(trs_disk_set_dir, filename, ".set", 0, "Disk Set") >= 0) {
    if (trs_diskset_load(filename) != 0)
      trs_gui_display_error(filename);
  }
}

void trs_gui_diskset_save(void)
{
  filename[0] = 0;
  if (trs_gui_input_string("Enter Filename for Disk Set",
      trs_disk_set_dir, filename, FILENAME_MAX - 5, 1) == 0) {
    trs_add_extension(filename, ".set");
    if (trs_gui_file_overwrite()) {
      if (trs_diskset_save(filename) != 0)
        trs_gui_display_error(filename);
    }
  }
}

void trs_gui_disk_management(void)
{
  MENU_ENTRY menu[] =
  {{" 0: ", MENU_DISK},
   {" 1: ", MENU_DISK},
   {" 2: ", MENU_DISK},
   {" 3: ", MENU_DISK},
   {" 4: ", MENU_DISK},
   {" 5: ", MENU_DISK},
   {" 6: ", MENU_DISK},
   {" 7: ", MENU_DISK},
   {"", MENU_TITLE},
   {"Save Disk Set", MENU_SAVE_SET},
   {"Load Disk Set", MENU_LOAD_SET},
   {"Create Blank Floppy Disk", MENU_NORMAL},
   {"Disk Drive Options", MENU_NORMAL},
   {"", 0}};
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      trs_gui_limit_string(trs_disk_getfilename(i), &menu[i].text[4], 56);
      menu[i].text[0] = trs_disk_getwriteprotect(i) ? '*' : ' ';
    }

    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Floppy Disk Management", menu, selection);
    switch (selection) {
      case 11:
        trs_gui_disk_creation();
        break;
      case 12:
        trs_gui_disk_options();
        break;
      case -1:
        return;
    }
  }
}

void trs_gui_hard_management(void)
{
  MENU_ENTRY menu[] =
  {{" 0: ", MENU_HARD},
   {" 1: ", MENU_HARD},
   {" 2: ", MENU_HARD},
   {" 3: ", MENU_HARD},
   {"", MENU_TITLE},
   {"Save Disk Set", MENU_SAVE_SET},
   {"Load Disk Set", MENU_LOAD_SET},
   {"", MENU_TITLE},
   {"Cylinder Count                                           ", MENU_NORMAL},
   {"Head Count                                               ", MENU_NORMAL},
   {"Sector Count                                             ", MENU_NORMAL},
   {"Insert Created Hard Disk Image Into Drive                ", MENU_NORMAL},
   {"Create Hard Disk Image with Above Parameters", MENU_NORMAL},
   {"", 0}};
  static int drive;
  int cylinders = trs_hard_getcyls(0) ? trs_hard_getcyls(0) : 202;
  int heads     = trs_hard_getheads(0) ? trs_hard_getheads(0) : 0;
  int sectors   = trs_hard_getsecs(0) ? trs_hard_getsecs(0) : 256;
  int selection = 0;

  while (1) {
    char input[5];
    int  i;
    int  value;

    for (i = 0; i < 4; i++) {
      trs_gui_limit_string(trs_hard_getfilename(i), &menu[i].text[4], 56);
      menu[i].text[0] = trs_hard_getwriteprotect(i) ? '*' : ' ';
    }

    snprintf(&menu[8].text[55], 6, "%5d", cylinders);
    snprintf(&menu[9].text[57], 4, "%3d", heads);
    snprintf(&menu[10].text[57], 4, "%3d", sectors);
    snprintf(&menu[11].text[55], 6, "%s", drives[drive]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Hard Disk Management", menu, selection);
    switch (selection) {
      case 0:
      case 1:
      case 2:
      case 3:
        if (trs_hard_getfilename(selection)[0]) {
          cylinders = trs_hard_getcyls(selection);
          heads     = trs_hard_getheads(selection);
          sectors   = trs_hard_getsecs(selection);
        }
        break;
      case 8:
        snprintf(input, 5, "%d", cylinders);
        if (trs_gui_input_string("Enter Cylinder Count", input, input, 4, 0) == 0) {
          value = atoi(input);
          if (value != cylinders) {
            if (value >= 3 && value <= 8192) {
              cylinders = value;
              if (cylinders > 203)
                debug("Cylinder Count > 203 is incompatible with XTRSHARD/DCT\n");
            } else {
              trs_gui_display_message("ERROR",
                  "Cylinder Count must be between 3 and 8192");
            }
          }
        }
        break;
      case 9:
        snprintf(input, 2, "%d", heads);
        if (trs_gui_input_string("Enter Head Count", input, input, 1, 0) == 0) {
          value = atoi(input);
          if (value != heads) {
            if (value >= 0 && value <= 8)
              heads = value;
            else
              trs_gui_display_message("ERROR",
                  "Head Count must be between 0 and 8");
          }
        }
        break;
      case 10:
        snprintf(input, 4, "%d", sectors);
        if (trs_gui_input_string("Enter Sector Count", input, input, 3, 0) == 0) {
          value = atoi(input);
          if (value != sectors) {
            if (value >= 4 && value <= 256)
              sectors = value;
            else
              trs_gui_display_message("ERROR",
                  "Sector Count must be between 4 and 256");
          }
        }
        break;
      case 11:
        drive = trs_gui_display_popup("Drive", drives, 5, drive);
        break;
      case 12:
        filename[0] = 0;
        if (trs_gui_input_string("Enter Filename for Hard Disk Image",
            trs_hard_dir, filename, FILENAME_MAX, 1) == 0) {
          if (trs_gui_file_overwrite()) {
            if (trs_create_blank_hard(filename, cylinders, heads, sectors) != 0)
              trs_gui_display_error(filename);
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

void trs_gui_stringy_management(void)
{
  MENU_ENTRY menu[] =
  {{" 0: ", MENU_WAFER},
   {" 1: ", MENU_WAFER},
   {" 2: ", MENU_WAFER},
   {" 3: ", MENU_WAFER},
   {" 4: ", MENU_WAFER},
   {" 5: ", MENU_WAFER},
   {" 6: ", MENU_WAFER},
   {" 7: ", MENU_WAFER},
   {"", MENU_TITLE},
   {"Save Disk Set", MENU_SAVE_SET},
   {"Load Disk Set", MENU_LOAD_SET},
   {"Insert Created Wafer Image Into Stringy Drive          ", MENU_NORMAL},
   {"Create Blank Floppy Wafer", MENU_NORMAL},
   {"", 0}};
  static int drive;
  int selection = 0;

  while (1) {
    int i;

    for (i = 0; i < 8; i++) {
      trs_gui_limit_string(stringy_get_name(i), &menu[i].text[4], 56);
      menu[i].text[0] = stringy_get_writeprotect(i) ? '*' : ' ';
    }

    snprintf(&menu[11].text[55], 6, "%5s", drives[drive]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Stringy Wafer Management", menu, selection);
    switch (selection) {
      case 11:
        drive = trs_gui_display_popup("Drive", drives, 9, drive);
        break;
      case 12:
        filename[0] = 0;
        if (trs_gui_input_string("Enter Filename for Wafer Image",
            trs_cass_dir, filename, FILENAME_MAX, 1) == 0) {
          if (trs_gui_file_overwrite()) {
            if (stringy_create(filename) != 0)
              trs_gui_display_error(filename);
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

void trs_gui_cassette_management(void)
{
  MENU_ENTRY menu[] =
  {{" Cass : ", MENU_CASS},
   {"", MENU_TITLE},
   {"Cassette Position                                     ", MENU_NORMAL},
   {"Cassette Default Sample Rate                          ", MENU_NORMAL},
   {"", MENU_TITLE},
   {"Image Type                                            ", MENU_NORMAL},
   {"Insert Created Cassette Into Drive                    ", MENU_NORMAL},
   {"Create Blank Cassette Image with Above Parameters", MENU_NORMAL},
   {"", 0}};
  const char *cass_type[] = {"   CAS", "   CPT", "   WAV"};
  static int type;
  static int insert = 1;
  int selection = 0;

  while (1) {
    char input[12];
    int  value;

    trs_gui_limit_string(trs_cassette_getfilename(), &menu[0].text[8], 52);
    menu[0].text[0] = trs_cass_getwriteprotect() ? '*' : ' ';

    snprintf(&menu[2].text[36], 25, "%10d of %10d", trs_get_cassette_position(),
             trs_get_cassette_length());
    snprintf(&menu[3].text[50], 11, "%10d", cassette_default_sample_rate);
    snprintf(&menu[5].text[54], 7, "%s", cass_type[type]);
    snprintf(&menu[6].text[50], 11, "%s", yes_no[insert]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Cassette Management", menu, selection);
    switch (selection) {
      case 2:
        snprintf(input, 11, "%d", trs_get_cassette_position());
        if (trs_gui_input_string("Enter Cassette Position in Bytes",
            input, input, 10, 0) == 0) {
          value = atoi(input);
          if (value != trs_get_cassette_position() &&
              value >= 0 && value <= trs_get_cassette_length())
            trs_set_cassette_position(value);
        }
        break;
      case 3:
        snprintf(input, 11, "%d", cassette_default_sample_rate);
        if (trs_gui_input_string("Enter Cassette Default Sample Rate",
            input, input, 10, 0) == 0) {
          value = atoi(input);
          if (value != cassette_default_sample_rate &&
              value >= 0 && value <= MAX_SAMPLE_RATE)
            cassette_default_sample_rate = value;
        }
        break;
      case 5:
        type = trs_gui_display_popup("Type", cass_type, 3, type);
        break;
      case 6:
        insert = trs_gui_display_popup("Insert", yes_no, 2, insert);
        break;
      case 7:
        filename[0] = 0;
        if (trs_gui_input_string("Enter Filename for Cassette Image",
            trs_cass_dir, filename, FILENAME_MAX, 1) == 0) {
          switch (type) {
            case 0:
              trs_add_extension(filename, ".cas");
              break;
            case 1:
              trs_add_extension(filename, ".cpt");
              break;
            default:
              trs_add_extension(filename, ".wav");
              break;
          }
          if (trs_gui_file_overwrite()) {
            FILE *cassette_file = fopen(filename, "wb");

            if (cassette_file) {
              if (type == 2) {
                 if (create_wav_header(cassette_file) < 0)
                   trs_gui_display_message("ERROR", "Failed to create WAVE header");
              }
              fclose(cassette_file);
              if (insert)
                trs_cassette_insert(filename);
            } else
              trs_gui_display_error(filename);
          }
        }
        break;
      case -1:
        return;
    }
  }
}

void trs_gui_emulator_settings(void)
{
  MENU_ENTRY menu[] =
  {{"Model                                             ", MENU_NORMAL},
   {"CPU Clock Speed                                   ", MENU_NORMAL},
   {"Speedup Kit Emulation for Model I/III/4/4P        ", MENU_NORMAL},
   {"Exatron Stringy Floppy Emulation for Model I      ", MENU_NORMAL},
   {"LE18 (Lowe Electronics) Graphics for Model I      ", MENU_NORMAL},
   {"Lowercase Modification for Model I                ", MENU_NORMAL},
   {"Lubomir Soft Banker Emulation for Model I         ", MENU_NORMAL},
   {"TRS-80 Users Society Selector I Memory Expansion  ", MENU_NORMAL},
   {"SuperMem (Alpha Technology) I/III Memory Expansion", MENU_NORMAL},
   {"Grafyx Solution (Micro-Labs) III/4/4P Graphics    ", MENU_NORMAL},
   {"MegaMem (Anitek Software) III/4/4P Memory Board   ", MENU_NORMAL},
   {"Dave Huffman (and other) 4/4P Memory Expansion    ", MENU_NORMAL},
   {"HyperMem (Anitek Software) 4/4P Memory Expansion  ", MENU_NORMAL},
   {"", 0}};
  const char *model[] = {"  TRS-80 Model I",
                         "TRS-80 Model III",
                         "  TRS-80 Model 4",
                         " TRS-80 Model 4P"};
  const char *speed[] = {"           None",
                         "       Archbold",
                         "  Holmes II/III",
                         "     Seatronics",
                         "        Banking",
                         "          LNW80",
                         "TCS SpeedMaster",
                         "    CT-80 Aster"};
  int selection = 0;
  int model_selection = trs_model == 1 ? 0 : trs_model - 2;
  float clock_mhz[4];

  clock_mhz[0] = clock_mhz_1;
  clock_mhz[1] = clock_mhz_3;
  clock_mhz[2] = clock_mhz_4;
  clock_mhz[3] = clock_mhz_4;

  while (1) {
    char input[8];

    snprintf(&menu[0].text[44], 17, "%s", model[model_selection]);
    snprintf(&menu[1].text[50], 11, "%6.2f MHz", clock_mhz[model_selection]);
    snprintf(&menu[2].text[45], 16, "%s", speed[speedup]);
    snprintf(&menu[3].text[50], 11, "%s", yes_no[stringy]);
    snprintf(&menu[4].text[50], 11, "%s", yes_no[lowe_le18]);
    snprintf(&menu[5].text[50], 11, "%s", yes_no[lowercase]);
    snprintf(&menu[6].text[50], 11, "%s", yes_no[lubomir]);
    snprintf(&menu[7].text[50], 11, "%s", yes_no[selector]);
    snprintf(&menu[8].text[50], 11, "%s", yes_no[supermem]);
    snprintf(&menu[9].text[50], 11, "%s", yes_no[grafyx_get_microlabs()]);
    snprintf(&menu[10].text[50], 11, "%s", yes_no[megamem]);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[huffman]);
    snprintf(&menu[12].text[50], 11, "%s", yes_no[hypermem]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Emulator Settings", menu, selection);
    switch (selection) {
      case 0:
        model_selection = trs_gui_display_popup("Model", model, 4, model_selection);
        break;
      case 1:
        snprintf(input, 6, "%.2f", clock_mhz[model_selection]);
        if (trs_gui_input_string("Enter CPU Clock Speed in MHz",
            input, input, 6, 0) == 0) {
          float const value = atof(input);

          if (value >= 0.1 && value <= 99.0) {
            clock_mhz[model_selection] = value;
            switch (model_selection) {
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
        speedup = trs_gui_display_popup("Speedup", speed, 8, speedup);
        break;
      case 3:
        stringy = trs_gui_display_popup("Stringy", yes_no, 2, stringy);
        break;
      case 4:
        lowe_le18 = trs_gui_display_popup("LE18", yes_no, 2, lowe_le18);
        break;
      case 5:
        lowercase = trs_gui_display_popup("Lowercase", yes_no, 2, lowercase);
        break;
      case 6:
        lubomir = trs_gui_display_popup("Lubomir", yes_no, 2, lubomir);
        break;
      case 7:
        selector = trs_gui_display_popup("Selector", yes_no, 2, selector);
        if (selector)
          supermem = 0;
        break;
      case 8:
        supermem = trs_gui_display_popup("SuperMem", yes_no, 2, supermem);
        if (supermem)
          selector = 0;
        break;
      case 9:
        grafyx_set_microlabs(trs_gui_display_popup("Grafyx", yes_no, 2,
            grafyx_get_microlabs()));
        break;
      case 10:
        megamem = trs_gui_display_popup("MegaMem", yes_no, 2, megamem);
        break;
      case 11:
        huffman = trs_gui_display_popup("Huffman", yes_no, 2, huffman);
        if (huffman)
          hypermem = 0;
        break;
      case 12:
        hypermem = trs_gui_display_popup("HyperMem", yes_no, 2, hypermem);
        if (hypermem)
          huffman = 0;
        break;
      case -1:
        model_selection = (model_selection == 0 ? 1 : model_selection + 2);
        if (trs_model != model_selection) {
          trs_model = model_selection;
          trs_reset(1);
        }
        return;
    }
  }
}

void trs_gui_display_settings(void)
{
  MENU_ENTRY menu[] =
  {{"Emulator Background Color                              ", MENU_NORMAL},
   {"Emulator Foreground Color                              ", MENU_NORMAL},
   {"GUI Background Color                                   ", MENU_NORMAL},
   {"GUI Foreground Color                                   ", MENU_NORMAL},
   {"Model I Character Set                                  ", MENU_NORMAL},
   {"Model III Character Set                                ", MENU_NORMAL},
   {"Model 4/4P Character Set                               ", MENU_NORMAL},
   {"Border Width                                           ", MENU_NORMAL},
   {"Resize Window on Mode Change for Model III             ", MENU_NORMAL},
   {"Resize Window on Mode Change for Model 4/4P            ", MENU_NORMAL},
   {"Scale Factor for Window                                ", MENU_NORMAL},
   {"LED Display for Disks and Turbo Mode                   ", MENU_NORMAL},
#ifdef OLD_SCANLINES
   {"Display Scanlines with Background Color                ", MENU_NORMAL},
#else
   {"Display Scanlines with brightness                      ", MENU_NORMAL},
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
  const char *scales[] = {" None", "  2 x", "  3 x", "  4 x"};
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
    snprintf(&menu[7].text[52], 9, "%8d", window_border_width);
    snprintf(&menu[8].text[50], 11, "%s", yes_no[resize3]);
    snprintf(&menu[9].text[50], 11, "%s", yes_no[resize4]);
    snprintf(&menu[10].text[55], 6, "%s", scales[scale - 1]);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[trs_show_led]);
#ifdef OLD_SCANLINES
    snprintf(&menu[12].text[50], 11, "%s", yes_no[scanlines]);
#else
    snprintf(&menu[12].text[34], 27, "%-3d%23s", scanshade, yes_no[scanlines]);
#endif
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Display Settings", menu, selection);
    switch (selection) {
      case 0:
        snprintf(input, 7, "%06X", background);
        if (trs_gui_input_string("Enter Background RGB color (Hex, RRGGBB)",
            input, input, 6, 0) == 0) {
          value = strtol(input, NULL, 16);
          if (value != background) {
            background = value;
            resize = 0;
          }
        }
        break;
      case 1:
        snprintf(input, 7, "%06X", foreground);
        if (trs_gui_input_string("Enter Foreground RGB color (Hex, RRGGBB)",
            input, input, 6, 0) == 0) {
          value = strtol(input, NULL, 16);
          if (value != foreground) {
            foreground = value;
            resize = 0;
          }
        }
        break;
      case 2:
        snprintf(input, 7, "%06X", gui_background);
        if (trs_gui_input_string("Enter GUI Background RGB color (Hex, RRGGBB)",
            input, input, 6, 0) == 0) {
          value = strtol(input, NULL, 16);
          if (value != gui_background) {
            gui_background = value;
            resize = 0;
          }
        }
        break;
      case 3:
        snprintf(input, 7, "%06X", gui_foreground);
        if (trs_gui_input_string("Enter GUI Foreground RGB color (Hex, RRGGBB)",
            input, input, 6, 0) == 0) {
          value = strtol(input, NULL, 16);
          if (value != gui_foreground) {
            gui_foreground = value;
            resize = 0;
          }
        }
        break;
      case 4:
        value = trs_gui_display_popup("Charset I", font1, 9, gui_charset1);
        if (value != gui_charset1) {
          gui_charset1 = value;
          trs_charset1 = value >= 4 ? value += 6 : value;
          resize = 1;
        }
        break;
      case 5:
        value = trs_gui_display_popup("Charset III", font34, 3, trs_charset3 - 4) + 4;
        if (value != trs_charset3) {
          trs_charset3 = value;
          resize = 0;
        }
        break;
      case 6:
        value = trs_gui_display_popup("Charset 4/4P", font34, 3, trs_charset4 - 7) + 7;
        if (value != trs_charset4) {
          trs_charset4 = value;
          resize = 0;
        }
        break;
      case 7:
        snprintf(input, 3, "%d", window_border_width);
        if (trs_gui_input_string("Enter Window border width in pixels (0 to 50)",
            input, input, 2, 0) == 0) {
          value = atol(input);
          if (value != window_border_width) {
            if (value < 0 || value > 50)
              value = 2;
            window_border_width = value;
            resize = 1;
          }
        }
        break;
      case 8:
        value = trs_gui_display_popup("Resize III", yes_no, 2, resize3);
        if (value != resize3) {
          resize3 = value;
          resize = 1;
        }
        break;
      case 9:
        value = trs_gui_display_popup("Resize 4", yes_no, 2, resize4);
        if (value != resize4) {
          resize4 = value;
          resize = 1;
        }
        break;
      case 10:
        value = trs_gui_display_popup("Scale", scales, 4, scale - 1) + 1;
        if (value != scale) {
          scale = value;
          fullscreen = 0;
          resize = 1;
        }
        break;
      case 11:
        value = trs_gui_display_popup("LEDs", yes_no, 2, trs_show_led);
        if (value != trs_show_led) {
          trs_show_led = value;
          resize = 1;
        }
        break;
      case 12:
        value = trs_gui_display_popup("Scanlines", yes_no, 2, scanlines);
        if (value != scanlines) {
          scanlines = value;
          resize = 0;
        }
#ifndef OLD_SCANLINES
        if (scanlines) {
          snprintf(input, 4, "%d", scanshade);
          if (trs_gui_input_string("Enter brightness (0 = dark - 255 = light)",
              input, input, 3, 0) == 0) {
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

void trs_gui_misc_settings(void)
{
  MENU_ENTRY menu[] =
  {{"Close and Reopen Printer Output File", MENU_NORMAL},
   {"", MENU_TITLE},
   {"Emulator Traps Safe                                     ", MENU_NORMAL},
   {"Keystretch Value                                        ", MENU_NORMAL},
   {"Printer Type                                            ", MENU_NORMAL},
   {"Serial Port Name:", MENU_TITLE},
   {"                                                        ", MENU_NORMAL},
   {"Serial Switches                                         ", MENU_NORMAL},
   {"Shift Bracket Emulation                                 ", MENU_NORMAL},
   {"Sound Output                                            ", MENU_NORMAL},
   {"Turbo Mode                                              ", MENU_NORMAL},
   {"Turbo Speed                                             ", MENU_NORMAL},
   {"Turbo Paste                                             ", MENU_NORMAL},
   {"", 0}};
  const char *printer[] = {"     None", "     Text"};
  int selection = 0;

  while (1) {
    char input[12];

    snprintf(&menu[2].text[50], 11, "%s", yes_no[trs_emtsafe]);
    snprintf(&menu[3].text[50], 11, "%10d", stretch_amount);
    snprintf(&menu[4].text[51], 10, "%s", printer[trs_printer]);
    trs_gui_limit_string(trs_uart_name, &menu[6].text[2], 58);
    snprintf(&menu[7].text[56], 5, "0x%02X", trs_uart_switches);
    snprintf(&menu[8].text[50], 11, "%s", yes_no[trs_kb_bracket_state]);
    snprintf(&menu[9].text[50], 11, "%s", yes_no[trs_sound]);
    snprintf(&menu[10].text[50], 11, "%s", yes_no[timer_overclock]);
    snprintf(&menu[11].text[50], 11, "%10d", timer_overclock_rate);
    snprintf(&menu[12].text[50], 11, "%s", yes_no[turbo_paste]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Miscellaneous/Printer", menu, selection);
    switch (selection) {
      case 0:
        if (trs_printer_reset() == 0)
          trs_gui_display_message("Status", "Printer file closed");
        else
          trs_gui_display_message("Warning", "No Printer Output in File");
        break;
      case 2:
        trs_emtsafe = trs_gui_display_popup("Emtsafe", yes_no, 2, trs_emtsafe);
        break;
      case 3:
        snprintf(input, 11, "%d", stretch_amount);
        if (trs_gui_input_string("Enter Keystretch in Cycles",
            input, input, 10, 0) == 0) {
          stretch_amount = atoi(input);
          if (stretch_amount < 0)
            stretch_amount = STRETCH_AMOUNT;
        }
        break;
      case 4:
        trs_printer = trs_gui_display_popup("Printer", printer, 2, trs_printer);
        break;
      case 6:
        filename[0] = 0;
        if (trs_gui_input_string("Enter Serial Port Name", trs_uart_name,
            filename, FILENAME_MAX, 0) == 0) {
          snprintf(trs_uart_name, FILENAME_MAX, "%s", filename);
          trs_uart_init(0);
        }
        break;
      case 7:
        snprintf(input, 3, "%2X", trs_uart_switches);
        if (trs_gui_input_string("Enter Serial Switches (Hex, XX)",
            input, input, 2, 0) == 0) {
          trs_uart_switches = strtol(input, NULL, 16);
          trs_uart_init(0);
        }
        break;
      case 8:
        trs_kb_bracket_state = trs_gui_display_popup("Bracket",
            yes_no, 2, trs_kb_bracket_state);
        trs_kb_bracket(trs_kb_bracket_state);
        break;
      case 9:
        trs_sound = trs_gui_display_popup("Sound", yes_no, 2, trs_sound);
        trs_screen_caption();
        break;
      case 10:
        timer_overclock = trs_gui_display_popup("Turbo", yes_no, 2, timer_overclock);
        trs_timer_mode(timer_overclock);
        break;
      case 11:
        snprintf(input, 11, "%d", timer_overclock_rate);
        if (trs_gui_input_string("Enter Turbo Rate Multiplier",
            input, input, 10, 0) == 0) {
          timer_overclock_rate = atoi(input);
          if (timer_overclock_rate <= 0)
            timer_overclock_rate = 1;
          trs_timer_mode(timer_overclock);
        }
        break;
      case 12:
        turbo_paste = trs_gui_display_popup("Paste", yes_no, 2, turbo_paste);
        break;
      case -1:
        return;
    }
  }
}

void trs_gui_save_state(void)
{
  filename[0] = 0;
  if (trs_gui_input_string("Save Emulator State",
      trs_state_file[0] != 0 ? trs_state_file : trs_state_dir, filename,
      FILENAME_MAX - 5, 1) == 0) {
    trs_add_extension(filename, ".t8s");
    if (trs_gui_file_overwrite()) {
      if (trs_state_save(filename) == 0)
        snprintf(trs_state_file, FILENAME_MAX, "%s", filename);
      else
        trs_gui_display_error(filename);
    }
  }
}

int trs_gui_load_state(void)
{
  if (trs_gui_file_browse(trs_state_dir, filename, ".t8s", 0,
      "State (.t8s)") >= 0) {
    if (trs_state_load(filename) == 0) {
      trs_screen_init(1);
      return 0;
    } else
      trs_gui_display_error(filename);
  }
  return -1;
}

void trs_gui_write_config(void)
{
  filename[0] = 0;
  if (trs_gui_input_string("Write Configuration",
      trs_config_file[0] != 0 ? trs_config_file : trs_state_dir, filename,
      FILENAME_MAX - 5, 1) == 0) {
    trs_add_extension(filename, ".t8c");
    if (trs_gui_file_overwrite()) {
      if (trs_write_config_file(filename) == 0)
        snprintf(trs_config_file, FILENAME_MAX, "%s", filename);
      else
        trs_gui_display_error(filename);
    }
  }
}

int trs_gui_read_config(void)
{
  if (trs_gui_file_browse(trs_config_file, trs_config_file, ".t8c", 0,
      "Configuration (.t8c)") >= 0) {
    if (trs_load_config_file() == 0) {
      trs_reset(1);
      return 0;
    }
    trs_gui_display_error(trs_config_file);
  }
  return -1;
}

static int trs_gui_config_management(void)
{
  MENU_ENTRY menu[] =
  {{"Save Emulator State (Alt-S)", MENU_NORMAL},
   {"Load Emulator State (Alt-L)", MENU_NORMAL},
   {"Write Configuration (Alt-W)", MENU_NORMAL},
   {"Read Configuration  (Alt-R)", MENU_NORMAL},
   {"", 0}};
  int selection = 0;

  while (1) {
    trs_gui_clear_screen();
    trs_gui_write_text("State File:", 2, 8, 0);
    trs_gui_write_text(trs_state_file, 4, 9, 0);
    trs_gui_write_text("Configuration File:", 2, 11, 0);
    trs_gui_write_text(trs_config_file, 4, 12, 0);

    selection = trs_gui_display_menu("Configuration/State Files", menu, selection);
    switch (selection) {
      case 0:
        trs_gui_save_state();
        break;
      case 1:
        if (trs_gui_load_state() == 0)
          return 0;
        break;
      case 2:
        trs_gui_write_config();
        break;
      case 3:
        if (trs_gui_read_config() == 0)
          return 0;
        break;
      case -1:
        return -1;
    }
  }
}

const char *trs_gui_get_key_name(int key)
{
  int i;

  for (i = 0; i < N_KEYS; i++) {
    if (key == key_syms[i])
      return key_names[i];

    if (key == key_syms_shifted[i])
      return key_names_shifted[i];
  }
  return "???";
}

int trs_gui_virtual_keyboard(void)
{
  static int saved_selection;
  int key_index = SHIFT, shifted = 0;

  while (key_index == SHIFT || (shifted && key_syms_shifted[key_index] == -1)) {
    if ((key_index = trs_gui_display_popup_matrix("Select Key",
        !shifted ? key_names : key_names_shifted, 4, 13, saved_selection)) == -1)
      return -1;

    if (key_index == SHIFT)
      shifted = !shifted;

    saved_selection = key_index;
  }
  return !shifted ? key_syms[key_index] : key_syms_shifted[key_index];
}

void trs_gui_get_virtual_key(void)
{
  int const key = trs_gui_virtual_keyboard();

  if (key != -1)
    trs_xlate_keysym(key);
}

void trs_gui_joy_gui(void)
{
  int const selection = trs_gui_display_popup_matrix("Joystick GUI", function_menu, 3, 2, 0);

  if (selection == -1)
    return;

  switch (function_codes[selection]) {
    case GUI:
      trs_gui();
      break;
    case KEYBRD:
      trs_gui_get_virtual_key();
      break;
    case SAVE:
      trs_gui_save_state();
      break;
    case LOAD:
      trs_gui_load_state();
      break;
    case RESET:
      trs_reset(1);
      break;
    case EXIT:
      trs_exit(1);
      break;
  }
}

int trs_gui_joystick_get_button(void)
{
  SDL_Event event;

  trs_gui_frame(25, 7, 38, 9);
  trs_gui_write_text("Press Button", 26, 8, 0);
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
          case SDLK_F7:
            return -1;
          default:
            break;
        }
        break;
      case SDL_JOYBUTTONDOWN:
      case SDL_MOUSEBUTTONDOWN:
        if (event.type == SDL_MOUSEBUTTONDOWN)
          event.jbutton.button = event.button.button;
        if (event.jbutton.button < N_JOYBUTTONS)
          return event.jbutton.button;
        else {
          trs_gui_display_message("ERROR", "Unsupported Button");
          return -1;
        }
    }
  }
}

void trs_gui_joystick_display_map(int button)
{
  int row, col;
  char text[12];

  for (col = 0; col < 5; col++) {
    for (row = 0; row < 4; row++) {
      int const pos = col * 4 + row;
      int const map = jbutton_map[pos];

      snprintf(text, 12, "%2d:%s", pos,
          map == -1     ? "---     " :
          map == GUI    ? "<GUI>   " :
          map == KEYBRD ? "<KEYBRD>" :
          map == SAVE   ? "<SAVE>  " :
          map == LOAD   ? "<LOAD>  " :
          map == RESET  ? "<RESET> " :
          map == EXIT   ? "<EXIT>  " :
          map == PAUSE  ? "<PAUSE> " :
          map == JOYGUI ? "<JOYGUI>" : trs_gui_get_key_name(map));
      trs_gui_write_text(text, 2 + col * 12, 11 + row, button == pos);
    }
  }
}

void trs_gui_joystick_settings(void)
{
  MENU_ENTRY menu[] =
  {{"Use Keypad for Joystick                           ", MENU_NORMAL},
   {"USB Joystick/Gamepad                              ", MENU_NORMAL},
   {"Map Joystick/Mouse to Arrow Keys                  ", MENU_NORMAL},
   {"Map Button to Key", MENU_NORMAL},
   {"Map Button to Function", MENU_NORMAL},
   {"Unmap Button", MENU_NORMAL},
   {"Unmap All Buttons", MENU_NORMAL},
   {"Check Button Mapping", MENU_NORMAL},
   {"", 0}};
  char *joystick[MAX_JOYSTICKS + 1];
  char joysticks[MAX_JOYSTICKS + 1][64];
  int selection = 0;
  int button, key;
  int i, num_joysticks, joy_index;

  for (i = 0; i < MAX_JOYSTICKS + 1; i++)
    joystick[i] = joysticks[i];

  while (1) {
    snprintf(&menu[0].text[50], 11, "%s", yes_no[trs_keypad_joystick]);

    if (trs_joystick_num == -1)
      snprintf(&menu[1].text[50], 11, "      None");
    else
      snprintf(&menu[1].text[50], 11, "Joystick %1d", trs_joystick_num);

    snprintf(&menu[2].text[50], 11, "%s", yes_no[jaxis_mapped]);
    trs_gui_clear_screen();
    trs_gui_joystick_display_map(-1);

    selection = trs_gui_display_menu("Joystick Settings", menu, selection);
    switch (selection) {
      case 0:
        trs_keypad_joystick = trs_gui_display_popup("Keypad",
            yes_no, 2, trs_keypad_joystick);
        trs_set_keypad_joystick();
        break;
      case 1:
        num_joysticks = SDL_NumJoysticks();
        if (num_joysticks > MAX_JOYSTICKS)
          num_joysticks = MAX_JOYSTICKS;

        snprintf(joystick[0], 61, "%60s", "None");

        for (i = 0; i < num_joysticks; i++) {
          snprintf(joystick[i + 1], 61, "Joystick %1d - %47s", i,
              SDL_JoystickName(SDL_JoystickOpen(i)));
        }

        if ((trs_joystick_num == -1) || (trs_joystick_num >= num_joysticks))
          joy_index = 0;
        else
          joy_index = trs_joystick_num + 1;

        joy_index = trs_gui_display_popup("Joystick", (const char**)joystick,
            num_joysticks + 1, joy_index);
        trs_joystick_num = joy_index - 1;
        trs_open_joystick();
        break;
      case 2:
        jaxis_mapped = trs_gui_display_popup("Arrow", yes_no, 2, jaxis_mapped);
        break;
      case 3:
        if ((key = trs_gui_virtual_keyboard()) != -1) {
          if ((button = trs_gui_joystick_get_button()) != -1)
            jbutton_map[button] = key;
        }
        break;
      case 4:
        if ((key = trs_gui_display_popup_matrix("Select Function",
            function_menu, 4, 2, 0)) != -1) {
          if ((button = trs_gui_joystick_get_button()) != -1)
            jbutton_map[button] = function_codes[key];
        }
        break;
      case 5:
        if ((button = trs_gui_joystick_get_button()) != -1)
          jbutton_map[button] = -1;
        break;
      case 6:
        if (trs_gui_display_question("Sure?") == 1) {
          for (i = 0; i < N_JOYBUTTONS; i++)
            jbutton_map[i] = -1;
         }
        break;
      case 7:
        while ((button = trs_gui_joystick_get_button()) != -1) {
          trs_gui_joystick_display_map(button);
          trs_screen_update();
        }
        break;
      case -1:
        return;
    }
  }
}

void trs_gui_default_dirs(void)
{
  MENU_ENTRY menu[] =
  {{"Floppy Disk:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"Hard Disk:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"Cassette/Wafer:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"Disk Set:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"State/Configuration:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"Printer Output/Screenshot:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"", 0}};
  int selection = 1;

  while (1) {
    trs_gui_limit_string(trs_disk_dir, &menu[1].text[2], 58);
    trs_gui_limit_string(trs_hard_dir, &menu[3].text[2], 58);
    trs_gui_limit_string(trs_cass_dir, &menu[5].text[2], 58);
    trs_gui_limit_string(trs_disk_set_dir, &menu[7].text[2], 58);
    trs_gui_limit_string(trs_state_dir, &menu[9].text[2], 58);
    trs_gui_limit_string(trs_printer_dir, &menu[11].text[2], 58);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Default Directories", menu, selection);
    switch (selection) {
      case 1:
        trs_gui_file_browse(trs_disk_dir, trs_disk_dir, NULL, 1, "Floppy Disk ");
        break;
      case 3:
        trs_gui_file_browse(trs_hard_dir, trs_hard_dir, NULL, 1, "Hard Disk ");
        break;
      case 5:
        trs_gui_file_browse(trs_cass_dir, trs_cass_dir, NULL, 1, "Cassette/Wafer ");
        break;
      case 7:
        trs_gui_file_browse(trs_disk_set_dir, trs_disk_set_dir, NULL, 1, "Disk Set ");
        break;
      case 9:
        trs_gui_file_browse(trs_state_dir, trs_state_dir, NULL, 1, "State/Configuration ");
        break;
      case 11:
        trs_gui_file_browse(trs_printer_dir, trs_printer_dir, NULL, 1, "Printer/Screenshot ");
        break;
      case -1:
        return;
    }
  }
}

void trs_gui_rom_files(void)
{
  MENU_ENTRY menu[] =
  {{"Model I ROM:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"", MENU_TITLE},
   {"Model III / 4 ROM:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"", MENU_TITLE},
   {"Model 4P ROM:", MENU_TITLE},
   {"   ", MENU_NORMAL},
   {"", MENU_TITLE},
   {"", MENU_TITLE},
   {"", MENU_TITLE},
   {"Patch Model I ROM for auto-boot from hard drive   ", MENU_NORMAL},
   {"", 0}};
  int selection = 1;

  while (1) {
    trs_gui_limit_string(romfile, &menu[1].text[2], 58);
    trs_gui_limit_string(romfile3, &menu[4].text[2], 58);
    trs_gui_limit_string(romfile4p, &menu[7].text[2], 58);
    snprintf(&menu[11].text[50], 11, "%s", yes_no[trs_hd_boot]);
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("ROM File Selection", menu, selection);
    switch (selection) {
      case 1:
        trs_gui_file_browse(romfile, romfile, NULL, 0, "Model I ROM");
        break;
      case 4:
        trs_gui_file_browse(romfile3, romfile3, NULL, 0, "Model III / 4 ROM");
        break;
      case 7:
        trs_gui_file_browse(romfile4p, romfile4p, NULL, 0, "Model 4P ROM");
        break;
      case 11:
        trs_hd_boot = trs_gui_display_popup("Patch", yes_no, 2, trs_hd_boot);
        break;
      case -1:
        return;
    }
  }
}

void trs_gui_about_sdltrs(void)
{
  trs_gui_clear_screen();
  trs_gui_frame(0, 0, 63, 15);
  trs_gui_write_text("About", 2, 0, 0);
  trs_gui_center_text("SDLTRS", 3, 0);
  trs_gui_center_text("Version 1.2.26", 4, 0);
  trs_gui_center_text("BSD 2-Clause License", 5, 0);
  trs_gui_center_text("Copyright (C) 2006-2011 Mark Grebe, 2018-2023", 6, 0);
  trs_gui_center_text("Alan Cox, Jens Guenther, Leonardo Brondani Schenkel", 7, 0);
  trs_gui_center_text("<https://gitlab.com/jengun/sdltrs>", 8, 0);
  trs_gui_center_text("Based on xtrs 4.9d by Tim Mann", 10, 0);
  trs_gui_center_text("<http://www.tim-mann.org/xtrs>", 11, 0);
  trs_gui_center_text("xtrs 1.0 Copyright (C) 1992 Clarendon Hill Software", 12, 0);
  trs_gui_center_text(" Press Any Key To Return ", 15, 1);
  trs_screen_update();
  trs_gui_get_key();
}

void trs_gui_keys_sdltrs(void)
{
  trs_gui_clear_screen();
  trs_gui_frame(0, 0, 63, 15);
  trs_gui_write_text("Keys", 2, 0, 0);
  trs_gui_write_text("F1-F3: Functions Keys F1/F2/F3  PgUp/PgDn: Left/Right Shift ", 2, 1, 0);
  trs_gui_write_text("F4: F4/CapsLock on TRS-80 4/4P  Insert: TRS-80 Underscore   ", 2, 2, 0);
  trs_gui_write_text("F5/ScrollLock: TRS-80 '@' Key   Shift-Up Arrow: TRS-80 ESC  ", 2, 3, 0);
  trs_gui_write_text("F6: TRS-80 Shifted '0' Key      Alt-PgUp/PgDn: Scale Window ", 2, 4, 0);
  trs_gui_write_text("F7/Alt-M: Main Menu of SDLTRS   Alt-Enter: Toggle Fullscreen", 2, 5, 0);
  trs_gui_write_text("F8/Shift-F8: Quit/Abort SDLTRS  Alt-A/C/V: Select/Copy/Paste", 2, 6, 0);
  trs_gui_write_text("F9/Alt-Z:"
#ifdef ZBX
  " Enter debugger (zbx)  "
#else
  " Toggle Fullscreen     "
#endif
  "Alt-D/F: Floppy Disk Menu   ", 2, 7, 0);
  trs_gui_write_text("F10/Shift-F10: Warm/Cold Reset  Alt-H: Hard Disk Menu       ", 2, 8, 0);
  trs_gui_write_text("F11/Alt-K: Show this key help   Alt-T: Cassette/Tape Menu   ", 2, 9, 0);
  trs_gui_write_text("F12/Alt-N: Switch Turbo On/Off  Alt-L/S: Load / Save State  ", 2, 10, 0);
  trs_gui_write_text("ESC: TRS-80 Break Key           Alt-R/W: Read / Write Config", 2, 11, 0);
  trs_gui_write_text("Home/Clear: TRS-80 Clear Key    Alt-P/Pause: Pause Emulator ", 2, 12, 0);
  trs_gui_write_text("End: TRS-80 Shifted Down Arrow  Alt-0...7: Insert Disk Drive", 2, 13, 0);
  trs_gui_write_text("Control: TRS-80 Control Key     Shift-Alt-0...7: Remove Disk", 2, 14, 0);
  trs_gui_center_text(" Press Any Key To Return ", 15, 1);
  trs_screen_update();
  trs_gui_get_key();
}

void trs_gui_display_pause(void)
{
  trs_gui_frame(1, 6, 62, 8);
  trs_gui_clear_rect(2, 7, 60, 1);
  trs_gui_center_text("Emulation Paused", 7, 0);
  trs_screen_update();
}

void trs_gui_exec_cmd(void)
{
  if (trs_gui_file_browse(trs_cmd_file, trs_cmd_file, ".cmd", 0, "CMD (.cmd)") >= 0) {
    if (trs_load_cmd(trs_cmd_file) != 0)
      trs_gui_display_message("ERROR", "Failed to load CMD file");
  }
}

int trs_gui_exit_sdltrs(void)
{
  return trs_gui_display_question("Quit?");
}

void trs_gui_save_bmp(void)
{
  filename[0] = 0;
  if (trs_gui_input_string("Save Screenshot",
      trs_printer_dir, filename, FILENAME_MAX - 5, 1) == 0) {
    trs_add_extension(filename, ".bmp");
    if (trs_gui_file_overwrite()) {
      if (trs_sdl_savebmp(filename) != 0)
        trs_gui_display_error(filename);
    }
  }
}

void trs_gui(void)
{
  MENU_ENTRY menu[] =
  {{"Floppy Disk Management   (Alt-D)", MENU_NORMAL},
   {"Hard Disk Management     (Alt-H)", MENU_NORMAL},
   {"Cassette Management      (Alt-T)", MENU_NORMAL},
   {"Stringy Wafer Management (Alt-G)", MENU_NORMAL},
   {"Emulator Settings        (Alt-E)", MENU_NORMAL},
   {"Display Settings         (Alt-I)", MENU_NORMAL},
   {"Miscellaneous/Printer    (Alt-O)", MENU_NORMAL},
   {"Configuration/State Files", MENU_NORMAL},
   {"Joystick Settings", MENU_NORMAL},
   {"Default Directories", MENU_NORMAL},
   {"ROM File Selection", MENU_NORMAL},
   {"TRS-80 Power Reset", MENU_NORMAL},
   {"About SDLTRS", MENU_NORMAL},
   {"", 0}};
  int selection = 0;

  while (1) {
    trs_gui_clear_screen();

    selection = trs_gui_display_menu("Main Menu", menu, selection);
    switch (selection) {
      case 0:
        trs_gui_disk_management();
        break;
      case 1:
        trs_gui_hard_management();
        break;
      case 2:
        trs_gui_cassette_management();
        break;
      case 3:
        trs_gui_stringy_management();
        break;
      case 4:
        trs_gui_emulator_settings();
        break;
      case 5:
        trs_gui_display_settings();
        break;
      case 6:
        trs_gui_misc_settings();
        break;
      case 7:
        if (trs_gui_config_management() == 0)
          return;
        break;
      case 8:
        trs_gui_joystick_settings();
        break;
      case 9:
        trs_gui_default_dirs();
        break;
      case 10:
        trs_gui_rom_files();
        break;
      case 11:
        if (trs_gui_display_question("Reset?")) {
          trs_reset(1);
          return;
        }
        break;
      case 12:
        trs_gui_about_sdltrs();
        break;
      case -1:
        return;
    }
  }
}

void call_function(int function)
{
  SDL_PauseAudio(1);

  switch (function) {
    case PAUSE:
      trs_paused = !trs_paused;
      break;
    case RESET:
      trs_reset(1);
      return;
    case EXIT:
      trs_exit(1);
      return;
    case GUI:
      trs_gui();
      break;
    case JOYGUI:
      trs_gui_joy_gui();
      break;
    case KEYBRD:
      trs_gui_get_virtual_key();
      break;
    case SAVE:
      trs_gui_save_state();
      break;
    case LOAD:
      trs_gui_load_state();
      break;
    case DISK:
      trs_gui_disk_management();
      break;
    case HARD:
      trs_gui_hard_management();
      break;
    case STRINGY:
      trs_gui_stringy_management();
      break;
    case TAPE:
      trs_gui_cassette_management();
      break;
    case WRITE:
      trs_gui_write_config();
      break;
    case READ:
      trs_gui_read_config();
      break;
    case EMULATOR:
      trs_gui_emulator_settings();
      break;
    case INTERFACE:
      trs_gui_display_settings();
      break;
    case OTHER:
      trs_gui_misc_settings();
      break;
    case KEYS:
      trs_gui_keys_sdltrs();
      break;
    case EXEC:
      trs_gui_exec_cmd();
      break;
    case SAVE_BMP:
      trs_gui_save_bmp();
      break;
  }

  trs_screen_refresh();
  SDL_PauseAudio(0);
}
