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

/*#define MOUSEDEBUG 1*/
/*#define SDLDEBUG 1*/

/*
 * trs_sdl_interface.c
 *
 * SDL2 interface for TRS-80 Emulator
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <SDL.h>
#include "error.h"
#include "trs.h"
#include "trs_clones.h"
#include "trs_disk.h"
#include "trs_memory.h"
#include "trs_sdl_gui.h"
#include "trs_sdl_keyboard.h"
#include "trs_state_save.h"
#include "trs_uart.h"

#define MAX_RECTS   1
#define MAX_SCALE   4
#define SCREEN_SIZE 2048

/* currentmode values */
#ifdef _WIN32
#undef  ALTERNATE
#endif
#define NORMAL    0
#define EXPANDED  1
#define INVERSE   2
#define ALTERNATE 4

/* TRS-80 character sizes */
#define MAX_CHARS        256
#define MAX_CHAR_HEIGHT  16
#define TRS_CHAR_WIDTH   8
#define TRS_CHAR_HEIGHT  12
#define TRS_CHAR_HEIGHT4 10

/* Public data */
int  aspect_ratio  = 1;
char scale_quality = '1';

/* Private data */
#include "trs_chars.c"

static Uint8 trs_screen[SCREEN_SIZE];
static Uint8 char_ram[MAX_CHARS][MAX_CHAR_HEIGHT];
static Uint8 *blitMap;
static Uint16 screen_chars = 1024;
static int trs_charset;
static int cpu_panel;
static int row_chars = 64;
static int col_chars = 16;
static int border_width = 2;
static int scale_factor = 2;
static int m6845_raster = 12;
static int trs_resize;
static int text80x24, screen640x240;
static int drawnRectCount;
static int top_margin;
static int left_margin;
static int screen_height;
static int currentmode;
static int OrigHeight, OrigWidth;
static int cur_char_width = TRS_CHAR_WIDTH;
static int cur_char_height = TRS_CHAR_HEIGHT * 2;
static int mouse_x_size = 640, mouse_y_size = 240;
static int mouse_sens = 3;
static int mouse_last_x = -1, mouse_last_y = -1;
static int mouse_old_style;
static unsigned int mouse_last_buttons;
static SDL_Surface *trs_char[6][MAX_CHARS];
static SDL_Surface *trs_box[2][64];
static SDL_Surface *image;
static SDL_Surface *screen;
static SDL_Window *window;
static SDL_Renderer *render;
static SDL_Texture *texture;
static float render_scale_x;
static float render_scale_y;
static int window_x = SDL_WINDOWPOS_UNDEFINED;
static int window_y = SDL_WINDOWPOS_UNDEFINED;
static int window_w;
static int window_h;
static Uint32 light_red;
static Uint32 bright_red;
static Uint32 light_orange;
static Uint32 bright_orange;
static Uint32 back_color;
static Uint32 last_key[256];

#define PASTE_IDLE    0
#define PASTE_GETNEXT 1
#define PASTE_KEYDOWN 2
#define PASTE_KEYUP   3
static int  paste_state;
static int  paste_lastkey;
extern int  PasteManagerStartPaste(void);
extern void PasteManagerStartCopy(const char *string);
extern int  PasteManagerGetChar(Uint8 *character);

#define COPY_IDLE     0
#define COPY_STARTED  1
#define COPY_DEFINED  2
#define COPY_CLEAR    3
static int copyStatus;
static int selection_x1;
static int selection_y1;
static int selection_x2;
static int selection_y2;
static int selectAll;
static int timer_saved;
static unsigned int cycles_saved;

/* Support for Micro-Labs Grafyx Solution and Radio Shack hi-res card
 * ... also used for other graphic cards ... */

/* True size of graphics memory -- some is offscreen */
#define G_XSIZE 128
#define G_YSIZE 512
#define G_MSIZE (2 * G_YSIZE) * G_XSIZE
static Uint8 grafyx[G_MSIZE];
static Uint8 grafyx_unscaled[G_YSIZE][G_XSIZE];
static int grafyx_x, grafyx_y, grafyx_mode;
static int grafyx_enable;
static int grafyx_overlay;
static int grafyx_xoffset, grafyx_yoffset;

/* Port 0x83 (grafyx_mode) bits */
#define G_ENABLE    1
#define G_UL_NOTEXT 2   /* Micro-Labs only */
#define G_RS_WAIT   2   /* Radio Shack only */
#define G_XDEC      4
#define G_YDEC      8
#define G_XNOCLKR   16
#define G_YNOCLKR   32
#define G_XNOCLKW   64
#define G_YNOCLKW   128

/* Port 0xFF (grafyx_m3_mode) bits */
#define G3_COORD    0x80
#define G3_ENABLE   0x40
#define G3_COMMAND  0x20
#define G3_YLOW(v)  (((v) & 0x1e) >> 1)

/* HRG1B, LE18, LWN80 and TCS Genie IIs/SpeedMaster */
#define HRG_MEMSIZE (1024 * 16)        /* 16k * 8 bit graphics memory */
static Uint8 hrg_screen[HRG_MEMSIZE];
static int hrg_enable;
static int hrg_addr;
static Uint8 le18_x, le18_y, le18_on;

/* Private routines */
static void bitmap_init(int ram);
static void bitmap_char(int char_index, int ram);
static void bitmap_free(int char_index, int start, int end);

static void CopyBlitImageTo1Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint8 *dst, int dstskip, const Uint8 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80)
        *dst = map[1];
      else
        *dst = map[0];

      byte <<= 1;
      dst++;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void CopyBlitImageTo2Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint16 *dst, int dstskip, const Uint16 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80)
        *dst = map[1];
      else
        *dst = map[0];

      byte <<= 1;
      dst++;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void CopyBlitImageTo3Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint8 *dst, int dstskip, const Uint8 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80) {
        dst[0] = map[3];
        dst[1] = map[4];
        dst[2] = map[5];
      } else {
        dst[0] = map[0];
        dst[1] = map[1];
        dst[2] = map[2];
      }

      byte <<= 1;
      dst += 3;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void CopyBlitImageTo4Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint32 *dst, int dstskip, const Uint32 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80)
        *dst = map[1];
      else
        *dst = map[0];

      byte <<= 1;
      dst++;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void XorBlitImageTo1Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint8 *dst, int dstskip, const Uint8 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80) {
        if (*dst == map[0])
          *dst = map[1];
        else
          *dst = map[0];
      }

      byte <<= 1;
      dst++;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void XorBlitImageTo2Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint16 *dst, int dstskip, const Uint16 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80) {
        if (*dst == map[0])
          *dst = map[1];
        else
          *dst = map[0];
      }

      byte <<= 1;
      dst++;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void XorBlitImageTo3Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint8 *dst, int dstskip, const Uint8 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80) {
        if ((dst[0] == map[0]) && (dst[1] == map[1]) && (dst[2] == map[2])) {
          dst[0] = map[3];
          dst[1] = map[4];
          dst[2] = map[5];
        } else {
          dst[0] = map[0];
          dst[1] = map[1];
          dst[2] = map[2];
        }
      }

      byte <<= 1;
      dst += 3;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void XorBlitImageTo4Byte(int width, int height, const Uint8 *src,
    int srcskip, Uint32 *dst, int dstskip, const Uint32 *map)
{
  while (height--) {
    Uint8 byte = 0;
    int c;

    for (c = 0; c < width; ++c) {
      if ((c & 7) == 0)
        byte = *src++;

      if (byte & 0x80) {
        if (*dst == map[0])
          *dst = map[1];
        else
          *dst = map[0];
      }

      byte <<= 1;
      dst++;
    }

    src += srcskip;
    dst += dstskip;
  }
}

static void TrsBlitMap(const SDL_Palette *src, SDL_PixelFormat *dst)
{
  Uint8 *map;
  int i;
  Uint32 alpha;
  Uint32 mapValue;

  if (blitMap != NULL)
    free(blitMap);

  map = (Uint8 *)malloc(src->ncolors * dst->BytesPerPixel);
  if (map == NULL) {
    return;
  }

  alpha = dst->Amask ? SDL_ALPHA_OPAQUE : 0;
  for (i = 0; i < src->ncolors; ++i) {

    mapValue = SDL_MapRGBA(dst,
        src->colors[i].r,
        src->colors[i].g,
        src->colors[i].b,
        alpha);

    switch (dst->BytesPerPixel) {
      case 1:
        map[i] = (Uint8)mapValue;
        break;
      case 2:
        *((Uint16 *)(&map[i * 2])) = (Uint16)mapValue;
        break;
      case 3:
        map[i * 3] = mapValue >> 16;
        map[i * 3 + 1] = mapValue >> 8;
        map[i * 3 + 2] = mapValue & 0xFF;
        break;
      case 4:
        *((Uint32 *)(&map[i * 4])) = (Uint32)mapValue;
        break;
    }
  }

  blitMap = map;
}

static void TrsSoftBlit(const SDL_Surface *src, const SDL_Rect *srcrect,
    SDL_Surface *dst, SDL_Rect *dstrect, int xor)
{
  /* Set up the blit information */
  int const rect_h  = srcrect->h;
  int const rect_w  = srcrect->w;
  int const bpp     = dst->format->BytesPerPixel;
  int const bit_pix = src->format->BitsPerPixel;
  int const srcskip = src->pitch - (rect_w * bit_pix) / 8;
  int const dstskip = dst->pitch -  rect_w * bpp;

  Uint8 const *srcpix = (Uint8 *)src->pixels +
      (Uint16)srcrect->y * src->pitch +
     ((Uint16)srcrect->x * bit_pix) / 8;

  Uint8 *dstpix = (Uint8 *)dst->pixels +
      (Uint16)dstrect->y * dst->pitch +
      (Uint16)dstrect->x * bpp;

  dstrect->h = rect_h;
  dstrect->w = rect_w;

  /* Lock the destination if it's in hardware */
  if (SDL_MUSTLOCK(dst)) {
    if (SDL_LockSurface(dst) < 0)
      return;
  }

  /* Run the actual software blit */
  switch (bpp) {
    case 1:
      if (xor)
        XorBlitImageTo1Byte(rect_w, rect_h, srcpix, srcskip,
            dstpix, dstskip, blitMap);
      else
        CopyBlitImageTo1Byte(rect_w, rect_h, srcpix, srcskip,
            dstpix, dstskip, blitMap);
      break;
    case 2:
      if (xor)
        XorBlitImageTo2Byte(rect_w, rect_h, srcpix, srcskip,
            (Uint16 *)dstpix, dstskip / 2, (Uint16 *)blitMap);
      else
        CopyBlitImageTo2Byte(rect_w, rect_h, srcpix, srcskip,
            (Uint16 *)dstpix, dstskip / 2, (Uint16 *)blitMap);
      break;
    case 3:
      if (xor)
        XorBlitImageTo3Byte(rect_w, rect_h, srcpix, srcskip,
            dstpix, dstskip, blitMap);
      else
        CopyBlitImageTo3Byte(rect_w, rect_h, srcpix, srcskip,
            dstpix, dstskip, blitMap);
      break;
    case 4:
      if (xor)
        XorBlitImageTo4Byte(rect_w, rect_h, srcpix, srcskip,
            (Uint32 *)dstpix, dstskip / 4, (Uint32 *)blitMap);
      else
        CopyBlitImageTo4Byte(rect_w, rect_h, srcpix, srcskip,
            (Uint32 *)dstpix, dstskip / 4, (Uint32 *)blitMap);
      break;
    default:
      break;
  }

  if (SDL_MUSTLOCK(dst))
    SDL_UnlockSurface(dst);
}

static Uint8 mirror_bits(Uint8 byte)
{
  byte = ((byte >> 4) & 0x0F) | ((byte << 4) & 0xF0);
  byte = ((byte >> 2) & 0x33) | ((byte << 2) & 0xCC);
  byte = ((byte >> 1) & 0x55) | ((byte << 1) & 0xAA);

  return byte;
}

void trs_screen_reset(void)
{
  currentmode = NORMAL;
  hrg_enable = 0;
  genie3s = 0;
  grafyx_enable = 0;
  grafyx_mode = 0;
  grafyx_overlay = 0;
  m6845_raster = 12;
  text80x24 = 0;
  screen640x240 = 0;
  screen_chars = 1024;
  row_chars = 64;
  col_chars = 16;
  scale_factor = 2;

  /* initially, screen is blank (i.e. full of spaces) */
  memset(trs_screen, ' ', SCREEN_SIZE);
  memset(char_ram, 0, MAX_CHARS * MAX_CHAR_HEIGHT);
  memset(grafyx, 0, G_MSIZE);
  memset(grafyx_unscaled, 0, G_YSIZE * G_XSIZE);
  memset(hrg_screen, 0, HRG_MEMSIZE);

  SDL_FillRect(image, NULL, background);
}

void trs_screen_caption(void)
{
  char title[80];

  if (cpu_panel)
    snprintf(title, 79, "AF:%04X BC:%04X DE:%04X HL:%04X IX/IY:%04X/%04X PC/SP:%04X/%04X",
             Z80_AF, Z80_BC, Z80_DE, Z80_HL, Z80_IX, Z80_IY, Z80_PC, Z80_SP);
  else {
    static const char *trs_name[] = {
        "TRS-80 Model I", "", "TRS-80 Model III", "TRS-80 Model 4", "TRS-80 Model 4P" };

    snprintf(title, 79, "%s%s (%.2f MHz)%s%s",
             timer_overclock ? "Turbo " : "",
             trs_clones.name ? trs_clones.name : trs_name[trs_model - 1],
             z80_state.clockMHz,
             trs_paused ? " PAUSED " : "",
             trs_sound ? "" : " (Mute)");
  }

  SDL_SetWindowTitle(window, title);
}

void trs_screen_init(int resize)
{
  int const led_height = trs_show_led ? 8 : 0;
  SDL_Color colors[2];
#if SDLDEBUG
  SDL_RendererInfo renderinfo = { 0 };
#endif

  border_width    = fullscreen ? 0 : window_border_width;
  cur_char_width  = TRS_CHAR_WIDTH;
  cur_char_height = TRS_CHAR_HEIGHT * 2;

  if (trs_model == 1) {
    switch (trs_clones.model) {
      case EG3200:
        /* Use alternate font for Holte-ROM */
        trs_charset = trs_rom_size > 2048 ? 16 : 15;
        break;
      case GENIE3S:
        cur_char_height = m6845_raster * scale_factor;
        break;
      default:
        currentmode = NORMAL;
        trs_charset = trs_charset1;
        if (trs_charset < 3)
          cur_char_width = 6;
        else if (trs_charset == 13)
          cur_char_height = 14 * scale_factor;
        break;
    }
  } else {
    if (trs_model == 3) {
      currentmode = NORMAL;
      trs_charset = trs_charset3;
       trs_resize = resize3;
    } else {
      trs_charset = trs_charset4;
       trs_resize = resize4;
    }

    if (screen640x240 || text80x24)
      cur_char_height = TRS_CHAR_HEIGHT4 * 2;
  }

  if (trs_model >= 3 && !trs_resize) {
    OrigWidth = cur_char_width * 80 + 2 * border_width;
    left_margin = cur_char_width * (80 - row_chars) / 2 + border_width;
    OrigHeight = TRS_CHAR_HEIGHT4 * 2 * 24 + 2 * border_width + led_height;
    top_margin = (TRS_CHAR_HEIGHT4 * 2 * 24 - cur_char_height * col_chars)
               / 2 + border_width;
  } else {
    OrigWidth = cur_char_width * (hrg_enable == 2 ? 80 : row_chars) + 2 * border_width;
    left_margin = border_width;
    OrigHeight = cur_char_height * col_chars + 2 * border_width + led_height;
    top_margin = border_width;
  }

  screen_height = OrigHeight - led_height;

  paste_state = 0;
  paste_lastkey = 0;
  copyStatus = 0;
  selectAll = 0;

  if (window == NULL) {
    window = SDL_CreateWindow(NULL,
                              window_x, window_y,
                              800, 600,
                              SDL_WINDOW_HIDDEN|SDL_WINDOW_RESIZABLE);
    if (window == NULL)
      fatal("SDL_CreateWindow failed: %s", SDL_GetError());

    render = SDL_CreateRenderer(window, -1, 0);
    if (render == NULL)
      fatal("SDL_CreateRenderer failed: %s", SDL_GetError());

    SDL_SetHint(SDL_HINT_RENDER_SCALE_QUALITY, &scale_quality);

#if SDLDEBUG
    SDL_GetRendererInfo(render, &renderinfo);
    debug("SDL_VIDEODRIVER=%s\n", SDL_GetCurrentVideoDriver());
    debug("SDL_RENDER_DRIVER=%s\n", renderinfo.name);
#endif

    screen = SDL_CreateRGBSurface(0, 1024, 1152, 32, 0, 0, 0, 0);
    if (screen == NULL)
      fatal("SDL_CreateRGBSurface failed: %s", SDL_GetError());

#if defined(big_endian) && !defined(__linux)
    light_red     = SDL_MapRGB(screen->format, 0x00, 0x00, 0x40);
    bright_red    = SDL_MapRGB(screen->format, 0x00, 0x00, 0xff);
    light_orange  = SDL_MapRGB(screen->format, 0x40, 0x28, 0x40);
    bright_orange = SDL_MapRGB(screen->format, 0x00, 0xa0, 0xff);
#else
    light_red     = SDL_MapRGB(screen->format, 0x40, 0x00, 0x00);
    bright_red    = SDL_MapRGB(screen->format, 0xff, 0x00, 0x00);
    light_orange  = SDL_MapRGB(screen->format, 0x40, 0x28, 0x00);
    bright_orange = SDL_MapRGB(screen->format, 0xff, 0xa0, 0x00);
#endif

    image = SDL_CreateRGBSurfaceFrom(grafyx, G_XSIZE * 8, G_YSIZE * 2, 1,
                                     G_XSIZE, 1, 1, 1, 0);
    if (image == NULL)
      fatal("SDL_CreateRGBSurface failed: %s", SDL_GetError());
  }

  if (texture)
    SDL_DestroyTexture(texture);
  texture = SDL_CreateTexture(render,
                              SDL_PIXELFORMAT_ARGB8888,
                              SDL_TEXTUREACCESS_STREAMING,
                              OrigWidth, OrigHeight);
  if (texture == NULL)
    fatal("SDL_CreateTexture failed: %s", SDL_GetError());

  if (aspect_ratio)
    SDL_RenderSetLogicalSize(render, OrigWidth, OrigHeight);

  if (resize && fullscreen == 0) {
    SDL_SetWindowPosition(window, window_x, window_y);
    if (window_w > (OrigWidth * scale) && window_h > (OrigHeight * scale))
      SDL_SetWindowSize(window, window_w, window_h);
    else
      SDL_SetWindowSize(window, OrigWidth * scale, OrigHeight * scale);
  }

  SDL_SetWindowFullscreen(window, fullscreen ? SDL_WINDOW_FULLSCREEN_DESKTOP : 0);
  SDL_ShowWindow(window);
  SDL_ShowCursor(mousepointer ? SDL_ENABLE : SDL_DISABLE);

#if defined(big_endian) && !defined(__linux)
  colors[0].r = (background) & 0xFF;
  colors[0].g = (background >> 8) & 0xFF;
  colors[0].b = (background >> 16) & 0xFF;
  colors[1].r = (foreground) & 0xFF;
  colors[1].g = (foreground >> 8) & 0xFF;
  colors[1].b = (foreground >> 16) & 0xFF;
#else
  colors[0].r = (background >> 16) & 0xFF;
  colors[0].g = (background >> 8) & 0xFF;
  colors[0].b = (background) & 0xFF;
  colors[1].r = (foreground >> 16) & 0xFF;
  colors[1].g = (foreground >> 8) & 0xFF;
  colors[1].b = (foreground) & 0xFF;
#endif

  back_color  = SDL_MapRGB(screen->format, colors[0].r, colors[0].g, colors[0].b);

  SDL_SetRenderDrawColor(render, colors[0].r, colors[0].g, colors[0].b, SDL_ALPHA_OPAQUE);
  SDL_RenderFillRect(render, NULL);
  SDL_SetPaletteColors(image->format->palette, colors, 0, 2);

  TrsBlitMap(image->format->palette, screen->format);

  bitmap_init(genie3s);
  trs_screen_refresh();
}

static void DrawRectangle(int orig_x, int orig_y, int copy_x, int copy_y)
{
  int const bpp   = screen->format->BytesPerPixel;
  int const pitch = screen->pitch;
  Uint8 *pixels   = screen->pixels;
  Uint8 *pixel;
  int x, y;

  if (copy_x < orig_x) {
    int swap = copy_x;

    copy_x = orig_x;
    orig_x = swap;
  }

  if (copy_y < orig_y) {
    int swap = copy_y;

    copy_y = orig_y;
    orig_y = swap;
  }

  if (SDL_MUSTLOCK(screen))
    SDL_LockSurface(screen);

  orig_x *= bpp;
  copy_x *= bpp;
  copy_y *= pitch;
  orig_y *= pitch;

  pixel = pixels + orig_y + orig_x;

  for (x = 0; x < copy_x - orig_x + bpp; x++)
    *pixel++ ^= 0xFF;

  if (copy_y > orig_y) {
    pixel = pixels + copy_y + orig_x;
    for (x = 0; x < copy_x - orig_x + bpp; x++)
      *pixel++ ^= 0xFF;
  }

  for (y = orig_y + pitch; y < copy_y; y += pitch) {
    pixel = pixels + y + orig_x;
    for (x = 0; x < bpp; x++)
      *pixel++ ^= 0xFF;
  }

  if (copy_x > orig_x) {
    for (y = orig_y + pitch; y < copy_y; y += pitch) {
      pixel = pixels + y + copy_x;
      for (x = 0; x < bpp; x++)
        *pixel++ ^= 0xFF;
    }
  }

  if (SDL_MUSTLOCK(screen))
    SDL_UnlockSurface(screen);
}

static void MarkSelection(void)
{
  static int orig_x, orig_y, end_x, end_y, copy_x, copy_y;
  static Uint8 mouse;

  if (selectAll) {
    if (copyStatus == COPY_STARTED)
      return;

    if (copyStatus == COPY_DEFINED || copyStatus == COPY_CLEAR)
      DrawRectangle(orig_x, orig_y, end_x, end_y);

    orig_x = 0;
    orig_y = 0;
    copy_x = end_x = OrigWidth - scale;
    copy_y = end_y = screen_height - scale;
    DrawRectangle(orig_x, orig_y, end_x, end_y);
    selection_x1 = orig_x;
    selection_y1 = orig_y;
    selection_x2 = copy_x;
    selection_y2 = copy_y;
    drawnRectCount = MAX_RECTS;
    copyStatus = COPY_DEFINED;
  } else {
    mouse = SDL_GetMouseState(&copy_x, &copy_y);
    if ((mouse & SDL_BUTTON(SDL_BUTTON_LEFT)) == 0 &&
        copyStatus == COPY_IDLE)
      return;

    if (aspect_ratio) {
      float const real_w = ((float)OrigWidth  + render_scale_x)
          * render_scale_x;
      float const real_h = ((float)OrigHeight + render_scale_y)
          * render_scale_y;

      copy_x = copy_x - ((window_w / 2) - (real_w / 2));
      copy_y = copy_y - ((window_h / 2) - (real_h / 2));
      copy_x = copy_x / real_w * OrigWidth;
      copy_y = copy_y / real_h * OrigHeight;
    } else {
      copy_x /= ((float)window_w / (float)OrigWidth);
      copy_y /= ((float)window_h / (float)OrigHeight);
    }

    if (copy_x < 0)
      copy_x = 0;
    else if (copy_x > OrigWidth - scale)
      copy_x = OrigWidth - scale;

    if (copy_y < 0)
      copy_y = 0;
    else if (copy_y > screen_height - scale)
      copy_y = screen_height - scale;
  }

  switch (copyStatus) {
    case COPY_IDLE:
      if (selectAll) {
        orig_x = 0;
        orig_y = 0;
        DrawRectangle(orig_x, orig_y, copy_x, copy_y);
        selection_x1 = orig_x;
        selection_y1 = orig_y;
        selection_x2 = copy_x;
        selection_y2 = copy_y;
        drawnRectCount = MAX_RECTS;
        copyStatus = COPY_DEFINED;
      }
      else if (mouse & SDL_BUTTON(SDL_BUTTON_LEFT) ) {
        orig_x = copy_x;
        orig_y = copy_y;
        DrawRectangle(orig_x, orig_y, copy_x, copy_y);
        drawnRectCount = MAX_RECTS;
        copyStatus = COPY_STARTED;
      }
      end_x = copy_x;
      end_y = copy_y;
      break;
    case COPY_STARTED:
      DrawRectangle(orig_x, orig_y, end_x, end_y);
      if (mouse & SDL_BUTTON(SDL_BUTTON_LEFT))
        DrawRectangle(orig_x, orig_y, copy_x, copy_y);

      drawnRectCount = MAX_RECTS;
      end_x = copy_x;
      end_y = copy_y;
      if ((mouse & SDL_BUTTON(SDL_BUTTON_LEFT)) == 0) {
        if (orig_x == copy_x && orig_y == copy_y) {
          copyStatus = COPY_IDLE;
        } else {
          DrawRectangle(orig_x, orig_y, end_x, end_y);
          selection_x1 = orig_x;
          selection_y1 = orig_y;
          selection_x2 = copy_x;
          selection_y2 = copy_y;
          copyStatus = COPY_DEFINED;
        }
      }
      break;
    case COPY_DEFINED:
      if (mouse & SDL_BUTTON(SDL_BUTTON_LEFT)) {
        DrawRectangle(orig_x, orig_y, end_x, end_y);
        orig_x = end_x = copy_x;
        orig_y = end_y = copy_y;
        DrawRectangle(orig_x, orig_y, copy_x, copy_y);
        drawnRectCount = MAX_RECTS;
        copyStatus = COPY_STARTED;
      }
      break;
    case COPY_CLEAR:
      DrawRectangle(orig_x, orig_y, end_x, end_y);
      drawnRectCount = MAX_RECTS;
      copyStatus = COPY_IDLE;
  }
}

static char *GetSelection(void)
{
  static char copy_data[SCREEN_SIZE];
  char *curr_data = copy_data;
  int col, row;
  int start_col, end_col, start_row, end_row;

  if (grafyx_enable && !grafyx_overlay) {
    copy_data[0] = 0;
    return copy_data;
  }

  if (selection_x1 > selection_x2) {
    int swap = selection_x1;

    selection_x1 = selection_x2;
    selection_x2 = swap;
  }

  if (selection_y1 > selection_y2) {
    int swap = selection_y1;

    selection_y1 = selection_y2;
    selection_y2 = swap;
  }

  selection_x1 -= left_margin;
  selection_x2 -= left_margin;
  selection_y1 -= top_margin;
  selection_y2 -= top_margin;

  if (selection_x1 < 0)
    selection_x1 = 0;

  if (selection_y1 < 0)
    selection_y1 = 0;

  if (selection_x1 % cur_char_width == 0)
    start_col = selection_x1 / cur_char_width;
  else
    start_col = selection_x1 / cur_char_width + 1;

  if (selection_x2 % cur_char_width == cur_char_width - 1)
    end_col = selection_x2 / cur_char_width;
  else
    end_col = selection_x2 / cur_char_width - 1;

  if (selection_y1 % cur_char_height == 0)
    start_row = selection_y1 / cur_char_height;
  else
    start_row = selection_y1 / cur_char_height + 1;

  if (selection_y2 % cur_char_height >= cur_char_height / 2)
    end_row = selection_y2 / cur_char_height;
  else
    end_row = selection_y2 / cur_char_height - 1;

  if (end_col >= row_chars)
    end_col = row_chars - 1;

  if (end_row >= col_chars)
    end_row = col_chars - 1;

  for (row = start_row; row <= end_row; row++) {
    Uint8 const *screen_ptr = &trs_screen[row * row_chars + start_col];

    for (col = start_col; col <= end_col; col++, screen_ptr++) {
      Uint8 const data = *screen_ptr;

      if (data < 0x20)
        *curr_data++ = data + 0x40;
      else
        *curr_data++ = data & 0x7F;
    }

    if (row != end_row) {
#ifdef _WIN32
      *curr_data++ = 0xd;
#endif
      *curr_data++ = 0xa;
    }
  }

  *curr_data = 0;
  return copy_data;
}

/*
 * Flush SDL output
 */
static void trs_screen_flush(void)
{
  if (mousepointer) {
    if (!trs_emu_mouse && paste_state == PASTE_IDLE) {
      MarkSelection();
      selectAll = 0;
    }
  }

  if (drawnRectCount == 0)
    return;

  if (scanlines) {
#ifdef OLD_SCANLINES
    SDL_Rect rect;

    rect.x = 0;
    rect.w = OrigWidth;
    rect.h = 1;

    for (rect.y = 0; rect.y < screen_height; rect.y += 2)
      SDL_FillRect(screen, &rect, back_color);
#else
    int const width = screen->format->BytesPerPixel * OrigWidth;
    int const pitch = screen->pitch;
    Uint8 *pixels   = screen->pixels;
    int x, y;

    if (SDL_MUSTLOCK(screen))
      SDL_LockSurface(screen);

    for (y = 0; y < pitch * screen_height; y += pitch * 2) {
      Uint8 *pixel = pixels + y;

      for (x = 0; x < width; x++)
        *pixel++ &= scanshade;
    }

    if (SDL_MUSTLOCK(screen))
      SDL_UnlockSurface(screen);
#endif
  }

  trs_screen_update();

  drawnRectCount = 0;
}

void trs_exit(int confirm)
{
  static int recursion;

  if (recursion && confirm)
    return;

  if (confirm) {
    SDL_Surface *buffer = SDL_ConvertSurface(screen, screen->format, 0);

    recursion = 1;

    if (!trs_gui_exit_sdltrs() && buffer) {
      SDL_BlitSurface(buffer, NULL, screen, NULL);
      SDL_FreeSurface(buffer);
      trs_screen_update();
      recursion = 0;
      return;
    }
  }

  exit(EXIT_SUCCESS);
}

static void trs_flip_fullscreen(void)
{
  fullscreen = !fullscreen;
  SDL_SetWindowFullscreen(window, fullscreen ? SDL_WINDOW_FULLSCREEN_DESKTOP : 0);
}

/*
 * Get and process SDL event(s).
 *   If wait is true, process one event, blocking until one is available.
 *   If wait is false, process as many events as are available, returning
 *     when none are left.
 * Handle interrupt-driven uart input here too.
 */
void trs_get_event(int wait)
{
  SDL_Event event;
  SDL_Keysym keysym;

  SDL_StartTextInput();

  if (trs_model > 1)
    (void)trs_uart_check_avail();

  trs_screen_flush();

  if (cpu_panel)
    trs_screen_caption();

  if (paste_state != PASTE_IDLE) {
    static Uint8 paste_key;

    if (SDL_PollEvent(&event)) {
      if (event.type == SDL_KEYDOWN) {
        if (paste_state == PASTE_KEYUP)
          trs_xlate_keysym(0x10000 | paste_key);
        paste_state = PASTE_KEYUP;
        paste_lastkey = 1;
      }
    }

    switch (paste_state) {
      case PASTE_GETNEXT:
        paste_lastkey = !PasteManagerGetChar(&paste_key);
#ifndef _WIN32
        if (paste_key == 0xa)
          paste_key = 0xd;
#endif
        trs_xlate_keysym(paste_key);
        paste_state = PASTE_KEYDOWN;
        break;
      case PASTE_KEYDOWN:
        trs_xlate_keysym(0x10000 | paste_key);
        paste_state = PASTE_KEYUP;
        break;
      case PASTE_KEYUP:
        if (paste_lastkey) {
          paste_state = PASTE_IDLE;
          if (turbo_paste)
            trs_timer_mode(timer_saved);
          cycles_per_timer = cycles_saved;
        }
        else
          paste_state = PASTE_GETNEXT;
        break;
    }
    return;
  }

  do {
    if (wait)
      SDL_WaitEvent(&event);
    else
      if (!SDL_PollEvent(&event))
        return;

    switch (event.type) {
      case SDL_QUIT:
        trs_exit(0);
        break;
      case SDL_WINDOWEVENT:
        trs_screen_update();
        drawnRectCount = MAX_RECTS;
        if (event.window.event & SDL_WINDOWEVENT_EXPOSED) {
          SDL_FlushEvent(SDL_KEYDOWN);
#if SDLDEBUG
          debug("Active\n");
#endif
        }
        switch (event.window.event) {
          case SDL_WINDOWEVENT_MOVED:
            window_x = event.window.data1;
            window_y = event.window.data2;
            break;
          case SDL_WINDOWEVENT_RESIZED:
          case SDL_WINDOWEVENT_SIZE_CHANGED:
            window_w = event.window.data1;
            window_h = event.window.data2;
            SDL_RenderGetScale(render, &render_scale_x, &render_scale_y);
            break;
          default:
            break;
        }
        break;

      case SDL_KEYDOWN:
        keysym = event.key.keysym;
#if SDLDEBUG
        debug("KeyDown: mod 0x%x, scancode 0x%x keycode 0x%x\n",
            keysym.mod, keysym.scancode, keysym.sym);
#endif
        if (keysym.sym != SDLK_LALT) {
          if (copyStatus != COPY_IDLE) {
            copyStatus = COPY_CLEAR;
            trs_screen_flush();
          }
        }

        switch (keysym.sym) {
          /* Trap some function keys here */
          case SDLK_F7:
            if ((trs_clones.model & (EG3200 | GENIE3S)) == 0) {
              if (SDL_GetModState() & KMOD_SHIFT)
                call_function(EMULATOR);
              else
                call_function(GUI);
              continue;
            }
            break;
          case SDLK_F8:
            if ((trs_clones.model & (EG3200 | GENIE3S)) == 0) {
              trs_exit(!(SDL_GetModState() & KMOD_SHIFT));
              continue;
            }
            break;
          case SDLK_F9:
            if (SDL_GetModState() & KMOD_SHIFT) {
              cpu_panel = !cpu_panel;
              trs_screen_caption();
            } else {
#ifdef ZBX
              if (fullscreen)
                trs_flip_fullscreen();
              trs_debug();
#else
              trs_flip_fullscreen();
#endif
            }
            continue;
          case SDLK_F10:
            trs_reset(SDL_GetModState() & KMOD_SHIFT);
            continue;
          case SDLK_F11:
            if (SDL_GetModState() & KMOD_SHIFT)
              call_function(SAVE_BMP);
            else
              call_function(KEYS);
            continue;
          case SDLK_F12:
            if (SDL_GetModState() & KMOD_SHIFT)
              trs_timer_init();
            else
              trs_timer_mode(!timer_overclock);
            continue;
          case SDLK_PAGEDOWN:
          case SDLK_PAGEUP:
            if (SDL_GetModState() & KMOD_SHIFT) {
              trs_timer_speed(keysym.sym == SDLK_PAGEUP);
              continue;
            }
            break;
          case SDLK_PAUSE:
            call_function(PAUSE);
            continue;
          case SDLK_NUMLOCKCLEAR:
            trs_keypad_joystick = !trs_keypad_joystick;
            trs_set_keypad_joystick();
            continue;
          default:
            break;
        }
        /* Trap the alt keys here */
        if (SDL_GetModState() & KMOD_LALT) {
          switch (keysym.sym) {
            case SDLK_c:
              PasteManagerStartCopy(GetSelection());
              copyStatus = COPY_IDLE;
              break;
            case SDLK_v:
              if (turbo_paste) {
                timer_saved = timer_overclock;
                trs_timer_mode(1);
              }
              cycles_saved = cycles_per_timer;
              cycles_per_timer *= 4;
              PasteManagerStartPaste();
              paste_state = PASTE_GETNEXT;
              break;
            case SDLK_a:
              selectAll = mousepointer = 1;
              SDL_ShowCursor(SDL_ENABLE);
              break;
            case SDLK_DELETE:
              trs_reset(0);
              break;
            case SDLK_INSERT:
              call_function(KEYBRD);
              break;
            case SDLK_RETURN:
              trs_flip_fullscreen();
              break;
            case SDLK_HOME:
              fullscreen = 0;
              scale = 1;
              SDL_SetWindowFullscreen(window, 0);
              SDL_SetWindowSize(window, OrigWidth * scale, OrigHeight * scale);
              break;
            case SDLK_PAGEDOWN:
              if (scale < MAX_SCALE) {
                scale++;
                fullscreen = 0;
                SDL_SetWindowFullscreen(window, 0);
                SDL_SetWindowSize(window, OrigWidth * scale, OrigHeight * scale);
              }
              break;
            case SDLK_PAGEUP:
              if (scale > 1) {
                scale--;
                fullscreen = 0;
                SDL_SetWindowFullscreen(window, 0);
                SDL_SetWindowSize(window, OrigWidth * scale, OrigHeight * scale);
              }
              break;
            case SDLK_MINUS:
            case SDLK_8:
              if (z80_state.clockMHz > 0.1) {
                z80_state.clockMHz -= 0.1;
                trs_timer_mode(-1);
              }
              break;
            case SDLK_PLUS:
            case SDLK_9:
              if (z80_state.clockMHz < 99.0) {
                z80_state.clockMHz += 0.1;
                trs_timer_mode(-1);
              }
              break;
            case SDLK_PERIOD:
              mousepointer = !mousepointer;
              SDL_SetRelativeMouseMode(mousepointer ? SDL_FALSE : SDL_TRUE);
              SDL_SetWindowGrab(window, mousepointer ? SDL_FALSE : SDL_TRUE);
              break;
            case SDLK_b:
              trs_show_led = !trs_show_led;
              trs_screen_init(1);
              break;
            case SDLK_d:
            case SDLK_f:
              call_function(DISK);
              break;
            case SDLK_e:
              call_function(EMULATOR);
              break;
            case SDLK_g:
              call_function(STRINGY);
              break;
            case SDLK_h:
              call_function(HARD);
              break;
            case SDLK_i:
              call_function(INTERFACE);
              break;
            case SDLK_j:
              call_function(JOYGUI);
              break;
            case SDLK_k:
              call_function(KEYS);
              break;
            case SDLK_l:
              call_function(LOAD);
              break;
            case SDLK_m:
            case SDLK_COMMA:
              call_function(GUI);
              break;
            case SDLK_n:
              trs_timer_mode(!timer_overclock);
              break;
            case SDLK_o:
              call_function(OTHER);
              break;
            case SDLK_p:
              call_function(PAUSE);
              break;
#if defined(__OS2__) || defined(_WIN32)
            case SDLK_F4:
#endif
            case SDLK_q:
            case SDLK_END:
              trs_exit(1);
              break;
            case SDLK_r:
              call_function(READ);
              break;
            case SDLK_s:
              call_function(SAVE);
              break;
            case SDLK_t:
              call_function(TAPE);
              break;
            case SDLK_u:
              trs_sound = !trs_sound;
              trs_screen_caption();
              break;
            case SDLK_w:
              call_function(WRITE);
              break;
            case SDLK_x:
              call_function(EXEC);
              break;
            case SDLK_y:
              scanlines = !scanlines;
              trs_screen_refresh();
              break;
            case SDLK_z:
#ifdef ZBX
              if (fullscreen)
                trs_flip_fullscreen();
              trs_debug();
#else
              trs_flip_fullscreen();
#endif
              break;
            default:
              if (keysym.sym >= SDLK_0 && keysym.sym <= SDLK_7) {
                keysym.sym -= SDLK_0;

                if (SDL_GetModState() & KMOD_SHIFT) {
                  trs_disk_remove(keysym.sym);
                } else {
                  char filename[FILENAME_MAX];

                  if (trs_gui_file_browse(trs_disk_dir, filename, NULL, 0,
                        "Floppy Disk Image") != -1)
                    trs_disk_insert(keysym.sym, filename);
                  trs_screen_refresh();
                }
              }
          }
          continue;
        }

        if (last_key[keysym.scancode])
        /*
         * We think this hardware key is already pressed.
         * Assume we are getting key repeat and ignore it.
         */
          break;

        /* Make Shift + CapsLock give lower case */
        if (((SDL_GetModState() & (KMOD_CAPS | KMOD_LSHIFT))
            == (KMOD_CAPS | KMOD_LSHIFT) ||
            ((SDL_GetModState() & (KMOD_CAPS | KMOD_RSHIFT))
            == (KMOD_CAPS | KMOD_RSHIFT)))
            && keysym.sym >= 'A' && keysym.sym <= 'Z')
          keysym.sym = (int) keysym.sym + 0x20;

        if (keysym.sym == SDLK_RSHIFT && trs_model == 1)
          keysym.sym = SDLK_LSHIFT;

        if (trs_clones.model & (EG3200 | GENIE3S)) {
          if (keysym.sym >= SDLK_F1 && keysym.sym <= SDLK_F8) {
            keysym.sym = (keysym.sym - SDLK_F1) + 0x080;
            goto done;
          }
          /* 1-5 on numeric keypad for P1-P5 / 6 for ESC */
          if (keysym.sym >= SDLK_KP_1 && keysym.sym <= SDLK_KP_6) {
            keysym.sym = (keysym.sym - SDLK_KP_1) + 0x88;
            goto done;
          }
          switch (keysym.sym) {
            case SDLK_END:   keysym.sym = 0x08d; goto done; /* ESC  */
            case SDLK_LCTRL: keysym.sym = 0x12f; goto done; /* Ctrl */
            default:
              break;
          }
        } else {
          if (trs_model == 1) {
            switch (keysym.sym) {
              case SDLK_F1:  keysym.sym = 0x115; goto done; /* _ */
              case SDLK_F2:  keysym.sym = 0x120; goto done; /* \ */
              case SDLK_F3:  keysym.sym = 0x121; goto done; /* ] */
              case SDLK_F4:  keysym.sym = 0x122; goto done; /* ^ */
              case SDLK_LCTRL: /* P1 on SpeedMaster or Control */
                keysym.sym = (speedup == 7) ? 0x11c : 0x11a;
                goto done;
              case SDLK_END:   /* P2 on SpeedMaster or Shifted Down Arrow */
                keysym.sym = (speedup == 7) ? 0x088 : 0x117;
                goto done;
              default:
                break;
            }
          } else {
            if (trs_model == 3 && keysym.sym == SDLK_LCTRL) {
              keysym.sym = 0x117; /* Shifted Down Arrow */
              goto done;
            }
          }
        }

        /* Convert numeric keypad */
        if (keysym.sym >= SDLK_KP_1 && keysym.sym <= SDLK_KP_9) {
          keysym.sym = (keysym.sym - SDLK_KP_1) + 0x101;
          goto done;
        }

        /* Convert arrow/control/function/shift keys */
        switch (keysym.sym) {
          case SDLK_KP_0:       keysym.sym = 0x100; goto done;
          case SDLK_KP_PERIOD:  keysym.sym = 0x10a; goto done;
          case SDLK_UP:         keysym.sym = 0x111; goto done;
          case SDLK_DOWN:       keysym.sym = 0x112; goto done;
          case SDLK_RIGHT:      keysym.sym = 0x113; goto done;
          case SDLK_LEFT:       keysym.sym = 0x114; goto done;
          case SDLK_INSERT:     keysym.sym = 0x115; goto done;
          case SDLK_HOME:       keysym.sym = 0x116; goto done;
          case SDLK_END:        keysym.sym = 0x117; goto done;
          case SDLK_PAGEUP:     keysym.sym = 0x118; goto done;
          case SDLK_PAGEDOWN:   keysym.sym = 0x119; goto done;
          case SDLK_CAPSLOCK:   keysym.sym = 0x11d; goto done;
          case SDLK_SCROLLLOCK: keysym.sym = 0x11e; goto done;
          case SDLK_F1:         keysym.sym = 0x11a; goto done;
          case SDLK_F2:         keysym.sym = 0x11b; goto done;
          case SDLK_F3:         keysym.sym = 0x11c; goto done;
          case SDLK_F4:         keysym.sym = 0x11d; goto done;
          case SDLK_F5:         keysym.sym = 0x11e; goto done;
          case SDLK_F6:         keysym.sym = 0x11f; goto done;
          case SDLK_RSHIFT:     keysym.sym = 0x12f; goto done;
          case SDLK_LSHIFT:     keysym.sym = 0x130; goto done;
          case SDLK_LCTRL:      keysym.sym = 0x132; goto done;
          default:
            break;
        }

        /* Convert uppercase and special chars */
        if (SDL_GetModState() & (KMOD_SHIFT | KMOD_RALT)) {
          if ((keysym.sym >= 0x21 && keysym.sym <= 0x7F) || keysym.sym == 0xDF) {
            if (SDL_PeepEvents(&event, 1, SDL_PEEKEVENT,
                SDL_FIRSTEVENT, SDL_LASTEVENT) == 1 && event.type == SDL_TEXTINPUT)
              keysym.sym = event.text.text[0] & 0xFF;
          }
        }

done:
        if (keysym.sym) {
          last_key[keysym.scancode] = keysym.sym;
          trs_xlate_keysym(keysym.sym);
        }
        break;

      case SDL_KEYUP:
        keysym = event.key.keysym;
#if SDLDEBUG
        debug("KeyUp: mod 0x%x, scancode 0x%x keycode 0x%x\n",
            keysym.mod, keysym.scancode, keysym.sym);
#endif
        if (SDL_GetModState() & KMOD_LALT)
          break;
        trs_xlate_keysym(0x10000 | last_key[keysym.scancode]);
        last_key[keysym.scancode] = 0;
        break;

      case SDL_JOYAXISMOTION:
        if (jaxis_mapped == 1 && (event.jaxis.axis == 0 || event.jaxis.axis == 1)) {
          static int hor_value, ver_value, hor_key, ver_key;
          int value = 0, trigger_keyup = 0, trigger_keydown = 0;

          if (event.jaxis.axis == 0)
            value = hor_value;
          else
            value = ver_value;

          if (event.jaxis.value < -JOY_BOUNCE) {
            if (value == 1)
              trigger_keyup = 1;

            if (value != -1)
              trigger_keydown = 1;

            value = -1;
          }
          else if (event.jaxis.value > JOY_BOUNCE) {
            if (value == -1)
              trigger_keyup = 1;

            if (value != 1)
              trigger_keydown = 1;

            value = 1;
          }
          else if (abs(event.jaxis.value) < JOY_BOUNCE / 8) {
            if (value)
              trigger_keyup = 1;

            value = 0;
          }

          if (trigger_keyup) {
            if (event.jaxis.axis == 0)
              trs_xlate_keysym(0x10000 | hor_key);
            else
              trs_xlate_keysym(0x10000 | ver_key);
          }

          if (trigger_keydown) {
            if (event.jaxis.axis == 0) {
              hor_key = (value == -1 ? 0x114 : 0x113); /* Left/Right */
              trs_xlate_keysym(hor_key);
            } else {
              ver_key = (value == -1 ? 0x111 : 0x112); /*  Up / Down */
              trs_xlate_keysym(ver_key);
            }
          }

          if (event.jaxis.axis == 0)
            hor_value = value;
          else
            ver_value = value;
        }
        else
          trs_joy_axis(event.jaxis.axis, event.jaxis.value, JOY_BOUNCE);
        break;

      case SDL_JOYHATMOTION:
        trs_joy_hat(event.jhat.value);
        break;

      case SDL_JOYBUTTONUP:
      case SDL_MOUSEBUTTONUP:
        if (event.type == SDL_MOUSEBUTTONUP) {
          if (mousepointer)
            break;
          else
            event.jbutton.button = event.button.button;
        }

        if (event.jbutton.button < N_JOYBUTTONS) {
          int key = jbutton_map[event.jbutton.button];

          if (key >= 0)
            trs_xlate_keysym(0x10000 | key);
          else if (key == -1)
            trs_joy_button_up();
        }
        else
          trs_joy_button_up();
        break;

      case SDL_JOYBUTTONDOWN:
      case SDL_MOUSEBUTTONDOWN:
        if (event.type == SDL_MOUSEBUTTONDOWN) {
          if (mousepointer) {
            if (event.button.button == SDL_BUTTON_RIGHT) {
              if (copyStatus != COPY_IDLE) {
                copyStatus = COPY_CLEAR;
                trs_screen_flush();
              }
              call_function(GUI);
            }
            break;
          }
          else
            event.jbutton.button = event.button.button;
        }

        if (event.jbutton.button < N_JOYBUTTONS) {
          int key = jbutton_map[event.jbutton.button];

          if (key >= 0)
            trs_xlate_keysym(key);
          else if (key == -1)
            trs_joy_button_down();
          else
            call_function(key);
        }
        else
          trs_joy_button_down();
        break;

      case SDL_MOUSEMOTION:
        if (!mousepointer) {
          SDL_MouseMotionEvent motion = event.motion;

          if (motion.xrel != 0) {
            if (jaxis_mapped) {
              if (abs(motion.xrel) > 2) {
                int key = motion.xrel < 0 ? 0x114 : 0x113;
                int i;

                for (i = 0; i < abs(motion.xrel); i++)
                  trs_xlate_keysym(key);
              }
            } else
              trs_joy_axis(0, motion.xrel, 1);
          } else
            if (jaxis_mapped)
              trs_xlate_keysym(0x10000);

          if (motion.yrel != 0) {
            if (jaxis_mapped) {
              if (abs(motion.yrel) > 2) {
                int key = motion.yrel < 0 ? 0x111 : 0x112;
                int i;

                for (i = 0; i < abs(motion.yrel); i++)
                  trs_xlate_keysym(key);
              }
            } else
              trs_joy_axis(1, motion.yrel, 2);
          } else
            if (jaxis_mapped)
              trs_xlate_keysym(0x10000);
        }
        break;

      default:
#if SDLDEBUG
      /* debug("Unhandled event: type %d\n", event.type); */
#endif
        break;
    }

    if (trs_paused && fullscreen)
      trs_gui_display_pause();

  } while (!wait);
  SDL_StopTextInput();
}

void trs_screen_expanded(int flag)
{
  if ((currentmode ^ (flag ? EXPANDED : 0)) & EXPANDED) {
    currentmode ^= EXPANDED;
    trs_screen_refresh();
  }
}

void trs_screen_inverse(int flag)
{
  int i;

  if ((currentmode ^ (flag ? INVERSE : 0)) & INVERSE) {
    currentmode ^= INVERSE;
    for (i = 0; i < screen_chars; i++) {
      if (trs_screen[i] & 0x80)
        trs_screen_write_char(i, trs_screen[i]);
    }
  }
}

void trs_screen_alternate(int flag)
{
  int i;

  if ((currentmode ^ (flag ? ALTERNATE : 0)) & ALTERNATE) {
    currentmode ^= ALTERNATE;
    for (i = 0; i < screen_chars; i++) {
      if (trs_screen[i] >= 0xc0)
        trs_screen_write_char(i, trs_screen[i]);
    }
  }
}

static void trs_screen_640x240(int flag)
{
  if (flag == screen640x240) return;

  screen640x240 = flag;

  if (flag) {
    row_chars = 80;
    col_chars = 24;
    cur_char_height = TRS_CHAR_HEIGHT4 * 2;
  } else {
    row_chars = 64;
    col_chars = 16;
    cur_char_height = TRS_CHAR_HEIGHT * 2;
  }

  screen_chars = row_chars * col_chars;

  if (trs_resize)
    trs_screen_init(1);
  else {
    left_margin = cur_char_width * (80 - row_chars) / 2 + border_width;
    top_margin = (TRS_CHAR_HEIGHT4 * 2 * 24 -
        cur_char_height * col_chars) / 2 + border_width;
    trs_screen_refresh();
  }
}

void trs_screen_80x24(int flag)
{
  if (flag == text80x24) return;

  text80x24 = flag;

  if (!grafyx_enable || grafyx_overlay)
    trs_screen_640x240(flag);
}

static void
boxes_init(int fg_color, int bg_color, int width, int height, int expanded)
{
  int graphics_char, bit;
  SDL_Rect fullrect;
  SDL_Rect bits[6];

  /*
   * Calculate what the 2x3 boxes look like.
   */
  bits[0].x = bits[2].x = bits[4].x = 0;
  bits[0].w = bits[2].w = bits[4].w =
      bits[1].x = bits[3].x = bits[5].x = width / 2;
  bits[1].w = bits[3].w = bits[5].w = width - bits[1].x;

  bits[0].y = bits[1].y = 0;
  bits[0].h = bits[1].h = bits[2].y = bits[3].y = height / 3;
  bits[4].y = bits[5].y = (height * 2) / 3;
  bits[2].h = bits[3].h = bits[4].y - bits[2].y;
  bits[4].h = bits[5].h = height - bits[4].y;

  fullrect.x = 0;
  fullrect.y = 0;
  fullrect.h = height;
  fullrect.w = width;

  for (graphics_char = 0; graphics_char < 64; ++graphics_char) {
    if (trs_box[expanded][graphics_char])
      SDL_FreeSurface(trs_box[expanded][graphics_char]);

    trs_box[expanded][graphics_char] =
      SDL_CreateRGBSurface(0, width, height, 32,
#if defined(big_endian) && !defined(__linux)
                           0x000000ff, 0x0000ff00, 0x00ff0000, 0);
#else
                           0x00ff0000, 0x0000ff00, 0x000000ff, 0);
#endif

    /* Clear everything */
    SDL_FillRect(trs_box[expanded][graphics_char], &fullrect, bg_color);

    for (bit = 0; bit < 6; ++bit) {
      if (graphics_char & (1 << bit))
        SDL_FillRect(trs_box[expanded][graphics_char], &bits[bit], fg_color);
    }
  }
}

static SDL_Surface *CreateSurfaceFromDataScale(const Uint8 *data,
    int fg_color, int bg_color, int scale_x)
{
  /*
   * The memory allocated for "fontdata" will be released in the
   * "bitmap_free" function.
   */
  int i, j, w;
  int *fontdata = (int*)calloc(1, TRS_CHAR_WIDTH * MAX_CHAR_HEIGHT *
      scale_x * scale_factor * sizeof(int));
  int *currdata = fontdata;
  Uint8 pixels[TRS_CHAR_WIDTH * MAX_CHAR_HEIGHT] = { 0 };

  if (fontdata == NULL)
    fatal("failed to allocate font data memory");

  /* Read the character data */
  for (j = 0; j < TRS_CHAR_WIDTH * MAX_CHAR_HEIGHT; j += 8)
    for (i = j + 7; i >= j; i--)
      pixels[i] = (*(data + (j >> 3)) >> (i - j)) & 1;

  /* And prepare our rescaled character. */
  for (j = 0; j < MAX_CHAR_HEIGHT * scale_factor; j++) {
    Uint8 const *currpixel = &pixels[((j / scale_factor) * TRS_CHAR_WIDTH)];

    for (w = 0; w < TRS_CHAR_WIDTH; w++) {
      if (*currpixel++ == 0) {
        for (i = 0; i < scale_x; i++)
          *currdata++ = bg_color;
      } else {
        for (i = 0; i < scale_x; i++)
          *currdata++ = fg_color;
      }
    }
  }

  return SDL_CreateRGBSurfaceFrom(fontdata, TRS_CHAR_WIDTH * scale_x,
         MAX_CHAR_HEIGHT * scale_factor, 32, TRS_CHAR_WIDTH * scale_x * 4,
#if defined(big_endian) && !defined(__linux)
         0x000000ff, 0x0000ff00, 0x00ff0000, 0);
#else
         0x00ff0000, 0x0000ff00, 0x000000ff, 0);
#endif
}

static void
bitmap_init(int ram)
{
  int const gui = trs_charset <= 2 ? 2 : 7;
  int const height = (trs_clones.model == CP500_M80)
      /* Adjust block graphics in CP-500/M80 80x24 video mode */
      ? MAX_CHAR_HEIGHT * 2
      : cur_char_height;
  int i;

  for (i = 0; i < MAX_CHARS; i++) {
    /* Create also bitmap chars 192-255 for Genie III EG 3210 PGA Card */
    bitmap_char(i, (i > 191 && eg3200) ? 1 : ram);

    /* GUI Normal + Inverse */
    bitmap_free(i, 4, 5);
    trs_char[4][i] = CreateSurfaceFromDataScale(
        trs_char_data[gui][i], gui_foreground, gui_background, 1);
    trs_char[5][i] = CreateSurfaceFromDataScale(
        trs_char_data[gui][i], gui_background, gui_foreground, 1);
  }

  boxes_init(foreground, background, cur_char_width, height, 0);
  boxes_init(foreground, background, cur_char_width * 2, height, 1);
}

static void
bitmap_char(int char_index, int ram)
{
  Uint8 const *char_data = ram ?
      char_ram[char_index] : trs_char_data[trs_charset][char_index];

  bitmap_free(char_index, 0, 3);

  /* Normal */
  trs_char[0][char_index] = CreateSurfaceFromDataScale(
      char_data, foreground, background, 1);
  /* Expanded */
  trs_char[1][char_index] = CreateSurfaceFromDataScale(
      char_data, foreground, background, 2);
  /* Inverse */
  trs_char[2][char_index] = CreateSurfaceFromDataScale(
      char_data, background, foreground, 1);
  /* Expanded + Inverse */
  trs_char[3][char_index] = CreateSurfaceFromDataScale(
      char_data, background, foreground, 2);
}

static void
bitmap_free(int char_index, int start, int end)
{
  int i;

  /* Free all surface pixels */
  for (i = start; i <= end; i++) {
    if (trs_char[i][char_index]) {
      free(trs_char[i][char_index]->pixels);
      SDL_FreeSurface(trs_char[i][char_index]);
    }
  }
}


void trs_screen_refresh(void)
{
#if SDLDEBUG
  debug("trs_screen_refresh\n");
#endif
  SDL_FillRect(screen, NULL, back_color);

  if (grafyx_enable && !grafyx_overlay) {
    int const srcx   = cur_char_width * grafyx_xoffset;
    int const srcy   = grafyx_yoffset * 2;
    int const dunx   = G_XSIZE * 8 - srcx;
    int const duny   = G_YSIZE * 2 - srcy;
    int const height = cur_char_height * col_chars;
    int const width  = cur_char_width  * row_chars;
    SDL_Rect srcRect, dstRect;

    srcRect.x = srcx;
    srcRect.y = srcy;
    srcRect.w = width;
    srcRect.h = height;
    dstRect.x = left_margin;
    dstRect.y = top_margin;
    SDL_BlitSurface(image, &srcRect, screen, &dstRect);

    /* Draw wrapped portions if any */
    if (dunx < width) {
      srcRect.x = 0;
      srcRect.y = srcy;
      srcRect.w = width - dunx;
      srcRect.h = height;
      dstRect.x = left_margin + dunx;
      dstRect.y = top_margin;
      SDL_BlitSurface(image, &srcRect, screen, &dstRect);
    }

    if (duny < height) {
      srcRect.x = srcx;
      srcRect.y = 0;
      srcRect.w = width;
      srcRect.h = height - duny;
      dstRect.x = left_margin;
      dstRect.y = top_margin + duny;
      SDL_BlitSurface(image, &srcRect, screen, &dstRect);

      if (dunx < width) {
        srcRect.x = 0;
        srcRect.y = 0;
        srcRect.w = width - dunx;
        srcRect.h = height - duny;
        dstRect.x = left_margin + dunx;
        dstRect.y = top_margin + duny;
        SDL_BlitSurface(image, &srcRect, screen, &dstRect);
      }
    }
  } else {
    int i;

    for (i = 0; i < screen_chars; i++)
      trs_screen_write_char(i, trs_screen[i]);

    /* Redraw HRG screen */
    if (hrg_enable == 2) {
      memset(grafyx_unscaled, 0, G_YSIZE * G_XSIZE);

      grafyx_overlay = 0;

      for (i = 0; i <= 0x3FFF; i++) {
        hrg_write_addr(i, 0x3FFF);
        hrg_write_data(hrg_screen[i]);
      }

      grafyx_overlay = 1;
    }
  }

  /* Redraw 6845 CRTC cursor */
  switch (get_mem_map()) {
    case 0x23: /* EG 3200 */
    case 0x24: /* Genie IIIs */
      z80_out(0xF6, 0x09);
      z80_out(0xF7, 0xFF);
      break;
    case 0x25: /* Schmidtke 80-Z Video Card */
      z80_out(0xD0, 0x09);
      z80_out(0xD1, 0xFF);
      break;
    case 0x27: /* Aster CT-80 */
      if (row_chars == 80) {
        z80_out(0xFC, 0x09);
        z80_out(0xFD, 0xFF);
      }
      break;
    default:
      break;
  }

  if (trs_show_led) {
    trs_disk_led(-1, 0);
    trs_hard_led(-1, 0);
    trs_turbo_led();
  }

  drawnRectCount = MAX_RECTS; /* Will force redraw of whole screen */
  trs_screen_flush();
  trs_screen_caption();
}

void trs_disk_led(int drive, int on_off)
{
  static int countdown[8];
  int const led_pos = border_width;
  int i;
  SDL_Rect rect;

  rect.w = 16;
  rect.h = 4;
  rect.y = OrigHeight - rect.h;

  if (drive == -1) {
    for (i = 0; i < 8; i++) {
      if (on_off == -1)
        countdown[i] = 0;

      rect.x = led_pos + 24 * i;
      SDL_FillRect(screen, &rect, countdown[i] ? bright_red : light_red);
      drawnRectCount = MAX_RECTS;
    }
  }
  else if (on_off) {
    if (countdown[drive] == 0) {
      rect.x = led_pos + 24 * drive;
      SDL_FillRect(screen, &rect, bright_red);
      drawnRectCount = MAX_RECTS;
    }
    countdown[drive] = 2 * timer_hz;
  }
  else {
    for (i = 0; i < 8; i++) {
      if (countdown[i]) {
        countdown[i]--;
        if (countdown[i] == 0) {
          rect.x = led_pos + 24 * i;
          SDL_FillRect(screen, &rect, light_red);
          drawnRectCount = MAX_RECTS;
        }
      }
    }
  }
}

void trs_hard_led(int drive, int on_off)
{
  static int countdown[4];
  int const led_pos = OrigWidth - border_width - 88;
  int i;
  SDL_Rect rect;

  rect.w = 16;
  rect.h = 4;
  rect.y = OrigHeight - rect.h;

  if (drive == -1) {
    for (i = 0; i < 4; i++) {
      if (on_off == -1)
        countdown[i] = 0;

      rect.x = led_pos + 24 * i;
      SDL_FillRect(screen, &rect, countdown[i] ? bright_red : light_red);
      drawnRectCount = MAX_RECTS;
    }
  }
  else if (on_off) {
    if (countdown[drive] == 0) {
      rect.x = led_pos + 24 * drive;
      SDL_FillRect(screen, &rect, bright_red);
      drawnRectCount = MAX_RECTS;
    }
    countdown[drive] = timer_hz / 2;
  }
  else {
    for (i = 0; i < 4; i++) {
      if (countdown[i]) {
        countdown[i]--;
        if (countdown[i] == 0) {
          rect.x = led_pos * i;
          SDL_FillRect(screen, &rect, light_red);
          drawnRectCount = MAX_RECTS;
        }
      }
    }
  }
}

void trs_turbo_led(void)
{
  SDL_Rect rect;

  rect.w = 16;
  rect.h = 4;
  rect.x = (OrigWidth - border_width) / 2 - 8;
  rect.y = OrigHeight - rect.h;

  SDL_FillRect(screen, &rect, timer_overclock ? bright_orange : light_orange);
  drawnRectCount = MAX_RECTS;
}

void trs_screen_write_char(unsigned int position, Uint8 char_index)
{
  unsigned int row, col;
  int expanded = (currentmode & EXPANDED) != 0;
  SDL_Rect srcRect, dstRect;

  if (position >= screen_chars)
    return;

  trs_screen[position] = char_index;

  if (expanded && (position & 1))
    return;

  if (grafyx_enable && !grafyx_overlay)
    return;

  if (row_chars == 64) {
    row = position / 64;
    col = position - (row * 64);
  } else {
    row = position / 80;
    col = position - (row * 80);
  }

  srcRect.x = 0;
  srcRect.y = 0;
  srcRect.w = cur_char_width;
  srcRect.h = cur_char_height;
  dstRect.x = srcRect.w * col + left_margin;
  dstRect.y = srcRect.h * row + top_margin;

  if (expanded)
    srcRect.w *= 2;

  if (genie3s) {
    SDL_BlitSurface(trs_char[expanded][char_index], &srcRect, screen, &dstRect);
  } else {
    if (trs_model == 1 && (trs_clones.model & (CT80 | EG3200)) == 0) {
      /* On Model I, 0xc0-0xff is another copy of 0x80-0xbf */
      if (char_index >= 0xc0)
        char_index -= 0x40;
    }
    if (!(currentmode & INVERSE) && char_index >= 0x80 && char_index <= 0xbf) {
      SDL_BlitSurface(trs_box[expanded][char_index - 0x80], &srcRect, screen, &dstRect);
    } else {
      if (trs_model > 1 && char_index >= 0xc0 &&
          (currentmode & (ALTERNATE + INVERSE)) == 0) {
        char_index -= 0x40;
      }
      if ((currentmode & INVERSE) && (char_index & 0x80)) {
        expanded += 2;
        char_index &= 0x7f;
      }
      SDL_BlitSurface(trs_char[expanded][char_index], &srcRect, screen, &dstRect);
    }
  }

  /* Overlay grafyx on character */
  if (grafyx_enable) {
    /* assert(grafyx_overlay); */
    int const srcx = ((col + grafyx_xoffset) % G_XSIZE) * cur_char_width;
    int const srcy = (row * cur_char_height + grafyx_yoffset * scale_factor)
        % (G_YSIZE * scale_factor);
    int const duny = (G_YSIZE * scale_factor) - srcy;

    srcRect.x = srcx;
    srcRect.y = srcy;
    TrsSoftBlit(image, &srcRect, screen, &dstRect, 1);

    /* Draw wrapped portion if any */
    if (duny < cur_char_height) {
      srcRect.y = 0;
      srcRect.h -= duny;
      dstRect.y += duny;
      TrsSoftBlit(image, &srcRect, screen, &dstRect, 1);
    }
  }

  drawnRectCount = MAX_RECTS;
}

void trs_screen_update(void)
{
  SDL_UpdateTexture(texture, NULL, screen->pixels, screen->pitch);
  SDL_RenderClear(render);
  SDL_RenderCopy(render, texture, NULL, NULL);
  SDL_RenderPresent(render);
}

static void trs_screen_rect(int x, int y, int w, int h, int color)
{
  SDL_Rect rect;

  rect.x = x;
  rect.y = y;
  rect.w = w;
  rect.h = h;

  SDL_FillRect(screen, &rect, SDL_MapRGB(screen->format,
#if defined(big_endian) && !defined(__linux)
      (color & 0xFF),
      (color >> 8) & 0xFF,
      (color >> 16) & 0xFF));
#else
      (color >> 16) & 0xFF,
      (color >> 8) & 0xFF,
      (color & 0xFF)));
#endif
}

void trs_gui_clear_rect(int x, int y, int w, int h, int frame)
{
  /* Add offsets to center */
  if (col_chars != 16) {
    x += (row_chars - 64) / 2;
    y += (col_chars - 16) / 2;
  } else if (hrg_enable == 2) {
    x += 8;
  }

  x = x * cur_char_width + left_margin;
  y = y * cur_char_height + top_margin;
  w = w * cur_char_width;
  h = h * cur_char_height;


  if (frame) {
    trs_screen_rect(x, y, w, h, gui_foreground);
    x += 2;
    y += 2;
    w -= 4;
    h -= 4;
  }

  trs_screen_rect(x, y, w, h, gui_background);
}

void trs_gui_write_char(int col, int row, Uint8 char_index, int invert)
{
  SDL_Rect srcRect, dstRect;

  /* Add offsets to center */
  if (col_chars != 16) {
    col += (row_chars - 64) / 2;
    row += (col_chars - 16) / 2;
  } else if (hrg_enable == 2) {
    col += 8;
  }

  srcRect.x = 0;
  srcRect.y = 0;
  srcRect.w = cur_char_width;
  srcRect.h = cur_char_height;
  dstRect.x = srcRect.w * col + left_margin;
  dstRect.y = srcRect.h * row + top_margin;

  SDL_BlitSurface(trs_char[invert ? 5 : 4][char_index], &srcRect, screen, &dstRect);
}

static void grafyx_write_byte(int x, int y, Uint8 byte)
{
  if (grafyx_unscaled[y][x] != byte) {
    int const screen_x = ((x - grafyx_xoffset + G_XSIZE) % G_XSIZE);
    int const screen_y = ((y - grafyx_yoffset + G_YSIZE) % G_YSIZE);
    int const on_screen = (screen_x < row_chars && screen_y < col_chars
        * cur_char_height / scale_factor) || (hrg_enable == 2 && y < 192);
    int const position = (y * scale_factor) * G_XSIZE + x;
    SDL_Rect srcRect, dstRect;

    srcRect.w = cur_char_width;
    srcRect.h = scale_factor;
    srcRect.x = srcRect.w * x;
    srcRect.y = srcRect.h * y;
    dstRect.x = srcRect.w * screen_x + left_margin;
    dstRect.y = srcRect.h * screen_y + top_margin;

    if (grafyx_enable && grafyx_overlay && on_screen)
     /* Erase old byte, preserving text */
      TrsSoftBlit(image, &srcRect, screen, &dstRect, 1);

    /* Save new byte in local memory */
    grafyx_unscaled[y][x] = byte;
    grafyx[position] = byte;

    if (scale_factor == 2)
      grafyx[position + G_XSIZE] = byte;

    if (grafyx_enable && on_screen) {
      /* Draw new byte */
      TrsSoftBlit(image, &srcRect, screen, &dstRect, grafyx_overlay);
      drawnRectCount = MAX_RECTS;
    }
  }
}

void grafyx_write_x(int value)
{
  grafyx_x = value;
}

void grafyx_write_y(int value)
{
  /* Genie III VideoExtension HRG */
  if (eg3200 && grafyx_x & 0x80)
    value |= 1 << 8; /* Y MSB in X-Reg */

  grafyx_y = value;
}

void grafyx_write_data(int value)
{
  grafyx_write_byte(grafyx_x % G_XSIZE, grafyx_y, value);

  if (!(grafyx_mode & G_XNOCLKW)) {
    if (grafyx_mode & G_XDEC)
      grafyx_x--;
    else
      grafyx_x++;
  }

  if (!(grafyx_mode & G_YNOCLKW)) {
    if (grafyx_mode & G_YDEC)
      grafyx_y--;
    else
      grafyx_y++;
  }
}

int grafyx_read_data(void)
{
  int const value = grafyx_unscaled[grafyx_y][grafyx_x % G_XSIZE];

  if (!(grafyx_mode & G_XNOCLKR)) {
    if (grafyx_mode & G_XDEC)
      grafyx_x--;
    else
      grafyx_x++;
  }

  if (!(grafyx_mode & G_YNOCLKR)) {
    if (grafyx_mode & G_YDEC)
      grafyx_y--;
    else
      grafyx_y++;
  }

  return value;
}

void grafyx_write_mode(int value)
{
  int const old_enable = grafyx_enable;
  int const old_overlay = grafyx_overlay;

  grafyx_mode = value;
  grafyx_enable = value & G_ENABLE;

  if (eg3200) /* Genie III VideoExtension HRG */
    grafyx_overlay = grafyx_enable;
  else if (grafyx_microlabs)
    grafyx_overlay = (value & G_UL_NOTEXT) == 0;

  if (trs_model >= 3)
    trs_screen_640x240((grafyx_enable && !grafyx_overlay) || text80x24);

  if (old_enable != grafyx_enable ||
      (grafyx_enable && old_overlay != grafyx_overlay))
    trs_screen_refresh();
}

int grafyx_read_mode(void)
{
  /* Genie III VideoExtension HRG */
  return grafyx_mode;
}

void grafyx_write_xoffset(int value)
{
  int const old_xoffset = grafyx_xoffset;

  grafyx_xoffset = value % G_XSIZE;

  if (grafyx_enable && old_xoffset != grafyx_xoffset)
    trs_screen_refresh();
}

void grafyx_write_yoffset(int value)
{
  int const old_yoffset = grafyx_yoffset;

  grafyx_yoffset = value;

  if (grafyx_enable && old_yoffset != grafyx_yoffset)
    trs_screen_refresh();
}

void grafyx_write_overlay(int value)
{
  int const old_overlay = grafyx_overlay;

  grafyx_overlay = value & 1;

  if (grafyx_enable && old_overlay != grafyx_overlay) {
    trs_screen_640x240((grafyx_enable && !grafyx_overlay) || text80x24);
    trs_screen_refresh();
  }
}

/* Model III MicroLabs support */
void grafyx_m3_reset(void)
{
  if (grafyx_microlabs) grafyx_m3_write_mode(0);
}

void grafyx_m3_write_mode(int value)
{
  int const enable = (value & G3_ENABLE) != 0;
  int const changed = (enable != grafyx_enable);

  grafyx_enable = enable;
  grafyx_overlay = enable;
  grafyx_mode = value;
  grafyx_y = G3_YLOW(value);

  if (changed) trs_screen_refresh();
}

int grafyx_m3_write_byte(unsigned int position, int byte)
{
  if (grafyx_microlabs && (grafyx_mode & G3_COORD)) {
    grafyx_write_byte(position % 64, (position / 64) * 12 + grafyx_y, byte);
    return 1;
  } else
    return 0;
}

Uint8 grafyx_m3_read_byte(unsigned int position)
{
  if (grafyx_microlabs && (grafyx_mode & G3_COORD))
    return grafyx_unscaled[(position / 64) * 12 + grafyx_y][position % 64];
  else
    return trs_screen[position];
}

/*
 *     The Lowe Electronics LE18 is yet another fairly simple
 *     I/O based 384x192 graphics adapter writing 6bits per
 *     TRS80 character
 *
 *     Port EC (R)
 *     7: goes high for blanking - can spin until high to avoid noise
 *     6: on/off status
 *     5-0: pixel data bit 0 is left
 *
 *     Port ED (W)
 *     7-6: unused
 *     5-0: X position (chars)
 *     Port EE (W)
 *     7-0: Y position (lines)
 *
 *     Port EF (W)
 *     7-1: unused
 *     0: hi res (1 = on)
 */
void lowe_le18_write_x(int value)
{
  /* This really is 0-255. The unit has 16K x 6bit of RAM
     of which only 12K is displayed. You can use the rest
     as a 4K x 6bit area for .. not a lot really */
  le18_x = value & 63;
}

void lowe_le18_write_y(int value)
{
  le18_y = value;
}

static Uint8 pack8to6(Uint8 c)
{
  return ((c & 0x70) >> 1) | (c & 7);
}

static Uint8 expand6to8(Uint8 c)
{
  Uint8 r = (c & 0x07);

  if (r & 0x04)
    r |= 0x08;

  r |= (c << 1) & 0x70;

  if (r & 0x40)
    r |= 0x80;

  return r;
}

int lowe_le18_read(void)
{
  if (!lowe_le18)
    return 0xFF;

  return pack8to6(grafyx_unscaled[le18_y][le18_x]) | 0x80
      | ((le18_on) ? 0x40 : 0x00);
}

void lowe_le18_write_data(int value)
{
  grafyx_write_byte(le18_x, le18_y, expand6to8(value & 0x3F));
}

void lowe_le18_write_control(int value)
{
  if (lowe_le18 && ((le18_on ^ value) & 1)) {
    le18_on = value & 1;
    grafyx_enable = le18_on;
    grafyx_overlay = le18_on;
    trs_screen_refresh();
  }
}

/*
 * Support for Model I HRG1B 384*192 graphics card
 * (sold in Germany for Model I and Video Genie by RB-Elektronik).
 *
 * Assignment of ports is as follows:
 *    Port 0x00 (out): switch graphics screen off (value ignored).
 *    Port 0x01 (out): switch graphics screen on (value ignored).
 *    Port 0x02 (out): select screen memory address (LSB).
 *    Port 0x03 (out): select screen memory address (MSB).
 *    Port 0x04 (in):  read byte from screen memory.
 *    Port 0x05 (out): write byte to screen memory.
 * (The real hardware decodes only address lines A0-A2 and A7, so
 * that there are several "shadow" ports in the region 0x08-0x7d.
 * However, these undocumented ports are not implemented here.)
 *
 * The 16-bit memory address (port 2 and 3) is used for subsequent
 * read or write operations. It corresponds to a position on the
 * graphics screen, in the following way:
 *    Bits 0-5:   character column address (0-63)
 *    Bits 6-9:   character row address (0-15)
 *                (i.e. bits 0-9 are the "PRINT @" position.)
 *    Bits 10-13: address of line within character cell (0-11)
 *    Bits 14-15: not used
 *
 *      <----port 2 (LSB)---->  <-------port 3 (MSB)------->
 * Bit: 0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15
 *      <-column addr.->  <row addr>  <-line addr.->  <n.u.>
 *
 * Reading from port 4 or writing to port 5 will access six
 * neighbouring pixels corresponding (from left to right) to bits
 * 0-5 of the data byte. Bits 6 and 7 are present in memory, but
 * are ignored.
 *
 * In expanded mode (32 chars per line), the graphics screen has
 * only 192*192 pixels. Pixels with an odd column address (i.e.
 * every second group of 6 pixels) are suppressed.
 *
 * The LNW80 and later TCS models (Genie IIs/SpeedMaster) uses
 * the 480*192 screen resolution with 96*192 "extension region":
 *    Bits 0-3:   additional character column address (0-15)
 *    Bits 4-5:   MSB of line within character cell (0-11)
 *    Bits 6-9:   character row address (0-15)
 *    Bits 10-11: LSB of line within character cell (0-11)
 *    Bits 12-13: Always set to 1 in the "extension region"
 *    Bits 14-15: not used
 *
 * Bit: 0  1  2  3  4  5  6  7  8  9  10  11  12  13  14  15
 *      <-column->  <MSB> <row addr>  <LSB>   1   1   <n.u.>
 */

/* Switch HRG on (1 = 384*192, 2 = 480*192) or off (0). */
void
hrg_onoff(int enable)
{
  if (hrg_enable == enable) return; /* State does not change. */

  hrg_enable = enable;
  grafyx_enable = enable;
  grafyx_overlay = enable;

  if (speedup > 4) {
    memset(grafyx, 0, G_YSIZE * G_XSIZE);
    trs_screen_init(1);
  } else
    trs_screen_refresh();
}

/* Write address to latch. */
void
hrg_write_addr(int addr, int mask)
{
  hrg_addr = (hrg_addr & ~mask) | (addr & mask);
}

/* Write byte to HRG memory. */
void
hrg_write_data(int data)
{
  if (hrg_addr >= HRG_MEMSIZE) return; /* nonexistent address */

  hrg_screen[hrg_addr] = data;

  if (!hrg_enable) return;
  if ((currentmode & EXPANDED) && (hrg_addr & 1)) return;

  /* Finer HRG font on "boosted" LNW80 */
  if (hrg_enable == 2 && selector)
    data = mirror_bits(data);
  else
    data = mirror_bits(expand6to8(data));

  /* Check for 96*192 extension region */
  if ((hrg_addr & 0x3000) == 0x3000) {
    grafyx_write_byte(64 + (hrg_addr & 0x0F), ((hrg_addr >> 6) & 0x0F) * 12
        + (4 * ((hrg_addr >> 4) & 0x03) + ((hrg_addr >> 10) & 0x03)), data);
  } else { /* 384*192 inner region */
    grafyx_write_byte((hrg_addr & 0x3FF) % 64, ((hrg_addr & 0x3FF) / 64) * 12
        + (hrg_addr >> 10), data); /* bits 0-9: "PRINT @" position */
  }
}

/* Read byte from HRG memory. */
int
hrg_read_data(void)
{
  if (hrg_addr >= HRG_MEMSIZE) return 0xff; /* nonexistent address */

  return hrg_screen[hrg_addr];
}

void m6845_cursor(unsigned int position, int start, int end, int visible)
{
  int expanded = (currentmode & EXPANDED) != 0;
  unsigned int row, col;
  int cur_char;
  SDL_Rect srcRect, dstRect;

  if (position >= screen_chars)
    return;

  cur_char = trs_screen[position];

  if (visible == 0) {
    trs_screen_write_char(position, cur_char);
    return;
  }

  if (row_chars == 64) {
    row = position / 64;
    col = position - (row * 64);
  } else {
    row = position / 80;
    col = position - (row * 80);
  }

  srcRect.x = 0;
  srcRect.y = start * scale_factor;
  srcRect.w = cur_char_width;
  srcRect.h = (end - start) * scale_factor + scale_factor;
  dstRect.x = srcRect.w * col + left_margin;
  dstRect.y = srcRect.y + row * cur_char_height + top_margin;

  if (expanded)
    srcRect.w *= 2;

  expanded += (currentmode & INVERSE) && (cur_char & 0x80) ? 0 : 2;

  SDL_BlitSurface(trs_char[expanded][cur_char], &srcRect, screen, &dstRect);
  drawnRectCount = MAX_RECTS;
}

void m6845_screen(int chars, int lines, int raster, int factor)
{
  int changed = 0;

  if (chars && (changed = (chars != row_chars)))
    row_chars = chars;

  if (lines && (changed = (lines != col_chars)))
    col_chars = lines;

  if (raster && (changed = (raster != m6845_raster)))
    m6845_raster = raster;

  if (factor && (changed = (factor != scale_factor)))
    scale_factor = factor;

  if (changed) {
    if (screen_chars < SCREEN_SIZE)
      memset(&trs_screen[screen_chars], ' ', SCREEN_SIZE - screen_chars - 1);

    screen_chars = row_chars * col_chars;
    if (screen_chars > SCREEN_SIZE)
      screen_chars = SCREEN_SIZE;

    trs_screen_init(1);
  }
}

void genie3s_char(int char_index, int scanline, int byte)
{
  char_ram[char_index][scanline] = eg3200 ? mirror_bits(byte) : byte;

  if (scanline == (m6845_raster - 1)) {
    bitmap_char(char_index, 1);

    if (eg3200)
      trs_screen_refresh();
  }
}

void genie3s_hrg(int value)
{
  int const changed = (value != grafyx_enable);

  grafyx_enable = value;
  grafyx_overlay = value;

  if (changed) trs_screen_refresh();
}

void genie3s_hrg_write(unsigned int position, int byte)
{
  unsigned int pos, row, col;

  if ((currentmode & EXPANDED) && (position & 1)) return;

  if (row_chars == 64) {
    pos = position & (screen_chars - 1);
    row = pos / 64;
    col = pos - (row * 64);
  } else {
    pos = position & 0x7FF;
    row = pos / 80;
    col = pos - (row * 80);
  }

  grafyx_write_byte(col, row * m6845_raster + (position >> 11),
      mirror_bits(byte));
}

Uint8 genie3s_hrg_read(unsigned int position)
{
  unsigned int pos, row, col;

  if (row_chars == 64) {
    pos = position & (screen_chars - 1);
    row = pos / 64;
    col = pos - (row * 64);
  } else {
    pos = position & 0x7FF;
    row = pos / 80;
    col = pos - (row * 80);
  }

  return mirror_bits(grafyx_unscaled[row * m6845_raster
      + (position >> 11)][col]);
}

void trs_get_mouse_pos(int *x, int *y, unsigned int *buttons)
{
  int win_x, win_y;
  Uint8 const mask = SDL_GetMouseState(&win_x, &win_y);

#if MOUSEDEBUG
  debug("get_mouse %d %d 0x%x ->", win_x, win_y, mask);
#endif
  if (win_x >= 0 && win_x < OrigWidth &&
      win_y >= 0 && win_y < OrigHeight) {
    /* Mouse is within emulator window */
    if (win_x < left_margin) win_x = left_margin;
    if (win_x >= OrigWidth - left_margin) win_x = OrigWidth - left_margin - 1;
    if (win_y < top_margin) win_y = top_margin;
    if (win_y >= OrigHeight - top_margin) win_y = OrigHeight - top_margin - 1;
    *x = mouse_last_x = (win_x - left_margin)
      * mouse_x_size
      / (OrigWidth - 2 * left_margin);
    *y = mouse_last_y = (win_y - top_margin)
      * mouse_y_size
      / (OrigHeight - 2 * top_margin);
    mouse_last_buttons = 7;
    /* !!Note: assuming 3-button mouse */
    if (mask & SDL_BUTTON(SDL_BUTTON_LEFT))   mouse_last_buttons &= ~4;
    if (mask & SDL_BUTTON(SDL_BUTTON_MIDDLE)) mouse_last_buttons &= ~2;
    if (mask & SDL_BUTTON(SDL_BUTTON_RIGHT))  mouse_last_buttons &= ~1;
  }

  *x = mouse_last_x;
  *y = mouse_last_y;
  *buttons = mouse_last_buttons;
#if MOUSEDEBUG
  debug("%d %d 0x%x\n",
      mouse_last_x, mouse_last_y, mouse_last_buttons);
#endif
}

void trs_set_mouse_pos(int x, int y)
{
  if (x == mouse_last_x && y == mouse_last_y) {
    /* Kludge: Ignore warp if it says to move the mouse to where we
       last said it was. In general someone could really want to do that,
       but with MDRAW, gratuitous warps to the last location occur frequently.
    */
    return;
  } else {
    int const dest_x = left_margin + x * (OrigWidth - 2 * left_margin) / mouse_x_size;
    int const dest_y = top_margin  + y * (OrigHeight - 2 * top_margin) / mouse_y_size;

#if MOUSEDEBUG
    debug("set_mouse %d %d -> %d %d\n", x, y, dest_x, dest_y);
#endif
    SDL_WarpMouseInWindow(window, dest_x, dest_y);
  }
}

void trs_get_mouse_max(int *x, int *y, unsigned int *sens)
{
  *x = mouse_x_size - (mouse_old_style ? 0 : 1);
  *y = mouse_y_size - (mouse_old_style ? 0 : 1);
  *sens = mouse_sens;
}

void trs_set_mouse_max(int x, int y, unsigned int sens)
{
  if ((x & 1) == 0 && (y & 1) == 0) {
    /* "Old style" mouse drivers took the size here; new style take
       the maximum. As a heuristic kludge, we assume old style if
       the values are even, new style if not. */
    mouse_old_style = 1;
  }

  mouse_x_size = x + (mouse_old_style ? 0 : 1);
  mouse_y_size = y + (mouse_old_style ? 0 : 1);
  mouse_sens = sens;
}

void trs_main_save(FILE *file)
{
  int i;

  trs_save_uint8(file, trs_screen, SCREEN_SIZE);
  trs_save_uint16(file, &screen_chars, 1);
  trs_save_int(file, &col_chars, 1);
  trs_save_int(file, &row_chars, 1);
  trs_save_int(file, &currentmode, 1);
  trs_save_int(file, &m6845_raster, 1);
  trs_save_int(file, &scale_factor, 1);
  trs_save_int(file, &text80x24, 1);
  trs_save_int(file, &screen640x240, 1);
  trs_save_int(file, &trs_charset, 1);
  trs_save_int(file, &trs_charset1, 1);
  trs_save_int(file, &trs_charset3, 1);
  trs_save_int(file, &trs_charset4, 1);

  for (i = 0; i < MAX_CHARS; i++)
    trs_save_uint8(file, char_ram[i], MAX_CHAR_HEIGHT);

  for (i = 0; i < G_YSIZE; i++)
    trs_save_uint8(file, grafyx_unscaled[i], G_XSIZE);

  trs_save_int(file, &grafyx_x, 1);
  trs_save_int(file, &grafyx_y, 1);
  trs_save_int(file, &grafyx_mode, 1);
  trs_save_int(file, &grafyx_enable, 1);
  trs_save_int(file, &grafyx_overlay, 1);
  trs_save_int(file, &grafyx_xoffset, 1);
  trs_save_int(file, &grafyx_yoffset, 1);
  trs_save_uint8(file, hrg_screen, HRG_MEMSIZE);
  trs_save_int(file, &hrg_enable, 1);
  trs_save_int(file, &hrg_addr, 1);
  trs_save_int(file, &lowe_le18, 1);
  trs_save_uint8(file, &le18_on, 1);
  trs_save_uint8(file, &le18_x, 1);
  trs_save_uint8(file, &le18_y, 1);
}

void trs_main_load(FILE *file)
{
  int i;

  trs_load_uint8(file, trs_screen, SCREEN_SIZE);
  trs_load_uint16(file, &screen_chars, 1);
  trs_load_int(file, &col_chars, 1);
  trs_load_int(file, &row_chars, 1);
  trs_load_int(file, &currentmode, 1);
  trs_load_int(file, &m6845_raster, 1);
  trs_load_int(file, &scale_factor, 1);
  trs_load_int(file, &text80x24, 1);
  trs_load_int(file, &screen640x240, 1);
  trs_load_int(file, &trs_charset, 1);
  trs_load_int(file, &trs_charset1, 1);
  trs_load_int(file, &trs_charset3, 1);
  trs_load_int(file, &trs_charset4, 1);

  for (i = 0; i < MAX_CHARS; i++)
    trs_load_uint8(file, char_ram[i], MAX_CHAR_HEIGHT);

  for (i = 0; i < G_YSIZE; i++)
    trs_load_uint8(file, grafyx_unscaled[i], G_XSIZE);

  trs_load_int(file, &grafyx_x, 1);
  trs_load_int(file, &grafyx_y, 1);
  trs_load_int(file, &grafyx_mode, 1);
  trs_load_int(file, &grafyx_enable, 1);
  trs_load_int(file, &grafyx_overlay, 1);
  trs_load_int(file, &grafyx_xoffset, 1);
  trs_load_int(file, &grafyx_yoffset, 1);
  trs_load_uint8(file, hrg_screen, HRG_MEMSIZE);
  trs_load_int(file, &hrg_enable, 1);
  trs_load_int(file, &hrg_addr, 1);
  trs_load_int(file, &lowe_le18, 1);
  trs_load_uint8(file, &le18_on, 1);
  trs_load_uint8(file, &le18_x, 1);
  trs_load_uint8(file, &le18_y, 1);
}

int trs_sdl_savebmp(const char *filename)
{
  SDL_Surface *buffer = SDL_CreateRGBSurface(SDL_SWSURFACE,
      OrigWidth, screen_height, 32,
#if defined(big_endian) && !defined(__linux)
      0x000000ff, 0x0000ff00, 0x00ff0000, 0);
#else
      0x00ff0000, 0x0000ff00, 0x000000ff, 0);
#endif

  if (buffer) {
    trs_screen_refresh();
    SDL_BlitSurface(screen, NULL, buffer, NULL);

    if (SDL_SaveBMP(buffer, filename) == 0) {
      SDL_FreeSurface(buffer);
      return 0;
    }

    SDL_FreeSurface(buffer);
    file_error("save Screenshot '%s'", filename);
  }
  return -1;
}
