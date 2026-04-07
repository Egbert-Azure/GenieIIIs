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

#ifdef SDL2
#include <stdlib.h>
#include <string.h>
#include <SDL.h>
#include <SDL_clipboard.h>

int  PasteChar(Uint8 *character);
int  PasteStart(void);
void PasteCopy(const char *string);

/* Emulator specific variables */
static int charCount;
static char *pasteString;
static int pasteStringLength;

int PasteChar(Uint8 *character)
{
  if (charCount) {
    *character = pasteString[pasteStringLength - charCount];
    charCount--;
    if (charCount)
      return 1;
  }
  return 0;
}

int PasteStart(void)
{ 
  pasteString = SDL_GetClipboardText();
  pasteStringLength = strlen(pasteString);
  charCount = pasteStringLength;

  if (charCount) {
    return 1;
  } else {
    free(pasteString);
    return 0;
  }
}


void PasteCopy(const char *string)
{
  SDL_SetClipboardText(string);
}

#elif _WIN32
#include <stdio.h>
#include <SDL_types.h>

#include "windows.h"

int  PasteChar(Uint8 *character);
int  PasteStart(void);
void PasteCopy(const char *string);

static int charCount;
static char *pasteString;
static int pasteStringLength;
static HANDLE hClipboardData;

int PasteChar(Uint8 *character)
{
  if (charCount) {
    *character = pasteString[pasteStringLength - charCount];
    charCount--;
    if (charCount)
      return 1;
    else {
      GlobalUnlock(hClipboardData);
      CloseClipboard();
    }
  }

  return 0;
}

int PasteStart(void)
{
  if (IsClipboardFormatAvailable(CF_TEXT)) {
    if (OpenClipboard(NULL)) {
      hClipboardData = GetClipboardData(CF_TEXT);
      pasteString = (char *)GlobalLock(hClipboardData);
      charCount = pasteStringLength = strlen(pasteString);
      return 1;
    }
  }

  pasteString = NULL;
  charCount = 0;
  return 0;
}

void PasteCopy(const char *string)
{
  HANDLE hCopyData;
  char *pchData;

  if (OpenClipboard(NULL)) {
    EmptyClipboard();
    hCopyData = GlobalAlloc(GMEM_DDESHARE, strlen(string) + 1);
    pchData = (char *)GlobalLock(hCopyData);
    snprintf(pchData, strlen(string), "%s", string);
    GlobalUnlock(hCopyData);
    SetClipboardData(CF_TEXT, hCopyData);
    CloseClipboard();
  }
}

#elif !NOX
#ifndef SDL_VIDEO_DRIVER_X11
#define SDL_VIDEO_DRIVER_X11 1
#endif

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <limits.h>
#include <time.h>
#include <X11/Xlib.h>
#include <X11/Xutil.h>
#include <SDL.h>
#include <SDL_syswm.h>
#include "error.h"

int  PasteChar(Uint8 *character);
int  PasteStart(void);
void PasteCopy(const char *string);

/* forward declarations */
static int init_scrap(void);
static int lost_scrap(void);
static void put_scrap(int srclen, const char *src);
static void get_scrap(int *dstlen, char **dst);
/* The system message filter function -- handle clipboard messages */
static int clipboard_filter(const SDL_Event *event);

/* SDL and X variables */
static Display *SDL_Display;
static Window SDL_Screen;
static void (*Lock_Display)(void);
static void (*Unlock_Display)(void);
static Atom _atom_CLIPBOARD;
static Atom _atom_SDL;
static Atom _atom_UTF8;
static Atom _atom_TEXT;
static Atom _atom_COMPOUND;
static Time _cliptime = CurrentTime;
static char *clipboard;

/* Maximum size to send or receive per request. */
#define MIN(a,b) (((a) < (b)) ? a : b)
#define MAX_CHUNK_SIZE(display)                                    \
    MIN(262144, /* 65536 * 4 */                                    \
        (XExtendedMaxRequestSize (display)) == 0                   \
        ? XMaxRequestSize (display) - 100                          \
        : XExtendedMaxRequestSize (display) - 100)

/* Emulator specific variables */
static int charCount;
static char *pasteString;
static int pasteStringLength;
static int firstTime = 1;

static int init_scrap(void)
{
  SDL_SysWMinfo info;

  /* Grab the window manager specific information */
  SDL_SetError("SDL is not running on known window manager");

  SDL_VERSION(&info.version);
  if (SDL_GetWMInfo(&info)) {
    /* Save the information for later use */
    if ( info.subsystem == SDL_SYSWM_X11 ) {
      SDL_Display = info.info.x11.display;
      SDL_Screen = info.info.x11.window;
      Lock_Display = info.info.x11.lock_func;
      Unlock_Display = info.info.x11.unlock_func;
      _atom_CLIPBOARD = XInternAtom(SDL_Display, "CLIPBOARD", False);
      _atom_UTF8 = XInternAtom(SDL_Display, "UTF8_STRING", False);
      _atom_TEXT = XInternAtom(SDL_Display, "TEXT", False);
      _atom_COMPOUND = XInternAtom(SDL_Display, "COMPOUND_TEXT", False);
      _atom_SDL = XInternAtom(SDL_Display, "SDL_SELECTION", False);

      /* Enable the special window hook events */
      SDL_EventState(SDL_SYSWMEVENT, SDL_ENABLE);
      SDL_SetEventFilter(clipboard_filter);
      return 0;
    }
    else {
      SDL_SetError("SDL is not running on X11");
    }

  }
  return -1;
}

static int lost_scrap(void)
{
  int retval;

  Lock_Display();
  retval = (XGetSelectionOwner(SDL_Display, _atom_CLIPBOARD) != SDL_Screen);
  Unlock_Display();

  return(retval);
}

static void put_scrap(int srclen, const char *src)
{
  char *dst;

  if ((dst = (char *)malloc(srclen)) == NULL)
    fatal("put_scrap: failed to allocate memory");
  strcpy(dst, src);
  if (clipboard)
    free(clipboard);
  clipboard = dst;
  Lock_Display();

  XChangeProperty(SDL_Display, DefaultRootWindow(SDL_Display),
                  _atom_CLIPBOARD, XA_STRING, 8, PropModeReplace,
                  (Uint8 *)dst, srclen);

  XSync (SDL_Display, False);
  if (lost_scrap()) {
    XSetSelectionOwner(SDL_Display, _atom_CLIPBOARD, SDL_Screen, CurrentTime);
  }
  Unlock_Display();
}

static Window get_scrap_owner (Atom *selection)
{
    int i = 0;
    static Atom buffers[] = { XA_PRIMARY, XA_SECONDARY, XA_CUT_BUFFER0,
                              XA_CUT_BUFFER1, XA_CUT_BUFFER2, XA_CUT_BUFFER3,
                              XA_CUT_BUFFER4, XA_CUT_BUFFER5, XA_CUT_BUFFER6,
                              XA_CUT_BUFFER7 };

    Window owner = XGetSelectionOwner(SDL_Display, *selection);
    if (owner != None)
        return owner;

    owner = XGetSelectionOwner(SDL_Display, _atom_CLIPBOARD);
    if (owner != None)
        return owner;

    while (i < 10) {
        owner = XGetSelectionOwner(SDL_Display, buffers[i]);
        if (owner != None) {
            *selection = buffers[i];
            return owner;
        }
        i++;
    }

    return None;
}

static void get_scrap(int *dstlen, char **dst)
{
  Window owner;
  Atom source;
  time_t start;
  Atom sel_type;
  int sel_format;
  unsigned long nbytes;
  unsigned long overflow;
  Uint8 *src;
  char *retval = NULL;
  unsigned long length;
  int step = 1;
  XEvent ev;
  Time timestamp;

  *dstlen = 0;

  /* If we are the owner, simply return the clip buffer, if it matches
   * the request type.
   */
  if (!lost_scrap ()) {
    *dstlen = strlen(clipboard);
    *dst = strdup(clipboard);
    if (*dst == NULL)
      *dstlen = 0;
    return;
  }

  Lock_Display();

  /* Find a selection owner. */
  source = _atom_CLIPBOARD;
  owner = get_scrap_owner(&source);
  if (owner == None) {
    retval = NULL;
    goto out;
  }

  timestamp = _cliptime;

  /* Copy and convert the selection into our SDL_SELECTION atom of the
   * window. 
   * Flush afterwards, so we have an immediate effect and do not receive
   * the old buffer anymore.
   */
  XConvertSelection(SDL_Display, source, XA_STRING, _atom_SDL, SDL_Screen,
                    timestamp);
  XSync(SDL_Display, False);

  /* Let's wait for the SelectionNotify event from the callee and
   * react upon it as soon as it is received.
   */
  for (start = time (0) ;;) {
      if (XCheckTypedWindowEvent(SDL_Display, SDL_Screen,
                                 SelectionNotify, &ev))
          break;
      if (time (0) - start >= 5) {
        retval = NULL;
        goto out;
      }
  }

  /* Get any property type and check the sel_type afterwards to decide
   * what to do.
   */
  if (XGetWindowProperty(SDL_Display, ev.xselection.requestor,
                         _atom_SDL, 0, 0, True,
                         AnyPropertyType, &sel_type, &sel_format,
                         &nbytes, &overflow, &src) != Success)
  {
    XFree(src);
    retval = NULL;
    goto out;
  }

  /* Any property type of
   * XA_STRING, UTF8_STRING and TEXT is valid.
   */
  if (sel_type != _atom_UTF8 && sel_type != _atom_TEXT
      && sel_type != XA_STRING) {
    /* No matching text type found. Return nothing then. */
    XFree(src);
    retval = NULL;
    goto out;
  }

  /* Anything is fine, so copy the buffer and return it. */
  switch (sel_format)
  {
    case 16:
      step = sizeof (short) / 2;
      break;
    case 32:
      step = sizeof (long) / 4;
      break;
    case 8:
    default:
      step = sizeof (char);
      break;
    }

  /* X11 guarantees NULL termination, add an extra byte. */
  length = (step * overflow) + 1;
  retval = malloc (length);
  if (retval) {
    unsigned long boffset = 0;
    unsigned long offset = 0;
    unsigned long chunk = MAX_CHUNK_SIZE(SDL_Display);

    memset (retval, 0, (size_t)(length));

    /* Read as long as there is data. */
    while (overflow) {
      if (XGetWindowProperty(SDL_Display, ev.xselection.requestor,
                             _atom_SDL, offset, chunk, True,
                             AnyPropertyType, &sel_type, &sel_format,
                             &nbytes, &overflow, &src) != Success)
      {
        break;
      }

      offset += nbytes / (32 / sel_format);
      nbytes *= step * sel_format / 8;
      memcpy(retval + boffset, src, nbytes);
      boffset += nbytes;
      XFree(src);
    }
  }
  else
  {
     /* ENOMEM */
  }

out:
  Unlock_Display();

  if (retval) {
    *dst = retval;
    *dstlen = strlen(retval);
  } else {
    *dst = NULL;
    *dstlen = 0;
  }
  return;
}

static int clipboard_filter(const SDL_Event *event)
{
  /* Post all non-window manager specific events */
  if (event->type != SDL_SYSWMEVENT) {
    return 1;
  }

  /* Handle window-manager specific clipboard events */
  switch (event->syswm.msg->event.xevent.type) {
    /* Copy the selection from clipboard to the requested property */
    case SelectionRequest: {
      XSelectionRequestEvent *req;
      XEvent sevent;
      int seln_format;
      unsigned long nbytes;
      unsigned long overflow;
      Uint8 *seln_data;

      req = &event->syswm.msg->event.xevent.xselectionrequest;
      sevent.xselection.type = SelectionNotify;
      sevent.xselection.display = req->display;
      sevent.xselection.selection = req->selection;
      sevent.xselection.target = None;
      sevent.xselection.property = None;
      sevent.xselection.requestor = req->requestor;
      sevent.xselection.time = req->time;
      if (XGetWindowProperty(SDL_Display, DefaultRootWindow(SDL_Display),
                             _atom_CLIPBOARD, 0, INT_MAX / 4, False, req->target,
                             &sevent.xselection.target, &seln_format,
                             &nbytes, &overflow, &seln_data) == Success)
        {
          if (sevent.xselection.target == req->target) {
              if (sevent.xselection.target == XA_STRING) {
                  if (seln_data[nbytes - 1] == '\0')
                    --nbytes;
                }
              XChangeProperty(SDL_Display, req->requestor, req->property,
                              sevent.xselection.target, seln_format, PropModeReplace,
                              seln_data, nbytes);
              sevent.xselection.property = req->property;
            }
          XFree(seln_data);
        }
      XSendEvent(SDL_Display, req->requestor, False, 0, &sevent);
      XSync(SDL_Display, False);
    }
    break;
  }

  /* Post the event for X11 clipboard reading above */
  return 1;
}

int PasteChar(Uint8 *character)
{
  if (charCount) {
    *character = pasteString[pasteStringLength - charCount];
    charCount--;
    if (charCount)
      return 1;
  }

  free(pasteString);
  return 0;
}

int PasteStart(void)
{
  if (firstTime) {
     if (init_scrap() == -1) {
       charCount = 0;
       return 0;
     }
     firstTime = 0;
  }

  get_scrap(&pasteStringLength, &pasteString);

  charCount = pasteStringLength;
  if (charCount) {
    return 1;
  } else {
    free(pasteString);
    return 0;
  }
}

void PasteCopy(const char *string)
{
  if (firstTime) {
     if (init_scrap() == -1) {
       return;
     }
     firstTime = 0;
  }

  put_scrap(strlen(string) + 1, string);
}
#endif
