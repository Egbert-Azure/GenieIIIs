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

#include <stdlib.h>
#include <string.h>
#include <SDL.h>
#include <SDL_clipboard.h>

int  PasteManagerGetChar(Uint8 *character);
int  PasteManagerStartPaste(void);
void PasteManagerStartCopy(const char *string);

/* Emulator specific variables */
static int charCount;
static char *pasteString;
static int pasteStringLength;

int PasteManagerGetChar(Uint8 *character)
{
  if (charCount) {
    *character = pasteString[pasteStringLength - charCount];
    charCount--;
    if (charCount)
      return 1;
  }
  return 0;
}

int PasteManagerStartPaste(void)
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


void PasteManagerStartCopy(const char *string)
{
  SDL_SetClipboardText(string);
}
