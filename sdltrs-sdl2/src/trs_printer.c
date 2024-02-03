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

#include <sys/stat.h>
#include "error.h"
#include "trs.h"

int trs_printer = 0;

static FILE *printer;
static char printer_filename[FILENAME_MAX];
static int printer_open = 0;

int trs_printer_reset(void)
{
  if (printer_open) {
    fclose(printer);
    printer_open = 0;
    return 0;
  } else
    return -1;
}

static void trs_printer_open(void)
{
  int file_num;
  struct stat st = { 0 };

  for (file_num = 0; file_num < 10000; file_num++) {
    if (snprintf(printer_filename, FILENAME_MAX, "%s%ctrsprn%04d.txt",
        trs_printer_dir, DIR_SLASH, file_num) < FILENAME_MAX) {
      if (stat(printer_filename, &st) < 0) {
        printer_open = 1;
        printer = fopen(printer_filename,"w");
        if (printer == NULL) {
          file_error("open printer output file '%s'", printer_filename);
          printer_open = 0;
        }
        return;
      }
    }
  }
}

void trs_printer_write(int value)
{
  if (trs_printer) {
    if (!printer_open)
      trs_printer_open();

    if (printer_open) {
      if (value == 0x0D) {
        fputc('\n',printer);
      } else {
        fputc(value,printer);
      }
    }
  }
}

int trs_printer_read(void)
{
    return 0x30;	/* printer selected, ready, with paper, not busy */
}
