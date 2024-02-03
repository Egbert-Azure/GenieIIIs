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

/*
 * mkdisk.c
 * Make a blank (unformatted) emulated floppy or hard drive in a file,
 * or write protect/unprotect an existing one.
 */

#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>

#include "error.h"
#include "reed.h"
#include "trs_cassette.h"
#include "trs_disk.h"
#include "trs_hard.h"
#include "trs_mkdisk.h"
#include "trs_stringy.h"

#define DISK  3
#define HARD  4
#define WAFER 5
#define CASS  6

#ifdef _WIN32
#include "wtypes.h"
#include "winnt.h"

static void win_set_readonly(const char *filename, int readonly)
{
  DWORD attr = GetFileAttributes(filename);
  SetFileAttributes(filename, readonly != 0
                    ? (attr | FILE_ATTRIBUTE_READONLY)
                    : (attr & ~FILE_ATTRIBUTE_READONLY));
}
#endif

int trs_write_protect(int type, int drive)
{
  char prot_filename[FILENAME_MAX];
  const char *filename = NULL;
  FILE *f = NULL;
  int emutype = 0;
  int writeprot = 0;
  int newmode;
  struct stat st = { 0 };

  switch (type) {
    case DISK:
      filename = trs_disk_getfilename(drive);
      break;
    case HARD:
      filename = trs_hard_getfilename(drive);
      break;
    case WAFER:
      filename = stringy_get_name(drive);
      break;
    case CASS:
      filename = trs_cassette_getfilename();
      break;
    default:
      return 0;
  }
  if (filename[0] == 0)
    return 0;

  if (stat(filename, &st) < 0) {
    file_error("'%s'", filename);
    return -1;
  }

  snprintf(prot_filename, FILENAME_MAX, "%s", filename);
#ifdef _WIN32
  win_set_readonly(prot_filename, 0);
#else
  if (chmod(prot_filename, st.st_mode | (S_IWUSR|S_IWGRP|S_IWOTH)) < 0) {
    file_error("chmod '%s'", prot_filename);
    return -1;
  }
#endif
  switch (type) {
    case DISK:
      emutype = trs_disk_getdisktype(drive);
      writeprot = !trs_disk_getwriteprotect(drive);
      trs_disk_remove(drive);

      if (emutype == JV3 || emutype == DMK) {
        f = fopen(prot_filename, "r+");
        if (f != NULL) {
          /* Set the magic byte */
          if (emutype == JV3) {
            fseek(f, 256 * 34 - 1, 0);
            putc(writeprot ? 0 : 0xff, f);
          } else {
            putc(writeprot ? 0xff : 0, f);
          }
        }
        fclose(f);
      }
      break;
    case HARD:
      writeprot = !trs_hard_getwriteprotect(drive);
      trs_hard_remove(drive);

      f = fopen(prot_filename, "r+");
      if (f != NULL) {
        fseek(f, 7, 0);
        newmode = getc(f);
        if (newmode != EOF) {
          newmode = (newmode & 0x7f) | (writeprot ? 0x80 : 0);
          fseek(f, 7, 0);
          putc(newmode, f);
        }
        fclose(f);
      }
      break;
    case WAFER:
      writeprot = !stringy_get_writeprotect(drive);
      stringy_remove(drive);

      f = fopen(prot_filename, "r+");
      if (f != NULL) {
        fseek(f, 5, 0);
        newmode = getc(f);
        if (newmode != EOF) {
          if (writeprot)
            newmode |= 1 << 0;
          else
            newmode &= ~(1 << 0);
          fseek(f, 5, 0);
          putc(newmode, f);
        }
        fclose(f);
      }
      break;
    case CASS:
      writeprot = !trs_cass_getwriteprotect();
      trs_cassette_remove();
      break;
    default:
      break;
  }
#ifdef _WIN32
  win_set_readonly(prot_filename, writeprot);
#else
  if (writeprot) {
    newmode = st.st_mode & ~S_IWUSR;
  } else {
    int const oumask = umask(0);

    umask(oumask);
    newmode = st.st_mode |
      (( (st.st_mode & (S_IRUSR|S_IRGRP|S_IROTH)) >> 1 ) & ~oumask);
  }
  if (chmod(prot_filename, newmode) < 0) {
    file_error("chmod '%s'", prot_filename);
    return -1;
  }
#endif
  switch (type) {
    case DISK:
      trs_disk_insert(drive, prot_filename);
      break;
    case HARD:
      trs_hard_attach(drive, prot_filename);
      break;
    case WAFER:
      stringy_insert(drive, prot_filename);
      break;
    case CASS:
      trs_cassette_insert(prot_filename);
      break;
    default:
      break;
  }
  return 0;
}

int trs_create_blank_jv1(const char *fname)
{
  FILE *f = fopen(fname, "wb");

  /* Unformatted JV1 disk - just an empty file! */
  if (f == NULL) {
    file_error("create JV1 disk '%s'", fname);
    return -1;
  }

  fclose(f);
  return 0;
}

int trs_create_blank_jv3(const char *fname)
{
  int i;
  FILE *f = fopen(fname, "wb");

  /* Unformatted JV3 disk. */
  if (f == NULL) {
    file_error("create JV3 disk '%s'", fname);
    return -1;
  }

  for (i = 0; i < (256 * 34); i++)
    putc(0xff, f);

  fclose(f);
  return 0;
}

int trs_create_blank_dmk(const char *fname, int sides, int density,
                         int eight, int ignden)
{
  int i;
  FILE *f = fopen(fname, "wb");

  /* Unformatted DMK disk */
  if (f == NULL) {
    file_error("create DMK disk '%s'", fname);
    return -1;
  }

  putc(0, f);           /* 0: not write protected */
  putc(0, f);           /* 1: initially zero tracks */
  if (eight) {
    if (density == 1)
       i = 0x14e0;
    else
       i = 0x2940;
  } else {
    if (density == 1)
      i = 0x0cc0;
    else
      i = 0x1900;
  }

  putc(i & 0xff, f);    /* 2: LSB of track length */
  putc(i >> 8, f);      /* 3: MSB of track length */

  i = 0;
  if (sides == 1)   i |= 0x10;
  if (density == 1) i |= 0x40;
  if (ignden)       i |= 0x80;
  putc(i, f);           /* 4: options */
  putc(0, f);           /* 5: reserved */
  putc(0, f);           /* 6: reserved */
  putc(0, f);           /* 7: reserved */
  putc(0, f);           /* 8: reserved */
  putc(0, f);           /* 9: reserved */
  putc(0, f);           /* a: reserved */
  putc(0, f);           /* b: reserved */
  putc(0, f);           /* c: MBZ */
  putc(0, f);           /* d: MBZ */
  putc(0, f);           /* e: MBZ */
  putc(0, f);           /* f: MBZ */

  fclose(f);
  return 0;
}

int trs_create_blank_hard(const char *fname, int cyls, int heads, int secs)
{
  /* Unformatted hard disk */
  /* We don't care about most of this header, but we generate
     it just in case some user wants to exchange hard drives with
     Matthew Reed's emulator or with Pete Cervasio's port of
     xtrshard/dct to Jeff Vavasour's Model III/4 emulator.
  */
  ReedHardHeader rhh = { 0 };
  FILE *f = fopen(fname, "wb");

  if (f == NULL) {
    file_error("create hard disk '%s'", fname);
    return -1;
  }

  rhh.id1 = 0x56;
  rhh.id2 = 0xcb;
  rhh.ver = 0x10;
  rhh.blks = 1;
  rhh.mb4 = 4;
  rhh.crtr = 0x42;
  rhh.heads = heads;
  rhh.cylhi = cyls >> 8;   /* MSB of cylinders */
  rhh.cyllo = cyls & 0xff; /* LSB of cylinders */
  rhh.sec = heads ? (heads * secs) : secs;
  rhh.gran = 8; /* deprecated */
  rhh.dcyl = 1; /* deprecated, should be 1 */
  snprintf(rhh.label, 9, "%s", "xtrshard");

  fwrite(&rhh, sizeof(rhh), 1, f);

  fclose(f);
  return 0;
}
