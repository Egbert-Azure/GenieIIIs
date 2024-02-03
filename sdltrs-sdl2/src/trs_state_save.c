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

#include <stdio.h>
#include <string.h>
#include "error.h"
#include "trs_state_save.h"

static const char stateFileBanner[] = "SDLTRS State Save File";
static int const stateFileBannerLen = sizeof(stateFileBanner) - 1;
static unsigned stateVersionNumber = 8;

int trs_state_save(const char *filename)
{
  FILE *file = fopen(filename, "wb");

  if (file) {
    trs_save_uint8(file, (Uint8 *)stateFileBanner, stateFileBannerLen);
    trs_save_uint32(file, &stateVersionNumber, 1);
    trs_main_save(file);
    trs_cassette_save(file);
    trs_clone_save(file);
    trs_cp500_save(file);
    trs_disk_save(file);
    trs_hard_save(file);
    trs_stringy_save(file);
    trs_interrupt_save(file);
    trs_io_save(file);
    trs_mem_save(file);
    trs_keyboard_save(file);
    trs_uart_save(file);
    trs_z80_save(file);
    trs_imp_exp_save(file);

    fclose(file);
    return 0;
  }

  file_error("save State '%s'", filename);
  return -1;
}

int trs_state_load(const char *filename)
{
  FILE *file = fopen(filename, "rb");

  if (file) {
    char banner[80];
    unsigned version;

    trs_load_uint8(file, (Uint8 *)banner, stateFileBannerLen);

    if (strncmp(banner, stateFileBanner, stateFileBannerLen)) {
      error("failed to get State Banner from '%s'", filename);
      fclose(file);
      return -1;
    }

    trs_load_uint32(file, &version, 1);

    if (version != stateVersionNumber) {
      error("unsupported version %d of State file", version);
      fclose(file);
      return -1;
    }

    trs_main_load(file);
    trs_cassette_load(file);
    trs_clone_load(file);
    trs_cp500_load(file);
    trs_disk_load(file);
    trs_hard_load(file);
    trs_stringy_load(file);
    trs_interrupt_load(file);
    trs_io_load(file);
    trs_mem_load(file);
    trs_keyboard_load(file);
    trs_uart_load(file);
    trs_z80_load(file);
    trs_imp_exp_load(file);

    fclose(file);
    return 0;
  }

  file_error("load State '%s'", filename);
  return -1;
}

void trs_save_uint8(FILE *file, const Uint8 *buffer, int count)
{
  fwrite(buffer, count, 1, file);
}

void trs_load_uint8(FILE *file, Uint8 *buffer, int count)
{
  fread(buffer, count, 1, file);
}

void trs_save_uint16(FILE *file, const Uint16 *buffer, int count)
{
  int i;
  Uint16 temp;
  Uint8 byte;

  for (i = 0; i < count; i++) {
    temp = *buffer++;
    byte = temp & 0xFF;
    fwrite(&byte, 1, 1, file);
    byte = temp >> 8;
    fwrite(&byte, 1, 1, file);
  }
}

void trs_load_uint16(FILE *file, Uint16 *buffer, int count)
{
  int i;
  Uint8 byte0, byte1;

  for (i = 0; i < count; i++) {
    fread(&byte0, 1, 1, file);
    fread(&byte1, 1, 1, file);

    *buffer++ = (byte1 << 8) | byte0;
  }
}

void trs_save_uint32(FILE *file, const Uint32 *buffer, int count)
{
  int i;
  unsigned temp;
  Uint8 byte;

  for (i = 0; i < count; i++) {
    temp = *buffer++;
    byte = temp & 0xFF;
    fwrite(&byte, 1, 1, file);

    temp >>= 8;
    byte = temp & 0xFF;
    fwrite(&byte, 1, 1, file);

    temp >>= 8;
    byte = temp & 0xFF;
    fwrite(&byte, 1, 1, file);

    temp >>= 8;
    byte = temp & 0xFF;
    fwrite(&byte, 1, 1, file);
  }
}

void trs_load_uint32(FILE *file, Uint32 *buffer, int count)
{
  int i;
  Uint8 byte0, byte1, byte2, byte3;

  for (i = 0; i < count; i++) {
    fread(&byte0, 1, 1, file);
    fread(&byte1, 1, 1, file);
    fread(&byte2, 1, 1, file);
    fread(&byte3, 1, 1, file);

    *buffer++ = (byte3 << 24) | (byte2 << 16) | (byte1 << 8) | byte0;
  }
}

void trs_save_uint64(FILE *file, const Uint64 *buffer, int count)
{
  int i, j;
  Uint64 temp;
  Uint8 byte;

  for (i = 0; i < count; i++) {
    temp = *buffer++;
     for (j = 0; j < 8; j++) {
       byte = temp & 0xFF;
       fwrite(&byte, 1, 1, file);
       temp = temp >> 8;
     }
  }
}

void trs_load_uint64(FILE *file, Uint64 *buffer, int count)
{
  int i, j;
  Uint8 byte[8];
  Uint64 temp;

  for (i = 0; i < count; i++) {
    for (j = 0; j < 8; j++)
      fread(&byte[j], 1, 1, file);

    temp = 0;
    for (j = 7; j >= 0; j--) {
      temp |= byte[j];
      if (j)
        temp = temp << 8;
    }

    *buffer++ = temp;
  }
}

void trs_save_int(FILE *file, const int *buffer, int count)
{
  int i;
  int num;
  unsigned int unum;
  Uint8 sign;
  Uint8 byte;

  for (i = 0; i < count; i++) {
    num = *buffer++;
    if (num < 0) {
      sign = 0x80;
      num = -num;
    } else {
      sign = 0;
    }

    unum = num;
    byte = unum & 0xFF;
    fwrite(&byte, 1, 1, file);

    unum = unum >> 8;
    byte = unum & 0xFF;
    fwrite(&byte, 1, 1, file);

    unum = unum >> 8;
    byte = unum & 0xFF;
    fwrite(&byte, 1, 1, file);

    unum = unum >> 8;
    byte = (unum & 0x7F) | sign;
    fwrite(&byte, 1, 1, file);
  }
}

void trs_load_int(FILE *file, int *buffer, int count)
{
  int i;
  int temp;
  Uint8 byte0, byte1, byte2, byte3, sign;

  for (i = 0; i < count; i++) {
    fread(&byte0, 1, 1, file);
    fread(&byte1, 1, 1, file);
    fread(&byte2, 1, 1, file);
    fread(&byte3, 1, 1, file);

    sign = byte3 & 0x80;
    byte3 &= 0x7f;

    temp = (byte3 << 24) | (byte2 << 16) | (byte1 << 8) | byte0;
    if (sign)
      temp = -temp;

    *buffer++ = temp;
  }
}

void trs_save_float(FILE *file, const float *buffer, int count)
{
  int i;
  char float_buff[21];

  for (i = 0; i < count; i++) {
    snprintf(float_buff, 21, "%20f", *buffer++);
    trs_save_uint8(file, (Uint8 *)float_buff, 20);
  }
}

void trs_load_float(FILE *file, float *buffer, int count)
{
  int i;
  char float_buff[21];

  for (i = 0; i < count; i++) {
    trs_load_uint8(file, (Uint8 *)float_buff, 20);
    sscanf(float_buff, "%f", buffer++);
  }
}

void trs_save_filename(FILE *file, char *filename)
{
  Uint16 length = strlen(filename);

  trs_save_uint16(file, &length, 1);
  trs_save_uint8(file, (Uint8 *)filename, length);
}

void trs_load_filename(FILE *file, char *filename)
{
  Uint16 length;

  trs_load_uint16(file, &length, 1);
  trs_load_uint8(file, (Uint8 *)filename, length);
  filename[length] = 0;
}
