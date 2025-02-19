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

#ifndef _TRS_STATE_SAVE_H
#define _TRS_STATE_SAVE_H

#include <SDL_types.h>

int  trs_state_save(const char *filename);
void trs_save_filename(FILE *file, char *filename);
void trs_save_float(FILE *file, const float *buffer, int count);
void trs_save_int(FILE *file, const int *buffer, int count);
void trs_save_uint8(FILE *file, const Uint8 *buffer, int count);
void trs_save_uint16(FILE *file, const Uint16 *buffer, int count);
void trs_save_uint32(FILE *file, const Uint32 *buffer, int count);
void trs_save_uint64(FILE *file, const Uint64 *buffer, int count);

int  trs_state_load(const char *filename);
void trs_load_filename(FILE *file, char *filename);
void trs_load_float(FILE *file, float *buffer, int count);
void trs_load_int(FILE *file, int *buffer, int count);
void trs_load_uint8(FILE *file, Uint8 *buffer, int count);
void trs_load_uint16(FILE *file, Uint16 *buffer, int count);
void trs_load_uint32(FILE *file, Uint32 *buffer, int count);
void trs_load_uint64(FILE *file, Uint64 *buffer, int count);

void trs_cassette_save(FILE *file);
void trs_clone_save(FILE *file);
void trs_cp500_save(FILE *file);
void trs_disk_save(FILE *file);
void trs_hard_save(FILE *file);
void trs_imp_exp_save(FILE *file);
void trs_interrupt_save(FILE *file);
void trs_io_save(FILE *file);
void trs_keyboard_save(FILE *file);
void trs_main_save(FILE *file);
void trs_mem_save(FILE *file);
void trs_stringy_save(FILE *file);
void trs_uart_save(FILE *file);
void trs_z80_save(FILE *file);

void trs_cassette_load(FILE *file);
void trs_clone_load(FILE *file);
void trs_cp500_load(FILE *file);
void trs_disk_load(FILE *file);
void trs_hard_load(FILE *file);
void trs_imp_exp_load(FILE *file);
void trs_interrupt_load(FILE *file);
void trs_io_load(FILE *file);
void trs_keyboard_load(FILE *file);
void trs_main_load(FILE *file);
void trs_mem_load(FILE *file);
void trs_stringy_load(FILE *file);
void trs_uart_load(FILE *file);
void trs_z80_load(FILE *file);

#endif /* _TRS_STATE_SAVE_H */
