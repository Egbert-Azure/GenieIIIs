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

#include <stdio.h>
#include <stdlib.h>
#include <SDL_types.h>
#include "error.h"
#include "trs.h"
#include "trs_disk.h"

#define BUFFER_SIZE 256

/*
 * Fake ROM for xtrs, initial hack
 * Small improvements 1-Nov-2020
 */
static const Uint8 trs_fakerom[] =
{
  0xf3,0xc3,0x00,0x01,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0xc3,0x00,
  0x01,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0x00,0x00,
  0xf3,0x21,0x42,0x01,0x11,0xc0,0x3d,0x3e,
  0x61,0x12,0x1a,0xfe,0x61,0x28,0x03,0x21,
  0x72,0x01,0x01,0x30,0x00,0xed,0xb0,0x3e,
  0x05,0xed,0x3c,0x3e,0x30,0x85,0xfe,0x35,
  0x28,0x16,0x32,0xf0,0x3d,0xfe,0x34,0x28,
  0x02,0x18,0xfe,0x21,0xa2,0x01,0x11,0x00,
  0x3e,0x01,0x2b,0x00,0xed,0xb0,0x18,0xfe,
  0x21,0xf0,0x3d,0x36,0x34,0x23,0x36,0x50,
  0x18,0xfe,0x59,0x6f,0x75,0x20,0x64,0x6f,
  0x20,0x6e,0x6f,0x74,0x20,0x68,0x61,0x76,
  0x65,0x20,0x61,0x20,0x52,0x4f,0x4d,0x20,
  0x69,0x6d,0x61,0x67,0x65,0x20,0x69,0x6e,
  0x73,0x74,0x61,0x6c,0x6c,0x65,0x64,0x20,
  0x66,0x6f,0x72,0x20,0x4d,0x6f,0x64,0x65,
  0x6c,0x20,0x59,0x4f,0x55,0x20,0x44,0x4f,
  0x20,0x4e,0x4f,0x54,0x20,0x48,0x41,0x56,
  0x45,0x20,0x41,0x20,0x52,0x4f,0x4d,0x20,
  0x49,0x4d,0x41,0x47,0x45,0x20,0x49,0x4e,
  0x53,0x54,0x41,0x4c,0x4c,0x45,0x44,0x20,
  0x46,0x4f,0x52,0x20,0x4d,0x4f,0x44,0x45,
  0x4c,0x20,0x28,0x4d,0x6f,0x64,0x65,0x6c,
  0x20,0x34,0x20,0x6d,0x6f,0x64,0x65,0x20,
  0x72,0x65,0x71,0x75,0x69,0x72,0x65,0x73,
  0x20,0x61,0x20,0x4d,0x6f,0x64,0x65,0x6c,
  0x20,0x33,0x20,0x52,0x4f,0x4d,0x20,0x69,
  0x6d,0x61,0x67,0x65,0x29,
};

/*
 * Free Model 4P mode boot ROM for xtrs - Version 0.06
 * Copyright 1999, Peter W. Cervasio (cervasio@airmail.net)
 * This software may be copied, modified, and used for any purpose
 * without fee, provided that (1) the above copyright notice is
 * retained, and (2) modified versions are clearly marked as having
 * been modified, with the modifier's name and the date included.
 */
static const Uint8 trs_rom4p[] =
{
  0xf3,0x3e,0x01,0xd3,0x9c,0xc3,0x69,0x00,
  0xc3,0x00,0x40,0x00,0x00,0x00,0x00,0x00,
  0xc3,0x03,0x40,0x00,0x00,0x00,0x00,0x00,
  0xc3,0x06,0x40,0x00,0x00,0x00,0x00,0x00,
  0xc3,0x09,0x40,0x00,0x00,0x00,0x00,0x00,
  0xc3,0x0c,0x40,0x00,0x00,0x00,0x00,0x00,
  0xc3,0x0f,0x40,0x00,0x00,0x00,0x00,0x00,
  0xc3,0x12,0x40,0xc9,0x00,0x00,0xc9,0x00,
  0x00,0xc9,0x00,0x00,0xc9,0x00,0x00,0xc9,
  0x00,0x00,0xc9,0x00,0x00,0xc9,0x00,0x00,
  0xc3,0x3a,0x01,0x4d,0x4f,0xaf,0xd3,0x9c,
  0xc3,0x00,0x43,0x00,0x00,0x00,0x00,0x00,
  0x00,0x00,0x00,0x00,0x00,0x00,0xc3,0x15,
  0x40,0xaf,0xd3,0xe4,0x3e,0x01,0xd3,0x84,
  0x3e,0x50,0xd3,0xec,0x31,0x00,0x42,0x21,
  0x3b,0x00,0x11,0x00,0x40,0x01,0x20,0x00,
  0xed,0xb0,0x21,0x00,0x3c,0x11,0x01,0x3c,
  0x01,0xff,0x03,0x36,0x20,0xed,0xb0,0xed,
  0x56,0xcd,0xa0,0x00,0xcd,0xd4,0x00,0xcc,
  0xf9,0x00,0xc2,0xb9,0x00,0xc3,0x1a,0x40,
  0x3e,0x05,0xed,0x3c,0x7d,0xfe,0x05,0xc8,
  0xf5,0x21,0x57,0x01,0x11,0xc0,0x3d,0x01,
  0x31,0x00,0xed,0xb0,0xc6,0x30,0x12,0x18,
  0xfe,0x21,0x88,0x01,0x37,0x1f,0x38,0x04,
  0x23,0x23,0x18,0xf9,0x5e,0x23,0x56,0xeb,
  0x11,0xc0,0x3d,0x7e,0xa7,0x28,0xfe,0x12,
  0x23,0x13,0x18,0xf7,0x3e,0x81,0xd3,0xf4,
  0x3e,0xd0,0xcd,0x47,0x01,0x06,0x00,0x10,
  0xfe,0x3e,0x0c,0xcd,0x47,0x01,0x06,0x00,
  0xcb,0x4f,0x28,0x08,0xdb,0xf0,0x10,0xf8,
  0xe6,0x02,0x18,0xc5,0xe6,0x99,0xc8,0x18,
  0xc0,0x11,0x01,0x00,0x21,0x00,0x43,0xcd,
  0x08,0x01,0xe6,0x1c,0xc8,0xc3,0xb9,0x00,
  0x06,0x81,0x0e,0xf4,0xed,0x41,0x0d,0x3e,
  0x18,0xed,0x51,0xcd,0x47,0x01,0x7b,0xd3,
  0xf2,0x3e,0x81,0xd3,0xf4,0x11,0x16,0x81,
  0x3e,0x80,0xd3,0xf0,0x06,0x64,0x10,0xfe,
  0x3e,0xc0,0xd3,0xe4,0xdb,0xf0,0xa3,0x28,
  0xfb,0xed,0xa2,0x7a,0xd3,0xf4,0xed,0xa2,
  0x18,0xfa,0xaf,0xd3,0xe4,0x3e,0x81,0xd3,
  0xf4,0xdb,0xf0,0xe3,0xe1,0xed,0x45,0xd3,
  0xf0,0x06,0x64,0x10,0xfe,0xdb,0xf0,0xcb,
  0x47,0xc8,0xcb,0x7f,0xc0,0x18,0xf6,0x54,
  0x68,0x69,0x73,0x20,0x52,0x4f,0x4d,0x20,
  0x63,0x6f,0x64,0x65,0x20,0x64,0x6f,0x65,
  0x73,0x20,0x4e,0x4f,0x54,0x20,0x77,0x6f,
  0x72,0x6b,0x20,0x77,0x68,0x65,0x6e,0x20,
  0x65,0x6d,0x75,0x6c,0x61,0x74,0x69,0x6e,
  0x67,0x20,0x4d,0x6f,0x64,0x65,0x6c,0x20,
  0xe2,0x01,0xe2,0x01,0x9a,0x01,0xae,0x01,
  0xc4,0x01,0x9a,0x01,0x9a,0x01,0xe2,0x01,
  0x9a,0x01,0x45,0x6d,0x75,0x6c,0x61,0x74,
  0x6f,0x72,0x20,0x6f,0x72,0x20,0x52,0x4f,
  0x4d,0x20,0x62,0x75,0x67,0x00,0x42,0x6f,
  0x6f,0x74,0x20,0x73,0x65,0x63,0x74,0x6f,
  0x72,0x20,0x43,0x52,0x43,0x20,0x65,0x72,
  0x72,0x6f,0x72,0x00,0x42,0x6f,0x6f,0x74,
  0x20,0x73,0x65,0x63,0x74,0x6f,0x72,0x20,
  0x6e,0x6f,0x74,0x20,0x66,0x6f,0x75,0x6e,
  0x64,0x20,0x6f,0x6e,0x20,0x64,0x69,0x73,
  0x6b,0x00,0x59,0x6f,0x75,0x20,0x64,0x6f,
  0x20,0x6e,0x6f,0x74,0x20,0x68,0x61,0x76,
  0x65,0x20,0x61,0x20,0x64,0x69,0x73,0x6b,
  0x20,0x69,0x6d,0x61,0x67,0x65,0x20,0x6c,
  0x6f,0x61,0x64,0x65,0x64,0x00,
};

/*
 * Exatron Stringy Floppy ROM code, from the "ESF Advanced Programmer's Guide".
 * Believed to be in the public domain.
 */
static const Uint8 trs_romesf[] =
{
  0xc3,0x42,0x32,0xc3,0xda,0x32,0xc3,0x5c,
  0x33,0xc3,0x6d,0x33,0xc3,0x82,0x33,0xc3,
  0x7f,0x34,0xc3,0x87,0x34,0x2a,0xe6,0x40,
  0xc3,0x1e,0x1d,0xc3,0x65,0x34,0xc3,0x2a,
  0x33,0xc3,0x6e,0x33,0xc3,0x5f,0x32,0xc3,
  0x64,0x33,0xc3,0x9a,0x34,0xc3,0xd2,0x30,
  0xc3,0x79,0x30,0x00,0x3c,0x3c,0x3c,0x3c,
  0x3c,0x3c,0x32,0x1a,0x40,0x21,0x82,0x30,
  0x22,0x04,0x40,0x2a,0xb1,0x40,0x36,0x19,
  0x2b,0x36,0x97,0x2b,0x36,0xc3,0x2b,0x36,
  0xf0,0x2b,0x22,0xb1,0x40,0x21,0xa2,0x34,
  0xcd,0x79,0x35,0x11,0x32,0x00,0xcd,0x83,
  0x1e,0x3a,0x1a,0x40,0xed,0x44,0x28,0x0e,
  0x21,0x5c,0x37,0x22,0x16,0x40,0xc6,0x06,
  0x4f,0xfe,0x05,0xc2,0x55,0x31,0xc3,0x2b,
  0x1a,0xcd,0xf8,0x1a,0x2a,0xa4,0x40,0x2b,
  0x18,0x96,0xe3,0x7d,0xfe,0x5b,0x20,0x03,
  0x7c,0xfe,0x1d,0xe3,0xc2,0x78,0x1d,0xd7,
  0xf5,0xe6,0xdf,0xfe,0x40,0x28,0x02,0xf1,
  0xc9,0xf1,0xf1,0xd7,0x28,0x05,0xfe,0x23,
  0x20,0x16,0xd7,0x28,0x63,0xcd,0x1c,0x2b,
  0xf5,0xfe,0x08,0x30,0x2f,0xcd,0x87,0x34,
  0xc2,0x4a,0x1e,0xf1,0x7e,0xca,0x1e,0x1d,
  0x11,0x15,0x30,0xd5,0xfe,0xad,0x28,0x45,
  0xfe,0xa7,0xca,0x52,0x31,0xfe,0xbb,0x28,
  0x1d,0x22,0xe6,0x40,0x2a,0xb1,0x40,0x23,
  0x23,0xe9,0xd7,0x0e,0x00,0x28,0x09,0xcd,
  0x1c,0x2b,0xfe,0x64,0xd2,0x4a,0x1e,0x4f,
  0x22,0xe6,0x40,0x79,0xb7,0xc9,0xcd,0xd2,
  0x30,0x21,0xc5,0x34,0xcd,0x79,0x35,0x61,
  0xcd,0x5f,0x32,0xf5,0xe6,0xb7,0x20,0x09,
  0xcd,0xaf,0x0f,0x21,0xce,0x34,0xcd,0x79,
  0x35,0xf1,0xc3,0xfe,0x31,0xcd,0xd2,0x30,
  0x28,0x1c,0x2b,0xd7,0x28,0x1b,0xcf,0x2c,
  0xcd,0x47,0x31,0xd5,0xcf,0x2c,0xcd,0x47,
  0x31,0xd5,0x11,0x15,0x30,0x28,0x19,0xcf,
  0x2c,0xcd,0x47,0x31,0x28,0x12,0xc3,0x97,
  0x19,0xcd,0xf8,0x1a,0x23,0xed,0x5b,0xa4,
  0x40,0xed,0x52,0xd5,0xe5,0x11,0x00,0x00,
  0x21,0xd6,0x34,0xcd,0x79,0x35,0x79,0xc1,
  0xe1,0xcd,0x82,0x33,0xc3,0xfb,0x31,0xc5,
  0xcd,0x02,0x2b,0x2b,0xd7,0xc1,0x22,0xe6,
  0x40,0xc9,0xcd,0xd2,0x30,0x21,0xdf,0x34,
  0xcd,0x79,0x35,0x2a,0xa4,0x40,0x11,0x64,
  0x00,0x19,0xed,0x5b,0xa0,0x40,0xed,0x52,
  0xe5,0x2a,0xa2,0x40,0x23,0x7c,0xb5,0x28,
  0x0b,0x2a,0xa4,0x40,0xed,0x5b,0xf9,0x40,
  0x37,0xed,0x52,0xe3,0xfd,0xe1,0x21,0x00,
  0x00,0xe5,0x61,0x2e,0xff,0x79,0xb7,0x20,
  0x01,0x6f,0xcd,0x34,0x37,0xcd,0x49,0x36,
  0x20,0x68,0x7a,0xb7,0x28,0xf7,0xfa,0x8d,
  0x31,0x94,0xa5,0x20,0xf0,0xcd,0x0b,0x37,
  0x20,0x58,0xc0,0xc0,0x62,0xe5,0xdd,0xe1,
  0xe5,0xe1,0x00,0xcd,0x0b,0x37,0x20,0x4a,
  0x62,0x7a,0xb5,0x28,0x3c,0xe5,0xfd,0xe1,
  0xe5,0xe1,0xcd,0x0b,0x37,0x20,0x3b,0x5d,
  0xe1,0x19,0x38,0x48,0xd5,0xd5,0xe1,0xcd,
  0xa5,0x36,0x20,0x2e,0xd1,0xfd,0xe5,0xe1,
  0x7d,0xb4,0x20,0x10,0x2a,0xa2,0x40,0x23,
  0x7c,0xb5,0x21,0x11,0x32,0x28,0x0e,0x21,
  0x79,0x30,0x18,0x09,0x3a,0x80,0x38,0xb7,
  0x28,0x03,0x21,0x1c,0x32,0xe3,0xaf,0x18,
  0x0a,0x00,0xfd,0xe3,0xdd,0x2a,0xa4,0x40,
  0x18,0xc0,0xe1,0xcd,0x25,0x37,0x21,0x37,
  0x35,0xca,0x79,0x35,0xcd,0x3c,0x35,0x1e,
  0x2a,0xc3,0xa2,0x19,0x3e,0x20,0xb7,0x18,
  0xea,0xdd,0x22,0xf9,0x40,0x2a,0xa4,0x40,
  0xe5,0xc3,0xe8,0x1a,0x21,0x31,0x35,0xcd,
  0x79,0x35,0xdd,0xe5,0xe1,0xaf,0xed,0x52,
  0xd5,0xcd,0xaf,0x0f,0x3e,0x2c,0xcd,0x2a,
  0x03,0xe1,0xcd,0xaf,0x0f,0x3e,0x2c,0xcd,
  0x2a,0x03,0xfd,0xe5,0xe1,0xcd,0xaf,0x0f,
  0x18,0xc5,0xc5,0xd5,0xcd,0x34,0x37,0xcd,
  0x4b,0x37,0x3a,0x40,0x38,0xe6,0x04,0x0f,
  0x20,0x07,0xed,0x78,0xe6,0x04,0x28,0xf2,
  0xaf,0xcd,0x25,0x37,0xd1,0xc1,0xc9,0xc5,
  0xd5,0xcd,0x33,0x37,0xe6,0x01,0x20,0x6c,
  0x7c,0xfe,0x02,0x30,0x18,0xcd,0x42,0x32,
  0x20,0x62,0x3e,0x85,0x32,0x1a,0x40,0xed,
  0x79,0xcd,0x4b,0x37,0x2e,0xff,0xcd,0x86,
  0x35,0x20,0x51,0x18,0x09,0xcd,0x49,0x36,
  0x20,0x4a,0x7a,0x84,0x20,0xf7,0x06,0x10,
  0x10,0xfe,0x2e,0x80,0xcd,0x92,0x35,0xc2,
  0xd4,0x32,0x1e,0x5a,0x26,0xff,0x54,0xcd,
  0x19,0x36,0xca,0x9c,0x32,0x06,0x01,0xcd,
  0x19,0x36,0x1d,0xc2,0xa5,0x32,0x06,0x01,
  0xed,0x41,0xcd,0x49,0x36,0x20,0x1d,0x7a,
  0xbd,0x20,0xf7,0x21,0xff,0xff,0x23,0xed,
  0x78,0xe6,0x04,0x20,0x0d,0xcd,0xcd,0x36,
  0xc2,0xd2,0x32,0xf5,0xf1,0x14,0x3e,0x08,
  0x28,0xec,0xe6,0xfb,0xcd,0x25,0x37,0xd1,
  0xc1,0xc9,0xdd,0xe5,0xd5,0xe5,0xdd,0xe1,
  0xe5,0xfd,0xe5,0xc5,0x79,0x2f,0x4f,0x78,
  0x2f,0x47,0xc5,0xfd,0xe1,0xcd,0x34,0x37,
  0xcd,0x49,0x36,0x20,0x25,0x7a,0xb7,0x3e,
  0x04,0x20,0x1f,0xdd,0x7e,0x00,0xcd,0x0b,
  0x37,0x20,0x17,0x62,0xeb,0xfd,0x19,0xeb,
  0x38,0x1b,0xd8,0x00,0xe5,0xe1,0xcd,0xa5,
  0x36,0x20,0x07,0xd1,0xfd,0x19,0xfd,0x23,
  0xfd,0xe5,0xcd,0x25,0x37,0xc1,0xfd,0xe1,
  0xe1,0xd1,0xdd,0xe1,0xc9,0x3e,0x20,0xb7,
  0x18,0xf0,0xdd,0xe5,0xe5,0xdd,0xe1,0xe5,
  0xd5,0xc5,0xcd,0x33,0x37,0x20,0x17,0x2e,
  0x00,0xcd,0x92,0x35,0x20,0x10,0xe1,0xe5,
  0xdd,0x7e,0x00,0xcd,0x3b,0x36,0x20,0x06,
  0x00,0xe1,0xe5,0xcd,0xc7,0x35,0xcd,0x25,
  0x37,0xe1,0xd1,0xe5,0xcc,0x4d,0x37,0xc1,
  0xe1,0xdd,0xe1,0xc9,0xd5,0x16,0x3d,0xcd,
  0x2a,0x33,0xd1,0xc9,0xf5,0x78,0xb1,0xc4,
  0x5c,0x33,0xf1,0x18,0x01,0x3d,0xc5,0xd5,
  0xe5,0x2f,0xf6,0x80,0x6f,0xcd,0x33,0x37,
  0xcc,0x86,0x35,0xcd,0x25,0x37,0xe1,0xd1,
  0xc1,0xc9,0xdd,0xe5,0xfd,0xe5,0xe5,0xd5,
  0xc5,0xe5,0xdd,0xe1,0xe5,0x6f,0xe5,0xd5,
  0xfd,0xe1,0xc5,0xcd,0x33,0x37,0x20,0x24,
  0xcd,0x49,0x36,0x20,0x1f,0x7a,0x85,0x20,
  0xf7,0x06,0x10,0x10,0xfe,0xcd,0x92,0x35,
  0x20,0x12,0xed,0x5a,0xdd,0xe5,0xe1,0xcd,
  0x3b,0x36,0x20,0x08,0xed,0x5a,0xfd,0xe5,
  0xe1,0xcd,0x3b,0x36,0x20,0x6f,0xed,0x5a,
  0xed,0x5a,0xe1,0xcd,0x3b,0x36,0x20,0x66,
  0xe5,0xe1,0x00,0xcd,0xc7,0x35,0x20,0x5e,
  0xe1,0xdd,0xe1,0x7d,0x2f,0x6f,0xcd,0x86,
  0x35,0x20,0x55,0x7d,0x2f,0x6f,0x3e,0x01,
  0xed,0x79,0xcd,0x49,0x36,0x20,0x49,0x7a,
  0xbd,0x20,0xf7,0xc0,0xe5,0xe1,0xcd,0x0b,
  0x37,0x20,0x3d,0xc0,0x00,0x7d,0xdd,0xe5,
  0xe1,0xbd,0x20,0x65,0x7a,0xbc,0x20,0x61,
  0xcd,0x0b,0x37,0x20,0x2b,0xc0,0x00,0x7d,
  0xfd,0xe5,0xe1,0xbd,0x20,0x53,0x7a,0xbc,
  0x20,0x4f,0xcd,0x0b,0x37,0x20,0x19,0xc0,
  0xc0,0xe5,0xe1,0x7d,0xe1,0xe5,0xbd,0x20,
  0x40,0x7a,0xbc,0x20,0x3c,0xcd,0xcd,0x36,
  0x20,0x06,0xc0,0x18,0x0e,0xe1,0xe1,0xe1,
  0xcd,0x25,0x37,0xc1,0xd1,0xe1,0xfd,0xe1,
  0xdd,0xe1,0xc9,0xdd,0x7e,0x00,0xba,0x20,
  0x20,0xdd,0x23,0x2b,0x7c,0xb5,0xc2,0x25,
  0x34,0xcd,0xcd,0x36,0x20,0xe2,0x06,0x06,
  0x10,0xfe,0xcd,0xcd,0x36,0x20,0xd9,0x3a,
  0x1a,0x40,0xb7,0x28,0x02,0x3e,0x0a,0x18,
  0xcf,0x3e,0x40,0x18,0xcb,0xc5,0xd5,0xf5,
  0xcd,0x34,0x37,0xcd,0x49,0x36,0x20,0x05,
  0xf1,0xf5,0x82,0x20,0xf6,0xcd,0x25,0x37,
  0xd1,0xd1,0xcc,0x4d,0x37,0xc1,0xc9,0xd5,
  0x16,0x3d,0xcd,0x65,0x34,0xd1,0xc9,0xc5,
  0xf6,0xf0,0x4f,0xed,0x78,0xe6,0x08,0x79,
  0xc1,0xc0,0xe5,0x2a,0xb1,0x40,0x23,0x77,
  0xe1,0xc9,0xe5,0xf5,0xcd,0x3c,0x35,0xf1,
  0xe1,0xc9,0x45,0x58,0x41,0x54,0x52,0x4f,
  0x4e,0x20,0x53,0x54,0x52,0x49,0x4e,0x47,
  0x59,0x20,0x46,0x4c,0x4f,0x50,0x50,0x59,
  0x20,0x56,0x45,0x52,0x53,0x49,0x4f,0x4e,
  0x20,0x34,0x2e,0x31,0x8d,0x45,0x52,0x41,
  0x53,0x49,0x4e,0x47,0x2e,0xae,0x20,0x42,
  0x59,0x54,0x45,0x53,0x2e,0xae,0x57,0x52,
  0x49,0x54,0x49,0x4e,0x47,0x2e,0xae,0x52,
  0x45,0x41,0x44,0x49,0x4e,0x47,0x2e,0xae,
  0x56,0x45,0x52,0x49,0x46,0xd9,0x50,0x41,
  0x52,0x49,0x54,0xd9,0x43,0x48,0x45,0x43,
  0x4b,0x53,0x55,0xcd,0x4f,0x55,0x54,0x20,
  0x4f,0x46,0x20,0x4d,0x45,0x4d,0x4f,0x52,
  0xd9,0x54,0x41,0x50,0x45,0x20,0x54,0x4f,
  0x4f,0x20,0x53,0x48,0x4f,0x52,0xd4,0x57,
  0x52,0x49,0x54,0x45,0x2d,0x50,0x52,0x4f,
  0x54,0x45,0x43,0x54,0x45,0x44,0x80,0x45,
  0x4f,0xc6,0x20,0x45,0x52,0x52,0x4f,0x52,
  0x8d,0x42,0x52,0x45,0x41,0x4b,0x8d,0x44,
  0x4f,0x4e,0x45,0x8d,0xcb,0x47,0x21,0x17,
  0x35,0x20,0x30,0xcb,0x4f,0x21,0x31,0x35,
  0x20,0x2f,0xcb,0x57,0x21,0x09,0x35,0x20,
  0x22,0xcb,0x5f,0x21,0xee,0x34,0x20,0x1b,
  0xcb,0x67,0x21,0xf4,0x34,0x20,0x14,0xcb,
  0x6f,0x21,0xfc,0x34,0x20,0x0d,0xcb,0x77,
  0x21,0xe8,0x34,0x20,0x06,0x21,0x27,0x35,
  0xcb,0x7f,0xc8,0xcd,0x79,0x35,0x21,0x2a,
  0x35,0x7e,0x23,0xf5,0xe6,0x7f,0xcd,0x2a,
  0x03,0xf1,0x17,0x30,0xf4,0xc9,0xcd,0x92,
  0x35,0xc0,0xc0,0x06,0x04,0x10,0xfe,0xc3,
  0x3b,0x36,0x3e,0x85,0x32,0x1a,0x40,0xed,
  0x79,0xcd,0x4b,0x37,0xc0,0xcd,0x14,0x36,
  0xc0,0x00,0x18,0x00,0xcd,0x19,0x36,0xc0,
  0xc0,0x06,0x04,0x10,0xfe,0x3a,0x1a,0x40,
  0xe6,0x7f,0xed,0x79,0x00,0x00,0x06,0x05,
  0x16,0x16,0xcd,0xec,0x35,0xc0,0x55,0x58,
  0x06,0x04,0xc3,0xea,0x35,0x00,0x00,0xdd,
  0x56,0x00,0xcd,0xee,0x35,0xc0,0xdd,0x23,
  0x2b,0x7c,0xb5,0x20,0xf0,0xc0,0x06,0x01,
  0x97,0x93,0x57,0xcd,0xec,0x35,0xc0,0xc0,
  0x00,0x06,0x04,0xcd,0xec,0x35,0xc0,0xc0,
  0x06,0x05,0x00,0x00,0x10,0xfe,0x3a,0x1a,
  0x40,0xed,0x79,0x7a,0x83,0x5f,0x06,0x08,
  0x23,0xcd,0x14,0x36,0x2b,0xc0,0xc0,0x06,
  0x02,0x10,0xfe,0x3a,0x1a,0x40,0xf6,0x80,
  0x32,0x1a,0x40,0xe6,0x7f,0xed,0x79,0xaf,
  0xc9,0xdd,0x7e,0x00,0xed,0x78,0xe6,0x05,
  0xc0,0xc5,0x3a,0x1a,0x40,0xcb,0x0a,0x30,
  0x16,0xee,0x80,0xed,0x79,0xee,0x80,0x00,
  0x00,0xbf,0x06,0x06,0x10,0xfe,0x32,0x1a,
  0x40,0xed,0x79,0xc1,0x10,0xdb,0xc9,0x00,
  0xc3,0x25,0x36,0x55,0xcd,0xee,0x35,0xc0,
  0x00,0x00,0x00,0x54,0x06,0x04,0xc3,0xec,
  0x35,0xcd,0x5e,0x36,0xc0,0x7a,0xd6,0x16,
  0xc2,0x49,0x36,0x3c,0xcd,0x19,0x37,0xcd,
  0xcd,0x36,0x32,0x1a,0x40,0xc9,0x06,0x14,
  0xcd,0x1c,0x37,0xed,0x78,0xf2,0x60,0x36,
  0xcd,0x1c,0x37,0xed,0x78,0xfa,0x68,0x36,
  0xed,0x78,0xfa,0x5e,0x36,0xed,0x78,0xf2,
  0x75,0x36,0x3e,0x06,0x3e,0x06,0xcd,0x19,
  0x37,0xed,0x78,0xf2,0x5e,0x36,0xed,0x78,
  0xfa,0x86,0x36,0xc3,0x8e,0x36,0x3e,0x05,
  0xcd,0x19,0x37,0x10,0xdb,0x04,0xed,0x78,
  0xf2,0x75,0x36,0xed,0x58,0xfa,0x9b,0x36,
  0x3e,0x00,0xc3,0xdf,0x36,0xcd,0xcd,0x36,
  0xc0,0xc0,0xdd,0x72,0x00,0xdd,0x23,0x2b,
  0x00,0xe5,0xe1,0x7c,0xb5,0xc2,0xa5,0x36,
  0xcd,0xcd,0x36,0xc0,0x3e,0x02,0xcd,0x19,
  0x37,0xcd,0xcd,0x36,0xc0,0x3a,0x1a,0x40,
  0xb7,0xc8,0x3e,0x10,0xc9,0xed,0x78,0xfa,
  0xd6,0x36,0x3e,0x08,0xb7,0xc9,0xed,0x58,
  0xfa,0xd6,0x36,0x3a,0x1a,0x40,0x82,0x32,
  0x1a,0x40,0x06,0x08,0x06,0x08,0x00,0x3e,
  0x03,0xcd,0x19,0x37,0x7b,0x2f,0xed,0x58,
  0xfa,0xfb,0x36,0xed,0x58,0xf2,0xf3,0x36,
  0xc3,0x03,0x37,0xed,0x58,0xfa,0xfb,0x36,
  0xc3,0x03,0x37,0xab,0x07,0xcb,0x1a,0xaf,
  0x10,0xdc,0xc9,0xcd,0xcd,0x36,0xc0,0x6a,
  0x00,0x3e,0x02,0xcd,0x19,0x37,0xc3,0xcd,
  0x36,0x3d,0x20,0xfd,0x3a,0x40,0x38,0xe6,
  0x04,0xc8,0x0f,0xd1,0xc9,0xf5,0xaf,0xed,
  0x79,0x3a,0x12,0x40,0xfe,0xfb,0x28,0x01,
  0xfb,0xf1,0xc9,0xf6,0xaf,0xe5,0x2a,0xb1,
  0x40,0x23,0x4e,0xe1,0x28,0x05,0xed,0x78,
  0xe6,0x01,0xc0,0x3c,0xed,0x79,0xf3,0x16,
  0x07,0x18,0x02,0x16,0x1f,0xaf,0xb7,0x20,
  0x04,0xed,0x78,0xe6,0x04,0x10,0xf7,0x15,
  0x20,0xf4,0xb7,0xc9,0x21,0x36,0x40,0x01,
  0x01,0x38,0x16,0x00,0x0a,0x5f,0xae,0x73,
  0xa3,0x20,0x07,0x14,0x2c,0xcb,0x01,0xf8,
  0x18,0xf2,0x5f,0xc5,0x06,0x07,0xcd,0x60,
  0x00,0xc1,0x0a,0xa3,0xc8,0xc3,0xfb,0x03,
};

/*
 * Patch Level II ROM for auto-boot from hard disk:
 * https://trs80.nl/software/model1hddboot/
 */
static void trs_boot_hd(void)
{
  rom_write(0x0066, 0xAF);  /* XOR A */
  rom_write(0x0067, 0x3D);  /* DEC A */
  rom_write(0x0068, 0x18);  /* JR 0000 */
  rom_write(0x0069, 0x98);
  rom_write(0x006A, 0xD3);  /* OUT(0CF),A */
  rom_write(0x006B, 0xCF);
  rom_write(0x006C, 0xDB);  /* IN A,(0CF) */
  rom_write(0x006D, 0xCF);
  rom_write(0x006E, 0xE6);  /* AND C0 */
  rom_write(0x006F, 0xC0);
  rom_write(0x0070, 0xEE);  /* XOR 40 */
  rom_write(0x0071, 0x40);
  rom_write(0x0072, 0x20);  /* JRNZ, 006C */
  rom_write(0x0073, 0xF8);
  rom_write(0x0074, 0xC9);  /* RET */
  rom_write(0x0075, 0x11);  /* LD DE,4080 */
  rom_write(0x0076, 0x80);
  rom_write(0x0077, 0x40);

  rom_write(0x00B5, 0x21);  /* LD HL,2FFB */
  rom_write(0x00B6, 0xFB);
  rom_write(0x00B7, 0x2F);

  rom_write(0x00FC, 0x21);  /* LD HL,0473 */
  rom_write(0x00FD, 0x73);
  rom_write(0x00FE, 0x04);

  rom_write(0x0105, 0xCD);  /* CALL 006C */
  rom_write(0x0106, 0x6C);
  rom_write(0x0107, 0x00);
  rom_write(0x0108, 0x01);  /* LD BC, 06CE */
  rom_write(0x0109, 0xCE);
  rom_write(0x010A, 0x06);
  rom_write(0x010B, 0xED);  /* OUT (C),A */
  rom_write(0x010C, 0x79);
  rom_write(0x010D, 0x0D);  /* DEC C */
  rom_write(0x010E, 0x10);  /* DJNZ 010B */
  rom_write(0x010F, 0xFB);
  rom_write(0x0110, 0x3E);  /* LD A, 20 */
  rom_write(0x0111, 0x20);
  rom_write(0x0112, 0xCD);  /* CALL 006A */
  rom_write(0x0113, 0x6A);
  rom_write(0x0114, 0x00);
  rom_write(0x0115, 0x21);  /* LD HL,4200 */
  rom_write(0x0116, 0x00);
  rom_write(0x0117, 0x42);
  rom_write(0x0118, 0xE5);  /* PUSH HL */
  rom_write(0x0119, 0xED);
  rom_write(0x011A, 0xB2);  /* INIR */
  rom_write(0x011B, 0xC9);  /* RET */

  rom_write(0x0471, 0x18);  /* JR 047D */
  rom_write(0x0472, 0x0A);
  rom_write(0x0473, 0x52);  /* DEFM 'R/S' */
  rom_write(0x0474, 0x2F);
  rom_write(0x0475, 0x53);
  rom_write(0x0476, 0x20);  /* DEFM ' L2' */
  rom_write(0x0477, 0x4C);
  rom_write(0x0478, 0x32);
  rom_write(0x0479, 0x2E);  /* DEFM '..' */
  rom_write(0x047A, 0x2E);
  rom_write(0x047B, 0x0D);  /* DEFB 0D */
  rom_write(0x047C, 0x00);  /* DEFB 00 */
  rom_write(0x047D, 0xCD);  /* CALL 0541 */
  rom_write(0x047E, 0x41);
  rom_write(0x047F, 0x05);

  rom_write(0x2FFB, 0x4D);  /* DEFM 'Mem' */
  rom_write(0x2FFC, 0x65);
  rom_write(0x2FFD, 0x6D);
  rom_write(0x2FFE, 0x2E);  /* DEFM '.' */
  rom_write(0x2FFF, 0x00);  /* DEFB 00 */

  rom_write(0x0674, 0x08);  /* EX AF, AF' */
  rom_write(0x0675, 0xAF);  /* XOR A */
  rom_write(0x0676, 0xD3);  /* OUT (0FF),A */
  rom_write(0x0677, 0xFF);
  rom_write(0x0678, 0x21);  /* LD HL,06D2 */
  rom_write(0x0679, 0xD2);
  rom_write(0x067A, 0x06);
  rom_write(0x067B, 0x11);  /* LD DE,4000 */
  rom_write(0x067C, 0x00);
  rom_write(0x067D, 0x40);
  rom_write(0x067E, 0x01);  /* LD BC, 0036 */
  rom_write(0x067F, 0x36);
  rom_write(0x0680, 0x00);
  rom_write(0x0681, 0xED);  /* LDIR */
  rom_write(0x0682, 0xB0);
  rom_write(0x0683, 0x3D);  /* DEC A */
  rom_write(0x0684, 0x20);  /* JR NZ,0678 */
  rom_write(0x0685, 0xF2);
  rom_write(0x0686, 0x06);  /* LD B,27 */
  rom_write(0x0687, 0x27);
  rom_write(0x0688, 0x12);  /* LD (DE), A */
  rom_write(0x0689, 0x13);  /* INC DE */
  rom_write(0x068A, 0x10);  /* DJNZ 0688 */
  rom_write(0x068B, 0xFC);
  rom_write(0x068C, 0x31);  /* LD SP, 407D */
  rom_write(0x068D, 0x7D);
  rom_write(0x068E, 0x40);
  rom_write(0x068F, 0x3A);  /* LD A,(3840) */
  rom_write(0x0690, 0x40);
  rom_write(0x0691, 0x38);
  rom_write(0x0692, 0xCB);  /* BIT 2, A */
  rom_write(0x0693, 0x57);
  rom_write(0x0694, 0x20);  /* JRNZ, 06C8 */
  rom_write(0x0695, 0x32);
  rom_write(0x0696, 0x17);  /* RLA */
  rom_write(0x0697, 0x38);  /* JR C,069F */
  rom_write(0x0698, 0x06);
  rom_write(0x0699, 0xDB);  /* IN A,(0CF) */
  rom_write(0x069A, 0xCF);
  rom_write(0x069B, 0x3C);  /* INC A */
  rom_write(0x069C, 0xC2);  /* JP NZ,0105 */
  rom_write(0x069D, 0x05);
  rom_write(0x069E, 0x01);
  rom_write(0x069F, 0x21);  /* LD HL,37EC */
  rom_write(0x06A0, 0xEC);
  rom_write(0x06A1, 0x37);
  rom_write(0x06A2, 0x7E);  /* LD A,(HL) */
  rom_write(0x06A3, 0x3C);  /* INC A */
  rom_write(0x06A4, 0x28);  /* JR Z, 06C8 */
  rom_write(0x06A5, 0x22);
  rom_write(0x06A6, 0x3E);  /* LD A, 01 */
  rom_write(0x06A7, 0x01);
  rom_write(0x06A8, 0x32);  /* LD(37E1), A */
  rom_write(0x06A9, 0xE1);
  rom_write(0x06AA, 0x37);
  rom_write(0x06AB, 0x11);  /* LD DE,37EE */
  rom_write(0x06AC, 0xEE);
  rom_write(0x06AD, 0x37);
  rom_write(0x06AE, 0x36);  /* LD (HL),3 */
  rom_write(0x06AF, 0x03);
  rom_write(0x06B0, 0xCD);  /* CALL 0060 */
  rom_write(0x06B1, 0x60);
  rom_write(0x06B2, 0x00);
  rom_write(0x06B3, 0xCB);  /* BIT 0,(HL) */
  rom_write(0x06B4, 0x46);
  rom_write(0x06B5, 0x20);  /* JR NZ, 06B3 */
  rom_write(0x06B6, 0xFC);
  rom_write(0x06B7, 0x12);  /* LD (DE),A */
  rom_write(0x06B8, 0x13);  /* INC DE */
  rom_write(0x06B9, 0x06);  /* LD B,42 */
  rom_write(0x06BA, 0x42);
  rom_write(0x06BB, 0xC5);  /* PUSH BC */
  rom_write(0x06BC, 0x36);  /* LD (HL),8C> */
  rom_write(0x06BD, 0x8C);
  rom_write(0x06BE, 0xCB);  /* BIT 1, (HL) */
  rom_write(0x06BF, 0x4E);
  rom_write(0x06C0, 0x28);  /* JR Z,06BE */
  rom_write(0x06C1, 0xFC);
  rom_write(0x06C2, 0x1A);  /* LD A,(DE) */
  rom_write(0x06C3, 0x02);  /* LD (BC), A */
  rom_write(0x06C4, 0x0C);  /* INC C */
  rom_write(0x06C5, 0x20);  /* JR NZ, 06BE */
  rom_write(0x06C6, 0xF7);
  rom_write(0x06C7, 0xC9);  /* RET */
  rom_write(0x06C8, 0x08);  /* EX AF, AF' */
  rom_write(0x06C9, 0xCA);  /* JP Z,0075 */
  rom_write(0x06CA, 0x75);
  rom_write(0x06CB, 0x00);
}

/*
 * TRS-80 DOS /cmd file loader.
 *
 * See the LDOS Quarterly, April 1, 1982 (Vol 1, No 4), for documentation
 * of the TRS-80 DOS /cmd file format.
 *
 * Load the /cmd file f into the given memory.  Return LOAD_CMD_OK for
 * success if f was a normal /cmd file, LOAD_CMD_EOF for failure due to
 * premature end of file, or a positive number B for an unknown or badly
 * formatted load block of typecode B (load file format error).
 */

#define LOAD_CMD_OK   0
#define LOAD_CMD_EOF -1

static int
load_cmd(FILE* f, Uint8 memory[65536], int *xferaddr)
{
  if (xferaddr)
    *xferaddr = -1;

  for (;;) {
    Uint16 addr; /* wrap at 2**16 */
    int a1, a2, count;
    int const c = getc(f); /* get block type code */

    if (c == EOF) return LOAD_CMD_OK;

    count = getc(f);
    if (count == EOF) {
      if (c == 3) return LOAD_CMD_OK;
      return LOAD_CMD_EOF;
    }
    if (count == 0) count = 256;

    switch (c) {
    case 1: /* load bytes into memory */
      a1 = getc(f);
      if (a1 == EOF) return LOAD_CMD_EOF;
      a2 = getc(f);
      if (a2 == EOF) return LOAD_CMD_EOF;
      addr = a1 + a2 * 256;

      count -= 2;
      if (count <= 0) count += 256;

      while (count-- > 0) {
        int const v = getc(f);

        if (v == EOF) return LOAD_CMD_EOF;
        if (memory)
          memory[addr++] = v;
        else
          mem_write(addr++, v);
      }
      break;

    case 2: /* transfer address and end of file */
      if (count != 2) return c;
      a1 = getc(f);
      if (a1 == EOF) return LOAD_CMD_EOF;
      a2 = getc(f);
      if (a2 == EOF) return LOAD_CMD_EOF;

      if (xferaddr) *xferaddr = a1 + a2 * 256;
      return LOAD_CMD_OK;

    case 3: /* end of file with no transfer address */
      while (count-- > 0) {
        if (getc(f) == EOF) return LOAD_CMD_EOF;
      }
      return LOAD_CMD_OK;

    case 5: /* module header */
      while (count-- > 0) {
        if (getc(f) == EOF) return LOAD_CMD_EOF;
      }
      break;

    case 0x10: /* yanked load block */
      a1 = getc(f);
      if (a1 == EOF) return LOAD_CMD_EOF;
      a2 = getc(f);
      if (a2 == EOF) return LOAD_CMD_EOF;
      addr = a1 + a2 * 256;

      count -= 2;
      if (count <= 0) count += 256;

      while (count-- > 0) {
        if (getc(f) == EOF) return LOAD_CMD_EOF;
      }
      break;

    default:
      return c;
    }
  }

  return LOAD_CMD_OK;
}

static int hex_byte(const char *string)
{
  char buf[3];

  buf[0] = string[0];
  buf[1] = string[1];
  buf[2] = '\0';

  return(strtol(buf, (char **)NULL, 16));
}

static int load_hex(FILE *file)
{
  char buffer[BUFFER_SIZE];
  int  high = 0;

  while (fgets(buffer, BUFFER_SIZE, file)) {
    if (buffer[0] == ':') {
      /* colon */
      const char *b = buffer + 1;

      /* number of bytes on the line */
      int num_bytes = hex_byte(b);
      int check = num_bytes;
      int address;

      b += 2;

      /* the starting address */
      address  = hex_byte(b) << 8;  b += 2;
      address |= hex_byte(b);  b += 2;
      check   += (address >> 8) + (address & 0xff);

      /* a zero? */
      b += 2;

      /* the data */
      if (num_bytes) {
        int value;

        while (num_bytes--) {
          value = hex_byte(b);
          b += 2;
          rom_write(address++, value);
          check += value;
        }

        if (address > high) high = address;

        /* the checksum */
        value = hex_byte(b);
        if (((0x100 - check) & 0xff) != value)
          return -1;
      }
    }
  }

  return high; /* returns highest address loaded + 1 */
}

static void trs_load_compiled_rom(int address, int size, const Uint8 rom[])
{
  int i;

  for (i = 0; i < size; i++)
    rom_write(address++, rom[i]);

  trs_rom_size = address;
}

static int trs_load_rom(const char *filename)
{
  FILE *program;
  int c;

  if (filename[0] == 0)
    return -1;

  if ((program = fopen(filename, "rb")) == NULL) {
    file_error("load ROM file '%s'", filename);
    return -1;
  }

  c = getc(program);
  if (c == ':') {
    /* Assume Intel hex format */
    rewind(program);
    trs_rom_size = load_hex(program);
    fclose(program);
    if (trs_rom_size == -1) {
      error("ROM file '%s' not in Intel hex format", filename);
      return -1;
    } else
      return 0;
  } else if (trs_model >= 3 && (c == 1 || c == 5)) {
    /* Assume MODELA/III file */
    Uint8 loadrom[Z80_ADDRESS_LIMIT] = { 0 };

    rewind(program);
    if (load_cmd(program, loadrom, NULL) == LOAD_CMD_OK) {
      trs_rom_size = Z80_ADDRESS_LIMIT;
      while (trs_rom_size > 0) {
        if (loadrom[--trs_rom_size] != 0) {
          trs_rom_size++;
          break;
        }
      }
      fclose(program);

      if (trs_rom_size > 0x3800) {
        error("MODELA/III file '%s' size %d exceeds 14 kB", filename, trs_rom_size);
        return -1;
      } else {
        trs_load_compiled_rom(0, trs_rom_size, loadrom);
        return 0;
      }
    } else {
      /* Guess it wasn't one */
      rewind(program);
      c = getc(program);
    }
  }

  /* Assume raw binary */
  trs_rom_size = 0;

  while (c != EOF) {
    rom_write(trs_rom_size++, c);
    c = getc(program);
  }

  return 0;
}

int trs_load_cmd(const char *filename)
{
  FILE *program;
  int entry;

  if ((program = fopen(filename, "rb")) == NULL) {
    file_error("load CMD file '%s'", filename);
    return -1;
  }

  if (load_cmd(program, NULL, &entry) == LOAD_CMD_OK) {
    debug("entry point of '%s': 0x%x (%d) ...\n", filename, entry, entry);
    if (entry >= 0)
      Z80_PC = entry;
  } else {
    error("unknown CMD format: '%s'", filename);
    fclose(program);
    return -1;
  }

  fclose(program);
  return 0;
}

void trs_rom_init(void)
{
  switch (trs_model) {
    case 1:
      if (trs_load_rom(romfile1) != 0)
        trs_load_compiled_rom(0, sizeof(trs_fakerom), trs_fakerom);

      if (stringy)
        trs_load_compiled_rom(0x3000, sizeof(trs_romesf), trs_romesf);

      /* Do not overwrite memory mapped disk I/O */
      if (trs_rom_size > 0x37E0 && trs_disk_controller)
          trs_rom_size = 0x37E0;

      if (trs_hd_boot)
        trs_boot_hd();
      break;
    case 3:
    case 4:
      if (trs_load_rom(romfile3) != 0)
        trs_load_compiled_rom(0, sizeof(trs_fakerom), trs_fakerom);
      break;
    case 5:
      if (trs_load_rom(romfile4p) != 0)
        trs_load_compiled_rom(0, sizeof(trs_rom4p), trs_rom4p);
      break;
  }

  /* Limit ROM size to maximum of 14 kB */
  if (trs_rom_size > 0x3800)
      trs_rom_size = 0x3800;
}