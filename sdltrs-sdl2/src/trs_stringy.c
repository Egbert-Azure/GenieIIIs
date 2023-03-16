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
 * Emulate Exatron stringy floppy.
 *
 * Still needs more work; see XXX comments below.
 *
 * XXX Check if I am exactly duplicating TRS32 output now.  However,
 * TRS32 seems to drop one bit on wrap, which might be a bug in
 * TRS32 that I don't need to duplicate.
 */

#include <assert.h>
#include <errno.h>
#include <string.h>
#include <stdio.h>
#include <stdlib.h>
#include <sys/types.h>
#include <unistd.h>
#include "error.h"
#include "trs.h"
#include "trs_state_save.h"
#include "trs_stringy.h"

#define STRINGYDEBUG_IN 0
#define STRINGYDEBUG_OUT 0
#define STRINGYDEBUG_STATE 0

/* Input port bits */
#define STRINGY_WRITE_PROT  0x01
#define STRINGY_END_OF_TAPE 0x04
#define STRINGY_NO_WAFER    0x08  /* is that what this bit means? */
#define STRINGY_FLUX        0x80
#define STRINGY_NO_UNIT     0xff

/* Output port bits */
#define STRINGY_MOTOR_ON    0x01
#define STRINGY_WRITE_GATE  0x04
/*#define STRINGY_FLUX      0x80*/

#define STRINGY_MAX_UNITS 8

#define STRINGY_CELL_WIDTH 124 /* in t-states */
#define STRINGY_LEN_DEFAULT (64 * 1024 * 2 * 9 / 8) /* 64K + gaps/leaders XXX? */
#define STRINGY_EOT_DEFAULT 60 /* a good value per MKR */

#define STRINGY_FMT_DEBUG 1
#define STRINGY_FMT_ESF 2

#define STRINGY_FMT_DEFAULT STRINGY_FMT_ESF

int stringy;

typedef long stringy_pos_t;

typedef struct {
  char name[FILENAME_MAX];
  FILE *file;
  stringy_pos_t length;
  stringy_pos_t eotWidth;
  stringy_pos_t pos;
  tstate_t pos_time;
  stringy_pos_t flux_change_pos;
  int flux_change_to;
  Uint8 in_port;
  Uint8 out_port;
  Uint8 format;
  /* for esf format: */
  long esf_bytelen;
  long esf_bytepos;
  Uint8 esf_bytebuf;
  Uint8 esf_bitpos;
#if STRINGYDEBUG_IN
  int prev_in_port;
#endif
} stringy_info_t;

static stringy_info_t stringy_info[STRINGY_MAX_UNITS];

/*
 * .esf file format used by TRS32.
 */
static const char stringy_esf_magic[4] = "ESF\x1a";
static const Uint8 stringy_esf_header_length = 12;
static const Uint8 stringy_esf_write_protected = 1;
/*
struct {
  char magic[4] = stringy_esf_magic;
  Uint8 headerLength = 12;  // length of this header, in bytes
  Uint8 flags;              // bit 0: write protected; others reserved
  Uint16 leaderLength = 60; // little endian, in bit cells
  Uint32 length;            // little endian, in bytes
  Uint8 data[length];
}
 */

static const char stringy_debug_header[] = "xtrs stringy debug %ld %ld %d\n";

#define STRINGY_STOPPED 0
#define STRINGY_READING 1
#define STRINGY_WRITING 2

static int
stringy_state(int out_port)
{
  if ((out_port & STRINGY_MOTOR_ON) == 0) return STRINGY_STOPPED;
  if ((out_port & STRINGY_WRITE_GATE) != 0) return STRINGY_WRITING;
  return STRINGY_READING;
}

/*
 * Create a blank virtual stringy floppy wafer with specified parameters.
 * Returns 0 if OK, errno value otherwise.
 */
static int
stringy_create_with(const char *name,
		    int format,
		    Uint32 lengthBytes, /* data length in bytes */
		    Uint32 eotCells,    /* leader length in bit cells */
		    int writeProt)
{
  FILE *f;
  int ires;
  size_t sres;

  f = fopen(name, "w");
  if (f == NULL) {
    error("failed to create Wafer Image '%s': %s", name, strerror(errno));
    return errno;
  }

  switch (format) {
  case STRINGY_FMT_DEBUG:
    ires = fprintf(f, stringy_debug_header,
		   (stringy_pos_t)lengthBytes * STRINGY_CELL_WIDTH * 8,
		   (stringy_pos_t)eotCells * STRINGY_CELL_WIDTH,
		   writeProt);
    if (ires < 0) goto error;
    break;

  case STRINGY_FMT_ESF:
    sres = fwrite(stringy_esf_magic, sizeof(stringy_esf_magic), 1, f);
    if (sres < 1) goto error;
    ires = fputc(stringy_esf_header_length, f);
    if (ires < 0) goto error;
    ires = fputc(writeProt ? stringy_esf_write_protected : 0, f);
    if (ires < 0) goto error;
    ires = put_twobyte(eotCells, f);
    if (ires < 0) goto error;
    ires = put_fourbyte(lengthBytes, f);
    if (ires < 0) goto error;
    break;

  default:
    error("unknown wafer image type on write");
    fclose(f);
    return -1;
  }

  ires = fclose(f);
  if (ires < 0) return errno;

  return 0;

error:
  fclose(f);
  return errno;
}

/*
 * Create a blank virtual stringy floppy wafer with default parameters.
 * Returns 0 if OK, errno value otherwise.
 */
int
stringy_create(const char *name)
{
  /*
   * Default parameters
   */
  return stringy_create_with(name,
			     STRINGY_FMT_DEFAULT,
			     STRINGY_LEN_DEFAULT,
			     STRINGY_EOT_DEFAULT,
			     FALSE);
}

static int
stringy_read_esf_header(stringy_info_t *s)
{
  char magic[sizeof(stringy_esf_magic)];
  Uint8 headerLength, flags;
  Uint16 eotWidth;
  size_t sres;
  int ires;
  Uint32 len;

  if (s->file == NULL)
    return -1;

  rewind(s->file);
  sres = fread(magic, sizeof(stringy_esf_magic), 1, s->file);
  if (sres < 1) return -1;
  if (memcmp(magic, stringy_esf_magic,
	     sizeof(stringy_esf_magic)) != 0) return -1;
  ires = fgetc(s->file);
  if (ires < 0) return ires;
  headerLength = ires;
  if (headerLength != stringy_esf_header_length) return -1;
  ires = fgetc(s->file);
  if (ires < 0) return ires;
  flags = ires;
  ires = get_twobyte(&eotWidth, s->file);
  if (ires < 0) return ires;
  ires = get_fourbyte(&len, s->file);
  if (ires < 0) return ires;

  s->format = STRINGY_FMT_ESF;
  s->length = len * STRINGY_CELL_WIDTH * 8;
  s->eotWidth = eotWidth * STRINGY_CELL_WIDTH;
  s->in_port = (s->in_port & ~STRINGY_WRITE_PROT) |
    ((flags & stringy_esf_write_protected) ? STRINGY_WRITE_PROT : 0);
  fseek(s->file, 0, SEEK_CUR);
  s->esf_bytelen = len;
  s->esf_bytepos = 0;
  s->esf_bytebuf = 0;
  s->esf_bitpos = 0;
  return 0;
}

static int
stringy_read_debug_header(stringy_info_t *s)
{
  int ires;
  stringy_pos_t len, eotw;
  int wprot;

  if (s->file == NULL)
    return -1;

  rewind(s->file);
  ires = fscanf(s->file, stringy_debug_header, &len, &eotw, &wprot);
  if (ires < 3) return -1;
  s->format = STRINGY_FMT_DEBUG;
  s->length = len;
  s->eotWidth = eotw;
  s->in_port = (s->in_port & ~STRINGY_WRITE_PROT) |
    (wprot ? STRINGY_WRITE_PROT : 0);
  fseek(s->file, 0, SEEK_CUR);
  return 0;
}

static int
stringy_read_header(stringy_info_t *s)
{
  int ires;

  ires = stringy_read_esf_header(s);
  if (ires >= 0) return ires;

  ires = stringy_read_debug_header(s);
  return ires;
}

/* Returns 0 if OK, -1 if invalid header, errno value otherwise. */
static int
stringy_change(int unit)
{
  stringy_info_t *s = &stringy_info[unit];
  int ires;

  if (s->file) {
    fclose(s->file);
    s->file = NULL;
  }
  if (s->name[0] == 0) {
    s->in_port = STRINGY_NO_WAFER;
    return 0;
  }

  s->file = fopen(s->name, "r+");
  if (s->file == NULL) {
    if (errno == EACCES || errno == EROFS) {
      s->file = fopen(s->name, "r");
    }
    if (s->file == NULL) {
      s->in_port = STRINGY_NO_WAFER;
      return errno;
    }
    s->in_port = STRINGY_WRITE_PROT;
  } else {
    s->in_port = 0;
  }  
  s->out_port = 0;

  ires = stringy_read_header(s);

  s->pos = 0;
  s->pos_time = z80_state.t_count;
  /* XXX is the following right? */
  s->flux_change_pos = 0;
  s->flux_change_to = 1;

  return ires;
}

void
stringy_change_all(void)
{
  int i;

  for (i = 0; i < STRINGY_MAX_UNITS; i++) {
    stringy_change(i);
  }
}

const char *
stringy_get_name(int drive)
{
  return stringy_info[drive].name;
}

int
stringy_get_writeprotect(int drive)
{
  return stringy_info[drive].in_port & STRINGY_WRITE_PROT;
}

/* Returns 0 if OK, -1 if invalid header, errno value otherwise. */
int
stringy_insert(int drive, const char *name)
{
  snprintf(stringy_info[drive].name, FILENAME_MAX, "%s", name);
  return stringy_change(drive);
}

void
stringy_remove(int drive)
{
  stringy_info[drive].file = NULL;
  stringy_info[drive].name[0] = 0;
  stringy_change(drive);
}

/* One-time initialization */
void
stringy_init(void)
{
  stringy_change_all();
}

/* Stringy controller hardware reset */
void
stringy_reset(void)
{
  /* Nothing to do (?) */
}

static void
stringy_byte_flush(stringy_info_t *s)
{
  int ires;
  Uint8 mask;

  if (s->format != STRINGY_FMT_ESF ||
      stringy_state(s->out_port) != STRINGY_WRITING ||
      s->esf_bitpos == 0) return;

  fseek(s->file, 0, SEEK_CUR);
  ires = fgetc(s->file);
  if (ires == EOF) {
    ires = 0;
  }
  fseek(s->file, -1, SEEK_CUR);
  mask = 0xff << s->esf_bitpos;
  s->esf_bytebuf = (ires & mask) | (s->esf_bytebuf & ~mask);
  if (fputc(s->esf_bytebuf, s->file) == EOF)
    error("stringy byte flush: %s", strerror(errno));
  fseek(s->file, -1, SEEK_CUR);
}

static void
stringy_bit_write(stringy_info_t *s, int flux)
{
  s->esf_bytebuf &= ~(1 << s->esf_bitpos);
  s->esf_bytebuf |= flux << s->esf_bitpos;
  s->esf_bitpos++;
  if (s->esf_bitpos == 8) {
    if (fputc(s->esf_bytebuf, s->file) == EOF)
      error("stringy bit write: %s", strerror(errno));
    if (++s->esf_bytepos >= s->esf_bytelen) {
      fseek(s->file, stringy_esf_header_length, SEEK_SET);
      s->esf_bytepos = 0;
    }
    s->esf_bitpos = 0;
    s->esf_bytebuf = 0;
  }
}

/*
 * flux = new flux state
 * delta = time in *previous* state
 */
static void
stringy_flux_write(stringy_info_t *s, int flux, stringy_pos_t delta)
{
  int cells;
  stringy_pos_t adjustment;

  switch (s->format) {
  case STRINGY_FMT_DEBUG:
    fprintf(s->file, "%u %lu\n", flux, delta);
    break;
  case STRINGY_FMT_ESF:
    cells = (delta + 1) / STRINGY_CELL_WIDTH;
    if (cells > 3) {
      /*
       * XXX Why is this needed?  Otherwise we get a huge run of cells
       * right at the start, and some later too -- see .debug format
       * output.  Did the real hardware/firmware leave gaps there
       * (maybe for motor startup?), or is this a bug in xtrs
       * emulation?  TRS32 shows no such gaps in its output format, so
       * we avoid them too by crushing them out here.
       */
      cells = 3;
    }
    adjustment = delta - cells * STRINGY_CELL_WIDTH;
    while (cells--) {
      stringy_bit_write(s, flux);
    }
    /*
     * Adjust the position to exactly match the nominal size of the
     * number of cells written.  This is necessary so that the end of
     * tape mark will be seen at exactly the same place when reading
     * the tape back as it was seen when writing the tape out.
     */
    s->pos -= adjustment;
    break;
  }
}

static int
stringy_bit_read(stringy_info_t *s, int *bit)
{
  int ires;

  if (s->esf_bitpos == 0) {
    if (s->esf_bytepos++ >= s->esf_bytelen) {
      fseek(s->file, stringy_esf_header_length, SEEK_SET);
      s->esf_bytepos = 0;
    }
    if ((ires = fgetc(s->file)) == EOF)
      if (ferror(s->file) != 0) {
        error("stringy bit read: %s", strerror(errno));
        clearerr(s->file);
        return FALSE;
    }
    if (ires < 0) {
      ires = 0;
    }
    s->esf_bytebuf = ires;
  }
  *bit = (s->esf_bytebuf & (1 << s->esf_bitpos)) != 0;
  s->esf_bitpos = (s->esf_bitpos + 1) % 8;
  return TRUE;
}

static int
stringy_flux_read(stringy_info_t *s, int *flux, stringy_pos_t *delta)
{
  int bres;
  int bit;

  switch(s->format) {
  case STRINGY_FMT_DEBUG:
    bres = fscanf(s->file, "%d %ld\n", flux, delta);
    if (bres == EOF) {
      if (ferror(s->file)) return FALSE;
      stringy_read_debug_header(s);
      bres = fscanf(s->file, "%d %ld\n", flux, delta);
      if (bres == EOF && ferror(s->file)) return FALSE;
    }
    return TRUE;

  case STRINGY_FMT_ESF:
    bres = stringy_bit_read(s, &bit);
    if (!bres) return bres;
    /*
     * This calls for some explanation.  Our caller wants the delta to
     * the next flux change and the resulting flux value, as in "xtrs
     * stringy debug" format.  We don't really know either; we only
     * know the current flux value and that it will continue for
     * STRINGY_CELL_WIDTH.  So we fib and say that the value will
     * change to the opposite of the current value after
     * STRINGY_CELL_WIDTH.  This is actually good enough for our
     * caller, which will request enough additional samples to get
     * correct information before using the information.
     */
    *flux = !bit;
    *delta = STRINGY_CELL_WIDTH;
    return TRUE;
  }

  return FALSE;
}

static void
stringy_update_pos(stringy_info_t *s)
{
  if (stringy_state(s->out_port) == STRINGY_STOPPED) {
    return;
  }

  s->pos += z80_state.t_count - s->pos_time;
  s->pos_time = z80_state.t_count;

  if (s->pos >= s->length) {
    s->pos -= s->length;
    s->flux_change_pos -= s->length;
  }
  if (s->pos >= s->length - s->eotWidth) {
    s->in_port |= STRINGY_END_OF_TAPE;
  } else {
    s->in_port &= ~STRINGY_END_OF_TAPE;
  }
}

int
stringy_in(int unit)
{
  stringy_info_t *s = &stringy_info[unit];
  int ret;

  if (s->in_port & STRINGY_NO_WAFER) {
    ret = s->in_port;
    goto done;
  }

  stringy_update_pos(s);

  if (stringy_state(s->out_port) == STRINGY_READING) {

    while (s->pos >= s->flux_change_pos) {
      int flux;
      stringy_pos_t delta;

      s->in_port = (s->in_port & ~STRINGY_FLUX) |
	(s->flux_change_to ? 0 : STRINGY_FLUX);

      if (!stringy_flux_read(s, &flux, &delta)) {
	break;
      }

      s->flux_change_to = flux;
      s->flux_change_pos += delta;
    }
  }
  ret = s->in_port;

 done:
#if STRINGYDEBUG_IN
  if (ret != s->prev_in_port) {
    debug("stringy_in(%d) -> %d\n", unit, ret);
    s->prev_in_port = ret;
  }
#endif  
  return ret;
}

void
stringy_out(int unit, int value)
{
  stringy_info_t *s = &stringy_info[unit];
  int old_state, new_state;

  if (s->in_port & STRINGY_NO_WAFER) return;

  old_state = stringy_state(s->out_port);
  new_state = stringy_state(value);
  stringy_update_pos(s);

#if STRINGYDEBUG_STATE
  if (old_state != new_state) {
    debug("stringy state %d -> %d, new pos %ld\n",
	  old_state, new_state, s->pos);
  }
#endif

  if (old_state == STRINGY_STOPPED &&
      new_state != STRINGY_STOPPED) {

    if (1 /*s->in_port & STRINGY_END_OF_TAPE*/) {
      /*
       * Start at the beginning after motor off/on.  This surely
       * doesn't emulate real hardware accurately, but I get "tape
       * too short" errors on @SAVE unless I do it at least in the
       * case where STRINGY_END_OF_TAPE is currently set.  Maybe the
       * ROM does its "rewind" thing, shuts off the motor, and relies
       * on tape coasting past the EOT marker during the off/on
       * transition.  XXX Read ROM code to try to understand this.  As
       * long as I'm doing this sometimes, doing it always is no
       * greater compromise.  And doing it always makes the state
       * change positions more reproducible, which is better for
       * regression testing and such.
       * 
       */
      s->pos = 0;
      s->in_port &= ~STRINGY_END_OF_TAPE;
      stringy_read_header(s);
    }

    s->pos_time = z80_state.t_count;
    /* XXX Is the following right? */
    s->in_port &= ~STRINGY_FLUX;
    s->flux_change_pos = s->pos;
    s->flux_change_to = 1;
  }

  if (old_state != STRINGY_WRITING &&
      new_state == STRINGY_WRITING) {
    if (s->format == STRINGY_FMT_DEBUG) {
      /* Debug format can't handle overwriting */
      fflush(s->file);
      assert(ftruncate(fileno(s->file), ftell(s->file)) == 0);
    }
    fseek(s->file, 0, SEEK_CUR);
    stringy_flux_write(s, 1, 0); /* XXX needed?  bad? */
  }

  if (old_state == STRINGY_WRITING) {
    if (((s->out_port ^ value) & STRINGY_FLUX) != 0 ||
	new_state != STRINGY_WRITING) {
      stringy_flux_write(s, (value & STRINGY_FLUX) != 0,
			 s->pos - s->flux_change_pos);
      s->flux_change_pos = s->pos;
    }

    if (new_state != STRINGY_WRITING) {
      stringy_byte_flush(s);
      fflush(s->file);
    }
  }

  s->out_port = value;

#if STRINGYDEBUG_OUT
  debug("stringy_out(%d, %d)\n", unit, value);
  s->prev_in_port = -1;
#endif
}

static void trs_save_stringy(FILE *file, stringy_info_t *d)
{
  int file_not_null = (d->file != NULL);

  trs_save_int(file, &file_not_null, 1);
  trs_save_filename(file, d->name);
  trs_save_uint64(file, (Uint64 *)&d->length, 1);
  trs_save_uint64(file, (Uint64 *)&d->eotWidth, 1);
  trs_save_uint64(file, (Uint64 *)&d->pos, 1);
  trs_save_uint64(file, (Uint64 *)&d->pos_time, 1);
  trs_save_uint64(file, (Uint64 *)&d->flux_change_pos, 1);
  trs_save_int(file, &d->flux_change_to, 1);
  trs_save_uint8(file, &d->in_port, 1);
  trs_save_uint8(file, &d->out_port, 1);
  trs_save_uint8(file, &d->format, 1);
  trs_save_uint64(file, (Uint64 *)&d->esf_bytelen, 1);
  trs_save_uint64(file, (Uint64 *)&d->esf_bytepos, 1);
  trs_save_uint8(file, &d->esf_bytebuf, 1);
  trs_save_uint8(file, &d->esf_bitpos, 1);
}

static void trs_load_stringy(FILE *file, stringy_info_t *d)
{
  int file_not_null;

  trs_load_int(file, &file_not_null, 1);

  if (file_not_null)
    d->file = (FILE *) 1;
  else
    d->file = NULL;

  trs_load_filename(file, d->name);
  trs_load_uint64(file, (Uint64 *)&d->length, 1);
  trs_load_uint64(file, (Uint64 *)&d->eotWidth, 1);
  trs_load_uint64(file, (Uint64 *)&d->pos, 1);
  trs_load_uint64(file, (Uint64 *)&d->pos_time, 1);
  trs_load_uint64(file, (Uint64 *)&d->flux_change_pos, 1);
  trs_load_int(file, &d->flux_change_to, 1);
  trs_load_uint8(file, &d->in_port, 1);
  trs_load_uint8(file, &d->out_port, 1);
  trs_load_uint8(file, &d->format, 1);
  trs_load_uint64(file, (Uint64 *)&d->esf_bytelen, 1);
  trs_load_uint64(file, (Uint64 *)&d->esf_bytepos, 1);
  trs_load_uint8(file, &d->esf_bytebuf, 1);
  trs_load_uint8(file, &d->esf_bitpos, 1);
}

void trs_stringy_save(FILE *file)
{
  int i;

  trs_save_int(file, &stringy, 1);

  for (i = 0; i < STRINGY_MAX_UNITS; i++)
    trs_save_stringy(file, &stringy_info[i]);
}

void trs_stringy_load(FILE *file)
{
  int i;

  trs_load_int(file, &stringy, 1);

  for (i = 0; i < STRINGY_MAX_UNITS; i++) {
    trs_load_stringy(file, &stringy_info[i]);
    if (stringy_info[i].file != NULL) {
      stringy_info[i].file = fopen(stringy_info[i].name, "rb+");
      if (stringy_info[i].file == NULL) {
        stringy_info[i].file = fopen(stringy_info[i].name, "rb");
        if (stringy_info[i].file == NULL) {
          error("failed to load wafer%d: '%s': %s", i, stringy_info[i].name,
              strerror(errno));
          stringy_info[i].name[0] = 0;
          stringy_info[i].in_port = 0;
          continue;
        }
        stringy_info[i].in_port |= 1 << 0;
      } else {
        stringy_info[i].in_port &= ~(1 << 0);;
      }
    }
  }
}
