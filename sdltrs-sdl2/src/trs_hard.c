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
 * Emulation of the Radio Shack TRS-80 Model I/III/4/4P
 * hard disk controller.  This is a Western Digital WD1000/WD1010
 * mapped at ports 0xc8-0xcf, plus control registers at 0xc0-0xc1.
 */

#include <errno.h>
#include <string.h>
#include "error.h"
#include "reed.h"
#include "trs.h"
#include "trs_hard.h"
#include "trs_imp_exp.h"
#include "trs_state_save.h"

#define HARDDEBUG1 (1 << 2)  /* show detail on all port i/o */
#define HARDDEBUG2 (1 << 3)  /* show all commands */
#define SECTORSIZE (1024)

/* Private types and data */

/* Structure describing one drive */
typedef struct {
  FILE* file;
  char filename[FILENAME_MAX];
  /* Values decoded from rhh */
  int writeprot;
  int cyls;  /* cyls per drive */
  int heads; /* tracks per cyl */
  int secs;  /* secs per track */
} Drive;

/* Structure describing controller state */
typedef struct {
  /* Controller present?  Yes if we have any drives, no if none */
  int present;

  /* Controller register images */
  Uint8  control;
  Uint8  data;
  Uint8  error;
  Uint8  seccnt;
  Uint8  secnum;
  Uint16 secsize;
  Uint16 cyl;
  Uint8  drive;
  Uint8  head;
  Uint8  sdh;
  Uint8  status;
  Uint8  command;
  Uint8  secbuf[SECTORSIZE];

  /* Number of bytes already done in current read/write */
  int bytesdone;

  /* Drive geometries and files */
  Drive d[TRS_HARD_MAXDRIVES];
} State;

static State state;

/* Forward */
static void hard_error(int error);
static int  hard_data_in(void);
static void hard_data_out(int value);
static void hard_restore(void);
static void hard_read(int cmd);
static void hard_write(int cmd);
static void hard_verify(void);
static void hard_format(void);
static void hard_init(int cmd);
static void hard_seek(void);
static int  open_drive(int drive);
static int  find_sector(int newstatus);

#ifdef ZBX
static const char *hard_cmd(int port)
{
  switch (port) {
    case TRS_HARD_WP:
      return "WP";
    case TRS_HARD_CONTROL:
      return "CONTROL";
    case TRS_HARD_DATA:
      return "DATA";
    case TRS_HARD_PRECOMP:
      return "PRECOMP/ERROR";
    case TRS_HARD_SECCNT:
      return "SECCNT";
    case TRS_HARD_SECNUM:
      return "SECNUM";
    case TRS_HARD_CYLLO:
      return "CYLLO";
    case TRS_HARD_CYLHI:
      return "CYLHI";
    case TRS_HARD_SDH:
      return "SDH";
    case TRS_HARD_COMMAND:
      return "COMMAND/STATUS";
    default:
      return "";
  }
}

static void hard_debug(const char *cmd)
{
  debug("%s: drive:%d, head:%d, cyl:%4d, sec:%3d/%3d\n",
    cmd, state.drive, state.head, state.cyl, state.secnum, state.seccnt);
}

void trs_hard_debug(void)
{
  int i;

  puts("Hard disk controller state:");
  printf("  hard drive:%d, head:%d, cyl:%d, sec:%d/%d, secsize:%d\n",
      state.drive, state.head, state.cyl, state.secnum, state.seccnt, state.secsize);
  printf("  status:0x%02X, command:0x%02X, control:0x%02X, data:0x%02X, error:0x%02X\n",
      state.status, state.command, state.control, state.data, state.error);

  for (i = 0; i < TRS_HARD_MAXDRIVES; i++) {
    if (state.d[i].file) {
      printf("\nhard%d: '%s'\n", i, state.d[i].filename);
      printf("\theads %d, cyls %4d, secs %4d, writeprot %d\n",
          state.d[i].heads, state.d[i].cyls, state.d[i].secs, state.d[i].writeprot);
    }
  }
}
#endif

/* Powerup or reset button */
void trs_hard_init(int poweron)
{
  state.control = 0;
  state.data = 0;
  state.error = 0;
  state.secnum = 0;
  state.secsize = 0;
  state.cyl = 0;
  state.drive = 0;
  state.head = 0;
  state.sdh = 0;
  state.command = 0;
  memset(state.secbuf, 0, SECTORSIZE);

  if (poweron) {
    int i;

    state.present = 0;
    state.seccnt = 0;
    state.status = 0;

    for (i = 0; i < TRS_HARD_MAXDRIVES; i++) {
      state.d[i].writeprot = 0;
      state.d[i].cyls = 0;
      state.d[i].heads = 0;
      state.d[i].secs = 0;

      if (open_drive(i) == 0) state.present = 1;
    }
  }
}

void trs_hard_attach(int drive, const char *diskname)
{
  snprintf(state.d[drive].filename, FILENAME_MAX, "%s", diskname);

  if (open_drive(drive) < 0)
    trs_hard_remove(drive);

  trs_impexp_xtrshard_attach(drive);
}

void trs_hard_remove(int drive)
{
  if (state.d[drive].file != NULL)
    fclose(state.d[drive].file);

  trs_impexp_xtrshard_remove(drive);

  state.d[drive].filename[0] = 0;
  state.d[drive].file = NULL;
  state.d[drive].writeprot = 0;
  state.d[drive].cyls = 0;
  state.d[drive].heads = 0;
  state.d[drive].secs = 0;
}

const char*
trs_hard_getfilename(int unit)
{
  return state.d[unit].filename;
}

int
trs_hard_getwriteprotect(int unit)
{
  return state.d[unit].writeprot;
}

void
trs_hard_getgeometry(int unit, int *cyls, int *head, int *secs)
{
  if (state.d[unit].file) {
    *cyls = state.d[unit].cyls;
    *head = state.d[unit].heads;
    *secs = state.d[unit].secs;
  }
}

/* Read from an I/O port mapped to the controller */
int trs_hard_in(int port)
{
  int v = 0xff;

  if (state.present) {
    switch (port) {
    case TRS_HARD_WP: {
      int i;

      v = 0;
      for (i = 0; i < TRS_HARD_MAXDRIVES; i++) {
        if (state.d[i].writeprot) {
          v |= TRS_HARD_WPBIT(i) | TRS_HARD_WPSOME;
        }
      }
      break; }
    case TRS_HARD_CONTROL:
      v = state.control;
      break;
    case TRS_HARD_DATA:
      v = hard_data_in();
      break;
    case TRS_HARD_ERROR:
      v = state.error;
      break;
    case TRS_HARD_SECCNT:
      v = state.seccnt;
      break;
    case TRS_HARD_SECNUM:
      v = state.secnum;
      break;
    case TRS_HARD_CYLLO:
      v = state.cyl & 0xff;
      break;
    case TRS_HARD_CYLHI:
      v = (state.cyl >> 8) & 0xff;
      break;
    case TRS_HARD_SDH:
      v = state.sdh;
      break;
    case TRS_HARD_STATUS:
      v = state.status;
      break;
    default:
      break;
    }
  }
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG1)
    debug("[PC=%04X] trs_hard_in(%s) => %02X\n", Z80_PC, hard_cmd(port), v);
#endif
  return v;
}

/* Write to an I/O port mapped to the controller */
void trs_hard_out(int port, int value)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG1)
    debug("[PC=%04X] trs_hard_out(%s), %02X\n", Z80_PC, hard_cmd(port), value);
#endif
  switch (port) {
  case TRS_HARD_WP:
    break;
  case TRS_HARD_CONTROL:
    if (value & TRS_HARD_SOFTWARE_RESET)
      trs_hard_init(0);

    if (value & TRS_HARD_DEVICE_ENABLE)
      trs_hard_init(1);

    state.control = value;
    break;
  case TRS_HARD_DATA:
    hard_data_out(value);
    break;
  case TRS_HARD_PRECOMP:
    break;
  case TRS_HARD_SECCNT:
    state.seccnt = value;
    break;
  case TRS_HARD_SECNUM:
    state.secnum = value;
    break;
  case TRS_HARD_CYLLO:
    state.cyl = (state.cyl & 0xff00) | (value & 0x00ff);
    break;
  case TRS_HARD_CYLHI:
    state.cyl = (state.cyl & 0x00ff) | ((value << 8) & 0xff00);
    break;
  case TRS_HARD_SDH:
    state.sdh = value;
    state.secsize = 256 << ((value & TRS_HARD_SIZEMASK) >> TRS_HARD_SIZESHIFT);
    if (state.secsize > SECTORSIZE) state.secsize = 128;
    state.drive = (value & TRS_HARD_DRIVEMASK) >> TRS_HARD_DRIVESHIFT;
    state.head = (value & TRS_HARD_HEADMASK) >> TRS_HARD_HEADSHIFT;
#if 0
    if (open_drive(state.drive) < 0) state.status &= ~TRS_HARD_READY;
#else
    /* Ready, but perhaps not able!  This way seems to work better; it
     * avoids a long delay in the Model 4P boot ROM when there is no
     * unit 0. */
    state.status = TRS_HARD_READY | TRS_HARD_SEEKDONE;
#endif
    break;

  case TRS_HARD_COMMAND:
    state.bytesdone = 0;
    state.command = value;
    switch (value & TRS_HARD_CMDMASK) {
    case TRS_HARD_RESTORE:
      hard_restore();
      break;
    case TRS_HARD_READ:
      hard_read(value);
      break;
    case TRS_HARD_WRITE:
      hard_write(value);
      break;
    case TRS_HARD_VERIFY:
      hard_verify();
      break;
    case TRS_HARD_FORMAT:
      hard_format();
      break;
    case TRS_HARD_INIT:
      hard_init(value);
      break;
    case TRS_HARD_SEEK:
      hard_seek();
      break;
    default:
      error("trs_hard: unknown command 0x%02X", value);
      break;
    }
    break;

  default:
    break;
  }
}

static void hard_error(int error)
{
  state.status = TRS_HARD_READY | TRS_HARD_SEEKDONE | TRS_HARD_ERR;
  state.error  = error;
}

static void hard_restore(void)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    debug("hard_restore drive:%d\n", state.drive);
#endif
  state.cyl = 0;
  /*!! should anything else be zeroed? */
  state.status = TRS_HARD_READY | TRS_HARD_SEEKDONE;
}

static void hard_read(int cmd)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    hard_debug("hard_read");
#endif
  if (cmd & TRS_HARD_MULTI) {
    error("trs_hard: multi-sector read not supported (0x%02X)", cmd);
    hard_error(TRS_HARD_ABRTERR);
    return;
  }
  if (find_sector(TRS_HARD_READY | TRS_HARD_SEEKDONE | TRS_HARD_DRQ) == 0) {
    FILE *f = state.d[state.drive].file;

    if (f && fread(state.secbuf, 1, state.secsize, f) != state.secsize) {
      file_error("reading on hard%d", state.drive);
      hard_error(TRS_HARD_DATAERR); /* arbitrary choice */
    }
  }
}

static void hard_write(int cmd)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    hard_debug("hard_write");
#endif
  if (cmd & TRS_HARD_MULTI) {
    error("trs_hard: multi-sector write not supported (0x%02X)", cmd);
    hard_error(TRS_HARD_ABRTERR);
    return;
  }
  find_sector(TRS_HARD_READY | TRS_HARD_SEEKDONE | TRS_HARD_DRQ);
}

static void hard_verify(void)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    hard_debug("hard_verify");
#endif
  find_sector(TRS_HARD_READY | TRS_HARD_SEEKDONE);
}

static void hard_format(void)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    hard_debug("hard_format");
#endif
  /* !!should probably set up to read skew table here */
  state.status = TRS_HARD_READY | TRS_HARD_SEEKDONE;
}

static void hard_init(int cmd)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    hard_debug("hard_init");
#endif
  /* I don't know what this command does */
  error("trs_hard: init command (0x%02X) not implemented", cmd);
  state.status = TRS_HARD_READY | TRS_HARD_SEEKDONE;
}

static void hard_seek(void)
{
#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    hard_debug("hard_seek");
#endif
  find_sector(TRS_HARD_READY | TRS_HARD_SEEKDONE);
}

/*
 * 1) Make sure the file for the current drive is open.  If the file
 * cannot be opened, return 0 and set the controller error status.
 *
 * 2) If newly opening the file, establish the hardware write protect
 * status and geometry in the Drive structure.
 *
 * 3) Set the hardware write protect status and geometry in the Drive
 * structure.
 *
 * 4) Return 0 if OK, -1 if invalid header, errno value otherwise.
 */
static int open_drive(int drive)
{
  Drive *d = &state.d[drive];
  ReedHardHeader rhh;
  size_t res;
  int err = 0;
  int secs;

  if (d->filename[0] == 0)
    goto fail;

  if (d->file != NULL) {
    fclose(d->file);
    d->file = NULL;
  }
  /* First try opening for reading and writing */
  d->file = fopen(d->filename, "rb+");
  if (d->file == NULL) {
    if (errno == EACCES || errno == EROFS) {
    /* No luck, try for reading only */
      d->file = fopen(d->filename, "rb");
    }
    if (d->file == NULL) {
      file_error("open hard%d: '%s'", drive, d->filename);
      err = errno;
      goto fail;
    }
    d->writeprot = 1;
  } else {
    d->writeprot = 0;
  }

  /* Read in the Reed header and check some basic magic numbers (not all) */
  res = fread(&rhh, sizeof(rhh), 1, d->file);
  if (res != 1 || rhh.id1 != 0x56 || rhh.id2 != 0xcb || rhh.ver >= 0x20) {
    error("unrecognized hard%d drive image: '%s'", drive, d->filename);
    err = -1;
    goto fail;
  }

  if (rhh.flag1 & 0x80) d->writeprot = 1;

  /* Use the number of cylinders specified in the header */
  d->cyls  = rhh.cylhi << 8;   /* MSB */
  d->cyls += rhh.cyllo & 0xFF; /* LSB */

  secs = rhh.sec ? rhh.sec : 256;
  if (rhh.heads == 0) {
    /* header gives only secs/cyl. Compute number of heads from
       this and the assumed number of secs/track */

    /* use the number of secs/track that RSHARD requires */
    d->secs  = TRS_HARD_SEC_PER_TRK;
    d->heads = secs / TRS_HARD_SEC_PER_TRK;
  } else {
    /* header includes head count */
    d->heads = rhh.heads;
    d->secs  = secs / d->heads;
  }

#if ZBX
  if (trs_io_debug_flags & HARDDEBUG2)
    debug("open_drive:%d, cyls:%d, heads:%d, secs:%d\n",
          drive, d->cyls, d->heads, d->secs);
#endif

  if ((rhh.sec % d->secs) != 0 ||
      d->heads <= 0 || d->heads > TRS_HARD_MAXHEADS) {
    error("unusable geometry (%d heads/%d secs) in hard%d image: '%s'",
          d->heads, d->secs, drive, d->filename);
    err = -1;
    goto fail;
  }

  state.seccnt = d->secs;
  state.status = TRS_HARD_READY | TRS_HARD_SEEKDONE;
  return 0;

fail:
  if (d->file) fclose(d->file);
  d->file = NULL;
  d->filename[0] = 0;
  hard_error(TRS_HARD_NFERR);
  return err;
}

/*
 * Check whether the current position is in bounds for the geometry.
 * If not, set the controller error status and return -1.  If so, fseek
 * the file to the start of the current sector and set the controller
 * status to newstatus and return 0.
 */
static int find_sector(int newstatus)
{
  const Drive *d = &state.d[state.drive];

  if (d->file == NULL) return -1;

  if (/**state.cyl >= d->cyls ||**/ /* ignore this limit */
      state.head >= d->heads ||
      state.secnum > d->secs /* allow 0-origin or 1-origin */ ) {
    error("hard%d: requested cyl:%d, head:%d, sec:%d (cyls:%d, heads:%d, secs:%d)",
        state.drive, state.cyl, state.head, state.secnum, d->cyls, d->heads, d->secs);
    hard_error(TRS_HARD_NFERR);
    return -1;
  }

  if (fseek(d->file,
    sizeof(ReedHardHeader) +
    state.secsize * (state.cyl * d->heads * d->secs
                  +  state.head * d->secs
                  + (state.secnum % d->secs)), 0) != 0) {
      file_error("hard%d: fseek '%s'", state.drive, d->filename);
      hard_error(TRS_HARD_NFERR);
      return -1;
  }

  state.status = newstatus;

  if (trs_show_led)
    trs_hard_led(state.drive, 1);

  return 0;
}

static int hard_data_in(void)
{
  if ((state.command & TRS_HARD_CMDMASK) == TRS_HARD_READ &&
      (state.status & TRS_HARD_ERR) == 0) {
    if (state.bytesdone < state.secsize) {
      state.data = state.secbuf[state.bytesdone];
      state.bytesdone++;
    }
  }
  return state.data;
}

static void hard_data_out(int value)
{
  state.data = value;

  if ((state.command & TRS_HARD_CMDMASK) == TRS_HARD_WRITE &&
      (state.status & TRS_HARD_ERR) == 0) {
    if (state.bytesdone < state.secsize) {
      state.secbuf[state.bytesdone] = value;
      state.bytesdone++;
      if (state.bytesdone == state.secsize) {
        FILE *f = state.d[state.drive].file;

        if (f && fwrite(state.secbuf, 1, state.secsize, f) != state.secsize) {
          file_error("writing on hard%d", state.drive);
          hard_error(TRS_HARD_DATAERR); /* arbitrary choice */
        }
      }
    }
  }
}

static void trs_save_harddrive(FILE *file, Drive *d)
{
  int file_not_null = (d->file != NULL);

  trs_save_int(file, &file_not_null, 1);
  trs_save_filename(file, d->filename);
  trs_save_int(file, &d->writeprot, 1);
  trs_save_int(file, &d->cyls, 1);
  trs_save_int(file, &d->heads, 1);
  trs_save_int(file, &d->secs, 1);
}

static void trs_load_harddrive(FILE *file, Drive *d)
{
  int file_not_null;

  trs_load_int(file, &file_not_null, 1);

  if (file_not_null)
    d->file = (FILE *) 1;
  else
    d->file = NULL;

  trs_load_filename(file, d->filename);
  trs_load_int(file, &d->writeprot, 1);
  trs_load_int(file, &d->cyls, 1);
  trs_load_int(file, &d->heads, 1);
  trs_load_int(file, &d->secs, 1);
}

void trs_hard_save(FILE *file)
{
  int i;

  trs_save_int(file, &state.present, 1);
  trs_save_uint8(file, &state.control, 1);
  trs_save_uint8(file, &state.data, 1);
  trs_save_uint8(file, &state.error, 1);
  trs_save_uint8(file, &state.seccnt, 1);
  trs_save_uint8(file, &state.secnum, 1);
  trs_save_uint16(file, &state.secsize, 1);
  trs_save_uint16(file, &state.cyl, 1);
  trs_save_uint8(file, &state.drive, 1);
  trs_save_uint8(file, &state.head, 1);
  trs_save_uint8(file, &state.sdh, 1);
  trs_save_uint8(file, &state.status, 1);
  trs_save_uint8(file, &state.command, 1);
  trs_save_uint8(file, state.secbuf, SECTORSIZE);
  trs_save_int(file, &state.bytesdone, 1);

  for (i = 0; i < TRS_HARD_MAXDRIVES; i++)
    trs_save_harddrive(file, &state.d[i]);
}

void trs_hard_load(FILE *file)
{
  int i;

  for (i = 0; i < TRS_HARD_MAXDRIVES; i++) {
    if (state.d[i].file != NULL)
      fclose(state.d[i].file);
  }

  trs_load_int(file, &state.present, 1);
  trs_load_uint8(file, &state.control, 1);
  trs_load_uint8(file, &state.data, 1);
  trs_load_uint8(file, &state.error, 1);
  trs_load_uint8(file, &state.seccnt, 1);
  trs_load_uint8(file, &state.secnum, 1);
  trs_load_uint16(file, &state.secsize, 1);
  trs_load_uint16(file, &state.cyl, 1);
  trs_load_uint8(file, &state.drive, 1);
  trs_load_uint8(file, &state.head, 1);
  trs_load_uint8(file, &state.sdh, 1);
  trs_load_uint8(file, &state.status, 1);
  trs_load_uint8(file, &state.command, 1);
  trs_load_uint8(file, state.secbuf, SECTORSIZE);
  trs_load_int(file, &state.bytesdone, 1);

  for (i = 0; i < TRS_HARD_MAXDRIVES; i++) {
    trs_load_harddrive(file, &state.d[i]);

    if (state.d[i].file != NULL) {
      state.d[i].file = fopen(state.d[i].filename, "rb+");
      if (state.d[i].file == NULL) {
        state.d[i].file = fopen(state.d[i].filename, "rb");
        if (state.d[i].file == NULL) {
          file_error("load hard%d: '%s'", i, state.d[i].filename);
          state.d[i].filename[0] = 0;
          state.d[i].writeprot = 0;
          continue;
        }
        state.d[i].writeprot = 1;
      } else {
        state.d[i].writeprot = 0;
      }
    }
  }
}
