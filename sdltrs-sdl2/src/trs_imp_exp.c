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
 * trs_imp_exp.c
 *
 * Features to make transferring files into and out of the emulator
 *  easier.
 */

#include <dirent.h>
#include <errno.h>
#include <fcntl.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <time.h>
#include <unistd.h>
#include "error.h"
#include "trs.h"
#include "trs_disk.h"
#include "trs_hard.h"
#include "trs_imp_exp.h"
#include "trs_state_save.h"

/*
   If the following option is set, potentially dangerous emulator traps
   will be blocked, including file writes to the host filesystem and shell
   command execution.
 */
int trs_emtsafe = 1;

/* New emulator traps */
typedef struct {
  DIR *dir;
  char pathname[FILENAME_MAX];
} OpenDir;

#define MAX_OPENDIR 32
static OpenDir dir[MAX_OPENDIR];

typedef struct {
  int fd;
  int inuse;
  int oflag;
  int xtrshard;
  int xtrshard_unit;
} OpenDisk;

#define MAX_OPENDISK 32
static OpenDisk od[MAX_OPENDISK];
static int xtrshard_fd[4] = {-1,-1,-1,-1};

static int emt_block(const char *emt_func)
{
  if (trs_emtsafe) {
    error("emt_%s: potentially dangerous emulator trap blocked", emt_func);
    Z80_A = EACCES;
    Z80_F &= ~ZERO_MASK;
  }
  return trs_emtsafe;
}

void do_emt_system(void)
{
  int res;

  if (emt_block("system"))
    return;

  res = system((char *)mem_pointer(Z80_HL, 0));
  if (res == -1) {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  } else {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  }
  Z80_BC = res;
}

void do_emt_mouse(void)
{
  int x, y;
  unsigned int buttons, sens;

  trs_emu_mouse = 1;

  switch (Z80_B) {
  case 1:
    trs_get_mouse_pos(&x, &y, &buttons);
    Z80_HL = x;
    Z80_DE = y;
    Z80_A = buttons;
    if (Z80_A)
      Z80_F &= ~ZERO_MASK;
    else
      Z80_F |= ZERO_MASK;
    break;
  case 2:
    trs_set_mouse_pos(Z80_HL, Z80_DE);
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
    break;
  case 3:
    trs_get_mouse_max(&x, &y, &sens);
    Z80_HL = x;
    Z80_DE = y;
    Z80_A = sens;
    if (Z80_A)
      Z80_F &= ~ZERO_MASK;
    else
      Z80_F |= ZERO_MASK;
    break;
  case 4:
    trs_set_mouse_max(Z80_HL, Z80_DE, Z80_C);
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
    break;
  case 5:
    Z80_A = 1;  /* !!Note: assuming 3-button mouse */
    if (Z80_A)
      Z80_F &= ~ZERO_MASK;
    else
      Z80_F |= ZERO_MASK;
    break;
  default:
    error("undefined emt_mouse function code %d", Z80_B);
    break;
  }
}

void do_emt_getddir(void)
{
  if (Z80_HL + Z80_BC > 0x10000 ||
      Z80_HL + strlen(trs_disk_dir) + 1 > Z80_HL + Z80_BC) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  strcpy((char *)mem_pointer(Z80_HL, 1), trs_disk_dir);
  Z80_A = 0;
  Z80_F |= ZERO_MASK;
  Z80_BC = strlen(trs_disk_dir);
}

void do_emt_setddir(void)
{
  if (emt_block("setddir"))
    return;

  snprintf(trs_disk_dir, FILENAME_MAX, "%s", (char *)mem_pointer(Z80_HL, 0));
  if (trs_disk_dir[0] == '~' &&
      (trs_disk_dir[1] == DIR_SLASH || trs_disk_dir[1] == '\0')) {
    const char *home = getenv("HOME");

    if (home) {
      char dirname[FILENAME_MAX];

      snprintf(dirname, FILENAME_MAX, "%s%c%s", home, DIR_SLASH, trs_disk_dir + 1);
      snprintf(trs_disk_dir, FILENAME_MAX, "%s", dirname);
    }
  }

  Z80_A = 0;
  Z80_F |= ZERO_MASK;
}

void do_emt_open(void)
{
  int fd, oflag, eoflag;

  eoflag = Z80_BC;
  switch (eoflag & EO_ACCMODE) {
  case EO_RDONLY:
  default:
    oflag = O_RDONLY;
    break;
  case EO_WRONLY:
    oflag = O_WRONLY;
    break;
  case EO_RDWR:
    oflag = O_RDWR;
    break;
  }
  if (eoflag & EO_CREAT)  oflag |= O_CREAT;
  if (eoflag & EO_EXCL)   oflag |= O_EXCL;
  if (eoflag & EO_TRUNC)  oflag |= O_TRUNC;
  if (eoflag & EO_APPEND) oflag |= O_APPEND;

  if (trs_emtsafe && oflag != O_RDONLY) {
    if (emt_block("open"))
      return;
  }

  fd = open((char *)mem_pointer(Z80_HL, 0), oflag, Z80_DE);
  if (fd >= 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }

  Z80_DE = fd;
}

void do_emt_close(void)
{
  if (close(Z80_DE) >= 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }
}

void do_emt_read(void)
{
  int size;
  int i;

  if (Z80_HL + Z80_BC > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  if (trs_show_led) {
    for (i = 0; i < 3; i++) {
      if (Z80_DE == xtrshard_fd[i])
        trs_hard_led(i, 1);
    }
  }

  size = read(Z80_DE, mem_pointer(Z80_HL, 1), Z80_BC);
  if (size >= 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }

  Z80_BC = size;
}


void do_emt_write(void)
{
  int size;
  int i;

  if (Z80_HL + Z80_BC > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  if (trs_show_led) {
    for (i = 0; i < 3; i++) {
      if (Z80_DE == xtrshard_fd[i])
        trs_hard_led(i, 1);
    }
  }

  size = write(Z80_DE, mem_pointer(Z80_HL, 0), Z80_BC);

  if (size >= 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }

  Z80_BC = size;
}

void do_emt_lseek(void)
{
  int i;
  off_t offset;

  if (Z80_HL + 8 > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    return;
  }

  offset = 0;
  for (i = 0; i < 8; i++) {
    offset = offset + ((off_t)mem_read(Z80_HL + i) << i*8);
  }

  offset = lseek(Z80_DE, offset, Z80_BC);

  if (offset != (off_t) -1) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }

  for (i = Z80_HL; i < 8; i++) {
    mem_write(Z80_HL + i, offset & 0xff);
    offset >>= 8;
  }
}

void do_emt_strerror(void)
{
  char *msg;
  int size;

  if (Z80_HL + Z80_BC > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  errno = 0;
  msg = strerror(Z80_A);
  size = strlen(msg);

  if (errno != 0) {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  } else if (Z80_BC < size + 2) {
    Z80_A = ERANGE;
    Z80_F &= ~ZERO_MASK;
    size = Z80_BC - 1;
  } else {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  }

  if (mem_pointer(Z80_HL, 1)) {
    memcpy(mem_pointer(Z80_HL, 1), msg, size);
    mem_write(Z80_HL + size++, '\r');
    mem_write(Z80_HL + size, '\0');
  }

  if (errno == 0)
    Z80_BC = size;
  else
    Z80_BC = 0xFFFF;
}

void do_emt_time(void)
{
  time_t now = time(0) + trs_timeoffset;

  if (Z80_A == 1) {
#if __alpha
    struct tm *loctm = localtime(&now);
    now += loctm->tm_gmtoff;
#else
    struct tm loctm = *(localtime(&now));
    struct tm gmtm = *(gmtime(&now));
    int daydiff = loctm.tm_mday - gmtm.tm_mday;
    now += (loctm.tm_sec - gmtm.tm_sec)
      + (loctm.tm_min - gmtm.tm_min) * 60
      + (loctm.tm_hour - gmtm.tm_hour) * 3600;
    switch (daydiff) {
    case 0:
    case 1:
    case -1:
      now += 24*3600 * daydiff;
      break;
    case 30:
    case 29:
    case 28:
    case 27:
      now -= 24*3600;
      break;
    case -30:
    case -29:
    case -28:
    case -27:
      now += 24*3600;
      break;
    default:
      error("trouble computing local time in emt_time");
    }
#endif
  } else if (Z80_A != 0) {
    error("unsupported function code %d to emt_time", Z80_A);
  }

  Z80_BC = (now >> 16) & 0xffff;
  Z80_DE = now & 0xffff;
}

void do_emt_opendir(void)
{
  int i;
  char *dirname;

  for (i = 0; i < MAX_OPENDIR; i++) {
    if (dir[i].dir == NULL) break;
  }

  if (i == MAX_OPENDIR) {
    Z80_DE = 0xffff;
    Z80_A = EMFILE;
    return;
  }

  dirname = (char *)mem_pointer(Z80_HL, 0);
  dir[i].dir = opendir(dirname);

  if (dir[i].dir == NULL) {
    Z80_DE = 0xffff;
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  } else {
    strncpy(dir[i].pathname, dirname, FILENAME_MAX);
    Z80_DE = i;
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  }
}

void do_emt_closedir(void)
{
  int const i = Z80_DE;
  int ok;

  if (i < 0 || i >= MAX_OPENDIR || dir[i].dir == NULL) {
    Z80_A = EBADF;
    Z80_F &= ~ZERO_MASK;
    return;
  }

  ok = closedir(dir[i].dir);
  dir[i].dir = NULL;

  if (ok >= 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }
}

void do_emt_readdir(void)
{
  int const i = Z80_DE;
  int size;
  struct dirent *result;

  if (i < 0 || i >= MAX_OPENDIR || dir[i].dir == NULL) {
    Z80_A = EBADF;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  if (Z80_HL + Z80_BC > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  result = readdir(dir[i].dir);

  if (result == NULL) {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  size = strlen(result->d_name);

  if (size + 1 > Z80_BC) {
    Z80_A = ERANGE;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  strcpy((char *)mem_pointer(Z80_HL, 1), result->d_name);

  Z80_A = 0;
  Z80_F |= ZERO_MASK;
  Z80_BC = size;
}

void do_emt_chdir(void)
{
  if (emt_block("chdir"))
    return;

  if (chdir((char *)mem_pointer(Z80_HL, 0)) < 0) {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  } else {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  }
}

void do_emt_getcwd(void)
{
  char *result;

  if (Z80_HL + Z80_BC > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  result = getcwd((char *)mem_pointer(Z80_HL, 1), Z80_BC);

  if (result == NULL) {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
    Z80_BC = 0xFFFF;
    return;
  }

  Z80_A = 0;
  Z80_F |= ZERO_MASK;
  Z80_BC = strlen(result);
}

void do_emt_misc(void)
{
  switch (Z80_A) {
  case 0:
  /* Removed for sdltrs - mdg */
    Z80_HL = 0;
    break;
  case 1:
    trs_exit(0);
    break;
  case 2:
#ifdef ZBX
    trs_debug();
#else
    error("ZBX debugger disabled");
#endif
    break;
  case 3:
    trs_reset(0);
    break;
  case 4:
    Z80_HL = 0;
    break;
  case 5:
    Z80_HL = trs_model;
    break;
  case 6:
    Z80_HL = trs_disk_getsize(Z80_BC);
    break;
  case 7:
    trs_disk_setsize(Z80_BC, Z80_HL);
    break;
#ifdef __linux
  case 8:
    Z80_HL = trs_disk_getstep(Z80_BC);
    break;
  case 9:
    trs_disk_setstep(Z80_BC, Z80_HL);
    break;
#endif
  case 10:
    Z80_HL = grafyx_microlabs ? 1 : 0;
    break;
  case 11:
    grafyx_microlabs = Z80_HL ? 1 : 0;
    break;
  case 12:
    Z80_HL = 0;
    Z80_BC = !timer_overclock;
    break;
  case 13:
    timer_overclock = !Z80_BC;
    trs_timer_mode(timer_overclock);
    break;
  case 14:
    Z80_HL = stretch_amount;
    break;
  case 15:
    stretch_amount = Z80_HL;
    break;
  case 16:
    Z80_HL = trs_disk_doubler;
    break;
  case 17:
    if (Z80_HL < 4)
      trs_disk_doubler = Z80_HL;
    break;
  case 18:
    Z80_HL = trs_sound ? 1 : 0;
    break;
  case 19:
    trs_sound = Z80_HL ? 1 : 0;
    break;
  case 20:
    Z80_HL = trs_disk_truedam ? 1 : 0;
    break;
  case 21:
    trs_disk_truedam = Z80_HL ? 1 : 0;
    break;
  case 24:
    Z80_HL = lowercase ? 1 : 0;
    break;
  case 25:
    lowercase = Z80_HL ? 1 : 0;
    break;
  default:
    error("unsupported function code %d to emt_misc", Z80_A);
    break;
  }
}

void do_emt_ftruncate(void)
{
  int i, result;
  off_t offset;

  if (Z80_HL + 8 > 0x10000) {
    Z80_A = EFAULT;
    Z80_F &= ~ZERO_MASK;
    return;
  }

  offset = 0;
  for (i = 0; i < 8; i++) {
    offset = offset + ((off_t)mem_read(Z80_HL + i) << i*8);
  }
#ifdef _WIN32
  result = chsize(Z80_DE, offset);
#else
  result = ftruncate(Z80_DE, offset);
#endif
  if (result == 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }
}

/*
 * In xtrs 4.9d and earlier, you could open any filename through this
 * API in any mode, with an API similar to emt_open -- filename
 * (relative to trs_disk_dir) in HL, flags in BC, mode in DE.  Now
 * instead we take the unit number in the three low-order bits of A
 * and open the same filename that trs_hard.c would open.  This keeps
 * compatibility with xtrshard, the only existing client of the API,
 * because xtrshard conveniently happens to leave the unit number
 * there.  The new API works better with SDLTRS where the user
 * selects the hard drive name from the Hard Disk menu.  It is also
 * safer; it does not need to be restricted by emt_safe.
 *
 * This API could be improved further (better integration with
 * trs_hard.c, handle write-protect better, reduce the amount of Z80
 * code in xtrshard, etc.), but I prefer to keep compatibility with
 * the old xtrshard.
 */
void do_emt_opendisk(void)
{
  int const drive = Z80_A % TRS_HARD_MAXDRIVES;
  int i;
  int readonly = 0;

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (!od[i].inuse) break;
  }

  if (i == MAX_OPENDISK) {
    Z80_DE = 0xffff;
    Z80_A = EMFILE;
    Z80_F &= ~ZERO_MASK;
    return;
  }

  od[i].fd = open(trs_hard_getfilename(drive), O_RDWR);
  if (od[i].fd < 0) {
    od[i].fd = open(trs_hard_getfilename(drive), O_RDONLY);
    readonly = 1;
  }

  if (od[i].fd >= 0) {
    od[i].inuse = 1;
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }

  Z80_DE = od[i].fd;
  Z80_BC = readonly;
}

static
int do_emt_closefd(int odindex)
{
  int i;

  if (od[odindex].xtrshard) {
    for (i = 0; i < 4; i++) {
      if (xtrshard_fd[i] == od[odindex].fd)
        xtrshard_fd[i] = -1;
    }
  }
  return(close(od[odindex].fd));
}

void do_emt_closedisk(void)
{
  int i;

  if (Z80_DE == 0xffff) {
    for (i = 0; i < MAX_OPENDISK; i++) {
      if (od[i].inuse) {
        do_emt_closefd(i);
        od[i].inuse = 0;
        od[i].xtrshard = 0;
      }
    }
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
    return;
  }

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (od[i].inuse && od[i].fd == Z80_DE) break;
  }

  if (i == MAX_OPENDISK) {
    Z80_A = EBADF;
    Z80_F &= ~ZERO_MASK;
    return;
  }

  od[i].inuse = 0;
  od[i].xtrshard = 0;

  if (do_emt_closefd(i) >= 0) {
    Z80_A = 0;
    Z80_F |= ZERO_MASK;
  } else {
    Z80_A = errno;
    Z80_F &= ~ZERO_MASK;
  }
}

void do_emt_resetdisk(void)
{
  int i;

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (od[i].inuse) {
      do_emt_closefd(i);
      od[i].inuse = 0;
      od[i].xtrshard = 0;
    }
  }
}

void trs_imp_exp_save(FILE *file)
{
  int i;

  for (i = 0; i < MAX_OPENDIR; i++) {
    int file_not_null = (dir[i].dir != NULL);

    trs_save_int(file, &file_not_null, 1);
    trs_save_filename(file, dir[i].pathname);
  }

  for (i = 0; i < MAX_OPENDISK; i++) {
    trs_save_int(file, &od[i].fd, 1);
    trs_save_int(file, &od[i].inuse, 1);
    trs_save_int(file, &od[i].oflag, 1);
    trs_save_int(file, &od[i].xtrshard, 1);
    trs_save_int(file, &od[i].xtrshard_unit, 1);
  }
}

void trs_imp_exp_load(FILE *file)
{
  int i;

  /* Close any open dirs and files */
  for (i = 0; i < MAX_OPENDIR; i++) {
    if (dir[i].dir)
      closedir(dir[i].dir);
  }

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (od[i].inuse)
      close(od[i].fd);
  }

  /* Load the state */
  for (i = 0; i < MAX_OPENDIR; i++) {
    int dir_present;

    trs_load_int(file, &dir_present, 1);
    trs_load_filename(file, dir[i].pathname);

    if (dir_present)
      dir[i].dir = opendir(dir[i].pathname);
    else
      dir[i].dir = NULL;
  }

  for (i = 0; i < MAX_OPENDISK; i++) {
    trs_load_int(file, &od[i].fd, 1);
    trs_load_int(file, &od[i].inuse, 1);
    trs_load_int(file, &od[i].oflag, 1);
    trs_load_int(file, &od[i].xtrshard, 1);
    trs_load_int(file, &od[i].xtrshard_unit, 1);
  }

  /* Reopen the files */
  for (i = 0; i < 4; i++)
    xtrshard_fd[i] = -1;

  for (i = 0; i < MAX_OPENDIR; i++) {
    if (dir[i].dir)
      dir[i].dir = opendir(dir[i].pathname);
  }

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (od[i].inuse) {
      od[i].fd = open(trs_hard_getfilename(i), od[i].oflag);
      if (od[i].xtrshard)
        xtrshard_fd[od[i].xtrshard_unit] = od[i].fd;
    }
  }
}

void
trs_impexp_xtrshard_attach(int drive)
{
  int i;

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (od[i].inuse && od[i].xtrshard && (od[i].xtrshard_unit == drive)) {
      close(od[i].fd);
      od[i].fd = open(trs_hard_getfilename(i), od[i].oflag);
      xtrshard_fd[od[i].xtrshard_unit] = od[i].fd;
    }
  }
}

void
trs_impexp_xtrshard_remove(int drive)
{
  int i;

  for (i = 0; i < MAX_OPENDISK; i++) {
    if (od[i].inuse && od[i].xtrshard && (od[i].xtrshard_unit == drive)) {
      close(od[i].fd);
      od[i].fd = -1;
      xtrshard_fd[od[i].xtrshard_unit] = -1;
    }
  }
}
