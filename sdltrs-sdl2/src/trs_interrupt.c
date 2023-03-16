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
 * Emulate interrupts
 */

#include <stdio.h>
#include <time.h>
#include <SDL.h>
#include "trs.h"
#include "trs_clones.h"
#include "trs_state_save.h"

/*#define EDEBUG 1*/
/*#define IDEBUG 1*/
/*#define IDEBUG2 1*/

/* IRQs */
#define M1_TIMER_BIT    0x80
#define M1_DISK_BIT     0x40
#define M3_UART_ERR_BIT 0x40
#define M3_UART_RCV_BIT 0x20
#define M3_UART_SND_BIT 0x10
#define M3_IOBUS_BIT    0x80 /* not emulated */
#define M3_TIMER_BIT    0x04
#define M3_CASSFALL_BIT 0x02
#define M3_CASSRISE_BIT 0x01
static Uint8 interrupt_latch;
static Uint8 interrupt_mask;

/* NMIs (M3/4/4P only) */
#define M3_INTRQ_BIT    0x80  /* FDC chip INTRQ line */
#define M3_MOTOROFF_BIT 0x40  /* FDC motor timed out (stopped) */
#define M3_RESET_BIT    0x20  /* User pressed Reset button */
static Uint8 nmi_latch = 1; /* ?? One diagnostic program needs this */
static Uint8 nmi_mask = M3_RESET_BIT;

#define TIMER_HZ_1 40
#define TIMER_HZ_3 30
#define TIMER_HZ_4 60
int timer_hz = TIMER_HZ_1;
int timer_overclock;
int timer_overclock_rate = 5;
int speedup = 1;
unsigned int cycles_per_timer;

#define CLOCK_1_MHZ 1.77408
#define CLOCK_2_MHZ 2.02752
#define CLOCK_4_MHZ 4.05504
#define CLOCK_5_MHZ 5.06880
#define CLOCK_8_MHZ 8.11008
#define EG_3200_MHZ 4.00000
#define TCS_SPM_MHZ 5.35000
#define TCS_G3S_MHZ 7.20000
float clock_mhz_1 = CLOCK_1_MHZ;
float clock_mhz_3 = CLOCK_2_MHZ;
float clock_mhz_4 = CLOCK_4_MHZ;

/* Kludge: LDOS hides the date (not time) in a memory area across reboots. */
/* We put it there on powerup, so LDOS magically knows the date! */
#define LDOS_MONTH  0x4306
#define LDOS_DAY    0x4307
#define LDOS_YEAR   0x4466
#define LDOS3_MONTH 0x442f
#define LDOS3_DAY   0x4457
#define LDOS3_YEAR  0x4413
#define LDOS4_MONTH 0x0035
#define LDOS4_DAY   0x0034
#define LDOS4_YEAR  0x0033

/* Kludge, continued: On NEWDOS/80, both date and time are stored in memory
   across reboots, but a test is done on boot to decide whether to use the
   stored values.  Here's how it works: NEWDOS/80 writes a special byte value
   to the memory address right before the stored date and time.  On reboot,
   this address is checked, and if it contains that special byte, the stored
   date and time are considered valid and are therefore used.

   By putting this info in memory on powerup, NEWDOS/80 gets initialized
   with the system date and time.
 */
#define NEWDOS_DATETIME_VALID_BYTE  0xa5
/* Model 1 */
#define NEWDOS_DATETIME_VALID_ADDR  0x43ab
#define NEWDOS_MONTH                0x43b1
#define NEWDOS_DAY                  0x43b0
#define NEWDOS_YEAR                 0x43af
#define NEWDOS_HOUR                 0x43ae
#define NEWDOS_MIN                  0x43ad
#define NEWDOS_SEC                  0x43ac
/* Model 3 */
#define NEWDOS3_DATETIME_VALID_ADDR 0x42cb
#define NEWDOS3_MONTH               0x42d1
#define NEWDOS3_DAY                 0x42d0
#define NEWDOS3_YEAR                0x42cf
#define NEWDOS3_HOUR                0x42ce
#define NEWDOS3_MIN                 0x42cd
#define NEWDOS3_SEC                 0x42cc

static Uint32 deltatime = 25;
static int timer_on = 1;
#ifdef IDEBUG
static long lost_timer_interrupts;
#endif

/* Note: the independent interrupt latch and mask model is not correct
   for all interrupts.  The cassette rise/fall interrupt enable is
   clocked into the interrupt latch when the event occurs (we get this
   right), and is *not* masked against the latch output (we get this
   wrong, but it doesn't really matter). */

void
trs_cassette_rise_interrupt(int dummy)
{
  interrupt_latch = (interrupt_latch & ~M3_CASSRISE_BIT) |
    (interrupt_mask & M3_CASSRISE_BIT);
  z80_state.irq = (interrupt_latch & interrupt_mask) != 0;

  trs_cassette_update(0);
}

void
trs_cassette_fall_interrupt(int dummy)
{
  interrupt_latch = (interrupt_latch & ~M3_CASSFALL_BIT) |
    (interrupt_mask & M3_CASSFALL_BIT);
  z80_state.irq = (interrupt_latch & interrupt_mask) != 0;

  trs_cassette_update(0);
}

void
trs_cassette_clear_interrupts(void)
{
  interrupt_latch &= ~(M3_CASSRISE_BIT|M3_CASSFALL_BIT);
  z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
}

int
trs_cassette_interrupts_enabled(void)
{
  return interrupt_mask & (M3_CASSRISE_BIT|M3_CASSFALL_BIT);
}

void
trs_timer_interrupt(int state)
{
  if (trs_model == 1) {
    if (state) {
#ifdef IDEBUG
      if (interrupt_latch & M1_TIMER_BIT) lost_timer_interrupts++;
#endif
      interrupt_latch |= M1_TIMER_BIT;
      z80_state.irq = 1;
    } else {
      interrupt_latch &= ~M1_TIMER_BIT;
    }
  } else {
    if (state) {
#ifdef IDEBUG
      if (interrupt_latch & M3_TIMER_BIT) lost_timer_interrupts++;
#endif
      interrupt_latch |= M3_TIMER_BIT;
    } else {
      interrupt_latch &= ~M3_TIMER_BIT;
    }
    z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
  }
}

void
trs_disk_intrq_interrupt(int state)
{
  if (trs_model == 1) {
    if (state) {
      interrupt_latch |= M1_DISK_BIT;
      z80_state.irq = 1;
    } else {
      interrupt_latch &= ~M1_DISK_BIT;
    }
  } else {
    if (state) {
      nmi_latch |= M3_INTRQ_BIT;
    } else {
      nmi_latch &= ~M3_INTRQ_BIT;
    }
    z80_state.nmi = (nmi_latch & nmi_mask) != 0;
    if (!z80_state.nmi) z80_state.nmi_seen = 0;
  }
}

void
trs_disk_motoroff_interrupt(int state)
{
  /* Drive motor timed out (stopped). */
  if (trs_model == 1) {
    /* no such interrupt */
  } else {
    if (state) {
      nmi_latch |= M3_MOTOROFF_BIT;
    } else {
      nmi_latch &= ~M3_MOTOROFF_BIT;
    }
    z80_state.nmi = (nmi_latch & nmi_mask) != 0;
    if (!z80_state.nmi) z80_state.nmi_seen = 0;
  }
}

void
trs_disk_drq_interrupt(int state)
{
  /* no effect */
}

/* UNUSED
void
trs_uart_err_interrupt(int state)
{
  if (trs_model > 1) {
    if (state) {
      interrupt_latch |= M3_UART_ERR_BIT;
    } else {
      interrupt_latch &= ~M3_UART_ERR_BIT;
    }
    z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
  }
}
*/

void
trs_uart_rcv_interrupt(int state)
{
  if (trs_model > 1) {
    if (state) {
      interrupt_latch |= M3_UART_RCV_BIT;
    } else {
      interrupt_latch &= ~M3_UART_RCV_BIT;
    }
    z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
  }
}

void
trs_uart_snd_interrupt(int state)
{
  if (trs_model > 1) {
    if (state) {
      interrupt_latch |= M3_UART_SND_BIT;
    } else {
      interrupt_latch &= ~M3_UART_SND_BIT;
    }
    z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
  }
}

void
trs_reset_button_interrupt(int state)
{
  if (trs_model == 1) {
    z80_state.nmi = state;
  } else {
    if (state) {
      nmi_latch |= M3_RESET_BIT;
    } else {
      nmi_latch &= ~M3_RESET_BIT;
    }
    z80_state.nmi = (nmi_latch & nmi_mask) != 0;
  }
  if (!z80_state.nmi) z80_state.nmi_seen = 0;
}

void
trs_interrupt_latch_clear(void)
{
  interrupt_latch = 0;
}

Uint8
trs_interrupt_latch_read(void)
{
  Uint8 tmp = interrupt_latch;

  if (trs_model == 1) {
    interrupt_latch &= ~M1_TIMER_BIT; /* acknowledge this one (only) */
    z80_state.irq = (interrupt_latch != 0);
    return tmp;
  } else {
    /* In some clones (like CP-500/M80) reading from the
       interrupt latch clears pending timer interrupts */
    if (trs_clones.model & CP500_M80) {
      interrupt_latch &= ~M3_TIMER_BIT;
      z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
    }
    return ~tmp;
  }
}

void
trs_interrupt_mask_write(Uint8 value)
{
  interrupt_mask = value;
  z80_state.irq = (interrupt_latch & interrupt_mask) != 0;
}

/* M3 only */
Uint8
trs_nmi_latch_read(void)
{
  return ~nmi_latch;
}

void
trs_nmi_mask_write(Uint8 value)
{
  nmi_mask = value | M3_RESET_BIT;
  z80_state.nmi = (nmi_latch & nmi_mask) != 0;
#if IDEBUG2
  if (z80_state.nmi && !z80_state.nmi_seen) {
    debug("mask write caused nmi, mask %02x latch %02x\n",
          nmi_mask, nmi_latch);
  }
#endif
  if (!z80_state.nmi) z80_state.nmi_seen = 0;
}

static void
trs_timer_event(void)
{
  if (timer_on) {
    trs_timer_interrupt(1); /* generate */
    trs_disk_motoroff_interrupt(trs_disk_motoroff());
    trs_kb_heartbeat(); /* part of keyboard stretch kludge */
  }
}

void trs_timer_sync_with_host(void)
{
  Uint32 curtime;
  static Uint32 lasttime;

  curtime = SDL_GetTicks();

  if (lasttime + deltatime > curtime)
    SDL_Delay(lasttime + deltatime - curtime);

  curtime = SDL_GetTicks();

  lasttime += deltatime;
  if ((lasttime + deltatime) < curtime)
    lasttime = curtime;

  if (trs_show_led) {
    trs_disk_led(0,0);
    trs_hard_led(0,0);
  }

  trs_timer_event();
}

void
trs_timer_init(void)
{
  switch (trs_model) {
    case 1:
      timer_hz = TIMER_HZ_1;
      if (eg3200) {
        z80_state.clockMHz = EG_3200_MHZ;
      } else if (genie3s) {
        z80_state.clockMHz = TCS_G3S_MHZ;
      } else {
        switch (speedup) {
          case 5: /* LNW80 */
            z80_state.clockMHz = CLOCK_4_MHZ;
            break;
          case 6: /* TCS SpeedMaster 5.3 */
            z80_state.clockMHz = TCS_SPM_MHZ;
            break;
          default:
            z80_state.clockMHz = clock_mhz_1;
            break;
        }
      }
      break;
    case 3:
      timer_hz = TIMER_HZ_3;
      z80_state.clockMHz = clock_mhz_3;
      break;
    default:
      timer_hz = TIMER_HZ_4;
      z80_state.clockMHz = clock_mhz_4;
  }

  trs_timer_event();
  trs_timer_mode(timer_overclock);

  if ((trs_clones.model & (EG3200 | GENIE3S)) == 0) {
    /* Also initialize the clock in memory - hack */
    time_t tt = time(NULL);
    struct tm *lt = localtime(&tt);
    extern Uint8 memory[];

    if (trs_model == 1) {
        memory[LDOS_MONTH]    = (lt->tm_mon + 1) ^ 0x50;
        memory[LDOS_DAY]      = lt->tm_mday;
        memory[LDOS_YEAR]     = lt->tm_year - 80;

        memory[NEWDOS_DATETIME_VALID_ADDR] = NEWDOS_DATETIME_VALID_BYTE;
        memory[NEWDOS_MONTH]  = lt->tm_mon + 1;
        memory[NEWDOS_DAY]    = lt->tm_mday;
        memory[NEWDOS_YEAR]   = lt->tm_year % 100;
        memory[NEWDOS_HOUR]   = lt->tm_hour;
        memory[NEWDOS_MIN]    = lt->tm_min;
        memory[NEWDOS_SEC]    = lt->tm_sec;
    } else {
        memory[LDOS3_MONTH]   = (lt->tm_mon + 1) ^ 0x50;
        memory[LDOS3_DAY]     = lt->tm_mday;
        memory[LDOS3_YEAR]    = lt->tm_year - 80;

        memory[NEWDOS3_DATETIME_VALID_ADDR] = NEWDOS_DATETIME_VALID_BYTE;
        memory[NEWDOS3_MONTH] = lt->tm_mon + 1;
        memory[NEWDOS3_DAY]   = lt->tm_mday;
        memory[NEWDOS3_YEAR]  = lt->tm_year % 100;
        memory[NEWDOS3_HOUR]  = lt->tm_hour;
        memory[NEWDOS3_MIN]   = lt->tm_min;
        memory[NEWDOS3_SEC]   = lt->tm_sec;

        if (trs_model >= 4) {
          memory[LDOS4_MONTH] = lt->tm_mon + 1;
          memory[LDOS4_DAY]   = lt->tm_mday;
          memory[LDOS4_YEAR]  = lt->tm_year;
        }
    }
  }
}

void
trs_timer_off(void)
{
  timer_on = 0;
}

void
trs_timer_on(void)
{
  if (!timer_on) {
    timer_on = 1;
    trs_timer_event();
  }
}

void
trs_timer_speed(int fast)
{
  if (speedup == 3) {
    /* The Seatronics Super Speed-Up uses bits 7 and 6
     * of port 0xec to select the Z80 CPU clock rate:
     *
     * Bit 7:  Bit 6:
     *   1       1     = 8 MHz
     *   1       0     = 5 MHz
     *   0       1     = 4 MHz
     *   0       0     = 2 MHz
     */
    if ((fast & 0x80) >> 7) {
      if ((fast & 0x40) >> 6)
        z80_state.clockMHz = CLOCK_8_MHZ;
      else
        z80_state.clockMHz = CLOCK_5_MHZ;
    } else {
      if ((fast & 0x40) >> 6)
        z80_state.clockMHz = CLOCK_4_MHZ;
      else
        z80_state.clockMHz = CLOCK_2_MHZ;
    }
  } else {
    switch (trs_model) {
      case 1:
        if (eg3200)
          z80_state.clockMHz = (fast & 1) ? EG_3200_MHZ : CLOCK_1_MHZ;
        else
        if (genie3s)
          z80_state.clockMHz = (fast & 1) ? TCS_G3S_MHZ : CLOCK_1_MHZ;
        else
        switch (speedup) {
        case 1: /*Archbold*/
        case 7: /*CT-80*/
          z80_state.clockMHz = clock_mhz_1 * ((fast & 1) + 1);
          break;
        case 2: /*Holmes Sprinter II*/
          z80_state.clockMHz = 10.6445 / (((fast + 4) & 7) + 2);
          break;
        case 5: /*LNW80*/
          z80_state.clockMHz = (fast & 1) ? CLOCK_4_MHZ : CLOCK_1_MHZ;
          break;
        case 6: /*TCS SpeedMaster*/
          z80_state.clockMHz = (fast & 1) ? TCS_SPM_MHZ : CLOCK_1_MHZ;
          break;
        default:
          break;
        }
        break;
      case 3:
        if (speedup == 2)
          /* Switch to fastest possible speed of Sprinter III */
          z80_state.clockMHz = (fast & 1) ? CLOCK_5_MHZ : clock_mhz_3;
        else
          z80_state.clockMHz = fast ? CLOCK_4_MHZ : clock_mhz_3;
        break;
      default:
        /* Model 4/4P */
        z80_state.clockMHz = fast ? clock_mhz_4 : clock_mhz_3;
      break;
    }
  }
  if (trs_model >= 4) {
    if (((fast & 0x80) >> 7) || ((fast & 0x40) >> 6))
      timer_hz = TIMER_HZ_4;
    else
      timer_hz = TIMER_HZ_3;
  }
  trs_timer_mode(-1);
}

void
trs_timer_mode(int mode)
{
  if (mode != -1) {
    timer_overclock = mode;
    if (timer_overclock)
      deltatime = 1000 / (timer_overclock_rate * timer_hz);
    else
      deltatime = 1000 / timer_hz;

    if (trs_show_led)
      trs_turbo_led();
  }
  trs_screen_caption();

  cycles_per_timer = z80_state.clockMHz * 1000000 / timer_hz;
}

static trs_event_func event_func = NULL;
static int event_arg;

/* Schedule an event to occur after "countdown" more t-states have
 *  executed.  0 makes the event happen immediately -- that is, at
 *  the end of the current instruction, but before the emulator checks
 *  for interrupts.  It is legal for an event function to call
 *  trs_schedule_event.
 *
 * Only one event can be buffered.  If you try to schedule a second
 *  event while one is still pending, the pending event (along with
 *  any further events that it schedules) is executed immediately.
 */
void
trs_schedule_event(trs_event_func f, int arg, int countdown)
{
  while (event_func) {
#if EDEBUG
    error("warning: trying to schedule two events");
#endif
    trs_do_event();
  }
  event_func = f;
  event_arg = arg;
  z80_state.sched = z80_state.t_count + (tstate_t) countdown;
  if (z80_state.sched == 0) z80_state.sched--;
}

/*
 * If an event is scheduled, do it now.  (If the event function
 * schedules a new event, however, leave that one pending.)
 */
void
trs_do_event(void)
{
  trs_event_func f = event_func;
  if (f) {
    event_func = NULL;
    z80_state.sched = 0;
    f(event_arg);
  }
}

/*
 * Cancel scheduled event, if any.
 */
void
trs_cancel_event(void)
{
  event_func = NULL;
  z80_state.sched = 0;
}

/*
 * Check event scheduled
 */
trs_event_func
trs_event_scheduled(void)
{
  return event_func;
}

void trs_interrupt_save(FILE *file)
{
  int event = 0;

  trs_save_uint8(file, &interrupt_latch, 1);
  trs_save_uint8(file, &interrupt_mask, 1);
  trs_save_uint8(file, &nmi_latch, 1);
  trs_save_int(file, &timer_hz, 1);
  trs_save_uint32(file, &cycles_per_timer, 1);
  trs_save_int(file, &timer_on, 1);

  if (event_func == assert_state_void)
    event = 1;
  else if (event_func == transition_out)
    event = 2;
  else if (event_func == trs_cassette_kickoff)
    event = 3;
  else if (event_func == orch90_flush)
    event = 4;
  else if (event_func == trs_cassette_fall_interrupt)
    event = 5;
  else if (event_func == trs_cassette_rise_interrupt)
    event = 6;
  else if (event_func == trs_cassette_update)
    event = 7;
  else if (event_func == trs_disk_lostdata)
    event = 8;
  else if (event_func == trs_disk_done)
    event = 9;
  else if (event_func == trs_disk_firstdrq)
    event = 10;
  else if (event_func == trs_reset_button_interrupt)
    event = 11;
  else if (event_func == trs_uart_set_avail)
    event = 12;
  else if (event_func == trs_uart_set_empty)
    event = 13;

  trs_save_int(file, &event, 1);
  trs_save_int(file, &event_arg, 1);
}

void trs_interrupt_load(FILE *file)
{
  int event;

  trs_load_uint8(file, &interrupt_latch, 1);
  trs_load_uint8(file, &interrupt_mask, 1);
  trs_load_uint8(file, &nmi_latch, 1);
  trs_load_int(file, &timer_hz, 1);
  trs_load_uint32(file, &cycles_per_timer, 1);
  trs_load_int(file, &timer_on, 1);
  trs_load_int(file, &event, 1);

  switch(event) {
    case 1:
      event_func = assert_state_void;
      break;
    case 2:
      event_func = transition_out;
      break;
    case 3:
      event_func = trs_cassette_kickoff;
      break;
    case 4:
      event_func = orch90_flush;
      break;
    case 5:
      event_func = trs_cassette_fall_interrupt;
      break;
    case 6:
      event_func = trs_cassette_rise_interrupt;
      break;
    case 7:
      event_func = trs_cassette_update;
      break;
    case 8:
      event_func = trs_disk_lostdata;
      break;
    case 9:
      event_func = trs_disk_done;
      break;
    case 10:
      event_func = trs_disk_firstdrq;
      break;
    case 11:
      event_func = trs_reset_button_interrupt;
      break;
    case 12:
      event_func = trs_uart_set_avail;
      break;
    case 13:
      event_func = trs_uart_set_empty;
      break;
    default:
      event_func = NULL;
      break;
  }

  trs_load_int(file, &event_arg, 1);
}
