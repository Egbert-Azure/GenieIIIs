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
 * Portions copyright (c) 1996-2020, Timothy P. Mann
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

#ifndef _Z80_H
#define _Z80_H

#include <stdio.h>
#include <ctype.h>
#include <SDL_types.h>

#ifndef TRUE
#define TRUE	(1)
#define FALSE	(0)
#endif


typedef Uint64 tstate_t;
#define TSTATE_T_MID (((Uint64) -1LL)/2ULL)

#if defined(SDL_PRIu64)
#define TSTATE_T_LEN SDL_PRIu64
#else
#if defined(_WIN32) && !defined(__MINGW64__)
#define TSTATE_T_LEN "I64u"
#else
#if __WORDSIZE == 64
#define TSTATE_T_LEN "lu"
#else
#define TSTATE_T_LEN "llu"
#endif
#endif
#endif

struct twobyte
{
#ifdef big_endian
    Uint8 high, low;
#else
    Uint8 low, high;
#endif
};

struct fourbyte
{
#ifdef big_endian
    Uint8 byte3, byte2, byte1, byte0;
#else
    Uint8 byte0, byte1, byte2, byte3;
#endif
};

/* for implementing registers which can be seen as bytes or words: */
typedef union
{
    struct twobyte byte;
    Uint16 word;
} wordregister;

struct z80_state_struct
{
    wordregister af;
    wordregister bc;
    wordregister de;
    wordregister hl;
    wordregister ix;
    wordregister iy;
    wordregister sp;
    wordregister pc;

    wordregister af_prime;
    wordregister bc_prime;
    wordregister de_prime;
    wordregister hl_prime;

    Uint8 i;	/* interrupt-page address register */
    Uint8 r;	/* memory-refresh register */
    Uint8 r7;	/* bit 7 of refresh register saved */

    Uint8 iff1, iff2;
    Uint8 interrupt_mode;

    /* To signal a maskable interrupt, set irq TRUE.  The CPU does not
     * turn off irq; the external device must turn it off when
     * appropriately tickled by some IO port or memory address that it
     * decodes.  INT is level triggered, so Z80 code must tickle the
     * device before reenabling interrupts.
     *
     * There is no support as yet for fetching an interrupt vector or
     * RST instruction from the interrupting device, as this gets
     * rather complex when there can be more than one device with an
     * interrupt pending.  So you'd better use interrupt_mode 1 only
     * (which is what the TRS-80 does).
     */
    int irq;

    /* To signal a nonmaskable interrupt, set nmi to TRUE.  The CPU
     * does not turn off nmi; the external device must turn it off
     * when tickled (or after a timeout).  NMI is edge triggered, so
     * it has to be turned off and back on again before it can cause
     * another interrupt.  nmi_seen remembers that an edge has been seen,
     * so turn off both nmi and nmi_seen when the interrupt is acknowledged.
     */
    int nmi, nmi_seen;

    /* Cyclic T-state counter */
    tstate_t t_count;

    /* Clock in MHz = T-states per microsecond */
    float clockMHz;

    /* Simple event scheduler.  If nonzero, when t_count passes sched,
     * trs_do_event() is called and sched is set to zero. */
    tstate_t sched;
};

#define Z80_ADDRESS_LIMIT	(65536)

/*
 * Register accessors:
 */

#define Z80_A			(z80_state.af.byte.high)
#define Z80_F			(z80_state.af.byte.low)
#define Z80_B			(z80_state.bc.byte.high)
#define Z80_C			(z80_state.bc.byte.low)
#define Z80_D			(z80_state.de.byte.high)
#define Z80_E			(z80_state.de.byte.low)
#define Z80_H			(z80_state.hl.byte.high)
#define Z80_L			(z80_state.hl.byte.low)
#define Z80_IX_HIGH		(z80_state.ix.byte.high)
#define Z80_IX_LOW		(z80_state.ix.byte.low)
#define Z80_IY_HIGH		(z80_state.iy.byte.high)
#define Z80_IY_LOW		(z80_state.iy.byte.low)

#define Z80_SP			(z80_state.sp.word)
#define Z80_PC			(z80_state.pc.word)

#define Z80_AF			(z80_state.af.word)
#define Z80_BC			(z80_state.bc.word)
#define Z80_DE			(z80_state.de.word)
#define Z80_HL			(z80_state.hl.word)

#define Z80_AF_PRIME		(z80_state.af_prime.word)
#define Z80_BC_PRIME		(z80_state.bc_prime.word)
#define Z80_DE_PRIME		(z80_state.de_prime.word)
#define Z80_HL_PRIME		(z80_state.hl_prime.word)

#define Z80_IX			(z80_state.ix.word)
#define Z80_IY			(z80_state.iy.word)

#define Z80_I			(z80_state.i)
#define Z80_R			(z80_state.r)
#define Z80_R7			(z80_state.r7)

#define HIGH(p)			(((struct twobyte *)(p))->high)
#define LOW(p)			(((struct twobyte *)(p))->low)

#define T_COUNT(n)		(z80_state.t_count += (n))

/*
 * Flag accessors:
 *
 * Flags are:
 *
 *	7   6   5   4   3   2   1   0
 *	S   Z   -   H   -  P/V  N   C
 *
 *	C	Carry
 *	N	Subtract
 *	P/V	Parity/Overflow
 *	H	Half-carry
 *	Z	Zero
 *	S	Sign
 */

#define CARRY_MASK		(0x1)
#define SUBTRACT_MASK		(0x2)
#define PARITY_MASK		(0x4)
#define OVERFLOW_MASK		(0x4)
#define UNDOC3_MASK             (0x8)
#define HALF_CARRY_MASK		(0x10)
#define UNDOC5_MASK             (0x20)
#define ZERO_MASK		(0x40)
#define	SIGN_MASK		(0x80)
#define ALL_FLAGS_MASK		(CARRY_MASK | SUBTRACT_MASK | OVERFLOW_MASK | \
				 HALF_CARRY_MASK | ZERO_MASK | SIGN_MASK)

#define SET_SIGN()		(Z80_F |= SIGN_MASK)
#define CLEAR_SIGN()		(Z80_F &= (~SIGN_MASK))
#define SET_ZERO()		(Z80_F |= ZERO_MASK)
#define CLEAR_ZERO()		(Z80_F &= (~ZERO_MASK))
#define SET_HALF_CARRY()       	(Z80_F |= HALF_CARRY_MASK)
#define CLEAR_HALF_CARRY()	(Z80_F &= (~HALF_CARRY_MASK))
#define SET_OVERFLOW()		(Z80_F |= OVERFLOW_MASK)
#define CLEAR_OVERFLOW()	(Z80_F &= (~OVERFLOW_MASK))
#define SET_PARITY()		(Z80_F |= PARITY_MASK)
#define CLEAR_PARITY()		(Z80_F &= (~PARITY_MASK))
#define SET_SUBTRACT()		(Z80_F |= SUBTRACT_MASK)
#define CLEAR_SUBTRACT()	(Z80_F &= (~SUBTRACT_MASK))
#define SET_CARRY()		(Z80_F |= CARRY_MASK)
#define CLEAR_CARRY()		(Z80_F &= (~CARRY_MASK))

#define SIGN_FLAG		(Z80_F & SIGN_MASK)
#define ZERO_FLAG		(Z80_F & ZERO_MASK)
#define HALF_CARRY_FLAG		(Z80_F & HALF_CARRY_MASK)
#define OVERFLOW_FLAG		(Z80_F & OVERFLOW_MASK)
#define PARITY_FLAG		(Z80_F & PARITY_MASK)
#define SUBTRACT_FLAG		(Z80_F & SUBTRACT_MASK)
#define CARRY_FLAG		(Z80_F & CARRY_MASK)

extern struct		z80_state_struct z80_state;
extern unsigned int	cycles_per_timer;

extern void	 z80_reset(void);
extern int	 z80_run(int continuous);
extern void	 z80_out(int port, int value);
extern int	 z80_in(int port);
extern int	 mem_read(int address);
extern void	 mem_write(int address, int value);
extern void	 rom_write(int address, int value);
extern int	 mem_read_word(int address);
extern void	 mem_write_word(int address, int value);
extern Uint8	*mem_pointer(int address, int writing);

#ifdef ZBX
extern int	 disassemble(Uint16 pc);
extern void	 debug_init(void);
extern void	 debug_shell(void);
#endif /* ZBX */
#endif
