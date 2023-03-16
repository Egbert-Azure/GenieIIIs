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

/*
 * z80.c:  The guts of the Z80 emulator.
 *
 * The Z80 emulator should be general and complete enough to be
 * easily adapted to emulate any Z80 machine.  All of the documented
 * Z80 flags and instructions are implemented.  The only documented
 * features we cheat a little on are interrupt handling (modes 0 and 2
 * are not supported).
 *
 * All of the undocumented instructions, flags, and features listed in
 * http://www.msxnet.org/tech/Z80/z80undoc.txt are implemented too,
 * with some minor exceptions.  There seems to be a disagreement about
 * undocumented flag handling for "bit" instructions between
 * z80undoc.txt and the ZEXALL validator from Yaze.  Since ZEXALL
 * passes on both a real Z80 and Yaze, but fails on my attempt to
 * implement "bit n,r" according to z80undoc.txt, I've imitated Yaze's
 * implementation.  On block in/out instructions, z80undoc.txt gives
 * some very complicated rules for undocumented flag handling that I
 * have not implemented.
 */

#include "error.h"
#include "trs.h"
#include "trs_imp_exp.h"
#include "trs_state_save.h"

/*
 * The state of our Z80 registers is kept in this structure:
 */
struct z80_state_struct z80_state;
static tstate_t last_t_count;

/* for parity flag, 1 = even parity, 0 = odd parity. */
static const short parity_table[256] =
{
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	0, 1, 1, 0, 1, 0, 0, 1, 1, 0, 0, 1, 0, 1, 1, 0,
	1, 0, 0, 1, 0, 1, 1, 0, 0, 1, 1, 0, 1, 0, 0, 1
};

#define parity(x)  parity_table[(x) & 0xFF]

/*
 * Tables and routines for computing various flag values:
 */

static const Uint8 sign_carry_overflow_table[] =
{
    0,
    OVERFLOW_MASK | SIGN_MASK,
    CARRY_MASK,
    SIGN_MASK,
    CARRY_MASK,
    SIGN_MASK,
    CARRY_MASK | OVERFLOW_MASK,
    CARRY_MASK | SIGN_MASK,
};

static const Uint8 half_carry_table[] =
{
    0,
    0,
    HALF_CARRY_MASK,
    0,
    HALF_CARRY_MASK,
    0,
    HALF_CARRY_MASK,
    HALF_CARRY_MASK,
};

static const Uint8 subtract_sign_carry_overflow_table[] =
{
    0,
    CARRY_MASK | SIGN_MASK,
    CARRY_MASK,
    OVERFLOW_MASK | CARRY_MASK | SIGN_MASK,
    OVERFLOW_MASK,
    SIGN_MASK,
    0,
    CARRY_MASK | SIGN_MASK,
};

static const Uint8 subtract_half_carry_table[] =
{
    0,
    HALF_CARRY_MASK,
    HALF_CARRY_MASK,
    HALF_CARRY_MASK,
    0,
    0,
    0,
    HALF_CARRY_MASK,
};

static void do_add_flags(int a, int b, int result)
{
    /*
     * Compute the flag values for a + b = result operation
     */

    /*
     * Sign, carry, and overflow depend upon values of bit 7.
     * Half-carry depends upon values of bit 3.
     * We mask those bits, munge them into an index, and look
     * up the flag values in the above tables.
     * Undocumented flags in bit 3, 5 of F come from the result.
     */

    int index = ((a & 0x88) >> 1) | ((b & 0x88) >> 2) |
      ((result & 0x88) >> 3);

    int f = half_carry_table[index & 7] |
      sign_carry_overflow_table[index >> 4] |
      (result & (UNDOC3_MASK|UNDOC5_MASK));

    if((result & 0xFF) == 0) f |= ZERO_MASK;

    Z80_F = f;
}

static void do_sub_flags(int a, int b, int result)
{
    /*
     * Sign, carry, and overflow depend upon values of bit 7.
     * Half-carry depends upon values of bit 3.
     * We mask those bits, munge them into an index, and look
     * up the flag values in the above tables.
     * Undocumented flags in bit 3, 5 of F come from the result.
     */

    int index = ((a & 0x88) >> 1) | ((b & 0x88) >> 2) |
      ((result & 0x88) >> 3);

    int f = SUBTRACT_MASK | subtract_half_carry_table[index & 7] |
      subtract_sign_carry_overflow_table[index >> 4] |
      (result & (UNDOC3_MASK|UNDOC5_MASK));

    if((result & 0xFF) == 0) f |= ZERO_MASK;

    Z80_F = f;
}


static void do_adc_word_flags(int a, int b, int result)
{
    /*
     * Sign, carry, and overflow depend upon values of bit 15.
     * Half-carry depends upon values of bit 11.
     * We mask those bits, munge them into an index, and look
     * up the flag values in the above tables.
     * Undocumented flags in bit 3, 5 of F come from the result high byte.
     */

    int index = ((a & 0x8800) >> 9) | ((b & 0x8800) >> 10) |
      ((result & 0x8800) >> 11);

    int f = half_carry_table[index & 7] |
      sign_carry_overflow_table[index >> 4] |
      ((result >> 8) & (UNDOC3_MASK|UNDOC5_MASK));

    if((result & 0xFFFF) == 0) f |= ZERO_MASK;

    Z80_F = f;
}

static void do_add_word_flags(int a, int b, int result)
{
    /*
     * Carry depends upon values of bit 15.
     * Half-carry depends upon values of bit 11.
     * We mask those bits, munge them into an index, and look
     * up the flag values in the above tables.
     * Undocumented flags in bit 3, 5 of F come from the result high byte.
     */

    int index = ((a & 0x8800) >> 9) | ((b & 0x8800) >> 10) |
      ((result & 0x8800) >> 11);

    int f = half_carry_table[index & 7] |
      (sign_carry_overflow_table[index >> 4] & CARRY_MASK) |
      (Z80_F & (ZERO_MASK | PARITY_MASK | SIGN_MASK)) |
      ((result >> 8) & (UNDOC3_MASK | UNDOC5_MASK));

    Z80_F = f;
}

static void do_sbc_word_flags(int a, int b, int result)
{
    /*
     * Sign, carry, and overflow depend upon values of bit 15.
     * Half-carry depends upon values of bit 11.
     * We mask those bits, munge them into an index, and look
     * up the flag values in the above tables.
     * Undocumented flags in bit 3, 5 of F come from the result high byte.
     */

    int index = ((a & 0x8800) >> 9) | ((b & 0x8800) >> 10) |
      ((result & 0x8800) >> 11);

    int f = SUBTRACT_MASK | subtract_half_carry_table[index & 7] |
      subtract_sign_carry_overflow_table[index >> 4] |
      ((result >> 8) & (UNDOC3_MASK | UNDOC5_MASK));

    if((result & 0xFFFF) == 0) f |= ZERO_MASK;

    Z80_F = f;
}

static void do_flags_dec_byte(int value)
{
    Uint8 set = SUBTRACT_MASK;

    if(value == 0x7f)
      set |= OVERFLOW_MASK;
    if((value & 0xF) == 0xF)
      set |= HALF_CARRY_MASK;
    if(value == 0)
      set |= ZERO_MASK;
    if(value & 0x80)
      set |= SIGN_MASK;

    Z80_F = (Z80_F & CARRY_MASK) | set
      | (value & (UNDOC3_MASK | UNDOC5_MASK));
}

static void do_flags_inc_byte(int value)
{
    Uint8 set = 0;

    if(value == 0x80)
      set |= OVERFLOW_MASK;
    if((value & 0xF) == 0)
      set |= HALF_CARRY_MASK;
    if(value == 0)
      set |= ZERO_MASK;
    if(value & 0x80)
      set |= SIGN_MASK;

    Z80_F = (Z80_F & CARRY_MASK) | set
      | (value & (UNDOC3_MASK | UNDOC5_MASK));
}

/*
 * Routines for executing or assisting various non-trivial arithmetic
 * instructions:
 */
static void do_and_byte(int value)
{
    int result = (Z80_A &= value);
    Uint8 set  = HALF_CARRY_MASK;

    if(parity(result))
      set |= PARITY_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(result & 0x80)
      set |= SIGN_MASK;

    Z80_F = set | (result & (UNDOC3_MASK | UNDOC5_MASK));
}

static void do_or_byte(int value)
{
    int result = (Z80_A |= value);  /* the result of the or operation */
    Uint8 set  = 0;

    if(parity(result))
      set |= PARITY_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(result & 0x80)
      set |= SIGN_MASK;

    Z80_F = set | (result & (UNDOC3_MASK | UNDOC5_MASK));
}

static void do_xor_byte(int value)
{
    int result = (Z80_A ^= value);  /* the result of the xor operation */
    Uint8 set  = 0;

    if(parity(result))
      set |= PARITY_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(result & 0x80)
      set |= SIGN_MASK;

    Z80_F = set | (result & (UNDOC3_MASK | UNDOC5_MASK));
}

static void do_add_byte(int value)
{
    int a = Z80_A;

    Z80_A = Z80_A + value;
    do_add_flags(a, value, Z80_A);
}

static void do_adc_byte(int value)
{
    int a = Z80_A;

    Z80_A = Z80_A + value + CARRY_FLAG;
    do_add_flags(a, value, Z80_A);
}

static void do_sub_byte(int value)
{
    int a = Z80_A;

    Z80_A = Z80_A - value;
    do_sub_flags(a, value, Z80_A);
}

static void do_negate(void)
{
    int a = Z80_A;

    Z80_A = - Z80_A;
    do_sub_flags(0, a, Z80_A);
}

static void do_sbc_byte(int value)
{
    int a = Z80_A;

    Z80_A = Z80_A - (value + CARRY_FLAG);
    do_sub_flags(a, value, Z80_A);
}

static void do_add_word(int value)
{
    int a = Z80_HL;

    Z80_HL = Z80_HL + value;

    do_add_word_flags(a, value, Z80_HL);
}

static void do_adc_word(int value)
{
    int a = Z80_HL;

    Z80_HL = Z80_HL + value + CARRY_FLAG;

    do_adc_word_flags(a, value, Z80_HL);
}

static void do_sbc_word(int value)
{
    int a = Z80_HL;

    Z80_HL = Z80_HL - (value + CARRY_FLAG);

    do_sbc_word_flags(a, value, Z80_HL);
}

static void do_add_word_index(Uint16 *regp, int value)
{
    int a = *regp;
    int result = a + value;

    *regp = result;

    do_add_word_flags(a, value, result);
}

/* compare this value with A's contents */
static void do_cp(int value)
{
    /*
     * Sign, carry, and overflow depend upon values of bit 7.
     * Half-carry depends upon values of bit 3.
     * We mask those bits, munge them into an index, and look
     * up the flag values in the above tables.
     * Undocumented flags in bit 3, 5 of F come from the second operand.
     */

    int a = Z80_A;
    int result = a - value;
    int index = ((a & 0x88) >> 1) | ((value & 0x88) >> 2) |
      ((result & 0x88) >> 3);
    int f = SUBTRACT_MASK | subtract_half_carry_table[index & 7] |
      subtract_sign_carry_overflow_table[index >> 4] |
      (value & (UNDOC3_MASK|UNDOC5_MASK));

    if((result & 0xFF) == 0) f |= ZERO_MASK;

    Z80_F = f;
}

static void do_cpd(void)
{
    int oldcarry = Z80_F & CARRY_MASK;
    int a = Z80_A;
    int value = mem_read(Z80_HL);
    int result = a - value;

    Z80_HL--;
    Z80_BC--;

    do_sub_flags(a, value, result);
    Z80_F = (Z80_F & ~(CARRY_MASK | OVERFLOW_MASK | UNDOC5_MASK))
      | oldcarry | (Z80_BC == 0 ? 0 : OVERFLOW_MASK)
      | (((result - ((Z80_F & HALF_CARRY_MASK) >> 4)) & 2) << 4);
    if ((result & 15) == 8 && (Z80_F & HALF_CARRY_MASK) != 0) {
      Z80_F &= ~UNDOC3_MASK;
    }

    T_COUNT(16);
}

static void do_cpi(void)
{
    int oldcarry = Z80_F & CARRY_MASK;
    int a = Z80_A;
    int value = mem_read(Z80_HL);
    int result = a - value;

    Z80_HL++;
    Z80_BC--;

    do_sub_flags(a, value, result);
    Z80_F = (Z80_F & ~(CARRY_MASK | OVERFLOW_MASK | UNDOC5_MASK))
      | oldcarry | (Z80_BC == 0 ? 0 : OVERFLOW_MASK)
      | (((result - ((Z80_F & HALF_CARRY_MASK) >> 4)) & 2) << 4);
    if ((result & 15) == 8 && (Z80_F & HALF_CARRY_MASK) != 0) {
      Z80_F &= ~UNDOC3_MASK;
    }

    T_COUNT(16);
}

#ifdef FAST_MOVE
static void do_cpdr(void)
{
    int oldcarry = Z80_F & CARRY_MASK;
    int a = Z80_A, value, result;
    do
    {
        result = a - (value = mem_read(Z80_HL));
	Z80_HL--;
	Z80_BC--;

	T_COUNT(21);

    } while((Z80_BC != 0) && (result != 0));

    do_sub_flags(a, value, result);
    Z80_F = (Z80_F & ~(CARRY_MASK | OVERFLOW_MASK | UNDOC5_MASK))
      | oldcarry | (Z80_BC == 0 ? 0 : OVERFLOW_MASK)
      | (((result - ((Z80_F & HALF_CARRY_MASK) >> 4)) & 2) << 4);
    if ((result & 15) == 8 && (Z80_F & HALF_CARRY_MASK) != 0) {
      Z80_F &= ~UNDOC3_MASK;
    }

    T_COUNT(-5);
}

static void do_cpir(void)
{
    int oldcarry = Z80_F & CARRY_MASK;
    int a = Z80_A, value, result;
    do
    {
        result = a - (value = mem_read(Z80_HL));
	Z80_HL++;
	Z80_BC--;

	T_COUNT(21);

    } while((Z80_BC != 0) && (result != 0));

    do_sub_flags(a, value, result);
    Z80_F = (Z80_F & ~(CARRY_MASK | OVERFLOW_MASK | UNDOC5_MASK))
      | oldcarry | (Z80_BC == 0 ? 0 : OVERFLOW_MASK)
      | (((result - ((Z80_F & HALF_CARRY_MASK) >> 4)) & 2) << 4);
    if ((result & 15) == 8 && (Z80_F & HALF_CARRY_MASK) != 0) {
      Z80_F &= ~UNDOC3_MASK;
    }

    T_COUNT(-5);
}
#else
static void do_cpdr(void)
{
    do_cpd();
    if(OVERFLOW_FLAG && !ZERO_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}

static void do_cpir(void)
{
    do_cpi();
    if(OVERFLOW_FLAG && !ZERO_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}
#endif

#if 1
/* The following passes ZEXALL and matches Yaze, but if you believe
   http://www.msxnet.org/tech/Z80/z80undoc.txt, it gets UNDOC3 and UNDOC5 wrong.
   It remains to be seen which (if either) is really right. */
static void do_test_bit(int op, int value, int bit)
{
    int result = value & (1 << bit);
    Z80_F = (Z80_F & CARRY_MASK) | HALF_CARRY_MASK | (result & SIGN_MASK)
      | (result ? 0 : (OVERFLOW_MASK | ZERO_MASK))
      | (((op & 7) == 6) ? 0 : (value & (UNDOC3_MASK | UNDOC5_MASK)));
}
#else
/* The following matches http://www.msxnet.org/tech/Z80/z80undoc.txt
   for "bit n,r", but does not attempt to emulate the full weirdness
   of "bit n,(ix/iy+d)" and "bit n,(hl)".  It fails ZEXALL even if
   code is added to make the latter two instructions behave as in
   the version that passes ZEXALL, leading me to think that z80undoc.txt
   may be mistaken about "bit n,r".  This should be checked in detail
   against a real Z80, I suppose.  Ugh. */
static void do_test_bit(int op, int value, int bit)
{
    int result = value & (1 << bit);
    Z80_F = (Z80_F & CARRY_MASK) | HALF_CARRY_MASK
      | (result & (UNDOC3_MASK | UNDOC5_MASK | SIGN_MASK))
      | (result ? 0 : (OVERFLOW_MASK | ZERO_MASK));
}
#endif

static int rl_byte(int value)
{
    /*
     * Compute rotate-left-through-carry
     * operation, setting flags as appropriate.
     */

    Uint8 set = 0;
    int result = ((value << 1) & 0xFF) | CARRY_FLAG;

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;
    if(value & 0x80)
      set |= CARRY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

static int rr_byte(int value)
{
    /*
     * Compute rotate-right-through-carry
     * operation, setting flags as appropriate.
     */

    Uint8 set = 0;
    int result = (value >> 1) | (CARRY_FLAG ? 0x80 : 0);

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;
    if(value & 0x1)
      set |= CARRY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

static int rlc_byte(int value)
{
    /*
     * Compute the result of an RLC operation and set the flags appropriately.
     * This does not do the right thing for the RLCA instruction.
     */

    Uint8 set = 0;
    int result;

    if(value & 0x80)
    {
	result = ((value << 1) & 0xFF) | 1;
	set |= CARRY_MASK;
    }
    else
    {
	result = (value << 1) & 0xFF;
    }

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

static int rrc_byte(int value)
{
    Uint8 set = 0;
    int result;

    if(value & 0x1)
    {
	result = (value >> 1) | 0x80;
	set |= CARRY_MASK;
    }
    else
    {
	result = (value >> 1);
    }

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

/*
 * Perform the RLA, RLCA, RRA, RRCA instructions.  These set the flags
 * differently than the other rotate instrucitons.
 */
static void do_rla(void)
{
    Uint8 set = 0;

    if(Z80_A & 0x80)
      set |= CARRY_MASK;

    Z80_A = ((Z80_A << 1) & 0xFF) | CARRY_FLAG;

    Z80_F = (Z80_F & (OVERFLOW_MASK | ZERO_MASK | SIGN_MASK))
      | set | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK ));
}

static void do_rra(void)
{
    Uint8 set = 0;

    if(Z80_A & 0x1)
      set |= CARRY_MASK;

    Z80_A = (Z80_A >> 1) | (CARRY_FLAG ? 0x80 : 0);

    Z80_F = (Z80_F & (OVERFLOW_MASK | ZERO_MASK | SIGN_MASK))
      | set | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK ));
}

static void do_rlca(void)
{
    Uint8 set = 0;

    if(Z80_A & 0x80)
    {
	Z80_A = ((Z80_A << 1) & 0xFF) | 1;
	set |= CARRY_MASK;
    }
    else
    {
	Z80_A = (Z80_A << 1) & 0xFF;
    }
    Z80_F = (Z80_F & (OVERFLOW_MASK | ZERO_MASK | SIGN_MASK))
      | set | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK ));
}

static void do_rrca(void)
{
    Uint8 set = 0;

    if(Z80_A & 0x1)
    {
	Z80_A = (Z80_A >> 1) | 0x80;
	set |= CARRY_MASK;
    }
    else
    {
	Z80_A = Z80_A >> 1;
    }
    Z80_F = (Z80_F & (OVERFLOW_MASK | ZERO_MASK | SIGN_MASK))
      | set | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK ));
}

static int sla_byte(int value)
{
    Uint8 set = 0;
    int result = (value << 1) & 0xFF;

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;
    if(value & 0x80)
      set |= CARRY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

static int sra_byte(int value)
{
    Uint8 set = 0;
    int result;

    if(value & 0x80)
    {
	result = (value >> 1) | 0x80;
	set |= SIGN_MASK;
    }
    else
    {
	result = value >> 1;
    }

    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;
    if(value & 0x1)
      set |= CARRY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

/* undocumented opcode slia: shift left and increment */
static int slia_byte(int value)
{
    Uint8 set = 0;
    int result = ((value << 1) & 0xFF) | 1;

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;
    if(value & 0x80)
      set |= CARRY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

static int srl_byte(int value)
{
    Uint8 set = 0;
    int result = value >> 1;

    if(result & 0x80)
      set |= SIGN_MASK;
    if(result == 0)
      set |= ZERO_MASK;
    if(parity(result))
      set |= PARITY_MASK;
    if(value & 0x1)
      set |= CARRY_MASK;

    Z80_F = (result & (UNDOC3_MASK | UNDOC5_MASK)) | set;

    return result;
}

static void do_ldd(void)
{
    int moved, undoc;
    mem_write(Z80_DE, moved = mem_read(Z80_HL));
    Z80_DE--;
    Z80_HL--;
    Z80_BC--;

    if(Z80_BC == 0)
      CLEAR_OVERFLOW();
    else
      SET_OVERFLOW();
    undoc = Z80_A + moved;
    Z80_F = (Z80_F & ~(UNDOC3_MASK|UNDOC5_MASK|HALF_CARRY_MASK|SUBTRACT_MASK))
      | (undoc & UNDOC3_MASK) | ((undoc & 2) ? UNDOC5_MASK : 0);
    T_COUNT(16);
}

static void do_ldi(void)
{
    int moved, undoc;
    mem_write(Z80_DE, moved = mem_read(Z80_HL));
    Z80_DE++;
    Z80_HL++;
    Z80_BC--;

    if(Z80_BC == 0)
      CLEAR_OVERFLOW();
    else
      SET_OVERFLOW();
    undoc = Z80_A + moved;
    Z80_F = (Z80_F & ~(UNDOC3_MASK|UNDOC5_MASK|HALF_CARRY_MASK|SUBTRACT_MASK))
      | (undoc & UNDOC3_MASK) | ((undoc & 2) ? UNDOC5_MASK : 0);
    T_COUNT(16);
}

#ifdef FAST_MOVE
static void do_ldir(void)
{
    int moved, undoc;

    do {
      mem_write(Z80_DE, moved = mem_read(Z80_HL));
      Z80_DE++;
      Z80_HL++;
      T_COUNT(21);
    } while (--Z80_BC);

    CLEAR_OVERFLOW();
    undoc = Z80_A + moved;
    Z80_F = (Z80_F & ~(UNDOC3_MASK|UNDOC5_MASK|HALF_CARRY_MASK|SUBTRACT_MASK))
      | (undoc & UNDOC3_MASK) | ((undoc & 2) ? UNDOC5_MASK : 0);
    T_COUNT(-5);
}

static void do_lddr(void)
{
    int moved, undoc;

    do {
      mem_write(Z80_DE, moved = mem_read(Z80_HL));
      Z80_DE--;
      Z80_HL--;
      T_COUNT(21);
    } while (--Z80_BC);

    CLEAR_OVERFLOW();
    undoc = Z80_A + moved;
    Z80_F = (Z80_F & ~(UNDOC3_MASK|UNDOC5_MASK|HALF_CARRY_MASK|SUBTRACT_MASK))
      | (undoc & UNDOC3_MASK) | ((undoc & 2) ? UNDOC5_MASK : 0);
    T_COUNT(-5);
}
#else
static void do_ldir(void)
{
    do_ldi();
    if(OVERFLOW_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}

static void do_lddr(void)
{
    do_ldd();
    if(OVERFLOW_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}
#endif

static void do_ld_a_i(void)
{
    Uint8 set = 0;

    Z80_A = Z80_I;

    if(Z80_A & 0x80)
      set |= SIGN_MASK;
    if(Z80_A == 0)
      set |= ZERO_MASK;

    if(z80_state.iff2)
      set |= OVERFLOW_MASK;

    Z80_F = (Z80_F & CARRY_MASK) | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK)) | set;
}

static void do_ld_a_r(void)
{
    Uint8 set = 0;

    Z80_A = (Z80_R & 0x7F) | Z80_R7;

    if(Z80_A & 0x80)
      set |= SIGN_MASK;
    if(Z80_A == 0)
      set |= ZERO_MASK;

    if(z80_state.iff2)
      set |= OVERFLOW_MASK;

    Z80_F = (Z80_F & CARRY_MASK) | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK)) | set;
}

/* Completely new implementation adapted from yaze.
   The old one was very wrong. */
static void do_daa(void)
{
  int a = Z80_A, f = Z80_F;
  int alow = a & 0xf;
  int carry = f & CARRY_MASK;
  int hcarry = f & HALF_CARRY_MASK;
  if (f & SUBTRACT_MASK) {
    int hd = carry || a > 0x99;
    if (hcarry || alow > 9) {
       if (alow > 5) hcarry = 0;
       a = (a - 6) & 0xff;
     }
     if (hd) a -= 0x160;
  } else {
    if (hcarry || alow > 9) {
      hcarry = alow > 9 ? HALF_CARRY_MASK : 0;
      a += 6;
    }
    if (carry || ((a & 0x1f0) > 0x90)) {
      a += 0x60;
    }
  }
  if (a & 0x100) carry = CARRY_MASK;

  Z80_A = a = a & 0xff;
  Z80_F = ((a & 0x80) ? SIGN_MASK : 0)
    | (a & (UNDOC3_MASK|UNDOC5_MASK))
    | (a ? 0 : ZERO_MASK)
    | (f & SUBTRACT_MASK)
    | hcarry | (parity(a) ? PARITY_MASK : 0) | carry;
}

static void do_rld(void)
{
    /*
     * Rotate-left-decimal.
     */
    Uint8 set = 0;
    int old_value = mem_read(Z80_HL);

    /* left-shift old value, add lower bits of a */
    int new_value = ((old_value << 4) | (Z80_A & 0x0f)) & 0xff;

    /* rotate high bits of old value into low bits of a */
    Z80_A = (Z80_A & 0xf0) | (old_value >> 4);

    if(Z80_A & 0x80)
      set |= SIGN_MASK;
    if(Z80_A == 0)
      set |= ZERO_MASK;
    if(parity(Z80_A))
      set |= PARITY_MASK;

    Z80_F = (Z80_F & CARRY_MASK) | set | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK));
    mem_write(Z80_HL,new_value);
}

static void do_rrd(void)
{
    /*
     * Rotate-right-decimal.
     */
    Uint8 set = 0;
    int old_value = mem_read(Z80_HL);

    /* right-shift old value, add lower bits of a */
    int new_value = (old_value >> 4) | ((Z80_A & 0x0f) << 4);

    /* rotate low bits of old value into low bits of a */
    Z80_A = (Z80_A & 0xf0) | (old_value & 0x0f);

    if(Z80_A & 0x80)
      set |= SIGN_MASK;
    if(Z80_A == 0)
      set |= ZERO_MASK;
    if(parity(Z80_A))
      set |= PARITY_MASK;

    Z80_F = (Z80_F & CARRY_MASK) | set | (Z80_A & (UNDOC3_MASK | UNDOC5_MASK));
    mem_write(Z80_HL,new_value);
}


/*
 * Input/output instruction support:
 */

static void do_ind(void)
{
    mem_write(Z80_HL, z80_in(Z80_C));
    Z80_HL--;
    Z80_B--;

    if(Z80_B == 0)
      SET_ZERO();
    else
      CLEAR_ZERO();

    SET_SUBTRACT();
    T_COUNT(15);
}

#ifdef FAST_MOVE
static void do_indr(void)
{
    do
    {
	mem_write(Z80_HL, z80_in(Z80_C));
	Z80_HL--;
	Z80_B--;
	T_COUNT(20);
    } while(Z80_B != 0);
    T_COUNT(-5);

    SET_ZERO();
    SET_SUBTRACT();
}
#else
static void do_indr(void)
{
    do_ind();
    if (!ZERO_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}
#endif

static void do_ini(void)
{
    mem_write(Z80_HL, z80_in(Z80_C));
    Z80_HL++;
    Z80_B--;

    if(Z80_B == 0)
      SET_ZERO();
    else
      CLEAR_ZERO();

    SET_SUBTRACT();
    T_COUNT(15);
}

#ifdef FAST_MOVE
static void do_inir(void)
{
    do
    {
	mem_write(Z80_HL, z80_in(Z80_C));
	Z80_HL++;
	Z80_B--;
	T_COUNT(20);
    } while(Z80_B != 0);
    T_COUNT(-5);

    SET_ZERO();
    SET_SUBTRACT();
}
#else
static void do_inir(void)
{
    do_ini();
    if(!ZERO_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}
#endif

static int in_with_flags(int port)
{
    /*
     * Do the appropriate flag calculations for the in instructions
     * which compute the flags.  Return the input value.
     */

    int value = z80_in(port);
    Uint8 clear = (Uint8) ~(SIGN_MASK | ZERO_MASK | HALF_CARRY_MASK |
			    PARITY_MASK | SUBTRACT_MASK);
    Uint8 set = 0;

    if(value & 0x80)
      set |= SIGN_MASK;
    if(value == 0)
      set |= ZERO_MASK;
    if(parity(value))
      set |= PARITY_MASK;

    /* What should the half-carry do?  Is this a mistake? */

    Z80_F = (Z80_F & clear) | set;

    return value;
}

static void do_outd(void)
{
    z80_out(Z80_C, mem_read(Z80_HL));
    Z80_HL--;
    Z80_B--;

    if(Z80_B == 0)
      SET_ZERO();
    else
      CLEAR_ZERO();

    SET_SUBTRACT();
    T_COUNT(15);
}

#ifdef FAST_MOVE
static void do_outdr(void)
{
    do
    {
	z80_out(Z80_C, mem_read(Z80_HL));
	Z80_HL--;
	Z80_B--;
	T_COUNT(20);
    } while(Z80_B != 0);
    T_COUNT(-5);

    SET_ZERO();
    SET_SUBTRACT();
}
#else
static void do_outdr(void)
{
    do_outd();
    if(!ZERO_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}
#endif

static void do_outi(void)
{
    z80_out(Z80_C, mem_read(Z80_HL));
    Z80_HL++;
    Z80_B--;

    if(Z80_B == 0)
      SET_ZERO();
    else
      CLEAR_ZERO();

    SET_SUBTRACT();
    T_COUNT(15);
}

#ifdef FAST_MOVE
static void do_outir(void)
{
    do
    {
	z80_out(Z80_C, mem_read(Z80_HL));
	Z80_HL++;
	Z80_B--;
	T_COUNT(20);
    } while(Z80_B != 0);
    T_COUNT(-5);

    SET_ZERO();
    SET_SUBTRACT();
}
#else
static void do_outir(void)
{
    do_outi();
    if (!ZERO_FLAG) {
      Z80_PC -= 2;
      T_COUNT(5);
    }
}
#endif


/*
 * Interrupt handling routines:
 */

static void do_di(void)
{
    z80_state.iff1 = z80_state.iff2 = 0;
}

static void do_ei(void)
{
    z80_state.iff1 = z80_state.iff2 = 1;
}

static void do_im0(void)
{
    z80_state.interrupt_mode = 0;
}

static void do_im1(void)
{
    z80_state.interrupt_mode = 1;
}

static void do_im2(void)
{
    z80_state.interrupt_mode = 2;
}

static void do_int(void)
{
    /* handle a maskable interrupt */
    Z80_SP -= 2;
    mem_write_word(Z80_SP, Z80_PC);
    z80_state.iff1 = z80_state.iff2 = 0;
    Z80_R++;
    switch (z80_state.interrupt_mode) {
    case 0:
      /* Z80_PC = get_irq_vector() & 0x38; */
      T_COUNT(13);
      error("interrupt in im0 not supported");
      break;
    case 1:
      Z80_PC = 0x38;
      T_COUNT(13);
      break;
    case 2:
      /* Z80_PC = Z80_I << 8 + get_irq_vector(); */
      T_COUNT(19);
      error("interrupt in im2 not supported");
      break;
    }
}

static void do_nmi(void)
{
    /* handle a non-maskable interrupt */
    Z80_SP -= 2;
    mem_write_word(Z80_SP, Z80_PC);
    z80_state.iff1 = 0;
    Z80_R++;
    Z80_PC = 0x66;
    T_COUNT(11);
}

/*
 * Extended instructions which have 0xCB as the first byte:
 */
static void do_CB_instruction(void)
{
    Uint8 instruction = mem_read(Z80_PC++);

    switch(instruction)
    {
      case 0x47:	/* bit 0, a */
	do_test_bit(instruction, Z80_A, 0);  T_COUNT(8);
	break;
      case 0x40:	/* bit 0, b */
	do_test_bit(instruction, Z80_B, 0);  T_COUNT(8);
	break;
      case 0x41:	/* bit 0, c */
	do_test_bit(instruction, Z80_C, 0);  T_COUNT(8);
	break;
      case 0x42:	/* bit 0, d */
	do_test_bit(instruction, Z80_D, 0);  T_COUNT(8);
	break;
      case 0x43:	/* bit 0, e */
	do_test_bit(instruction, Z80_E, 0);  T_COUNT(8);
	break;
      case 0x44:	/* bit 0, h */
	do_test_bit(instruction, Z80_H, 0);  T_COUNT(8);
	break;
      case 0x45:	/* bit 0, l */
	do_test_bit(instruction, Z80_L, 0);  T_COUNT(8);
	break;
      case 0x4F:	/* bit 1, a */
	do_test_bit(instruction, Z80_A, 1);  T_COUNT(8);
	break;
      case 0x48:	/* bit 1, b */
	do_test_bit(instruction, Z80_B, 1);  T_COUNT(8);
	break;
      case 0x49:	/* bit 1, c */
	do_test_bit(instruction, Z80_C, 1);  T_COUNT(8);
	break;
      case 0x4A:	/* bit 1, d */
	do_test_bit(instruction, Z80_D, 1);  T_COUNT(8);
	break;
      case 0x4B:	/* bit 1, e */
	do_test_bit(instruction, Z80_E, 1);  T_COUNT(8);
	break;
      case 0x4C:	/* bit 1, h */
	do_test_bit(instruction, Z80_H, 1);  T_COUNT(8);
	break;
      case 0x4D:	/* bit 1, l */
	do_test_bit(instruction, Z80_L, 1);  T_COUNT(8);
	break;
      case 0x57:	/* bit 2, a */
	do_test_bit(instruction, Z80_A, 2);  T_COUNT(8);
	break;
      case 0x50:	/* bit 2, b */
	do_test_bit(instruction, Z80_B, 2);  T_COUNT(8);
	break;
      case 0x51:	/* bit 2, c */
	do_test_bit(instruction, Z80_C, 2);  T_COUNT(8);
	break;
      case 0x52:	/* bit 2, d */
	do_test_bit(instruction, Z80_D, 2);  T_COUNT(8);
	break;
      case 0x53:	/* bit 2, e */
	do_test_bit(instruction, Z80_E, 2);  T_COUNT(8);
	break;
      case 0x54:	/* bit 2, h */
	do_test_bit(instruction, Z80_H, 2);  T_COUNT(8);
	break;
      case 0x55:	/* bit 2, l */
	do_test_bit(instruction, Z80_L, 2);  T_COUNT(8);
	break;
      case 0x5F:	/* bit 3, a */
	do_test_bit(instruction, Z80_A, 3);  T_COUNT(8);
	break;
      case 0x58:	/* bit 3, b */
	do_test_bit(instruction, Z80_B, 3);  T_COUNT(8);
	break;
      case 0x59:	/* bit 3, c */
	do_test_bit(instruction, Z80_C, 3);  T_COUNT(8);
	break;
      case 0x5A:	/* bit 3, d */
	do_test_bit(instruction, Z80_D, 3);  T_COUNT(8);
	break;
      case 0x5B:	/* bit 3, e */
	do_test_bit(instruction, Z80_E, 3);  T_COUNT(8);
	break;
      case 0x5C:	/* bit 3, h */
	do_test_bit(instruction, Z80_H, 3);  T_COUNT(8);
	break;
      case 0x5D:	/* bit 3, l */
	do_test_bit(instruction, Z80_L, 3);  T_COUNT(8);
	break;
      case 0x67:	/* bit 4, a */
	do_test_bit(instruction, Z80_A, 4);  T_COUNT(8);
	break;
      case 0x60:	/* bit 4, b */
	do_test_bit(instruction, Z80_B, 4);  T_COUNT(8);
	break;
      case 0x61:	/* bit 4, c */
	do_test_bit(instruction, Z80_C, 4);  T_COUNT(8);
	break;
      case 0x62:	/* bit 4, d */
	do_test_bit(instruction, Z80_D, 4);  T_COUNT(8);
	break;
      case 0x63:	/* bit 4, e */
	do_test_bit(instruction, Z80_E, 4);  T_COUNT(8);
	break;
      case 0x64:	/* bit 4, h */
	do_test_bit(instruction, Z80_H, 4);  T_COUNT(8);
	break;
      case 0x65:	/* bit 4, l */
	do_test_bit(instruction, Z80_L, 4);  T_COUNT(8);
	break;
      case 0x6F:	/* bit 5, a */
	do_test_bit(instruction, Z80_A, 5);  T_COUNT(8);
	break;
      case 0x68:	/* bit 5, b */
	do_test_bit(instruction, Z80_B, 5);  T_COUNT(8);
	break;
      case 0x69:	/* bit 5, c */
	do_test_bit(instruction, Z80_C, 5);  T_COUNT(8);
	break;
      case 0x6A:	/* bit 5, d */
	do_test_bit(instruction, Z80_D, 5);  T_COUNT(8);
	break;
      case 0x6B:	/* bit 5, e */
	do_test_bit(instruction, Z80_E, 5);  T_COUNT(8);
	break;
      case 0x6C:	/* bit 5, h */
	do_test_bit(instruction, Z80_H, 5);  T_COUNT(8);
	break;
      case 0x6D:	/* bit 5, l */
	do_test_bit(instruction, Z80_L, 5);  T_COUNT(8);
	break;
      case 0x77:	/* bit 6, a */
	do_test_bit(instruction, Z80_A, 6);  T_COUNT(8);
	break;
      case 0x70:	/* bit 6, b */
	do_test_bit(instruction, Z80_B, 6);  T_COUNT(8);
	break;
      case 0x71:	/* bit 6, c */
	do_test_bit(instruction, Z80_C, 6);  T_COUNT(8);
	break;
      case 0x72:	/* bit 6, d */
	do_test_bit(instruction, Z80_D, 6);  T_COUNT(8);
	break;
      case 0x73:	/* bit 6, e */
	do_test_bit(instruction, Z80_E, 6);  T_COUNT(8);
	break;
      case 0x74:	/* bit 6, h */
	do_test_bit(instruction, Z80_H, 6);  T_COUNT(8);
	break;
      case 0x75:	/* bit 6, l */
	do_test_bit(instruction, Z80_L, 6);  T_COUNT(8);
	break;
      case 0x7F:	/* bit 7, a */
	do_test_bit(instruction, Z80_A, 7);  T_COUNT(8);
	break;
      case 0x78:	/* bit 7, b */
	do_test_bit(instruction, Z80_B, 7);  T_COUNT(8);
	break;
      case 0x79:	/* bit 7, c */
	do_test_bit(instruction, Z80_C, 7);  T_COUNT(8);
	break;
      case 0x7A:	/* bit 7, d */
	do_test_bit(instruction, Z80_D, 7);  T_COUNT(8);
	break;
      case 0x7B:	/* bit 7, e */
	do_test_bit(instruction, Z80_E, 7);  T_COUNT(8);
	break;
      case 0x7C:	/* bit 7, h */
	do_test_bit(instruction, Z80_H, 7);  T_COUNT(8);
	break;
      case 0x7D:	/* bit 7, l */
	do_test_bit(instruction, Z80_L, 7);  T_COUNT(8);
	break;

      case 0x46:	/* bit 0, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 0);  T_COUNT(12);
	break;
      case 0x4E:	/* bit 1, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 1);  T_COUNT(12);
	break;
      case 0x56:	/* bit 2, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 2);  T_COUNT(12);
	break;
      case 0x5E:	/* bit 3, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 3);  T_COUNT(12);
	break;
      case 0x66:	/* bit 4, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 4);  T_COUNT(12);
	break;
      case 0x6E:	/* bit 5, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 5);  T_COUNT(12);
	break;
      case 0x76:	/* bit 6, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 6);  T_COUNT(12);
	break;
      case 0x7E:	/* bit 7, (hl) */
	do_test_bit(instruction, mem_read(Z80_HL), 7);  T_COUNT(12);
	break;

      case 0x87:	/* res 0, a */
	Z80_A &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x80:	/* res 0, b */
	Z80_B &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x81:	/* res 0, c */
	Z80_C &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x82:	/* res 0, d */
	Z80_D &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x83:	/* res 0, e */
	Z80_E &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x84:	/* res 0, h */
	Z80_H &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x85:	/* res 0, l */
	Z80_L &= ~(1 << 0);  T_COUNT(8);
	break;
      case 0x8F:	/* res 1, a */
	Z80_A &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x88:	/* res 1, b */
	Z80_B &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x89:	/* res 1, c */
	Z80_C &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x8A:	/* res 1, d */
	Z80_D &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x8B:	/* res 1, e */
	Z80_E &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x8C:	/* res 1, h */
	Z80_H &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x8D:	/* res 1, l */
	Z80_L &= ~(1 << 1);  T_COUNT(8);
	break;
      case 0x97:	/* res 2, a */
	Z80_A &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x90:	/* res 2, b */
	Z80_B &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x91:	/* res 2, c */
	Z80_C &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x92:	/* res 2, d */
	Z80_D &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x93:	/* res 2, e */
	Z80_E &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x94:	/* res 2, h */
	Z80_H &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x95:	/* res 2, l */
	Z80_L &= ~(1 << 2);  T_COUNT(8);
	break;
      case 0x9F:	/* res 3, a */
	Z80_A &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0x98:	/* res 3, b */
	Z80_B &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0x99:	/* res 3, c */
	Z80_C &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0x9A:	/* res 3, d */
	Z80_D &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0x9B:	/* res 3, e */
	Z80_E &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0x9C:	/* res 3, h */
	Z80_H &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0x9D:	/* res 3, l */
	Z80_L &= ~(1 << 3);  T_COUNT(8);
	break;
      case 0xA7:	/* res 4, a */
	Z80_A &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xA0:	/* res 4, b */
	Z80_B &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xA1:	/* res 4, c */
	Z80_C &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xA2:	/* res 4, d */
	Z80_D &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xA3:	/* res 4, e */
	Z80_E &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xA4:	/* res 4, h */
	Z80_H &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xA5:	/* res 4, l */
	Z80_L &= ~(1 << 4);  T_COUNT(8);
	break;
      case 0xAF:	/* res 5, a */
	Z80_A &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xA8:	/* res 5, b */
	Z80_B &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xA9:	/* res 5, c */
	Z80_C &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xAA:	/* res 5, d */
	Z80_D &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xAB:	/* res 5, e */
	Z80_E &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xAC:	/* res 5, h */
	Z80_H &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xAD:	/* res 5, l */
	Z80_L &= ~(1 << 5);  T_COUNT(8);
	break;
      case 0xB7:	/* res 6, a */
	Z80_A &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xB0:	/* res 6, b */
	Z80_B &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xB1:	/* res 6, c */
	Z80_C &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xB2:	/* res 6, d */
	Z80_D &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xB3:	/* res 6, e */
	Z80_E &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xB4:	/* res 6, h */
	Z80_H &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xB5:	/* res 6, l */
	Z80_L &= ~(1 << 6);  T_COUNT(8);
	break;
      case 0xBF:	/* res 7, a */
	Z80_A &= ~(1 << 7);  T_COUNT(8);
	break;
      case 0xB8:	/* res 7, b */
	Z80_B &= ~(1 << 7);  T_COUNT(8);
	break;
      case 0xB9:	/* res 7, c */
	Z80_C &= ~(1 << 7);  T_COUNT(8);
	break;
      case 0xBA:	/* res 7, d */
	Z80_D &= ~(1 << 7);  T_COUNT(8);
	break;
      case 0xBB:	/* res 7, e */
	Z80_E &= ~(1 << 7);  T_COUNT(8);
	break;
      case 0xBC:	/* res 7, h */
	Z80_H &= ~(1 << 7);  T_COUNT(8);
	break;
      case 0xBD:	/* res 7, l */
	Z80_L &= ~(1 << 7);  T_COUNT(8);
	break;

      case 0x86:	/* res 0, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 0));  T_COUNT(15);
	break;
      case 0x8E:	/* res 1, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 1));  T_COUNT(15);
	break;
      case 0x96:	/* res 2, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 2));  T_COUNT(15);
	break;
      case 0x9E:	/* res 3, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 3));  T_COUNT(15);
	break;
      case 0xA6:	/* res 4, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 4));  T_COUNT(15);
	break;
      case 0xAE:	/* res 5, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 5));  T_COUNT(15);
	break;
      case 0xB6:	/* res 6, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 6));  T_COUNT(15);
	break;
      case 0xBE:	/* res 7, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) & ~(1 << 7));  T_COUNT(15);
	break;

      case 0x17:	/* rl a */
	Z80_A = rl_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x10:	/* rl b */
	Z80_B = rl_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x11:	/* rl c */
	Z80_C = rl_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x12:	/* rl d */
	Z80_D = rl_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x13:	/* rl e */
	Z80_E = rl_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x14:	/* rl h */
	Z80_H = rl_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x15:	/* rl l */
	Z80_L = rl_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x16:	/* rl (hl) */
	mem_write(Z80_HL, rl_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0x07:	/* rlc a */
	Z80_A = rlc_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x00:	/* rlc b */
	Z80_B = rlc_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x01:	/* rlc c */
	Z80_C = rlc_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x02:	/* rlc d */
	Z80_D = rlc_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x03:	/* rlc e */
	Z80_E = rlc_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x04:	/* rlc h */
	Z80_H = rlc_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x05:	/* rlc l */
	Z80_L = rlc_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x06:	/* rlc (hl) */
	mem_write(Z80_HL, rlc_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0x1F:	/* rr a */
	Z80_A = rr_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x18:	/* rr b */
	Z80_B = rr_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x19:	/* rr c */
	Z80_C = rr_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x1A:	/* rr d */
	Z80_D = rr_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x1B:	/* rr e */
	Z80_E = rr_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x1C:	/* rr h */
	Z80_H = rr_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x1D:	/* rr l */
	Z80_L = rr_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x1E:	/* rr (hl) */
	mem_write(Z80_HL, rr_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0x0F:	/* rrc a */
	Z80_A = rrc_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x08:	/* rrc b */
	Z80_B = rrc_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x09:	/* rrc c */
	Z80_C = rrc_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x0A:	/* rrc d */
	Z80_D = rrc_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x0B:	/* rrc e */
	Z80_E = rrc_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x0C:	/* rrc h */
	Z80_H = rrc_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x0D:	/* rrc l */
	Z80_L = rrc_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x0E:	/* rrc (hl) */
	mem_write(Z80_HL, rrc_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0xC7:	/* set 0, a */
	Z80_A |= (1 << 0);  T_COUNT(8);
	break;
      case 0xC0:	/* set 0, b */
	Z80_B |= (1 << 0);  T_COUNT(8);
	break;
      case 0xC1:	/* set 0, c */
	Z80_C |= (1 << 0);  T_COUNT(8);
	break;
      case 0xC2:	/* set 0, d */
	Z80_D |= (1 << 0);  T_COUNT(8);
	break;
      case 0xC3:	/* set 0, e */
	Z80_E |= (1 << 0);  T_COUNT(8);
	break;
      case 0xC4:	/* set 0, h */
	Z80_H |= (1 << 0);  T_COUNT(8);
	break;
      case 0xC5:	/* set 0, l */
	Z80_L |= (1 << 0);  T_COUNT(8);
	break;
      case 0xCF:	/* set 1, a */
	Z80_A |= (1 << 1);  T_COUNT(8);
	break;
      case 0xC8:	/* set 1, b */
	Z80_B |= (1 << 1);  T_COUNT(8);
	break;
      case 0xC9:	/* set 1, c */
	Z80_C |= (1 << 1);  T_COUNT(8);
	break;
      case 0xCA:	/* set 1, d */
	Z80_D |= (1 << 1);  T_COUNT(8);
	break;
      case 0xCB:	/* set 1, e */
	Z80_E |= (1 << 1);  T_COUNT(8);
	break;
      case 0xCC:	/* set 1, h */
	Z80_H |= (1 << 1);  T_COUNT(8);
	break;
      case 0xCD:	/* set 1, l */
	Z80_L |= (1 << 1);  T_COUNT(8);
	break;
      case 0xD7:	/* set 2, a */
	Z80_A |= (1 << 2);  T_COUNT(8);
	break;
      case 0xD0:	/* set 2, b */
	Z80_B |= (1 << 2);  T_COUNT(8);
	break;
      case 0xD1:	/* set 2, c */
	Z80_C |= (1 << 2);  T_COUNT(8);
	break;
      case 0xD2:	/* set 2, d */
	Z80_D |= (1 << 2);  T_COUNT(8);
	break;
      case 0xD3:	/* set 2, e */
	Z80_E |= (1 << 2);  T_COUNT(8);
	break;
      case 0xD4:	/* set 2, h */
	Z80_H |= (1 << 2);  T_COUNT(8);
	break;
      case 0xD5:	/* set 2, l */
	Z80_L |= (1 << 2);  T_COUNT(8);
	break;
      case 0xDF:	/* set 3, a */
	Z80_A |= (1 << 3);  T_COUNT(8);
	break;
      case 0xD8:	/* set 3, b */
	Z80_B |= (1 << 3);  T_COUNT(8);
	break;
      case 0xD9:	/* set 3, c */
	Z80_C |= (1 << 3);  T_COUNT(8);
	break;
      case 0xDA:	/* set 3, d */
	Z80_D |= (1 << 3);  T_COUNT(8);
	break;
      case 0xDB:	/* set 3, e */
	Z80_E |= (1 << 3);  T_COUNT(8);
	break;
      case 0xDC:	/* set 3, h */
	Z80_H |= (1 << 3);  T_COUNT(8);
	break;
      case 0xDD:	/* set 3, l */
	Z80_L |= (1 << 3);  T_COUNT(8);
	break;
      case 0xE7:	/* set 4, a */
	Z80_A |= (1 << 4);  T_COUNT(8);
	break;
      case 0xE0:	/* set 4, b */
	Z80_B |= (1 << 4);  T_COUNT(8);
	break;
      case 0xE1:	/* set 4, c */
	Z80_C |= (1 << 4);  T_COUNT(8);
	break;
      case 0xE2:	/* set 4, d */
	Z80_D |= (1 << 4);  T_COUNT(8);
	break;
      case 0xE3:	/* set 4, e */
	Z80_E |= (1 << 4);  T_COUNT(8);
	break;
      case 0xE4:	/* set 4, h */
	Z80_H |= (1 << 4);  T_COUNT(8);
	break;
      case 0xE5:	/* set 4, l */
	Z80_L |= (1 << 4);  T_COUNT(8);
	break;
      case 0xEF:	/* set 5, a */
	Z80_A |= (1 << 5);  T_COUNT(8);
	break;
      case 0xE8:	/* set 5, b */
	Z80_B |= (1 << 5);  T_COUNT(8);
	break;
      case 0xE9:	/* set 5, c */
	Z80_C |= (1 << 5);  T_COUNT(8);
	break;
      case 0xEA:	/* set 5, d */
	Z80_D |= (1 << 5);  T_COUNT(8);
	break;
      case 0xEB:	/* set 5, e */
	Z80_E |= (1 << 5);  T_COUNT(8);
	break;
      case 0xEC:	/* set 5, h */
	Z80_H |= (1 << 5);  T_COUNT(8);
	break;
      case 0xED:	/* set 5, l */
	Z80_L |= (1 << 5);  T_COUNT(8);
	break;
      case 0xF7:	/* set 6, a */
	Z80_A |= (1 << 6);  T_COUNT(8);
	break;
      case 0xF0:	/* set 6, b */
	Z80_B |= (1 << 6);  T_COUNT(8);
	break;
      case 0xF1:	/* set 6, c */
	Z80_C |= (1 << 6);  T_COUNT(8);
	break;
      case 0xF2:	/* set 6, d */
	Z80_D |= (1 << 6);  T_COUNT(8);
	break;
      case 0xF3:	/* set 6, e */
	Z80_E |= (1 << 6);  T_COUNT(8);
	break;
      case 0xF4:	/* set 6, h */
	Z80_H |= (1 << 6);  T_COUNT(8);
	break;
      case 0xF5:	/* set 6, l */
	Z80_L |= (1 << 6);  T_COUNT(8);
	break;
      case 0xFF:	/* set 7, a */
	Z80_A |= (1 << 7);  T_COUNT(8);
	break;
      case 0xF8:	/* set 7, b */
	Z80_B |= (1 << 7);  T_COUNT(8);
	break;
      case 0xF9:	/* set 7, c */
	Z80_C |= (1 << 7);  T_COUNT(8);
	break;
      case 0xFA:	/* set 7, d */
	Z80_D |= (1 << 7);  T_COUNT(8);
	break;
      case 0xFB:	/* set 7, e */
	Z80_E |= (1 << 7);  T_COUNT(8);
	break;
      case 0xFC:	/* set 7, h */
	Z80_H |= (1 << 7);  T_COUNT(8);
	break;
      case 0xFD:	/* set 7, l */
	Z80_L |= (1 << 7);  T_COUNT(8);
	break;

      case 0xC6:	/* set 0, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 0));  T_COUNT(15);
	break;
      case 0xCE:	/* set 1, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 1));  T_COUNT(15);
	break;
      case 0xD6:	/* set 2, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 2));  T_COUNT(15);
	break;
      case 0xDE:	/* set 3, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 3));  T_COUNT(15);
	break;
      case 0xE6:	/* set 4, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 4));  T_COUNT(15);
	break;
      case 0xEE:	/* set 5, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 5));  T_COUNT(15);
	break;
      case 0xF6:	/* set 6, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 6));  T_COUNT(15);
	break;
      case 0xFE:	/* set 7, (hl) */
	mem_write(Z80_HL, mem_read(Z80_HL) | (1 << 7));  T_COUNT(15);
	break;

      case 0x27:	/* sla a */
	Z80_A = sla_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x20:	/* sla b */
	Z80_B = sla_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x21:	/* sla c */
	Z80_C = sla_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x22:	/* sla d */
	Z80_D = sla_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x23:	/* sla e */
	Z80_E = sla_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x24:	/* sla h */
	Z80_H = sla_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x25:	/* sla l */
	Z80_L = sla_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x26:	/* sla (hl) */
	mem_write(Z80_HL, sla_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0x2F:	/* sra a */
	Z80_A = sra_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x28:	/* sra b */
	Z80_B = sra_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x29:	/* sra c */
	Z80_C = sra_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x2A:	/* sra d */
	Z80_D = sra_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x2B:	/* sra e */
	Z80_E = sra_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x2C:	/* sra h */
	Z80_H = sra_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x2D:	/* sra l */
	Z80_L = sra_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x2E:	/* sra (hl) */
	mem_write(Z80_HL, sra_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0x37:	/* slia a [undocumented] */
	Z80_A = slia_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x30:	/* slia b [undocumented] */
	Z80_B = slia_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x31:	/* slia c [undocumented] */
	Z80_C = slia_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x32:	/* slia d [undocumented] */
	Z80_D = slia_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x33:	/* slia e [undocumented] */
	Z80_E = slia_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x34:	/* slia h [undocumented] */
	Z80_H = slia_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x35:	/* slia l [undocumented] */
	Z80_L = slia_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x36:	/* slia (hl) [undocumented] */
	mem_write(Z80_HL, slia_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;

      case 0x3F:	/* srl a */
	Z80_A = srl_byte(Z80_A);  T_COUNT(8);
	break;
      case 0x38:	/* srl b */
	Z80_B = srl_byte(Z80_B);  T_COUNT(8);
	break;
      case 0x39:	/* srl c */
	Z80_C = srl_byte(Z80_C);  T_COUNT(8);
	break;
      case 0x3A:	/* srl d */
	Z80_D = srl_byte(Z80_D);  T_COUNT(8);
	break;
      case 0x3B:	/* srl e */
	Z80_E = srl_byte(Z80_E);  T_COUNT(8);
	break;
      case 0x3C:	/* srl h */
	Z80_H = srl_byte(Z80_H);  T_COUNT(8);
	break;
      case 0x3D:	/* srl l */
	Z80_L = srl_byte(Z80_L);  T_COUNT(8);
	break;
      case 0x3E:	/* srl (hl) */
	mem_write(Z80_HL, srl_byte(mem_read(Z80_HL)));  T_COUNT(15);
	break;
    }
}


/*
 * Extended instructions which have 0xDD or 0xFD as the first byte:
 */
static void do_indexed_instruction(Uint16 *ixp)
{
    Uint8 instruction = mem_read(Z80_PC++);

    switch(instruction)
    {
	/* same for FD, except uses IY */

      case 0x8E:	/* adc a, (ix + offset) */
	do_adc_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0x86:	/* add a, (ix + offset) */
	do_add_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0x09:	/* add ix, bc */
	do_add_word_index(ixp, Z80_BC);  T_COUNT(15);
	break;
      case 0x19:	/* add ix, de */
	do_add_word_index(ixp, Z80_DE);  T_COUNT(15);
	break;
      case 0x29:	/* add ix, ix */
	do_add_word_index(ixp, *ixp);  T_COUNT(15);
	break;
      case 0x39:	/* add ix, sp */
	do_add_word_index(ixp, Z80_SP);  T_COUNT(15);
	break;

      case 0xA6:	/* and (ix + offset) */
	do_and_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0xBE:	/* cp (ix + offset) */
	do_cp(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0x35:	/* dec (ix + offset) */
        {
	  Uint16 address = *ixp + (signed char) mem_read(Z80_PC++);
	  Uint8 value = mem_read(address) - 1;
	  mem_write(address, value);
	  do_flags_dec_byte(value);
        }
	T_COUNT(23);
	break;

      case 0x2B:	/* dec ix */
	(*ixp)--;
	T_COUNT(10);
	break;

      case 0xE3:	/* ex (sp), ix */
        {
	  Uint16 temp = mem_read_word(Z80_SP);
	  mem_write_word(Z80_SP, *ixp);
	  *ixp = temp;
        }
	T_COUNT(23);
	break;

      case 0x34:	/* inc (ix + offset) */
        {
	  Uint16 address = *ixp + (signed char) mem_read(Z80_PC++);
	  Uint8 value = mem_read(address) + 1;
	  mem_write(address, value);
	  do_flags_inc_byte(value);
        }
	T_COUNT(23);
	break;

      case 0x23:	/* inc ix */
	(*ixp)++;
	T_COUNT(10);
	break;

      case 0xE9:	/* jp (ix) */
	Z80_PC = *ixp;
	T_COUNT(8);
	break;

      case 0x7E:	/* ld a, (ix + offset) */
	Z80_A = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;
      case 0x46:	/* ld b, (ix + offset) */
	Z80_B = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;
      case 0x4E:	/* ld c, (ix + offset) */
	Z80_C = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;
      case 0x56:	/* ld d, (ix + offset) */
	Z80_D = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;
      case 0x5E:	/* ld e, (ix + offset) */
	Z80_E = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;
      case 0x66:	/* ld h, (ix + offset) */
	Z80_H = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;
      case 0x6E:	/* ld l, (ix + offset) */
	Z80_L = mem_read(*ixp + (signed char) mem_read(Z80_PC++));
	T_COUNT(19);
	break;

      case 0x36:	/* ld (ix + offset), value */
	mem_write(*ixp + (signed char) mem_read(Z80_PC), mem_read(Z80_PC+1));
	Z80_PC += 2;
	T_COUNT(19);
	break;

      case 0x77:	/* ld (ix + offset), a */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_A);
	T_COUNT(19);
	break;
      case 0x70:	/* ld (ix + offset), b */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_B);
	T_COUNT(19);
	break;
      case 0x71:	/* ld (ix + offset), c */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_C);
	T_COUNT(19);
	break;
      case 0x72:	/* ld (ix + offset), d */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_D);
	T_COUNT(19);
	break;
      case 0x73:	/* ld (ix + offset), e */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_E);
	T_COUNT(19);
	break;
      case 0x74:	/* ld (ix + offset), h */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_H);
	T_COUNT(19);
	break;
      case 0x75:	/* ld (ix + offset), l */
	mem_write(*ixp + (signed char) mem_read(Z80_PC++), Z80_L);
	T_COUNT(19);
	break;

      case 0x22:	/* ld (address), ix */
	mem_write_word(mem_read_word(Z80_PC), *ixp);
	Z80_PC += 2;
	T_COUNT(20);
	break;

      case 0xF9:	/* ld sp, ix */
	Z80_SP = *ixp;
	T_COUNT(10);
	break;

      case 0x21:	/* ld ix, value */
	*ixp = mem_read_word(Z80_PC);
        Z80_PC += 2;
	T_COUNT(14);
	break;

      case 0x2A:	/* ld ix, (address) */
	*ixp = mem_read_word(mem_read_word(Z80_PC));
	Z80_PC += 2;
	T_COUNT(20);
	break;

      case 0xB6:	/* or (ix + offset) */
	do_or_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0xE1:	/* pop ix */
	*ixp = mem_read_word(Z80_SP);
	Z80_SP += 2;
	T_COUNT(14);
	break;

      case 0xE5:	/* push ix */
	Z80_SP -= 2;
	mem_write_word(Z80_SP, *ixp);
	T_COUNT(15);
	break;

      case 0x9E:	/* sbc a, (ix + offset) */
	do_sbc_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0x96:	/* sub a, (ix + offset) */
	do_sub_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0xAE:	/* xor (ix + offset) */
	do_xor_byte(mem_read(*ixp + (signed char) mem_read(Z80_PC++)));
	T_COUNT(19);
	break;

      case 0xCB:
        {
	  signed char offset = (signed char) mem_read(Z80_PC++);
	  signed char result = 0;
	  Uint8 sub_instruction = mem_read(Z80_PC++);

	  /* Instructions with (sub_instruction & 7) != 6 are undocumented;
	     their extra effect is handled after this switch */
	  switch(sub_instruction&0xf8)
	  {
	    case 0x00:	/* rlc (ix + offset) */
	      result = rlc_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x08:	/* rrc (ix + offset) */
	      result = rrc_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x10:	/* rl (ix + offset) */
	      result = rl_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x18:	/* rr (ix + offset) */
	      result = rr_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x20:	/* sla (ix + offset) */
	      result = sla_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x28:	/* sra (ix + offset) */
	      result = sra_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x30:	/* slia (ix + offset) [undocumented] */
	      result = slia_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x38:	/* srl (ix + offset) */
	      result = srl_byte(mem_read(*ixp + offset));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0x40:  /* bit 0, (ix + offset) */
	    case 0x48:  /* bit 1, (ix + offset) */
	    case 0x50:  /* bit 2, (ix + offset) */
	    case 0x58:  /* bit 3, (ix + offset) */
	    case 0x60:  /* bit 4, (ix + offset) */
	    case 0x68:  /* bit 5, (ix + offset) */
	    case 0x70:  /* bit 6, (ix + offset) */
	    case 0x78:  /* bit 7, (ix + offset) */
	      do_test_bit(sub_instruction, mem_read(*ixp + offset),
			  (sub_instruction >> 3) & 7);
	      T_COUNT(20);
	      break;

	    case 0x80:	/* res 0, (ix + offset) */
	    case 0x88:	/* res 1, (ix + offset) */
	    case 0x90:	/* res 2, (ix + offset) */
	    case 0x98:	/* res 3, (ix + offset) */
	    case 0xA0:	/* res 4, (ix + offset) */
	    case 0xA8:	/* res 5, (ix + offset) */
	    case 0xB0:	/* res 6, (ix + offset) */
	    case 0xB8:	/* res 7, (ix + offset) */
	      result = mem_read(*ixp + offset) &
		~(1 << ((sub_instruction >> 3) & 7));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;

	    case 0xC0:	/* set 0, (ix + offset) */
	    case 0xC8:	/* set 1, (ix + offset) */
	    case 0xD0:	/* set 2, (ix + offset) */
	    case 0xD8:	/* set 3, (ix + offset) */
	    case 0xE0:	/* set 4, (ix + offset) */
	    case 0xE8:	/* set 5, (ix + offset) */
	    case 0xF0:	/* set 6, (ix + offset) */
	    case 0xF8:	/* set 7, (ix + offset) */
	      result = mem_read(*ixp + offset) |
		(1 << ((sub_instruction >> 3) & 7));
	      mem_write(*ixp + offset, result);
	      T_COUNT(23);
	      break;
	  }

	  if (sub_instruction < 0x40 || sub_instruction > 0x7f)
          {
	    switch (sub_instruction & 7)
	    {
	      /* Undocumented cases */
	      case 0:  Z80_B = result; break;
	      case 1:  Z80_C = result; break;
	      case 2:  Z80_D = result; break;
	      case 3:  Z80_E = result; break;
	      case 4:  Z80_H = result; break;
	      case 5:  Z80_L = result; break;
	      case 7:  Z80_A = result; break;
	    }
	  }
        }
	break;

      /* begin undocumented instructions -- timings are a (good) guess */
      case 0x8C:	/* adc a, ixh */
	do_adc_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0x8D:	/* adc a, ixl */
	do_adc_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0x84:	/* add a, ixh */
	do_add_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0x85:	/* add a, ixl */
	do_add_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0xA4:	/* and ixh */
	do_and_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0xA5:	/* and ixl */
	do_and_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0xBC:	/* cp ixh */
	do_cp(HIGH(ixp));  T_COUNT(8);
	break;
      case 0xBD:	/* cp ixl */
	do_cp(LOW(ixp));  T_COUNT(8);
	break;
      case 0x25:	/* dec ixh */
	do_flags_dec_byte(--HIGH(ixp));  T_COUNT(8);
	break;
      case 0x2D:	/* dec ixl */
	do_flags_dec_byte(--LOW(ixp));  T_COUNT(8);
	break;
      case 0x24:	/* inc ixh */
	HIGH(ixp)++;
	do_flags_inc_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0x2C:	/* inc ixl */
	LOW(ixp)++;
	do_flags_inc_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0x7C:	/* ld a, ixh */
	Z80_A = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x7D:	/* ld a, ixl */
	Z80_A = LOW(ixp);  T_COUNT(8);
	break;
      case 0x44:	/* ld b, ixh */
	Z80_B = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x45:	/* ld b, ixl */
	Z80_B = LOW(ixp);  T_COUNT(8);
	break;
      case 0x4C:	/* ld c, ixh */
	Z80_C = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x4D:	/* ld c, ixl */
	Z80_C = LOW(ixp);  T_COUNT(8);
	break;
      case 0x54:	/* ld d, ixh */
	Z80_D = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x55:	/* ld d, ixl */
	Z80_D = LOW(ixp);  T_COUNT(8);
	break;
      case 0x5C:	/* ld e, ixh */
	Z80_E = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x5D:	/* ld e, ixl */
	Z80_E = LOW(ixp);  T_COUNT(8);
	break;
      case 0x67:	/* ld ixh, a */
	HIGH(ixp) = Z80_A;  T_COUNT(8);
	break;
      case 0x60:	/* ld ixh, b */
	HIGH(ixp) = Z80_B;  T_COUNT(8);
	break;
      case 0x61:	/* ld ixh, c */
	HIGH(ixp) = Z80_C;  T_COUNT(8);
	break;
      case 0x62:	/* ld ixh, d */
	HIGH(ixp) = Z80_D;  T_COUNT(8);
	break;
      case 0x63:	/* ld ixh, e */
	HIGH(ixp) = Z80_E;  T_COUNT(8);
	break;
      case 0x64:	/* ld ixh, ixh */
	HIGH(ixp) = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x65:	/* ld ixh, ixl */
	HIGH(ixp) = LOW(ixp);  T_COUNT(8);
	break;
      case 0x6F:	/* ld ixl, a */
	LOW(ixp) = Z80_A;  T_COUNT(8);
	break;
      case 0x68:	/* ld ixl, b */
	LOW(ixp) = Z80_B;  T_COUNT(8);
	break;
      case 0x69:	/* ld ixl, c */
	LOW(ixp) = Z80_C;  T_COUNT(8);
	break;
      case 0x6A:	/* ld ixl, d */
	LOW(ixp) = Z80_D;  T_COUNT(8);
	break;
      case 0x6B:	/* ld ixl, e */
	LOW(ixp) = Z80_E;  T_COUNT(8);
	break;
      case 0x6C:	/* ld ixl, ixh */
	LOW(ixp) = HIGH(ixp);  T_COUNT(8);
	break;
      case 0x6D:	/* ld ixl, ixl */
	LOW(ixp) = LOW(ixp);  T_COUNT(8);
	break;
      case 0x26:	/* ld ixh, value */
	HIGH(ixp) = mem_read(Z80_PC++);  T_COUNT(11);
	break;
      case 0x2E:	/* ld ixl, value */
	LOW(ixp) = mem_read(Z80_PC++);  T_COUNT(11);
	break;
      case 0xB4:	/* or ixh */
	do_or_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0xB5:	/* or ixl */
	do_or_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0x9C:	/* sbc a, ixh */
	do_sbc_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0x9D:	/* sbc a, ixl */
	do_sbc_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0x94:	/* sub a, ixh */
	do_sub_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0x95:	/* sub a, ixl */
	do_sub_byte(LOW(ixp));  T_COUNT(8);
	break;
      case 0xAC:	/* xor ixh */
	do_xor_byte(HIGH(ixp));  T_COUNT(8);
	break;
      case 0xAD:	/* xor ixl */
	do_xor_byte(LOW(ixp));  T_COUNT(8);
	break;
      /* end undocumented instructions */

      default:
	/* Ignore DD or FD prefix and retry as normal instruction;
	   this is a correct emulation. [undocumented, timing guessed] */
	Z80_R--;
	Z80_PC--;
	T_COUNT(4);
	break;
    }
}


/*
 * Extended instructions which have 0xED as the first byte:
 */
static int do_ED_instruction(void)
{
    Uint8 instruction = mem_read(Z80_PC++);
    int debug = 0;

    switch(instruction)
    {
      case 0x4A:	/* adc hl, bc */
	do_adc_word(Z80_BC);  T_COUNT(15);
	break;
      case 0x5A:	/* adc hl, de */
	do_adc_word(Z80_DE);  T_COUNT(15);
	break;
      case 0x6A:	/* adc hl, hl */
	do_adc_word(Z80_HL);  T_COUNT(15);
	break;
      case 0x7A:	/* adc hl, sp */
	do_adc_word(Z80_SP);  T_COUNT(15);
	break;

      case 0xA9:	/* cpd */
	do_cpd();
	break;
      case 0xB9:	/* cpdr */
	do_cpdr();
	break;

      case 0xA1:	/* cpi */
	do_cpi();
	break;
      case 0xB1:	/* cpir */
	do_cpir();
	break;

      case 0x46:	/* im 0 */
      case 0x66:	/* im 0 [undocumented]*/
	do_im0();  T_COUNT(8);
	break;
      case 0x56:	/* im 1 */
      case 0x76:	/* im 1 [undocumented] */
	do_im1();  T_COUNT(8);
	break;
      case 0x5E:	/* im 2 */
      case 0x7E:	/* im 2 [undocumented] */
	do_im2();  T_COUNT(8);
	break;

      case 0x78:	/* in a, (c) */
	Z80_A = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x40:	/* in b, (c) */
	Z80_B = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x48:	/* in c, (c) */
	Z80_C = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x50:	/* in d, (c) */
	Z80_D = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x58:	/* in e, (c) */
	Z80_E = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x60:	/* in h, (c) */
	Z80_H = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x68:	/* in l, (c) */
	Z80_L = in_with_flags(Z80_C);  T_COUNT(11);
	break;
      case 0x70:	/* in (c) [undocumented] */
	(void) in_with_flags(Z80_C);  T_COUNT(11);
	break;

      case 0xAA:	/* ind */
	do_ind();
	break;
      case 0xBA:	/* indr */
	do_indr();
	break;
      case 0xA2:	/* ini */
	do_ini();
	break;
      case 0xB2:	/* inir */
	do_inir();
	break;

      case 0x57:	/* ld a, i */
	do_ld_a_i();  T_COUNT(9);
	break;
      case 0x47:	/* ld i, a */
	Z80_I = Z80_A;  T_COUNT(9);
	break;

      case 0x5F:	/* ld a, r */
	do_ld_a_r();  T_COUNT(9);
	break;
      case 0x4F:	/* ld r, a */
	Z80_R = Z80_A;
	Z80_R7 = Z80_A & 0x80;
	T_COUNT(9);
	break;

      case 0x4B:	/* ld bc, (address) */
	Z80_BC = mem_read_word(mem_read_word(Z80_PC));
	Z80_PC += 2;
	T_COUNT(20);
	break;
      case 0x5B:	/* ld de, (address) */
	Z80_DE = mem_read_word(mem_read_word(Z80_PC));
	Z80_PC += 2;
	T_COUNT(20);
	break;
      case 0x6B:	/* ld hl, (address) */
	/* this instruction is redundant with the 2A instruction */
	Z80_HL = mem_read_word(mem_read_word(Z80_PC));
	Z80_PC += 2;
	T_COUNT(20);
	break;
      case 0x7B:	/* ld sp, (address) */
	Z80_SP = mem_read_word(mem_read_word(Z80_PC));
	Z80_PC += 2;
	T_COUNT(20);
	break;

      case 0x43:	/* ld (address), bc */
	mem_write_word(mem_read_word(Z80_PC), Z80_BC);
	Z80_PC += 2;
	T_COUNT(20);
	break;
      case 0x53:	/* ld (address), de */
	mem_write_word(mem_read_word(Z80_PC), Z80_DE);
	Z80_PC += 2;
	T_COUNT(20);
	break;
      case 0x63:	/* ld (address), hl */
	/* this instruction is redundant with the 22 instruction */
	mem_write_word(mem_read_word(Z80_PC), Z80_HL);
	Z80_PC += 2;
	T_COUNT(20);
	break;
      case 0x73:	/* ld (address), sp */
	mem_write_word(mem_read_word(Z80_PC), Z80_SP);
	Z80_PC += 2;
	T_COUNT(20);
	break;

      case 0xA8:	/* ldd */
	do_ldd();
	break;
      case 0xB8:	/* lddr */
	do_lddr();
	break;
      case 0xA0:	/* ldi */
	do_ldi();
	break;
      case 0xB0:	/* ldir */
	do_ldir();
	break;

      case 0x44:	/* neg */
      case 0x4C:	/* neg [undocumented] */
      case 0x54:	/* neg [undocumented] */
      case 0x5C:	/* neg [undocumented] */
      case 0x64:	/* neg [undocumented] */
      case 0x6C:	/* neg [undocumented] */
      case 0x74:	/* neg [undocumented] */
      case 0x7C:	/* neg [undocumented] */
	do_negate();
	T_COUNT(8);
	break;

      case 0x79:	/* out (c), a */
	z80_out(Z80_C, Z80_A);
	T_COUNT(12);
	break;
      case 0x41:	/* out (c), b */
	z80_out(Z80_C, Z80_B);
	T_COUNT(12);
	break;
      case 0x49:	/* out (c), c */
	z80_out(Z80_C, Z80_C);
	T_COUNT(12);
	break;
      case 0x51:	/* out (c), d */
	z80_out(Z80_C, Z80_D);
	T_COUNT(12);
	break;
      case 0x59:	/* out (c), e */
	z80_out(Z80_C, Z80_E);
	T_COUNT(12);
	break;
      case 0x61:	/* out (c), h */
	z80_out(Z80_C, Z80_H);
	T_COUNT(12);
	break;
      case 0x69:	/* out (c), l */
	z80_out(Z80_C, Z80_L);
	T_COUNT(12);
	break;
      case 0x71:	/* out (c), 0 [undocumented] */
        /* Note: on a CMOS part this outputs 0xFF */
	z80_out(Z80_C, 0);
	T_COUNT(12);
	break;

      case 0xAB:	/* outd */
	do_outd();
	break;
      case 0xBB:	/* outdr */
	do_outdr();
	break;
      case 0xA3:	/* outi */
	do_outi();
	break;
      case 0xB3:	/* outir */
	do_outir();
	break;

      case 0x4D:	/* reti */
	/* no support for alerting peripherals, just like ret */
	Z80_PC = mem_read_word(Z80_SP);
	Z80_SP += 2;
	/* Yes RETI does this, it's not mentioned in the documentation but
	   it happens on real silicon */
	z80_state.iff1 = z80_state.iff2;  /* restore the iff state */
	T_COUNT(14);
	break;

      case 0x45:	/* retn */
      case 0x55:	/* retn [undocumented] */
      case 0x5D:	/* retn [undocumented] */
      case 0x65:	/* retn [undocumented] */
      case 0x6D:	/* retn [undocumented] */
      case 0x75:	/* retn [undocumented] */
      case 0x7D:	/* retn [undocumented] */
	Z80_PC = mem_read_word(Z80_SP);
	Z80_SP += 2;
	z80_state.iff1 = z80_state.iff2;  /* restore the iff state */
	T_COUNT(14);
	break;

      case 0x6F:	/* rld */
	do_rld();
	T_COUNT(18);
	break;

      case 0x67:	/* rrd */
	do_rrd();
	T_COUNT(18);
	break;

      case 0x42:	/* sbc hl, bc */
	do_sbc_word(Z80_BC);
	T_COUNT(15);
	break;
      case 0x52:	/* sbc hl, de */
	do_sbc_word(Z80_DE);
	T_COUNT(15);
	break;
      case 0x62:	/* sbc hl, hl */
	do_sbc_word(Z80_HL);
	T_COUNT(15);
	break;
      case 0x72:	/* sbc hl, sp */
	do_sbc_word(Z80_SP);
	T_COUNT(15);
	break;

      /* Emulator traps -- not real Z80 instructions */
      case 0x28:        /* emt_system */
	do_emt_system();
	break;
      case 0x29:        /* emt_mouse */
	do_emt_mouse();
	break;
      case 0x2a:        /* emt_getddir */
	do_emt_getddir();
	break;
      case 0x2b:        /* emt_setddir */
	do_emt_setddir();
	break;
      case 0x2e:        /* SSPD A (David Keil) */
	timer_overclock = ((Z80_A & (1 << 2)) != 0);
	trs_timer_mode(timer_overclock);
	break;
      case 0x2f:        /* emt_debug */
	if (trs_continuous > 0) trs_continuous = 0;
	debug = 1;
	break;
      case 0x30:        /* emt_open */
	do_emt_open();
	break;
      case 0x31:	/* emt_close */
	do_emt_close();
	break;
      case 0x32:	/* emt_read */
	do_emt_read();
	break;
      case 0x33:	/* emt_write */
	do_emt_write();
	break;
      case 0x34:	/* emt_lseek */
	do_emt_lseek();
	break;
      case 0x35:	/* emt_strerror */
	do_emt_strerror();
	break;
      case 0x36:	/* emt_time */
	do_emt_time();
	break;
      case 0x37:        /* emt_opendir */
	do_emt_opendir();
	break;
      case 0x38:	/* emt_closedir */
	do_emt_closedir();
	break;
      case 0x39:	/* emt_readdir */
	do_emt_readdir();
	break;
      case 0x3a:	/* emt_chdir */
	do_emt_chdir();
	break;
      case 0x3b:	/* emt_getcwd */
	do_emt_getcwd();
	break;
      case 0x3c:	/* emt_misc */
	do_emt_misc();
	break;
      case 0x3d:	/* emt_ftruncate */
	do_emt_ftruncate();
	break;
      case 0x3e:        /* emt_opendisk */
	do_emt_opendisk();
	break;
      case 0x3f:	/* emt_closedisk */
	do_emt_closedisk();
	break;
      case 0xf6:	/* Exit in David Keil's TRS-80 emulator */
	trs_exit(0);
	break;

      default:
	/* undocumented no-op */
	T_COUNT(4);
#ifdef ZBX
	disassemble(Z80_PC - 2);
#endif
	error("unsupported ED instruction: 0x%x", instruction);
    }

    return debug;
}

int trs_continuous;

int z80_run(int continuous)
     /*
      * -1 = single-step and disallow interrupts
      *  0 = single-step
      *  1 = continuous
      */
{
    Uint8 instruction;
    Uint16 address; /* generic temps */
    int ret = 0;
    tstate_t t_delta;
    trs_continuous = continuous;

    /* loop to do a z80 instruction */
    do {
    /* Speed control */
	if (z80_state.t_count > last_t_count)
	  t_delta = z80_state.t_count - last_t_count;
	else
	  t_delta = last_t_count - z80_state.t_count;

	if (t_delta >= cycles_per_timer) {
	  trs_get_event(0);
	  if (trs_paused) {
	    while (trs_paused)
	      trs_get_event(1);
	  }
	  trs_timer_sync_with_host();
	  last_t_count = z80_state.t_count;
	}

	Z80_R++;
	instruction = mem_read(Z80_PC++);

	switch(instruction)
	{
	  case 0xCB:	/* CB.. extended instruction */
	    Z80_R++;
	    do_CB_instruction();
	    break;
	  case 0xDD:	/* DD.. extended instruction */
	    Z80_R++;
	    do_indexed_instruction(&Z80_IX);
	    break;
	  case 0xED:	/* ED.. extended instruction */
	    Z80_R++;
	    ret = do_ED_instruction();
	    break;
	  case 0xFD:	/* FD.. extended instruction */
	    Z80_R++;
	    do_indexed_instruction(&Z80_IY);
	    break;

	  case 0x8F:	/* adc a, a */
	    do_adc_byte(Z80_A);	 T_COUNT(4);
	    break;
	  case 0x88:	/* adc a, b */
	    do_adc_byte(Z80_B);	 T_COUNT(4);
	    break;
	  case 0x89:	/* adc a, c */
	    do_adc_byte(Z80_C);	 T_COUNT(4);
	    break;
	  case 0x8A:	/* adc a, d */
	    do_adc_byte(Z80_D);	 T_COUNT(4);
	    break;
	  case 0x8B:	/* adc a, e */
	    do_adc_byte(Z80_E);	 T_COUNT(4);
	    break;
	  case 0x8C:	/* adc a, h */
	    do_adc_byte(Z80_H);	 T_COUNT(4);
	    break;
	  case 0x8D:	/* adc a, l */
	    do_adc_byte(Z80_L);	 T_COUNT(4);
	    break;
	  case 0xCE:	/* adc a, value */
	    do_adc_byte(mem_read(Z80_PC++));  T_COUNT(7);
	    break;
	  case 0x8E:	/* adc a, (hl) */
	    do_adc_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0x87:	/* add a, a */
	    do_add_byte(Z80_A);	 T_COUNT(4);
	    break;
	  case 0x80:	/* add a, b */
	    do_add_byte(Z80_B);	 T_COUNT(4);
	    break;
	  case 0x81:	/* add a, c */
	    do_add_byte(Z80_C);	 T_COUNT(4);
	    break;
	  case 0x82:	/* add a, d */
	    do_add_byte(Z80_D);	 T_COUNT(4);
	    break;
	  case 0x83:	/* add a, e */
	    do_add_byte(Z80_E);	 T_COUNT(4);
	    break;
	  case 0x84:	/* add a, h */
	    do_add_byte(Z80_H);	 T_COUNT(4);
	    break;
	  case 0x85:	/* add a, l */
	    do_add_byte(Z80_L);	 T_COUNT(4);
	    break;
	  case 0xC6:	/* add a, value */
	    do_add_byte(mem_read(Z80_PC++));  T_COUNT(7);
	    break;
	  case 0x86:	/* add a, (hl) */
	    do_add_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0x09:	/* add hl, bc */
	    do_add_word(Z80_BC);  T_COUNT(11);
	    break;
	  case 0x19:	/* add hl, de */
	    do_add_word(Z80_DE);  T_COUNT(11);
	    break;
	  case 0x29:	/* add hl, hl */
	    do_add_word(Z80_HL);  T_COUNT(11);
	    break;
	  case 0x39:	/* add hl, sp */
	    do_add_word(Z80_SP);  T_COUNT(11);
	    break;

	  case 0xA7:	/* and a */
	    do_and_byte(Z80_A);	 T_COUNT(4);
	    break;
	  case 0xA0:	/* and b */
	    do_and_byte(Z80_B);	 T_COUNT(4);
	    break;
	  case 0xA1:	/* and c */
	    do_and_byte(Z80_C);	 T_COUNT(4);
	    break;
	  case 0xA2:	/* and d */
	    do_and_byte(Z80_D);	 T_COUNT(4);
	    break;
	  case 0xA3:	/* and e */
	    do_and_byte(Z80_E);	 T_COUNT(4);
	    break;
	  case 0xA4:	/* and h */
	    do_and_byte(Z80_H);	 T_COUNT(4);
	    break;
	  case 0xA5:	/* and l */
	    do_and_byte(Z80_L);  T_COUNT(4);
	    break;
	  case 0xE6:	/* and value */
	    do_and_byte(mem_read(Z80_PC++));  T_COUNT(7);
	    break;
	  case 0xA6:	/* and (hl) */
	    do_and_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0xCD:	/* call address */
	    address = mem_read_word(Z80_PC);
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC + 2);
	    Z80_PC = address;
	    T_COUNT(17);
	    break;

	  case 0xC4:	/* call nz, address */
	    if(!ZERO_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xCC:	/* call z, address */
	    if(ZERO_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xD4:	/* call nc, address */
	    if(!CARRY_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xDC:	/* call c, address */
	    if(CARRY_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xE4:	/* call po, address */
	    if(!PARITY_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xEC:	/* call pe, address */
	    if(PARITY_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xF4:	/* call p, address */
	    if(!SIGN_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;
	  case 0xFC:	/* call m, address */
	    if(SIGN_FLAG)
	    {
		address = mem_read_word(Z80_PC);
		Z80_SP -= 2;
		mem_write_word(Z80_SP, Z80_PC + 2);
		Z80_PC = address;
		T_COUNT(17);
	    }
	    else
	    {
		Z80_PC += 2;
		T_COUNT(10);
	    }
	    break;


	  case 0x3F:	/* ccf */
	    Z80_F = (Z80_F & (ZERO_MASK|PARITY_MASK|SIGN_MASK))
	      | (~Z80_F & CARRY_MASK)
	      | ((Z80_F & CARRY_MASK) ? HALF_CARRY_MASK : 0)
	      | (Z80_A & (UNDOC3_MASK|UNDOC5_MASK));
	    T_COUNT(4);
	    break;

	  case 0xBF:	/* cp a */
	    do_cp(Z80_A);  T_COUNT(4);
	    break;
	  case 0xB8:	/* cp b */
	    do_cp(Z80_B);  T_COUNT(4);
	    break;
	  case 0xB9:	/* cp c */
	    do_cp(Z80_C);  T_COUNT(4);
	    break;
	  case 0xBA:	/* cp d */
	    do_cp(Z80_D);  T_COUNT(4);
	    break;
	  case 0xBB:	/* cp e */
	    do_cp(Z80_E);  T_COUNT(4);
	    break;
	  case 0xBC:	/* cp h */
	    do_cp(Z80_H);  T_COUNT(4);
	    break;
	  case 0xBD:	/* cp l */
	    do_cp(Z80_L);  T_COUNT(4);
	    break;
	  case 0xFE:	/* cp value */
	    do_cp(mem_read(Z80_PC++));  T_COUNT(7);
	    break;
	  case 0xBE:	/* cp (hl) */
	    do_cp(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0x2F:	/* cpl */
	    Z80_A = ~Z80_A;
	    Z80_F = (Z80_F & (CARRY_MASK|PARITY_MASK|ZERO_MASK|SIGN_MASK))
	      | (HALF_CARRY_MASK|SUBTRACT_MASK)
	      | (Z80_A & (UNDOC3_MASK|UNDOC5_MASK));
	    T_COUNT(4);
	    break;

	  case 0x27:	/* daa */
	    do_daa();
	    T_COUNT(4);
	    break;

	  case 0x3D:	/* dec a */
	    do_flags_dec_byte(--Z80_A);  T_COUNT(4);
	    break;
	  case 0x05:	/* dec b */
	    do_flags_dec_byte(--Z80_B);  T_COUNT(4);
	    break;
	  case 0x0D:	/* dec c */
	    do_flags_dec_byte(--Z80_C);  T_COUNT(4);
	    break;
	  case 0x15:	/* dec d */
	    do_flags_dec_byte(--Z80_D);  T_COUNT(4);
	    break;
	  case 0x1D:	/* dec e */
	    do_flags_dec_byte(--Z80_E);  T_COUNT(4);
	    break;
	  case 0x25:	/* dec h */
	    do_flags_dec_byte(--Z80_H);  T_COUNT(4);
	    break;
	  case 0x2D:	/* dec l */
	    do_flags_dec_byte(--Z80_L);  T_COUNT(4);
	    break;

	  case 0x35:	/* dec (hl) */
	    {
	      Uint8 value = mem_read(Z80_HL) - 1;
	      mem_write(Z80_HL, value);
	      do_flags_dec_byte(value);
	    }
	    T_COUNT(11);
	    break;

	  case 0x0B:	/* dec bc */
	    Z80_BC--;
	    T_COUNT(6);
	    break;
	  case 0x1B:	/* dec de */
	    Z80_DE--;
	    T_COUNT(6);
	    break;
	  case 0x2B:	/* dec hl */
	    Z80_HL--;
	    T_COUNT(6);
	    break;
	  case 0x3B:	/* dec sp */
	    Z80_SP--;
	    T_COUNT(6);
	    break;

	  case 0xF3:	/* di */
	    do_di();
	    T_COUNT(4);
	    break;

	  case 0x10:	/* djnz offset */
	    /* Zaks says no flag changes. */
	    if(--Z80_B != 0)
	    {
		Z80_PC += (signed char) mem_read(Z80_PC) + 1;
		T_COUNT(13);
	    }
	    else
	    {
		Z80_PC++;
		T_COUNT(8);
	    }
	    break;

	  case 0xFB:	/* ei */
	    do_ei();
	    T_COUNT(4);
	    break;

	  case 0x08:	/* ex af, af' */
	  {
	      Uint16 temp = Z80_AF;
	      Z80_AF = Z80_AF_PRIME;
	      Z80_AF_PRIME = temp;
	  }
	    T_COUNT(4);
	    break;

	  case 0xEB:	/* ex de, hl */
	  {
	      Uint16 temp = Z80_DE;
	      Z80_DE = Z80_HL;
	      Z80_HL = temp;
	  }
	    T_COUNT(4);
	    break;

	  case 0xE3:	/* ex (sp), hl */
	  {
	      Uint16 temp = mem_read_word(Z80_SP);
	      mem_write_word(Z80_SP, Z80_HL);
	      Z80_HL = temp;
	  }
	    T_COUNT(19);
	    break;

	  case 0xD9:	/* exx */
	  {
	      Uint16 tmp = Z80_BC_PRIME;
	      Z80_BC_PRIME = Z80_BC;
	      Z80_BC = tmp;
	      tmp = Z80_DE_PRIME;
	      Z80_DE_PRIME = Z80_DE;
	      Z80_DE = tmp;
	      tmp = Z80_HL_PRIME;
	      Z80_HL_PRIME = Z80_HL;
	      Z80_HL = tmp;
	  }
	    T_COUNT(4);
	    break;

	  case 0x76:	/* halt */
	    if (trs_model == 1) {
		/* Z80 HALT output is tied to reset button circuit */
		trs_reset(0);
	    } else {
		/* Really halt (i.e., wait for interrupt) */
	        /* Slight kludge: we back up the PC and keep going
		   around the main loop reexecuting the halt.  A real
		   Z80 does not back up and re-fetch the halt
		   instruction repeatedly; it just executes NOPs
		   internally.  When an interrupt or NMI is delivered,
		   (see below) we undo this decrement to get out of
		   the halt state. */
	        Z80_PC--;
#if 0 /* Removed for new sdltrs timing scheme */
		if (continuous > 0 &&
		    !(z80_state.nmi && !z80_state.nmi_seen) &&
		    !(z80_state.irq && z80_state.iff1) &&
		    !trs_event_scheduled()) {
		    pause();
		}
#endif
	    }
	    T_COUNT(4);
	    break;

	  case 0xDB:	/* in a, (port) */
	    Z80_A = z80_in(mem_read(Z80_PC++));
	    T_COUNT(10);
	    break;

	  case 0x3C:	/* inc a */
	    Z80_A++;
	    do_flags_inc_byte(Z80_A);  T_COUNT(4);
	    break;
	  case 0x04:	/* inc b */
	    Z80_B++;
	    do_flags_inc_byte(Z80_B);  T_COUNT(4);
	    break;
	  case 0x0C:	/* inc c */
	    Z80_C++;
	    do_flags_inc_byte(Z80_C);  T_COUNT(4);
	    break;
	  case 0x14:	/* inc d */
	    Z80_D++;
	    do_flags_inc_byte(Z80_D);  T_COUNT(4);
	    break;
	  case 0x1C:	/* inc e */
	    Z80_E++;
	    do_flags_inc_byte(Z80_E);  T_COUNT(4);
	    break;
	  case 0x24:	/* inc h */
	    Z80_H++;
	    do_flags_inc_byte(Z80_H);  T_COUNT(4);
	    break;
	  case 0x2C:	/* inc l */
	    Z80_L++;
	    do_flags_inc_byte(Z80_L);  T_COUNT(4);
	    break;

	  case 0x34:	/* inc (hl) */
	  {
	      Uint8 value = mem_read(Z80_HL) + 1;
	      mem_write(Z80_HL, value);
	      do_flags_inc_byte(value);
	  }
	    T_COUNT(11);
	    break;

	  case 0x03:	/* inc bc */
	    Z80_BC++;
	    T_COUNT(6);
	    break;
	  case 0x13:	/* inc de */
	    Z80_DE++;
	    T_COUNT(6);
	    break;
	  case 0x23:	/* inc hl */
	    Z80_HL++;
	    T_COUNT(6);
	    break;
	  case 0x33:	/* inc sp */
	    Z80_SP++;
	    T_COUNT(6);
	    break;

	  case 0xC3:	/* jp address */
	    Z80_PC = mem_read_word(Z80_PC);
	    T_COUNT(10);
	    break;

	  case 0xE9:	/* jp (hl) */
	    Z80_PC = Z80_HL;
	    T_COUNT(4);
	    break;

	  case 0xC2:	/* jp nz, address */
	    if(!ZERO_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xCA:	/* jp z, address */
	    if(ZERO_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xD2:	/* jp nc, address */
	    if(!CARRY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xDA:	/* jp c, address */
	    if(CARRY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xE2:	/* jp po, address */
	    if(!PARITY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xEA:	/* jp pe, address */
	    if(PARITY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xF2:	/* jp p, address */
	    if(!SIGN_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;
	  case 0xFA:	/* jp m, address */
	    if(SIGN_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_PC);
	    }
	    else
	    {
		Z80_PC += 2;
	    }
	    T_COUNT(10);
	    break;

	  case 0x18:	/* jr offset */
	    Z80_PC += (signed char) mem_read(Z80_PC) + 1;
	    T_COUNT(12);
	    break;

	  case 0x20:	/* jr nz, offset */
	    if(!ZERO_FLAG)
	    {
		Z80_PC += (signed char) mem_read(Z80_PC) + 1;
		T_COUNT(12);
	    }
	    else
	    {
		Z80_PC++;
		T_COUNT(7);
	    }
	    break;
	  case 0x28:	/* jr z, offset */
	    if(ZERO_FLAG)
	    {
		Z80_PC += (signed char) mem_read(Z80_PC) + 1;
		T_COUNT(12);
	    }
	    else
	    {
		Z80_PC++;
		T_COUNT(7);
	    }
	    break;
	  case 0x30:	/* jr nc, offset */
	    if(!CARRY_FLAG)
	    {
		Z80_PC += (signed char) mem_read(Z80_PC) + 1;
		T_COUNT(12);
	    }
	    else
	    {
		Z80_PC++;
		T_COUNT(7);
	    }
	    break;
	  case 0x38:	/* jr c, offset */
	    if(CARRY_FLAG)
	    {
		Z80_PC += (signed char) mem_read(Z80_PC) + 1;
		T_COUNT(12);
	    }
	    else
	    {
		Z80_PC++;
		T_COUNT(7);
	    }
	    break;

	  case 0x7F:	/* ld a, a */
	    Z80_A = Z80_A;  T_COUNT(4);
	    break;
	  case 0x78:	/* ld a, b */
	    Z80_A = Z80_B;  T_COUNT(4);
	    break;
	  case 0x79:	/* ld a, c */
	    Z80_A = Z80_C;  T_COUNT(4);
	    break;
	  case 0x7A:	/* ld a, d */
	    Z80_A = Z80_D;  T_COUNT(4);
	    break;
	  case 0x7B:	/* ld a, e */
	    Z80_A = Z80_E;  T_COUNT(4);
	    break;
	  case 0x7C:	/* ld a, h */
	    Z80_A = Z80_H;  T_COUNT(4);
	    break;
	  case 0x7D:	/* ld a, l */
	    Z80_A = Z80_L;  T_COUNT(4);
	    break;
	  case 0x47:	/* ld b, a */
	    Z80_B = Z80_A;  T_COUNT(4);
	    break;
	  case 0x40:	/* ld b, b */
	    Z80_B = Z80_B;  T_COUNT(4);
	    break;
	  case 0x41:	/* ld b, c */
	    Z80_B = Z80_C;  T_COUNT(4);
	    break;
	  case 0x42:	/* ld b, d */
	    Z80_B = Z80_D;  T_COUNT(4);
	    break;
	  case 0x43:	/* ld b, e */
	    Z80_B = Z80_E;  T_COUNT(4);
	    break;
	  case 0x44:	/* ld b, h */
	    Z80_B = Z80_H;  T_COUNT(4);
	    break;
	  case 0x45:	/* ld b, l */
	    Z80_B = Z80_L;  T_COUNT(4);
	    break;
	  case 0x4F:	/* ld c, a */
	    Z80_C = Z80_A;  T_COUNT(4);
	    break;
	  case 0x48:	/* ld c, b */
	    Z80_C = Z80_B;  T_COUNT(4);
	    break;
	  case 0x49:	/* ld c, c */
	    Z80_C = Z80_C;  T_COUNT(4);
	    break;
	  case 0x4A:	/* ld c, d */
	    Z80_C = Z80_D;  T_COUNT(4);
	    break;
	  case 0x4B:	/* ld c, e */
	    Z80_C = Z80_E;  T_COUNT(4);
	    break;
	  case 0x4C:	/* ld c, h */
	    Z80_C = Z80_H;  T_COUNT(4);
	    break;
	  case 0x4D:	/* ld c, l */
	    Z80_C = Z80_L;  T_COUNT(4);
	    break;
	  case 0x57:	/* ld d, a */
	    Z80_D = Z80_A;  T_COUNT(4);
	    break;
	  case 0x50:	/* ld d, b */
	    Z80_D = Z80_B;  T_COUNT(4);
	    break;
	  case 0x51:	/* ld d, c */
	    Z80_D = Z80_C;  T_COUNT(4);
	    break;
	  case 0x52:	/* ld d, d */
	    Z80_D = Z80_D;  T_COUNT(4);
	    break;
	  case 0x53:	/* ld d, e */
	    Z80_D = Z80_E;  T_COUNT(4);
	    break;
	  case 0x54:	/* ld d, h */
	    Z80_D = Z80_H;  T_COUNT(4);
	    break;
	  case 0x55:	/* ld d, l */
	    Z80_D = Z80_L;  T_COUNT(4);
	    break;
	  case 0x5F:	/* ld e, a */
	    Z80_E = Z80_A;  T_COUNT(4);
	    break;
	  case 0x58:	/* ld e, b */
	    Z80_E = Z80_B;  T_COUNT(4);
	    break;
	  case 0x59:	/* ld e, c */
	    Z80_E = Z80_C;  T_COUNT(4);
	    break;
	  case 0x5A:	/* ld e, d */
	    Z80_E = Z80_D;  T_COUNT(4);
	    break;
	  case 0x5B:	/* ld e, e */
	    Z80_E = Z80_E;  T_COUNT(4);
	    break;
	  case 0x5C:	/* ld e, h */
	    Z80_E = Z80_H;  T_COUNT(4);
	    break;
	  case 0x5D:	/* ld e, l */
	    Z80_E = Z80_L;  T_COUNT(4);
	    break;
	  case 0x67:	/* ld h, a */
	    Z80_H = Z80_A;  T_COUNT(4);
	    break;
	  case 0x60:	/* ld h, b */
	    Z80_H = Z80_B;  T_COUNT(4);
	    break;
	  case 0x61:	/* ld h, c */
	    Z80_H = Z80_C;  T_COUNT(4);
	    break;
	  case 0x62:	/* ld h, d */
	    Z80_H = Z80_D;  T_COUNT(4);
	    break;
	  case 0x63:	/* ld h, e */
	    Z80_H = Z80_E;  T_COUNT(4);
	    break;
	  case 0x64:	/* ld h, h */
	    Z80_H = Z80_H;  T_COUNT(4);
	    break;
	  case 0x65:	/* ld h, l */
	    Z80_H = Z80_L;  T_COUNT(4);
	    break;
	  case 0x6F:	/* ld l, a */
	    Z80_L = Z80_A;  T_COUNT(4);
	    break;
	  case 0x68:	/* ld l, b */
	    Z80_L = Z80_B;  T_COUNT(4);
	    break;
	  case 0x69:	/* ld l, c */
	    Z80_L = Z80_C;  T_COUNT(4);
	    break;
	  case 0x6A:	/* ld l, d */
	    Z80_L = Z80_D;  T_COUNT(4);
	    break;
	  case 0x6B:	/* ld l, e */
	    Z80_L = Z80_E;  T_COUNT(4);
	    break;
	  case 0x6C:	/* ld l, h */
	    Z80_L = Z80_H;  T_COUNT(4);
	    break;
	  case 0x6D:	/* ld l, l */
	    Z80_L = Z80_L;  T_COUNT(4);
	    break;

	  case 0x02:	/* ld (bc), a */
	    mem_write(Z80_BC, Z80_A);  T_COUNT(7);
	    break;
	  case 0x12:	/* ld (de), a */
	    mem_write(Z80_DE, Z80_A);  T_COUNT(7);
	    break;
	  case 0x77:	/* ld (hl), a */
	    mem_write(Z80_HL, Z80_A);  T_COUNT(7);
	    break;
	  case 0x70:	/* ld (hl), b */
	    mem_write(Z80_HL, Z80_B);  T_COUNT(7);
	    break;
	  case 0x71:	/* ld (hl), c */
	    mem_write(Z80_HL, Z80_C);  T_COUNT(7);
	    break;
	  case 0x72:	/* ld (hl), d */
	    mem_write(Z80_HL, Z80_D);  T_COUNT(7);
	    break;
	  case 0x73:	/* ld (hl), e */
	    mem_write(Z80_HL, Z80_E);  T_COUNT(7);
	    break;
	  case 0x74:	/* ld (hl), h */
	    mem_write(Z80_HL, Z80_H);  T_COUNT(7);
	    break;
	  case 0x75:	/* ld (hl), l */
	    mem_write(Z80_HL, Z80_L);  T_COUNT(7);
	    break;

	  case 0x7E:	/* ld a, (hl) */
	    Z80_A = mem_read(Z80_HL);  T_COUNT(7);
	    break;
	  case 0x46:	/* ld b, (hl) */
	    Z80_B = mem_read(Z80_HL);  T_COUNT(7);
	    break;
	  case 0x4E:	/* ld c, (hl) */
	    Z80_C = mem_read(Z80_HL);  T_COUNT(7);
	    break;
	  case 0x56:	/* ld d, (hl) */
	    Z80_D = mem_read(Z80_HL);  T_COUNT(7);
	    break;
	  case 0x5E:	/* ld e, (hl) */
	    Z80_E = mem_read(Z80_HL);  T_COUNT(7);
	    break;
	  case 0x66:	/* ld h, (hl) */
	    Z80_H = mem_read(Z80_HL);  T_COUNT(7);
	    break;
	  case 0x6E:	/* ld l, (hl) */
	    Z80_L = mem_read(Z80_HL);  T_COUNT(7);
	    break;

	  case 0x3E:	/* ld a, value */
	    Z80_A = mem_read(Z80_PC++);  T_COUNT(7);
	    break;
	  case 0x06:	/* ld b, value */
	    Z80_B = mem_read(Z80_PC++);  T_COUNT(7);
	    break;
	  case 0x0E:	/* ld c, value */
	    Z80_C = mem_read(Z80_PC++);  T_COUNT(7);
	    break;
	  case 0x16:	/* ld d, value */
	    Z80_D = mem_read(Z80_PC++);  T_COUNT(7);
	    break;
	  case 0x1E:	/* ld e, value */
	    Z80_E = mem_read(Z80_PC++);  T_COUNT(7);
	    break;
	  case 0x26:	/* ld h, value */
	    Z80_H = mem_read(Z80_PC++);  T_COUNT(7);
	    break;
	  case 0x2E:	/* ld l, value */
	    Z80_L = mem_read(Z80_PC++);  T_COUNT(7);
	    break;

	  case 0x01:	/* ld bc, value */
	    Z80_BC = mem_read_word(Z80_PC);
	    Z80_PC += 2;
	    T_COUNT(10);
	    break;
	  case 0x11:	/* ld de, value */
	    Z80_DE = mem_read_word(Z80_PC);
	    Z80_PC += 2;
	    T_COUNT(10);
	    break;
	  case 0x21:	/* ld hl, value */
	    Z80_HL = mem_read_word(Z80_PC);
	    Z80_PC += 2;
	    T_COUNT(10);
	    break;
	  case 0x31:	/* ld sp, value */
	    Z80_SP = mem_read_word(Z80_PC);
	    Z80_PC += 2;
	    T_COUNT(10);
	    break;


	  case 0x3A:	/* ld a, (address) */
	    /* this one is missing from Zaks */
	    Z80_A = mem_read(mem_read_word(Z80_PC));
	    Z80_PC += 2;
	    T_COUNT(13);
	    break;

	  case 0x0A:	/* ld a, (bc) */
	    Z80_A = mem_read(Z80_BC);
	    T_COUNT(7);
	    break;
	  case 0x1A:	/* ld a, (de) */
	    Z80_A = mem_read(Z80_DE);
	    T_COUNT(7);
	    break;

	  case 0x32:	/* ld (address), a */
	    mem_write(mem_read_word(Z80_PC), Z80_A);
	    Z80_PC += 2;
	    T_COUNT(13);
	    break;

	  case 0x22:	/* ld (address), hl */
	    mem_write_word(mem_read_word(Z80_PC), Z80_HL);
	    Z80_PC += 2;
	    T_COUNT(16);
	    break;

	  case 0x36:	/* ld (hl), value */
	    mem_write(Z80_HL, mem_read(Z80_PC++));
	    T_COUNT(10);
	    break;

	  case 0x2A:	/* ld hl, (address) */
	    Z80_HL = mem_read_word(mem_read_word(Z80_PC));
	    Z80_PC += 2;
	    T_COUNT(16);
	    break;

	  case 0xF9:	/* ld sp, hl */
	    Z80_SP = Z80_HL;
	    T_COUNT(6);
	    break;

	  case 0x00:	/* nop */
	    T_COUNT(4);
	    break;

	  case 0xF6:	/* or value */
	    do_or_byte(mem_read(Z80_PC++));
	    T_COUNT(7);
	    break;

	  case 0xB7:	/* or a */
	    do_or_byte(Z80_A);  T_COUNT(4);
	    break;
	  case 0xB0:	/* or b */
	    do_or_byte(Z80_B);  T_COUNT(4);
	    break;
	  case 0xB1:	/* or c */
	    do_or_byte(Z80_C);  T_COUNT(4);
	    break;
	  case 0xB2:	/* or d */
	    do_or_byte(Z80_D);  T_COUNT(4);
	    break;
	  case 0xB3:	/* or e */
	    do_or_byte(Z80_E);  T_COUNT(4);
	    break;
	  case 0xB4:	/* or h */
	    do_or_byte(Z80_H);  T_COUNT(4);
	    break;
	  case 0xB5:	/* or l */
	    do_or_byte(Z80_L);  T_COUNT(4);
	    break;

	  case 0xB6:	/* or (hl) */
	    do_or_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0xD3:	/* out (port), a */
	    z80_out(mem_read(Z80_PC++), Z80_A);
	    T_COUNT(11);
	    break;

	  case 0xC1:	/* pop bc */
	    Z80_BC = mem_read_word(Z80_SP);
	    Z80_SP += 2;
	    T_COUNT(10);
	    break;
	  case 0xD1:	/* pop de */
	    Z80_DE = mem_read_word(Z80_SP);
	    Z80_SP += 2;
	    T_COUNT(10);
	    break;
	  case 0xE1:	/* pop hl */
	    Z80_HL = mem_read_word(Z80_SP);
	    Z80_SP += 2;
	    T_COUNT(10);
	    break;
	  case 0xF1:	/* pop af */
	    Z80_AF = mem_read_word(Z80_SP);
	    Z80_SP += 2;
	    T_COUNT(10);
	    break;

	  case 0xC5:	/* push bc */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_BC);
	    T_COUNT(11);
	    break;
	  case 0xD5:	/* push de */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_DE);
	    T_COUNT(11);
	    break;
	  case 0xE5:	/* push hl */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_HL);
	    T_COUNT(11);
	    break;
	  case 0xF5:	/* push af */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_AF);
	    T_COUNT(11);
	    break;

	  case 0xC9:	/* ret */
	    Z80_PC = mem_read_word(Z80_SP);
	    Z80_SP += 2;
	    T_COUNT(10);
	    break;

	  case 0xC0:	/* ret nz */
	    if(!ZERO_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xC8:	/* ret z */
	    if(ZERO_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xD0:	/* ret nc */
	    if(!CARRY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xD8:	/* ret c */
	    if(CARRY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xE0:	/* ret po */
	    if(!PARITY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xE8:	/* ret pe */
	    if(PARITY_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xF0:	/* ret p */
	    if(!SIGN_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;
	  case 0xF8:	/* ret m */
	    if(SIGN_FLAG)
	    {
		Z80_PC = mem_read_word(Z80_SP);
		Z80_SP += 2;
		T_COUNT(11);
            } else {
	        T_COUNT(5);
	    }
	    break;

	  case 0x17:	/* rla */
	    do_rla();
	    T_COUNT(4);
	    break;

	  case 0x07:	/* rlca */
	    do_rlca();
	    T_COUNT(4);
	    break;

	  case 0x1F:	/* rra */
	    do_rra();
	    T_COUNT(4);
	    break;

	  case 0x0F:	/* rrca */
	    do_rrca();
	    T_COUNT(4);
	    break;

	  case 0xC7:	/* rst 00h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x00;
	    T_COUNT(11);
	    break;
	  case 0xCF:	/* rst 08h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x08;
	    T_COUNT(11);
	    break;
	  case 0xD7:	/* rst 10h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x10;
	    T_COUNT(11);
	    break;
	  case 0xDF:	/* rst 18h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x18;
	    T_COUNT(11);
	    break;
	  case 0xE7:	/* rst 20h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x20;
	    T_COUNT(11);
	    break;
	  case 0xEF:	/* rst 28h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x28;
	    T_COUNT(11);
	    break;
	  case 0xF7:	/* rst 30h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x30;
	    T_COUNT(11);
	    break;
	  case 0xFF:	/* rst 38h */
	    Z80_SP -= 2;
	    mem_write_word(Z80_SP, Z80_PC);
	    Z80_PC = 0x38;
	    T_COUNT(11);
	    break;

	  case 0x37:	/* scf */
	    Z80_F = (Z80_F & (ZERO_FLAG|PARITY_FLAG|SIGN_FLAG))
	      | CARRY_MASK
	      | (Z80_A & (UNDOC3_MASK|UNDOC5_MASK));
	    T_COUNT(4);
	    break;

	  case 0x9F:	/* sbc a, a */
	    do_sbc_byte(Z80_A);  T_COUNT(4);
	    break;
	  case 0x98:	/* sbc a, b */
	    do_sbc_byte(Z80_B);  T_COUNT(4);
	    break;
	  case 0x99:	/* sbc a, c */
	    do_sbc_byte(Z80_C);  T_COUNT(4);
	    break;
	  case 0x9A:	/* sbc a, d */
	    do_sbc_byte(Z80_D);  T_COUNT(4);
	    break;
	  case 0x9B:	/* sbc a, e */
	    do_sbc_byte(Z80_E);  T_COUNT(4);
	    break;
	  case 0x9C:	/* sbc a, h */
	    do_sbc_byte(Z80_H);  T_COUNT(4);
	    break;
	  case 0x9D:	/* sbc a, l */
	    do_sbc_byte(Z80_L);  T_COUNT(4);
	    break;
	  case 0xDE:	/* sbc a, value */
	    do_sbc_byte(mem_read(Z80_PC++));  T_COUNT(7);
	    break;
	  case 0x9E:	/* sbc a, (hl) */
	    do_sbc_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0x97:	/* sub a, a */
	    do_sub_byte(Z80_A);  T_COUNT(4);
	    break;
	  case 0x90:	/* sub a, b */
	    do_sub_byte(Z80_B);  T_COUNT(4);
	    break;
	  case 0x91:	/* sub a, c */
	    do_sub_byte(Z80_C);  T_COUNT(4);
	    break;
	  case 0x92:	/* sub a, d */
	    do_sub_byte(Z80_D);  T_COUNT(4);
	    break;
	  case 0x93:	/* sub a, e */
	    do_sub_byte(Z80_E);  T_COUNT(4);
	    break;
	  case 0x94:	/* sub a, h */
	    do_sub_byte(Z80_H);  T_COUNT(4);
	    break;
	  case 0x95:	/* sub a, l */
	    do_sub_byte(Z80_L);  T_COUNT(4);
	    break;
	  case 0xD6:	/* sub a, value */
	    do_sub_byte(mem_read(Z80_PC++));  T_COUNT(7);
	    break;
	  case 0x96:	/* sub a, (hl) */
	    do_sub_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;

	  case 0xEE:	/* xor value */
	    do_xor_byte(mem_read(Z80_PC++));  T_COUNT(7);
	    break;

	  case 0xAF:	/* xor a */
	    do_xor_byte(Z80_A);  T_COUNT(4);
	    break;
	  case 0xA8:	/* xor b */
	    do_xor_byte(Z80_B);  T_COUNT(4);
	    break;
	  case 0xA9:	/* xor c */
	    do_xor_byte(Z80_C);  T_COUNT(4);
	    break;
	  case 0xAA:	/* xor d */
	    do_xor_byte(Z80_D);  T_COUNT(4);
	    break;
	  case 0xAB:	/* xor e */
	    do_xor_byte(Z80_E);  T_COUNT(4);
	    break;
	  case 0xAC:	/* xor h */
	    do_xor_byte(Z80_H);  T_COUNT(4);
	    break;
	  case 0xAD:	/* xor l */
	    do_xor_byte(Z80_L);  T_COUNT(4);
	    break;
	  case 0xAE:	/* xor (hl) */
	    do_xor_byte(mem_read(Z80_HL));  T_COUNT(7);
	    break;
	}

	/* Event scheduler */
	if (z80_state.sched &&
	    (z80_state.sched - z80_state.t_count > TSTATE_T_MID)) {
	  /* Subtraction wrapped; time for event to happen */
	  trs_do_event();
	}

	/* Check for an interrupt */
	if (trs_continuous >= 0)
        {
	    /* Handle NMI first */
	    if (z80_state.nmi && !z80_state.nmi_seen)
	    {
	        if (instruction == 0x76) {
		    /* Taking a NMI gets us out of a halt */
		    Z80_PC++;
		}
	        do_nmi();
	        z80_state.nmi_seen = TRUE;
                if (trs_model == 1) {
		  /* Simulate releasing the pushbutton here; ugh. */
		  trs_reset_button_interrupt(0);
		}
	    }
	    /* Allow IRQ if enabled and instruction was not EI */
	    else if (z80_state.irq && z80_state.iff1 == 1
		     && instruction != 0xFB)
            {
	        if (instruction == 0x76) {
		    /* Taking an interrupt gets us out of a halt */
		    Z80_PC++;
		}
	        do_int();
	    }
	}
    } while (trs_continuous > 0);
    return ret;
}


void z80_reset(void)
{
    Z80_PC = 0;
    Z80_A = 0xFF;
    Z80_F = 0xFF;
    Z80_SP = 0xFFFF;
    z80_state.i = 0;
    z80_state.r = 0;
    z80_state.r7 = 0;
    z80_state.iff1 = 0;
    z80_state.iff2 = 0;
    z80_state.interrupt_mode = 0;
    z80_state.irq = z80_state.nmi = FALSE;
    z80_state.sched = 0;
}

void trs_z80_save(FILE *file)
{
  trs_save_uint16(file, &z80_state.af.word, 1);
  trs_save_uint16(file, &z80_state.bc.word, 1);
  trs_save_uint16(file, &z80_state.de.word, 1);
  trs_save_uint16(file, &z80_state.hl.word, 1);
  trs_save_uint16(file, &z80_state.ix.word, 1);
  trs_save_uint16(file, &z80_state.iy.word, 1);
  trs_save_uint16(file, &z80_state.sp.word, 1);
  trs_save_uint16(file, &z80_state.pc.word, 1);
  trs_save_uint16(file, &z80_state.af_prime.word, 1);
  trs_save_uint16(file, &z80_state.bc_prime.word, 1);
  trs_save_uint16(file, &z80_state.de_prime.word, 1);
  trs_save_uint16(file, &z80_state.hl_prime.word, 1);
  trs_save_uint8(file, &z80_state.i, 1);
  trs_save_uint8(file, &z80_state.r, 1);
  trs_save_uint8(file, &z80_state.r7, 1);
  trs_save_uint8(file, &z80_state.iff1, 1);
  trs_save_uint8(file, &z80_state.iff2, 1);
  trs_save_uint8(file, &z80_state.interrupt_mode, 1);
  trs_save_int(file, &z80_state.irq, 1);
  trs_save_int(file, &z80_state.nmi, 1);
  trs_save_int(file, &z80_state.nmi_seen, 1);
  trs_save_uint64(file, &z80_state.t_count, 1);
  trs_save_float(file, &z80_state.clockMHz, 1);
  trs_save_uint64(file, &z80_state.sched, 1);
  trs_save_uint64(file, &last_t_count, 1);
}

void trs_z80_load(FILE *file)
{
  trs_load_uint16(file, &z80_state.af.word, 1);
  trs_load_uint16(file, &z80_state.bc.word, 1);
  trs_load_uint16(file, &z80_state.de.word, 1);
  trs_load_uint16(file, &z80_state.hl.word, 1);
  trs_load_uint16(file, &z80_state.ix.word, 1);
  trs_load_uint16(file, &z80_state.iy.word, 1);
  trs_load_uint16(file, &z80_state.sp.word, 1);
  trs_load_uint16(file, &z80_state.pc.word, 1);
  trs_load_uint16(file, &z80_state.af_prime.word, 1);
  trs_load_uint16(file, &z80_state.bc_prime.word, 1);
  trs_load_uint16(file, &z80_state.de_prime.word, 1);
  trs_load_uint16(file, &z80_state.hl_prime.word, 1);
  trs_load_uint8(file, &z80_state.i, 1);
  trs_load_uint8(file, &z80_state.r, 1);
  trs_load_uint8(file, &z80_state.r7, 1);
  trs_load_uint8(file, &z80_state.iff1, 1);
  trs_load_uint8(file, &z80_state.iff2, 1);
  trs_load_uint8(file, &z80_state.interrupt_mode, 1);
  trs_load_int(file, &z80_state.irq, 1);
  trs_load_int(file, &z80_state.nmi, 1);
  trs_load_int(file, &z80_state.nmi_seen, 1);
  trs_load_uint64(file, &z80_state.t_count, 1);
  trs_load_float(file, &z80_state.clockMHz, 1);
  trs_load_uint64(file, &z80_state.sched, 1);
  trs_load_uint64(file, &last_t_count, 1);
}

