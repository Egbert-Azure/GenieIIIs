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

#ifdef ZBX
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <strings.h>

#ifdef READLINE
#include <readline/readline.h>
#include <readline/history.h>
#endif

#include "error.h"
#include "trs.h"
#include "trs_memory.h"
#include "trs_state_save.h"

#define MAXLINE			(256)
#define ADDRESS_SPACE		(0x10000)
#define MAX_TRAPS		(100)

#define BREAKPOINT_FLAG		(0x1)
#define TRACE_FLAG		(0x2)
#define DISASSEMBLE_ON_FLAG	(0x4)
#define DISASSEMBLE_OFF_FLAG	(0x8)
#define BREAK_ONCE_FLAG		(0x10)
#define WATCHPOINT_FLAG		(0x20)

static Uint8 traps[ADDRESS_SPACE];
static int num_traps;
static int print_instructions;
static int stop_signaled;
static unsigned int num_watchpoints;

static struct
{
    int   valid;
    int   address;
    int   flag;
    Uint8 byte; /* used only by watchpoints */
} trap_table[MAX_TRAPS];

static void help(void)
{
    puts("(zbx) commands:\n\
\nRunning:\n\
  r(un) ................. Hard reset Z80 and devices and commence execution\n\
  c(ont) | g(o) <addr> .. Continue execution at current or hex address for PC\n\
  s(tep) ................ Execute one instruction (one iteration/no interrupt)\n\
  s(tep)i(nt) ........... Execute one instruction (one iteration/interrupt)\n\
  n(ext) ................ Execute one instruction (interrupt allowed)\n\
  n(ext)i(nt) ........... Execute one instruction (interrupt allowed afterwards)\n\
  re(set) ............... Hard reset Z80 and devices\n\
  s(oft)r(eset) ......... Press system reset button\n\
\nPrinting:\n\
  (dum)p ................ Print Z80 registers\n\
  l(ist) | d(is) <addr> . Disassemble 10 instructions at current or hex address\n\
  l | d <start> , <end> . Disassemble instructions in the range of hex addresses\n\
  <start> , <end> ....... Print memory values in specified address range\n\
  <start> / <num bytes> . Print number of memory values starting at hex address\n\
  <addr> ................ Print memory value at specified hex address\n\
  tr(ace)on ............. Enable tracing of all instructions\n\
  tr(ace)off ............ Disable tracing\n\
  d(isk)d(ump) .......... Print state of floppy disk controller emulation\n\
  h(ard)d(ump) .......... Print state of hard disk controller emulation\n\
\nTraps:\n\
  st(atus) .............. Show all traps (breakpoints, tracepoints, watchpoints)\n\
  cl(ear) <addr> ........ Delete trap at current or specified hex address\n\
  del(ete) <n> | * ...... Delete trap <n> or all traps\n\
  b(reak) <addr> ........ Set breakpoint at specified hex address\n\
  t(race) <addr> ........ Set trap to trace execution at specified hex address\n\
  tr(ace)on <addr> ...... Set trap to enable tracing at specified hex address\n\
  tr(ace)off <addr> ..... Set trap to disable tracing at specified hex address\n\
  w(atch) <addr> ........ Set trap to watch specified hex address for changes\n\
\nMiscellaneous:\n\
  a(ssign) | set\n\
    $<reg> = <value> .... Change value of register\n\
    <addr> [= <value>] .. Change value at hex memory address (enter '.' to end)\n\
  rom <addr> = <value> .. Change byte in ROM\n\
  in <port> ............. Input from given I/O port\n\
  out <port> = <value> .. Output to given I/O port\n\
  cmd <file> ............ Load specified CMD file\n\
  peek <addr> [,<end>] .. Print memory values of specified hex address range\n\
  poke <addr> , <value> . Write value to specified hex address\n\
  load <addr> <file> .... Load memory from file to specified hex address\n\
  f(ill) <start>, <end>, <value> ..... Fill memory range with hex value\n\
  m(ove) <start>, <end>, <addr> ...... Move memory range to hex address\n\
  save <start> , <end> <file> ........ Save memory in range of specified\n\
  save <start> / <num bytes> <file>    hex addresses to file\n\
  state load <file> ..... Load emulator state from file\n\
  state save <file> ..... Save current emulator state to file\n\
  timeroff / timeron .... Disable / enable TRS-80 real time clock interrupt\n\
  halt <action> ......... Show / change Z80 HALT action: debug, halt or reset\n\
  di[skdebug] <hexval> .. Set floppy disk controller debug flags to hexval:\n\
                            1 = FDC register I/O      10 = Phys sector sizes\n\
                            2 = FDC commands          20 = Readadr timing\n\
                            4 = VTOS 3.0 JV3 kludges  40 = DMK\n\
                            8 = Gaps                  80 = IOCTL errors\n\
  io[debug] <hexval> .... Set I/O port debug flags:\n\
                            1 = port input             4 = Hard Disk I/O\n\
                            2 = port output            8 = Hard Disk Command\n\
  z(bx)i(nfo) ........... Display information about this debugger\n\
  h(elp) | ? ............ Print this help message\n\
  exit | quit ........... Exit from SDLTRS");
}

static const char *trap_name(int flag)
{
    switch(flag)
    {
      case BREAKPOINT_FLAG:
	return "breakpoint";
      case TRACE_FLAG:
	return "trace";
      case DISASSEMBLE_ON_FLAG:
	return "traceon";
      case DISASSEMBLE_OFF_FLAG:
	return "traceoff";
      case BREAK_ONCE_FLAG:
	return "temporary breakpoint";
      case WATCHPOINT_FLAG:
	return "watchpoint";
      default:
	return "unknown trap";
    }
}

static void info(void)
{
    puts("zbx: Z80 debugger by David Gingold, Alex Wolman, and Timothy Mann\n");
    printf("Traps set: %d (maximum %d)\n", num_traps, MAX_TRAPS);
    printf("Size of address space: 0x%X\n", ADDRESS_SPACE);
    printf("Maximum length of command line: %d\n", MAXLINE);
#ifdef READLINE
    puts("GNU Readline library support enabled.");
#else
    puts("GNU Readline library support disabled.");
#endif
}

static void clear_all_traps(void)
{
    int i;

    for(i = 0; i < MAX_TRAPS; ++i)
    {
	if(trap_table[i].valid)
	{
	    traps[trap_table[i].address] &= ~(trap_table[i].flag);
	    trap_table[i].valid = 0;
	}
    }
    num_traps = 0;
    num_watchpoints = 0;
}

static void print_traps(void)
{
    if(num_traps)
    {
	int i;

	for(i = 0; i < MAX_TRAPS; ++i)
	{
	    if(trap_table[i].valid)
	    {
		printf("[%d] %.4x (%s)\n", i, trap_table[i].address,
		       trap_name(trap_table[i].flag));
	    }
	}
    }
    else
    {
	puts("No traps are set.");
    }
}

static void set_trap(int address, int flag)
{
    if(num_traps == MAX_TRAPS)
    {
	printf("Cannot set more than %d traps.\n", MAX_TRAPS);
    }
    else
    {
	int i = 0;

	address &= 0xFFFF; /* allow callers to be sloppy */

	while(trap_table[i].valid) ++i;

	trap_table[i].valid = 1;
	trap_table[i].address = address;
	trap_table[i].flag = flag;

	if (trap_table[i].flag == WATCHPOINT_FLAG) {
	    /* Initialize the byte field to current memory contents. */
	    trap_table[i].byte = mem_read(address);
	    /* Increment number of set watchpoints. */
	    num_watchpoints++;
	}

	traps[address] |= flag;
	num_traps++;

	printf("Set %s [%d] at %.4x\n", trap_name(flag), i, address);
    }
}

static void clear_trap(int i)
{
    if((i < 0) || (i > MAX_TRAPS - 1) || !trap_table[i].valid)
    {
	printf("[%d] is not a valid trap.\n", i);
    }
    else
    {
	traps[trap_table[i].address] &= ~(trap_table[i].flag);
	trap_table[i].valid = 0;

	if (trap_table[i].flag == WATCHPOINT_FLAG) {
	    /* Decrement number of set watchpoints. */
	    num_watchpoints--;
	}

	num_traps--;
	printf("Cleared %s [%d] at %.4x\n",
	       trap_name(trap_table[i].flag), i, trap_table[i].address);
    }
}

static void clear_trap_address(int address, int flag)
{
    int i;

    for(i = 0; i < MAX_TRAPS; ++i)
    {
	if(trap_table[i].valid && (trap_table[i].address == address)
	   && ((flag == 0) || (trap_table[i].flag == flag)))
	{
	    clear_trap(i);
	}
    }
}

static void print_registers(void)
{
    puts("\n       S Z - H - PV N C   IFF1 IFF2 IM");
    printf("Flags: %d %d %d %d %d  %d %d %d     %d    %d   %d\n\n",
	   (SIGN_FLAG != 0),
	   (ZERO_FLAG != 0),
	   (Z80_F & UNDOC5_MASK) != 0,
	   (HALF_CARRY_FLAG != 0),
	   (Z80_F & UNDOC3_MASK) != 0,
	   (OVERFLOW_FLAG != 0),
	   (SUBTRACT_FLAG != 0),
	   (CARRY_FLAG != 0),
	   z80_state.iff1, z80_state.iff2, z80_state.interrupt_mode);

    printf("A F: %.2x %.2x    IX: %.4x    AF': %.4x\n",
	   Z80_A, Z80_F, Z80_IX, Z80_AF_PRIME);
    printf("B C: %.2x %.2x    IY: %.4x    BC': %.4x\n",
	   Z80_B, Z80_C, Z80_IY, Z80_BC_PRIME);
    printf("D E: %.2x %.2x    PC: %.4x    DE': %.4x\n",
	   Z80_D, Z80_E, Z80_PC, Z80_DE_PRIME);
    printf("H L: %.2x %.2x    SP: %.4x    HL': %.4x\n",
	   Z80_H, Z80_L, Z80_SP, Z80_HL_PRIME);
    printf("I R: %.2x %.2x\n", Z80_I, Z80_R7 | (Z80_R & 0x7F));

    printf("\nMem map/command: %02X / %02X", get_mem_map(), get_mem_cmd());
    printf("\nSystem Byte 1/2: %02X / %02X", sys_byte_in(), sys_byte_2_in());
    printf("\nT-state counter: %" TSTATE_T_LEN "", z80_state.t_count);
    printf("\nZ80 Clock Speed: %.2f MHz\n", z80_state.clockMHz);
}


void trs_debug(void)
{
    stop_signaled = 1;
    if (trs_continuous > 0) trs_continuous = 0;
}

static void print_memory(int address, int num_bytes, int peek)
{
    while(num_bytes > 0)
    {
	int byte[16];
	int const bytes_to_print = num_bytes < 16 ? num_bytes : 16;
	int i;

	printf("%.4x: ", address);

	for(i = 0; i < bytes_to_print; ++i)
	{
	    byte[i] = peek ? mem_peek(address + i) : mem_read(address + i);
	    printf("%.2x ", byte[i]);
	}

	for(i = bytes_to_print; i < 16; ++i)
	{
	    printf("   ");
	}
	printf("    ");

	for(i = 0; i < bytes_to_print; ++i)
	{
	    putchar(isprint(byte[i]) ? byte[i] : '.');
	}

	putchar('\n');
	num_bytes -= bytes_to_print;
	address += bytes_to_print;
    }
}

static void load_memory(int address, const char *filename)
{
    FILE *file = fopen(filename, "rb");

    if(file)
    {
	int c;

	while((c = getc(file)) != EOF)
	{
	    mem_write(address++, c);
	}
	fclose(file);
    }
    else
    {
	file_error("load memory: '%s'", filename);
    }
}

static void save_memory(int address, int num_bytes, const char *filename)
{
    FILE *file = fopen(filename, "wb");

    if(file)
    {
	int i;

	for(i = 0; i < num_bytes; ++i)
	{
	    putc(mem_read(address + i), file);
	}
	fclose(file);
    }
    else
    {
	file_error("save memory: '%s'", filename);
    }
}

static void run(void)
{
    Uint8 t = traps[Z80_PC];

    stop_signaled = 0;

    while(!stop_signaled)
    {
	int continuous;

	if(t)
	{
	    if(t & TRACE_FLAG)
	    {
		printf("Trace: ");
		disassemble(Z80_PC);
	    }
	    if(t & DISASSEMBLE_ON_FLAG)
	    {
		print_instructions = 1;
	    }
	    if(t & DISASSEMBLE_OFF_FLAG)
	    {
		print_instructions = 0;
	    }
	}

	if(print_instructions) disassemble(Z80_PC);

	continuous = (!print_instructions && num_traps == 0);
	if (z80_run(continuous)) {
	  puts("emt_debug instruction executed.");
	  stop_signaled = 1;
	}

	t = traps[Z80_PC];

	if(t & BREAKPOINT_FLAG)
	{
	    stop_signaled = 1;
	}

	if(t & BREAK_ONCE_FLAG)
	{
	    stop_signaled = 1;
	    clear_trap_address(Z80_PC, BREAK_ONCE_FLAG);
	}

	/*
	 * Iterate over the trap list looking for watchpoints only if we
	 * know there are any to be found.
	 */
	if (num_watchpoints)
	{
	    int watch_triggered = 0;
	    int i;

	    for (i = 0; i < MAX_TRAPS; ++i)
	    {
		if (trap_table[i].valid &&
		    trap_table[i].flag == WATCHPOINT_FLAG)
		{
		    Uint8 const byte = mem_read(trap_table[i].address);
		    if (byte != trap_table[i].byte)
		    {
			/*
			 * If a watched memory location has changed, report
			 * it, update the watch entry in the trap table to
			 * reflect the new value, and set the
			 * watch_triggered flag so that we stop after all
			 * watchpoints have been processed.
			 */
			printf("Memory location 0x%.4x changed value from "
			       "0x%.2x to 0x%.2x.\n", trap_table[i].address,
			       trap_table[i].byte, byte);
			trap_table[i].byte = byte;
			watch_triggered = 1;
		    }
		}
	    }
	    if (watch_triggered)
	    {
		stop_signaled = 1;
	    }
	}

    }
    printf("Stopped at %.4x\n", Z80_PC);
}

void debug_shell(void)
{
#ifdef READLINE
    char history_file[MAXLINE];
    const char *home = (char *)getenv ("HOME");

    if (!home) home = ".";
    snprintf(history_file, MAXLINE - 1, "%s/.zbx-history", home);
    using_history();
    read_history(history_file);
#endif

    puts("Type \"h(elp)\" for a list of commands.");

    while(1)
    {
	char input[MAXLINE];
	char command[MAXLINE];

	putchar('\n');
	disassemble(Z80_PC);

#ifdef READLINE
	/*
	 * Use the way cool gnu readline() utility.  Get completion,
	 * history, way way cool.
         */
        {

	    char *line = readline("(zbx) ");
	    if(line)
	    {
		if(strlen(line) > 0)
		{
		    add_history(line);
		}
		strncpy(input, line, MAXLINE - 1);
		free(line);
	    }
	    else
	    {
		break;
	    }
	}
#else
	printf("(zbx) ");  fflush(stdout);

	if (fgets(input, MAXLINE, stdin) == NULL) break;
#endif

	if(sscanf(input, "%s", command))
	{
	    if(!strcmp(command, "help") || !strcmp(command, "?") ||
	       !strcmp(command, "h"))
	    {
		help();
	    }
	    else if (!strncmp(command, "z", 1) || !strcmp(command, "i"))
	    {
		info();
	    }
	    else if(!strncmp(command, "cl", 2))
	    {
		unsigned int address = Z80_PC;

		if(sscanf(input, "%*s %x", &address) == 1)
		{
			address &= 0xFFFF;
		}
		clear_trap_address(address, 0);
	    }
	    else if(!strcmp(command, "cont") || !strcmp(command, "c") ||
		    !strcmp(command, "go")   || !strcmp(command, "g"))
	    {
		unsigned int address;

		if(sscanf(input, "%*s %x", &address) == 1) {
		    Z80_PC = address & 0xFFFF;
		}
		run();
	    }
	    else if(!strcmp(command, "dump") || !strcmp(command, "p"))
	    {
		print_registers();
	    }
	    else if(!strcmp(command, "delete") || !strcmp(command, "del"))
	    {
		int i;

		if(!strncmp(input, "delete *", 8) || !strncmp(input, "del *", 5))
		{
		    clear_all_traps();
		}
		else if(sscanf(input, "%*s %d", &i) != 1)
		{
		    puts("Trap must be specified.");
		}
		else
		{
		    clear_trap(i);
		}
	    }
	    else if(!strcmp(command, "list") || !strcmp(command, "l") ||
	            !strcmp(command, "dis")  || !strcmp(command, "d"))
	    {
		unsigned int x, y;
		int start = Z80_PC;
		int bytes = 0;
		int lines = 10;

		if(sscanf(input, "%*s %x %*c %x", &x, &y) == 2)
		{
		    start = x & 0xFFFF;
		    bytes = (y - x) & 0xFFFF;
		    lines = 0;
		}
		else if(sscanf(input, "%*s %x", &x) == 1)
		{
		    start = x & 0xFFFF;
		}

		if(lines)
		{
		    while(lines--)
		    {
			start = disassemble(start);
		    }
		}
		else
		{
		    while (bytes >= 0) {
			int const old_start = start;

			start = disassemble(old_start);
			bytes -= (start - old_start) & 0xFFFF;
		    }
		}
	    }
	    else if(!strcmp(command, "in"))
	    {
		unsigned int port;

		if(sscanf(input, "in %x", &port) == 1)
		{
		    printf("in %x = %x\n", port, z80_in(port));
		}
		else
		{
		    puts("Port must be specified.");
		}
	    }
	    else if(!strcmp(command, "out"))
	    {
		unsigned int port, value;

		if(sscanf(input, "out %x %*c %x", &port, &value) == 2)
		{
		    z80_out(port, value);
		}
		else
		{
		    puts("Port and value must be specified.");
		}
	    }
	    else if(!strcmp(command, "cmd"))
	    {
		char *file = input;

		if(sscanf(input, "%*s %s", file) == 1)
		{
		    trs_load_cmd(file);
		}
		else
		{
		    puts("Filename must be specified.");
		}
	    }
	    else if(!strcmp(command, "next") || !strcmp(command, "nextint") ||
		    !strcmp(command, "n") || !strcmp(command, "ni"))
	    {
		int is_call = 0, is_rst = 0, is_rep = 0;

		switch(mem_read(Z80_PC)) {
		  case 0xCD:	/* call address */
		    is_call = 1;
		    break;
		  case 0xC4:	/* call nz, address */
		    is_call = !ZERO_FLAG;
		    break;
		  case 0xCC:	/* call z, address */
		    is_call = ZERO_FLAG;
		    break;
		  case 0xD4:	/* call nc, address */
		    is_call = !CARRY_FLAG;
		    break;
		  case 0xDC:	/* call c, address */
		    is_call = CARRY_FLAG;
		    break;
		  case 0xE4:	/* call po, address */
		    is_call = !PARITY_FLAG;
		    break;
		  case 0xEC:	/* call pe, address */
		    is_call = PARITY_FLAG;
		    break;
		  case 0xF4:	/* call p, address */
		    is_call = !SIGN_FLAG;
		    break;
		  case 0xFC:	/* call m, address */
		    is_call = SIGN_FLAG;
		    break;
		  case 0xC7:
		  case 0xCF:
		  case 0xD7:
		  case 0xDF:
		  case 0xE7:
		  case 0xEF:
		  case 0xF7:
		  case 0xFF:
		    is_rst = 1;
		    break;
		  case 0xED:
		    switch(mem_read(Z80_PC + 1)) {
		      case 0xB0: /* ldir */
		      case 0xB8: /* lddr */
		      case 0xB1: /* cpir */
		      case 0xB9: /* cpdr */
		      case 0xB2: /* inir */
		      case 0xBA: /* indr */
		      case 0xB3: /* otir */
		      case 0xBB: /* otdr */
		        is_rep = 1;
		        break;
		      default:
		        break;
		    }
		  default:
		    break;
		}
		if (is_call) {
		    set_trap(Z80_PC + 3, BREAK_ONCE_FLAG);
		    run();
		} else if (is_rst) {
		    set_trap(Z80_PC + 1, BREAK_ONCE_FLAG);
		    run();
		} else if (is_rep) {
		    set_trap(Z80_PC + 2, BREAK_ONCE_FLAG);
		    run();
		} else {
		    z80_run((!strcmp(command, "nextint") || !strcmp(command, "ni")) ? 0 : -1);
		}
	    }
	    else if(!strcmp(command, "exit") || !strcmp(command, "quit"))
	    {
		break;
	    }
	    else if(!strncmp(command, "re", 2))
	    {
		puts("Performing hard reset.");
		trs_reset(1);
	    }
	    else if(!strcmp(command, "softreset") || !strcmp(command, "sr"))
	    {
		puts("Pressing reset button.");
		trs_reset(0);
	    }
	    else if(!strncmp(command, "pe", 2))
	    {
		unsigned int start, end;

		if (sscanf(input, "%*s %x %*c %x", &start, &end) == 2)
		{
		    print_memory(start, end - start + 1, 1);
		}
		else if (sscanf(input, "%*s %x", &start) == 1)
		{
		    print_memory(start, 1, 1);
		}
		else
		{
		    puts("Address or range must be specified.");
		}
	    }
	    else if(!strncmp(command, "po", 2))
	    {
		unsigned int address, value;

		if (sscanf(input, "%*s %x %*c %x", &address, &value) == 2)
		{
		    mem_poke(address, value);
		}
		else
		{
		    puts("Address and value must be specified.");
		}
	    }
	    else if(!strcmp(command, "rom"))
	    {
		unsigned int address, value;

		if (sscanf(input, "%*s %x %*c %x", &address, &value) == 2)
		{
		    rom_write(address, value);
		}
		else
		{
		    puts("ROM address and value must be specified.");
		}
	    }
	    else if(!strcmp(command, "run") || !strcmp(command, "r"))
	    {
		puts("Performing hard reset and running.");
		trs_reset(1);
		run();
	    }
	    else if(!strcmp(command, "status") || !strcmp(command, "st"))
	    {
		print_traps();
	    }
	    else if(!strcmp(command, "set") || !strncmp(command, "as", 2) ||
		    !strcmp(command, "a"))
	    {
		char regname[4];
		unsigned int address, value;

		if(sscanf(input, "%*s $%3[a-zA-Z'] %*c %x", regname, &value) == 2)
		{
		    if(!strcasecmp(regname, "a")) {
			Z80_A = value;
		    } else if(!strcasecmp(regname, "f")) {
			Z80_F = value;
		    } else if(!strcasecmp(regname, "b")) {
			Z80_B = value;
		    } else if(!strcasecmp(regname, "c")) {
			Z80_C = value;
		    } else if(!strcasecmp(regname, "d")) {
			Z80_D = value;
		    } else if(!strcasecmp(regname, "e")) {
			Z80_E = value;
		    } else if(!strcasecmp(regname, "h")) {
			Z80_H = value;
		    } else if(!strcasecmp(regname, "l")) {
			Z80_L = value;
		    } else if(!strcasecmp(regname, "sp")) {
			Z80_SP = value;
		    } else if(!strcasecmp(regname, "pc")) {
			Z80_PC = value;
		    } else if(!strcasecmp(regname, "af")) {
			Z80_AF = value;
		    } else if(!strcasecmp(regname, "bc")) {
			Z80_BC = value;
		    } else if(!strcasecmp(regname, "de")) {
			Z80_DE = value;
		    } else if(!strcasecmp(regname, "hl")) {
			Z80_HL = value;
		    } else if(!strcasecmp(regname, "af'")) {
			Z80_AF_PRIME = value;
		    } else if(!strcasecmp(regname, "bc'")) {
			Z80_BC_PRIME = value;
		    } else if(!strcasecmp(regname, "de'")) {
			Z80_DE_PRIME = value;
		    } else if(!strcasecmp(regname, "hl'")) {
			Z80_HL_PRIME = value;
		    } else if(!strcasecmp(regname, "ix")) {
			Z80_IX = value;
		    } else if(!strcasecmp(regname, "iy")) {
			Z80_IY = value;
		    } else if(!strcasecmp(regname, "i")) {
			Z80_I = value;
		    } else if(!strcasecmp(regname, "r")) {
			Z80_R = value;
			Z80_R7 = value & 0x80;
		    } else {
			printf("Unrecognized register name '%s'.\n", regname);
		    }
		}
		else if(sscanf(input, "%*s %x %*c %x", &address, &value) == 2)
		{
		    mem_write(address, value);
		}
		else if(sscanf(input, "%*s %x", &address) == 1)
		{
		    while(1)
		    {
		        address &= 0xFFFF;
			printf("%.4x: %.2x ", address, mem_read(address));

			if(fgets(input, MAXLINE, stdin) == NULL) continue;
			if(!strncmp(input, ".", 1)) break;
			if(sscanf(input, "%x", &value) == 1)
			{
			    mem_write(address, value);
			}
			address++;
		    }
		}
		else
		{
		    puts("Register or address must be specified.");
		}
	    }
	    else if(!strcmp(command, "fill") || !strcmp(command, "f"))
	    {
		unsigned start, end, value;

		if(sscanf(input, "%*s %x %*c %x %*c %x", &start, &end, &value) == 3)
		{
		    start &= 0xFFFF;
		    end   &= 0xFFFF;

		    while(start <= end)
			mem_write(start++, value);
		}
		else
		{
		    puts("Address range and value must be specified.");
		}
	    }
	    else if(!strcmp(command, "move") || !strcmp(command, "m"))
	    {
		unsigned start, end, addr;

		if(sscanf(input, "%*s %x %*c %x %*c %x", &start, &end, &addr) == 3)
		{
		    start &= 0xFFFF;
		    end   &= 0xFFFF;
		    addr  &= 0xFFFF;

		    while(start <= end)
			mem_write(addr++, mem_read(start++));
		}
		else
		{
		    puts("Address range and destination must be specified.");
		}
	    }
	    else if(!strcmp(command, "step") || !strcmp(command, "s"))
	    {
		z80_run(-1);
	    }
	    else if(!strcmp(command, "stepint") || !strcmp(command, "si"))
	    {
		z80_run(0);
	    }
	    else if(!strcmp(command, "stop") || !strncmp(command, "br", 2) ||
		    !strcmp(command, "b"))
	    {
		unsigned int address;

		if(sscanf(input, "%*s %x", &address) != 1)
		{
		    address = Z80_PC;
		}
		set_trap(address, BREAKPOINT_FLAG);
	    }
	    else if(!strcmp(command, "trace") || !strcmp(command, "t"))
	    {
		unsigned int address;

		if(sscanf(input, "%*s %x", &address) != 1)
		{
		    address = Z80_PC;
		}
		set_trap(address, TRACE_FLAG);
	    }
	    else if(!strcmp(command, "traceon") || !strcmp(command, "tron"))
	    {
		unsigned int address;

		if(sscanf(input, "%*s %x", &address) == 1)
		{
		    set_trap(address, DISASSEMBLE_ON_FLAG);
		}
		else
		{
		    print_instructions = 1;
		    puts("Tracing enabled.");
		}
	    }
	    else if(!strcmp(command, "traceoff") || !strcmp(command, "troff"))
	    {
		unsigned int address;

		if(sscanf(input, "%*s %x", &address) == 1)
		{
		    set_trap(address, DISASSEMBLE_OFF_FLAG);
		}
		else
		{
		    print_instructions = 0;
		    puts("Tracing disabled.");
		}
	    }
	    else if(!strncmp(command, "w", 1))
	    {
		unsigned int address;

		if(sscanf(input, "%*s %x", &address) == 1)
		{
		    set_trap(address, WATCHPOINT_FLAG);
		}
		else
		{
		    puts("Address must be specified.");
		}
	    }
	    else if(!strcmp(command, "timeroff"))
	    {
		/* Turn off emulated real time clock interrupt */
		trs_timer(0);
            }
	    else if(!strcmp(command, "timeron"))
	    {
		/* Turn on  emulated real time clock interrupt */
		trs_timer(1);
            }
	    else if(!strcmp(command, "halt"))
	    {
		char halt;

		if(sscanf(input, "%*s %c", &halt) == 1)
		{
		    Z80_HALT = TOLOWER((int)halt);
		}
		switch (Z80_HALT) {
		    case 'd':
			puts("debug");
			break;
		    case 'h':
			puts("halt");
			break;
		    case 'r':
			puts("reset");
			break;
		    default:
			puts("default");
			break;
		}
	    }
	    else if(!strcmp(command, "diskdump") || !strcmp(command, "dd"))
	    {
		trs_disk_debug();
	    }
	    else if(!strcmp(command, "harddump") || !strcmp(command, "hd"))
	    {
		trs_hard_debug();
	    }
	    else if(!strncmp(command, "di", 2))
	    {
		trs_disk_debug_flags = 0;
		sscanf(input, "%*s %x", (unsigned int *)&trs_disk_debug_flags);
	    }
	    else if(!strncmp(command, "io", 2))
	    {
		trs_io_debug_flags = 0;
		sscanf(input, "%*s %x", (unsigned int *)&trs_io_debug_flags);
	    }
	    else if(!strcmp(command, "load"))
	    {
		unsigned int address;
		char *file = input;

		if(sscanf(input,"load %x %s", &address, file) == 2)
		{
		    load_memory(address, file);
		}
		else
		{
		    puts("Address and filename must be specified.");
		}
	    }
	    else if(!strcmp(command, "save"))
	    {
		unsigned int start, end;
		char *file = input;

		if(sscanf(input, "save %x / %x %s", &start, &end, file) == 3)
		{
		    save_memory(start, end, file);
		}
		else if(sscanf(input, "save %x %*c %x %s", &start, &end, file) == 3)
		{
		    save_memory(start, end - start + 1, file);
		}
		else
		{
		    puts("Address range and filename must be specified.");
		}
	    }
	    else if(!strcmp(command, "state"))
	    {
		char *file = input;

		if(sscanf(input, "state load %s", file) == 1)
		{
		    trs_state_load(file);
		}
		else if(sscanf(input, "state save %s", file) == 1)
		{
		    trs_state_save(file);
		}
		else
		{
		    puts("Filename must be specified.");
		}
	    }
	    else
	    {
		unsigned int start, end;

		if(sscanf(input, "%x / %x ", &start, &end) == 2)
		{
		    print_memory(start, end, 0);
		}
		else if(sscanf(input, "%x %*c %x", &start, &end) == 2)
		{
		    print_memory(start, end - start + 1, 0);
		}
		else if(sscanf(input, "%x", &start) == 1)
		{
		    print_memory(start, 1, 0);
		}
		else
		{
		    puts("Syntax error.  (Type \"h(elp)\" for commands.)");
		}
	    }
	}
    }

#ifdef READLINE
    write_history(history_file);
#endif

}
#endif
