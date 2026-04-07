#ifndef _TRS_MEMORY_H
#define _TRS_MEMORY_H

#include <SDL_types.h>

/* Locations for Model I, Model III, and Model 4/4P map 0 and 1 */
#define VIDEO_START     (0x3C00)
#define KEYBOARD_START  (0x3800)
#define RAM_START       (0x4000)

/* Memory Expansion Cards */
#define HUFFMAN         (1) /* Dave Huffman (and other) 2 MB (4/4P) */
#define HYPERMEM        (2) /* Anitek HyperMem 1 MB (4/4P) */
#define LUBOMIR         (3) /* Lubomir Soft Banker TRS-80 Model I */
#define MEGAMEM         (4) /* Anitek MegaMem  3 MB (III/4/4P) */
#define SELECTOR        (5) /* Selector Card for TRS-80 Model I */
#define SUPERMEM        (6) /* Alpha Technology SuperMem 1024 KB */
#define XMEM80          (7) /* X-MEM/80 M1 16K page 512 KB */

extern int    huffman;
extern int    hypermem;
extern int    lubomir;
extern int    megamem;
extern int    selector;
extern int    supermem;
extern int    xmem80;

extern int    trs80_model3_mem_read(int address);
extern void   trs80_model3_mem_write(int address, int value);
extern Uint8 *trs80_model3_mem_addr(int address, int writing);

extern int    mem_peek(int address);
extern void   mem_poke(int address, int value);
extern int    mem_read(int address);
extern void   mem_write(int address, int value);
extern void   rom_write(int address, int value);
extern int    mem_read_word(int address);
extern void   mem_write_word(int address, int value);
extern Uint8 *mem_pointer(int address, int writing);

extern void   mem_video_page(int offset);
extern Uint8  mem_video_page_read(int vaddr);
extern int    mem_video_page_write(int vaddr, Uint8 value);
extern Uint8 *mem_video_page_addr(int vaddr);

extern int    get_mem_cmd(void);
extern int    get_mem_map(void);
extern void   mem_bank(int which);
extern void   mem_map(int which);

extern void   ct80_ramdisk_out(int port, Uint8 byte);
extern Uint8  ct80_ramdisk_in(int port);
extern void   ct80_video_addr(int address);

extern void   eg3200_genieplus_out(int bits);
extern void   eg64_mba_out(int value);
extern void   genie3s_mem_out(int bits);
extern void   huffman_out(int bits);
extern int    huffman_in(void);
extern void   hypermem_out(int bits);
extern void   lnw80_bank_out(int value);
extern void   lubomir_out(int bits);
extern void   megamem_out(int mem_slot, Uint8 value);
extern void   s80z_out(int value);
extern void   selector_out(int bits);
extern void   supermem_out(int bits);
extern int    supermem_in(void);
extern void   sys_byte_out(int value);
extern void   sys_byte_write(int value);
extern int    sys_byte_in(void);
extern int    sys_byte_2_in(void);
extern void   tcs_ram192b_out(int bits);
extern void   vid80_vx3_out(int bits);

extern void   xmem80_out(int port, int page);
extern int    xmem80_in(int port);

#endif /* _TRS_MEMORY_H */
