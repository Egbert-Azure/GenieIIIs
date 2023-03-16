#ifndef _TRS_MEMORY_H
#define _TRS_MEMORY_H

/* Locations for Model I, Model III, and Model 4 map 0 */
#define VIDEO_START     (0x3C00)
#define PRINTER_ADDRESS (0x37E8)
#define KEYBOARD_START  (0x3800)
#define RAM_START       (0x4000)

/* Memory Expansion Cards */
#define GENIEPLUS       (1) /* EACA EG 3200 Genie III 384 KB */
#define HUFFMAN         (2) /* Dave Huffman (and other) 2 MB (4/4P) */
#define HYPERMEM        (3) /* Anitek HyperMem 1 MB (4/4P) */
#define RAM192B         (4) /* TCS Genie IIs/SpeedMaster 768 KB */
#define SELECTOR        (5) /* Selector Card for TRS-80 Model I */
#define SUPERMEM        (6) /* AlphaTech SuperMem 512 KB (I/III) */

int    trs80_model3_mem_read(int address);
void   trs80_model3_mem_write(int address, int value);
Uint8 *trs80_model3_mem_addr(int address, int writing);

void   mem_video_page(int offset);
Uint8  mem_video_read(int vaddr);
Uint8  mem_video_page_read(int vaddr);
int    mem_video_write(int vaddr, Uint8 value);
int    mem_video_page_write(int vaddr, Uint8 value);
Uint8 *mem_video_page_addr(int vaddr);

extern void mem_bank(int which);
extern void mem_map(int which);
extern void mem_bank_base(int card, int bits);
extern int  mem_read_bank_base(int card);
extern void mem_romin(int state);

extern void megamem_out(int mem_slot, Uint8 value);

extern void eg64_mba_out(int value);
extern void lsb_bank_out(int value);
extern void s80z_out(int value);
extern void sys_byte_out(int value);
extern int  sys_byte_in(void);

#endif
