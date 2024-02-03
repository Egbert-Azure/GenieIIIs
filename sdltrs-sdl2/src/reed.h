/* Matthew Reed's hard drive format.  Thanks to Matthew for providing
   documentation.  The comments below are copied from his mail
   messages, with some additions. HDV 1.0 File Format (HDV1):
   http://www.trs-80emulators.com/hdv1-format/ */

#ifndef _REED_H
#define _REED_H

#include <SDL_types.h>

typedef struct {
  Uint8 id1;       /* 0: Identifier #1: 56H */
  Uint8 id2;       /* 1: Identifier #2: CBH */
  Uint8 ver;       /* 2: Version of format: 10H = version 1.0 */
  Uint8 cksum;     /* 3: checksum (not used) */
  Uint8 blks;      /* 4: Number of 256 byte blocks in header: must be 1 */
  Uint8 mb4;       /* 5: Must be 4 */
  Uint8 media;     /* 6: Media type (must be 0 for hard disk) */
  Uint8 flag1;     /* 7: Write protection: 0x80 if write protected, 0x00 if not */
  Uint8 flag2;     /* 8: Flags: bit 7-1: reserved, bit 0: set if auto-boot */
  Uint8 flag3;     /* 9: Reserved */
  Uint8 crtr;      /* 10: Created by:
		      14H = HDFORMAT
		      42H = xtrs mkdisk
                      80H = Cervasio xtrshard port to Vavasour M4 emulator */
  Uint8 dfmt;      /* 11: DOS type (only needed for auto-boot):
		      0 = Model 4 LS-DOS
		      1 = Model I/III LDOS
		      2 = CP/M
		      3 = NEWDOS */
  Uint8 res1[14];  /* 12 - 25: reserved */
  Uint8 heads;     /* 26: If non-zero, number of heads per cylinder.
			  If zero, number of heads per cylinder is calculated as
			  number of sectors per cylinder (byte 29) divided by 32. */
  Uint8 cylhi;     /* 27: Number of cylinders per disk (high 8 bits) */
  Uint8 cyllo;     /* 28: Number of cylinders per disk (lower 8 bits)
			  This is the number of cylinders on the drive. which shouldn’t
			  be higher than 1024. To preserve backwards compatibility,
			  values of 0 in both bytes 27 and 28 means 256.*/
  Uint8 sec;       /* 29: Number of sectors per cylinder */
  Uint8 gran;      /* 30: Number of granules per track (deprecated) */
  Uint8 dcyl;      /* 31: Directory cylinder (deprecated, should be 1) */
  char label[32];  /* 32: Volume label: 31 bytes terminated by 0 */
  Uint8 res2[192]; /* 64 - 255: reserved */
} ReedHardHeader;

#endif /* _REED_H */
