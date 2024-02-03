#ifndef _TRS_CLONES_H
#define _TRS_CLONES_H

/* Clones */
#define CP500       (1 << 1)
#define CP500_M80   (1 << 2)
#define CT80        (1 << 3)
#define EG3200      (1 << 4)
#define GENIE3S     (1 << 5)
#define LNW80       (1 << 6)
#define SPEEDMASTER (1 << 7)

struct trs_clones {
  /*
   * Clone Model:
   */
  int model;
  /*
   * Name of the clone:
   */
  char *name;
};

extern struct trs_clones trs_clones;
extern void trs_clones_model(int clone);

#endif /* _TRS_CLONES_H */

