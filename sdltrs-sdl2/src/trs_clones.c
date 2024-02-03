#include <stdio.h>
#include "trs.h"
#include "trs_clones.h"
#include "trs_state_save.h"

struct trs_clones trs_clones;
static const struct trs_clones clones[] = {
  /*
   * Clone       Name of clone
   */
  { 0,           NULL,                    },
  { CP500,       "Prologica CP-500"       },
  { CP500_M80,   "Prologica CP-500 M80"   },
  { CT80,        "Aster CT-80"            },
  { EG3200,      "EACA EG 3200 Genie III" },
  { GENIE3S,     "TCS Genie IIIs"         },
  { LNW80,       "LNW Research LNW80"     },
  { SPEEDMASTER, "TCS SpeedMaster"        },
};

static int current_clone;

void trs_clones_model(int clone)
{
  if (clone == current_clone)
    return;

  current_clone = clone;

  switch (clone) {
    case CP500:
      trs_clones = clones[1];
      break;
    case CP500_M80:
      trs_clones = clones[2];
      break;
    case CT80:
      trs_clones = clones[3];
      break;
    case EG3200:
      trs_clones = clones[4];
      break;
    case GENIE3S:
      trs_clones = clones[5];
      break;
    case LNW80:
      trs_clones = clones[6];
      break;
    case SPEEDMASTER:
      trs_clones = clones[7];
      break;
    default:
      trs_clones = clones[0];
  }

  trs_screen_caption();
}

void trs_clone_save(FILE *file)
{
  trs_save_int(file, &current_clone, 1);
}

void trs_clone_load(FILE *file)
{
  int clone_model;

  trs_load_int(file, &clone_model, 1);
  trs_clones_model(clone_model);
}
