#ifndef __STRUCTURE_CHANGE_HEADER__
#define __STRUCTURE_CHANGE_HEADER__

#include "structure_defstruct.h"
#include "typedef.h"

#define structure_change1_ _n(structure_change1_)
#define structure_change2_ _n(structure_change2_)
#define structure_change3_ _n(structure_change3_)

int structure_change1_(struct defstruct *str);
int structure_change2_(struct defstruct *str);
int structure_change3_(struct defstruct *str);

#endif

