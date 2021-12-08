#ifndef __STRUCTURE_DEFINE_HEADER__
#define __STRUCTURE_DEFINE_HEADER__

#include "execute.h"
#include "typedef.h"

#define ensure_structure_common_ _n(ensure_structure_common_)
#define init_structure_define _n(init_structure_define)

int ensure_structure_common_(Execute ptr, addr name, addr slots, addr rest);
void init_structure_define(void);

#endif

