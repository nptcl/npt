#ifndef __SUBTYPEP_COMPOUND_HEADER__
#define __SUBTYPEP_COMPOUND_HEADER__

#include "subtypep_typedef.h"
#include "typedef.h"

#define subtypep_atomic_not_ _n(subtypep_atomic_not_)
#define subtypep_compound_ _n(subtypep_compound_)
#define subtypep_force_number_ _n(subtypep_force_number_)

int subtypep_atomic_not_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_compound_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_force_number_(Execute ptr, addr x, addr y, SubtypepResult *ret);

#endif

