#ifndef __SUBTYPEP_COMPOUND_HEADER__
#define __SUBTYPEP_COMPOUND_HEADER__

#include "subtypep_typedef.h"
#include "typedef.h"

#define subtypep_atomic_not_ _n(subtypep_atomic_not_)
#define subtypep_compound_ _n(subtypep_compound_)
#define subtypep_force_ _n(subtypep_force_)
#define subtypep_normal_ _n(subtypep_normal_)

int subtypep_atomic_not_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_compound_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_force_(Execute ptr, addr x, addr y, SubtypepResult *ret);
int subtypep_normal_(Execute ptr, addr x, addr y, SubtypepResult *ret);

#endif

