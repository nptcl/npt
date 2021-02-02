#ifndef __SUBTYPEP_HEADER__
#define __SUBTYPEP_HEADER__

#include "execute.h"
#include "subtypep_typedef.h"
#include "typedef.h"

#define init_subtypep _n(init_subtypep)
#define subtypep_extend_ _n(subtypep_extend_)
#define subtypep_scope_ _n(subtypep_scope_)
#define subtypep_check_ _n(subtypep_check_)

void init_subtypep(void);
int subtypep_extend_(Execute ptr, addr x, addr y, addr env, addr check, addr *ret);
int subtypep_scope_(Execute ptr, addr x, addr y, addr env, SubtypepResult *ret);
int subtypep_check_(Execute ptr, addr x, addr y, addr env, int *ret, int *validp);

#endif

