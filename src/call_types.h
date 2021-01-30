#ifndef __CALL_TYPES_HEADER__
#define __CALL_TYPES_HEADER__

#include "execute.h"
#include "typedef.h"

#define type_of_common_ _n(type_of_common_)
#define typep_common_ _n(typep_common_)

int type_of_common_(addr pos, addr *ret);
int typep_common_(Execute ptr, addr x, addr y, addr env, addr *ret);

#endif

