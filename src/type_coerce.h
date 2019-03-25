#ifndef __TYPE_COERCE_HEADER__
#define __TYPE_COERCE_HEADER__

#include "execute.h"

void init_type_coerce(void);
int coerce_common(Execute ptr, addr pos, addr type, addr *ret);

#endif

