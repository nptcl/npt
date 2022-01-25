#ifndef __TYPE_COERCE_HEADER__
#define __TYPE_COERCE_HEADER__

#include "define.h"
#include "execute.h"

#define init_type_coerce _n(init_type_coerce)
#define coerce_common_ _n(coerce_common_)

void init_type_coerce(void);
int coerce_common_(Execute ptr, addr pos, addr type, addr *ret);

#endif

