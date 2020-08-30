#ifndef __TYPE_COERCE_HEADER__
#define __TYPE_COERCE_HEADER__

#include "define.h"
#include "execute.h"

#define init_type_coerce _n(init_type_coerce)
#define coerce_common _n(coerce_common)

_g void init_type_coerce(void);
_g int coerce_common(Execute ptr, addr pos, addr type, addr *ret);

#endif

