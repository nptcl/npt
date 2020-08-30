#ifndef __SCOPE_HEADER__
#define __SCOPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define eval_scope _n(eval_scope)
#define eval_scope_compile _n(eval_scope_compile)
#define init_scope _n(init_scope)

_g int eval_scope(Execute ptr, addr *ret, addr eval);
_g int eval_scope_compile(Execute ptr, addr *ret, addr eval);
_g void init_scope(void);

#endif

