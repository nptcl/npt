#ifndef __SCOPE_FUNCTION_HEADER__
#define __SCOPE_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

_g int scope_progn(Execute ptr, addr *ret, addr eval);
_g int scope_locally(Execute ptr, addr *ret, addr eval);
_g int scope_eval_when(Execute ptr, addr *ret, addr eval);

_g void init_scope_function(void);

#endif

