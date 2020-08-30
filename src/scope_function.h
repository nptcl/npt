#ifndef __SCOPE_FUNCTION_HEADER__
#define __SCOPE_FUNCTION_HEADER__

#include "execute.h"
#include "typedef.h"

#define scope_progn _n(scope_progn)
#define scope_locally _n(scope_locally)
#define scope_eval_when _n(scope_eval_when)
#define init_scope_function _n(init_scope_function)

_g int scope_progn(Execute ptr, addr *ret, addr eval);
_g int scope_locally(Execute ptr, addr *ret, addr eval);
_g int scope_eval_when(Execute ptr, addr *ret, addr eval);

_g void init_scope_function(void);

#endif

