#ifndef __SCOPE_HEADER__
#define __SCOPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define eval_scope _n(eval_scope)
#define eval_scope_compile _n(eval_scope_compile)
#define init_scope _n(init_scope)

int eval_scope(Execute ptr, addr *ret, addr eval);
int eval_scope_compile(Execute ptr, addr *ret, addr eval);
void init_scope(void);

#endif

