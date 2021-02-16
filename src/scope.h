#ifndef __SCOPE_HEADER__
#define __SCOPE_HEADER__

#include "execute.h"
#include "typedef.h"

#define begin_scope_ _n(begin_scope_)
#define eval_scope_ _n(eval_scope_)
#define init_scope _n(init_scope)

int begin_scope_(Execute ptr);
int eval_scope_(Execute ptr, addr *ret, addr eval);
void init_scope(void);

#endif

