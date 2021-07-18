#ifndef __TERME_HISTORY_HEADER__
#define __TERME_HISTORY_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_history_build _n(terme_history_build)
#define terme_history_clear_ _n(terme_history_clear_)
#define terme_history_return_ _n(terme_history_return_)
#define terme_history_select_ _n(terme_history_select_)

void terme_history_build(addr *ret);
int terme_history_clear_(Execute ptr);
int terme_history_return_(Execute ptr);
int terme_history_select_(Execute ptr, int diff, int *ret);

#endif

