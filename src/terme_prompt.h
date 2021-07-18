#ifndef __TERME_PROMPT_HEADER__
#define __TERME_PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

#define terme_readline_ _n(terme_readline_)

int terme_readline_(Execute ptr, addr *ret);

#endif

