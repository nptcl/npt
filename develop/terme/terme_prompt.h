#ifndef __TERME_PROMPT_HEADER__
#define __TERME_PROMPT_HEADER__

#include "execute.h"
#include "prompt.h"
#include "typedef.h"

#define terme_prompt_ _n(terme_prompt_)
#define terme_readline_ _n(terme_readline_)

int terme_prompt_(Execute ptr, addr pos, enum prompt_mode mode);
int terme_readline_(Execute ptr, addr *ret);

#endif

