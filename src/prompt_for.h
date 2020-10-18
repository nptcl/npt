#ifndef __PROMPT_FOR_HEADER__
#define __PROMPT_FOR_HEADER__

#include "execute.h"
#include "typedef.h"

#define prompt_for_stream _n(prompt_for_stream)
#define yes_or_no_p_common _n(yes_or_no_p_common)

_g int prompt_for_stream(Execute ptr, addr check, addr prompt, addr *ret);
_g int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret);

#endif

