#ifndef __PROMPT_FOR_HEADER__
#define __PROMPT_FOR_HEADER__

#include "execute.h"
#include "typedef.h"

#define prompt_for_stream _n(prompt_for_stream)
#define prompt_string_stream _n(prompt_string_stream)
#define yes_or_no_p_common _n(yes_or_no_p_common)

int prompt_for_stream(Execute ptr, addr check, addr prompt, addr *ret);
int prompt_string_stream(Execute ptr, addr prompt, addr *ret);
int yes_or_no_p_common(Execute ptr, addr args, int exactp, int *ret);

#endif

