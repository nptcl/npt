#ifndef __PROMPT_FOR_HEADER__
#define __PROMPT_FOR_HEADER__

#include "execute.h"
#include "typedef.h"

#define prompt_for_stream_ _n(prompt_for_stream_)
#define prompt_string_stream_ _n(prompt_string_stream_)
#define yes_or_no_p_common_ _n(yes_or_no_p_common_)

int prompt_for_stream_(Execute ptr, addr check, addr prompt, addr *ret);
int prompt_string_stream_(Execute ptr, addr prompt, int errorp, addr *ret);
int yes_or_no_p_common_(Execute ptr, addr args, int exactp, int *ret);

#endif

