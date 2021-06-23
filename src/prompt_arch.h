#ifndef __PROMPT_ARCH_HEADER__
#define __PROMPT_ARCH_HEADER__

#include "execute.h"
#include "typedef.h"

#define show_prompt_ _n(show_prompt_)
#define input_prompt_ _n(input_prompt_)

int show_prompt_(Execute ptr, addr io);
int input_prompt_(addr *ret, addr *prompt, const char *str);

#endif

