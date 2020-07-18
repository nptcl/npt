#ifndef __PROMPT_HEADER__
#define __PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

_g void push_prompt_info(Execute ptr);
_g size_t getindex_prompt(Execute ptr);
_g void setindex_prompt(Execute ptr, size_t value);
_g int getbreak_prompt(Execute ptr);
_g void setbreak_prompt(Execute ptr, int value);
_g int getshow_prompt(Execute ptr);
_g void setshow_prompt(Execute ptr, int value);

_g int show_prompt_(Execute ptr, addr io);
_g int input_prompt(addr *ret, addr *prompt, const char *message);

#endif

