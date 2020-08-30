#ifndef __PROMPT_HEADER__
#define __PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

#define push_prompt_info _n(push_prompt_info)
#define getindex_prompt _n(getindex_prompt)
#define setindex_prompt _n(setindex_prompt)
#define getbreak_prompt _n(getbreak_prompt)
#define setbreak_prompt _n(setbreak_prompt)
#define getshow_prompt _n(getshow_prompt)
#define setshow_prompt _n(setshow_prompt)
#define show_prompt_ _n(show_prompt_)
#define input_prompt _n(input_prompt)

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

