#ifndef __PROMPT_HEADER__
#define __PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

#define push_prompt_info _n(push_prompt_info)
#define getindex_prompt _n(getindex_prompt)
#define getindex_prompt_safe _n(getindex_prompt_safe)
#define setindex_prompt _n(setindex_prompt)
#define getbreak_prompt _n(getbreak_prompt)
#define setbreak_prompt _n(setbreak_prompt)
#define getshow_prompt _n(getshow_prompt)
#define getshow_prompt_safe _n(getshow_prompt_safe)
#define setshow_prompt _n(setshow_prompt)
#define endshow_prompt_safe _n(endshow_prompt_safe)
#define show_prompt_ _n(show_prompt_)
#define input_prompt _n(input_prompt)

void push_prompt_info(Execute ptr);
size_t getindex_prompt(Execute ptr);
size_t getindex_prompt_safe(Execute ptr);
void setindex_prompt(Execute ptr, size_t value);
int getbreak_prompt(Execute ptr);
void setbreak_prompt(Execute ptr, int value);
int getshow_prompt(Execute ptr);
int getshow_prompt_safe(Execute ptr);
void setshow_prompt(Execute ptr, int value);
void endshow_prompt_safe(Execute ptr);

int show_prompt_(Execute ptr, addr io);
int input_prompt(addr *ret, addr *prompt, const char *message);

#endif

