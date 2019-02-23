#ifndef __PROMPT_HEADER__
#define __PROMPT_HEADER__

#include "execute.h"
#include "typedef.h"

void push_prompt_info(Execute ptr);
size_t getindex_prompt(Execute ptr);
void setindex_prompt(Execute ptr, size_t value);
int getbreak_prompt(Execute ptr);
void setbreak_prompt(Execute ptr, int value);
int getshow_prompt(Execute ptr);
void setshow_prompt(Execute ptr, int value);

void show_prompt(Execute ptr, addr io);
int input_prompt(addr *ret);

#endif

