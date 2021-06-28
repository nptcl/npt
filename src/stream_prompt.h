#ifndef __STREAM_PROMPT_HEADER__
#define __STREAM_PROMPT_HEADER__

#include "define.h"
#include "execute.h"
#include "typedef.h"

#define open_prompt_stream _n(open_prompt_stream)
#define init_stream_prompt _n(init_stream_prompt)

void open_prompt_stream(addr *stream);
void init_stream_prompt(void);

#endif

