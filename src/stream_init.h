#ifndef __STREAM_INIT_HEADER__
#define __STREAM_INIT_HEADER__

#include "define.h"
#include "execute.h"

#define init_stream _n(init_stream)
#define build_stream _n(build_stream)
#define push_close_stream _n(push_close_stream)

void init_stream(void);
void build_stream(void);
void push_close_stream(Execute ptr, addr stream);

#endif

