#ifndef __STREAM_CONCAT_HEADER__
#define __STREAM_CONCAT_HEADER__

#include "typedef.h"

void open_concatenated_stream(addr *stream, addr list);
void push_concatenated_stream(addr stream, addr input);
void get_concatenated_stream(addr stream, addr *ret);
void init_stream_concatenated(void);

#endif

