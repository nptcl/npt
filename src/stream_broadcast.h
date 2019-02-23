#ifndef __STREAM_BROADCAST_HEADER__
#define __STREAM_BROADCAST_HEADER__

#include "typedef.h"

void open_broadcast_stream(addr *stream, addr list);
void push_broadcast_stream(addr stream, addr input);
void get_broadcast_stream(addr stream, addr *ret);
void init_stream_broadcast(void);

#endif

