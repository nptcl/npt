#ifndef __STREAM_BROADCAST_HEADER__
#define __STREAM_BROADCAST_HEADER__

#include "typedef.h"

#define open_broadcast_stream_ _n(open_broadcast_stream_)
#define push_broadcast_stream _n(push_broadcast_stream)
#define get_broadcast_stream _n(get_broadcast_stream)
#define init_stream_broadcast _n(init_stream_broadcast)

int open_broadcast_stream_(addr *stream, addr list);
void push_broadcast_stream(addr stream, addr output);
void get_broadcast_stream(addr stream, addr *ret);
void init_stream_broadcast(void);

#endif

