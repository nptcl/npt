#ifndef __STREAM_BROADCAST_HEADER__
#define __STREAM_BROADCAST_HEADER__

#include "typedef.h"

_g void open_broadcast_stream(addr *stream, addr list);
_g void push_broadcast_stream(addr stream, addr output);
_g void get_broadcast_stream(addr stream, addr *ret);
_g void init_stream_broadcast(void);

#endif

