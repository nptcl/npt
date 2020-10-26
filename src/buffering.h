#ifndef __BUFFERING_HEADER__
#define __BUFFERING_HEADER__

#include "typedef.h"

#define bufferingp _n(bufferingp)
#define buffering_heap _n(buffering_heap)
#define clear_buffering _n(clear_buffering)
#define push_buffering _n(push_buffering)
#define position_buffering _n(position_buffering)
#define get_length_buffering _n(get_length_buffering)
#define get_buffering_heap_ _n(get_buffering_heap_)

_g int bufferingp(addr pos);
_g void buffering_heap(addr *ret, size_t size);
_g void clear_buffering(addr pos);
_g void push_buffering(addr pos, byte c);
_g void position_buffering(addr pos, size_t value, int *ret);
_g void get_length_buffering(addr pos, size_t *ret);
_g int get_buffering_heap_(addr pos, addr *ret);

#endif

