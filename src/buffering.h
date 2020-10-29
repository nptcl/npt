#ifndef __BUFFERING_HEADER__
#define __BUFFERING_HEADER__

#include "typedef.h"

#define bufferingp _n(bufferingp)
#define buffering_heap _n(buffering_heap)
#define clear_buffering _n(clear_buffering)
#define getcell_buffering _n(getcell_buffering)
#define getwidth_buffering _n(getwidth_buffering)
#define putc_buffering _n(putc_buffering)
#define getc_buffering _n(getc_buffering)
#define position_get_buffering _n(position_get_buffering)
#define position_set_buffering _n(position_set_buffering)
#define position_start_buffering _n(position_start_buffering)
#define position_end_buffering _n(position_end_buffering)
#define length_buffering _n(length_buffering)
#define make_vector_buffering_heap_ _n(make_vector_buffering_heap_)
#define read_buffering_ _n(read_buffering_)

_g int bufferingp(addr pos);
_g void buffering_heap(addr *ret, size_t cell, size_t array);
_g void clear_buffering(addr pos);
_g void getcell_buffering(addr pos, size_t *ret);
_g void getwidth_buffering(addr pos, size_t *ret);

_g int putc_buffering(addr pos, byte c);
_g int getc_buffering(addr pos, byte *ret);
_g void position_get_buffering(addr pos, size_t *ret);
_g void position_set_buffering(addr pos, size_t value);
_g void position_start_buffering(addr pos);
_g void position_end_buffering(addr pos);
_g void length_buffering(addr pos, size_t *ret);
_g int make_vector_buffering_heap_(addr pos, addr *ret);
_g int read_buffering_(addr pos, addr vector);

#endif

