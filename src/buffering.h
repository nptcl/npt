#ifndef __BUFFERING_HEADER__
#define __BUFFERING_HEADER__

#include "typedef.h"

#define bufferingp _n(bufferingp)
#define buffering_heap _n(buffering_heap)
#define clear_buffering _n(clear_buffering)
#define end_buffering _n(end_buffering)
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

int bufferingp(addr pos);
void buffering_heap(addr *ret, size_t cell, size_t array);
void clear_buffering(addr pos);
int end_buffering(addr pos);
void getcell_buffering(addr pos, size_t *ret);
void getwidth_buffering(addr pos, size_t *ret);

int putc_buffering(addr pos, byte c);
int getc_buffering(addr pos, byte *ret);
void position_get_buffering(addr pos, size_t *ret);
void position_set_buffering(addr pos, size_t value);
void position_start_buffering(addr pos);
void position_end_buffering(addr pos);
void length_buffering(addr pos, size_t *ret);
int make_vector_buffering_heap_(addr pos, addr *ret);
int read_buffering_(addr pos, addr vector);

#endif

