#ifndef __LOAD_OBJECT_HEADER__
#define __LOAD_OBJECT_HEADER__

#include "execute.h"
#include "typedef.h"

#define load_time_value_heap _n(load_time_value_heap)
#define get_index_load_time_value _n(get_index_load_time_value)
#define result_load_time_value_ _n(result_load_time_value_)

void load_time_value_heap(addr *ret, addr value, addr index);
void get_index_load_time_value(addr pos, size_t *ret);
int result_load_time_value_(Execute ptr, addr pos, addr *ret);

#endif

