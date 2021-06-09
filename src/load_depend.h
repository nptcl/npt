#ifndef __LOAD_DEPEND_HEADER__
#define __LOAD_DEPEND_HEADER__

#include "execute.h"
#include "typedef.h"

#define get_depend_root_ _n(get_depend_root_)

#define load_depend_heap _n(load_depend_heap)
#define get_stream_load_depend _n(get_stream_load_depend)
#define get_index_load_depend _n(get_index_load_depend)
#define begin_load_push _n(begin_load_push)
#define end_load_push_ _n(end_load_push_)
#define push_load_push_ _n(push_load_push_)

#define load_depend_code_ _n(load_depend_code_)
#define load_depend_partial_ _n(load_depend_partial_)
#define load_depend_instance_ _n(load_depend_instance_)
#define compile_depend_make_ _n(compile_depend_make_)

#define init_load_depend _n(init_load_depend)
#define disable_load_depend _n(disable_load_depend)

int get_depend_root_(Execute ptr, addr *ret);

void load_depend_heap(addr *ret, addr stream, addr value, addr index);
void get_stream_load_depend(addr pos, addr *ret);
void get_index_load_depend(addr pos, addr *ret);
void begin_load_push(Execute ptr);
int end_load_push_(Execute ptr, addr code);
int push_load_push_(Execute ptr, addr code);

int load_depend_code_(Execute ptr, addr stream, addr value);
int load_depend_partial_(Execute ptr, addr stream, addr value, addr *ret);
int load_depend_instance_(Execute ptr, addr instance, addr make, addr init);
int compile_depend_make_(Execute ptr, addr stream, addr code);

void init_load_depend(Execute ptr);
void disable_load_depend(Execute ptr);

#endif

