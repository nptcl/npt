#ifndef __STREAM_MEMORY_HEADER__
#define __STREAM_MEMORY_HEADER__

#include "typedef.h"

#define open_input_memory_stream_ _n(open_input_memory_stream_)
#define open_output_memory_stream _n(open_output_memory_stream)
#define memory_stream_heap_ _n(memory_stream_heap_)
#define clear_output_memory_stream _n(clear_output_memory_stream)
#define init_stream_memory_input _n(init_stream_memory_input)
#define init_stream_memory_output _n(init_stream_memory_output)

_g int open_input_memory_stream_(addr *ret, addr pos);
_g void open_output_memory_stream(addr *ret, size_t size);
_g int memory_stream_heap_(addr stream, addr *ret);
_g void clear_output_memory_stream(addr stream);
_g void init_stream_memory_input(void);
_g void init_stream_memory_output(void);

#endif

