#ifndef __STREAM_MEMORY_HEADER__
#define __STREAM_MEMORY_HEADER__

#include "typedef.h"

#define INPUT_MEMORY_UNREAD_SIZE	16

#define open_input_memory_stream_ _n(open_input_memory_stream_)
#define open_output_memory_stream_ _n(open_output_memory_stream_)
#define open_io_memory_stream_ _n(open_io_memory_stream_)
#define memory_stream_heap_ _n(memory_stream_heap_)
#define clear_memory_stream_ _n(clear_memory_stream_)
#define getsize_memory_stream _n(getsize_memory_stream)
#define getarray_memory_stream _n(getarray_memory_stream)
#define getcache_memory_stream _n(getcache_memory_stream)
#define gettype_memory_stream _n(gettype_memory_stream)
#define settype_memory_stream_ _n(settype_memory_stream_)
#define init_stream_memory_input _n(init_stream_memory_input)
#define init_stream_memory_output _n(init_stream_memory_output)
#define init_stream_memory_io _n(init_stream_memory_io)

#define read_byte_memory_stream _n(read_byte_memory_stream)
#define write_byte_memory_stream _n(write_byte_memory_stream)
#define file_length_memory_stream _n(file_length_memory_stream)
#define file_position_memory_stream _n(file_position_memory_stream)
#define file_position_start_memory_stream _n(file_position_start_memory_stream)
#define file_position_end_memory_stream _n(file_position_end_memory_stream)
#define file_position_set_memory_stream _n(file_position_set_memory_stream)

_g int open_input_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache);
_g int open_output_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache);
_g int open_io_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache);
_g int memory_stream_heap_(addr stream, addr *ret);
_g int clear_memory_stream_(addr stream);
_g void getsize_memory_stream(addr stream, size_t *ret);
_g void getarray_memory_stream(addr stream, size_t *ret);
_g int getcache_memory_stream(addr stream);
_g void gettype_memory_stream(addr stream, addr *ret);
_g int settype_memory_stream_(addr stream, addr value);

_g void init_stream_memory_input(void);
_g void init_stream_memory_output(void);
_g void init_stream_memory_io(void);

/* file-buffering */
_g int read_byte_memory_stream(addr stream, byte *value, int *ret);
_g int write_byte_memory_stream(addr stream, byte value);
_g int file_length_memory_stream(addr stream, size_t *ret);
_g int file_position_memory_stream(addr stream, size_t *ret);
_g int file_position_start_memory_stream(addr stream);
_g int file_position_end_memory_stream(addr stream);
_g int file_position_set_memory_stream(addr stream, size_t value);

#endif

