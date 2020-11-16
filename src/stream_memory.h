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

int open_input_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache);
int open_output_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache);
int open_io_memory_stream_(addr *ret, addr input,
		size_t cell, size_t array, int cache);
int memory_stream_heap_(addr stream, addr *ret);
int clear_memory_stream_(addr stream);
void getsize_memory_stream(addr stream, size_t *ret);
void getarray_memory_stream(addr stream, size_t *ret);
int getcache_memory_stream(addr stream);
void gettype_memory_stream(addr stream, addr *ret);
int settype_memory_stream_(addr stream, addr value);

void init_stream_memory_input(void);
void init_stream_memory_output(void);
void init_stream_memory_io(void);

/* file-buffering */
int read_byte_memory_stream(addr stream, byte *value, int *ret);
int write_byte_memory_stream(addr stream, byte value);
int file_length_memory_stream(addr stream, size_t *ret);
int file_position_memory_stream(addr stream, size_t *ret);
int file_position_start_memory_stream(addr stream);
int file_position_end_memory_stream(addr stream);
int file_position_set_memory_stream(addr stream, size_t value);

#endif

