#ifndef __STREAM_STRING_HEADER__
#define __STREAM_STRING_HEADER__

#include "local.h"
#include "typedef.h"

_g int open_input_string_stream_(addr *stream, addr string);
_g int open_input_string_stream1_(addr *stream, addr string, size_t start);
_g int open_input_string_stream2_(addr *stream, addr string, size_t start, size_t end);
_g void open_input_char_stream(addr *stream, const char *str);
_g void null_input_string_stream(addr *stream);
_g void getindex_input_stream(addr stream, size_t *ret);
_g void setvalue_input_string_stream(addr stream, addr value);
_g void clear_input_string_stream(addr stream);
_g void close_input_string_stream(addr stream);

_g void open_output_string_stream(addr *stream, size_t size);
_g int copy_termsize_string_stream_(addr stream, addr src);
_g int string_stream_alloc_(LocalRoot local, addr stream, addr *string);
_g int string_stream_local_(LocalRoot local, addr stream, addr *string);
_g int string_stream_heap_(addr stream, addr *string);
_g void clear_output_string_stream(addr stream);
_g void set_pretty_output_string_stream(addr stream);
_g int get_pretty_output_string_stream(addr stream);
_g void open_extend_output_stream(addr *stream, addr array);
_g void close_output_string_stream(addr stream);

_g void init_stream_string_input(void);
_g void init_stream_string_output(void);

#endif

